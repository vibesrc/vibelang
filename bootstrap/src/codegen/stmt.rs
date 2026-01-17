//! Statement compilation

use super::{Codegen, CodegenError, VarInfo};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::types::BasicTypeEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_block(&mut self, block: &Block) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let mut last_value = None;

        for stmt in &block.stmts {
            last_value = self.compile_stmt(stmt)?;
        }

        Ok(last_value)
    }

    pub(crate) fn compile_stmt(&mut self, stmt: &Stmt) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        match stmt {
            Stmt::Let { name, ty, value, .. } => {
                // Track moves: if we're copying a struct from a variable, the source is moved
                let (struct_name, source_var) = match value {
                    Expr::StructInit { name: sname, generics, .. } => {
                        // For generic structs, use mangled name
                        let mono_name = if generics.is_empty() {
                            sname.clone()
                        } else {
                            self.mangle_name(sname, generics)
                        };
                        (Some(mono_name), None)
                    }
                    Expr::Ident(src_name, _) => {
                        // Check if source variable is a struct type (moves on assignment)
                        if let Some(var_info) = self.variables.get(src_name) {
                            if var_info.struct_name.is_some() && !var_info.is_ref {
                                // This is a move - mark source as moved
                                (var_info.struct_name.clone(), Some(src_name.clone()))
                            } else {
                                (None, None)
                            }
                        } else {
                            (None, None)
                        }
                    }
                    // Handle static method calls like IntVec.new() or enum constructors like Color.Red
                    // Also handles instance method calls like self.as_bytes()
                    Expr::MethodCall { receiver, method, args, .. } => {
                        if let Expr::Ident(type_name, _) = receiver.as_ref() {
                            // Check if receiver is a struct type (static method call) or enum type (variant constructor)
                            if self.struct_types.contains_key(type_name) {
                                // Static method call on a struct - look up method return type
                                let mangled_method = format!("{}_{}", type_name, method);
                                let struct_name = self.function_return_types
                                    .get(&mangled_method)
                                    .and_then(|ret_ty| ret_ty.as_ref())
                                    .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty))
                                    .or_else(|| Some(type_name.clone())); // fallback to type name if method not found
                                (struct_name, None)
                            } else if self.enum_types.contains_key(type_name) {
                                // Enum variant constructor - the result is the enum type
                                (Some(type_name.clone()), None)
                            } else if let Some(generic_enum) = self.generic_enums.get(type_name).cloned() {
                                // Generic enum variant constructor - infer type args to get mangled name
                                if let Some(variant) = generic_enum.variants.iter().find(|v| v.name == *method) {
                                    if let Ok(Some(inferred_types)) = self.infer_enum_type_args(&generic_enum, variant, args) {
                                        let mangled = self.mangle_name(type_name, &inferred_types);
                                        (Some(mangled), None)
                                    } else {
                                        (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                    }
                                } else {
                                    (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                }
                            } else if let Some(var_info) = self.variables.get(type_name) {
                                // Instance method call - look up method return type
                                if let Some(ref receiver_struct) = var_info.struct_name {
                                    // Look up the method for this struct type
                                    if let Some(methods) = self.type_methods.get(receiver_struct) {
                                        if let Some(mangled_method) = methods.get(method) {
                                            // Get return type from the mangled method
                                            let struct_name = self.function_return_types
                                                .get(mangled_method)
                                                .and_then(|ret_ty| ret_ty.as_ref())
                                                .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                            (struct_name, None)
                                        } else {
                                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                        }
                                    } else {
                                        (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                    }
                                } else if var_info.ref_struct_name.is_some() {
                                    // Reference to a struct - look up method on the referenced struct
                                    let ref_struct = var_info.ref_struct_name.as_ref().unwrap();
                                    if let Some(methods) = self.type_methods.get(ref_struct) {
                                        if let Some(mangled_method) = methods.get(method) {
                                            let struct_name = self.function_return_types
                                                .get(mangled_method)
                                                .and_then(|ret_ty| ret_ty.as_ref())
                                                .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                            (struct_name, None)
                                        } else {
                                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                        }
                                    } else {
                                        (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                    }
                                } else {
                                    (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                }
                            } else {
                                (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                            }
                        } else if let Expr::Field { object, field: type_name, .. } = receiver.as_ref() {
                            // Check for module-qualified static method call: fs.File.read()
                            // Pattern: MethodCall { receiver: Field { object: Ident(module), field: TypeName }, method, ... }
                            if let Expr::Ident(module_name, _) = object.as_ref() {
                                if self.module_aliases.contains_key(module_name) {
                                    // Resolve the type: fs_File -> File
                                    let qualified_type = format!("{}_{}", module_name, type_name);
                                    let resolved_type = self.imports.get(&qualified_type)
                                        .cloned()
                                        .unwrap_or(type_name.clone());

                                    // Look up the method return type
                                    if let Some(methods) = self.type_methods.get(&resolved_type) {
                                        if let Some(mangled_method) = methods.get(method) {
                                            let struct_name = self.function_return_types
                                                .get(mangled_method)
                                                .and_then(|ret_ty| ret_ty.as_ref())
                                                .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                            (struct_name, None)
                                        } else {
                                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                        }
                                    } else {
                                        (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                    }
                                } else {
                                    (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                }
                            } else {
                                (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                            }
                        } else {
                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                        }
                    }
                    // Handle regular Call expressions that might return structs
                    Expr::Call { func, args, type_args, .. } => {
                        // Handle generic struct static method call like Array<i64>.new()
                        if let Expr::Field { object, field, .. } = func.as_ref() {
                            if let Expr::StructInit { name: type_name, generics, fields, .. } = object.as_ref() {
                                if fields.is_empty() && !generics.is_empty() {
                                    // This is a generic type static method call
                                    let mono_name = self.mangle_name(type_name, generics);
                                    // Look up the return type of this method
                                    if let Some(methods) = self.type_methods.get(&mono_name) {
                                        if let Some(mangled_method) = methods.get(field) {
                                            let struct_name = self.function_return_types
                                                .get(mangled_method)
                                                .and_then(|ret_ty| ret_ty.as_ref())
                                                .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                            (struct_name, None)
                                        } else {
                                            // Fallback: assume it returns Self type
                                            (Some(mono_name), None)
                                        }
                                    } else {
                                        // Methods not yet compiled, assume returns Self
                                        (Some(mono_name), None)
                                    }
                                } else {
                                    (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                }
                            } else if let Expr::Field { object: inner_obj, field: type_name, .. } = object.as_ref() {
                                // Check for module-qualified static method call: fs.File.read()
                                // Pattern: Field { object: Field { object: Ident(module), field: TypeName }, field: method }
                                if let Expr::Ident(module_name, _) = inner_obj.as_ref() {
                                    if self.module_aliases.contains_key(module_name) {
                                        // Resolve the type: fs_File -> File
                                        let qualified_type = format!("{}_{}", module_name, type_name);
                                        let resolved_type = self.imports.get(&qualified_type)
                                            .cloned()
                                            .unwrap_or(type_name.clone());

                                        // Look up the method return type
                                        if let Some(methods) = self.type_methods.get(&resolved_type) {
                                            if let Some(mangled_method) = methods.get(field) {
                                                let struct_name = self.function_return_types
                                                    .get(mangled_method)
                                                    .and_then(|ret_ty| ret_ty.as_ref())
                                                    .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                                (struct_name, None)
                                            } else {
                                                (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                            }
                                        } else {
                                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                        }
                                    } else {
                                        (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                    }
                                } else {
                                    (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                                }
                            } else {
                                (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                            }
                        } else if let Expr::Ident(fn_name, _) = func.as_ref() {
                            // First check explicit type annotation
                            if let Some(t) = ty.as_ref() {
                                (self.get_struct_name_for_type(t), None)
                            } else if fn_name == "ptr_read" && !type_args.is_empty() {
                                // ptr_read<T>() returns T - extract struct name from type arg
                                (self.get_struct_name_for_type(&type_args[0]), None)
                            } else if let Some(generic_func) = self.generic_functions.get(fn_name).cloned() {
                                // Generic function - infer return type with substituted type params
                                let inferred_types = if !type_args.is_empty() {
                                    type_args.clone()
                                } else if let Ok(types) = self.infer_function_type_args(&generic_func, args) {
                                    types
                                } else {
                                    vec![]
                                };

                                if !inferred_types.is_empty() {
                                    // Substitute type params in return type
                                    if let Some(ref ret_ty) = generic_func.return_type {
                                        let substituted = self.substitute_type_params(ret_ty, &generic_func.generics, &inferred_types);
                                        (self.get_struct_name_for_type(&substituted), None)
                                    } else {
                                        (None, None)
                                    }
                                } else {
                                    (None, None)
                                }
                            } else {
                                // Infer from function return type
                                let struct_name = self.function_return_types
                                    .get(fn_name)
                                    .and_then(|ret_ty| ret_ty.as_ref())
                                    .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                (struct_name, None)
                            }
                        } else {
                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                        }
                    }
                    // Handle Try expressions (? operator): let file = File.read(path)?
                    Expr::Try { operand, .. } => {
                        // Need to get the payload type from the Result/Option
                        // The operand returns Result<T, E> or Option<T>, and ? unwraps to T
                        let inner_struct_name = match operand.as_ref() {
                            Expr::MethodCall { receiver, method, .. } => {
                                if let Expr::Ident(name, _) = receiver.as_ref() {
                                    // Check if this is a static method call (File.read)
                                    if self.struct_types.contains_key(name) {
                                        let mangled_method = format!("{}_{}", name, method);
                                        self.function_return_types
                                            .get(&mangled_method)
                                            .and_then(|ret_ty| ret_ty.as_ref())
                                            .and_then(|ret_ty| self.get_result_ok_type(ret_ty))
                                            .and_then(|ok_ty| self.get_struct_name_for_type(&ok_ty))
                                    } else if let Some(var_info) = self.variables.get(name) {
                                        // Instance method call (file.read_all)
                                        let struct_name = var_info.struct_name.as_ref()
                                            .or(var_info.ref_struct_name.as_ref());
                                        if let Some(struct_name) = struct_name {
                                            // Look up method return type
                                            if let Some(methods) = self.type_methods.get(struct_name) {
                                                if let Some(mangled_method) = methods.get(method) {
                                                    self.function_return_types
                                                        .get(mangled_method)
                                                        .and_then(|ret_ty| ret_ty.as_ref())
                                                        .and_then(|ret_ty| self.get_result_ok_type(ret_ty))
                                                        .and_then(|ok_ty| self.get_struct_name_for_type(&ok_ty))
                                                } else {
                                                    None
                                                }
                                            } else {
                                                None
                                            }
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        };
                        (inner_struct_name, None)
                    }
                    // Handle field access for unit enum variants like Color.Red or Option<i32>.None
                    Expr::Field { object, .. } => {
                        if let Expr::Ident(type_name, _) = object.as_ref() {
                            // Check if object is an enum type (unit variant constructor)
                            if self.enum_types.contains_key(type_name) {
                                (Some(type_name.clone()), None)
                            } else {
                                (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                            }
                        } else if let Expr::StructInit { name, generics, fields, .. } = object.as_ref() {
                            // Handle generic enum unit variant like Option<i32>.None
                            if fields.is_empty() && !generics.is_empty() {
                                let mono_name = self.mangle_name(name, generics);
                                (Some(mono_name), None)
                            } else {
                                (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                            }
                        } else {
                            (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None)
                        }
                    }
                    _ => (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None),
                };

                // Track reference info: detect if this is a reference to a struct
                let (is_ref, is_mut_ref, ref_struct_name, slice_elem_type) = match value {
                    Expr::Ref { operand, .. } => {
                        if let Expr::Ident(ref_name, _) = operand.as_ref() {
                            if let Some(var_info) = self.variables.get(ref_name) {
                                if var_info.ty.is_array_type() {
                                    // Reference to array - extract element type
                                    let array_ty = var_info.ty.into_array_type();
                                    let elem_llvm_ty = array_ty.get_element_type();
                                    let elem_ty = self.llvm_type_to_ast_type(elem_llvm_ty);
                                    (true, false, None, elem_ty)
                                } else if let Some(ref sn) = var_info.struct_name {
                                    // Reference to struct - track the struct name
                                    (true, false, Some(sn.clone()), None)
                                } else {
                                    (true, false, None, None)
                                }
                            } else {
                                (true, false, None, None)
                            }
                        } else {
                            (true, false, None, None)
                        }
                    }
                    Expr::RefMut { operand, .. } => {
                        if let Expr::Ident(ref_name, _) = operand.as_ref() {
                            if let Some(var_info) = self.variables.get(ref_name) {
                                if var_info.ty.is_array_type() {
                                    // Mutable reference to array
                                    let array_ty = var_info.ty.into_array_type();
                                    let elem_llvm_ty = array_ty.get_element_type();
                                    let elem_ty = self.llvm_type_to_ast_type(elem_llvm_ty);
                                    (false, true, None, elem_ty)
                                } else if let Some(ref sn) = var_info.struct_name {
                                    // Mutable reference to struct
                                    (false, true, Some(sn.clone()), None)
                                } else {
                                    (false, true, None, None)
                                }
                            } else {
                                (false, true, None, None)
                            }
                        } else {
                            (false, true, None, None)
                        }
                    }
                    _ => (false, false, None, None),
                };

                // Compile expression with expected type for literal coercion
                let init_value = self.compile_expr_with_type(value, ty.as_ref())?;
                let alloca_type = match ty {
                    Some(t) => self.llvm_type(t)?,
                    None => init_value.get_type(),
                };
                let alloca = self.create_entry_block_alloca(name, alloca_type);
                self.builder.build_store(alloca, init_value).unwrap();

                // Mark source variable as moved if this was a struct move
                if let Some(src) = source_var {
                    self.moved_vars.insert(src);
                }

                // Get the AST type - either from explicit annotation or inferred from expression
                let ast_type = match ty {
                    Some(t) => Some(t.clone()),
                    None => self.get_expr_type(value).ok(),
                };

                self.variables.insert(name.clone(), VarInfo {
                    ptr: alloca,
                    ty: alloca_type,
                    struct_name,
                    ast_type,
                    is_ref,
                    is_mut_ref,
                    ref_struct_name,
                    slice_elem_type,
                });
                Ok(None)
            }
            Stmt::LetPattern { pattern, value, .. } => {
                // Compile the value expression first
                let val = self.compile_expr(value)?;

                // Handle tuple destructuring
                self.compile_pattern_binding(pattern, val, value)?;
                Ok(None)
            }
            Stmt::Return { value, .. } => {
                // Execute deferred expressions in reverse order before return
                self.emit_deferred_exprs()?;

                match value {
                    Some(expr) => {
                        // Compile return value with expected return type for proper coercion
                        let expected_type = self.current_function_return_type.clone();
                        let ret_val = self.compile_expr_with_type(expr, expected_type.as_ref())?;
                        self.builder.build_return(Some(&ret_val)).unwrap();
                    }
                    None => {
                        self.builder.build_return(None).unwrap();
                    }
                }
                Ok(None)
            }
            Stmt::Expr(expr) => {
                let val = self.compile_expr(expr)?;
                Ok(Some(val))
            }
            Stmt::If { condition, then_block, else_block, .. } => {
                self.compile_if(condition, then_block, else_block.as_ref())
            }
            Stmt::While { condition, body, .. } => {
                self.compile_while(condition, body)
            }
            Stmt::Match { value, arms, .. } => {
                self.compile_match(value, arms)
            }
            Stmt::For { name, iter, body, .. } => {
                self.compile_for(name, iter, body)
            }
            Stmt::Break { .. } => {
                if let Some(break_bb) = self.loop_break_block {
                    self.builder.build_unconditional_branch(break_bb).unwrap();
                } else {
                    return Err(CodegenError::NotImplemented("break outside of loop".to_string()));
                }
                Ok(None)
            }
            Stmt::Continue { .. } => {
                if let Some(continue_bb) = self.loop_continue_block {
                    self.builder.build_unconditional_branch(continue_bb).unwrap();
                } else {
                    return Err(CodegenError::NotImplemented("continue outside of loop".to_string()));
                }
                Ok(None)
            }
            Stmt::Defer { expr, .. } => {
                // Add expression to deferred list (will be executed at function exit)
                self.deferred_exprs.push(expr.as_ref().clone());
                Ok(None)
            }
            _ => {
                // TODO: implement other statements
                Ok(None)
            }
        }
    }

    /// Emit deferred expressions in reverse order (LIFO semantics)
    pub(crate) fn emit_deferred_exprs(&mut self) -> Result<(), CodegenError> {
        // Clone and reverse to get LIFO order
        let deferred: Vec<Expr> = self.deferred_exprs.iter().rev().cloned().collect();
        for expr in deferred {
            self.compile_expr(&expr)?;
        }
        Ok(())
    }

    pub(crate) fn compile_if(
        &mut self,
        condition: &Expr,
        then_block: &Block,
        else_block: Option<&Block>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let cond_value = self.compile_expr(condition)?;
        let cond_int = cond_value.into_int_value();

        // Ensure condition is i1 for branching
        // If it's not i1 (e.g., i64 from ptr_is_null), compare with 0
        let cond_bool = if cond_int.get_type().get_bit_width() != 1 {
            self.builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond_int,
                    cond_int.get_type().const_zero(),
                    "cond_bool",
                )
                .unwrap()
        } else {
            cond_int
        };

        let fn_value = self.current_function.unwrap();
        let then_bb = self.context.append_basic_block(fn_value, "then");
        let else_bb = self.context.append_basic_block(fn_value, "else");
        let merge_bb = self.context.append_basic_block(fn_value, "merge");

        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .unwrap();

        // Then block
        self.builder.position_at_end(then_bb);
        self.compile_block(then_block)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Else block
        self.builder.position_at_end(else_bb);
        if let Some(else_blk) = else_block {
            self.compile_block(else_blk)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Merge block
        self.builder.position_at_end(merge_bb);

        Ok(None)
    }

    pub(crate) fn compile_while(
        &mut self,
        condition: &Expr,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let cond_bb = self.context.append_basic_block(fn_value, "while.cond");
        let body_bb = self.context.append_basic_block(fn_value, "while.body");
        let end_bb = self.context.append_basic_block(fn_value, "while.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(cond_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition block
        self.builder.position_at_end(cond_bb);
        let cond_value = self.compile_expr(condition)?;
        let cond_int = cond_value.into_int_value();

        // Ensure condition is i1 for branching
        let cond_bool = if cond_int.get_type().get_bit_width() != 1 {
            self.builder
                .build_int_compare(
                    inkwell::IntPredicate::NE,
                    cond_int,
                    cond_int.get_type().const_zero(),
                    "cond_bool",
                )
                .unwrap()
        } else {
            cond_int
        };

        self.builder
            .build_conditional_branch(cond_bool, body_bb, end_bb)
            .unwrap();

        // Body block
        self.builder.position_at_end(body_bb);
        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(cond_bb).unwrap();
        }

        // End block
        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }

    pub(crate) fn compile_for(
        &mut self,
        name: &str,
        iter: &Expr,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();

        // Handle different iterator types
        match iter {
            // Range iteration: for i in 0..5
            Expr::Range { start, end, .. } => {
                let start_val = self.compile_expr(start)?.into_int_value();
                let end_val = self.compile_expr(end)?.into_int_value();

                // Create loop variable
                let i64_type = self.context.i64_type();
                let loop_var = self.create_entry_block_alloca(name, i64_type.into());
                self.builder.build_store(loop_var, start_val).unwrap();

                self.variables.insert(name.to_string(), VarInfo {
                    ptr: loop_var,
                    ty: i64_type.into(),
                    struct_name: None,
                    ast_type: Some(Type::I64),
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                    slice_elem_type: None,
                });

                let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
                let body_bb = self.context.append_basic_block(fn_value, "for.body");
                let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
                let end_bb = self.context.append_basic_block(fn_value, "for.end");

                // Save outer loop targets and set new ones for break/continue
                let outer_break = self.loop_break_block;
                let outer_continue = self.loop_continue_block;
                self.loop_break_block = Some(end_bb);
                self.loop_continue_block = Some(inc_bb);

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                // Condition: i < end
                self.builder.position_at_end(cond_bb);
                let current = self.builder.build_load(i64_type, loop_var, "i").unwrap().into_int_value();
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT, current, end_val, "cmp"
                ).unwrap();
                self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

                // Body
                self.builder.position_at_end(body_bb);
                self.compile_block(body)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(inc_bb).unwrap();
                }

                // Increment: i = i + 1
                self.builder.position_at_end(inc_bb);
                let current = self.builder.build_load(i64_type, loop_var, "i").unwrap().into_int_value();
                let next = self.builder.build_int_add(current, i64_type.const_int(1, false), "inc").unwrap();
                self.builder.build_store(loop_var, next).unwrap();
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(end_bb);

                // Restore outer loop targets
                self.loop_break_block = outer_break;
                self.loop_continue_block = outer_continue;
            }

            // Array/Slice iteration: for x in arr or for x in slice
            Expr::Ident(arr_name, _) => {
                let var_info = self.variables.get(arr_name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(arr_name.clone()))?
                    .clone();

                // Check if this is a slice (struct with {ptr, len} layout)
                if var_info.ty.is_struct_type() {
                    let struct_ty = var_info.ty.into_struct_type();
                    // Slices are structs with 2 fields: { ptr, len }
                    if struct_ty.count_fields() == 2 {
                        return self.compile_for_slice(name, &var_info, body);
                    }
                    // Vec<T> is a struct with 3 fields: { ptr, len, capacity }
                    // Check if ast_type indicates this is a Vec<T>
                    if struct_ty.count_fields() == 3 {
                        // First check explicit ast_type
                        if let Some(Type::Named { name: type_name, generics }) = &var_info.ast_type {
                            if type_name == "Vec" && generics.len() == 1 {
                                // Get element type from the generic parameter
                                let elem_ast_type = &generics[0];
                                return self.compile_for_dynamic_array(name, &var_info, elem_ast_type, body);
                            }
                        }
                        // Fallback: check struct_name for mangled Vec types (e.g., "Vec_String")
                        if let Some(ref struct_name) = var_info.struct_name {
                            if let Some((base_name, type_args)) = self.parse_mangled_name(struct_name) {
                                if base_name == "Vec" && type_args.len() == 1 {
                                    let elem_ast_type = &type_args[0];
                                    return self.compile_for_dynamic_array(name, &var_info, elem_ast_type, body);
                                }
                            }
                        }
                    }
                }

                // Check if this is a reference to an array (e.g., &i32[1024])
                if var_info.is_ref {
                    if let Some(Type::Ref(inner)) = &var_info.ast_type {
                        if let Type::Array(elem_ty, size) = inner.as_ref() {
                            return self.compile_for_ref_array(name, arr_name, &var_info, elem_ty.as_ref(), *size, body);
                        }
                    }
                    // Also check RefMut (~T)
                    if let Some(Type::RefMut(inner)) = &var_info.ast_type {
                        if let Type::Array(elem_ty, size) = inner.as_ref() {
                            return self.compile_for_ref_array(name, arr_name, &var_info, elem_ty.as_ref(), *size, body);
                        }
                    }
                    return Err(CodegenError::NotImplemented(format!(
                        "cannot iterate over reference '{}'. Only references to arrays are iterable",
                        arr_name
                    )));
                }

                if !var_info.ty.is_array_type() {
                    return Err(CodegenError::NotImplemented(format!(
                        "variable '{}' is not iterable. For loops require arrays, slices, or ranges (e.g., '0..10')",
                        arr_name
                    )));
                }

                let array_ty = var_info.ty.into_array_type();
                let len = array_ty.len();
                let elem_type = array_ty.get_element_type();

                // Create index variable
                let i64_type = self.context.i64_type();
                let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
                self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

                // Create element variable
                let elem_var = self.create_entry_block_alloca(name, elem_type);
                self.variables.insert(name.to_string(), VarInfo {
                    ptr: elem_var,
                    ty: elem_type,
                    struct_name: None,
                    ast_type: self.llvm_type_to_ast_type(elem_type),
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                    slice_elem_type: None,
                });

                let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
                let body_bb = self.context.append_basic_block(fn_value, "for.body");
                let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
                let end_bb = self.context.append_basic_block(fn_value, "for.end");

                // Save outer loop targets and set new ones for break/continue
                let outer_break = self.loop_break_block;
                let outer_continue = self.loop_continue_block;
                self.loop_break_block = Some(end_bb);
                self.loop_continue_block = Some(inc_bb);

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                // Condition: idx < len
                self.builder.position_at_end(cond_bb);
                let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
                let len_val = i64_type.const_int(len as u64, false);
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
                ).unwrap();
                self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

                // Body - load current element
                self.builder.position_at_end(body_bb);
                let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        var_info.ty,
                        var_info.ptr,
                        &[self.context.i32_type().const_zero(), current_idx],
                        "elem_ptr"
                    ).unwrap()
                };
                let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
                self.builder.build_store(elem_var, elem_val).unwrap();

                self.compile_block(body)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(inc_bb).unwrap();
                }

                // Increment index
                self.builder.position_at_end(inc_bb);
                let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
                let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
                self.builder.build_store(idx_var, next_idx).unwrap();
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(end_bb);

                // Restore outer loop targets
                self.loop_break_block = outer_break;
                self.loop_continue_block = outer_continue;
            }

            _ => return Err(CodegenError::NotImplemented(
                "for loop only supports arrays, slices, and ranges (e.g., '0..10'). \
                 Got an unsupported iterator type".to_string()
            )),
        }

        Ok(None)
    }

    pub(crate) fn compile_for_slice(
        &mut self,
        name: &str,
        var_info: &VarInfo<'ctx>,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let struct_ty = var_info.ty.into_struct_type();
        let i64_type = self.context.i64_type();

        // Load the slice value
        let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();

        // Extract pointer (field 0) and length (field 1) from slice
        let data_ptr = self.builder
            .build_extract_value(slice_val.into_struct_value(), 0, "slice_ptr")
            .unwrap()
            .into_pointer_value();
        let len_val = self.builder
            .build_extract_value(slice_val.into_struct_value(), 1, "slice_len")
            .unwrap()
            .into_int_value();

        // Get element type from tracked slice_elem_type, fallback to i64
        let elem_type: BasicTypeEnum = if let Some(ref ast_ty) = var_info.slice_elem_type {
            self.llvm_type(ast_ty)?
        } else {
            i64_type.into()
        };

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(name, elem_type);
        self.variables.insert(name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: None,
            ast_type: var_info.slice_elem_type.clone(),
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
            slice_elem_type: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(inc_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition: idx < len
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
        ).unwrap();
        self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

        // Body - load current element from slice
        self.builder.position_at_end(body_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let elem_ptr = unsafe {
            self.builder.build_gep(
                elem_type,
                data_ptr,
                &[current_idx],
                "slice_elem_ptr"
            ).unwrap()
        };
        let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
        self.builder.build_store(elem_var, elem_val).unwrap();

        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment index
        self.builder.position_at_end(inc_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
        self.builder.build_store(idx_var, next_idx).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }

    /// Compile for loop over a dynamic Vec<T> (struct with {ptr, len, capacity})
    pub(crate) fn compile_for_dynamic_array(
        &mut self,
        name: &str,
        var_info: &VarInfo<'ctx>,
        elem_ast_type: &Type,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let struct_ty = var_info.ty.into_struct_type();
        let i64_type = self.context.i64_type();

        // Load the Vec<T> value
        let array_val = self.builder.build_load(struct_ty, var_info.ptr, "array").unwrap();

        // Extract pointer (field 0) and length (field 1) from Vec<T>
        let data_ptr = self.builder
            .build_extract_value(array_val.into_struct_value(), 0, "array_ptr")
            .unwrap()
            .into_pointer_value();
        let len_val = self.builder
            .build_extract_value(array_val.into_struct_value(), 1, "array_len")
            .unwrap()
            .into_int_value();

        // Get element type from AST type
        let elem_type: BasicTypeEnum = self.llvm_type(elem_ast_type)?;

        // Get struct name for element if it's a struct type
        let elem_struct_name = self.get_struct_name_for_type(elem_ast_type);

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(name, elem_type);
        self.variables.insert(name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: elem_struct_name,
            ast_type: Some(elem_ast_type.clone()),
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
            slice_elem_type: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(inc_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition: idx < len
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
        ).unwrap();
        self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

        // Body - load current element from array
        self.builder.position_at_end(body_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let elem_ptr = unsafe {
            self.builder.build_gep(
                elem_type,
                data_ptr,
                &[current_idx],
                "array_elem_ptr"
            ).unwrap()
        };
        let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
        self.builder.build_store(elem_var, elem_val).unwrap();

        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment index
        self.builder.position_at_end(inc_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
        self.builder.build_store(idx_var, next_idx).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }

    /// Compile for loop over a reference to an array (e.g., for x in arr where arr: &i32[1024])
    pub(crate) fn compile_for_ref_array(
        &mut self,
        loop_var_name: &str,
        arr_name: &str,
        var_info: &VarInfo<'ctx>,
        elem_ty: &Type,
        array_len: usize,
    body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let i64_type = self.context.i64_type();

        // Get the LLVM type for elements
        let elem_type = self.llvm_type(elem_ty)?;

        // Get the array type for GEP - need to match on elem_type to get concrete type
        let array_type = match elem_type {
            BasicTypeEnum::IntType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::FloatType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::PointerType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::StructType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::ArrayType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::VectorType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::ScalableVectorType(t) => t.array_type(array_len as u32),
        };

        // Dereference the pointer to get the array pointer
        // var_info.ptr points to the storage of the reference (ptr*)
        // We need to load it to get the actual array pointer
        let array_ptr = self.builder
            .build_load(self.context.ptr_type(inkwell::AddressSpace::default()), var_info.ptr, "deref_arr")
            .unwrap()
            .into_pointer_value();

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(loop_var_name, elem_type);
        self.variables.insert(loop_var_name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: None,
            ast_type: Some(elem_ty.clone()),
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
            slice_elem_type: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(inc_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition: idx < len
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let len_val = i64_type.const_int(array_len as u64, false);
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
        ).unwrap();
        self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

        // Body - load current element from array
        self.builder.position_at_end(body_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let elem_ptr = unsafe {
            self.builder.build_gep(
                array_type,
                array_ptr,
                &[self.context.i32_type().const_zero(), current_idx],
                "elem_ptr"
            ).unwrap()
        };
        let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
        self.builder.build_store(elem_var, elem_val).unwrap();

        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment index
        self.builder.position_at_end(inc_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
        self.builder.build_store(idx_var, next_idx).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }

    /// Compile pattern binding for let destructuring
    fn compile_pattern_binding(
        &mut self,
        pattern: &Pattern,
        value: BasicValueEnum<'ctx>,
        value_expr: &Expr,
    ) -> Result<(), CodegenError> {
        match pattern {
            Pattern::Tuple(patterns) => {
                // Get the tuple type from the value
                let tuple_type = value.get_type();

                if !tuple_type.is_struct_type() {
                    return Err(CodegenError::NotImplemented(
                        "tuple destructuring requires a tuple/struct value".to_string()
                    ));
                }

                let struct_type = tuple_type.into_struct_type();

                // Infer element types from the value expression if it's a tuple
                let elem_types: Vec<Option<Type>> = if let Expr::Tuple { elements, .. } = value_expr {
                    elements.iter().map(|_| None).collect()
                } else {
                    vec![None; patterns.len()]
                };

                // Store the tuple to get a pointer for GEP
                let alloca = self.builder.build_alloca(struct_type, "tuple_destructure").unwrap();
                self.builder.build_store(alloca, value).unwrap();

                // Extract each element and bind to pattern
                for (i, pat) in patterns.iter().enumerate() {
                    let field_ptr = self.builder
                        .build_struct_gep(struct_type, alloca, i as u32, &format!("tuple.{}", i))
                        .unwrap();

                    let field_type = struct_type.get_field_type_at_index(i as u32)
                        .ok_or_else(|| CodegenError::UndefinedField(format!("tuple index {}", i)))?;

                    let field_val = self.builder
                        .build_load(field_type, field_ptr, &format!("elem{}", i))
                        .unwrap();

                    // Recursively bind the pattern
                    match pat {
                        Pattern::Ident(name) => {
                            // Create a variable for this binding
                            let var_alloca = self.create_entry_block_alloca(name, field_type);
                            self.builder.build_store(var_alloca, field_val).unwrap();

                            // Get AST type if available
                            let ast_type = elem_types.get(i).cloned().flatten();

                            self.variables.insert(name.clone(), VarInfo {
                                ptr: var_alloca,
                                ty: field_type,
                                struct_name: None,
                                ast_type,
                                is_ref: false,
                                is_mut_ref: false,
                                ref_struct_name: None,
                                slice_elem_type: None,
                            });
                        }
                        Pattern::Wildcard => {
                            // Ignore this element
                        }
                        Pattern::Tuple(_) => {
                            // Nested tuple destructuring - create a dummy expr for recursion
                            let dummy_expr = Expr::Tuple {
                                elements: vec![],
                                span: crate::lexer::Span { start: 0, end: 0, line: 0, column: 0 },
                            };
                            self.compile_pattern_binding(pat, field_val, &dummy_expr)?;
                        }
                        _ => {
                            return Err(CodegenError::NotImplemented(
                                format!("pattern type {:?} in tuple destructuring", pat)
                            ));
                        }
                    }
                }

                Ok(())
            }
            Pattern::Ident(name) => {
                // Simple binding
                let val_type = value.get_type();
                let alloca = self.create_entry_block_alloca(name, val_type);
                self.builder.build_store(alloca, value).unwrap();

                self.variables.insert(name.clone(), VarInfo {
                    ptr: alloca,
                    ty: val_type,
                    struct_name: None,
                    ast_type: None,
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                    slice_elem_type: None,
                });

                Ok(())
            }
            Pattern::Wildcard => Ok(()),
            _ => Err(CodegenError::NotImplemented(
                format!("pattern type {:?} in let binding", pattern)
            )),
        }
    }
}
