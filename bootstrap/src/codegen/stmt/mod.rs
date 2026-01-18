//! Statement compilation
//!
//! This module handles compilation of statements:
//!
//! - `control`: if and while statements
//! - `loops`: for loop variants (range, array, slice, Vec)
//! - `pattern_binding`: pattern destructuring in let bindings

mod control;
mod loops;
mod pattern_binding;

use super::{Codegen, CodegenError, VarInfo};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

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
                // Track struct names for method call resolution
                let struct_name = match value {
                    Expr::StructInit { name: sname, generics, .. } => {
                        // For generic structs, use mangled name
                        let mono_name = if generics.is_empty() {
                            sname.clone()
                        } else {
                            self.mangle_name(sname, generics)
                        };
                        Some(mono_name)
                    }
                    Expr::Ident(src_name, _) => {
                        // Check if source variable is a struct type (moves on assignment)
                        if let Some(var_info) = self.variables.get(src_name) {
                            if var_info.struct_name.is_some() && !var_info.is_ref {
                                // This is a move - mark source as moved
                                var_info.struct_name.clone()
                            } else {
                                None
                            }
                        } else {
                            None
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
                                struct_name
                            } else if self.enum_types.contains_key(type_name) {
                                // Enum variant constructor - the result is the enum type
                                Some(type_name.clone())
                            } else if let Some(generic_enum) = self.generic_enums.get(type_name).cloned() {
                                // Generic enum variant constructor - infer type args to get mangled name
                                if let Some(variant) = generic_enum.variants.iter().find(|v| v.name == *method) {
                                    if let Ok(Some(inferred_types)) = self.infer_enum_type_args(&generic_enum, variant, args) {
                                        let mangled = self.mangle_name(type_name, &inferred_types);
                                        Some(mangled)
                                    } else {
                                        ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                    }
                                } else {
                                    ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
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
                                            struct_name
                                        } else {
                                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                        }
                                    } else {
                                        ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
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
                                            struct_name
                                        } else {
                                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                        }
                                    } else {
                                        ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                    }
                                } else {
                                    ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                }
                            } else {
                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
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
                                            struct_name
                                        } else {
                                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                        }
                                    } else {
                                        ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                    }
                                } else {
                                    ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                }
                            } else {
                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                            }
                        } else if let Expr::StructInit { name: type_name, generics, fields, .. } = receiver.as_ref() {
                            // Handle Vec<u8>.new() style calls - receiver is a generic struct init with no fields
                            if fields.is_empty() && !generics.is_empty() {
                                // Check if it's a generic struct (e.g., Vec<u8>)
                                if self.generic_structs.contains_key(type_name) || self.generic_enums.contains_key(type_name) {
                                    // Monomorphize and return the mangled name
                                    Some(self.mangle_name(type_name, generics))
                                } else {
                                    ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                }
                            } else {
                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                            }
                        } else {
                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
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
                                            struct_name
                                        } else {
                                            // Fallback: assume it returns Self type
                                            Some(mono_name)
                                        }
                                    } else {
                                        // Methods not yet compiled, assume returns Self
                                        Some(mono_name)
                                    }
                                } else {
                                    ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
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
                                                struct_name
                                            } else {
                                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                            }
                                        } else {
                                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                        }
                                    } else {
                                        ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                    }
                                } else {
                                    ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                                }
                            } else {
                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                            }
                        } else if let Expr::Ident(fn_name, _) = func.as_ref() {
                            // First check explicit type annotation
                            if let Some(t) = ty.as_ref() {
                                self.get_struct_name_for_type(t)
                            } else if fn_name == "ptr_read" && !type_args.is_empty() {
                                // ptr_read<T>() returns T - extract struct name from type arg
                                self.get_struct_name_for_type(&type_args[0])
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
                                        self.get_struct_name_for_type(&substituted)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                // Infer from function return type
                                let struct_name = self.function_return_types
                                    .get(fn_name)
                                    .and_then(|ret_ty| ret_ty.as_ref())
                                    .and_then(|ret_ty| self.get_struct_name_for_type(ret_ty));
                                struct_name
                            }
                        } else {
                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
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
                        inner_struct_name
                    }
                    // Handle field access for unit enum variants like Color.Red or Option<i32>.None
                    Expr::Field { object, .. } => {
                        if let Expr::Ident(type_name, _) = object.as_ref() {
                            // Check if object is an enum type (unit variant constructor)
                            if self.enum_types.contains_key(type_name) {
                                Some(type_name.clone())
                            } else {
                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                            }
                        } else if let Expr::StructInit { name, generics, fields, .. } = object.as_ref() {
                            // Handle generic enum unit variant like Option<i32>.None
                            if fields.is_empty() && !generics.is_empty() {
                                let mono_name = self.mangle_name(name, generics);
                                Some(mono_name)
                            } else {
                                ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                            }
                        } else {
                            ty.as_ref().and_then(|t| self.get_struct_name_for_type(t))
                        }
                    }
                    _ => ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)),
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
}
