//! Pattern binding compilation for let destructuring

use super::{Codegen, CodegenError, VarInfo};
use crate::ast::*;
use crate::lexer::Span;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    /// Compile pattern binding for let destructuring
    pub(crate) fn compile_pattern_binding(
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
                                span: Span { start: 0, end: 0, line: 0, column: 0 },
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
            Pattern::Struct { fields, .. } => {
                // Get the struct name from the expression
                let struct_name = self.get_struct_name_from_expr(value_expr)
                    .ok_or_else(|| CodegenError::NotImplemented(
                        "struct destructuring requires a known struct type".to_string()
                    ))?;

                let struct_info = self.struct_types.get(&struct_name)
                    .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                    .clone();

                // Store the struct value to get a pointer for GEP
                let alloca = self.builder.build_alloca(struct_info.llvm_type, "struct_destructure").unwrap();
                self.builder.build_store(alloca, value).unwrap();

                // For each field in the pattern, extract the value
                for (field_name, pattern) in fields {
                    let field_idx = *struct_info.field_indices.get(field_name)
                        .ok_or_else(|| CodegenError::UndefinedField(field_name.clone()))?;

                    let field_ptr = self.builder
                        .build_struct_gep(struct_info.llvm_type, alloca, field_idx, &format!("field.{}", field_name))
                        .unwrap();

                    let field_type = struct_info.llvm_type.get_field_type_at_index(field_idx)
                        .ok_or_else(|| CodegenError::UndefinedField(field_name.clone()))?;

                    let field_val = self.builder
                        .build_load(field_type, field_ptr, field_name)
                        .unwrap();

                    // Get the AST type for this field if available
                    let field_ast_type = struct_info.ast_field_types.get(field_idx as usize).cloned();
                    let field_struct_name = field_ast_type.as_ref()
                        .and_then(|t| self.get_struct_name_for_type(t));

                    match pattern {
                        Pattern::Ident(var_name) => {
                            let var_alloca = self.create_entry_block_alloca(var_name, field_type);
                            self.builder.build_store(var_alloca, field_val).unwrap();

                            self.variables.insert(var_name.clone(), VarInfo {
                                ptr: var_alloca,
                                ty: field_type,
                                struct_name: field_struct_name,
                                ast_type: field_ast_type,
                                is_ref: false,
                                is_mut_ref: false,
                                ref_struct_name: None,
                                slice_elem_type: None,
                            });
                        }
                        Pattern::Wildcard => {
                            // Ignore this field
                        }
                        _ => {
                            // Nested pattern - recurse
                            let dummy_expr = Expr::Ident(
                                field_name.clone(),
                                Span { start: 0, end: 0, line: 0, column: 0 },
                            );
                            self.compile_pattern_binding(pattern, field_val, &dummy_expr)?;
                        }
                    }
                }

                Ok(())
            }
            Pattern::Wildcard => Ok(()),
            _ => Err(CodegenError::NotImplemented(
                format!("pattern type {:?} in let binding", pattern)
            )),
        }
    }

    /// Get the struct name from an expression (used for destructuring)
    pub(crate) fn get_struct_name_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name, _) => {
                self.variables.get(name)
                    .and_then(|info| info.struct_name.clone())
            }
            Expr::Field { object, field, .. } => {
                // Get the type of the field
                let struct_name = self.get_struct_name_from_expr(object)?;
                let struct_info = self.struct_types.get(&struct_name)?;
                let field_idx = *struct_info.field_indices.get(field)?;
                struct_info.ast_field_types.get(field_idx as usize)
                    .and_then(|t| self.get_struct_name_for_type(t))
            }
            Expr::MethodCall { receiver, method, .. } => {
                // For method calls, try to get the return type
                let recv_struct = self.get_struct_name_from_expr(receiver)?;
                let full_name = format!("{}_{}", recv_struct, method);
                self.function_return_types.get(&full_name)
                    .and_then(|opt_t| opt_t.as_ref())
                    .and_then(|t| self.get_struct_name_for_type(t))
            }
            Expr::Call { func, .. } => {
                // For function calls, try to get the return type
                if let Expr::Ident(name, _) = func.as_ref() {
                    self.function_return_types.get(name)
                        .and_then(|opt_t| opt_t.as_ref())
                        .and_then(|t| self.get_struct_name_for_type(t))
                } else {
                    None
                }
            }
            Expr::StructInit { name, .. } => Some(name.clone()),
            _ => None,
        }
    }
}
