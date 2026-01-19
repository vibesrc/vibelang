//! Expression compilation
//!
//! This module handles compilation of all expression types to LLVM IR.
//! It's split into submodules for better organization:
//!
//! - `binary`: Binary and unary operations, logical and/or
//! - `literal`: Literal value compilation
//! - `cast`: Type casts and tuple expressions
//! - `closure`: Closure compilation and function-to-closure coercion
//! - `string`: String interpolation and type-to-string conversion

mod binary;
mod cast;
mod closure;
mod literal;
mod string;

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_expr_with_type(expr, None)
    }

    /// Compile an expression with an optional expected type for literal coercion
    pub(crate) fn compile_expr_with_type(&mut self, expr: &Expr, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match expr {
            Expr::Literal(lit, _) => self.compile_literal_with_type(lit, expected_type),
            Expr::Ident(name, _) => {
                // Check local variables first
                if let Some(var_info) = self.variables.get(name) {
                    let val = self.builder.build_load(var_info.ty, var_info.ptr, name).unwrap();
                    return Ok(val);
                }
                // Check static (global) variables
                if let Some(static_ptr) = self.static_vars.get(name) {
                    // Get the type from the global value
                    let global = self.module.get_global(name)
                        .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                    let any_ty = global.get_value_type();
                    // Convert AnyTypeEnum to BasicTypeEnum
                    let ty: inkwell::types::BasicTypeEnum = match any_ty {
                        inkwell::types::AnyTypeEnum::IntType(t) => t.into(),
                        inkwell::types::AnyTypeEnum::FloatType(t) => t.into(),
                        inkwell::types::AnyTypeEnum::StructType(t) => t.into(),
                        inkwell::types::AnyTypeEnum::PointerType(t) => t.into(),
                        inkwell::types::AnyTypeEnum::ArrayType(t) => t.into(),
                        _ => return Err(CodegenError::NotImplemented(
                            format!("unsupported static variable type for '{}'", name)
                        )),
                    };
                    let val = self.builder.build_load(ty, *static_ptr, name).unwrap();
                    return Ok(val);
                }
                // Check if this is a named function - if so, coerce to closure fat pointer
                if let Some(func) = self.module.get_function(name) {
                    return self.coerce_function_to_closure(name, func);
                }
                Err(CodegenError::UndefinedVariable(name.clone()))
            }
            Expr::Binary { op, left, right, .. } => {
                self.compile_binary(*op, left, right)
            }
            Expr::Unary { op, operand, .. } => {
                self.compile_unary_with_type(*op, operand, expected_type)
            }
            Expr::Call { func, type_args, args, .. } => {
                self.compile_call(func, type_args, args)
            }
            Expr::StructInit { name, generics, fields, .. } => {
                self.compile_struct_init(name, generics, fields)
            }
            Expr::Field { object, field, .. } => {
                self.compile_field_access(object, field)
            }
            Expr::MethodCall { receiver, method, args, .. } => {
                // Check for generic struct static method call with explicit type args: Vec<u8>.new()
                if let Expr::StructInit { name, generics, fields, .. } = receiver.as_ref() {
                    if fields.is_empty() && !generics.is_empty() {
                        // This is a generic type with explicit type args
                        // First check if it's a generic enum
                        if self.generic_enums.contains_key(name) {
                            let mono_name = self.ensure_monomorphized_enum(name, generics)?;
                            let enum_info = self.enum_types.get(&mono_name).cloned()
                                .ok_or_else(|| CodegenError::UndefinedType(mono_name.clone()))?;
                            return self.compile_enum_variant_constructor(&enum_info, &mono_name, method, args);
                        }
                        // Check if it's a generic struct
                        if self.generic_structs.contains_key(name) {
                            let mono_name = self.ensure_monomorphized_struct(name, generics)?;
                            // Ensure impl methods are also monomorphized
                            self.ensure_monomorphized_impl(name, generics)?;
                            return self.compile_static_method_call(&mono_name, method, args);
                        }
                    }
                }

                // Check if this is an enum variant constructor (e.g., Option.Some(42))
                if let Expr::Ident(name, _) = receiver.as_ref() {
                    // Check concrete enum first
                    if let Some(enum_info) = self.enum_types.get(name).cloned() {
                        return self.compile_enum_variant_constructor(&enum_info, name, method, args);
                    }

                    // Check if this is a generic enum - infer type from arguments
                    if let Some(generic_enum) = self.generic_enums.get(name).cloned() {
                        // Find the variant being constructed
                        if let Some(variant) = generic_enum.variants.iter().find(|v| v.name == *method) {
                            // Infer type arguments from constructor arguments
                            if let Some(inferred_types) = self.infer_enum_type_args(&generic_enum, variant, args)? {
                                // Monomorphize with inferred types
                                let mono_name = self.ensure_monomorphized_enum(name, &inferred_types)?;
                                let enum_info = self.enum_types.get(&mono_name).cloned()
                                    .ok_or_else(|| CodegenError::UndefinedType(mono_name.clone()))?;
                                return self.compile_enum_variant_constructor(&enum_info, &mono_name, method, args);
                            }
                        }
                    }

                    // Check if this is a static method call on a struct type (e.g., IntVec.new())
                    if self.struct_types.contains_key(name) || self.type_methods.contains_key(name) {
                        return self.compile_static_method_call(name, method, args);
                    }

                    // Check if this is a static method call on a generic struct (e.g., Vec.new())
                    // For generic structs without explicit type args, we need to infer the type
                    // from context or use a default monomorphization
                    if self.generic_structs.contains_key(name) {
                        // Try to infer type arguments from function return type expectation
                        // For now, we require explicit type args for generic structs
                        return Err(CodegenError::NotImplemented(format!(
                            "static method call on generic struct '{}' requires explicit type args (e.g., {}<T>.{}())",
                            name, name, method
                        )));
                    }

                    // Check for array .len() method
                    if method == "len" && args.is_empty() {
                        if let Some(var_info) = self.variables.get(name) {
                            if var_info.ty.is_array_type() {
                                let array_ty = var_info.ty.into_array_type();
                                let len = array_ty.len();
                                return Ok(self.context.i64_type().const_int(len as u64, false).into());
                            }
                            // Check for slice .len() method
                            if var_info.ty.is_struct_type() {
                                let struct_ty = var_info.ty.into_struct_type();
                                // Slices are structs with 2 fields: { ptr, len }
                                if struct_ty.count_fields() == 2 {
                                    // Load the slice value
                                    let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();
                                    // Extract length from slice (field 1)
                                    let len_val = self.builder
                                        .build_extract_value(slice_val.into_struct_value(), 1, "slice_len")
                                        .unwrap();
                                    return Ok(len_val);
                                }
                            }
                        }
                    }
                }
                // Try to resolve as a method call on a type
                self.compile_method_call(receiver, method, args)
            }
            Expr::Ref { operand, .. } => {
                // &expr - get address of the operand
                self.compile_ref(operand, false)
            }
            Expr::RefMut { operand, .. } => {
                // ~expr - get mutable address of the operand
                self.compile_ref(operand, true)
            }
            Expr::Deref { operand, .. } => {
                // *expr - dereference a pointer
                self.compile_deref(operand)
            }
            Expr::ArrayInit { elements, .. } => {
                // Extract element type from expected array type if available
                let elem_type = match expected_type {
                    Some(Type::Array(inner, _)) => Some(inner.as_ref()),
                    _ => None,
                };
                self.compile_array_init_with_type(elements, elem_type)
            }
            Expr::ArrayRepeat { value, count, .. } => {
                // [val; count] - create array with count copies of val
                let elem_type = match expected_type {
                    Some(Type::Array(inner, _)) => Some(inner.as_ref()),
                    _ => None,
                };
                self.compile_array_repeat(value, *count, elem_type)
            }
            Expr::Index { array, index, .. } => {
                self.compile_index(array, index)
            }
            Expr::Try { operand, .. } => {
                self.compile_try_operator(operand)
            }
            Expr::Block(block) => {
                // Compile all statements in the block
                self.compile_block(block)?;
                // Return a dummy value for blocks (the last statement's value)
                Ok(self.context.i64_type().const_zero().into())
            }
            Expr::InterpolatedString { parts, .. } => {
                self.compile_interpolated_string(parts)
            }
            Expr::Unsafe { block, .. } => {
                // Compile unsafe block - same as regular block but marks unsafe context
                let was_in_unsafe = self.in_unsafe;
                self.in_unsafe = true;
                let result = self.compile_block(block)?;
                self.in_unsafe = was_in_unsafe;
                // Return the block's result, or a dummy value if no result
                Ok(result.unwrap_or_else(|| self.context.i64_type().const_zero().into()))
            }
            Expr::Cast { expr, ty, .. } => {
                self.compile_cast(expr, ty)
            }
            Expr::Tuple { elements, .. } => {
                self.compile_tuple(elements, expected_type)
            }
            Expr::Closure { params, return_type, body, .. } => {
                self.compile_closure(params, return_type.as_ref(), body)
            }
            other => {
                Err(CodegenError::NotImplemented(format!(
                    "unsupported expression type: {:?}. This expression may not be implemented yet", other
                )))
            }
        }
    }
}
