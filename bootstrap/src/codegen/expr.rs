//! Expression compilation

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_expr_with_type(expr, None)
    }

    /// Compile an expression with an optional expected type for literal coercion
    pub(crate) fn compile_expr_with_type(&mut self, expr: &Expr, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match expr {
            Expr::Literal(lit, _) => self.compile_literal_with_type(lit, expected_type),
            Expr::Ident(name, _) => {
                // Check for use-after-move
                if self.moved_vars.contains(name) {
                    return Err(CodegenError::BorrowError(
                        format!("use of moved value: '{}'", name)
                    ));
                }
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

    pub(crate) fn compile_literal(&mut self, lit: &Literal) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_literal_with_type(lit, None)
    }

    /// Compile a literal with an optional expected type for coercion
    pub(crate) fn compile_literal_with_type(&mut self, lit: &Literal, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        use crate::ast::IntSuffix;

        match lit {
            Literal::Int(n, suffix) => {
                // Priority: explicit suffix > expected type > default (i32)
                let int_type = match suffix {
                    IntSuffix::I8 => self.context.i8_type(),
                    IntSuffix::I16 => self.context.i16_type(),
                    IntSuffix::I32 => self.context.i32_type(),
                    IntSuffix::I64 => self.context.i64_type(),
                    IntSuffix::U8 => self.context.i8_type(),
                    IntSuffix::U16 => self.context.i16_type(),
                    IntSuffix::U32 => self.context.i32_type(),
                    IntSuffix::U64 => self.context.i64_type(),
                    IntSuffix::None => {
                        // No suffix - use expected type or default to i32
                        match expected_type {
                            Some(Type::I8) => self.context.i8_type(),
                            Some(Type::I16) => self.context.i16_type(),
                            Some(Type::I32) => self.context.i32_type(),
                            Some(Type::I64) => self.context.i64_type(),
                            Some(Type::U8) => self.context.i8_type(),
                            Some(Type::U16) => self.context.i16_type(),
                            Some(Type::U32) => self.context.i32_type(),
                            Some(Type::U64) => self.context.i64_type(),
                            _ => self.context.i32_type(), // Default to i32
                        }
                    }
                };
                let val = int_type.const_int(*n as u64, true);
                Ok(val.into())
            }
            Literal::Float(n) => {
                let val: BasicValueEnum<'ctx> = match expected_type {
                    Some(Type::F32) => self.context.f32_type().const_float(*n).into(),
                    _ => self.context.f64_type().const_float(*n).into(), // Default to f64
                };
                Ok(val)
            }
            Literal::Bool(b) => {
                let val = self.context.bool_type().const_int(*b as u64, false);
                Ok(val.into())
            }
            Literal::Char(c) => {
                // char is u8
                let val = self.context.i8_type().const_int(*c as u64, false);
                Ok(val.into())
            }
            Literal::String(s) => {
                // Create string as Slice<u8> (fat pointer: {ptr, len})
                let global = self.builder.build_global_string_ptr(s, "str").unwrap();
                let ptr = global.as_pointer_value();
                let len = self.context.i64_type().const_int(s.len() as u64, false);

                // Try to use the user-defined Slice_u8 struct type
                let slice_type = if let Some(struct_info) = self.struct_types.get("Slice_u8") {
                    struct_info.llvm_type
                } else {
                    // Fallback: create anonymous struct type (for bootstrapping)
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let len_type = self.context.i64_type();
                    self.context.struct_type(&[ptr_type.into(), len_type.into()], false)
                };

                // Build the slice value
                let slice_val = slice_type.const_named_struct(&[ptr.into(), len.into()]);
                Ok(slice_val.into())
            }
        }
    }

    pub(crate) fn compile_binary(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Handle logical AND with short-circuit evaluation
        if matches!(op, BinOp::And) {
            return self.compile_logical_and(left, right);
        }

        // Handle logical OR with short-circuit evaluation
        if matches!(op, BinOp::Or) {
            return self.compile_logical_or(left, right);
        }

        // Handle assignment specially - don't compile LHS yet for assignments
        if matches!(op, BinOp::Assign) {
            let rhs = self.compile_expr(right)?;

            if let Expr::Ident(name, _) = left {
                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                self.builder.build_store(var_info.ptr, rhs).unwrap();
                return Ok(rhs);
            }

            // Handle field assignment (e.g., p.x = val)
            if let Expr::Field { object, field, .. } = left {
                let field_ptr = self.compile_field_ptr(object, field)?;
                self.builder.build_store(field_ptr, rhs).unwrap();
                return Ok(rhs);
            }

            // Handle dereference assignment (e.g., *ptr = val, *(ptr + offset) = val)
            if let Expr::Deref { operand, .. } = left {
                return self.compile_deref_assign(operand, rhs);
            }

            // Handle array index assignment (e.g., arr[i] = val)
            if let Expr::Index { array, index, .. } = left {
                let (elem_ptr, elem_type) = self.compile_index_ptr(array, index)?;

                // Coerce the value to the element type if needed
                let coerced_rhs = if rhs.is_int_value() && elem_type.is_int_type() {
                    let rhs_int = rhs.into_int_value();
                    let elem_int_type = elem_type.into_int_type();
                    let rhs_width = rhs_int.get_type().get_bit_width();
                    let elem_width = elem_int_type.get_bit_width();

                    if rhs_width > elem_width {
                        // Truncate (e.g., i64 -> i32)
                        self.builder.build_int_truncate(rhs_int, elem_int_type, "trunc").unwrap().into()
                    } else if rhs_width < elem_width {
                        // Sign extend (e.g., i32 -> i64)
                        self.builder.build_int_s_extend(rhs_int, elem_int_type, "sext").unwrap().into()
                    } else {
                        rhs
                    }
                } else {
                    rhs
                };

                self.builder.build_store(elem_ptr, coerced_rhs).unwrap();
                return Ok(coerced_rhs);
            }

            return Err(CodegenError::InvalidAssignment);
        }

        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        // Handle pointer operations (comparison and arithmetic)
        if lhs.is_pointer_value() || rhs.is_pointer_value() {
            // Pointer arithmetic: ptr + int, ptr - int
            if lhs.is_pointer_value() && rhs.is_int_value() {
                let ptr = lhs.into_pointer_value();
                let offset = rhs.into_int_value();

                // Extend offset to i64 if needed
                let offset_i64 = if offset.get_type().get_bit_width() < 64 {
                    self.builder.build_int_s_extend(offset, self.context.i64_type(), "offset_ext").unwrap()
                } else {
                    offset
                };

                match op {
                    BinOp::Add => {
                        // Use GEP for pointer arithmetic (type-aware)
                        // Note: LLVM opaque pointers use i8 as the element type for byte offsets
                        let result = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                ptr,
                                &[offset_i64],
                                "ptr_add"
                            ).unwrap()
                        };
                        return Ok(result.into());
                    }
                    BinOp::Sub => {
                        // Subtract: ptr - offset = ptr + (-offset)
                        let neg_offset = self.builder.build_int_neg(offset_i64, "neg_offset").unwrap();
                        let result = unsafe {
                            self.builder.build_gep(
                                self.context.i8_type(),
                                ptr,
                                &[neg_offset],
                                "ptr_sub"
                            ).unwrap()
                        };
                        return Ok(result.into());
                    }
                    _ => {}
                }
            }

            // Pointer comparisons
            let lhs_ptr = if lhs.is_pointer_value() {
                lhs.into_pointer_value()
            } else {
                // Convert int to null pointer for comparison
                self.context.ptr_type(AddressSpace::default()).const_null()
            };
            let rhs_ptr = if rhs.is_pointer_value() {
                rhs.into_pointer_value()
            } else {
                // Convert int to null pointer for comparison
                self.context.ptr_type(AddressSpace::default()).const_null()
            };

            let result = match op {
                BinOp::Eq => self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    self.builder.build_ptr_to_int(lhs_ptr, self.context.i64_type(), "lhs_int").unwrap(),
                    self.builder.build_ptr_to_int(rhs_ptr, self.context.i64_type(), "rhs_int").unwrap(),
                    "eq"
                ).unwrap(),
                BinOp::Ne => self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    self.builder.build_ptr_to_int(lhs_ptr, self.context.i64_type(), "lhs_int").unwrap(),
                    self.builder.build_ptr_to_int(rhs_ptr, self.context.i64_type(), "rhs_int").unwrap(),
                    "ne"
                ).unwrap(),
                _ => return Err(CodegenError::NotImplemented(format!(
                    "pointer operations only support +, -, ==, and !=; got '{:?}'", op
                ))),
            };

            return Ok(result.into());
        }

        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

        // Coerce integer types to the same width for binary operations
        let (lhs_int, rhs_int) = if lhs_int.get_type().get_bit_width() != rhs_int.get_type().get_bit_width() {
            let lhs_width = lhs_int.get_type().get_bit_width();
            let rhs_width = rhs_int.get_type().get_bit_width();
            if lhs_width > rhs_width {
                // Widen rhs to match lhs
                let widened = self.builder.build_int_s_extend(rhs_int, lhs_int.get_type(), "widen").unwrap();
                (lhs_int, widened)
            } else {
                // Widen lhs to match rhs
                let widened = self.builder.build_int_s_extend(lhs_int, rhs_int.get_type(), "widen").unwrap();
                (widened, rhs_int)
            }
        } else {
            (lhs_int, rhs_int)
        };

        let result = match op {
            BinOp::Add => self.builder.build_int_add(lhs_int, rhs_int, "add").unwrap(),
            BinOp::Sub => self.builder.build_int_sub(lhs_int, rhs_int, "sub").unwrap(),
            BinOp::Mul => self.builder.build_int_mul(lhs_int, rhs_int, "mul").unwrap(),
            BinOp::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, "div").unwrap(),
            BinOp::Mod => self.builder.build_int_signed_rem(lhs_int, rhs_int, "mod").unwrap(),
            BinOp::Eq => self.builder.build_int_compare(
                inkwell::IntPredicate::EQ, lhs_int, rhs_int, "eq"
            ).unwrap(),
            BinOp::Ne => self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, rhs_int, "ne"
            ).unwrap(),
            BinOp::Lt => self.builder.build_int_compare(
                inkwell::IntPredicate::SLT, lhs_int, rhs_int, "lt"
            ).unwrap(),
            BinOp::Le => self.builder.build_int_compare(
                inkwell::IntPredicate::SLE, lhs_int, rhs_int, "le"
            ).unwrap(),
            BinOp::Gt => self.builder.build_int_compare(
                inkwell::IntPredicate::SGT, lhs_int, rhs_int, "gt"
            ).unwrap(),
            BinOp::Ge => self.builder.build_int_compare(
                inkwell::IntPredicate::SGE, lhs_int, rhs_int, "ge"
            ).unwrap(),
            BinOp::BitAnd => self.builder.build_and(lhs_int, rhs_int, "and").unwrap(),
            BinOp::BitOr => self.builder.build_or(lhs_int, rhs_int, "or").unwrap(),
            BinOp::BitXor => self.builder.build_xor(lhs_int, rhs_int, "xor").unwrap(),
            BinOp::Shl => self.builder.build_left_shift(lhs_int, rhs_int, "shl").unwrap(),
            BinOp::Shr => self.builder.build_right_shift(lhs_int, rhs_int, true, "shr").unwrap(),
            _ => return Err(CodegenError::NotImplemented(format!(
                "binary operator '{:?}' not supported for integer types", op
            ))),
        };

        Ok(result.into())
    }

    pub(crate) fn compile_unary(&mut self, op: UnaryOp, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_unary_with_type(op, operand, None)
    }

    pub(crate) fn compile_unary_with_type(&mut self, op: UnaryOp, operand: &Expr, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // For negation, pass the expected type to the operand so -1 becomes the right type
        let val = self.compile_expr_with_type(operand, expected_type)?;

        match op {
            UnaryOp::Neg => {
                let int_val = val.into_int_value();
                let result = self.builder.build_int_neg(int_val, "neg").unwrap();
                Ok(result.into())
            }
            UnaryOp::BitNot => {
                let int_val = val.into_int_value();
                let result = self.builder.build_not(int_val, "not").unwrap();
                Ok(result.into())
            }
            UnaryOp::Not => {
                let int_val = val.into_int_value();
                let zero = self.context.bool_type().const_zero();
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ, int_val, zero, "lnot"
                ).unwrap();
                Ok(result.into())
            }
        }
    }

    /// Compile an interpolated string like "hello ${name}"
    /// Returns a Slice<u8> (fat pointer: {ptr, len})
    pub(crate) fn compile_interpolated_string(&mut self, parts: &[StringPart]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Strategy: For each part, get its string representation, then concatenate all parts
        // We'll allocate a buffer, sprintf into it, and return a slice

        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();
        let slice_type = self.context.struct_type(&[ptr_type.into(), i64_type.into()], false);

        // First pass: calculate total length and collect string parts
        let mut string_parts: Vec<BasicValueEnum<'ctx>> = Vec::new();
        let mut lengths: Vec<inkwell::values::IntValue<'ctx>> = Vec::new();

        for part in parts {
            match part {
                StringPart::Literal(s) => {
                    // Create a global string for this literal
                    let global = self.builder.build_global_string_ptr(s, "str_part").unwrap();
                    string_parts.push(global.as_pointer_value().into());
                    lengths.push(i64_type.const_int(s.len() as u64, false));
                }
                StringPart::Expr(expr) => {
                    // Compile the expression and convert to string
                    let (ptr, len) = self.expr_to_string(expr)?;
                    string_parts.push(ptr.into());
                    lengths.push(len);
                }
            }
        }

        // Calculate total length
        let mut total_len = i64_type.const_zero();
        for len in &lengths {
            total_len = self.builder.build_int_add(total_len, *len, "total_len").unwrap();
        }

        // Add 1 for null terminator
        let total_with_null = self.builder.build_int_add(
            total_len,
            i64_type.const_int(1, false),
            "total_with_null"
        ).unwrap();

        // Allocate buffer
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let call_result = self.builder
            .build_call(malloc_fn, &[total_with_null.into()], "str_buf")
            .unwrap();
        let buffer = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
            _ => return Err(CodegenError::NotImplemented("malloc call failed".to_string())),
        };

        // Copy each part into the buffer
        let memcpy_fn = self.module.get_function("memcpy").unwrap();
        let mut offset = i64_type.const_zero();

        for (i, (ptr, len)) in string_parts.iter().zip(lengths.iter()).enumerate() {
            // Calculate destination pointer
            let dest = unsafe {
                self.builder.build_gep(
                    self.context.i8_type(),
                    buffer,
                    &[offset],
                    &format!("dest_{}", i)
                ).unwrap()
            };

            // Copy the string
            self.builder.build_call(
                memcpy_fn,
                &[dest.into(), (*ptr).into(), (*len).into()],
                ""
            ).unwrap();

            // Update offset
            offset = self.builder.build_int_add(offset, *len, "offset").unwrap();
        }

        // Null-terminate the string
        let null_ptr = unsafe {
            self.builder.build_gep(
                self.context.i8_type(),
                buffer,
                &[total_len],
                "null_ptr"
            ).unwrap()
        };
        self.builder.build_store(null_ptr, self.context.i8_type().const_zero()).unwrap();

        // Build the slice struct { ptr, len } using runtime values
        let undef_slice = slice_type.get_undef();
        let slice_with_ptr = self.builder
            .build_insert_value(undef_slice, buffer, 0, "slice_ptr")
            .unwrap()
            .into_struct_value();
        let slice_val = self.builder
            .build_insert_value(slice_with_ptr, total_len, 1, "slice_len")
            .unwrap()
            .into_struct_value();
        Ok(slice_val.into())
    }

    /// Convert an expression's value to a string (ptr, len)
    /// Returns (pointer to string data, length)
    fn expr_to_string(&mut self, expr: &Expr) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();

        // First, try to determine the type name from the expression
        let type_name = self.infer_expr_type_name(expr);

        let val = self.compile_expr(expr)?;

        // Check if this is already a string (Slice<u8>) - but not a user struct
        if val.is_struct_value() && type_name.is_none() {
            let struct_val = val.into_struct_value();
            let struct_ty = struct_val.get_type();

            // Check if it's a slice (2 fields: ptr, len - first field is pointer type)
            if struct_ty.count_fields() == 2 {
                // Check if first field is a pointer (slice) vs i32 (enum tag)
                let first_field_ty = struct_ty.get_field_type_at_index(0);
                if first_field_ty.map(|t| t.is_pointer_type()).unwrap_or(false) {
                    // Extract ptr and len
                    let ptr = self.builder
                        .build_extract_value(struct_val, 0, "str_ptr")
                        .unwrap()
                        .into_pointer_value();
                    let len = self.builder
                        .build_extract_value(struct_val, 1, "str_len")
                        .unwrap()
                        .into_int_value();
                    return Ok((ptr, len));
                }
            }
        }

        // Try to get AST type for proper formatting first
        // This handles char, specific int types, and importantly Slice<u8> (str) correctly
        if let Ok(ast_type) = self.get_expr_type(expr) {
            // Helper to check if type is a string (Slice<u8>) or reference to string
            fn is_string_type(ty: &crate::ast::Type) -> bool {
                match ty {
                    crate::ast::Type::Slice(inner) => matches!(inner.as_ref(), crate::ast::Type::U8),
                    crate::ast::Type::Ref(inner) => is_string_type(inner),
                    crate::ast::Type::RefMut(inner) => is_string_type(inner),
                    _ => false,
                }
            }

            // For string types (str, &str, ~str), handle appropriately
            if is_string_type(&ast_type) {
                // The value should already be a struct value (Slice is a struct {ptr, len})
                // If it's a pointer, that means we have a reference - skip the deref, let the
                // existing slice detection handle it
                if val.is_struct_value() {
                    let slice_type = crate::ast::Type::Slice(Box::new(crate::ast::Type::U8));
                    return self.value_to_string(val, &slice_type);
                }
            }
            // For other types that are not structs/enums, use value_to_string
            if !matches!(&ast_type, crate::ast::Type::Named { .. }) {
                return self.value_to_string(val, &ast_type);
            }
        }

        // Handle user-defined types (structs and enums)
        if let Some(ref name) = type_name {
            // If the value is a pointer (reference to a struct), dereference it
            let actual_val = if val.is_pointer_value() {
                // Check if this is a reference variable that we need to dereference
                if let Expr::Ident(var_name, _) = expr {
                    if let Some(var_info) = self.variables.get(var_name) {
                        if var_info.is_ref || var_info.is_mut_ref {
                            // val is already the pointer to the struct (from compile_expr)
                            // Load the struct value through this pointer
                            if let Some(struct_info) = self.struct_types.get(name) {
                                let ptr = val.into_pointer_value();
                                let struct_val = self.builder.build_load(
                                    struct_info.llvm_type,
                                    ptr,
                                    "deref_struct"
                                ).unwrap();
                                struct_val
                            } else {
                                val
                            }
                        } else {
                            val
                        }
                    } else {
                        val
                    }
                } else {
                    val
                }
            } else {
                val
            };

            // Check if this type has a to_string method
            let to_string_method = self.type_methods
                .get(name)
                .and_then(|methods| methods.get("to_string"))
                .cloned();

            if let Some(mangled_name) = to_string_method {
                // Call the to_string method
                return self.call_to_string_method(actual_val, &mangled_name);
            }

            // Check if it's a struct - generate default representation
            if let Some(struct_info) = self.struct_types.get(name).cloned() {
                return self.struct_to_string(actual_val, &struct_info);
            }

            // Check if it's an enum - generate default representation
            if let Some(enum_info) = self.enum_types.get(name).cloned() {
                return self.enum_to_string(actual_val, &enum_info);
            }
        }

        // Fallback: try value_to_string with inferred type (for remaining cases)
        if let Ok(ast_type) = self.get_expr_type(expr) {
            return self.value_to_string(val, &ast_type);
        }

        // Fallback: Handle primitive types by LLVM type
        if val.is_int_value() {
            let int_val = val.into_int_value();
            let bit_width = int_val.get_type().get_bit_width();

            // Determine format string based on bit width
            let fmt_str = if bit_width == 1 {
                // Boolean
                return self.bool_to_string(int_val);
            } else {
                // Integer - use %lld for 64-bit, %d for smaller
                if bit_width == 64 {
                    "%lld"
                } else {
                    "%d"
                }
            };

            return self.int_to_string(int_val, fmt_str);
        }

        if val.is_float_value() {
            let float_val = val.into_float_value();
            return self.float_to_string(float_val);
        }

        // For other types, just return a placeholder
        let unknown = self.builder.build_global_string_ptr("<unknown>", "unknown_str").unwrap();
        Ok((unknown.as_pointer_value(), i64_type.const_int(9, false)))
    }

    /// Infer the type name from an expression (for struct/enum lookup)
    fn infer_expr_type_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name, _) => {
                // Look up variable info to get struct name (or ref_struct_name for references)
                self.variables.get(name).and_then(|info| {
                    info.struct_name.clone().or_else(|| info.ref_struct_name.clone())
                })
            }
            Expr::StructInit { name, .. } => Some(name.clone()),
            Expr::MethodCall { receiver, method: _, .. } => {
                // Check if this is an enum variant constructor (e.g., Option.Some(42))
                if let Expr::Ident(name, _) = receiver.as_ref() {
                    if self.enum_types.contains_key(name) {
                        return Some(name.clone());
                    }
                }
                // For method calls, we'd need to know the return type
                // This is a limitation - for now, return None
                None
            }
            Expr::Field { object, field, .. } => {
                // For field access, we need the type of the field, not the object
                // Look up the struct info to get the field type
                let object_type = self.infer_expr_type_name(object)?;
                let struct_info = self.struct_types.get(&object_type)?;
                let field_idx = *struct_info.field_indices.get(field)?;
                let field_type = struct_info.ast_field_types.get(field_idx as usize)?;
                // Return struct name only if the field is itself a struct
                self.get_struct_name_for_type(field_type)
            }
            _ => None,
        }
    }

    /// Call a type's to_string method
    fn call_to_string_method(
        &mut self,
        val: BasicValueEnum<'ctx>,
        mangled_name: &str,
    ) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {

        // Get the to_string function
        let func = self.module.get_function(mangled_name)
            .ok_or_else(|| CodegenError::UndefinedFunction(mangled_name.to_string()))?;

        // Allocate space for the value and get a pointer to it
        let ptr = self.builder.build_alloca(val.get_type(), "to_string_arg").unwrap();
        self.builder.build_store(ptr, val).unwrap();

        // Call to_string with the reference
        let call_result = self.builder
            .build_call(func, &[ptr.into()], "to_string_result")
            .unwrap();

        // to_string returns a Slice<u8> (ptr, len)
        let slice_val = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_struct_value(),
            _ => return Err(CodegenError::NotImplemented("to_string call failed".to_string())),
        };

        let ptr = self.builder
            .build_extract_value(slice_val, 0, "str_ptr")
            .unwrap()
            .into_pointer_value();
        let len = self.builder
            .build_extract_value(slice_val, 1, "str_len")
            .unwrap()
            .into_int_value();

        Ok((ptr, len))
    }

    /// Generate default string representation for a struct: "StructName { field1: val1, ... }"
    fn struct_to_string(
        &mut self,
        val: BasicValueEnum<'ctx>,
        struct_info: &super::StructTypeInfo<'ctx>,
    ) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();

        // Start with "StructName { "
        let prefix = format!("{} {{ ", struct_info.name);
        let prefix_str = self.builder.build_global_string_ptr(&prefix, "struct_prefix").unwrap();

        // Build up all the string parts
        let mut parts: Vec<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>)> = Vec::new();
        parts.push((prefix_str.as_pointer_value(), i64_type.const_int(prefix.len() as u64, false)));

        let struct_val = val.into_struct_value();

        for (i, field_name) in struct_info.field_names.iter().enumerate() {
            // Add separator for non-first fields
            if i > 0 {
                let sep = self.builder.build_global_string_ptr(", ", "field_sep").unwrap();
                parts.push((sep.as_pointer_value(), i64_type.const_int(2, false)));
            }

            // Add "field_name: "
            let field_prefix = format!("{}: ", field_name);
            let field_prefix_str = self.builder.build_global_string_ptr(&field_prefix, "field_prefix").unwrap();
            parts.push((field_prefix_str.as_pointer_value(), i64_type.const_int(field_prefix.len() as u64, false)));

            // Extract the field value and convert it to string
            let field_val = self.builder
                .build_extract_value(struct_val, i as u32, &format!("field_{}", field_name))
                .unwrap();

            let (field_ptr, field_len) = self.value_to_string(field_val, &struct_info.ast_field_types[i])?;
            parts.push((field_ptr, field_len));
        }

        // Add closing " }"
        let suffix = self.builder.build_global_string_ptr(" }", "struct_suffix").unwrap();
        parts.push((suffix.as_pointer_value(), i64_type.const_int(2, false)));

        // Calculate total length
        let mut total_len = i64_type.const_int(0, false);
        for (_, len) in &parts {
            total_len = self.builder.build_int_add(total_len, *len, "add_len").unwrap();
        }

        // Allocate buffer
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let call_result = self.builder
            .build_call(malloc_fn, &[total_len.into()], "struct_str_buf")
            .unwrap();
        let buffer = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
            _ => return Err(CodegenError::NotImplemented("malloc call failed".to_string())),
        };

        // Copy all parts
        let memcpy_fn = self.module.get_function("memcpy").unwrap();
        let mut offset = i64_type.const_int(0, false);
        for (ptr, len) in &parts {
            let dest = unsafe {
                self.builder.build_gep(self.context.i8_type(), buffer, &[offset], "dest").unwrap()
            };
            self.builder.build_call(memcpy_fn, &[dest.into(), (*ptr).into(), (*len).into()], "copy").unwrap();
            offset = self.builder.build_int_add(offset, *len, "new_offset").unwrap();
        }

        Ok((buffer, total_len))
    }

    /// Generate default string representation for an enum: "EnumName.Variant" or "EnumName.Variant(payload)"
    fn enum_to_string(
        &mut self,
        val: BasicValueEnum<'ctx>,
        enum_info: &super::EnumTypeInfo<'ctx>,
    ) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let struct_val = val.into_struct_value();

        // Extract the tag
        let tag = self.builder
            .build_extract_value(struct_val, 0, "enum_tag")
            .unwrap()
            .into_int_value();

        // For a simple implementation, we'll generate a static string for each variant
        // and use a switch to select the right one
        let fn_value = self.current_function.unwrap();
        let merge_bb = self.context.append_basic_block(fn_value, "enum_str.merge");

        // Create blocks for each variant
        let mut variant_blocks = Vec::new();
        for variant_name in &enum_info.variant_names {
            let bb = self.context.append_basic_block(fn_value, &format!("enum_str.{}", variant_name));
            variant_blocks.push(bb);
        }

        // Build switch cases
        let cases: Vec<_> = variant_blocks.iter().enumerate()
            .map(|(i, bb)| (i32_type.const_int(i as u64, false), *bb))
            .collect();

        let default_bb = variant_blocks.first().cloned().unwrap_or(merge_bb);
        self.builder.build_switch(tag, default_bb, &cases).unwrap();

        // Generate string for each variant
        let mut incoming: Vec<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();

        for (i, variant_name) in enum_info.variant_names.iter().enumerate() {
            self.builder.position_at_end(variant_blocks[i]);

            // Check if this variant has a payload
            let payload_types = enum_info.variant_payloads.get(variant_name).cloned().unwrap_or_default();

            if payload_types.is_empty() {
                // No payload: just "EnumName.Variant"
                let str_val = format!("{}.{}", enum_info.name, variant_name);
                let str_ptr = self.builder.build_global_string_ptr(&str_val, "enum_str").unwrap();
                let str_len = i64_type.const_int(str_val.len() as u64, false);
                incoming.push((str_ptr.as_pointer_value(), str_len, variant_blocks[i]));
            } else {
                // Has payload: "EnumName.Variant(payload)"
                // For now, just show placeholder for payload
                let str_val = format!("{}.{}(...)", enum_info.name, variant_name);
                let str_ptr = self.builder.build_global_string_ptr(&str_val, "enum_str").unwrap();
                let str_len = i64_type.const_int(str_val.len() as u64, false);
                incoming.push((str_ptr.as_pointer_value(), str_len, variant_blocks[i]));
            }

            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Merge block with phi nodes
        self.builder.position_at_end(merge_bb);

        let phi_ptr = self.builder.build_phi(ptr_type, "enum_ptr").unwrap();
        let phi_len = self.builder.build_phi(i64_type, "enum_len").unwrap();

        for (ptr, len, bb) in &incoming {
            phi_ptr.add_incoming(&[(ptr, *bb)]);
            phi_len.add_incoming(&[(len, *bb)]);
        }

        Ok((phi_ptr.as_basic_value().into_pointer_value(), phi_len.as_basic_value().into_int_value()))
    }

    /// Convert an LLVM value to string based on its AST type
    fn value_to_string(
        &mut self,
        val: BasicValueEnum<'ctx>,
        ast_type: &crate::ast::Type,
    ) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();

        match ast_type {
            crate::ast::Type::I8 | crate::ast::Type::I16 | crate::ast::Type::I32 => {
                self.int_to_string(val.into_int_value(), "%d")
            }
            crate::ast::Type::I64 => {
                self.int_to_string(val.into_int_value(), "%lld")
            }
            crate::ast::Type::U8 | crate::ast::Type::U16 | crate::ast::Type::U32 => {
                self.int_to_string(val.into_int_value(), "%u")
            }
            crate::ast::Type::U64 => {
                self.int_to_string(val.into_int_value(), "%llu")
            }
            crate::ast::Type::Bool => {
                self.bool_to_string(val.into_int_value())
            }
            crate::ast::Type::Char => {
                // char is u8, print as character
                self.char_to_string(val.into_int_value())
            }
            crate::ast::Type::F32 | crate::ast::Type::F64 => {
                self.float_to_string(val.into_float_value())
            }
            crate::ast::Type::Named { name, generics } => {
                // For generic types, use the mangled name
                let lookup_name = if generics.is_empty() {
                    name.clone()
                } else {
                    self.mangle_name(name, generics)
                };

                // Check if it's a struct
                if let Some(struct_info) = self.struct_types.get(&lookup_name).cloned() {
                    return self.struct_to_string(val, &struct_info);
                }
                // Check if it's an enum
                if let Some(enum_info) = self.enum_types.get(&lookup_name).cloned() {
                    return self.enum_to_string(val, &enum_info);
                }
                // Unknown type
                let unknown = self.builder.build_global_string_ptr("<unknown>", "unknown_str").unwrap();
                Ok((unknown.as_pointer_value(), i64_type.const_int(9, false)))
            }
            crate::ast::Type::Slice(inner) => {
                // Slice<u8> is a string
                if matches!(inner.as_ref(), crate::ast::Type::U8) {
                    let struct_val = val.into_struct_value();
                    let ptr = self.builder
                        .build_extract_value(struct_val, 0, "str_ptr")
                        .unwrap()
                        .into_pointer_value();
                    let len = self.builder
                        .build_extract_value(struct_val, 1, "str_len")
                        .unwrap()
                        .into_int_value();
                    return Ok((ptr, len));
                }
                // Other slices - placeholder
                let unknown = self.builder.build_global_string_ptr("<slice>", "slice_str").unwrap();
                Ok((unknown.as_pointer_value(), i64_type.const_int(7, false)))
            }
            _ => {
                // Default placeholder
                let unknown = self.builder.build_global_string_ptr("<unknown>", "unknown_str").unwrap();
                Ok((unknown.as_pointer_value(), i64_type.const_int(9, false)))
            }
        }
    }

    /// Convert an integer to a string
    fn int_to_string(&mut self, val: inkwell::values::IntValue<'ctx>, fmt: &str) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();

        // Allocate buffer (32 bytes is enough for any 64-bit int)
        let buf_size = i64_type.const_int(32, false);
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let call_result = self.builder
            .build_call(malloc_fn, &[buf_size.into()], "int_buf")
            .unwrap();
        let buffer = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
            _ => return Err(CodegenError::NotImplemented("malloc call failed".to_string())),
        };

        // Format the integer using snprintf
        let snprintf_fn = self.module.get_function("snprintf").unwrap();
        let fmt_str = self.builder.build_global_string_ptr(fmt, "int_fmt").unwrap();

        // Extend smaller ints to i64 for consistent formatting
        let extended_val = if val.get_type().get_bit_width() < 64 {
            self.builder.build_int_s_extend(val, i64_type, "extended").unwrap()
        } else {
            val
        };

        let call_result = self.builder
            .build_call(
                snprintf_fn,
                &[buffer.into(), buf_size.into(), fmt_str.as_pointer_value().into(), extended_val.into()],
                "len"
            )
            .unwrap();
        let len = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_int_value(),
            _ => return Err(CodegenError::NotImplemented("snprintf call failed".to_string())),
        };

        // Convert i32 length to i64
        let len_i64 = self.builder.build_int_z_extend(len, i64_type, "len_i64").unwrap();

        Ok((buffer, len_i64))
    }

    /// Convert a bool to a string ("true" or "false")
    fn bool_to_string(&mut self, val: inkwell::values::IntValue<'ctx>) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();
        let fn_value = self.current_function.unwrap();

        // Create blocks for true/false branches
        let true_bb = self.context.append_basic_block(fn_value, "bool.true");
        let false_bb = self.context.append_basic_block(fn_value, "bool.false");
        let merge_bb = self.context.append_basic_block(fn_value, "bool.merge");

        // Branch based on bool value
        let zero = val.get_type().const_zero();
        let is_true = self.builder.build_int_compare(
            inkwell::IntPredicate::NE, val, zero, "is_true"
        ).unwrap();
        self.builder.build_conditional_branch(is_true, true_bb, false_bb).unwrap();

        // True branch
        self.builder.position_at_end(true_bb);
        let true_str = self.builder.build_global_string_ptr("true", "true_str").unwrap();
        let true_len = i64_type.const_int(4, false);
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        // False branch
        self.builder.position_at_end(false_bb);
        let false_str = self.builder.build_global_string_ptr("false", "false_str").unwrap();
        let false_len = i64_type.const_int(5, false);
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        // Merge block with phi nodes
        self.builder.position_at_end(merge_bb);
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let phi_ptr = self.builder.build_phi(ptr_type, "bool_ptr").unwrap();
        phi_ptr.add_incoming(&[(&true_str.as_pointer_value(), true_bb), (&false_str.as_pointer_value(), false_bb)]);

        let phi_len = self.builder.build_phi(i64_type, "bool_len").unwrap();
        phi_len.add_incoming(&[(&true_len, true_bb), (&false_len, false_bb)]);

        Ok((phi_ptr.as_basic_value().into_pointer_value(), phi_len.as_basic_value().into_int_value()))
    }

    /// Convert a char to a string (single character)
    fn char_to_string(&mut self, val: inkwell::values::IntValue<'ctx>) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();

        // Allocate 2-byte buffer (char + null terminator)
        let buf_size = i64_type.const_int(2, false);
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let call_result = self.builder
            .build_call(malloc_fn, &[buf_size.into()], "char_buf")
            .unwrap();
        let buffer = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
            _ => return Err(CodegenError::NotImplemented("malloc call failed".to_string())),
        };

        // Store the character
        self.builder.build_store(buffer, val).unwrap();

        // Store null terminator at position 1
        let one = i64_type.const_int(1, false);
        let null_ptr = unsafe {
            self.builder.build_gep(i8_type, buffer, &[one], "null_pos").unwrap()
        };
        self.builder.build_store(null_ptr, i8_type.const_zero()).unwrap();

        // Return ptr and length 1
        let len = i64_type.const_int(1, false);
        Ok((buffer, len))
    }

    /// Convert a float to a string
    fn float_to_string(&mut self, val: inkwell::values::FloatValue<'ctx>) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();

        // Allocate buffer (64 bytes is enough for any float)
        let buf_size = i64_type.const_int(64, false);
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let call_result = self.builder
            .build_call(malloc_fn, &[buf_size.into()], "float_buf")
            .unwrap();
        let buffer = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_pointer_value(),
            _ => return Err(CodegenError::NotImplemented("malloc call failed".to_string())),
        };

        // Format the float using snprintf
        let snprintf_fn = self.module.get_function("snprintf").unwrap();
        let fmt_str = self.builder.build_global_string_ptr("%g", "float_fmt").unwrap();

        // Convert f32 to f64 if needed for printf
        let double_val = if val.get_type() == self.context.f32_type() {
            self.builder.build_float_ext(val, self.context.f64_type(), "f64").unwrap()
        } else {
            val
        };

        let call_result = self.builder
            .build_call(
                snprintf_fn,
                &[buffer.into(), buf_size.into(), fmt_str.as_pointer_value().into(), double_val.into()],
                "len"
            )
            .unwrap();
        let len = match call_result.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(v) => v.into_int_value(),
            _ => return Err(CodegenError::NotImplemented("snprintf call failed".to_string())),
        };

        // Convert i32 length to i64
        let len_i64 = self.builder.build_int_z_extend(len, i64_type, "len_i64").unwrap();

        Ok((buffer, len_i64))
    }

    /// Compile logical AND with short-circuit evaluation
    /// if left is false, skip right and return false
    fn compile_logical_and(&mut self, left: &Expr, right: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let bool_type = self.context.bool_type();

        // Create basic blocks
        let rhs_block = self.context.append_basic_block(current_fn, "and.rhs");
        let merge_block = self.context.append_basic_block(current_fn, "and.merge");

        // Compile left side
        let lhs = self.compile_expr(left)?;
        let lhs_int = lhs.into_int_value();
        // Ensure condition is i1 for branching (handles ptr_is_null returning i64)
        let lhs_bool = if lhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, lhs_int.get_type().const_zero(), "lhs_bool"
            ).unwrap()
        } else {
            lhs_int
        };

        // Save the block we're coming from for the phi
        let lhs_block = self.builder.get_insert_block().unwrap();

        // Branch: if lhs is true, evaluate rhs; else short-circuit to false
        self.builder.build_conditional_branch(lhs_bool, rhs_block, merge_block).unwrap();

        // Compile right side
        self.builder.position_at_end(rhs_block);
        let rhs = self.compile_expr(right)?;
        let rhs_int = rhs.into_int_value();
        // Ensure rhs is i1 for phi node
        let rhs_bool = if rhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, rhs_int, rhs_int.get_type().const_zero(), "rhs_bool"
            ).unwrap()
        } else {
            rhs_int
        };
        let rhs_final_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block).unwrap();

        // Merge: phi node to get the result
        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_type, "and.result").unwrap();

        // If we came from lhs_block (short-circuit), result is false
        // If we came from rhs_block, result is the rhs value
        phi.add_incoming(&[
            (&bool_type.const_zero(), lhs_block),
            (&rhs_bool, rhs_final_block),
        ]);

        Ok(phi.as_basic_value())
    }

    /// Compile logical OR with short-circuit evaluation
    /// if left is true, skip right and return true
    fn compile_logical_or(&mut self, left: &Expr, right: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let current_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let bool_type = self.context.bool_type();

        // Create basic blocks
        let rhs_block = self.context.append_basic_block(current_fn, "or.rhs");
        let merge_block = self.context.append_basic_block(current_fn, "or.merge");

        // Compile left side
        let lhs = self.compile_expr(left)?;
        let lhs_int = lhs.into_int_value();
        // Ensure condition is i1 for branching (handles ptr_is_null returning i64)
        let lhs_bool = if lhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, lhs_int.get_type().const_zero(), "lhs_bool"
            ).unwrap()
        } else {
            lhs_int
        };

        // Save the block we're coming from for the phi
        let lhs_block = self.builder.get_insert_block().unwrap();

        // Branch: if lhs is true, short-circuit to true; else evaluate rhs
        self.builder.build_conditional_branch(lhs_bool, merge_block, rhs_block).unwrap();

        // Compile right side
        self.builder.position_at_end(rhs_block);
        let rhs = self.compile_expr(right)?;
        let rhs_int = rhs.into_int_value();
        // Ensure rhs is i1 for phi node
        let rhs_bool = if rhs_int.get_type().get_bit_width() != 1 {
            self.builder.build_int_compare(
                inkwell::IntPredicate::NE, rhs_int, rhs_int.get_type().const_zero(), "rhs_bool"
            ).unwrap()
        } else {
            rhs_int
        };
        let rhs_final_block = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(merge_block).unwrap();

        // Merge: phi node to get the result
        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_type, "or.result").unwrap();

        // If we came from lhs_block (short-circuit), result is true
        // If we came from rhs_block, result is the rhs value
        phi.add_incoming(&[
            (&bool_type.const_int(1, false), lhs_block),
            (&rhs_bool, rhs_final_block),
        ]);

        Ok(phi.as_basic_value())
    }

    /// Compile a type cast expression (expr as Type)
    pub(crate) fn compile_cast(&mut self, expr: &Expr, target_type: &Type) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let val = self.compile_expr(expr)?;
        let target_llvm_type = self.llvm_type(target_type)?;

        // Handle different cast scenarios
        match (val, target_llvm_type) {
            // Pointer to pointer (reinterpret cast - no-op in LLVM with opaque pointers)
            (BasicValueEnum::PointerValue(ptr), inkwell::types::BasicTypeEnum::PointerType(_)) => {
                // No actual conversion needed with opaque pointers
                Ok(ptr.into())
            }
            // Integer to pointer
            (BasicValueEnum::IntValue(int_val), inkwell::types::BasicTypeEnum::PointerType(ptr_ty)) => {
                let ptr = self.builder
                    .build_int_to_ptr(int_val, ptr_ty, "int_to_ptr")
                    .unwrap();
                Ok(ptr.into())
            }
            // Pointer to integer
            (BasicValueEnum::PointerValue(ptr), inkwell::types::BasicTypeEnum::IntType(int_ty)) => {
                let int_val = self.builder
                    .build_ptr_to_int(ptr, int_ty, "ptr_to_int")
                    .unwrap();
                Ok(int_val.into())
            }
            // Integer to integer (truncate or extend)
            (BasicValueEnum::IntValue(int_val), inkwell::types::BasicTypeEnum::IntType(int_ty)) => {
                let src_bits = int_val.get_type().get_bit_width();
                let dst_bits = int_ty.get_bit_width();

                let result = if src_bits == dst_bits {
                    // Same size, no conversion needed
                    int_val
                } else if src_bits < dst_bits {
                    // Zero extend to larger type
                    self.builder.build_int_z_extend(int_val, int_ty, "zext").unwrap()
                } else {
                    // Truncate to smaller type
                    self.builder.build_int_truncate(int_val, int_ty, "trunc").unwrap()
                };
                Ok(result.into())
            }
            // Float to integer
            (BasicValueEnum::FloatValue(float_val), inkwell::types::BasicTypeEnum::IntType(int_ty)) => {
                let result = self.builder
                    .build_float_to_signed_int(float_val, int_ty, "fptosi")
                    .unwrap();
                Ok(result.into())
            }
            // Integer to float
            (BasicValueEnum::IntValue(int_val), inkwell::types::BasicTypeEnum::FloatType(float_ty)) => {
                let result = self.builder
                    .build_signed_int_to_float(int_val, float_ty, "sitofp")
                    .unwrap();
                Ok(result.into())
            }
            _ => Err(CodegenError::NotImplemented(format!(
                "cast from {:?} to {:?} not supported",
                val.get_type(),
                target_type
            ))),
        }
    }

    /// Compile a tuple expression: (a, b, c)
    pub(crate) fn compile_tuple(
        &mut self,
        elements: &[Expr],
        expected_type: Option<&Type>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Get expected element types if available
        let expected_elem_types: Vec<Option<&Type>> = match expected_type {
            Some(Type::Tuple(types)) => types.iter().map(Some).collect(),
            _ => vec![None; elements.len()],
        };

        // Compile each element
        let mut values = Vec::new();
        for (i, elem) in elements.iter().enumerate() {
            let expected = expected_elem_types.get(i).copied().flatten();
            let val = self.compile_expr_with_type(elem, expected)?;
            values.push(val);
        }

        // Create the tuple type (anonymous struct)
        let field_types: Vec<_> = values.iter().map(|v| v.get_type()).collect();
        let tuple_type = self.context.struct_type(&field_types, false);

        // Allocate and initialize the tuple
        let alloca = self.builder.build_alloca(tuple_type, "tuple").unwrap();

        for (i, val) in values.iter().enumerate() {
            let field_ptr = self.builder
                .build_struct_gep(tuple_type, alloca, i as u32, &format!("tuple.{}", i))
                .unwrap();
            self.builder.build_store(field_ptr, *val).unwrap();
        }

        // Load and return the tuple value
        let tuple_val = self.builder.build_load(tuple_type, alloca, "tuple_val").unwrap();
        Ok(tuple_val)
    }

    /// Compile a closure expression
    fn compile_closure(
        &mut self,
        params: &[(String, Option<Type>)],
        return_type: Option<&Type>,
        body: &ClosureBody,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        use inkwell::AddressSpace;
        use super::capture;

        // Analyze the closure body for captured variables
        let captures = capture::collect_free_variables(params, body);

        // Filter to only include variables that exist in our scope
        let valid_captures: Vec<_> = captures.into_iter()
            .filter(|c| self.variables.contains_key(&c.name))
            .collect();

        // Generate a unique name for the closure function
        let closure_id = self.closure_counter;
        self.closure_counter += 1;
        let closure_name = format!("__closure_{}", closure_id);

        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Build parameter types: env_ptr first, then user params
        let mut all_param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = Vec::new();
        all_param_types.push(ptr_type.into()); // env_ptr is always first

        let mut user_param_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = Vec::new();
        for (_, ty) in params.iter() {
            let llvm_ty = match ty {
                Some(t) => self.llvm_type(t).unwrap_or(self.context.i32_type().into()),
                None => self.context.i32_type().into(), // default to i32
            };
            user_param_types.push(llvm_ty);
            all_param_types.push(llvm_ty.into());
        }

        // Determine return type
        let ret_type = match return_type {
            Some(t) => self.llvm_type(t)?,
            None => self.context.i32_type().into(), // default to i32
        };

        // Create function type with env_ptr as first param
        let fn_type = match ret_type {
            inkwell::types::BasicTypeEnum::IntType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::FloatType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::PointerType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::StructType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::ArrayType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::VectorType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::ScalableVectorType(_) => {
                return Err(CodegenError::NotImplemented("scalable vector types not supported in closures".to_string()));
            }
        };

        // Create the closure function
        let closure_fn = self.module.add_function(&closure_name, fn_type, None);

        // Collect information about captured variables before we clear self.variables
        let captured_vars_info: Vec<_> = valid_captures.iter().map(|cap| {
            let var_info = self.variables.get(&cap.name).unwrap().clone();
            (cap.name.clone(), var_info, cap.is_mutated)
        }).collect();

        // Build the environment struct type
        // - Immutable captures: store the value directly
        // - Mutable captures: store a pointer to a heap-allocated "box"
        let env_struct_type = if !captured_vars_info.is_empty() {
            let field_types: Vec<inkwell::types::BasicTypeEnum> = captured_vars_info.iter()
                .map(|(_, var_info, is_mutated)| {
                    if *is_mutated {
                        // Mutable captures store a pointer to the boxed value
                        ptr_type.into()
                    } else {
                        // Immutable captures store the value directly
                        var_info.ty
                    }
                })
                .collect();
            Some(self.context.struct_type(&field_types, false))
        } else {
            None
        };

        // Save current state
        let saved_fn = self.current_function;
        let saved_block = self.builder.get_insert_block();
        let saved_vars = self.variables.clone();

        // Set up the closure function
        self.current_function = Some(closure_fn);
        let entry = self.context.append_basic_block(closure_fn, "entry");
        self.builder.position_at_end(entry);

        // Clear variables for the closure scope
        self.variables.clear();

        // Bind captured variables from env_ptr
        // - Immutable captures: env field contains the value directly
        // - Mutable captures: env field contains a pointer to the boxed value
        if let Some(env_type) = env_struct_type {
            let env_ptr_param = closure_fn.get_nth_param(0).unwrap().into_pointer_value();

            for (i, (name, var_info, is_mutated)) in captured_vars_info.iter().enumerate() {
                // Get pointer to the field in the environment struct
                let cap_field_ptr = self.builder
                    .build_struct_gep(env_type, env_ptr_param, i as u32, &format!("cap_{}", name))
                    .unwrap();

                let var_ptr = if *is_mutated {
                    // Mutable capture: env field contains a pointer to the box
                    // Load the pointer to get the actual storage location
                    self.builder
                        .build_load(ptr_type, cap_field_ptr, &format!("box_{}", name))
                        .unwrap()
                        .into_pointer_value()
                } else {
                    // Immutable capture: value is stored directly in env field
                    cap_field_ptr
                };

                self.variables.insert(name.clone(), super::VarInfo {
                    ptr: var_ptr,
                    ty: var_info.ty,
                    struct_name: var_info.struct_name.clone(),
                    ast_type: var_info.ast_type.clone(),
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: var_info.ref_struct_name.clone(),
                    slice_elem_type: var_info.slice_elem_type.clone(),
                });
            }
        }

        // Bind user parameters (starting from index 1, skip env_ptr)
        for (i, (name, ty)) in params.iter().enumerate() {
            let param_val = closure_fn.get_nth_param((i + 1) as u32).unwrap(); // +1 to skip env_ptr
            let param_type = user_param_types[i];

            let alloca = self.create_entry_block_alloca(name, param_type);
            self.builder.build_store(alloca, param_val).unwrap();

            self.variables.insert(name.clone(), super::VarInfo {
                ptr: alloca,
                ty: param_type,
                struct_name: ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)),
                ast_type: ty.clone(),
                is_ref: false,
                is_mut_ref: false,
                ref_struct_name: None,
                slice_elem_type: None,
            });
        }

        // Compile the body
        match body {
            ClosureBody::Expr(expr) => {
                let result = self.compile_expr(expr)?;
                self.builder.build_return(Some(&result)).unwrap();
            }
            ClosureBody::Block(block) => {
                self.compile_block(block)?;
                // If no return was encountered, return void equivalent
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_return(Some(&self.context.i32_type().const_zero())).unwrap();
                }
            }
        }

        // Restore state
        self.current_function = saved_fn;
        self.variables = saved_vars;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        // Build the environment struct if we have captures
        let env_ptr_val = if let Some(env_type) = env_struct_type {
            // Allocate the environment struct on the HEAP so it survives after the
            // creating function returns (closures may escape their defining scope)
            let malloc_fn = self.module.get_function("malloc").unwrap();
            let env_size = env_type.size_of().unwrap();
            let call_result = self.builder
                .build_call(malloc_fn, &[env_size.into()], "closure_env_heap")
                .unwrap();
            let env_ptr = match call_result.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
                _ => return Err(CodegenError::NotImplemented("malloc call failed for closure env".to_string())),
            };

            // Fill the environment struct with captured variables
            // - Immutable captures: copy value directly into env
            // - Mutable captures: allocate a "box" on heap, store pointer in env
            for (i, (name, var_info, is_mutated)) in captured_vars_info.iter().enumerate() {
                let current_var_info = self.variables.get(name).unwrap().clone();

                let field_ptr = self.builder
                    .build_struct_gep(env_type, env_ptr, i as u32, &format!("env_{}", name))
                    .unwrap();

                if *is_mutated {
                    // Mutable capture: create a heap-allocated "box" for the value
                    // This allows both the closure and outer scope to share the same storage
                    let value_size = match var_info.ty {
                        inkwell::types::BasicTypeEnum::IntType(t) => t.size_of(),
                        inkwell::types::BasicTypeEnum::FloatType(t) => t.size_of(),
                        inkwell::types::BasicTypeEnum::PointerType(t) => t.size_of(),
                        inkwell::types::BasicTypeEnum::ArrayType(t) => t.size_of().unwrap(),
                        inkwell::types::BasicTypeEnum::StructType(t) => t.size_of().unwrap(),
                        inkwell::types::BasicTypeEnum::VectorType(t) => t.size_of().unwrap(),
                        inkwell::types::BasicTypeEnum::ScalableVectorType(_) => {
                            return Err(CodegenError::NotImplemented(
                                "mutable capture not supported for scalable vector types".to_string()
                            ));
                        }
                    };
                    let box_result = self.builder
                        .build_call(malloc_fn, &[value_size.into()], &format!("box_{}", name))
                        .unwrap();
                    let box_ptr = match box_result.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
                        _ => return Err(CodegenError::NotImplemented("malloc call failed for capture box".to_string())),
                    };

                    // Copy the current value into the box
                    let value = self.builder
                        .build_load(var_info.ty, current_var_info.ptr, &format!("cap_val_{}", name))
                        .unwrap();
                    self.builder.build_store(box_ptr, value).unwrap();

                    // Store the box pointer in the environment struct
                    self.builder.build_store(field_ptr, box_ptr).unwrap();

                    // Update the outer scope's variable to point to the box
                    // This way both the closure and outer scope share the same storage
                    if let Some(outer_var) = self.variables.get_mut(name) {
                        outer_var.ptr = box_ptr;
                    }
                } else {
                    // Immutable capture: copy value directly into env
                    let value = self.builder
                        .build_load(var_info.ty, current_var_info.ptr, &format!("cap_val_{}", name))
                        .unwrap();
                    self.builder.build_store(field_ptr, value).unwrap();
                }
            }

            env_ptr
        } else {
            ptr_type.const_null()
        };

        // Build fat pointer struct: {fn_ptr, env_ptr}
        let fn_ptr = closure_fn.as_global_value().as_pointer_value();

        let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);
        let fat_ptr_alloca = self.create_entry_block_alloca("closure_fat_ptr", fat_ptr_type.into());

        // Store fn_ptr at index 0
        let fn_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 0, "fn_ptr_gep").unwrap();
        self.builder.build_store(fn_ptr_gep, fn_ptr).unwrap();

        // Store env_ptr at index 1
        let env_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 1, "env_ptr_gep").unwrap();
        self.builder.build_store(env_ptr_gep, env_ptr_val).unwrap();

        // Load and return the fat pointer
        let fat_ptr_val = self.builder.build_load(fat_ptr_type, fat_ptr_alloca, "closure_val").unwrap();
        Ok(fat_ptr_val)
    }

    /// Coerce a named function to a closure fat pointer.
    /// This generates a thunk function that has the closure calling convention
    /// (env_ptr as first parameter) and wraps the original function.
    fn coerce_function_to_closure(
        &mut self,
        name: &str,
        func: inkwell::values::FunctionValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());

        // Check if we already generated a thunk for this function
        let thunk_name = format!("__thunk_{}", name);
        let thunk_fn = if let Some(existing) = self.module.get_function(&thunk_name) {
            existing
        } else {
            // Generate a thunk function with closure calling convention:
            // fn __thunk_name(env_ptr: *void, params...) -> ReturnType
            let original_type = func.get_type();
            let original_params: Vec<_> = original_type.get_param_types();
            let return_type = original_type.get_return_type();

            // Build thunk parameter types: env_ptr first, then original params
            let mut thunk_params: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
            thunk_params.push(ptr_type.into()); // env_ptr (will be ignored)
            for param in &original_params {
                thunk_params.push((*param).into());
            }

            // Create thunk function type
            let thunk_type = match return_type {
                Some(inkwell::types::BasicTypeEnum::IntType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::FloatType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::PointerType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::StructType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::ArrayType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::VectorType(t)) => t.fn_type(&thunk_params, false),
                None => self.context.void_type().fn_type(&thunk_params, false),
                _ => return Err(CodegenError::NotImplemented(
                    format!("unsupported return type for function coercion: {}", name)
                )),
            };

            // Create thunk function
            let thunk = self.module.add_function(&thunk_name, thunk_type, None);

            // Save current state
            let saved_fn = self.current_function;
            let saved_block = self.builder.get_insert_block();

            // Build thunk body
            self.current_function = Some(thunk);
            let entry = self.context.append_basic_block(thunk, "entry");
            self.builder.position_at_end(entry);

            // Collect arguments (skip env_ptr at index 0)
            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            for i in 0..original_params.len() {
                let param = thunk.get_nth_param((i + 1) as u32).unwrap(); // +1 to skip env_ptr
                call_args.push(param.into());
            }

            // Call original function
            let call_result = self.builder.build_call(func, &call_args, "thunk_call").unwrap();

            // Return result (or void)
            if return_type.is_some() {
                match call_result.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(ret_val) => {
                        self.builder.build_return(Some(&ret_val)).unwrap();
                    }
                    _ => {
                        self.builder.build_return(None).unwrap();
                    }
                }
            } else {
                self.builder.build_return(None).unwrap();
            }

            // Restore state
            self.current_function = saved_fn;
            if let Some(block) = saved_block {
                self.builder.position_at_end(block);
            }

            thunk
        };

        // Build fat pointer struct: {thunk_fn_ptr, null}
        let fn_ptr = thunk_fn.as_global_value().as_pointer_value();
        let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);
        let fat_ptr_alloca = self.create_entry_block_alloca("fn_closure_ptr", fat_ptr_type.into());

        // Store fn_ptr at index 0
        let fn_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 0, "fn_ptr_gep").unwrap();
        self.builder.build_store(fn_ptr_gep, fn_ptr).unwrap();

        // Store null env_ptr at index 1
        let env_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 1, "env_ptr_gep").unwrap();
        self.builder.build_store(env_ptr_gep, ptr_type.const_null()).unwrap();

        // Load and return the fat pointer
        let fat_ptr_val = self.builder.build_load(fat_ptr_type, fat_ptr_alloca, "fn_as_closure").unwrap();
        Ok(fat_ptr_val)
    }
}
