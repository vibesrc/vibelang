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
                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                let val = self.builder.build_load(var_info.ty, var_info.ptr, name).unwrap();
                Ok(val)
            }
            Expr::Binary { op, left, right, .. } => {
                self.compile_binary(*op, left, right)
            }
            Expr::Unary { op, operand, .. } => {
                self.compile_unary(*op, operand)
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

                    // Check if this is a module function call (e.g., math.add())
                    if self.module_items.contains_key(name) {
                        return self.compile_module_function_call(name, method, args);
                    }

                    // Check if this is a static method call on a struct type (e.g., IntArray.new())
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
            _ => {
                // TODO: implement other expressions
                Err(CodegenError::NotImplemented("expression type".to_string()))
            }
        }
    }

    pub(crate) fn compile_literal(&mut self, lit: &Literal) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.compile_literal_with_type(lit, None)
    }

    /// Compile a literal with an optional expected type for coercion
    pub(crate) fn compile_literal_with_type(&mut self, lit: &Literal, expected_type: Option<&Type>) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match lit {
            Literal::Int(n) => {
                // Coerce integer literal to expected type if specified
                let int_type = match expected_type {
                    Some(Type::I8) => self.context.i8_type(),
                    Some(Type::I16) => self.context.i16_type(),
                    Some(Type::I32) => self.context.i32_type(),
                    Some(Type::U8) => self.context.i8_type(),
                    Some(Type::U16) => self.context.i16_type(),
                    Some(Type::U32) => self.context.i32_type(),
                    Some(Type::U64) => self.context.i64_type(),
                    _ => self.context.i64_type(), // Default to i64
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
            Literal::String(s) => {
                // Create string as &[u8] slice (fat pointer: {ptr, len})
                let global = self.builder.build_global_string_ptr(s, "str").unwrap();
                let ptr = global.as_pointer_value();
                let len = self.context.i64_type().const_int(s.len() as u64, false);

                // Build slice struct type { ptr, len }
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let len_type = self.context.i64_type();
                let slice_type = self.context.struct_type(&[ptr_type.into(), len_type.into()], false);

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

            return Err(CodegenError::InvalidAssignment);
        }

        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        // Handle pointer comparisons
        if lhs.is_pointer_value() || rhs.is_pointer_value() {
            // Convert both to pointers for comparison (int 0 becomes null pointer)
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
                _ => return Err(CodegenError::NotImplemented(format!("pointer binary op {:?}", op))),
            };

            return Ok(result.into());
        }

        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

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
            _ => return Err(CodegenError::NotImplemented(format!("binary op {:?}", op))),
        };

        Ok(result.into())
    }

    pub(crate) fn compile_unary(&mut self, op: UnaryOp, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let val = self.compile_expr(operand)?;

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

        // Build the slice struct { ptr, len }
        let slice_val = slice_type.const_named_struct(&[buffer.into(), total_len.into()]);
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

        // Handle user-defined types (structs and enums)
        if let Some(ref name) = type_name {
            // Check if this type has a to_string method
            let to_string_method = self.type_methods
                .get(name)
                .and_then(|methods| methods.get("to_string"))
                .cloned();

            if let Some(mangled_name) = to_string_method {
                // Call the to_string method
                return self.call_to_string_method(val, &mangled_name);
            }

            // Check if it's a struct - generate default representation
            if let Some(struct_info) = self.struct_types.get(name).cloned() {
                return self.struct_to_string(val, &struct_info);
            }

            // Check if it's an enum - generate default representation
            if let Some(enum_info) = self.enum_types.get(name).cloned() {
                return self.enum_to_string(val, &enum_info);
            }
        }

        // Handle primitive types
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
                // Look up variable info to get struct name
                self.variables.get(name).and_then(|info| info.struct_name.clone())
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
            Expr::Field { object, .. } => {
                // For field access, recurse to get the struct type
                self.infer_expr_type_name(object)
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
            crate::ast::Type::F32 | crate::ast::Type::F64 => {
                self.float_to_string(val.into_float_value())
            }
            crate::ast::Type::Named { name, .. } => {
                // Check if it's a struct
                if let Some(struct_info) = self.struct_types.get(name).cloned() {
                    return self.struct_to_string(val, &struct_info);
                }
                // Check if it's an enum
                if let Some(enum_info) = self.enum_types.get(name).cloned() {
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
}
