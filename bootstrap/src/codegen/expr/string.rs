//! String interpolation and type-to-string conversion

use crate::codegen::{Codegen, CodegenError, StructTypeInfo, EnumTypeInfo};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
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
    pub(crate) fn expr_to_string(&mut self, expr: &Expr) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
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
            // Helper to check if type is a string (str or Slice<u8>) or reference to string
            fn is_string_type(ty: &Type) -> bool {
                match ty {
                    Type::Str => true,
                    Type::Slice(inner) => matches!(inner.as_ref(), Type::U8),
                    Type::Ref(inner) => is_string_type(inner),
                    Type::RefMut(inner) => is_string_type(inner),
                    _ => false,
                }
            }

            // For string types (str, &str, ~str), handle appropriately
            if is_string_type(&ast_type) {
                // The value should already be a struct value (str is a struct {ptr, len})
                // If it's a pointer, that means we have a reference - skip the deref, let the
                // existing slice detection handle it
                if val.is_struct_value() {
                    return self.value_to_string(val, &Type::Str);
                }
            }
            // For other types that are not structs/enums, use value_to_string
            if !matches!(&ast_type, Type::Named { .. }) {
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
    pub(crate) fn struct_to_string(
        &mut self,
        val: BasicValueEnum<'ctx>,
        struct_info: &StructTypeInfo<'ctx>,
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
    pub(crate) fn enum_to_string(
        &mut self,
        val: BasicValueEnum<'ctx>,
        enum_info: &EnumTypeInfo<'ctx>,
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
    pub(crate) fn value_to_string(
        &mut self,
        val: BasicValueEnum<'ctx>,
        ast_type: &Type,
    ) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
        let i64_type = self.context.i64_type();

        match ast_type {
            Type::I8 | Type::I16 | Type::I32 => {
                self.int_to_string(val.into_int_value(), "%d")
            }
            Type::I64 => {
                self.int_to_string(val.into_int_value(), "%lld")
            }
            Type::U8 | Type::U16 | Type::U32 => {
                self.int_to_string(val.into_int_value(), "%u")
            }
            Type::U64 => {
                self.int_to_string(val.into_int_value(), "%llu")
            }
            Type::Bool => {
                self.bool_to_string(val.into_int_value())
            }
            Type::Char => {
                // char is u8, print as character
                self.char_to_string(val.into_int_value())
            }
            Type::F32 | Type::F64 => {
                self.float_to_string(val.into_float_value())
            }
            Type::Named { name, generics } => {
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
            Type::Str => {
                // str is a UTF-8 string slice (same representation as Slice<u8>)
                let struct_val = val.into_struct_value();
                let ptr = self.builder
                    .build_extract_value(struct_val, 0, "str_ptr")
                    .unwrap()
                    .into_pointer_value();
                let len = self.builder
                    .build_extract_value(struct_val, 1, "str_len")
                    .unwrap()
                    .into_int_value();
                Ok((ptr, len))
            }
            Type::Slice(inner) => {
                // Slice<u8> is also a string (for compatibility)
                if matches!(inner.as_ref(), Type::U8) {
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
    pub(crate) fn int_to_string(&mut self, val: inkwell::values::IntValue<'ctx>, fmt: &str) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
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
    pub(crate) fn bool_to_string(&mut self, val: inkwell::values::IntValue<'ctx>) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
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
    pub(crate) fn char_to_string(&mut self, val: inkwell::values::IntValue<'ctx>) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
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
    pub(crate) fn float_to_string(&mut self, val: inkwell::values::FloatValue<'ctx>) -> Result<(inkwell::values::PointerValue<'ctx>, inkwell::values::IntValue<'ctx>), CodegenError> {
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
