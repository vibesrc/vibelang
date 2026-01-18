//! Literal expression compilation

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
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
}
