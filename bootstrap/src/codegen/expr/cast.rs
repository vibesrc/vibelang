//! Type cast and tuple compilation

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
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
}
