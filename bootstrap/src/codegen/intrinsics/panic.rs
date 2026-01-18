//! Panic and error handling intrinsics: panic, try operator (?)

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_panic_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Print message if provided
        if !args.is_empty() {
            self.compile_print_call(args)?;
        }

        // Call exit(1) to terminate
        let exit_fn = self.module.get_function("exit").unwrap();
        self.builder.build_call(exit_fn, &[self.context.i32_type().const_int(1, false).into()], "").unwrap();

        // Mark as unreachable
        self.builder.build_unreachable().unwrap();

        // Return a dummy value
        Ok(self.context.i64_type().const_zero().into())
    }

    /// Compile the ? operator for Result/Option propagation
    pub(crate) fn compile_try_operator(&mut self, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let result_val = self.compile_expr(operand)?;

        // The result should be an enum (Result or Option) with { tag: i32, payload }
        // Tag 0 = Ok/Some, Tag 1 = Err/None
        if !result_val.is_struct_value() {
            return Err(CodegenError::NotImplemented(
                "? operator requires Result or Option type".to_string()
            ));
        }

        let struct_val = result_val.into_struct_value();
        let struct_ty = struct_val.get_type();

        // Store the enum to get a pointer for GEP operations
        let alloca = self.builder.build_alloca(struct_ty, "try_val").unwrap();
        self.builder.build_store(alloca, struct_val).unwrap();

        // Extract tag (field 0)
        let tag_ptr = self.builder
            .build_struct_gep(struct_ty, alloca, 0, "tag_ptr")
            .unwrap();
        let tag = self.builder.build_load(self.context.i32_type(), tag_ptr, "tag").unwrap().into_int_value();

        // Create basic blocks for success and error paths
        let ok_bb = self.context.append_basic_block(fn_value, "try.ok");
        let err_bb = self.context.append_basic_block(fn_value, "try.err");
        let merge_bb = self.context.append_basic_block(fn_value, "try.merge");

        // Compare tag: 0 = Ok/Some, non-zero = Err/None
        let zero = self.context.i32_type().const_zero();
        let is_ok = self.builder.build_int_compare(
            inkwell::IntPredicate::EQ, tag, zero, "is_ok"
        ).unwrap();
        self.builder.build_conditional_branch(is_ok, ok_bb, err_bb).unwrap();

        // Error path: return the error/None value
        self.builder.position_at_end(err_bb);
        // Execute deferred expressions before early return
        self.emit_deferred_exprs()?;
        // Load the original result and return it
        let err_val = self.builder.build_load(struct_ty, alloca, "err_val").unwrap();
        self.builder.build_return(Some(&err_val)).unwrap();

        // Success path: extract the Ok/Some payload
        self.builder.position_at_end(ok_bb);
        // Payload is at field 1
        let payload_ptr = self.builder
            .build_struct_gep(struct_ty, alloca, 1, "payload_ptr")
            .unwrap();

        // Determine payload type from struct field
        let payload_ty = struct_ty.get_field_type_at_index(1)
            .ok_or_else(|| CodegenError::NotImplemented("enum has no payload".to_string()))?;
        let payload = self.builder.build_load(payload_ty, payload_ptr, "payload").unwrap();

        self.builder.build_unconditional_branch(merge_bb).unwrap();

        // Merge block - continue with extracted value
        self.builder.position_at_end(merge_bb);

        // Use phi to get the payload value (only one incoming path for now)
        let phi = self.builder.build_phi(payload_ty, "try_result").unwrap();
        phi.add_incoming(&[(&payload, ok_bb)]);

        Ok(phi.as_basic_value())
    }
}
