//! Intrinsic function compilation - print, memory allocation, panic, etc.

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_print_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::InvalidArguments("print requires an argument".to_string()));
        }

        let puts = self.module.get_function("puts").unwrap();

        // Compile the argument
        let arg = self.compile_expr(&args[0])?;

        // Call puts with the string pointer
        let call_site = self
            .builder
            .build_call(puts, &[arg.into()], "puts_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.i32_type().const_zero().into())
            }
        }
    }

    pub(crate) fn compile_print_int_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::InvalidArguments("print_int requires an argument".to_string()));
        }

        let printf = self.module.get_function("printf").unwrap();

        // Create format string "%d\n"
        let fmt_str = self.builder.build_global_string_ptr("%d\n", "int_fmt").unwrap();

        // Compile the argument
        let arg = self.compile_expr(&args[0])?;

        // Call printf with format and value
        let call_site = self
            .builder
            .build_call(printf, &[fmt_str.as_pointer_value().into(), arg.into()], "printf_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.i32_type().const_zero().into())
            }
        }
    }

    pub(crate) fn compile_malloc_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments("malloc requires 1 argument (size)".to_string()));
        }

        let malloc_fn = self.module.get_function("malloc").unwrap();
        let size = self.compile_expr(&args[0])?;

        let call_site = self.builder
            .build_call(malloc_fn, &[size.into()], "malloc_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.ptr_type(AddressSpace::default()).const_null().into())
            }
        }
    }

    pub(crate) fn compile_realloc_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments("realloc requires 2 arguments (ptr, size)".to_string()));
        }

        let realloc_fn = self.module.get_function("realloc").unwrap();
        let ptr = self.compile_expr(&args[0])?;
        let size = self.compile_expr(&args[1])?;

        let call_site = self.builder
            .build_call(realloc_fn, &[ptr.into(), size.into()], "realloc_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.ptr_type(AddressSpace::default()).const_null().into())
            }
        }
    }

    pub(crate) fn compile_free_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments("free requires 1 argument (ptr)".to_string()));
        }

        let free_fn = self.module.get_function("free").unwrap();
        let ptr = self.compile_expr(&args[0])?;

        self.builder.build_call(free_fn, &[ptr.into()], "").unwrap();

        // Return a dummy value since free returns void
        Ok(self.context.i64_type().const_zero().into())
    }

    pub(crate) fn compile_memcpy_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments("memcpy requires 3 arguments (dest, src, size)".to_string()));
        }

        let memcpy_fn = self.module.get_function("memcpy").unwrap();
        let dest = self.compile_expr(&args[0])?;
        let src = self.compile_expr(&args[1])?;
        let size = self.compile_expr(&args[2])?;

        let call_site = self.builder
            .build_call(memcpy_fn, &[dest.into(), src.into(), size.into()], "memcpy_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.ptr_type(AddressSpace::default()).const_null().into())
            }
        }
    }

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

    pub(crate) fn compile_ptr_write_i64(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // ptr_write_i64(ptr, index: i64, value: i64)
        // Writes value to *(ptr + index * 8)
        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments("ptr_write_i64 requires 3 arguments (ptr, index, value)".to_string()));
        }

        let ptr_val = self.compile_expr(&args[0])?;
        let index = self.compile_expr(&args[1])?.into_int_value();
        let value = self.compile_expr(&args[2])?.into_int_value();

        // Get pointer - could be a PointerValue or an IntValue to convert
        let ptr = if ptr_val.is_pointer_value() {
            ptr_val.into_pointer_value()
        } else {
            self.builder.build_int_to_ptr(
                ptr_val.into_int_value(),
                self.context.ptr_type(AddressSpace::default()),
                "ptr"
            ).unwrap()
        };

        // Calculate offset and get element pointer
        let elem_ptr = unsafe {
            self.builder.build_gep(
                self.context.i64_type(),
                ptr,
                &[index],
                "elem_ptr"
            ).unwrap()
        };

        // Store the value
        self.builder.build_store(elem_ptr, value).unwrap();

        Ok(self.context.i64_type().const_zero().into())
    }

    pub(crate) fn compile_ptr_read_i64(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // ptr_read_i64(ptr, index: i64) -> i64
        // Returns *(ptr + index * 8)
        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments("ptr_read_i64 requires 2 arguments (ptr, index)".to_string()));
        }

        let ptr_val = self.compile_expr(&args[0])?;
        let index = self.compile_expr(&args[1])?.into_int_value();

        // Get pointer - could be a PointerValue or an IntValue to convert
        let ptr = if ptr_val.is_pointer_value() {
            ptr_val.into_pointer_value()
        } else {
            self.builder.build_int_to_ptr(
                ptr_val.into_int_value(),
                self.context.ptr_type(AddressSpace::default()),
                "ptr"
            ).unwrap()
        };

        // Calculate offset and get element pointer
        let elem_ptr = unsafe {
            self.builder.build_gep(
                self.context.i64_type(),
                ptr,
                &[index],
                "elem_ptr"
            ).unwrap()
        };

        // Load and return the value
        let val = self.builder.build_load(self.context.i64_type(), elem_ptr, "val").unwrap();
        Ok(val)
    }
}
