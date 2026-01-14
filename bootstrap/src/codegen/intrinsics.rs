//! Intrinsic function compilation - print, memory allocation, panic, etc.

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    /// Helper to extract string pointer from Slice<u8> or raw pointer
    fn extract_string_ptr(&mut self, arg: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        if arg.is_struct_value() {
            let struct_val = arg.into_struct_value();
            self.builder.build_extract_value(struct_val, 0, "str_ptr").unwrap()
        } else {
            arg
        }
    }

    /// print(s) - print string without newline
    pub(crate) fn compile_print_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::InvalidArguments("print requires an argument".to_string()));
        }

        let printf = self.module.get_function("printf").unwrap();
        let fmt_str = self.builder.build_global_string_ptr("%s", "str_fmt").unwrap();
        let arg = self.compile_expr(&args[0])?;

        // Type check: print only accepts strings (Slice<u8> struct or pointer)
        if arg.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "print requires a string argument, got an integer. Use string interpolation: print(\"${x}\")".to_string()
            ));
        }

        let ptr_val = self.extract_string_ptr(arg);

        let call_site = self.builder
            .build_call(printf, &[fmt_str.as_pointer_value().into(), ptr_val.into()], "printf_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// println(s) - print string with newline
    pub(crate) fn compile_println_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::InvalidArguments("println requires an argument".to_string()));
        }

        let puts = self.module.get_function("puts").unwrap();
        let arg = self.compile_expr(&args[0])?;

        // Type check: println only accepts strings (Slice<u8> struct or pointer)
        if arg.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "println requires a string argument, got an integer. Use string interpolation: println(\"${x}\")".to_string()
            ));
        }

        let ptr_val = self.extract_string_ptr(arg);

        let call_site = self.builder
            .build_call(puts, &[ptr_val.into()], "puts_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    // Note: print_int and println_int have been removed.
    // Use string interpolation instead: println("${x}")

    pub(crate) fn compile_malloc_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments("malloc requires 1 argument (size)".to_string()));
        }

        let malloc_fn = self.module.get_function("malloc").unwrap();
        let size = self.compile_expr(&args[0])?;

        // Type check: size must be an integer
        if !size.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "malloc requires an integer size argument".to_string()
            ));
        }

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

        // Type check: ptr must be a pointer
        if !ptr.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "realloc requires a pointer as first argument".to_string()
            ));
        }

        // Type check: size must be an integer
        if !size.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "realloc requires an integer size as second argument".to_string()
            ));
        }

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

        // Type check: ptr must be a pointer
        if !ptr.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "free requires a pointer argument".to_string()
            ));
        }

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

        // Type check: dest must be a pointer
        if !dest.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "memcpy requires a pointer as dest (first argument)".to_string()
            ));
        }

        // Type check: src must be a pointer
        if !src.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "memcpy requires a pointer as src (second argument)".to_string()
            ));
        }

        // Type check: size must be an integer
        if !size.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "memcpy requires an integer size (third argument)".to_string()
            ));
        }

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
        let index_val = self.compile_expr(&args[1])?;
        let value_val = self.compile_expr(&args[2])?;

        // Type check: ptr must be a pointer or integer (for int-to-ptr conversion)
        if !ptr_val.is_pointer_value() && !ptr_val.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_write_i64 requires a pointer or integer as first argument".to_string()
            ));
        }

        // Type check: index must be an integer
        if !index_val.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_write_i64 requires an integer index as second argument".to_string()
            ));
        }

        // Type check: value must be an integer
        if !value_val.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_write_i64 requires an integer value as third argument".to_string()
            ));
        }

        let index = index_val.into_int_value();
        let value = value_val.into_int_value();

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
        let index_val = self.compile_expr(&args[1])?;

        // Type check: ptr must be a pointer or integer (for int-to-ptr conversion)
        if !ptr_val.is_pointer_value() && !ptr_val.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_read_i64 requires a pointer or integer as first argument".to_string()
            ));
        }

        // Type check: index must be an integer
        if !index_val.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_read_i64 requires an integer index as second argument".to_string()
            ));
        }

        let index = index_val.into_int_value();

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

    /// null<T>() - returns a null pointer of type *T
    pub(crate) fn compile_null_call(&mut self, _type_args: &[Type]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Return a null pointer (type doesn't matter in LLVM's opaque pointer model)
        Ok(self.context.ptr_type(AddressSpace::default()).const_null().into())
    }

    /// sizeof<T>() - returns the size of type T in bytes
    pub(crate) fn compile_sizeof_call(&mut self, type_args: &[Type]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if type_args.is_empty() {
            return Err(CodegenError::InvalidArguments(
                "sizeof requires a type argument: sizeof<T>()".to_string()
            ));
        }

        let llvm_type = self.llvm_type(&type_args[0])?;

        // Calculate size using LLVM's size_of method
        // This returns an IntValue<'ctx> representing the size
        let size_val = match llvm_type {
            inkwell::types::BasicTypeEnum::IntType(t) => t.size_of(),
            inkwell::types::BasicTypeEnum::FloatType(t) => t.size_of(),
            inkwell::types::BasicTypeEnum::PointerType(t) => t.size_of(),
            inkwell::types::BasicTypeEnum::ArrayType(t) => t.size_of().unwrap(),
            inkwell::types::BasicTypeEnum::StructType(t) => t.size_of().unwrap(),
            inkwell::types::BasicTypeEnum::VectorType(t) => t.size_of().unwrap(),
            inkwell::types::BasicTypeEnum::ScalableVectorType(_) => {
                return Err(CodegenError::NotImplemented(
                    "sizeof not supported for scalable vector types".to_string()
                ));
            }
        };

        Ok(size_val.into())
    }

    /// ptr_null<T>() - returns a null pointer of type *T
    pub(crate) fn compile_ptr_null_call(&mut self, _type_args: &[Type]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Return a null pointer (type doesn't matter in LLVM's opaque pointer model)
        Ok(self.context.ptr_type(AddressSpace::default()).const_null().into())
    }

    /// ptr_is_null<T>(ptr: *T) -> bool - check if pointer is null
    pub(crate) fn compile_ptr_is_null_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments(
                "ptr_is_null requires 1 argument (ptr)".to_string()
            ));
        }

        let ptr_val = self.compile_expr(&args[0])?;

        if !ptr_val.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_is_null requires a pointer argument".to_string()
            ));
        }

        let ptr = ptr_val.into_pointer_value();
        let null_ptr = self.context.ptr_type(AddressSpace::default()).const_null();

        // Compare pointer to null
        let is_null = self.builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                self.builder.build_ptr_to_int(ptr, self.context.i64_type(), "ptr_int").unwrap(),
                self.builder.build_ptr_to_int(null_ptr, self.context.i64_type(), "null_int").unwrap(),
                "is_null"
            )
            .unwrap();

        // Zero-extend i1 to i64 for bool representation
        let result = self.builder
            .build_int_z_extend(is_null, self.context.i64_type(), "is_null_i64")
            .unwrap();

        Ok(result.into())
    }

    /// ptr_write<T>(ptr: *T, value: T) - write value through typed pointer
    pub(crate) fn compile_ptr_write_call(&mut self, type_args: &[Type], args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "ptr_write requires 2 arguments (ptr, value)".to_string()
            ));
        }

        let ptr_val = self.compile_expr(&args[0])?;
        let value_val = self.compile_expr(&args[1])?;

        if !ptr_val.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_write requires a pointer as first argument".to_string()
            ));
        }

        let ptr = ptr_val.into_pointer_value();

        // If type args provided, we can verify the type, but for now just store
        let _ = type_args; // Type args help with type inference but aren't strictly needed for store

        // Store the value through the pointer
        self.builder.build_store(ptr, value_val).unwrap();

        Ok(self.context.i64_type().const_zero().into())
    }

    /// ptr_read<T>(ptr: *T) -> T - read value through typed pointer
    pub(crate) fn compile_ptr_read_call(&mut self, type_args: &[Type], args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments(
                "ptr_read requires 1 argument (ptr)".to_string()
            ));
        }

        let ptr_val = self.compile_expr(&args[0])?;

        if !ptr_val.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_read requires a pointer argument".to_string()
            ));
        }

        let ptr = ptr_val.into_pointer_value();

        // Determine the type to load
        let load_type = if !type_args.is_empty() {
            self.llvm_type(&type_args[0])?
        } else {
            // Default to i64 if no type argument provided
            self.context.i64_type().into()
        };

        // Load and return the value
        let val = self.builder.build_load(load_type, ptr, "ptr_read").unwrap();
        Ok(val)
    }

    /// ptr_add<T>(ptr: *T, offset: i64) -> *T - add offset to typed pointer
    pub(crate) fn compile_ptr_add_call(&mut self, type_args: &[Type], args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "ptr_add requires 2 arguments (ptr, offset)".to_string()
            ));
        }

        let ptr_val = self.compile_expr(&args[0])?;
        let offset_val = self.compile_expr(&args[1])?;

        if !ptr_val.is_pointer_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_add requires a pointer as first argument".to_string()
            ));
        }

        if !offset_val.is_int_value() {
            return Err(CodegenError::InvalidArguments(
                "ptr_add requires an integer offset as second argument".to_string()
            ));
        }

        let ptr = ptr_val.into_pointer_value();
        let offset = offset_val.into_int_value();

        // Determine element type for GEP
        let elem_type = if !type_args.is_empty() {
            self.llvm_type(&type_args[0])?
        } else {
            // Default to i8 for byte-wise pointer arithmetic
            self.context.i8_type().into()
        };

        // Use GEP to calculate the new pointer
        let new_ptr = unsafe {
            self.builder.build_gep(elem_type, ptr, &[offset], "ptr_add").unwrap()
        };

        Ok(new_ptr.into())
    }

    // ========== File I/O Syscalls ==========

    /// sys_open(path: *u8, flags: i32, mode: i32) -> i32
    /// Wrapper around Unix open() syscall
    pub(crate) fn compile_sys_open_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_open requires 3 arguments (path, flags, mode)".to_string()
            ));
        }

        let open_fn = self.module.get_function("open").unwrap();

        let path_val = self.compile_expr(&args[0])?;
        let flags_val = self.compile_expr(&args[1])?;
        let mode_val = self.compile_expr(&args[2])?;

        // Extract pointer from Slice<u8> if needed
        let path_ptr = self.extract_string_ptr(path_val);

        // Convert flags and mode to i32 if needed
        let flags = if flags_val.is_int_value() {
            let int_val = flags_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "flags_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_open flags must be an integer".to_string()
            ));
        };

        let mode = if mode_val.is_int_value() {
            let int_val = mode_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "mode_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_open mode must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(open_fn, &[path_ptr.into(), flags.into(), mode.into()], "open_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_close(fd: i32) -> i32
    /// Wrapper around Unix close() syscall
    pub(crate) fn compile_sys_close_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments(
                "sys_close requires 1 argument (fd)".to_string()
            ));
        }

        let close_fn = self.module.get_function("close").unwrap();

        let fd_val = self.compile_expr(&args[0])?;

        let fd = if fd_val.is_int_value() {
            let int_val = fd_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "fd_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_close fd must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(close_fn, &[fd.into()], "close_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_read(fd: i32, buf: *u8, count: i64) -> i64
    /// Wrapper around Unix read() syscall
    pub(crate) fn compile_sys_read_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_read requires 3 arguments (fd, buf, count)".to_string()
            ));
        }

        let read_fn = self.module.get_function("read").unwrap();

        let fd_val = self.compile_expr(&args[0])?;
        let buf_val = self.compile_expr(&args[1])?;
        let count_val = self.compile_expr(&args[2])?;

        let fd = if fd_val.is_int_value() {
            let int_val = fd_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "fd_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_read fd must be an integer".to_string()
            ));
        };

        // buf should be a pointer
        let buf_ptr = if buf_val.is_pointer_value() {
            buf_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_read buf must be a pointer".to_string()
            ));
        };

        let count = if count_val.is_int_value() {
            count_val.into_int_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_read count must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(read_fn, &[fd.into(), buf_ptr.into(), count.into()], "read_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }

    /// sys_write(fd: i32, buf: *u8, count: i64) -> i64
    /// Wrapper around Unix write() syscall
    pub(crate) fn compile_sys_write_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_write requires 3 arguments (fd, buf, count)".to_string()
            ));
        }

        let write_fn = self.module.get_function("write").unwrap();

        let fd_val = self.compile_expr(&args[0])?;
        let buf_val = self.compile_expr(&args[1])?;
        let count_val = self.compile_expr(&args[2])?;

        let fd = if fd_val.is_int_value() {
            let int_val = fd_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "fd_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_write fd must be an integer".to_string()
            ));
        };

        // buf can be a pointer or a Slice<u8>
        let buf_ptr = if buf_val.is_pointer_value() {
            buf_val.into_pointer_value()
        } else if buf_val.is_struct_value() {
            // Extract pointer from Slice<u8>
            let struct_val = buf_val.into_struct_value();
            self.builder.build_extract_value(struct_val, 0, "buf_ptr").unwrap().into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_write buf must be a pointer or Slice<u8>".to_string()
            ));
        };

        let count = if count_val.is_int_value() {
            count_val.into_int_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_write count must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(write_fn, &[fd.into(), buf_ptr.into(), count.into()], "write_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }

    /// sys_lseek(fd: i32, offset: i64, whence: i32) -> i64
    /// Wrapper around Unix lseek() syscall
    pub(crate) fn compile_sys_lseek_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_lseek requires 3 arguments (fd, offset, whence)".to_string()
            ));
        }

        let lseek_fn = self.module.get_function("lseek").unwrap();

        let fd_val = self.compile_expr(&args[0])?;
        let offset_val = self.compile_expr(&args[1])?;
        let whence_val = self.compile_expr(&args[2])?;

        let fd = if fd_val.is_int_value() {
            let int_val = fd_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "fd_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_lseek fd must be an integer".to_string()
            ));
        };

        let offset = if offset_val.is_int_value() {
            offset_val.into_int_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_lseek offset must be an integer".to_string()
            ));
        };

        let whence = if whence_val.is_int_value() {
            let int_val = whence_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "whence_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_lseek whence must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(lseek_fn, &[fd.into(), offset.into(), whence.into()], "lseek_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i64_type().const_zero().into()),
        }
    }
}
