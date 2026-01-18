//! Memory allocation intrinsics: malloc, realloc, free, memcpy

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
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
}
