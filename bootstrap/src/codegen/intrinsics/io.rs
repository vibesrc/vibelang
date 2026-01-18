//! I/O intrinsics: print, println

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
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
}
