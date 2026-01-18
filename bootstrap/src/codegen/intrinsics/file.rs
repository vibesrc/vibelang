//! File I/O syscalls: open, close, read, write, lseek, clock_gettime, nanosleep

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    /// sys_open(path: *u8, flags: i32, mode: i32) -> i32
    pub(crate) fn compile_sys_open_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_open")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_open requires 3 arguments (path, flags, mode)".to_string()
            ));
        }

        let open_fn = self.module.get_function("open").unwrap();

        let path_val = self.compile_expr(&args[0])?;
        let flags_val = self.compile_expr(&args[1])?;
        let mode_val = self.compile_expr(&args[2])?;

        let path_ptr = self.extract_string_ptr(path_val);

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
    pub(crate) fn compile_sys_close_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_close")?;

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
    pub(crate) fn compile_sys_read_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_read")?;

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
    pub(crate) fn compile_sys_write_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_write")?;

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

        let buf_ptr = if buf_val.is_pointer_value() {
            buf_val.into_pointer_value()
        } else if buf_val.is_struct_value() {
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
    pub(crate) fn compile_sys_lseek_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_lseek")?;

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

    /// sys_clock_gettime(clockid: i32, timespec_ptr: *u8) -> i32
    pub(crate) fn compile_sys_clock_gettime_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_clock_gettime")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_clock_gettime requires 2 arguments (clockid, timespec_ptr)".to_string()
            ));
        }

        let clock_gettime_fn = self.module.get_function("clock_gettime").unwrap();

        let clockid_val = self.compile_expr(&args[0])?;
        let ptr_val = self.compile_expr(&args[1])?;

        let clockid = if clockid_val.is_int_value() {
            let int_val = clockid_val.into_int_value();
            self.builder.build_int_truncate(int_val, self.context.i32_type(), "clockid_i32").unwrap()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_clock_gettime clockid must be an integer".to_string()
            ));
        };

        let ptr = if ptr_val.is_pointer_value() {
            ptr_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_clock_gettime timespec_ptr must be a pointer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(clock_gettime_fn, &[clockid.into(), ptr.into()], "clock_gettime_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_nanosleep(req_ptr: *u8, rem_ptr: *u8) -> i32
    pub(crate) fn compile_sys_nanosleep_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_nanosleep")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_nanosleep requires 2 arguments (req_ptr, rem_ptr)".to_string()
            ));
        }

        let nanosleep_fn = self.module.get_function("nanosleep").unwrap();

        let req_val = self.compile_expr(&args[0])?;
        let rem_val = self.compile_expr(&args[1])?;

        let req_ptr = if req_val.is_pointer_value() {
            req_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_nanosleep req_ptr must be a pointer".to_string()
            ));
        };

        let rem_ptr = if rem_val.is_pointer_value() {
            rem_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_nanosleep rem_ptr must be a pointer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(nanosleep_fn, &[req_ptr.into(), rem_ptr.into()], "nanosleep_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }
}
