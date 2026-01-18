//! Memory mapping syscalls: mmap, munmap, mprotect, madvise

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    /// sys_mmap(addr: *u8, length: i64, prot: i32, flags: i32, fd: i32, offset: i64) -> *u8
    pub(crate) fn compile_sys_mmap_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_mmap")?;

        if args.len() != 6 {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap requires 6 arguments (addr, length, prot, flags, fd, offset)".to_string()
            ));
        }

        let mmap_fn = self.module.get_function("mmap")
            .ok_or_else(|| CodegenError::UndefinedFunction("mmap".to_string()))?;

        let addr_val = self.compile_expr(&args[0])?;
        let length_val = self.compile_expr(&args[1])?;
        let prot_val = self.compile_expr(&args[2])?;
        let flags_val = self.compile_expr(&args[3])?;
        let fd_val = self.compile_expr(&args[4])?;
        let offset_val = self.compile_expr(&args[5])?;

        let addr = if addr_val.is_pointer_value() {
            addr_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap addr must be a pointer".to_string()
            ));
        };

        let length = if length_val.is_int_value() {
            let int_val = length_val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "length_i64"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap length must be an integer".to_string()
            ));
        };

        let prot = if prot_val.is_int_value() {
            let int_val = prot_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "prot_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap prot must be an integer".to_string()
            ));
        };

        let flags = if flags_val.is_int_value() {
            let int_val = flags_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "flags_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap flags must be an integer".to_string()
            ));
        };

        let fd = if fd_val.is_int_value() {
            let int_val = fd_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "fd_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap fd must be an integer".to_string()
            ));
        };

        let offset = if offset_val.is_int_value() {
            let int_val = offset_val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "offset_i64"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mmap offset must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(mmap_fn, &[
                addr.into(),
                length.into(),
                prot.into(),
                flags.into(),
                fd.into(),
                offset.into(),
            ], "mmap_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.ptr_type(AddressSpace::default()).const_null().into())
            }
        }
    }

    /// sys_munmap(addr: *u8, length: i64) -> i32
    pub(crate) fn compile_sys_munmap_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_munmap")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_munmap requires 2 arguments (addr, length)".to_string()
            ));
        }

        let munmap_fn = self.module.get_function("munmap")
            .ok_or_else(|| CodegenError::UndefinedFunction("munmap".to_string()))?;

        let addr_val = self.compile_expr(&args[0])?;
        let length_val = self.compile_expr(&args[1])?;

        let addr = if addr_val.is_pointer_value() {
            addr_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_munmap addr must be a pointer".to_string()
            ));
        };

        let length = if length_val.is_int_value() {
            let int_val = length_val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "length_i64"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_munmap length must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(munmap_fn, &[addr.into(), length.into()], "munmap_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_mprotect(addr: *u8, len: i64, prot: i32) -> i32
    pub(crate) fn compile_sys_mprotect_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_mprotect")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_mprotect requires 3 arguments (addr, len, prot)".to_string()
            ));
        }

        let mprotect_fn = self.module.get_function("mprotect")
            .ok_or_else(|| CodegenError::UndefinedFunction("mprotect".to_string()))?;

        let addr_val = self.compile_expr(&args[0])?;
        let len_val = self.compile_expr(&args[1])?;
        let prot_val = self.compile_expr(&args[2])?;

        let addr = if addr_val.is_pointer_value() {
            addr_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mprotect addr must be a pointer".to_string()
            ));
        };

        let len = if len_val.is_int_value() {
            let int_val = len_val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "len_i64"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mprotect len must be an integer".to_string()
            ));
        };

        let prot = if prot_val.is_int_value() {
            let int_val = prot_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "prot_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_mprotect prot must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(mprotect_fn, &[addr.into(), len.into(), prot.into()], "mprotect_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_madvise(addr: *u8, length: i64, advice: i32) -> i32
    pub(crate) fn compile_sys_madvise_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_madvise")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_madvise requires 3 arguments (addr, length, advice)".to_string()
            ));
        }

        let madvise_fn = self.module.get_function("madvise")
            .ok_or_else(|| CodegenError::UndefinedFunction("madvise".to_string()))?;

        let addr_val = self.compile_expr(&args[0])?;
        let length_val = self.compile_expr(&args[1])?;
        let advice_val = self.compile_expr(&args[2])?;

        let addr = if addr_val.is_pointer_value() {
            addr_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_madvise addr must be a pointer".to_string()
            ));
        };

        let length = if length_val.is_int_value() {
            let int_val = length_val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "length_i64"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_madvise length must be an integer".to_string()
            ));
        };

        let advice = if advice_val.is_int_value() {
            let int_val = advice_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "advice_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_madvise advice must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(madvise_fn, &[addr.into(), length.into(), advice.into()], "madvise_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }
}
