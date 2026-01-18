//! Process syscalls: getpid, getppid, exit, getcwd, chdir, getenv, setenv, fork, execve, waitpid, kill

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    /// sys_getpid() -> i32
    pub(crate) fn compile_sys_getpid_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_getpid")?;

        if !args.is_empty() {
            return Err(CodegenError::InvalidArguments(
                "sys_getpid takes no arguments".to_string()
            ));
        }

        let getpid_fn = self.module.get_function("getpid")
            .ok_or_else(|| CodegenError::UndefinedFunction("getpid".to_string()))?;

        let call_site = self.builder
            .build_call(getpid_fn, &[], "getpid_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_getppid() -> i32
    pub(crate) fn compile_sys_getppid_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_getppid")?;

        if !args.is_empty() {
            return Err(CodegenError::InvalidArguments(
                "sys_getppid takes no arguments".to_string()
            ));
        }

        let getppid_fn = self.module.get_function("getppid")
            .ok_or_else(|| CodegenError::UndefinedFunction("getppid".to_string()))?;

        let call_site = self.builder
            .build_call(getppid_fn, &[], "getppid_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_exit(status: i32) -> !
    pub(crate) fn compile_sys_exit_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_exit")?;

        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments(
                "sys_exit requires 1 argument (status)".to_string()
            ));
        }

        let exit_fn = self.module.get_function("exit")
            .ok_or_else(|| CodegenError::UndefinedFunction("exit".to_string()))?;

        let status_val = self.compile_expr(&args[0])?;

        let status = if status_val.is_int_value() {
            let int_val = status_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "status_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_exit status must be an integer".to_string()
            ));
        };

        self.builder.build_call(exit_fn, &[status.into()], "").unwrap();
        self.builder.build_unreachable().unwrap();

        Ok(self.context.i32_type().const_zero().into())
    }

    /// sys_getcwd(buf: *u8, size: i64) -> *u8
    pub(crate) fn compile_sys_getcwd_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_getcwd")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_getcwd requires 2 arguments (buf, size)".to_string()
            ));
        }

        let getcwd_fn = self.module.get_function("getcwd")
            .ok_or_else(|| CodegenError::UndefinedFunction("getcwd".to_string()))?;

        let buf_val = self.compile_expr(&args[0])?;
        let size_val = self.compile_expr(&args[1])?;

        let buf_ptr = if buf_val.is_pointer_value() {
            buf_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_getcwd buf must be a pointer".to_string()
            ));
        };

        let size = if size_val.is_int_value() {
            let int_val = size_val.into_int_value();
            if int_val.get_type().get_bit_width() != 64 {
                self.builder.build_int_s_extend_or_bit_cast(
                    int_val, self.context.i64_type(), "size_i64"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_getcwd size must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(getcwd_fn, &[buf_ptr.into(), size.into()], "getcwd_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.ptr_type(AddressSpace::default()).const_null().into()),
        }
    }

    /// sys_chdir(path: *u8) -> i32
    pub(crate) fn compile_sys_chdir_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_chdir")?;
        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments(
                "sys_chdir requires 1 argument (path)".to_string()
            ));
        }

        let chdir_fn = self.module.get_function("chdir")
            .ok_or_else(|| CodegenError::UndefinedFunction("chdir".to_string()))?;

        let path_val = self.compile_expr(&args[0])?;

        let path_ptr = if path_val.is_pointer_value() {
            path_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_chdir path must be a pointer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(chdir_fn, &[path_ptr.into()], "chdir_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_getenv(name: *u8) -> *u8
    pub(crate) fn compile_sys_getenv_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_getenv")?;

        if args.len() != 1 {
            return Err(CodegenError::InvalidArguments(
                "sys_getenv requires 1 argument (name)".to_string()
            ));
        }

        let getenv_fn = self.module.get_function("getenv")
            .ok_or_else(|| CodegenError::UndefinedFunction("getenv".to_string()))?;

        let name_val = self.compile_expr(&args[0])?;

        let name_ptr = if name_val.is_pointer_value() {
            name_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_getenv name must be a pointer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(getenv_fn, &[name_ptr.into()], "getenv_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.ptr_type(AddressSpace::default()).const_null().into()),
        }
    }

    /// sys_setenv(name: *u8, value: *u8, overwrite: i32) -> i32
    pub(crate) fn compile_sys_setenv_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_setenv")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_setenv requires 3 arguments (name, value, overwrite)".to_string()
            ));
        }

        let setenv_fn = self.module.get_function("setenv")
            .ok_or_else(|| CodegenError::UndefinedFunction("setenv".to_string()))?;

        let name_val = self.compile_expr(&args[0])?;
        let value_val = self.compile_expr(&args[1])?;
        let overwrite_val = self.compile_expr(&args[2])?;

        let name_ptr = if name_val.is_pointer_value() {
            name_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_setenv name must be a pointer".to_string()
            ));
        };

        let value_ptr = if value_val.is_pointer_value() {
            value_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_setenv value must be a pointer".to_string()
            ));
        };

        let overwrite = if overwrite_val.is_int_value() {
            let int_val = overwrite_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "overwrite_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_setenv overwrite must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(setenv_fn, &[name_ptr.into(), value_ptr.into(), overwrite.into()], "setenv_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_fork() -> i32
    pub(crate) fn compile_sys_fork_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_fork")?;

        if !args.is_empty() {
            return Err(CodegenError::InvalidArguments(
                "sys_fork takes no arguments".to_string()
            ));
        }

        let fork_fn = self.module.get_function("fork")
            .ok_or_else(|| CodegenError::UndefinedFunction("fork".to_string()))?;

        let call_site = self.builder
            .build_call(fork_fn, &[], "fork_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_execve(pathname: *u8, argv: **u8, envp: **u8) -> i32
    pub(crate) fn compile_sys_execve_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_execve")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_execve requires 3 arguments (pathname, argv, envp)".to_string()
            ));
        }

        let execve_fn = self.module.get_function("execve")
            .ok_or_else(|| CodegenError::UndefinedFunction("execve".to_string()))?;

        let pathname_val = self.compile_expr(&args[0])?;
        let argv_val = self.compile_expr(&args[1])?;
        let envp_val = self.compile_expr(&args[2])?;

        let pathname_ptr = if pathname_val.is_pointer_value() {
            pathname_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_execve pathname must be a pointer".to_string()
            ));
        };

        let argv_ptr = if argv_val.is_pointer_value() {
            argv_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_execve argv must be a pointer".to_string()
            ));
        };

        let envp_ptr = if envp_val.is_pointer_value() {
            envp_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_execve envp must be a pointer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(execve_fn, &[pathname_ptr.into(), argv_ptr.into(), envp_ptr.into()], "execve_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_int(u64::MAX, true).into()),
        }
    }

    /// sys_waitpid(pid: i32, status: *i32, options: i32) -> i32
    pub(crate) fn compile_sys_waitpid_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_waitpid")?;

        if args.len() != 3 {
            return Err(CodegenError::InvalidArguments(
                "sys_waitpid requires 3 arguments (pid, status, options)".to_string()
            ));
        }

        let waitpid_fn = self.module.get_function("waitpid")
            .ok_or_else(|| CodegenError::UndefinedFunction("waitpid".to_string()))?;

        let pid_val = self.compile_expr(&args[0])?;
        let status_val = self.compile_expr(&args[1])?;
        let options_val = self.compile_expr(&args[2])?;

        let pid = if pid_val.is_int_value() {
            let int_val = pid_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "pid_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_waitpid pid must be an integer".to_string()
            ));
        };

        let status_ptr = if status_val.is_pointer_value() {
            status_val.into_pointer_value()
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_waitpid status must be a pointer".to_string()
            ));
        };

        let options = if options_val.is_int_value() {
            let int_val = options_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "options_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_waitpid options must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(waitpid_fn, &[pid.into(), status_ptr.into(), options.into()], "waitpid_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }

    /// sys_kill(pid: i32, sig: i32) -> i32
    pub(crate) fn compile_sys_kill_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.require_unsafe("sys_kill")?;

        if args.len() != 2 {
            return Err(CodegenError::InvalidArguments(
                "sys_kill requires 2 arguments (pid, sig)".to_string()
            ));
        }

        let kill_fn = self.module.get_function("kill")
            .ok_or_else(|| CodegenError::UndefinedFunction("kill".to_string()))?;

        let pid_val = self.compile_expr(&args[0])?;
        let sig_val = self.compile_expr(&args[1])?;

        let pid = if pid_val.is_int_value() {
            let int_val = pid_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "pid_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_kill pid must be an integer".to_string()
            ));
        };

        let sig = if sig_val.is_int_value() {
            let int_val = sig_val.into_int_value();
            if int_val.get_type().get_bit_width() != 32 {
                self.builder.build_int_truncate_or_bit_cast(
                    int_val, self.context.i32_type(), "sig_i32"
                ).unwrap()
            } else {
                int_val
            }
        } else {
            return Err(CodegenError::InvalidArguments(
                "sys_kill sig must be an integer".to_string()
            ));
        };

        let call_site = self.builder
            .build_call(kill_fn, &[pid.into(), sig.into()], "kill_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => Ok(self.context.i32_type().const_zero().into()),
        }
    }
}
