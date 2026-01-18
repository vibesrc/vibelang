//! Intrinsic function compilation - print, memory allocation, panic, etc.
//!
//! This module handles compilation of built-in intrinsic functions:
//!
//! - `io`: print/println functions
//! - `memory`: malloc, realloc, free, memcpy
//! - `panic`: panic and try operator (?)
//! - `ptr`: pointer operations (null, sizeof, ptr_read, ptr_write, etc.)
//! - `file`: file I/O syscalls (open, close, read, write, etc.)
//! - `process`: process syscalls (fork, exec, wait, etc.)
//! - `mmap`: memory mapping syscalls (mmap, munmap, etc.)
//! - `network`: networking syscalls (socket, bind, listen, etc.)

mod file;
mod io;
mod memory;
mod mmap;
mod network;
mod panic;
mod process;
mod ptr;

use super::{Codegen, CodegenError};
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    /// Helper to extract string pointer from Slice<u8> or raw pointer
    pub(crate) fn extract_string_ptr(&mut self, arg: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        if arg.is_struct_value() {
            let struct_val = arg.into_struct_value();
            self.builder.build_extract_value(struct_val, 0, "str_ptr").unwrap()
        } else {
            arg
        }
    }

    /// Helper to require unsafe context for dangerous operations
    pub(crate) fn require_unsafe(&self, func_name: &str) -> Result<(), CodegenError> {
        if !self.in_unsafe {
            Err(CodegenError::UnsafeRequired(func_name.to_string()))
        } else {
            Ok(())
        }
    }
}
