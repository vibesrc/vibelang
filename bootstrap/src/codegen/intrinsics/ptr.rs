//! Pointer intrinsics: null, sizeof, ptr_null, ptr_is_null, ptr_write, ptr_read, ptr_add

use crate::codegen::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
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
        self.require_unsafe("ptr_write")?;

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
        self.require_unsafe("ptr_read")?;

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
        self.require_unsafe("ptr_add")?;

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
}
