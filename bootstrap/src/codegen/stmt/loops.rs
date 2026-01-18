//! For loop compilation: range, array, slice, and Vec iteration

use super::{Codegen, CodegenError, VarInfo};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::types::BasicTypeEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_for(
        &mut self,
        name: &str,
        iter: &Expr,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();

        // Handle different iterator types
        match iter {
            // Range iteration: for i in 0..5
            Expr::Range { start, end, .. } => {
                let start_val = self.compile_expr(start)?.into_int_value();
                let end_val = self.compile_expr(end)?.into_int_value();

                // Create loop variable
                let i64_type = self.context.i64_type();
                let loop_var = self.create_entry_block_alloca(name, i64_type.into());
                self.builder.build_store(loop_var, start_val).unwrap();

                self.variables.insert(name.to_string(), VarInfo {
                    ptr: loop_var,
                    ty: i64_type.into(),
                    struct_name: None,
                    ast_type: Some(Type::I64),
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                    slice_elem_type: None,
                });

                let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
                let body_bb = self.context.append_basic_block(fn_value, "for.body");
                let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
                let end_bb = self.context.append_basic_block(fn_value, "for.end");

                // Save outer loop targets and set new ones for break/continue
                let outer_break = self.loop_break_block;
                let outer_continue = self.loop_continue_block;
                self.loop_break_block = Some(end_bb);
                self.loop_continue_block = Some(inc_bb);

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                // Condition: i < end
                self.builder.position_at_end(cond_bb);
                let current = self.builder.build_load(i64_type, loop_var, "i").unwrap().into_int_value();
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT, current, end_val, "cmp"
                ).unwrap();
                self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

                // Body
                self.builder.position_at_end(body_bb);
                self.compile_block(body)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(inc_bb).unwrap();
                }

                // Increment: i = i + 1
                self.builder.position_at_end(inc_bb);
                let current = self.builder.build_load(i64_type, loop_var, "i").unwrap().into_int_value();
                let next = self.builder.build_int_add(current, i64_type.const_int(1, false), "inc").unwrap();
                self.builder.build_store(loop_var, next).unwrap();
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(end_bb);

                // Restore outer loop targets
                self.loop_break_block = outer_break;
                self.loop_continue_block = outer_continue;
            }

            // Array/Slice iteration: for x in arr or for x in slice
            Expr::Ident(arr_name, _) => {
                let var_info = self.variables.get(arr_name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(arr_name.clone()))?
                    .clone();

                // Check if this is a slice (struct with {ptr, len} layout)
                if var_info.ty.is_struct_type() {
                    let struct_ty = var_info.ty.into_struct_type();
                    // Slices are structs with 2 fields: { ptr, len }
                    if struct_ty.count_fields() == 2 {
                        return self.compile_for_slice(name, &var_info, body);
                    }
                    // Vec<T> is a struct with 3 fields: { ptr, len, capacity }
                    // Check if ast_type indicates this is a Vec<T>
                    if struct_ty.count_fields() == 3 {
                        // First check explicit ast_type
                        if let Some(Type::Named { name: type_name, generics }) = &var_info.ast_type {
                            if type_name == "Vec" && generics.len() == 1 {
                                // Get element type from the generic parameter
                                let elem_ast_type = &generics[0];
                                return self.compile_for_dynamic_array(name, &var_info, elem_ast_type, body);
                            }
                        }
                        // Fallback: check struct_name for mangled Vec types (e.g., "Vec_String")
                        if let Some(ref struct_name) = var_info.struct_name {
                            if let Some((base_name, type_args)) = self.parse_mangled_name(struct_name) {
                                if base_name == "Vec" && type_args.len() == 1 {
                                    let elem_ast_type = &type_args[0];
                                    return self.compile_for_dynamic_array(name, &var_info, elem_ast_type, body);
                                }
                            }
                        }
                    }
                }

                // Check if this is a reference to an array (e.g., &i32[1024])
                if var_info.is_ref {
                    if let Some(Type::Ref(inner)) = &var_info.ast_type {
                        if let Type::Array(elem_ty, size) = inner.as_ref() {
                            return self.compile_for_ref_array(name, arr_name, &var_info, elem_ty.as_ref(), *size, body);
                        }
                    }
                    // Also check RefMut (~T)
                    if let Some(Type::RefMut(inner)) = &var_info.ast_type {
                        if let Type::Array(elem_ty, size) = inner.as_ref() {
                            return self.compile_for_ref_array(name, arr_name, &var_info, elem_ty.as_ref(), *size, body);
                        }
                    }
                    return Err(CodegenError::NotImplemented(format!(
                        "cannot iterate over reference '{}'. Only references to arrays are iterable",
                        arr_name
                    )));
                }

                if !var_info.ty.is_array_type() {
                    return Err(CodegenError::NotImplemented(format!(
                        "variable '{}' is not iterable. For loops require arrays, slices, or ranges (e.g., '0..10')",
                        arr_name
                    )));
                }

                let array_ty = var_info.ty.into_array_type();
                let len = array_ty.len();
                let elem_type = array_ty.get_element_type();

                // Create index variable
                let i64_type = self.context.i64_type();
                let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
                self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

                // Create element variable
                let elem_var = self.create_entry_block_alloca(name, elem_type);
                self.variables.insert(name.to_string(), VarInfo {
                    ptr: elem_var,
                    ty: elem_type,
                    struct_name: None,
                    ast_type: self.llvm_type_to_ast_type(elem_type),
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                    slice_elem_type: None,
                });

                let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
                let body_bb = self.context.append_basic_block(fn_value, "for.body");
                let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
                let end_bb = self.context.append_basic_block(fn_value, "for.end");

                // Save outer loop targets and set new ones for break/continue
                let outer_break = self.loop_break_block;
                let outer_continue = self.loop_continue_block;
                self.loop_break_block = Some(end_bb);
                self.loop_continue_block = Some(inc_bb);

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                // Condition: idx < len
                self.builder.position_at_end(cond_bb);
                let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
                let len_val = i64_type.const_int(len as u64, false);
                let cond = self.builder.build_int_compare(
                    inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
                ).unwrap();
                self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

                // Body - load current element
                self.builder.position_at_end(body_bb);
                let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        var_info.ty,
                        var_info.ptr,
                        &[self.context.i32_type().const_zero(), current_idx],
                        "elem_ptr"
                    ).unwrap()
                };
                let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
                self.builder.build_store(elem_var, elem_val).unwrap();

                self.compile_block(body)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(inc_bb).unwrap();
                }

                // Increment index
                self.builder.position_at_end(inc_bb);
                let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
                let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
                self.builder.build_store(idx_var, next_idx).unwrap();
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(end_bb);

                // Restore outer loop targets
                self.loop_break_block = outer_break;
                self.loop_continue_block = outer_continue;
            }

            _ => return Err(CodegenError::NotImplemented(
                "for loop only supports arrays, slices, and ranges (e.g., '0..10'). \
                 Got an unsupported iterator type".to_string()
            )),
        }

        Ok(None)
    }

    pub(crate) fn compile_for_slice(
        &mut self,
        name: &str,
        var_info: &VarInfo<'ctx>,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let struct_ty = var_info.ty.into_struct_type();
        let i64_type = self.context.i64_type();

        // Load the slice value
        let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();

        // Extract pointer (field 0) and length (field 1) from slice
        let data_ptr = self.builder
            .build_extract_value(slice_val.into_struct_value(), 0, "slice_ptr")
            .unwrap()
            .into_pointer_value();
        let len_val = self.builder
            .build_extract_value(slice_val.into_struct_value(), 1, "slice_len")
            .unwrap()
            .into_int_value();

        // Get element type from tracked slice_elem_type, fallback to i64
        let elem_type: BasicTypeEnum = if let Some(ref ast_ty) = var_info.slice_elem_type {
            self.llvm_type(ast_ty)?
        } else {
            i64_type.into()
        };

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(name, elem_type);
        self.variables.insert(name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: None,
            ast_type: var_info.slice_elem_type.clone(),
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
            slice_elem_type: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(inc_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition: idx < len
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
        ).unwrap();
        self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

        // Body - load current element from slice
        self.builder.position_at_end(body_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let elem_ptr = unsafe {
            self.builder.build_gep(
                elem_type,
                data_ptr,
                &[current_idx],
                "slice_elem_ptr"
            ).unwrap()
        };
        let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
        self.builder.build_store(elem_var, elem_val).unwrap();

        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment index
        self.builder.position_at_end(inc_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
        self.builder.build_store(idx_var, next_idx).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }

    /// Compile for loop over a dynamic Vec<T> (struct with {ptr, len, capacity})
    pub(crate) fn compile_for_dynamic_array(
        &mut self,
        name: &str,
        var_info: &VarInfo<'ctx>,
        elem_ast_type: &Type,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let struct_ty = var_info.ty.into_struct_type();
        let i64_type = self.context.i64_type();

        // Load the Vec<T> value
        let array_val = self.builder.build_load(struct_ty, var_info.ptr, "array").unwrap();

        // Extract pointer (field 0) and length (field 1) from Vec<T>
        let data_ptr = self.builder
            .build_extract_value(array_val.into_struct_value(), 0, "array_ptr")
            .unwrap()
            .into_pointer_value();
        let len_val = self.builder
            .build_extract_value(array_val.into_struct_value(), 1, "array_len")
            .unwrap()
            .into_int_value();

        // Get element type from AST type
        let elem_type: BasicTypeEnum = self.llvm_type(elem_ast_type)?;

        // Get struct name for element if it's a struct type
        let elem_struct_name = self.get_struct_name_for_type(elem_ast_type);

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(name, elem_type);
        self.variables.insert(name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: elem_struct_name,
            ast_type: Some(elem_ast_type.clone()),
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
            slice_elem_type: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(inc_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition: idx < len
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
        ).unwrap();
        self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

        // Body - load current element from array
        self.builder.position_at_end(body_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let elem_ptr = unsafe {
            self.builder.build_gep(
                elem_type,
                data_ptr,
                &[current_idx],
                "array_elem_ptr"
            ).unwrap()
        };
        let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
        self.builder.build_store(elem_var, elem_val).unwrap();

        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment index
        self.builder.position_at_end(inc_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
        self.builder.build_store(idx_var, next_idx).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }

    /// Compile for loop over a reference to an array (e.g., for x in arr where arr: &i32[1024])
    pub(crate) fn compile_for_ref_array(
        &mut self,
        loop_var_name: &str,
        _arr_name: &str,
        var_info: &VarInfo<'ctx>,
        elem_ty: &Type,
        array_len: usize,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let i64_type = self.context.i64_type();

        // Get the LLVM type for elements
        let elem_type = self.llvm_type(elem_ty)?;

        // Get the array type for GEP - need to match on elem_type to get concrete type
        let array_type = match elem_type {
            BasicTypeEnum::IntType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::FloatType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::PointerType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::StructType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::ArrayType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::VectorType(t) => t.array_type(array_len as u32),
            BasicTypeEnum::ScalableVectorType(t) => t.array_type(array_len as u32),
        };

        // Dereference the pointer to get the array pointer
        // var_info.ptr points to the storage of the reference (ptr*)
        // We need to load it to get the actual array pointer
        let array_ptr = self.builder
            .build_load(self.context.ptr_type(inkwell::AddressSpace::default()), var_info.ptr, "deref_arr")
            .unwrap()
            .into_pointer_value();

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(loop_var_name, elem_type);
        self.variables.insert(loop_var_name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: None,
            ast_type: Some(elem_ty.clone()),
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
            slice_elem_type: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

        // Save outer loop targets and set new ones for break/continue
        let outer_break = self.loop_break_block;
        let outer_continue = self.loop_continue_block;
        self.loop_break_block = Some(end_bb);
        self.loop_continue_block = Some(inc_bb);

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition: idx < len
        self.builder.position_at_end(cond_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let len_val = i64_type.const_int(array_len as u64, false);
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT, current_idx, len_val, "cmp"
        ).unwrap();
        self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();

        // Body - load current element from array
        self.builder.position_at_end(body_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let elem_ptr = unsafe {
            self.builder.build_gep(
                array_type,
                array_ptr,
                &[self.context.i32_type().const_zero(), current_idx],
                "elem_ptr"
            ).unwrap()
        };
        let elem_val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
        self.builder.build_store(elem_var, elem_val).unwrap();

        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment index
        self.builder.position_at_end(inc_bb);
        let current_idx = self.builder.build_load(i64_type, idx_var, "idx").unwrap().into_int_value();
        let next_idx = self.builder.build_int_add(current_idx, i64_type.const_int(1, false), "inc").unwrap();
        self.builder.build_store(idx_var, next_idx).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(end_bb);

        // Restore outer loop targets
        self.loop_break_block = outer_break;
        self.loop_continue_block = outer_continue;

        Ok(None)
    }
}
