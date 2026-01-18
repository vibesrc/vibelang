//! Closure compilation

use crate::codegen::{Codegen, CodegenError, VarInfo, capture};
use crate::ast::*;
use inkwell::values::BasicValueEnum;
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    /// Compile a closure expression
    pub(crate) fn compile_closure(
        &mut self,
        params: &[(String, Option<Type>)],
        return_type: Option<&Type>,
        body: &ClosureBody,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Analyze the closure body for captured variables
        let captures = capture::collect_free_variables(params, body);

        // Filter to only include variables that exist in our scope
        let valid_captures: Vec<_> = captures.into_iter()
            .filter(|c| self.variables.contains_key(&c.name))
            .collect();

        // Generate a unique name for the closure function
        let closure_id = self.closure_counter;
        self.closure_counter += 1;
        let closure_name = format!("__closure_{}", closure_id);

        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Build parameter types: env_ptr first, then user params
        let mut all_param_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = Vec::new();
        all_param_types.push(ptr_type.into()); // env_ptr is always first

        let mut user_param_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = Vec::new();
        for (_, ty) in params.iter() {
            let llvm_ty = match ty {
                Some(t) => self.llvm_type(t).unwrap_or(self.context.i32_type().into()),
                None => self.context.i32_type().into(), // default to i32
            };
            user_param_types.push(llvm_ty);
            all_param_types.push(llvm_ty.into());
        }

        // Determine return type
        let ret_type = match return_type {
            Some(t) => self.llvm_type(t)?,
            None => self.context.i32_type().into(), // default to i32
        };

        // Create function type with env_ptr as first param
        let fn_type = match ret_type {
            inkwell::types::BasicTypeEnum::IntType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::FloatType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::PointerType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::StructType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::ArrayType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::VectorType(t) => t.fn_type(&all_param_types, false),
            inkwell::types::BasicTypeEnum::ScalableVectorType(_) => {
                return Err(CodegenError::NotImplemented("scalable vector types not supported in closures".to_string()));
            }
        };

        // Create the closure function
        let closure_fn = self.module.add_function(&closure_name, fn_type, None);

        // Collect information about captured variables before we clear self.variables
        let captured_vars_info: Vec<_> = valid_captures.iter().map(|cap| {
            let var_info = self.variables.get(&cap.name).unwrap().clone();
            (cap.name.clone(), var_info, cap.is_mutated)
        }).collect();

        // Build the environment struct type
        // - Immutable captures: store the value directly
        // - Mutable captures: store a pointer to a heap-allocated "box"
        let env_struct_type = if !captured_vars_info.is_empty() {
            let field_types: Vec<inkwell::types::BasicTypeEnum> = captured_vars_info.iter()
                .map(|(_, var_info, is_mutated)| {
                    if *is_mutated {
                        // Mutable captures store a pointer to the boxed value
                        ptr_type.into()
                    } else {
                        // Immutable captures store the value directly
                        var_info.ty
                    }
                })
                .collect();
            Some(self.context.struct_type(&field_types, false))
        } else {
            None
        };

        // Save current state
        let saved_fn = self.current_function;
        let saved_block = self.builder.get_insert_block();
        let saved_vars = self.variables.clone();

        // Set up the closure function
        self.current_function = Some(closure_fn);
        let entry = self.context.append_basic_block(closure_fn, "entry");
        self.builder.position_at_end(entry);

        // Clear variables for the closure scope
        self.variables.clear();

        // Bind captured variables from env_ptr
        // - Immutable captures: env field contains the value directly
        // - Mutable captures: env field contains a pointer to the boxed value
        if let Some(env_type) = env_struct_type {
            let env_ptr_param = closure_fn.get_nth_param(0).unwrap().into_pointer_value();

            for (i, (name, var_info, is_mutated)) in captured_vars_info.iter().enumerate() {
                // Get pointer to the field in the environment struct
                let cap_field_ptr = self.builder
                    .build_struct_gep(env_type, env_ptr_param, i as u32, &format!("cap_{}", name))
                    .unwrap();

                let var_ptr = if *is_mutated {
                    // Mutable capture: env field contains a pointer to the box
                    // Load the pointer to get the actual storage location
                    self.builder
                        .build_load(ptr_type, cap_field_ptr, &format!("box_{}", name))
                        .unwrap()
                        .into_pointer_value()
                } else {
                    // Immutable capture: value is stored directly in env field
                    cap_field_ptr
                };

                self.variables.insert(name.clone(), VarInfo {
                    ptr: var_ptr,
                    ty: var_info.ty,
                    struct_name: var_info.struct_name.clone(),
                    ast_type: var_info.ast_type.clone(),
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: var_info.ref_struct_name.clone(),
                    slice_elem_type: var_info.slice_elem_type.clone(),
                });
            }
        }

        // Bind user parameters (starting from index 1, skip env_ptr)
        for (i, (name, ty)) in params.iter().enumerate() {
            let param_val = closure_fn.get_nth_param((i + 1) as u32).unwrap(); // +1 to skip env_ptr
            let param_type = user_param_types[i];

            let alloca = self.create_entry_block_alloca(name, param_type);
            self.builder.build_store(alloca, param_val).unwrap();

            self.variables.insert(name.clone(), VarInfo {
                ptr: alloca,
                ty: param_type,
                struct_name: ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)),
                ast_type: ty.clone(),
                is_ref: false,
                is_mut_ref: false,
                ref_struct_name: None,
                slice_elem_type: None,
            });
        }

        // Compile the body
        match body {
            ClosureBody::Expr(expr) => {
                let result = self.compile_expr(expr)?;
                self.builder.build_return(Some(&result)).unwrap();
            }
            ClosureBody::Block(block) => {
                self.compile_block(block)?;
                // If no return was encountered, return void equivalent
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_return(Some(&self.context.i32_type().const_zero())).unwrap();
                }
            }
        }

        // Restore state
        self.current_function = saved_fn;
        self.variables = saved_vars;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        // Build the environment struct if we have captures
        let env_ptr_val = if let Some(env_type) = env_struct_type {
            // Allocate the environment struct on the HEAP so it survives after the
            // creating function returns (closures may escape their defining scope)
            let malloc_fn = self.module.get_function("malloc").unwrap();
            let env_size = env_type.size_of().unwrap();
            let call_result = self.builder
                .build_call(malloc_fn, &[env_size.into()], "closure_env_heap")
                .unwrap();
            let env_ptr = match call_result.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
                _ => return Err(CodegenError::NotImplemented("malloc call failed for closure env".to_string())),
            };

            // Fill the environment struct with captured variables
            // - Immutable captures: copy value directly into env
            // - Mutable captures: allocate a "box" on heap, store pointer in env
            for (i, (name, var_info, is_mutated)) in captured_vars_info.iter().enumerate() {
                let current_var_info = self.variables.get(name).unwrap().clone();

                let field_ptr = self.builder
                    .build_struct_gep(env_type, env_ptr, i as u32, &format!("env_{}", name))
                    .unwrap();

                if *is_mutated {
                    // Mutable capture: create a heap-allocated "box" for the value
                    // This allows both the closure and outer scope to share the same storage
                    let value_size = match var_info.ty {
                        inkwell::types::BasicTypeEnum::IntType(t) => t.size_of(),
                        inkwell::types::BasicTypeEnum::FloatType(t) => t.size_of(),
                        inkwell::types::BasicTypeEnum::PointerType(t) => t.size_of(),
                        inkwell::types::BasicTypeEnum::ArrayType(t) => t.size_of().unwrap(),
                        inkwell::types::BasicTypeEnum::StructType(t) => t.size_of().unwrap(),
                        inkwell::types::BasicTypeEnum::VectorType(t) => t.size_of().unwrap(),
                        inkwell::types::BasicTypeEnum::ScalableVectorType(_) => {
                            return Err(CodegenError::NotImplemented(
                                "mutable capture not supported for scalable vector types".to_string()
                            ));
                        }
                    };
                    let box_result = self.builder
                        .build_call(malloc_fn, &[value_size.into()], &format!("box_{}", name))
                        .unwrap();
                    let box_ptr = match box_result.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => val.into_pointer_value(),
                        _ => return Err(CodegenError::NotImplemented("malloc call failed for capture box".to_string())),
                    };

                    // Copy the current value into the box
                    let value = self.builder
                        .build_load(var_info.ty, current_var_info.ptr, &format!("cap_val_{}", name))
                        .unwrap();
                    self.builder.build_store(box_ptr, value).unwrap();

                    // Store the box pointer in the environment struct
                    self.builder.build_store(field_ptr, box_ptr).unwrap();

                    // Update the outer scope's variable to point to the box
                    // This way both the closure and outer scope share the same storage
                    if let Some(outer_var) = self.variables.get_mut(name) {
                        outer_var.ptr = box_ptr;
                    }
                } else {
                    // Immutable capture: copy value directly into env
                    let value = self.builder
                        .build_load(var_info.ty, current_var_info.ptr, &format!("cap_val_{}", name))
                        .unwrap();
                    self.builder.build_store(field_ptr, value).unwrap();
                }
            }

            env_ptr
        } else {
            ptr_type.const_null()
        };

        // Build fat pointer struct: {fn_ptr, env_ptr}
        let fn_ptr = closure_fn.as_global_value().as_pointer_value();

        let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);
        let fat_ptr_alloca = self.create_entry_block_alloca("closure_fat_ptr", fat_ptr_type.into());

        // Store fn_ptr at index 0
        let fn_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 0, "fn_ptr_gep").unwrap();
        self.builder.build_store(fn_ptr_gep, fn_ptr).unwrap();

        // Store env_ptr at index 1
        let env_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 1, "env_ptr_gep").unwrap();
        self.builder.build_store(env_ptr_gep, env_ptr_val).unwrap();

        // Load and return the fat pointer
        let fat_ptr_val = self.builder.build_load(fat_ptr_type, fat_ptr_alloca, "closure_val").unwrap();
        Ok(fat_ptr_val)
    }

    /// Coerce a named function to a closure fat pointer.
    /// This generates a thunk function that has the closure calling convention
    /// (env_ptr as first parameter) and wraps the original function.
    pub(crate) fn coerce_function_to_closure(
        &mut self,
        name: &str,
        func: inkwell::values::FunctionValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        // Check if we already generated a thunk for this function
        let thunk_name = format!("__thunk_{}", name);
        let thunk_fn = if let Some(existing) = self.module.get_function(&thunk_name) {
            existing
        } else {
            // Generate a thunk function with closure calling convention:
            // fn __thunk_name(env_ptr: *void, params...) -> ReturnType
            let original_type = func.get_type();
            let original_params: Vec<_> = original_type.get_param_types();
            let return_type = original_type.get_return_type();

            // Build thunk parameter types: env_ptr first, then original params
            let mut thunk_params: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
            thunk_params.push(ptr_type.into()); // env_ptr (will be ignored)
            for param in &original_params {
                thunk_params.push((*param).into());
            }

            // Create thunk function type
            let thunk_type = match return_type {
                Some(inkwell::types::BasicTypeEnum::IntType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::FloatType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::PointerType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::StructType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::ArrayType(t)) => t.fn_type(&thunk_params, false),
                Some(inkwell::types::BasicTypeEnum::VectorType(t)) => t.fn_type(&thunk_params, false),
                None => self.context.void_type().fn_type(&thunk_params, false),
                _ => return Err(CodegenError::NotImplemented(
                    format!("unsupported return type for function coercion: {}", name)
                )),
            };

            // Create thunk function
            let thunk = self.module.add_function(&thunk_name, thunk_type, None);

            // Save current state
            let saved_fn = self.current_function;
            let saved_block = self.builder.get_insert_block();

            // Build thunk body
            self.current_function = Some(thunk);
            let entry = self.context.append_basic_block(thunk, "entry");
            self.builder.position_at_end(entry);

            // Collect arguments (skip env_ptr at index 0)
            let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
            for i in 0..original_params.len() {
                let param = thunk.get_nth_param((i + 1) as u32).unwrap(); // +1 to skip env_ptr
                call_args.push(param.into());
            }

            // Call original function
            let call_result = self.builder.build_call(func, &call_args, "thunk_call").unwrap();

            // Return result (or void)
            if return_type.is_some() {
                match call_result.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(ret_val) => {
                        self.builder.build_return(Some(&ret_val)).unwrap();
                    }
                    _ => {
                        self.builder.build_return(None).unwrap();
                    }
                }
            } else {
                self.builder.build_return(None).unwrap();
            }

            // Restore state
            self.current_function = saved_fn;
            if let Some(block) = saved_block {
                self.builder.position_at_end(block);
            }

            thunk
        };

        // Build fat pointer struct: {thunk_fn_ptr, null}
        let fn_ptr = thunk_fn.as_global_value().as_pointer_value();
        let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);
        let fat_ptr_alloca = self.create_entry_block_alloca("fn_closure_ptr", fat_ptr_type.into());

        // Store fn_ptr at index 0
        let fn_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 0, "fn_ptr_gep").unwrap();
        self.builder.build_store(fn_ptr_gep, fn_ptr).unwrap();

        // Store null env_ptr at index 1
        let env_ptr_gep = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 1, "env_ptr_gep").unwrap();
        self.builder.build_store(env_ptr_gep, ptr_type.const_null()).unwrap();

        // Load and return the fat pointer
        let fat_ptr_val = self.builder.build_load(fat_ptr_type, fat_ptr_alloca, "fn_as_closure").unwrap();
        Ok(fat_ptr_val)
    }
}
