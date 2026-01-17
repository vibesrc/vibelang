//! Function and method call compilation

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_call(&mut self, func: &Expr, type_args: &[Type], args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Check for module-qualified static method call: fs.File.create()
        // Pattern: Field { object: Field { object: Ident(module), field: Type }, field: method }
        if let Expr::Field { object: outer_obj, field: method, .. } = func {
            if let Expr::Field { object: inner_obj, field: type_name, .. } = outer_obj.as_ref() {
                if let Expr::Ident(module_name, _) = inner_obj.as_ref() {
                    if self.module_aliases.contains_key(module_name) {
                        // Resolve as module_Type (e.g., fs.File -> fs_File)
                        let qualified_type = format!("{}_{}", module_name, type_name);
                        let resolved_type = self.imports.get(&qualified_type)
                            .cloned()
                            .unwrap_or(qualified_type);

                        // Check if it's a static method call on a struct
                        if self.struct_types.contains_key(&resolved_type) {
                            return self.compile_static_method_call(&resolved_type, method, args);
                        }

                        // Check if it's an enum variant constructor
                        if let Some(enum_info) = self.enum_types.get(&resolved_type).cloned() {
                            return self.compile_enum_variant_constructor(&enum_info, &resolved_type, method, args);
                        }
                    }
                }
            }
        }

        // Check for enum variant constructor: EnumName.Variant(args)
        if let Expr::Field { object, field, .. } = func {
            // Non-generic enum: Expr::Ident
            if let Expr::Ident(enum_name, _) = object.as_ref() {
                // Check for enum variant constructor
                if let Some(enum_info) = self.enum_types.get(enum_name).cloned() {
                    return self.compile_enum_variant_constructor(&enum_info, enum_name, field, args);
                }

                // Check for module-qualified function call: fs.read_file()
                if self.module_aliases.contains_key(enum_name) {
                    // Resolve as module_function (e.g., fs.read_file -> fs_read_file)
                    let qualified_name = format!("{}_{}", enum_name, field);
                    let resolved_name = self.imports.get(&qualified_name)
                        .cloned()
                        .unwrap_or(qualified_name);

                    // Look up the function
                    let fn_value = self.module
                        .get_function(&resolved_name)
                        .ok_or_else(|| CodegenError::UndefinedFunction(
                            format!("'{}' not found in module '{}'", field, enum_name)
                        ))?;

                    // Get parameter types for proper coercion
                    let param_types = self.function_param_types.get(&resolved_name).cloned();

                    // Compile arguments with expected types
                    let mut compiled_args: Vec<BasicValueEnum> = Vec::new();
                    for (i, arg) in args.iter().enumerate() {
                        let expected_type = param_types.as_ref()
                            .and_then(|types| types.get(i))
                            .cloned();
                        compiled_args.push(self.compile_expr_with_type(arg, expected_type.as_ref())?);
                    }

                    let args_meta: Vec<_> = compiled_args.iter().map(|a| (*a).into()).collect();

                    let call_site = self.builder
                        .build_call(fn_value, &args_meta, "call")
                        .unwrap();

                    return match call_site.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(val) => Ok(val),
                        inkwell::values::ValueKind::Instruction(_) => {
                            Ok(self.context.i64_type().const_zero().into())
                        }
                    };
                }

                // Check for static method call on non-generic struct: Type.method()
                if self.struct_types.contains_key(enum_name) {
                    return self.compile_static_method_call(enum_name, field, args);
                }
            }
            // Generic enum or struct: Expr::StructInit used to carry name and generics
            if let Expr::StructInit { name: type_name, generics, fields, .. } = object.as_ref() {
                if fields.is_empty() && !generics.is_empty() {
                    // Check if it's a generic enum
                    if self.generic_enums.contains_key(type_name) {
                        let mono_name = self.ensure_monomorphized_enum(type_name, generics)?;
                        let enum_info = self.enum_types.get(&mono_name).cloned()
                            .ok_or_else(|| CodegenError::UndefinedType(format!("enum '{}' not found after monomorphization", mono_name)))?;
                        return self.compile_enum_variant_constructor(&enum_info, &mono_name, field, args);
                    }
                    // Check if it's a generic struct - this is a static method call
                    if self.generic_structs.contains_key(type_name) {
                        let mono_name = self.ensure_monomorphized_struct(type_name, generics)?;
                        // Ensure impl methods are also monomorphized
                        self.ensure_monomorphized_impl(type_name, generics)?;
                        return self.compile_static_method_call(&mono_name, field, args);
                    }
                }
            }
        }

        let name = match func {
            Expr::Ident(name, _) => name,
            _ => return Err(CodegenError::NotImplemented(
                "function call requires a simple function name. \
                 Expression calls like '(get_fn())(args)' are not yet supported".to_string()
            )),
        };

        // Handle intrinsic functions
        if name == "print" {
            return self.compile_print_call(args);
        }
        if name == "println" {
            return self.compile_println_call(args);
        }
        // print_int and println_int are deprecated - use string interpolation instead
        // e.g., println("${x}") instead of println_int(x)
        if name == "malloc" {
            return self.compile_malloc_call(args);
        }
        if name == "realloc" {
            return self.compile_realloc_call(args);
        }
        if name == "free" {
            return self.compile_free_call(args);
        }
        if name == "memcpy" {
            return self.compile_memcpy_call(args);
        }
        if name == "panic" {
            return self.compile_panic_call(args);
        }
        // Low-level memory access intrinsics
        if name == "ptr_write_i64" {
            return self.compile_ptr_write_i64(args);
        }
        if name == "ptr_read_i64" {
            return self.compile_ptr_read_i64(args);
        }
        // null<T>() - returns a null pointer of type *T
        if name == "null" {
            return self.compile_null_call(type_args);
        }
        // sizeof<T>() - returns the size of type T in bytes
        if name == "sizeof" {
            return self.compile_sizeof_call(type_args);
        }
        // ptr_null<T>() - returns a null pointer
        if name == "ptr_null" {
            return self.compile_ptr_null_call(type_args);
        }
        // ptr_is_null<T>(ptr) - check if pointer is null
        if name == "ptr_is_null" {
            return self.compile_ptr_is_null_call(args);
        }
        // ptr_write<T>(ptr, value) - write value through pointer
        if name == "ptr_write" {
            return self.compile_ptr_write_call(type_args, args);
        }
        // ptr_read<T>(ptr) - read value through pointer
        if name == "ptr_read" {
            return self.compile_ptr_read_call(type_args, args);
        }
        // ptr_add<T>(ptr, offset) - add offset to pointer
        if name == "ptr_add" {
            return self.compile_ptr_add_call(type_args, args);
        }

        // File I/O syscalls
        if name == "sys_open" {
            return self.compile_sys_open_call(args);
        }
        if name == "sys_close" {
            return self.compile_sys_close_call(args);
        }
        if name == "sys_read" {
            return self.compile_sys_read_call(args);
        }
        if name == "sys_write" {
            return self.compile_sys_write_call(args);
        }
        if name == "sys_lseek" {
            return self.compile_sys_lseek_call(args);
        }

        // Time syscalls
        if name == "sys_clock_gettime" {
            return self.compile_sys_clock_gettime_call(args);
        }
        if name == "sys_nanosleep" {
            return self.compile_sys_nanosleep_call(args);
        }

        // Process syscalls
        if name == "sys_getpid" {
            return self.compile_sys_getpid_call(args);
        }
        if name == "sys_getppid" {
            return self.compile_sys_getppid_call(args);
        }
        if name == "sys_exit" {
            return self.compile_sys_exit_call(args);
        }
        if name == "sys_getcwd" {
            return self.compile_sys_getcwd_call(args);
        }
        if name == "sys_chdir" {
            return self.compile_sys_chdir_call(args);
        }
        if name == "sys_getenv" {
            return self.compile_sys_getenv_call(args);
        }
        if name == "sys_setenv" {
            return self.compile_sys_setenv_call(args);
        }
        if name == "sys_fork" {
            return self.compile_sys_fork_call(args);
        }
        if name == "sys_execve" {
            return self.compile_sys_execve_call(args);
        }
        if name == "sys_waitpid" {
            return self.compile_sys_waitpid_call(args);
        }
        if name == "sys_kill" {
            return self.compile_sys_kill_call(args);
        }

        // Memory syscalls
        if name == "sys_mmap" {
            return self.compile_sys_mmap_call(args);
        }
        if name == "sys_munmap" {
            return self.compile_sys_munmap_call(args);
        }
        if name == "sys_mprotect" {
            return self.compile_sys_mprotect_call(args);
        }
        if name == "sys_madvise" {
            return self.compile_sys_madvise_call(args);
        }

        // Socket syscalls
        if name == "sys_socket" {
            return self.compile_sys_socket_call(args);
        }
        if name == "sys_bind" {
            return self.compile_sys_bind_call(args);
        }
        if name == "sys_listen" {
            return self.compile_sys_listen_call(args);
        }
        if name == "sys_accept" {
            return self.compile_sys_accept_call(args);
        }
        if name == "sys_connect" {
            return self.compile_sys_connect_call(args);
        }
        if name == "sys_send" {
            return self.compile_sys_send_call(args);
        }
        if name == "sys_recv" {
            return self.compile_sys_recv_call(args);
        }
        if name == "sys_sendto" {
            return self.compile_sys_sendto_call(args);
        }
        if name == "sys_recvfrom" {
            return self.compile_sys_recvfrom_call(args);
        }
        if name == "sys_setsockopt" {
            return self.compile_sys_setsockopt_call(args);
        }
        if name == "sys_getsockopt" {
            return self.compile_sys_getsockopt_call(args);
        }
        if name == "sys_shutdown" {
            return self.compile_sys_shutdown_call(args);
        }

        // Generate monomorphized function if type args are present
        let mono_name = if !type_args.is_empty() {
            // Explicit type arguments provided
            self.ensure_monomorphized_function(name, type_args)?
        } else if self.module.get_function(name).is_some() {
            // Non-generic function exists directly
            name.clone()
        } else if let Some(generic_func) = self.generic_functions.get(name).cloned() {
            // Generic function - infer type arguments from actual arguments
            let inferred_types = self.infer_function_type_args(&generic_func, args)?;
            self.ensure_monomorphized_function(name, &inferred_types)?
        } else {
            // Function not found
            return Err(CodegenError::UndefinedFunction(name.clone()));
        };

        let fn_value = self
            .module
            .get_function(&mono_name)
            .ok_or_else(|| CodegenError::UndefinedFunction(mono_name.clone()))?;

        // Save borrow state - temporary borrows in function arguments should be released after the call
        let borrows_before = self.borrowed_vars.clone();

        // Get parameter types for reference coercion
        let param_types = self.function_param_types.get(&mono_name).cloned();

        // Compile the arguments with type coercion for references
        let mut compiled_args: Vec<BasicValueEnum> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            // Get expected type for this argument to enable proper type coercion
            let expected_type = param_types.as_ref()
                .and_then(|types| types.get(i))
                .cloned();
            let arg_val = self.compile_expr_with_type(arg, expected_type.as_ref())?;

            // Check if we need to coerce value to reference
            let coerced_val = if let Some(ref params) = param_types {
                if i < params.len() {
                    match &params[i] {
                        Type::Ref(inner) | Type::RefMut(inner) => {
                            // Check if parameter expects a reference to a fixed-size array
                            // and arg_val is a slice struct (from &array coercion)
                            if let Type::Array(_, _) = inner.as_ref() {
                                // Parameter expects &T[N], but compile_ref produces a slice {ptr, len}
                                // Extract the pointer from the slice struct
                                if arg_val.is_struct_value() {
                                    let struct_val = arg_val.into_struct_value();
                                    // Extract field 0 (the pointer)
                                    let ptr = self.builder
                                        .build_extract_value(struct_val, 0, "array_ptr")
                                        .unwrap();
                                    ptr
                                } else {
                                    arg_val
                                }
                            } else {
                                // Parameter expects a reference but we have a value
                                // Check if arg_val is a struct value (not a pointer)
                                if arg_val.is_struct_value() {
                                    // Create temporary storage and pass pointer
                                    let temp = self.builder.build_alloca(arg_val.get_type(), "ref_temp").unwrap();
                                    self.builder.build_store(temp, arg_val).unwrap();
                                    temp.into()
                                } else {
                                    arg_val
                                }
                            }
                        }
                        _ => arg_val,
                    }
                } else {
                    arg_val
                }
            } else {
                arg_val
            };

            compiled_args.push(coerced_val);
        }

        // Track moves: if an argument is a struct variable passed by value, mark it as moved
        for arg in args {
            if let Expr::Ident(arg_name, _) = arg {
                if let Some(var_info) = self.variables.get(arg_name) {
                    // If the variable is a struct (not a reference), it's being moved
                    if var_info.struct_name.is_some() && !var_info.is_ref && !var_info.is_mut_ref {
                        self.moved_vars.insert(arg_name.clone());
                    }
                }
            }
        }

        let args_meta: Vec<_> = compiled_args.iter().map(|a| (*a).into()).collect();

        let call_site = self
            .builder
            .build_call(fn_value, &args_meta, "call")
            .unwrap();

        // Restore borrow state - borrows for function arguments end when function returns
        self.borrowed_vars = borrows_before;

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                // Void return - return a dummy value
                Ok(self.context.i64_type().const_zero().into())
            }
        }
    }

    /// Compile a static method call (Type.method(args))
    pub(crate) fn compile_static_method_call(
        &mut self,
        type_name: &str,
        method: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Look up the method in the type's method table
        let mangled_name = self.type_methods
            .get(type_name)
            .and_then(|methods| methods.get(method))
            .cloned()
            .ok_or_else(|| CodegenError::UndefinedFunction(
                format!("static method '{}' not found on type '{}'", method, type_name)
            ))?;

        // Get the function
        let fn_value = self.module
            .get_function(&mangled_name)
            .ok_or_else(|| CodegenError::UndefinedFunction(mangled_name.clone()))?;

        // Save borrow state
        let borrows_before = self.borrowed_vars.clone();

        // Get parameter types for reference coercion
        let param_types = self.function_param_types.get(&mangled_name).cloned();

        // Compile the arguments with type coercion for references
        let mut compiled_args: Vec<BasicValueEnum> = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            // Get expected type for this argument to enable proper type coercion
            let expected_type = param_types.as_ref()
                .and_then(|types| types.get(i))
                .cloned();
            let arg_val = self.compile_expr_with_type(arg, expected_type.as_ref())?;

            // Check if we need to coerce value to reference
            let coerced_val = if let Some(ref params) = param_types {
                if i < params.len() {
                    match &params[i] {
                        Type::Ref(inner) | Type::RefMut(inner) => {
                            // Check if parameter expects a reference to a fixed-size array
                            // and arg_val is a slice struct (from &array coercion)
                            if let Type::Array(_, _) = inner.as_ref() {
                                // Parameter expects &T[N], but compile_ref produces a slice {ptr, len}
                                // Extract the pointer from the slice struct
                                if arg_val.is_struct_value() {
                                    let struct_val = arg_val.into_struct_value();
                                    // Extract field 0 (the pointer)
                                    let ptr = self.builder
                                        .build_extract_value(struct_val, 0, "array_ptr")
                                        .unwrap();
                                    ptr
                                } else {
                                    arg_val
                                }
                            } else {
                                // Parameter expects a reference but we have a value
                                // Check if arg_val is a struct value (not a pointer)
                                if arg_val.is_struct_value() {
                                    // Create temporary storage and pass pointer
                                    let temp = self.builder.build_alloca(arg_val.get_type(), "ref_temp").unwrap();
                                    self.builder.build_store(temp, arg_val).unwrap();
                                    temp.into()
                                } else {
                                    arg_val
                                }
                            }
                        }
                        _ => arg_val,
                    }
                } else {
                    arg_val
                }
            } else {
                arg_val
            };

            compiled_args.push(coerced_val);
        }

        let args_meta: Vec<_> = compiled_args.iter().map(|a| (*a).into()).collect();

        let call_site = self.builder
            .build_call(fn_value, &args_meta, "static_method_call")
            .unwrap();

        // Restore borrow state
        self.borrowed_vars = borrows_before;

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.i64_type().const_zero().into())
            }
        }
    }

    /// Compile a method call (receiver.method(args))
    pub(crate) fn compile_method_call(
        &mut self,
        receiver: &Expr,
        method: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Check for module-qualified function call: fs.exists() where fs is a module alias
        if let Expr::Ident(name, _) = receiver {
            if self.module_aliases.contains_key(name) {
                // This is a module-qualified function call, not a method call
                // Resolve as module_function (e.g., fs.exists -> fs_exists)
                let qualified_name = format!("{}_{}", name, method);
                let resolved_name = self.imports.get(&qualified_name)
                    .cloned()
                    .unwrap_or(qualified_name.clone());

                // Look up the function
                let fn_value = self.module
                    .get_function(&resolved_name)
                    .ok_or_else(|| CodegenError::UndefinedFunction(
                        format!("'{}' not found in module '{}' (looking for '{}')", method, name, resolved_name)
                    ))?;

                // Get parameter types for proper coercion
                let param_types = self.function_param_types.get(&resolved_name).cloned();

                // Compile arguments with expected types
                let mut compiled_args: Vec<BasicValueEnum> = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let expected_type = param_types.as_ref()
                        .and_then(|types| types.get(i))
                        .cloned();
                    compiled_args.push(self.compile_expr_with_type(arg, expected_type.as_ref())?);
                }

                let args_meta: Vec<_> = compiled_args.iter().map(|a| (*a).into()).collect();

                let call_site = self.builder
                    .build_call(fn_value, &args_meta, "call")
                    .unwrap();

                return match call_site.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(val) => Ok(val),
                    inkwell::values::ValueKind::Instruction(_) => {
                        Ok(self.context.i64_type().const_zero().into())
                    }
                };
            }
        }

        // Check for module-qualified static method call: fs.File.create() where fs is a module alias
        // Pattern: receiver = Field { object: Ident(module), field: TypeName }, method = "create"
        if let Expr::Field { object, field: type_name, .. } = receiver {
            if let Expr::Ident(module_name, _) = object.as_ref() {
                if self.module_aliases.contains_key(module_name) {
                    // Resolve the type name: fs.File -> File
                    let qualified_type = format!("{}_{}", module_name, type_name);
                    let resolved_type = self.imports.get(&qualified_type)
                        .cloned()
                        .unwrap_or(type_name.clone());

                    // This is a static method call on a type from the module
                    return self.compile_static_method_call(&resolved_type, method, args);
                }
            }
        }

        // Get the type name of the receiver
        let type_name = if let Expr::Ident(name, _) = receiver {
            // Get variable info to find its type
            let var_info = self.variables.get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;

            // Determine the type name from variable info
            if let Some(ref sn) = var_info.struct_name {
                sn.clone()
            } else if let Some(ref sn) = var_info.ref_struct_name {
                sn.clone()
            } else {
                return Err(CodegenError::NotImplemented(
                    format!("method call on variable '{}' without struct type", name)
                ));
            }
        } else if let Expr::Field { object, field, .. } = receiver {
            // Handle field access like result.data.push(...)
            // Get the type of the field
            self.get_field_type_name(object, field)?
        } else {
            return Err(CodegenError::NotImplemented(
                "method call receiver must be a simple variable (e.g., 'x.method()'). \
                 Chained calls like 'a.b().c()' require intermediate variables".to_string()
            ));
        };

        // If this is a monomorphized generic type (e.g., Vec_String), ensure impl methods are generated
        // Try to parse the type name to see if it's a monomorphized generic type
        if !self.type_methods.contains_key(&type_name) {
            if let Some((base_name, type_args)) = self.parse_mangled_name(&type_name) {
                // Check if there's a generic impl for the base type
                if self.generic_impls.contains_key(&base_name) {
                    self.ensure_monomorphized_impl(&base_name, &type_args)?;
                }
            }
        }

        // Look up the method in the type's method table
        let mangled_name = self.type_methods
            .get(&type_name)
            .and_then(|methods| methods.get(method))
            .cloned()
            .ok_or_else(|| CodegenError::UndefinedFunction(
                format!("method '{}' not found on type '{}'", method, type_name)
            ))?;

        // Get the function
        let fn_value = self.module
            .get_function(&mangled_name)
            .ok_or_else(|| CodegenError::UndefinedFunction(mangled_name.clone()))?;

        // Save borrow state
        let borrows_before = self.borrowed_vars.clone();

        // Compile receiver as first argument (pass as pointer/reference)
        let receiver_val = if let Expr::Ident(name, _) = receiver {
            let var_info = self.variables.get(name).unwrap();
            if var_info.is_ref || var_info.is_mut_ref {
                // Receiver is already a reference - load the pointer value
                self.builder.build_load(self.context.ptr_type(inkwell::AddressSpace::default()), var_info.ptr, "self_ptr").unwrap()
            } else {
                // Return the pointer to the struct directly
                var_info.ptr.into()
            }
        } else if let Expr::Field { object, field, .. } = receiver {
            // For field access receivers like result.data.push(...),
            // we need to get a pointer to the field
            // Note: compile_field_ptr checks for mutable access, but for method calls
            // on fields we allow read-only access to call methods like push
            // (the method itself will have ~self or &self and handle mutability)
            self.compile_field_ptr_for_method(object, field)?.into()
        } else {
            self.compile_expr(receiver)?
        };

        // Compile the rest of the arguments with type coercion for references
        let mut compiled_args: Vec<BasicValueEnum> = vec![receiver_val];

        // Get parameter types for this method (index 0 is self, so skip it)
        let param_types = self.function_param_types.get(&mangled_name).cloned();

        for (i, arg) in args.iter().enumerate() {
            // Get expected type for this argument (param_types index is i+1 because index 0 is self)
            let expected_type = param_types.as_ref()
                .and_then(|types| types.get(i + 1))
                .cloned();
            let arg_val = self.compile_expr_with_type(arg, expected_type.as_ref())?;

            // Check if we need to coerce value to reference
            // param_types index is i+1 because index 0 is self
            let coerced_val = if let Some(ref params) = param_types {
                if i + 1 < params.len() {
                    match &params[i + 1] {
                        Type::Ref(_) | Type::RefMut(_) => {
                            // Parameter expects a reference but we have a value
                            // Check if arg_val is a struct value (not a pointer)
                            if arg_val.is_struct_value() {
                                // Create temporary storage and pass pointer
                                let temp = self.builder.build_alloca(arg_val.get_type(), "ref_temp").unwrap();
                                self.builder.build_store(temp, arg_val).unwrap();
                                temp.into()
                            } else {
                                arg_val
                            }
                        }
                        _ => arg_val,
                    }
                } else {
                    arg_val
                }
            } else {
                arg_val
            };

            compiled_args.push(coerced_val);
        }

        let args_meta: Vec<_> = compiled_args.iter().map(|a| (*a).into()).collect();

        let call_site = self.builder
            .build_call(fn_value, &args_meta, "method_call")
            .unwrap();

        // Restore borrow state
        self.borrowed_vars = borrows_before;

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.i64_type().const_zero().into())
            }
        }
    }

    /// Compile a field access and return a pointer to the field (for method calls)
    /// Unlike compile_field_ptr, this doesn't check mutability - the method signature handles that
    fn compile_field_ptr_for_method(&mut self, object: &Expr, field: &str) -> Result<inkwell::values::PointerValue<'ctx>, CodegenError> {
        if let Expr::Ident(name, _) = object {
            let var_info = self.variables.get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            let struct_name = var_info.struct_name.as_ref()
                .or(var_info.ref_struct_name.as_ref())
                .ok_or_else(|| CodegenError::NotImplemented(
                    format!("field access on '{}' which is not a struct", name)
                ))?;

            let struct_info = self.struct_types.get(struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                .clone();

            let field_idx = *struct_info.field_indices.get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            // Get pointer to the struct
            let struct_ptr = if var_info.is_ref || var_info.is_mut_ref {
                self.builder.build_load(
                    self.context.ptr_type(inkwell::AddressSpace::default()),
                    var_info.ptr,
                    "deref"
                ).unwrap().into_pointer_value()
            } else {
                var_info.ptr
            };

            // GEP to get pointer to the field
            let field_ptr = self.builder.build_struct_gep(
                struct_info.llvm_type,
                struct_ptr,
                field_idx,
                &format!("{}.{}_ptr", name, field)
            ).unwrap();

            Ok(field_ptr)
        } else if let Expr::Field { object: inner_obj, field: inner_field, .. } = object {
            // Nested field access: a.b.c - get pointer to inner field, then access outer field
            let inner_ptr = self.compile_field_ptr_for_method(inner_obj, inner_field)?;
            let inner_type_name = self.get_field_type_name(inner_obj, inner_field)?;

            let struct_info = self.struct_types.get(&inner_type_name)
                .ok_or_else(|| CodegenError::UndefinedType(inner_type_name.clone()))?
                .clone();

            let field_idx = *struct_info.field_indices.get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            let field_ptr = self.builder.build_struct_gep(
                struct_info.llvm_type,
                inner_ptr,
                field_idx,
                &format!("nested.{}_ptr", field)
            ).unwrap();

            Ok(field_ptr)
        } else {
            Err(CodegenError::NotImplemented(
                "complex field access for method calls not yet supported".to_string()
            ))
        }
    }

    /// Get the type name of a field for method lookup
    fn get_field_type_name(&self, object: &Expr, field: &str) -> Result<String, CodegenError> {
        // Get the struct type name of the object
        let struct_name = if let Expr::Ident(name, _) = object {
            let var_info = self.variables.get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
            var_info.struct_name.clone()
                .or_else(|| var_info.ref_struct_name.clone())
                .ok_or_else(|| CodegenError::NotImplemented(
                    format!("field access on '{}' which is not a struct", name)
                ))?
        } else if let Expr::Field { object: inner_obj, field: inner_field, .. } = object {
            // Nested field: a.b.c - get type of a.b, then look up c in that
            let inner_type = self.get_field_type_name(inner_obj, inner_field)?;
            inner_type
        } else {
            return Err(CodegenError::NotImplemented(
                "complex field access for method calls not yet supported".to_string()
            ));
        };

        // Look up the field type in the struct info
        let struct_info = self.struct_types.get(&struct_name)
            .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?;

        // Get the field's AST type to find its struct name
        let field_idx = *struct_info.field_indices.get(field)
            .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

        let field_ast_type = struct_info.ast_field_types.get(field_idx as usize)
            .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

        // Get struct name from the field's AST type
        self.get_struct_name_for_type(field_ast_type)
            .ok_or_else(|| CodegenError::NotImplemented(
                format!("field '{}' is not a struct type", field)
            ))
    }
}
