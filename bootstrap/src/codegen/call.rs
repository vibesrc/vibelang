//! Function and method call compilation

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn compile_call(&mut self, func: &Expr, type_args: &[Type], args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Check for enum variant constructor: EnumName.Variant(args)
        if let Expr::Field { object, field, .. } = func {
            // Non-generic enum: Expr::Ident
            if let Expr::Ident(enum_name, _) = object.as_ref() {
                if let Some(enum_info) = self.enum_types.get(enum_name).cloned() {
                    return self.compile_enum_variant_constructor(&enum_info, enum_name, field, args);
                }
            }
            // Generic enum: Expr::StructInit used to carry name and generics
            if let Expr::StructInit { name: enum_name, generics, fields, .. } = object.as_ref() {
                if fields.is_empty() && !generics.is_empty() {
                    // This is a generic enum variant constructor: EnumName<Type>.Variant(args)
                    let mono_name = self.ensure_monomorphized_enum(enum_name, generics)?;
                    let enum_info = self.enum_types.get(&mono_name).cloned()
                        .ok_or_else(|| CodegenError::UndefinedType(format!("enum '{}' not found after monomorphization", mono_name)))?;
                    return self.compile_enum_variant_constructor(&enum_info, &mono_name, field, args);
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

        let compiled_args: Vec<BasicValueEnum> = args
            .iter()
            .map(|a| self.compile_expr(a))
            .collect::<Result<_, _>>()?;

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

        // Compile the arguments (no receiver for static methods)
        let compiled_args: Vec<BasicValueEnum> = args
            .iter()
            .map(|a| self.compile_expr(a))
            .collect::<Result<_, _>>()?;

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
        } else {
            return Err(CodegenError::NotImplemented(
                "method call receiver must be a simple variable (e.g., 'x.method()'). \
                 Chained calls like 'a.b().c()' require intermediate variables".to_string()
            ));
        };

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
            // Return the pointer to the struct directly
            var_info.ptr.into()
        } else {
            self.compile_expr(receiver)?
        };

        // Compile the rest of the arguments
        let mut compiled_args: Vec<BasicValueEnum> = vec![receiver_val];
        for arg in args {
            compiled_args.push(self.compile_expr(arg)?);
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
}
