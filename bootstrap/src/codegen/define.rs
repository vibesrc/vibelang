//! Struct, enum, and function definition and compilation

use super::{Codegen, CodegenError, VarInfo, StructTypeInfo, EnumTypeInfo};
use crate::ast::*;
use inkwell::values::FunctionValue;
use inkwell::types::BasicType;
use std::collections::HashMap;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn define_struct(&mut self, s: &Struct) -> Result<(), CodegenError> {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();
        let mut field_names = Vec::new();
        let mut ast_field_types = Vec::new();

        for (i, field) in s.fields.iter().enumerate() {
            let field_type = self.llvm_type(&field.ty)?;
            field_types.push(field_type);
            field_indices.insert(field.name.clone(), i as u32);
            field_names.push(field.name.clone());
            ast_field_types.push(field.ty.clone());
        }

        let llvm_field_types: Vec<_> = field_types.iter().map(|t| *t).collect();
        let struct_type = self.context.struct_type(&llvm_field_types, false);

        self.struct_types.insert(
            s.name.clone(),
            StructTypeInfo {
                llvm_type: struct_type,
                field_indices,
                field_types,
                field_names,
                ast_field_types,
                name: s.name.clone(),
            },
        );

        Ok(())
    }

    pub(crate) fn define_enum(&mut self, e: &Enum) -> Result<(), CodegenError> {
        let mut variant_tags = HashMap::new();
        let mut variant_payloads: HashMap<String, Vec<inkwell::types::BasicTypeEnum<'ctx>>> = HashMap::new();
        let mut max_payload_fields: Vec<inkwell::types::BasicTypeEnum<'ctx>> = Vec::new();
        let mut variant_names = Vec::new();

        for (i, variant) in e.variants.iter().enumerate() {
            variant_tags.insert(variant.name.clone(), i as u32);
            variant_names.push(variant.name.clone());

            // Get payload types for this variant
            let payload_types: Vec<inkwell::types::BasicTypeEnum<'ctx>> = match &variant.fields {
                VariantFields::Unit => Vec::new(),
                VariantFields::Tuple(types) => {
                    types.iter()
                        .map(|t| self.llvm_type(t))
                        .collect::<Result<_, _>>()?
                }
                VariantFields::Struct(fields) => {
                    fields.iter()
                        .map(|f| self.llvm_type(&f.ty))
                        .collect::<Result<_, _>>()?
                }
            };

            // Track the variant with most payload fields (simplified union approximation)
            if payload_types.len() > max_payload_fields.len() {
                max_payload_fields = payload_types.clone();
            }

            variant_payloads.insert(variant.name.clone(), payload_types);
        }

        // Create LLVM type: { i32 tag, payload... }
        let i32_type = self.context.i32_type();
        let mut struct_fields: Vec<inkwell::types::BasicTypeEnum<'ctx>> = vec![i32_type.into()]; // tag

        // Add payload fields from the largest variant
        for ty in &max_payload_fields {
            struct_fields.push(*ty);
        }

        let llvm_type = self.context.struct_type(&struct_fields, false);

        self.enum_types.insert(
            e.name.clone(),
            EnumTypeInfo {
                llvm_type,
                variant_tags,
                variant_payloads,
                payload_size: max_payload_fields.len() as u32,
                variant_names,
                name: e.name.clone(),
            },
        );

        Ok(())
    }

    pub(crate) fn declare_function(&mut self, func: &Function) -> Result<FunctionValue<'ctx>, CodegenError> {
        let param_types: Vec<inkwell::types::BasicTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.ty))
            .collect::<Result<_, _>>()?;

        let param_types_ref: Vec<_> = param_types.iter().map(|t| (*t).into()).collect();

        // Main function must return i32 for C runtime compatibility
        let fn_type = if func.name == "main" {
            self.context.i32_type().fn_type(&param_types_ref, false)
        } else {
            match &func.return_type {
                Some(ty) => {
                    let ret_ty = self.llvm_type(ty)?;
                    ret_ty.fn_type(&param_types_ref, false)
                }
                None => self.context.void_type().fn_type(&param_types_ref, false),
            }
        };

        let fn_value = self.module.add_function(&func.name, fn_type, None);

        // Store return type for later inference
        self.function_return_types.insert(func.name.clone(), func.return_type.clone());

        Ok(fn_value)
    }

    pub(crate) fn compile_function(&mut self, func: &Function) -> Result<(), CodegenError> {
        let fn_value = self
            .module
            .get_function(&func.name)
            .ok_or_else(|| CodegenError::UndefinedFunction(func.name.clone()))?;

        self.current_function = Some(fn_value);

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        // Clear variables and ownership state for new function scope
        self.variables.clear();
        self.moved_vars.clear();
        self.borrowed_vars.clear();

        // Allocate parameters
        for (i, param) in func.params.iter().enumerate() {
            let param_value = fn_value.get_nth_param(i as u32).unwrap();
            let param_type = param_value.get_type();
            let alloca = self.create_entry_block_alloca(&param.name, param_type);
            self.builder.build_store(alloca, param_value).unwrap();

            // Determine if this is a reference type and track appropriately
            let (is_ref, is_mut_ref, ref_struct_name) = self.get_ref_info(&param.ty);
            let struct_name = if is_ref { None } else { self.get_struct_name_for_type(&param.ty) };

            self.variables.insert(param.name.clone(), VarInfo {
                ptr: alloca,
                ty: param_type,
                struct_name,
                ast_type: Some(param.ty.clone()),
                is_ref,
                is_mut_ref,
                ref_struct_name,
                slice_elem_type: None,
            });
        }

        // Compile body
        let last_value = self.compile_block(&func.body)?;

        // Add implicit return if needed
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            if func.name == "main" {
                // Main returns 0 by default
                let zero = self.context.i32_type().const_zero();
                self.builder.build_return(Some(&zero)).unwrap();
            } else if func.return_type.is_some() {
                // Return the last expression value if function has return type
                if let Some(val) = last_value {
                    self.builder.build_return(Some(&val)).unwrap();
                }
            } else {
                self.builder.build_return(None).unwrap();
            }
        }

        Ok(())
    }

    /// Declare methods from an impl block
    pub(crate) fn declare_impl_methods(&mut self, impl_block: &Impl) -> Result<(), CodegenError> {
        let type_name = self.get_type_name(&impl_block.target)
            .ok_or_else(|| CodegenError::UndefinedType("impl target must be a named type".to_string()))?;

        for method in &impl_block.methods {
            // Create mangled function name: TypeName_methodName
            let mangled_name = format!("{}_{}", type_name, method.name);

            // Substitute Self type with the actual type in params and return type
            let resolved_params: Vec<Param> = method.params.iter().map(|p| {
                Param {
                    name: p.name.clone(),
                    ty: self.substitute_self_type(&p.ty, &type_name),
                    span: p.span,
                }
            }).collect();

            let resolved_return_type = method.return_type.as_ref()
                .map(|rt| self.substitute_self_type(rt, &type_name));

            // Create a modified function with the mangled name
            let mangled_func = Function {
                name: mangled_name.clone(),
                generics: method.generics.clone(),
                params: resolved_params,
                return_type: resolved_return_type,
                body: method.body.clone(),
                is_pub: method.is_pub,
                span: method.span,
            };

            if method.generics.is_empty() {
                self.declare_function(&mangled_func)?;
            } else {
                // Store generic method for later monomorphization
                self.generic_functions.insert(mangled_name.clone(), mangled_func);
            }

            // Register the method for this type
            self.type_methods
                .entry(type_name.clone())
                .or_insert_with(HashMap::new)
                .insert(method.name.clone(), mangled_name);
        }

        Ok(())
    }

    /// Compile methods from an impl block
    pub(crate) fn compile_impl_methods(&mut self, impl_block: &Impl) -> Result<(), CodegenError> {
        let type_name = self.get_type_name(&impl_block.target)
            .ok_or_else(|| CodegenError::UndefinedType("impl target must be a named type".to_string()))?;

        for method in &impl_block.methods {
            if method.generics.is_empty() {
                let mangled_name = format!("{}_{}", type_name, method.name);

                // Substitute Self type with the actual type in params and return type
                let resolved_params: Vec<Param> = method.params.iter().map(|p| {
                    Param {
                        name: p.name.clone(),
                        ty: self.substitute_self_type(&p.ty, &type_name),
                        span: p.span,
                    }
                }).collect();

                let resolved_return_type = method.return_type.as_ref()
                    .map(|rt| self.substitute_self_type(rt, &type_name));

                // Create a modified function with the mangled name
                let mangled_func = Function {
                    name: mangled_name,
                    generics: method.generics.clone(),
                    params: resolved_params,
                    return_type: resolved_return_type,
                    body: method.body.clone(),
                    is_pub: method.is_pub,
                    span: method.span,
                };

                self.compile_function(&mangled_func)?;
            }
        }

        Ok(())
    }

    /// Substitute Type::SelfType with the actual named type
    fn substitute_self_type(&self, ty: &Type, type_name: &str) -> Type {
        match ty {
            Type::SelfType => Type::Named {
                name: type_name.to_string(),
                generics: vec![],
            },
            Type::Ref(inner) => Type::Ref(Box::new(self.substitute_self_type(inner, type_name))),
            Type::RefMut(inner) => Type::RefMut(Box::new(self.substitute_self_type(inner, type_name))),
            Type::Pointer(inner) => Type::Pointer(Box::new(self.substitute_self_type(inner, type_name))),
            Type::Array(inner, size) => Type::Array(Box::new(self.substitute_self_type(inner, type_name)), *size),
            Type::Slice(inner) => Type::Slice(Box::new(self.substitute_self_type(inner, type_name))),
            Type::Named { name, generics } => Type::Named {
                name: name.clone(),
                generics: generics.iter().map(|g| self.substitute_self_type(g, type_name)).collect(),
            },
            // Primitives and other types don't need substitution
            _ => ty.clone(),
        }
    }
}
