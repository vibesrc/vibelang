//! Type-related code generation utilities

use super::{Codegen, CodegenError};
use crate::ast::*;
use inkwell::types::{BasicTypeEnum, BasicType};
use inkwell::AddressSpace;

impl<'ctx> Codegen<'ctx> {
    pub(crate) fn llvm_type(&mut self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
        match ty {
            Type::I8 => Ok(self.context.i8_type().into()),
            Type::I16 => Ok(self.context.i16_type().into()),
            Type::I32 => Ok(self.context.i32_type().into()),
            Type::I64 => Ok(self.context.i64_type().into()),
            Type::U8 => Ok(self.context.i8_type().into()),
            Type::U16 => Ok(self.context.i16_type().into()),
            Type::U32 => Ok(self.context.i32_type().into()),
            Type::U64 => Ok(self.context.i64_type().into()),
            Type::F32 => Ok(self.context.f32_type().into()),
            Type::F64 => Ok(self.context.f64_type().into()),
            Type::Bool => Ok(self.context.bool_type().into()),
            Type::Pointer(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Ref(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::RefMut(_) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Array(inner, size) => {
                let inner_ty = self.llvm_type(inner)?;
                Ok(inner_ty.array_type(*size as u32).into())
            }
            Type::Named { name, generics } => {
                // Handle Slice<T> as a built-in slice type
                if name == "Slice" && generics.len() == 1 {
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let len_type = self.context.i64_type();
                    return Ok(self
                        .context
                        .struct_type(&[ptr_type.into(), len_type.into()], false)
                        .into());
                }

                // Generate mangled name if generics present
                let lookup_name = if generics.is_empty() {
                    name.clone()
                } else {
                    self.mangle_name(name, generics)
                };

                // Check if it's an enum type (already defined)
                if let Some(enum_info) = self.enum_types.get(&lookup_name) {
                    return Ok(enum_info.llvm_type.as_basic_type_enum());
                }

                // Check if it's a struct type (already defined)
                if let Some(struct_info) = self.struct_types.get(&lookup_name) {
                    return Ok(struct_info.llvm_type.as_basic_type_enum());
                }

                // Not found - try to monomorphize if generics present
                if !generics.is_empty() {
                    // Try monomorphizing as enum first
                    if self.generic_enums.contains_key(name) {
                        self.ensure_monomorphized_enum(name, generics)?;
                        let enum_info = self.enum_types.get(&lookup_name)
                            .ok_or_else(|| CodegenError::UndefinedType(lookup_name.clone()))?;
                        return Ok(enum_info.llvm_type.as_basic_type_enum());
                    }
                    // Try monomorphizing as struct
                    if self.generic_structs.contains_key(name) {
                        self.ensure_monomorphized_struct(name, generics)?;
                        let struct_info = self.struct_types.get(&lookup_name)
                            .ok_or_else(|| CodegenError::UndefinedType(lookup_name.clone()))?;
                        return Ok(struct_info.llvm_type.as_basic_type_enum());
                    }
                }

                Err(CodegenError::UndefinedType(lookup_name))
            }
            Type::Slice(_inner) => {
                // Slice is a struct: { ptr: *T, len: i64 }
                let ptr_type = self.context.ptr_type(AddressSpace::default());
                let len_type = self.context.i64_type();
                Ok(self
                    .context
                    .struct_type(&[ptr_type.into(), len_type.into()], false)
                    .into())
            }
            _ => Err(CodegenError::NotImplemented(format!(
                "unsupported type '{:?}'. Supported types: i8, i16, i32, i64, u8, u16, u32, u64, \
                 f32, f64, bool, structs, enums, arrays, slices, and references", ty
            ))),
        }
    }

    /// Generate a mangled name for monomorphized types/functions
    /// e.g., "Pair" + [i32] => "Pair_i32"
    pub(crate) fn mangle_name(&self, name: &str, type_args: &[Type]) -> String {
        if type_args.is_empty() {
            return name.to_string();
        }

        let type_suffixes: Vec<String> = type_args
            .iter()
            .map(|ty| self.type_name(ty))
            .collect();

        format!("{}_{}", name, type_suffixes.join("_"))
    }

    /// Get a string representation of a type for mangling
    pub(crate) fn type_name(&self, ty: &Type) -> String {
        match ty {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Pointer(inner) => format!("ptr_{}", self.type_name(inner)),
            Type::Ref(inner) => format!("ref_{}", self.type_name(inner)),
            Type::RefMut(inner) => format!("mut_{}", self.type_name(inner)),
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    name.clone()
                } else {
                    self.mangle_name(name, generics)
                }
            }
            Type::Slice(inner) => format!("slice_{}", self.type_name(inner)),
            _ => "unknown".to_string(),
        }
    }

    /// Convert an LLVM type back to an AST type (for primitive types only)
    pub(crate) fn llvm_type_to_ast_type(&self, ty: BasicTypeEnum<'ctx>) -> Option<Type> {
        if ty.is_int_type() {
            let int_ty = ty.into_int_type();
            let bit_width = int_ty.get_bit_width();
            match bit_width {
                1 => Some(Type::Bool),
                8 => Some(Type::I8),   // Could be U8, but we default to signed
                16 => Some(Type::I16),
                32 => Some(Type::I32),
                64 => Some(Type::I64),
                _ => None,
            }
        } else if ty.is_float_type() {
            let float_ty = ty.into_float_type();
            // Check if it's f32 or f64 based on type
            if float_ty == self.context.f32_type() {
                Some(Type::F32)
            } else if float_ty == self.context.f64_type() {
                Some(Type::F64)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn get_struct_name_for_type(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Named { name, generics } => {
                // Check for monomorphized name first (e.g., Pair_i32)
                if !generics.is_empty() {
                    let mangled = self.mangle_name(name, generics);
                    if self.struct_types.contains_key(&mangled) {
                        return Some(mangled);
                    }
                }
                // Fall back to base name
                if self.struct_types.contains_key(name) {
                    Some(name.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(crate) fn get_ref_info(&self, ty: &Type) -> (bool, bool, Option<String>) {
        // Returns (is_ref, is_mut_ref, ref_struct_name)
        match ty {
            Type::Ref(inner) => {
                let struct_name = self.get_struct_name_for_type(inner);
                (true, false, struct_name)
            }
            Type::RefMut(inner) => {
                let struct_name = self.get_struct_name_for_type(inner);
                (true, true, struct_name)
            }
            _ => (false, false, None),
        }
    }

    /// Get the type name from a Type for method lookup
    pub(crate) fn get_type_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    Some(name.clone())
                } else {
                    Some(self.mangle_name(name, generics))
                }
            }
            _ => None,
        }
    }

    /// Infer type arguments for a generic enum variant constructor from the call arguments
    /// Returns None if this is a unit variant (no inference needed), or the inferred types
    pub(crate) fn infer_enum_type_args(
        &mut self,
        generic_enum: &crate::ast::Enum,
        variant: &crate::ast::Variant,
        args: &[crate::ast::Expr],
    ) -> Result<Option<Vec<Type>>, CodegenError> {
        use crate::ast::VariantFields;

        // Get the expected types from the variant
        let expected_types = match &variant.fields {
            VariantFields::Unit => {
                // Unit variant with no generic params used - default to i32 if enum has generics
                if generic_enum.generics.is_empty() {
                    return Ok(None);
                }
                // For unit variants like Option.None, we can't infer - need explicit type
                return Err(CodegenError::NotImplemented(
                    format!("cannot infer type for unit variant '{}' - use explicit type like {}<i32>.{}",
                        variant.name, generic_enum.name, variant.name)
                ));
            }
            VariantFields::Tuple(types) => types.clone(),
            VariantFields::Struct(fields) => fields.iter().map(|f| f.ty.clone()).collect(),
        };

        if args.len() != expected_types.len() {
            return Err(CodegenError::NotImplemented(
                format!("wrong number of arguments for variant {}: expected {}, got {}",
                    variant.name, expected_types.len(), args.len())
            ));
        }

        // Build type parameter mapping by inferring from arguments
        let mut type_map: std::collections::HashMap<String, Type> = std::collections::HashMap::new();

        for (expected_ty, arg) in expected_types.iter().zip(args.iter()) {
            self.infer_type_param(expected_ty, arg, &generic_enum.generics, &mut type_map)?;
        }

        // Build the final type args in order
        let mut inferred_types = Vec::new();
        for param in &generic_enum.generics {
            if let Some(ty) = type_map.get(param) {
                inferred_types.push(ty.clone());
            } else {
                return Err(CodegenError::NotImplemented(
                    format!("could not infer type for generic parameter '{}'", param)
                ));
            }
        }

        Ok(Some(inferred_types))
    }

    /// Infer a type parameter from an expected type and an actual argument
    fn infer_type_param(
        &mut self,
        expected: &Type,
        arg: &crate::ast::Expr,
        generics: &[String],
        type_map: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), CodegenError> {
        // Check if expected type is a generic parameter
        if let Type::Named { name, generics: type_generics } = expected {
            if type_generics.is_empty() && generics.contains(name) {
                // This is a type parameter - infer from argument
                let inferred = self.infer_type_from_expr(arg)?;
                type_map.insert(name.clone(), inferred);
                return Ok(());
            }
        }

        // For non-generic types, no inference needed
        Ok(())
    }

    /// Infer the type of an expression
    fn infer_type_from_expr(&mut self, expr: &crate::ast::Expr) -> Result<Type, CodegenError> {
        use crate::ast::{Expr, Literal};

        match expr {
            Expr::Literal(lit, _) => match lit {
                Literal::Int(_) => Ok(Type::I32),
                Literal::Float(_) => Ok(Type::F64),
                Literal::Bool(_) => Ok(Type::Bool),
                Literal::String(_) => Ok(Type::Named {
                    name: "Slice".to_string(),
                    generics: vec![Type::U8]
                }),
            },
            Expr::Ident(name, _) => {
                // Look up variable type
                if let Some(var_info) = self.variables.get(name) {
                    if let Some(ref struct_name) = var_info.struct_name {
                        return Ok(Type::Named { name: struct_name.clone(), generics: vec![] });
                    }
                    // Try to infer from LLVM type
                    if let Some(ast_ty) = self.llvm_type_to_ast_type(var_info.ty) {
                        return Ok(ast_ty);
                    }
                }
                Err(CodegenError::UndefinedVariable(name.clone()))
            }
            Expr::Binary { .. } => {
                // For simplicity, assume binary ops return i32 (could be improved)
                Ok(Type::I32)
            }
            Expr::Call { func, .. } => {
                // Try to get return type from function
                if let Expr::Ident(name, _) = func.as_ref() {
                    if let Some(fn_val) = self.module.get_function(name) {
                        if let Some(ret_ty) = fn_val.get_type().get_return_type() {
                            if let Some(ast_ty) = self.llvm_type_to_ast_type(ret_ty) {
                                return Ok(ast_ty);
                            }
                        }
                    }
                }
                Ok(Type::I32) // fallback
            }
            _ => Ok(Type::I32), // fallback for other expressions
        }
    }

    /// Infer type arguments for a generic function call from actual arguments
    pub(crate) fn infer_function_type_args(
        &mut self,
        generic_func: &crate::ast::Function,
        args: &[crate::ast::Expr],
    ) -> Result<Vec<Type>, CodegenError> {
        if generic_func.generics.is_empty() {
            return Ok(vec![]);
        }

        if args.len() != generic_func.params.len() {
            return Err(CodegenError::NotImplemented(
                format!("wrong number of arguments for function '{}': expected {}, got {}",
                    generic_func.name, generic_func.params.len(), args.len())
            ));
        }

        // Build type parameter mapping by inferring from arguments
        let mut type_map: std::collections::HashMap<String, Type> = std::collections::HashMap::new();

        for (param, arg) in generic_func.params.iter().zip(args.iter()) {
            self.infer_function_type_param(&param.ty, arg, &generic_func.generics, &mut type_map)?;
        }

        // Build the final type args in order
        let mut inferred_types = Vec::new();
        for param in &generic_func.generics {
            if let Some(ty) = type_map.get(param) {
                inferred_types.push(ty.clone());
            } else {
                return Err(CodegenError::NotImplemented(
                    format!("could not infer type for generic parameter '{}' in function '{}'",
                        param, generic_func.name)
                ));
            }
        }

        Ok(inferred_types)
    }

    /// Infer a type parameter from an expected parameter type and an actual argument
    fn infer_function_type_param(
        &mut self,
        expected: &Type,
        arg: &crate::ast::Expr,
        generics: &[String],
        type_map: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), CodegenError> {
        // Get the actual type of the argument
        let actual_type = self.get_expr_type(arg)?;

        // Try to unify expected with actual to extract type parameter bindings
        self.unify_types(expected, &actual_type, generics, type_map)
    }

    /// Get the type of an expression for type inference
    fn get_expr_type(&self, expr: &crate::ast::Expr) -> Result<Type, CodegenError> {
        use crate::ast::{Expr, Literal};

        match expr {
            Expr::Literal(lit, _) => match lit {
                Literal::Int(_) => Ok(Type::I32),
                Literal::Float(_) => Ok(Type::F64),
                Literal::Bool(_) => Ok(Type::Bool),
                Literal::String(_) => Ok(Type::Named {
                    name: "Slice".to_string(),
                    generics: vec![Type::U8]
                }),
            },
            Expr::Ident(name, _) => {
                if let Some(var_info) = self.variables.get(name) {
                    // First try the explicit AST type if available
                    if let Some(ref ast_ty) = var_info.ast_type {
                        return Ok(ast_ty.clone());
                    }
                    // Try to unpack from struct_name (mangled name like "Option_Point")
                    if let Some(ref struct_name) = var_info.struct_name {
                        return Ok(self.unpack_mangled_type(struct_name));
                    }
                    // Try LLVM type for primitives
                    if let Some(ast_ty) = self.llvm_type_to_ast_type(var_info.ty) {
                        return Ok(ast_ty);
                    }
                }
                Err(CodegenError::UndefinedVariable(name.clone()))
            }
            Expr::StructInit { name, generics, .. } => {
                Ok(Type::Named { name: name.clone(), generics: generics.clone() })
            }
            _ => {
                // For other expressions, try to infer
                // Fallback to i32 for now
                Ok(Type::I32)
            }
        }
    }

    /// Unpack a mangled type name like "Option_Point" into Type::Named { name: "Option", generics: [Point] }
    fn unpack_mangled_type(&self, mangled: &str) -> Type {
        // Check if this is a monomorphized enum or struct
        // Format: BaseName_TypeArg1_TypeArg2...

        // First check if the whole name exists as a non-generic type
        if self.struct_types.contains_key(mangled) || self.enum_types.contains_key(mangled) {
            // Try to find the base generic type
            if let Some(idx) = mangled.find('_') {
                let base_name = &mangled[..idx];
                let type_args_str = &mangled[idx + 1..];

                // Check if base_name is a generic enum or struct
                if self.generic_enums.contains_key(base_name) || self.generic_structs.contains_key(base_name) {
                    // Parse type arguments (split by underscore, but this is simplified)
                    let type_args: Vec<Type> = type_args_str
                        .split('_')
                        .map(|s| self.parse_type_name(s))
                        .collect();

                    return Type::Named {
                        name: base_name.to_string(),
                        generics: type_args,
                    };
                }
            }
        }

        // Fallback: return as simple named type
        Type::Named { name: mangled.to_string(), generics: vec![] }
    }

    /// Parse a simple type name into a Type
    fn parse_type_name(&self, name: &str) -> Type {
        match name {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            _ => Type::Named { name: name.to_string(), generics: vec![] }
        }
    }

    /// Unify expected type with actual type to extract type parameter bindings
    fn unify_types(
        &self,
        expected: &Type,
        actual: &Type,
        generics: &[String],
        type_map: &mut std::collections::HashMap<String, Type>,
    ) -> Result<(), CodegenError> {
        match expected {
            Type::Named { name, generics: expected_generics } => {
                // Check if this is a type parameter
                if expected_generics.is_empty() && generics.contains(name) {
                    // This is a type parameter - bind it to actual
                    type_map.insert(name.clone(), actual.clone());
                    return Ok(());
                }

                // Not a type parameter - try to match structurally
                if let Type::Named { name: actual_name, generics: actual_generics } = actual {
                    if name == actual_name && expected_generics.len() == actual_generics.len() {
                        // Recursively unify generic arguments
                        for (exp_arg, act_arg) in expected_generics.iter().zip(actual_generics.iter()) {
                            self.unify_types(exp_arg, act_arg, generics, type_map)?;
                        }
                    }
                }
            }
            Type::Ref(inner) => {
                if let Type::Ref(actual_inner) = actual {
                    self.unify_types(inner, actual_inner, generics, type_map)?;
                }
            }
            Type::RefMut(inner) => {
                if let Type::RefMut(actual_inner) = actual {
                    self.unify_types(inner, actual_inner, generics, type_map)?;
                }
            }
            Type::Pointer(inner) => {
                if let Type::Pointer(actual_inner) = actual {
                    self.unify_types(inner, actual_inner, generics, type_map)?;
                }
            }
            Type::Array(inner, _) => {
                if let Type::Array(actual_inner, _) = actual {
                    self.unify_types(inner, actual_inner, generics, type_map)?;
                }
            }
            Type::Slice(inner) => {
                if let Type::Slice(actual_inner) = actual {
                    self.unify_types(inner, actual_inner, generics, type_map)?;
                }
            }
            // Primitive types don't contribute to type inference
            _ => {}
        }
        Ok(())
    }

    /// Substitute type parameters in a type with concrete types
    pub(crate) fn substitute_type_params(&self, ty: &Type, params: &[String], args: &[Type]) -> Type {
        // Build substitution map using HashMap<String, Type> for compatibility with substitute_type
        let substitutions: std::collections::HashMap<String, Type> = params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .collect();

        self.substitute_type(ty, &substitutions)
    }
}
