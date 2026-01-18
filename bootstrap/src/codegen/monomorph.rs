//! Monomorphization - specializing generic types and functions

use super::{Codegen, CodegenError};
use crate::ast::*;
use std::collections::HashMap;

impl<'ctx> Codegen<'ctx> {
    /// Ensure a monomorphized struct exists, creating it if necessary
    pub(crate) fn ensure_monomorphized_struct(&mut self, name: &str, type_args: &[Type]) -> Result<String, CodegenError> {
        let mono_name = self.mangle_name(name, type_args);

        // Already exists?
        if self.struct_types.contains_key(&mono_name) {
            return Ok(mono_name);
        }

        // Get generic definition
        let generic_struct = self.generic_structs
            .get(name)
            .ok_or_else(|| CodegenError::UndefinedType(format!("generic struct '{}' not found", name)))?
            .clone();

        // Build type parameter mapping: T -> i32, U -> String, etc.
        let mut type_map: HashMap<String, Type> = HashMap::new();
        for (param, arg) in generic_struct.generics.iter().zip(type_args.iter()) {
            type_map.insert(param.clone(), arg.clone());
        }

        // Create monomorphized struct
        let mono_struct = Struct {
            name: mono_name.clone(),
            generics: Vec::new(), // No longer generic
            fields: generic_struct.fields.iter().map(|f| {
                Field {
                    name: f.name.clone(),
                    ty: self.substitute_type(&f.ty, &type_map),
                    is_pub: f.is_pub,
                    span: f.span,
                }
            }).collect(),
            is_pub: generic_struct.is_pub,
            span: generic_struct.span,
        };

        // Define the monomorphized struct
        self.define_struct(&mono_struct)?;

        Ok(mono_name)
    }

    /// Ensure a monomorphized function exists, creating it if necessary
    pub(crate) fn ensure_monomorphized_function(&mut self, name: &str, type_args: &[Type]) -> Result<String, CodegenError> {
        let mono_name = self.mangle_name(name, type_args);

        // Already exists?
        if self.module.get_function(&mono_name).is_some() {
            return Ok(mono_name);
        }

        // Get generic definition
        let generic_func = self.generic_functions
            .get(name)
            .ok_or_else(|| CodegenError::UndefinedFunction(format!("generic function '{}' not found", name)))?
            .clone();

        // Build type parameter mapping
        let mut type_map: HashMap<String, Type> = HashMap::new();
        for (param, arg) in generic_func.generics.iter().zip(type_args.iter()) {
            type_map.insert(param.clone(), arg.clone());
        }

        // Create monomorphized function
        let mono_func = Function {
            name: mono_name.clone(),
            generics: Vec::new(), // No longer generic
            params: generic_func.params.iter().map(|p| {
                Param {
                    name: p.name.clone(),
                    ty: self.substitute_type(&p.ty, &type_map),
                    span: p.span,
                }
            }).collect(),
            return_type: generic_func.return_type.as_ref().map(|t| self.substitute_type(t, &type_map)),
            body: self.substitute_block(&generic_func.body, &type_map),
            is_pub: generic_func.is_pub,
            span: generic_func.span,
        };

        // Save current compilation state
        let saved_function = self.current_function;
        let saved_variables = std::mem::take(&mut self.variables);
        let saved_block = self.builder.get_insert_block();

        // Declare and define the monomorphized function
        self.declare_function(&mono_func)?;
        self.compile_function(&mono_func)?;

        // Restore previous compilation state
        self.current_function = saved_function;
        self.variables = saved_variables;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Ok(mono_name)
    }

    /// Ensure a monomorphized enum exists, creating it if necessary
    pub(crate) fn ensure_monomorphized_enum(&mut self, name: &str, type_args: &[Type]) -> Result<String, CodegenError> {
        let mono_name = self.mangle_name(name, type_args);

        // Already exists?
        if self.enum_types.contains_key(&mono_name) {
            return Ok(mono_name);
        }

        // Get generic definition
        let generic_enum = self.generic_enums
            .get(name)
            .ok_or_else(|| CodegenError::UndefinedType(format!("generic enum '{}' not found", name)))?
            .clone();

        // Build type parameter mapping
        let mut type_map: HashMap<String, Type> = HashMap::new();
        for (param, arg) in generic_enum.generics.iter().zip(type_args.iter()) {
            type_map.insert(param.clone(), arg.clone());
        }

        // Create monomorphized enum
        let mono_enum = Enum {
            name: mono_name.clone(),
            generics: Vec::new(),
            variants: generic_enum.variants.iter().map(|v| {
                Variant {
                    name: v.name.clone(),
                    fields: match &v.fields {
                        VariantFields::Unit => VariantFields::Unit,
                        VariantFields::Tuple(types) => {
                            VariantFields::Tuple(types.iter().map(|t| self.substitute_type(t, &type_map)).collect())
                        }
                        VariantFields::Struct(fields) => {
                            VariantFields::Struct(fields.iter().map(|f| Field {
                                name: f.name.clone(),
                                ty: self.substitute_type(&f.ty, &type_map),
                                is_pub: f.is_pub,
                                span: f.span,
                            }).collect())
                        }
                    },
                    span: v.span,
                }
            }).collect(),
            is_pub: generic_enum.is_pub,
            span: generic_enum.span,
        };

        // Define the monomorphized enum
        self.define_enum(&mono_enum)?;

        Ok(mono_name)
    }

    /// Ensure impl methods are monomorphized for a generic struct/enum
    pub(crate) fn ensure_monomorphized_impl(&mut self, name: &str, type_args: &[Type]) -> Result<(), CodegenError> {
        let mono_name = self.mangle_name(name, type_args);

        // Already have methods for this type?
        if self.type_methods.contains_key(&mono_name) {
            return Ok(());
        }

        // Get generic impl block
        let generic_impl = match self.generic_impls.get(name) {
            Some(impl_block) => impl_block.clone(),
            None => {
                return Ok(()); // No impl block for this type
            }
        };

        // Build type parameter mapping
        // The type parameters come from the generic struct/enum definition
        let type_params = if let Some(s) = self.generic_structs.get(name) {
            s.generics.clone()
        } else if let Some(e) = self.generic_enums.get(name) {
            e.generics.clone()
        } else {
            return Ok(());
        };

        let mut type_map: HashMap<String, Type> = HashMap::new();
        for (param, arg) in type_params.iter().zip(type_args.iter()) {
            type_map.insert(param.clone(), arg.clone());
        }

        // Save current compilation state
        let saved_function = self.current_function;
        let saved_variables = std::mem::take(&mut self.variables);
        let saved_block = self.builder.get_insert_block();

        // First pass: create monomorphized methods and declare them
        let mut mono_methods = Vec::new();
        for method in &generic_impl.methods {
            let mangled_method_name = format!("{}_{}", mono_name, method.name);

            // Already declared?
            if self.module.get_function(&mangled_method_name).is_some() {
                continue;
            }

            // Substitute types in method
            let mono_method = Function {
                name: mangled_method_name.clone(),
                generics: Vec::new(),
                params: method.params.iter().map(|p| {
                    let mut ty = self.substitute_type(&p.ty, &type_map);
                    // Also substitute Self with the monomorphized type
                    if let Type::SelfType = ty {
                        ty = Type::Named { name: mono_name.clone(), generics: vec![] };
                    }
                    // Handle &Self and ~Self
                    if let Type::Ref(inner) = &ty {
                        if let Type::SelfType = inner.as_ref() {
                            ty = Type::Ref(Box::new(Type::Named { name: mono_name.clone(), generics: vec![] }));
                        }
                    }
                    if let Type::RefMut(inner) = &ty {
                        if let Type::SelfType = inner.as_ref() {
                            ty = Type::RefMut(Box::new(Type::Named { name: mono_name.clone(), generics: vec![] }));
                        }
                    }
                    Param {
                        name: p.name.clone(),
                        ty,
                        span: p.span,
                    }
                }).collect(),
                return_type: method.return_type.as_ref().map(|t| {
                    let mut ty = self.substitute_type(t, &type_map);
                    if let Type::SelfType = ty {
                        ty = Type::Named { name: mono_name.clone(), generics: vec![] };
                    }
                    ty
                }),
                body: self.substitute_block(&method.body, &type_map),
                is_pub: method.is_pub,
                span: method.span,
            };

            // Declare the function
            self.declare_function(&mono_method)?;

            // Register the method for the monomorphized type
            self.type_methods
                .entry(mono_name.clone())
                .or_insert_with(HashMap::new)
                .insert(method.name.clone(), mangled_method_name);

            mono_methods.push(mono_method);
        }

        // Second pass: compile all methods
        for mono_method in &mono_methods {
            self.compile_function(mono_method)?;
        }

        // Restore compilation state
        self.current_function = saved_function;
        self.variables = saved_variables;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Ok(())
    }

    /// Substitute type parameters in a type
    pub(crate) fn substitute_type(&self, ty: &Type, type_map: &HashMap<String, Type>) -> Type {
        match ty {
            Type::Named { name, generics } => {
                // Check if this is a type parameter
                if generics.is_empty() {
                    if let Some(replacement) = type_map.get(name) {
                        return replacement.clone();
                    }
                }
                // Otherwise substitute in generics
                Type::Named {
                    name: name.clone(),
                    generics: generics.iter().map(|t| self.substitute_type(t, type_map)).collect(),
                }
            }
            Type::Ref(inner) => Type::Ref(Box::new(self.substitute_type(inner, type_map))),
            Type::RefMut(inner) => Type::RefMut(Box::new(self.substitute_type(inner, type_map))),
            Type::Pointer(inner) => Type::Pointer(Box::new(self.substitute_type(inner, type_map))),
            Type::Array(inner, size) => Type::Array(Box::new(self.substitute_type(inner, type_map)), *size),
            _ => ty.clone(),
        }
    }

    /// Substitute type parameters in a block (for function body)
    pub(crate) fn substitute_block(&self, block: &Block, type_map: &HashMap<String, Type>) -> Block {
        Block {
            stmts: block.stmts.iter().map(|s| self.substitute_stmt(s, type_map)).collect(),
            span: block.span,
        }
    }

    /// Substitute type parameters in a statement
    pub(crate) fn substitute_stmt(&self, stmt: &Stmt, type_map: &HashMap<String, Type>) -> Stmt {
        match stmt {
            Stmt::Let { name, ty, value, span } => Stmt::Let {
                name: name.clone(),
                ty: ty.as_ref().map(|t| self.substitute_type(t, type_map)),
                value: self.substitute_expr(value, type_map),
                span: *span,
            },
            Stmt::Return { value, span } => Stmt::Return {
                value: value.as_ref().map(|e| self.substitute_expr(e, type_map)),
                span: *span,
            },
            Stmt::Expr(e) => Stmt::Expr(self.substitute_expr(e, type_map)),
            Stmt::If { condition, then_block, else_block, span } => Stmt::If {
                condition: Box::new(self.substitute_expr(condition, type_map)),
                then_block: self.substitute_block(then_block, type_map),
                else_block: else_block.as_ref().map(|b| self.substitute_block(b, type_map)),
                span: *span,
            },
            Stmt::While { condition, body, span } => Stmt::While {
                condition: Box::new(self.substitute_expr(condition, type_map)),
                body: self.substitute_block(body, type_map),
                span: *span,
            },
            Stmt::Match { value, arms, span } => Stmt::Match {
                value: Box::new(self.substitute_expr(value, type_map)),
                arms: arms.iter().map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    body: self.substitute_expr(&arm.body, type_map),
                    span: arm.span,
                }).collect(),
                span: *span,
            },
            Stmt::For { name, iter, body, span } => Stmt::For {
                name: name.clone(),
                iter: Box::new(self.substitute_expr(iter, type_map)),
                body: self.substitute_block(body, type_map),
                span: *span,
            },
            _ => stmt.clone(),
        }
    }

    /// Substitute type parameters in an expression
    pub(crate) fn substitute_expr(&self, expr: &Expr, type_map: &HashMap<String, Type>) -> Expr {
        match expr {
            Expr::StructInit { name, generics, fields, span } => Expr::StructInit {
                name: name.clone(),
                generics: generics.iter().map(|t| self.substitute_type(t, type_map)).collect(),
                fields: fields.iter().map(|(n, e)| (n.clone(), self.substitute_expr(e, type_map))).collect(),
                span: *span,
            },
            Expr::Call { func, type_args, args, span } => Expr::Call {
                func: Box::new(self.substitute_expr(func, type_map)),
                type_args: type_args.iter().map(|t| self.substitute_type(t, type_map)).collect(),
                args: args.iter().map(|a| self.substitute_expr(a, type_map)).collect(),
                span: *span,
            },
            Expr::Binary { op, left, right, span } => Expr::Binary {
                op: *op,
                left: Box::new(self.substitute_expr(left, type_map)),
                right: Box::new(self.substitute_expr(right, type_map)),
                span: *span,
            },
            Expr::Field { object, field, span } => Expr::Field {
                object: Box::new(self.substitute_expr(object, type_map)),
                field: field.clone(),
                span: *span,
            },
            Expr::Ref { operand, span } => Expr::Ref {
                operand: Box::new(self.substitute_expr(operand, type_map)),
                span: *span,
            },
            Expr::RefMut { operand, span } => Expr::RefMut {
                operand: Box::new(self.substitute_expr(operand, type_map)),
                span: *span,
            },
            Expr::Range { start, end, span } => Expr::Range {
                start: Box::new(self.substitute_expr(start, type_map)),
                end: Box::new(self.substitute_expr(end, type_map)),
                span: *span,
            },
            Expr::Cast { expr: e, ty, span } => Expr::Cast {
                expr: Box::new(self.substitute_expr(e, type_map)),
                ty: self.substitute_type(ty, type_map),
                span: *span,
            },
            Expr::Unsafe { block, span } => Expr::Unsafe {
                block: self.substitute_block(block, type_map),
                span: *span,
            },
            Expr::Block(block) => Expr::Block(self.substitute_block(block, type_map)),
            Expr::MethodCall { receiver, method, args, span } => Expr::MethodCall {
                receiver: Box::new(self.substitute_expr(receiver, type_map)),
                method: method.clone(),
                args: args.iter().map(|a| self.substitute_expr(a, type_map)).collect(),
                span: *span,
            },
            Expr::Unary { op, operand, span } => Expr::Unary {
                op: *op,
                operand: Box::new(self.substitute_expr(operand, type_map)),
                span: *span,
            },
            Expr::Index { array, index, span } => Expr::Index {
                array: Box::new(self.substitute_expr(array, type_map)),
                index: Box::new(self.substitute_expr(index, type_map)),
                span: *span,
            },
            Expr::Deref { operand, span } => Expr::Deref {
                operand: Box::new(self.substitute_expr(operand, type_map)),
                span: *span,
            },
            Expr::ArrayInit { elements, span } => Expr::ArrayInit {
                elements: elements.iter().map(|e| self.substitute_expr(e, type_map)).collect(),
                span: *span,
            },
            Expr::ArrayRepeat { value, count, span } => Expr::ArrayRepeat {
                value: Box::new(self.substitute_expr(value, type_map)),
                count: *count,
                span: *span,
            },
            Expr::Try { operand, span } => Expr::Try {
                operand: Box::new(self.substitute_expr(operand, type_map)),
                span: *span,
            },
            Expr::InterpolatedString { parts, span } => Expr::InterpolatedString {
                parts: parts.iter().map(|part| match part {
                    StringPart::Literal(s) => StringPart::Literal(s.clone()),
                    StringPart::Expr(e) => StringPart::Expr(Box::new(self.substitute_expr(e, type_map))),
                }).collect(),
                span: *span,
            },
            // Ident and Literal don't need substitution
            _ => expr.clone(),
        }
    }
}
