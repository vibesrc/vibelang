//! LLVM code generation using inkwell

use crate::ast::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::types::{BasicTypeEnum, BasicType};
use inkwell::AddressSpace;
use std::collections::{HashMap, HashSet};

/// Tracks the borrow state of a variable
#[derive(Clone, Copy, Debug, PartialEq)]
enum BorrowState {
    Shared,    // Has one or more & borrows
    Mutable,   // Has a single ~ borrow
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, VarInfo<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    struct_types: HashMap<String, StructTypeInfo<'ctx>>,
    enum_types: HashMap<String, EnumTypeInfo<'ctx>>,
    // Ownership tracking
    moved_vars: HashSet<String>,              // Variables that have been moved
    borrowed_vars: HashMap<String, BorrowState>, // Active borrows on variables
    // Generic definitions (stored for monomorphization)
    generic_structs: HashMap<String, Struct>,
    generic_enums: HashMap<String, Enum>,
    generic_functions: HashMap<String, Function>,
}

#[derive(Clone)]
struct VarInfo<'ctx> {
    ptr: PointerValue<'ctx>,       // Pointer to the variable storage
    ty: BasicTypeEnum<'ctx>,       // LLVM type
    struct_name: Option<String>,   // If it's a struct, the struct name
    is_ref: bool,                  // Is this a reference (&T or ~T)?
    is_mut_ref: bool,              // Is this a mutable reference (~T)?
    ref_struct_name: Option<String>, // If it's a ref to a struct, the struct name
}

#[derive(Clone)]
struct StructTypeInfo<'ctx> {
    llvm_type: inkwell::types::StructType<'ctx>,
    field_indices: HashMap<String, u32>,
    field_types: Vec<BasicTypeEnum<'ctx>>,
}

#[derive(Clone)]
struct EnumTypeInfo<'ctx> {
    llvm_type: inkwell::types::StructType<'ctx>, // { i32 tag, payload }
    variant_tags: HashMap<String, u32>,           // variant name -> tag value
    variant_payloads: HashMap<String, Vec<BasicTypeEnum<'ctx>>>, // variant name -> payload types
    payload_size: u32, // size of largest payload in bytes
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        let mut codegen = Codegen {
            context,
            module,
            builder,
            variables: HashMap::new(),
            current_function: None,
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            moved_vars: HashSet::new(),
            borrowed_vars: HashMap::new(),
            generic_structs: HashMap::new(),
            generic_enums: HashMap::new(),
            generic_functions: HashMap::new(),
        };

        // Declare intrinsics
        codegen.declare_intrinsics();

        codegen
    }

    fn declare_intrinsics(&mut self) {
        // Declare puts from libc for print functionality
        let i32_type = self.context.i32_type();
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let puts_type = i32_type.fn_type(&[ptr_type.into()], false);
        self.module.add_function("puts", puts_type, None);

        // Declare printf for formatted output
        let printf_type = i32_type.fn_type(&[ptr_type.into()], true); // variadic
        self.module.add_function("printf", printf_type, None);
    }

    pub fn compile(&mut self, program: &Program) -> Result<(), CodegenError> {
        // First pass: store generic definitions, define concrete struct/enum types
        for item in &program.items {
            match item {
                Item::Struct(s) => {
                    if s.generics.is_empty() {
                        self.define_struct(s)?;
                    } else {
                        // Store generic struct for later monomorphization
                        self.generic_structs.insert(s.name.clone(), s.clone());
                    }
                }
                Item::Enum(e) => {
                    if e.generics.is_empty() {
                        self.define_enum(e)?;
                    } else {
                        // Store generic enum for later monomorphization
                        self.generic_enums.insert(e.name.clone(), e.clone());
                    }
                }
                Item::Function(func) => {
                    if !func.generics.is_empty() {
                        // Store generic function for later monomorphization
                        self.generic_functions.insert(func.name.clone(), func.clone());
                    }
                }
                _ => {}
            }
        }

        // Second pass: declare all concrete functions
        for item in &program.items {
            if let Item::Function(func) = item {
                if func.generics.is_empty() {
                    self.declare_function(func)?;
                }
            }
        }

        // Third pass: define all concrete functions
        for item in &program.items {
            if let Item::Function(func) = item {
                if func.generics.is_empty() {
                    self.compile_function(func)?;
                }
            }
        }

        Ok(())
    }

    /// Ensure a monomorphized struct exists, creating it if necessary
    fn ensure_monomorphized_struct(&mut self, name: &str, type_args: &[Type]) -> Result<String, CodegenError> {
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
    fn ensure_monomorphized_function(&mut self, name: &str, type_args: &[Type]) -> Result<String, CodegenError> {
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
        let saved_moved = std::mem::take(&mut self.moved_vars);
        let saved_borrowed = std::mem::take(&mut self.borrowed_vars);
        let saved_block = self.builder.get_insert_block();

        // Declare and define the monomorphized function
        self.declare_function(&mono_func)?;
        self.compile_function(&mono_func)?;

        // Restore previous compilation state
        self.current_function = saved_function;
        self.variables = saved_variables;
        self.moved_vars = saved_moved;
        self.borrowed_vars = saved_borrowed;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }

        Ok(mono_name)
    }

    /// Ensure a monomorphized enum exists, creating it if necessary
    fn ensure_monomorphized_enum(&mut self, name: &str, type_args: &[Type]) -> Result<String, CodegenError> {
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

    /// Substitute type parameters in a type
    fn substitute_type(&self, ty: &Type, type_map: &HashMap<String, Type>) -> Type {
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
    fn substitute_block(&self, block: &Block, type_map: &HashMap<String, Type>) -> Block {
        Block {
            stmts: block.stmts.iter().map(|s| self.substitute_stmt(s, type_map)).collect(),
            span: block.span,
        }
    }

    /// Substitute type parameters in a statement
    fn substitute_stmt(&self, stmt: &Stmt, type_map: &HashMap<String, Type>) -> Stmt {
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
    fn substitute_expr(&self, expr: &Expr, type_map: &HashMap<String, Type>) -> Expr {
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
            _ => expr.clone(),
        }
    }

    fn define_struct(&mut self, s: &Struct) -> Result<(), CodegenError> {
        let mut field_types = Vec::new();
        let mut field_indices = HashMap::new();

        for (i, field) in s.fields.iter().enumerate() {
            let field_type = self.llvm_type(&field.ty)?;
            field_types.push(field_type);
            field_indices.insert(field.name.clone(), i as u32);
        }

        let llvm_field_types: Vec<_> = field_types.iter().map(|t| *t).collect();
        let struct_type = self.context.struct_type(&llvm_field_types, false);

        self.struct_types.insert(
            s.name.clone(),
            StructTypeInfo {
                llvm_type: struct_type,
                field_indices,
                field_types,
            },
        );

        Ok(())
    }

    fn define_enum(&mut self, e: &Enum) -> Result<(), CodegenError> {
        let mut variant_tags = HashMap::new();
        let mut variant_payloads: HashMap<String, Vec<BasicTypeEnum<'ctx>>> = HashMap::new();
        let mut max_payload_size: u32 = 0;

        for (i, variant) in e.variants.iter().enumerate() {
            variant_tags.insert(variant.name.clone(), i as u32);

            // Get payload types for this variant
            let payload_types: Vec<BasicTypeEnum<'ctx>> = match &variant.fields {
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

            // Calculate payload size (simplified: just count i32s for now)
            let payload_size = payload_types.len() as u32 * 4;
            if payload_size > max_payload_size {
                max_payload_size = payload_size;
            }

            variant_payloads.insert(variant.name.clone(), payload_types);
        }

        // Create LLVM type: { i32 tag, payload... }
        // For simplicity, use the largest variant's types as the payload
        let i32_type = self.context.i32_type();
        let mut struct_fields: Vec<BasicTypeEnum<'ctx>> = vec![i32_type.into()]; // tag

        // Add payload field (use i32 array for max size, or actual types if single variant with data)
        if max_payload_size > 0 {
            let payload_slots = (max_payload_size + 3) / 4; // round up to i32 slots
            for _ in 0..payload_slots {
                struct_fields.push(i32_type.into());
            }
        }

        let llvm_type = self.context.struct_type(&struct_fields, false);

        self.enum_types.insert(
            e.name.clone(),
            EnumTypeInfo {
                llvm_type,
                variant_tags,
                variant_payloads,
                payload_size: max_payload_size,
            },
        );

        Ok(())
    }

    fn declare_function(&mut self, func: &Function) -> Result<FunctionValue<'ctx>, CodegenError> {
        let param_types: Vec<BasicTypeEnum> = func
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
        Ok(fn_value)
    }

    fn compile_function(&mut self, func: &Function) -> Result<(), CodegenError> {
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
                is_ref,
                is_mut_ref,
                ref_struct_name,
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

    fn compile_block(&mut self, block: &Block) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let mut last_value = None;

        for stmt in &block.stmts {
            last_value = self.compile_stmt(stmt)?;
        }

        Ok(last_value)
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        match stmt {
            Stmt::Let { name, ty, value, .. } => {
                // Track moves: if we're copying a struct from a variable, the source is moved
                let (struct_name, source_var) = match value {
                    Expr::StructInit { name: sname, generics, .. } => {
                        // For generic structs, use mangled name
                        let mono_name = if generics.is_empty() {
                            sname.clone()
                        } else {
                            self.mangle_name(sname, generics)
                        };
                        (Some(mono_name), None)
                    }
                    Expr::Ident(src_name, _) => {
                        // Check if source variable is a struct type (moves on assignment)
                        if let Some(var_info) = self.variables.get(src_name) {
                            if var_info.struct_name.is_some() && !var_info.is_ref {
                                // This is a move - mark source as moved
                                (var_info.struct_name.clone(), Some(src_name.clone()))
                            } else {
                                (None, None)
                            }
                        } else {
                            (None, None)
                        }
                    }
                    _ => (ty.as_ref().and_then(|t| self.get_struct_name_for_type(t)), None),
                };

                let init_value = self.compile_expr(value)?;
                let alloca_type = match ty {
                    Some(t) => self.llvm_type(t)?,
                    None => init_value.get_type(),
                };
                let alloca = self.create_entry_block_alloca(name, alloca_type);
                self.builder.build_store(alloca, init_value).unwrap();

                // Mark source variable as moved if this was a struct move
                if let Some(src) = source_var {
                    self.moved_vars.insert(src);
                }

                self.variables.insert(name.clone(), VarInfo {
                    ptr: alloca,
                    ty: alloca_type,
                    struct_name,
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                });
                Ok(None)
            }
            Stmt::Return { value, .. } => {
                match value {
                    Some(expr) => {
                        let ret_val = self.compile_expr(expr)?;
                        self.builder.build_return(Some(&ret_val)).unwrap();
                    }
                    None => {
                        self.builder.build_return(None).unwrap();
                    }
                }
                Ok(None)
            }
            Stmt::Expr(expr) => {
                let val = self.compile_expr(expr)?;
                Ok(Some(val))
            }
            Stmt::If { condition, then_block, else_block, .. } => {
                self.compile_if(condition, then_block, else_block.as_ref())
            }
            Stmt::While { condition, body, .. } => {
                self.compile_while(condition, body)
            }
            Stmt::Match { value, arms, .. } => {
                self.compile_match(value, arms)
            }
            Stmt::For { name, iter, body, .. } => {
                self.compile_for(name, iter, body)
            }
            _ => {
                // TODO: implement other statements
                Ok(None)
            }
        }
    }

    fn compile_if(
        &mut self,
        condition: &Expr,
        then_block: &Block,
        else_block: Option<&Block>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let cond_value = self.compile_expr(condition)?;
        let cond_bool = cond_value.into_int_value();

        let fn_value = self.current_function.unwrap();
        let then_bb = self.context.append_basic_block(fn_value, "then");
        let else_bb = self.context.append_basic_block(fn_value, "else");
        let merge_bb = self.context.append_basic_block(fn_value, "merge");

        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .unwrap();

        // Then block
        self.builder.position_at_end(then_bb);
        self.compile_block(then_block)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Else block
        self.builder.position_at_end(else_bb);
        if let Some(else_blk) = else_block {
            self.compile_block(else_blk)?;
        }
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Merge block
        self.builder.position_at_end(merge_bb);

        Ok(None)
    }

    fn compile_while(
        &mut self,
        condition: &Expr,
        body: &Block,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let fn_value = self.current_function.unwrap();
        let cond_bb = self.context.append_basic_block(fn_value, "while.cond");
        let body_bb = self.context.append_basic_block(fn_value, "while.body");
        let end_bb = self.context.append_basic_block(fn_value, "while.end");

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition block
        self.builder.position_at_end(cond_bb);
        let cond_value = self.compile_expr(condition)?;
        let cond_bool = cond_value.into_int_value();
        self.builder
            .build_conditional_branch(cond_bool, body_bb, end_bb)
            .unwrap();

        // Body block
        self.builder.position_at_end(body_bb);
        self.compile_block(body)?;
        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_unconditional_branch(cond_bb).unwrap();
        }

        // End block
        self.builder.position_at_end(end_bb);

        Ok(None)
    }

    fn compile_for(
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
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                });

                let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
                let body_bb = self.context.append_basic_block(fn_value, "for.body");
                let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
                let end_bb = self.context.append_basic_block(fn_value, "for.end");

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
                }

                if !var_info.ty.is_array_type() {
                    return Err(CodegenError::NotImplemented("for loop over non-array".to_string()));
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
                    is_ref: false,
                    is_mut_ref: false,
                    ref_struct_name: None,
                });

                let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
                let body_bb = self.context.append_basic_block(fn_value, "for.body");
                let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
                let end_bb = self.context.append_basic_block(fn_value, "for.end");

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
            }

            _ => return Err(CodegenError::NotImplemented("for loop over this iterator type".to_string())),
        }

        Ok(None)
    }

    fn compile_for_slice(
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

        // For now, assume element type is i64 (need better type tracking)
        let elem_type = i64_type.as_basic_type_enum();

        // Create index variable
        let idx_var = self.create_entry_block_alloca("__idx", i64_type.into());
        self.builder.build_store(idx_var, i64_type.const_zero()).unwrap();

        // Create element variable
        let elem_var = self.create_entry_block_alloca(name, elem_type);
        self.variables.insert(name.to_string(), VarInfo {
            ptr: elem_var,
            ty: elem_type,
            struct_name: None,
            is_ref: false,
            is_mut_ref: false,
            ref_struct_name: None,
        });

        let cond_bb = self.context.append_basic_block(fn_value, "for.cond");
        let body_bb = self.context.append_basic_block(fn_value, "for.body");
        let inc_bb = self.context.append_basic_block(fn_value, "for.inc");
        let end_bb = self.context.append_basic_block(fn_value, "for.end");

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
                i64_type,
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
        Ok(None)
    }

    fn compile_match(
        &mut self,
        value: &Expr,
        arms: &[MatchArm],
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let match_val = self.compile_expr(value)?;

        // Get enum info - first try from the pattern, then try to find monomorphized version
        let enum_info = if let Some(arm) = arms.first() {
            if let Pattern::Enum { path, .. } = &arm.pattern {
                if path.len() >= 1 {
                    let base_name = &path[0];
                    // First try direct lookup
                    if let Some(info) = self.enum_types.get(base_name).cloned() {
                        Some(info)
                    } else {
                        // Try to find a monomorphized version by checking the LLVM type name
                        // The match value's struct type name will be the monomorphized enum name
                        let llvm_type_name = match_val.get_type().print_to_string().to_string();
                        // Look for any enum type whose name starts with the pattern name
                        self.enum_types.iter()
                            .find(|(name, _)| name.starts_with(base_name))
                            .map(|(_, info)| info.clone())
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        // Store value and extract tag from the matched value
        let (match_int, match_ptr) = if let Some(ref info) = enum_info {
            // Store value to get a pointer for GEP (used for both tag and payload extraction)
            let alloca = self.builder.build_alloca(info.llvm_type, "match_val").unwrap();
            self.builder.build_store(alloca, match_val).unwrap();

            // Extract tag (field 0)
            let tag_ptr = self.builder
                .build_struct_gep(info.llvm_type, alloca, 0, "tag_ptr")
                .unwrap();
            let tag = self.builder.build_load(self.context.i32_type(), tag_ptr, "tag").unwrap().into_int_value();
            (tag, Some(alloca))
        } else {
            // Fallback to direct int for simple enums
            (match_val.into_int_value(), None)
        };

        let fn_value = self.current_function.unwrap();
        let merge_bb = self.context.append_basic_block(fn_value, "match.merge");

        // Collect arm results for phi node
        let mut incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();

        // Create default block (unreachable for exhaustive match)
        let default_bb = self.context.append_basic_block(fn_value, "match.default");

        // Build switch with cases - use the resolved enum_info
        let mut cases: Vec<(inkwell::values::IntValue<'ctx>, inkwell::basic_block::BasicBlock<'ctx>)> = Vec::new();
        let mut arm_info: Vec<(inkwell::basic_block::BasicBlock<'ctx>, Option<(String, Vec<Pattern>)>)> = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let arm_bb = self.context.append_basic_block(fn_value, &format!("match.arm{}", i));

            // Get the tag value for this pattern
            if let Pattern::Enum { path, fields } = &arm.pattern {
                if path.len() == 2 {
                    let variant_name = &path[1];

                    // Use the already-resolved enum_info instead of looking up by pattern name
                    if let Some(ref info) = enum_info {
                        if let Some(&tag) = info.variant_tags.get(variant_name) {
                            let tag_val = self.context.i32_type().const_int(tag as u64, false);
                            cases.push((tag_val, arm_bb));
                            arm_info.push((arm_bb, Some((variant_name.clone(), fields.clone()))));
                        }
                    }
                }
            }
        }

        // Build switch instruction
        self.builder.build_switch(match_int, default_bb, &cases).unwrap();

        // Compile each arm body
        for (i, arm) in arms.iter().enumerate() {
            if let Some((bb, binding_info)) = arm_info.get(i) {
                self.builder.position_at_end(*bb);

                // Extract payload bindings if any - use the resolved enum_info
                if let (Some(ptr), Some((variant_name, fields))) = (match_ptr, binding_info) {
                    if let Some(ref info) = enum_info {
                        if let Some(payload_types) = info.variant_payloads.get(variant_name) {
                            for (j, pattern) in fields.iter().enumerate() {
                                if let Pattern::Ident(var_name) = pattern {
                                    if j < payload_types.len() {
                                        let payload_type = payload_types[j];
                                        // Extract payload at index j+1 (0 is tag)
                                        let payload_ptr = self.builder
                                            .build_struct_gep(info.llvm_type, ptr, (j + 1) as u32, &format!("payload{}", j))
                                            .unwrap();
                                        let payload_val = self.builder
                                            .build_load(payload_type, payload_ptr, var_name)
                                            .unwrap();

                                        // Bind to variable
                                        let var_alloca = self.create_entry_block_alloca(var_name, payload_type);
                                        self.builder.build_store(var_alloca, payload_val).unwrap();
                                        self.variables.insert(var_name.clone(), VarInfo {
                                            ptr: var_alloca,
                                            ty: payload_type,
                                            struct_name: None,
                                            is_ref: false,
                                            is_mut_ref: false,
                                            ref_struct_name: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }

                let arm_val = self.compile_expr(&arm.body)?;

                // Get the current block (may have changed during compile_expr)
                let current_bb = self.builder.get_insert_block().unwrap();
                if current_bb.get_terminator().is_none() {
                    incoming.push((arm_val, current_bb));
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
            }
        }

        // Default block - unreachable
        self.builder.position_at_end(default_bb);
        self.builder.build_unreachable().unwrap();

        // Merge block with phi
        self.builder.position_at_end(merge_bb);

        if !incoming.is_empty() {
            let phi_type = incoming[0].0.get_type();
            let phi = self.builder.build_phi(phi_type, "match.result").unwrap();
            for (val, bb) in &incoming {
                phi.add_incoming(&[(val, *bb)]);
            }
            return Ok(Some(phi.as_basic_value()));
        }

        Ok(None)
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match expr {
            Expr::Literal(lit, _) => self.compile_literal(lit),
            Expr::Ident(name, _) => {
                // Check for use-after-move
                if self.moved_vars.contains(name) {
                    return Err(CodegenError::BorrowError(
                        format!("use of moved value: '{}'", name)
                    ));
                }
                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                let val = self.builder.build_load(var_info.ty, var_info.ptr, name).unwrap();
                Ok(val)
            }
            Expr::Binary { op, left, right, .. } => {
                self.compile_binary(*op, left, right)
            }
            Expr::Unary { op, operand, .. } => {
                self.compile_unary(*op, operand)
            }
            Expr::Call { func, type_args, args, .. } => {
                self.compile_call(func, type_args, args)
            }
            Expr::StructInit { name, generics, fields, .. } => {
                self.compile_struct_init(name, generics, fields)
            }
            Expr::Field { object, field, .. } => {
                self.compile_field_access(object, field)
            }
            Expr::MethodCall { receiver, method, args, .. } => {
                // Check if this is an enum variant constructor (e.g., Option.Some(42))
                if let Expr::Ident(name, _) = receiver.as_ref() {
                    if let Some(enum_info) = self.enum_types.get(name).cloned() {
                        return self.compile_enum_variant_constructor(&enum_info, name, method, args);
                    }

                    // Check for array .len() method
                    if method == "len" && args.is_empty() {
                        if let Some(var_info) = self.variables.get(name) {
                            if var_info.ty.is_array_type() {
                                let array_ty = var_info.ty.into_array_type();
                                let len = array_ty.len();
                                return Ok(self.context.i64_type().const_int(len as u64, false).into());
                            }
                            // Check for slice .len() method
                            if var_info.ty.is_struct_type() {
                                let struct_ty = var_info.ty.into_struct_type();
                                // Slices are structs with 2 fields: { ptr, len }
                                if struct_ty.count_fields() == 2 {
                                    // Load the slice value
                                    let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();
                                    // Extract length from slice (field 1)
                                    let len_val = self.builder
                                        .build_extract_value(slice_val.into_struct_value(), 1, "slice_len")
                                        .unwrap();
                                    return Ok(len_val);
                                }
                            }
                        }
                    }
                }
                Err(CodegenError::NotImplemented("method calls".to_string()))
            }
            Expr::Ref { operand, .. } => {
                // &expr - get address of the operand
                self.compile_ref(operand, false)
            }
            Expr::RefMut { operand, .. } => {
                // ~expr - get mutable address of the operand
                self.compile_ref(operand, true)
            }
            Expr::Deref { operand, .. } => {
                // *expr - dereference a pointer
                self.compile_deref(operand)
            }
            Expr::ArrayInit { elements, .. } => {
                self.compile_array_init(elements)
            }
            Expr::Index { array, index, .. } => {
                self.compile_index(array, index)
            }
            _ => {
                // TODO: implement other expressions
                Err(CodegenError::NotImplemented("expression type".to_string()))
            }
        }
    }

    fn compile_literal(&mut self, lit: &Literal) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        match lit {
            Literal::Int(n) => {
                let val = self.context.i64_type().const_int(*n as u64, true);
                Ok(val.into())
            }
            Literal::Float(n) => {
                let val = self.context.f64_type().const_float(*n);
                Ok(val.into())
            }
            Literal::Bool(b) => {
                let val = self.context.bool_type().const_int(*b as u64, false);
                Ok(val.into())
            }
            Literal::String(s) => {
                let global = self.builder.build_global_string_ptr(s, "str").unwrap();
                Ok(global.as_pointer_value().into())
            }
        }
    }

    fn compile_binary(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Handle assignment specially - don't compile LHS yet for assignments
        if matches!(op, BinOp::Assign) {
            let rhs = self.compile_expr(right)?;

            if let Expr::Ident(name, _) = left {
                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?;
                self.builder.build_store(var_info.ptr, rhs).unwrap();
                return Ok(rhs);
            }

            // Handle field assignment (e.g., p.x = val)
            if let Expr::Field { object, field, .. } = left {
                let field_ptr = self.compile_field_ptr(object, field)?;
                self.builder.build_store(field_ptr, rhs).unwrap();
                return Ok(rhs);
            }

            return Err(CodegenError::InvalidAssignment);
        }

        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        let lhs_int = lhs.into_int_value();
        let rhs_int = rhs.into_int_value();

        let result = match op {
            BinOp::Add => self.builder.build_int_add(lhs_int, rhs_int, "add").unwrap(),
            BinOp::Sub => self.builder.build_int_sub(lhs_int, rhs_int, "sub").unwrap(),
            BinOp::Mul => self.builder.build_int_mul(lhs_int, rhs_int, "mul").unwrap(),
            BinOp::Div => self.builder.build_int_signed_div(lhs_int, rhs_int, "div").unwrap(),
            BinOp::Mod => self.builder.build_int_signed_rem(lhs_int, rhs_int, "mod").unwrap(),
            BinOp::Eq => self.builder.build_int_compare(
                inkwell::IntPredicate::EQ, lhs_int, rhs_int, "eq"
            ).unwrap(),
            BinOp::Ne => self.builder.build_int_compare(
                inkwell::IntPredicate::NE, lhs_int, rhs_int, "ne"
            ).unwrap(),
            BinOp::Lt => self.builder.build_int_compare(
                inkwell::IntPredicate::SLT, lhs_int, rhs_int, "lt"
            ).unwrap(),
            BinOp::Le => self.builder.build_int_compare(
                inkwell::IntPredicate::SLE, lhs_int, rhs_int, "le"
            ).unwrap(),
            BinOp::Gt => self.builder.build_int_compare(
                inkwell::IntPredicate::SGT, lhs_int, rhs_int, "gt"
            ).unwrap(),
            BinOp::Ge => self.builder.build_int_compare(
                inkwell::IntPredicate::SGE, lhs_int, rhs_int, "ge"
            ).unwrap(),
            BinOp::BitAnd => self.builder.build_and(lhs_int, rhs_int, "and").unwrap(),
            BinOp::BitOr => self.builder.build_or(lhs_int, rhs_int, "or").unwrap(),
            BinOp::BitXor => self.builder.build_xor(lhs_int, rhs_int, "xor").unwrap(),
            BinOp::Shl => self.builder.build_left_shift(lhs_int, rhs_int, "shl").unwrap(),
            BinOp::Shr => self.builder.build_right_shift(lhs_int, rhs_int, true, "shr").unwrap(),
            _ => return Err(CodegenError::NotImplemented(format!("binary op {:?}", op))),
        };

        Ok(result.into())
    }

    fn compile_unary(&mut self, op: UnaryOp, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let val = self.compile_expr(operand)?;

        match op {
            UnaryOp::Neg => {
                let int_val = val.into_int_value();
                let result = self.builder.build_int_neg(int_val, "neg").unwrap();
                Ok(result.into())
            }
            UnaryOp::BitNot => {
                let int_val = val.into_int_value();
                let result = self.builder.build_not(int_val, "not").unwrap();
                Ok(result.into())
            }
            UnaryOp::Not => {
                let int_val = val.into_int_value();
                let zero = self.context.bool_type().const_zero();
                let result = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ, int_val, zero, "lnot"
                ).unwrap();
                Ok(result.into())
            }
        }
    }

    fn compile_call(&mut self, func: &Expr, type_args: &[Type], args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
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
            _ => return Err(CodegenError::NotImplemented("non-ident function calls".to_string())),
        };

        // Handle intrinsic functions
        if name == "print" {
            return self.compile_print_call(args);
        }
        if name == "print_int" {
            return self.compile_print_int_call(args);
        }

        // Generate monomorphized function if type args are present
        let mono_name = if type_args.is_empty() {
            name.clone()
        } else {
            // Ensure monomorphized function exists
            self.ensure_monomorphized_function(name, type_args)?
        };

        let fn_value = self
            .module
            .get_function(&mono_name)
            .ok_or_else(|| CodegenError::UndefinedFunction(mono_name.clone()))?;

        let compiled_args: Vec<BasicValueEnum> = args
            .iter()
            .map(|a| self.compile_expr(a))
            .collect::<Result<_, _>>()?;

        let args_meta: Vec<_> = compiled_args.iter().map(|a| (*a).into()).collect();

        let call_site = self
            .builder
            .build_call(fn_value, &args_meta, "call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                // Void return - return a dummy value
                Ok(self.context.i64_type().const_zero().into())
            }
        }
    }

    fn compile_print_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::InvalidArguments("print requires an argument".to_string()));
        }

        let puts = self.module.get_function("puts").unwrap();

        // Compile the argument
        let arg = self.compile_expr(&args[0])?;

        // Call puts with the string pointer
        let call_site = self
            .builder
            .build_call(puts, &[arg.into()], "puts_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.i32_type().const_zero().into())
            }
        }
    }

    fn compile_struct_init(
        &mut self,
        name: &str,
        generics: &[Type],
        fields: &[(String, Expr)],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // Generate monomorphized struct if needed
        let mono_name = if generics.is_empty() {
            name.to_string()
        } else {
            // Ensure monomorphized struct exists
            self.ensure_monomorphized_struct(name, generics)?
        };

        let struct_info = self
            .struct_types
            .get(&mono_name)
            .ok_or_else(|| CodegenError::UndefinedType(mono_name.clone()))?
            .clone();

        // Allocate struct on stack
        let alloca = self.builder.build_alloca(struct_info.llvm_type, name).unwrap();

        // Initialize each field
        for (field_name, field_expr) in fields {
            let field_idx = *struct_info
                .field_indices
                .get(field_name)
                .ok_or_else(|| CodegenError::UndefinedField(field_name.clone()))?;

            let field_value = self.compile_expr(field_expr)?;

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, alloca, field_idx, &format!("{}.{}", name, field_name))
                .unwrap();

            self.builder.build_store(field_ptr, field_value).unwrap();
        }

        // Load and return the struct value
        let struct_val = self
            .builder
            .build_load(struct_info.llvm_type, alloca, &format!("{}_val", name))
            .unwrap();

        Ok(struct_val)
    }

    fn compile_enum_variant_constructor(
        &mut self,
        enum_info: &EnumTypeInfo<'ctx>,
        enum_name: &str,
        variant_name: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let tag = *enum_info
            .variant_tags
            .get(variant_name)
            .ok_or_else(|| CodegenError::UndefinedField(format!("{}::{}", enum_name, variant_name)))?;

        // Allocate enum on stack
        let alloca = self.builder.build_alloca(enum_info.llvm_type, enum_name).unwrap();

        // Store tag at index 0
        let tag_ptr = self
            .builder
            .build_struct_gep(enum_info.llvm_type, alloca, 0, &format!("{}.tag", enum_name))
            .unwrap();
        let tag_val = self.context.i32_type().const_int(tag as u64, false);
        self.builder.build_store(tag_ptr, tag_val).unwrap();

        // Store payload values starting at index 1
        for (i, arg) in args.iter().enumerate() {
            let arg_val = self.compile_expr(arg)?;
            let payload_ptr = self
                .builder
                .build_struct_gep(enum_info.llvm_type, alloca, (i + 1) as u32, &format!("{}.payload{}", enum_name, i))
                .unwrap();
            self.builder.build_store(payload_ptr, arg_val).unwrap();
        }

        // Load and return the enum value
        let enum_val = self
            .builder
            .build_load(enum_info.llvm_type, alloca, &format!("{}_val", enum_name))
            .unwrap();

        Ok(enum_val)
    }

    fn compile_field_access(
        &mut self,
        object: &Expr,
        field: &str,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // If object is an identifier, check if it's an enum type or a variable
        if let Expr::Ident(name, _) = object {
            // Check if this is an enum variant access (e.g., Color.Green for unit variant)
            if let Some(enum_info) = self.enum_types.get(name).cloned() {
                // For unit variants, construct the full tagged union
                return self.compile_enum_variant_constructor(&enum_info, name, field, &[]);
            }
        }

        // Check for generic enum unit variant: EnumName<Type>.Variant
        if let Expr::StructInit { name, generics, fields, .. } = object {
            if fields.is_empty() && !generics.is_empty() {
                let mono_name = self.ensure_monomorphized_enum(name, generics)?;
                let enum_info = self.enum_types.get(&mono_name).cloned()
                    .ok_or_else(|| CodegenError::UndefinedType(format!("enum '{}' not found after monomorphization", mono_name)))?;
                return self.compile_enum_variant_constructor(&enum_info, &mono_name, field, &[]);
            }
        }

        // Continue with struct field access for identifiers
        if let Expr::Ident(name, _) = object {

            // Check for use-after-move
            if self.moved_vars.contains(name) {
                return Err(CodegenError::BorrowError(
                    format!("use of moved value: '{}'", name)
                ));
            }

            // Otherwise it's a struct field access
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            // Determine struct name - either direct or through reference
            let struct_name = if var_info.is_ref {
                var_info.ref_struct_name.as_ref()
            } else {
                var_info.struct_name.as_ref()
            };

            let struct_name = struct_name
                .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", name)))?;

            let struct_info = self
                .struct_types
                .get(struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            let field_type = struct_info.field_types[field_idx as usize];

            // If it's a reference, load the pointer first, then GEP
            let struct_ptr = if var_info.is_ref {
                // Load the pointer value (ptr -> ptr to struct -> load -> ptr to struct)
                self.builder
                    .build_load(self.context.ptr_type(AddressSpace::default()), var_info.ptr, "deref_ptr")
                    .unwrap()
                    .into_pointer_value()
            } else {
                var_info.ptr
            };

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, struct_ptr, field_idx, &format!("{}.{}", name, field))
                .unwrap();

            let field_val = self
                .builder
                .build_load(field_type, field_ptr, field)
                .unwrap();

            Ok(field_val)
        } else {
            Err(CodegenError::NotImplemented("field access on non-identifier".to_string()))
        }
    }

    fn compile_field_ptr(
        &mut self,
        object: &Expr,
        field: &str,
    ) -> Result<PointerValue<'ctx>, CodegenError> {
        // Get a pointer to a field (for assignment)
        if let Expr::Ident(name, _) = object {
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            // Check: cannot mutate through read-only reference (&T)
            if var_info.is_ref && !var_info.is_mut_ref {
                return Err(CodegenError::BorrowError(
                    format!("cannot mutate through read-only borrow '&{}': use '~{}' for mutable access",
                        name, name)
                ));
            }

            // Determine struct name - either direct or through reference
            let struct_name = if var_info.is_ref {
                var_info.ref_struct_name.as_ref()
            } else {
                var_info.struct_name.as_ref()
            };

            let struct_name = struct_name
                .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", name)))?;

            let struct_info = self
                .struct_types
                .get(struct_name)
                .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                .clone();

            let field_idx = *struct_info
                .field_indices
                .get(field)
                .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

            // If it's a reference, load the pointer first, then GEP
            let struct_ptr = if var_info.is_ref {
                self.builder
                    .build_load(self.context.ptr_type(AddressSpace::default()), var_info.ptr, "deref_ptr")
                    .unwrap()
                    .into_pointer_value()
            } else {
                var_info.ptr
            };

            let field_ptr = self
                .builder
                .build_struct_gep(struct_info.llvm_type, struct_ptr, field_idx, &format!("{}.{}", name, field))
                .unwrap();

            Ok(field_ptr)
        } else {
            Err(CodegenError::NotImplemented("field pointer on non-identifier".to_string()))
        }
    }

    fn compile_ref(&mut self, operand: &Expr, mutable: bool) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // &expr or ~expr - get the address of the operand
        // The operand must be an lvalue (something we can take address of)
        match operand {
            Expr::Ident(name, _) => {
                // Check for use-after-move
                if self.moved_vars.contains(name) {
                    return Err(CodegenError::BorrowError(
                        format!("cannot borrow '{}' because it has been moved", name)
                    ));
                }

                // Check for borrow conflicts
                if let Some(current_borrow) = self.borrowed_vars.get(name) {
                    match (current_borrow, mutable) {
                        (BorrowState::Mutable, _) => {
                            return Err(CodegenError::BorrowError(
                                format!("cannot borrow '{}': already mutably borrowed", name)
                            ));
                        }
                        (BorrowState::Shared, true) => {
                            return Err(CodegenError::BorrowError(
                                format!("cannot borrow '{}' as mutable: already borrowed as immutable", name)
                            ));
                        }
                        (BorrowState::Shared, false) => {
                            // Multiple shared borrows are OK
                        }
                    }
                }

                // Track this borrow
                let new_state = if mutable { BorrowState::Mutable } else { BorrowState::Shared };
                self.borrowed_vars.insert(name.clone(), new_state);

                let var_info = self
                    .variables
                    .get(name)
                    .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                    .clone();

                // If it's an array, create a slice: { ptr: *T, len: i64 }
                if var_info.ty.is_array_type() {
                    let array_ty = var_info.ty.into_array_type();
                    let len = array_ty.len() as u64;

                    // Get pointer to first element
                    let elem_ptr = unsafe {
                        self.builder.build_gep(
                            array_ty,
                            var_info.ptr,
                            &[self.context.i32_type().const_zero(), self.context.i32_type().const_zero()],
                            "slice_ptr"
                        ).unwrap()
                    };

                    // Build slice struct type: { ptr, len }
                    let ptr_type = self.context.ptr_type(AddressSpace::default());
                    let len_type = self.context.i64_type();
                    let slice_struct_type = self.context.struct_type(&[ptr_type.into(), len_type.into()], false);

                    // Allocate and populate the slice struct
                    let slice_alloca = self.create_entry_block_alloca("slice_tmp", slice_struct_type.into());

                    // Store pointer field
                    let ptr_field = self.builder
                        .build_struct_gep(slice_struct_type, slice_alloca, 0, "slice_ptr_field")
                        .unwrap();
                    self.builder.build_store(ptr_field, elem_ptr).unwrap();

                    // Store length field
                    let len_field = self.builder
                        .build_struct_gep(slice_struct_type, slice_alloca, 1, "slice_len_field")
                        .unwrap();
                    let len_val = self.context.i64_type().const_int(len, false);
                    self.builder.build_store(len_field, len_val).unwrap();

                    // Load and return the slice struct value
                    let slice_val = self.builder
                        .build_load(slice_struct_type, slice_alloca, "slice")
                        .unwrap();
                    return Ok(slice_val);
                }

                // Return the pointer value directly (it's already an address)
                Ok(var_info.ptr.into())
            }
            Expr::Field { object, field, .. } => {
                // &expr.field - get address of a field
                if let Expr::Ident(var_name, _) = object.as_ref() {
                    let var_info = self
                        .variables
                        .get(var_name)
                        .ok_or_else(|| CodegenError::UndefinedVariable(var_name.clone()))?
                        .clone();

                    let struct_name = var_info.struct_name
                        .as_ref()
                        .ok_or_else(|| CodegenError::UndefinedType(format!("variable '{}' is not a struct", var_name)))?;

                    let struct_info = self
                        .struct_types
                        .get(struct_name)
                        .ok_or_else(|| CodegenError::UndefinedType(struct_name.clone()))?
                        .clone();

                    let field_idx = *struct_info
                        .field_indices
                        .get(field)
                        .ok_or_else(|| CodegenError::UndefinedField(field.to_string()))?;

                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_info.llvm_type, var_info.ptr, field_idx, &format!("{}.{}", var_name, field))
                        .unwrap();

                    Ok(field_ptr.into())
                } else {
                    Err(CodegenError::NotImplemented("reference to complex field".to_string()))
                }
            }
            _ => Err(CodegenError::NotImplemented("reference to non-lvalue".to_string())),
        }
    }

    fn compile_deref(&mut self, operand: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // *expr - dereference a pointer
        let ptr_val = self.compile_expr(operand)?;
        let ptr = ptr_val.into_pointer_value();

        // For now, assume we're dereferencing to i32 (need type inference for general case)
        let val = self.builder.build_load(self.context.i32_type(), ptr, "deref").unwrap();
        Ok(val)
    }

    fn compile_array_init(&mut self, elements: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if elements.is_empty() {
            return Err(CodegenError::InvalidArguments("empty array literal".to_string()));
        }

        // Compile all elements
        let compiled: Vec<BasicValueEnum<'ctx>> = elements
            .iter()
            .map(|e| self.compile_expr(e))
            .collect::<Result<_, _>>()?;

        // Determine element type from first element
        let elem_type = compiled[0].get_type();
        let array_type = elem_type.array_type(elements.len() as u32);

        // Create alloca for the array
        let array_alloca = self.create_entry_block_alloca("array_tmp", array_type.into());

        // Store each element
        for (i, val) in compiled.iter().enumerate() {
            let idx = self.context.i32_type().const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder.build_gep(
                    array_type,
                    array_alloca,
                    &[self.context.i32_type().const_zero(), idx],
                    &format!("elem{}", i)
                ).unwrap()
            };
            self.builder.build_store(elem_ptr, *val).unwrap();
        }

        // Load and return the array value
        let array_val = self.builder.build_load(array_type, array_alloca, "array").unwrap();
        Ok(array_val)
    }

    fn compile_index(&mut self, array: &Expr, index: &Expr) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // For now, handle array variable indexing
        if let Expr::Ident(name, _) = array {
            let var_info = self
                .variables
                .get(name)
                .ok_or_else(|| CodegenError::UndefinedVariable(name.clone()))?
                .clone();

            let idx = self.compile_expr(index)?;
            let idx_val = idx.into_int_value();

            // Check if this is a slice (struct with {ptr, len} layout)
            if var_info.ty.is_struct_type() {
                let struct_ty = var_info.ty.into_struct_type();
                // Slices are structs with 2 fields: { ptr, i64 }
                if struct_ty.count_fields() == 2 {
                    // Load the slice value
                    let slice_val = self.builder.build_load(struct_ty, var_info.ptr, "slice").unwrap();

                    // Extract pointer from slice (field 0)
                    let data_ptr = self.builder
                        .build_extract_value(slice_val.into_struct_value(), 0, "slice_ptr")
                        .unwrap()
                        .into_pointer_value();

                    // Get element type - assume i64 for now (need better type tracking)
                    let elem_type = self.context.i64_type();

                    // GEP to get element at index
                    let elem_ptr = unsafe {
                        self.builder.build_gep(
                            elem_type,
                            data_ptr,
                            &[idx_val],
                            "slice_elem_ptr"
                        ).unwrap()
                    };

                    let val = self.builder.build_load(elem_type.as_basic_type_enum(), elem_ptr, "slice_elem").unwrap();
                    return Ok(val);
                }
            }

            if var_info.is_ref {
                // Indexing through a reference - load the pointer, then index
                // The pointer points to an array
                let array_ptr = self.builder.build_load(
                    self.context.ptr_type(inkwell::AddressSpace::default()),
                    var_info.ptr,
                    "array_ptr"
                ).unwrap().into_pointer_value();

                // For array references, we need to know the element type
                // For now, assume i64 (need better type tracking for arrays)
                let elem_type = self.context.i64_type();

                // GEP into the array
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        elem_type.array_type(0), // Use unsized array for GEP
                        array_ptr,
                        &[self.context.i32_type().const_zero(), idx_val],
                        "elem_ptr"
                    ).unwrap()
                };

                let val = self.builder.build_load(elem_type.as_basic_type_enum(), elem_ptr, "elem").unwrap();
                Ok(val)
            } else {
                // Direct array variable
                // Get element pointer using GEP
                let elem_ptr = unsafe {
                    self.builder.build_gep(
                        var_info.ty,
                        var_info.ptr,
                        &[self.context.i32_type().const_zero(), idx_val],
                        "elem_ptr"
                    ).unwrap()
                };

                // Get element type from array type
                let array_ty = var_info.ty.into_array_type();
                let elem_type: BasicTypeEnum = array_ty.get_element_type();

                let val = self.builder.build_load(elem_type, elem_ptr, "elem").unwrap();
                Ok(val)
            }
        } else {
            Err(CodegenError::NotImplemented("indexing non-variable expressions".to_string()))
        }
    }

    fn get_struct_name_for_type(&self, ty: &Type) -> Option<String> {
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

    fn get_ref_info(&self, ty: &Type) -> (bool, bool, Option<String>) {
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

    fn compile_print_int_call(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::InvalidArguments("print_int requires an argument".to_string()));
        }

        let printf = self.module.get_function("printf").unwrap();

        // Create format string "%d\n"
        let fmt_str = self.builder.build_global_string_ptr("%d\n", "int_fmt").unwrap();

        // Compile the argument
        let arg = self.compile_expr(&args[0])?;

        // Call printf with format and value
        let call_site = self
            .builder
            .build_call(printf, &[fmt_str.as_pointer_value().into(), arg.into()], "printf_call")
            .unwrap();

        match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(val) => Ok(val),
            inkwell::values::ValueKind::Instruction(_) => {
                Ok(self.context.i32_type().const_zero().into())
            }
        }
    }

    fn llvm_type(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, CodegenError> {
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
                // Generate mangled name if generics present
                let lookup_name = if generics.is_empty() {
                    name.clone()
                } else {
                    self.mangle_name(name, generics)
                };

                // Check if it's an enum type
                if let Some(enum_info) = self.enum_types.get(&lookup_name) {
                    return Ok(enum_info.llvm_type.as_basic_type_enum());
                }
                // Look up the struct type
                let struct_info = self
                    .struct_types
                    .get(&lookup_name)
                    .ok_or_else(|| CodegenError::UndefinedType(lookup_name.clone()))?;
                Ok(struct_info.llvm_type.as_basic_type_enum())
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
            _ => Err(CodegenError::NotImplemented(format!("type {:?}", ty))),
        }
    }

    fn create_entry_block_alloca(&self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        let fn_value = self.current_function.unwrap();
        let entry = fn_value.get_first_basic_block().unwrap();

        let builder = self.context.create_builder();
        match entry.get_first_instruction() {
            Some(inst) => builder.position_before(&inst),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name).unwrap()
    }

    /// Generate a mangled name for monomorphized types/functions
    /// e.g., "Pair" + [i32] => "Pair_i32"
    fn mangle_name(&self, name: &str, type_args: &[Type]) -> String {
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
    fn type_name(&self, ty: &Type) -> String {
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

    pub fn print_ir(&self) {
        println!("{}", self.module.print_to_string().to_string());
    }

    pub fn write_ir_to_file(&self, path: &str) -> Result<(), CodegenError> {
        self.module
            .print_to_file(path)
            .map_err(|e| CodegenError::IoError(e.to_string()))
    }

    pub fn write_object_file(&self, path: &str) -> Result<(), CodegenError> {
        use inkwell::targets::{
            CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
        };
        use inkwell::OptimizationLevel;

        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| CodegenError::TargetError(e.to_string()))?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| CodegenError::TargetError(e.to_string()))?;

        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();

        let target_machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                OptimizationLevel::Default,
                RelocMode::PIC,  // Use PIC for PIE compatibility
                CodeModel::Default,
            )
            .ok_or_else(|| CodegenError::TargetError("failed to create target machine".to_string()))?;

        target_machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
            .map_err(|e| CodegenError::IoError(e.to_string()))
    }
}

#[derive(Debug)]
pub enum CodegenError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    UndefinedType(String),
    UndefinedField(String),
    InvalidAssignment,
    InvalidArguments(String),
    BorrowError(String),
    NotImplemented(String),
    IoError(String),
    TargetError(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
            CodegenError::UndefinedFunction(name) => write!(f, "undefined function: {}", name),
            CodegenError::UndefinedType(name) => write!(f, "undefined type: {}", name),
            CodegenError::UndefinedField(name) => write!(f, "undefined field: {}", name),
            CodegenError::InvalidAssignment => write!(f, "invalid assignment target"),
            CodegenError::InvalidArguments(msg) => write!(f, "invalid arguments: {}", msg),
            CodegenError::BorrowError(msg) => write!(f, "borrow error: {}", msg),
            CodegenError::NotImplemented(what) => write!(f, "not implemented: {}", what),
            CodegenError::IoError(msg) => write!(f, "IO error: {}", msg),
            CodegenError::TargetError(msg) => write!(f, "target error: {}", msg),
        }
    }
}
