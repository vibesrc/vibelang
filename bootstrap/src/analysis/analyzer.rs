//! Semantic analyzer for Vibelang
//!
//! Performs a single pass over the AST to:
//! - Build the symbol table (functions, structs, enums, variables, methods)
//! - Track ownership (moves) and borrows per scope
//! - Validate types and borrow rules
//! - Collect all semantic errors
//!
//! Both the LSP and codegen use this analyzer.

use std::collections::{HashMap, HashSet};
use crate::ast::*;
use crate::lexer::Span;
use super::borrow::{BorrowChecker, BorrowState};
use super::ownership::{OwnershipTracker, is_copy_type};
use super::symbols::*;
use super::errors::{SemanticError, BorrowKind};
use super::types::{is_builtin_type, is_builtin_function, is_prelude_type};

/// Result of semantic analysis
pub struct AnalysisResult {
    /// The populated symbol table
    pub symbols: SymbolTable,
    /// All semantic errors found
    pub errors: Vec<SemanticError>,
}

/// Semantic analyzer that walks the AST and validates semantic rules
pub struct SemanticAnalyzer {
    /// The symbol table being built
    symbols: SymbolTable,
    /// Collected errors
    errors: Vec<SemanticError>,
    /// Ownership tracker for the current function
    ownership: OwnershipTracker,
    /// Borrow checker for the current function
    borrows: BorrowChecker,
    /// Current function's generic type parameters
    current_type_params: Vec<String>,
    /// Type names imported via `use` statements (treated as valid)
    imported_types: HashSet<String>,
}

impl SemanticAnalyzer {
    /// Create a new semantic analyzer
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            errors: Vec::new(),
            ownership: OwnershipTracker::new(),
            borrows: BorrowChecker::new(),
            current_type_params: Vec::new(),
            imported_types: HashSet::new(),
        }
    }

    /// Analyze a program and return the result
    pub fn analyze(mut self, program: &Program) -> AnalysisResult {
        // Inject prelude symbols so type inference works for Vec, String, etc.
        self.inject_prelude_symbols();

        // First pass: extract all symbols (types, functions, etc.)
        self.extract_symbols(program);

        // Second pass: analyze semantics (types, borrows, moves)
        self.analyze_semantics(program);

        AnalysisResult {
            symbols: self.symbols,
            errors: self.errors,
        }
    }

    /// Analyze a program, using an existing symbol table (for incremental analysis)
    pub fn analyze_with_symbols(mut self, program: &Program, symbols: SymbolTable) -> AnalysisResult {
        self.symbols = symbols;
        self.analyze_semantics(program);
        AnalysisResult {
            symbols: self.symbols,
            errors: self.errors,
        }
    }

    // ========================================================================
    // Symbol Extraction (First Pass)
    // ========================================================================

    fn extract_symbols(&mut self, program: &Program) {
        // Sub-pass 1: Extract all type and function signatures first
        // This allows variable type inference to look up any function/method
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.extract_function_signature(func);
                }
                Item::Struct(s) => {
                    self.extract_struct(s);
                }
                Item::Enum(e) => {
                    self.extract_enum(e);
                }
                Item::Impl(impl_block) => {
                    self.extract_impl(impl_block);
                }
                Item::Use(use_stmt) => {
                    self.check_duplicate_imports(use_stmt);
                }
                _ => {}
            }
        }

        // Sub-pass 2: Extract variables from function bodies
        // Now all functions and types are known, so type inference works
        for item in &program.items {
            if let Item::Function(func) = item {
                self.extract_function_variables(func);
            }
        }
    }

    /// Extract function signature only (for first sub-pass)
    fn extract_function_signature(&mut self, func: &Function) {
        let params: Vec<(String, String)> = func
            .params
            .iter()
            .map(|p| (p.name.clone(), self.type_to_string(&p.ty)))
            .collect();

        self.symbols.functions.insert(
            func.name.clone(),
            FunctionInfo {
                name: func.name.clone(),
                params,
                return_type: func.return_type.as_ref().map(|t| self.type_to_string(t)),
                generics: func.generics.clone(),
                span: func.span,
                is_pub: func.is_pub,
            },
        );
    }

    /// Extract variables from function body (for second sub-pass)
    fn extract_function_variables(&mut self, func: &Function) {
        // Extract variables from function body
        self.extract_variables_from_block(&func.body, func.span.start, func.span.end);

        // Add function parameters as variables
        for param in &func.params {
            let borrow_state = match &param.ty {
                Type::Ref(_) => BorrowState::Borrowed,
                Type::RefMut(_) => BorrowState::MutBorrowed,
                _ => BorrowState::Owned,
            };
            self.symbols.variables.push(VariableInfo {
                name: param.name.clone(),
                ty: Some(self.type_to_string(&param.ty)),
                span: param.span,
                scope_start: func.span.start,
                scope_end: func.span.end,
                borrow_state,
            });
        }
    }

    fn extract_struct(&mut self, s: &Struct) {
        let fields: Vec<(String, String, bool)> = s
            .fields
            .iter()
            .map(|f| (f.name.clone(), self.type_to_string(&f.ty), f.is_pub))
            .collect();

        self.symbols.structs.insert(
            s.name.clone(),
            StructInfo {
                name: s.name.clone(),
                fields,
                generics: s.generics.clone(),
                span: s.span,
                is_pub: s.is_pub,
            },
        );
    }

    fn extract_enum(&mut self, e: &Enum) {
        let variants: Vec<VariantData> = e
            .variants
            .iter()
            .map(|v| VariantData {
                name: v.name.clone(),
                fields: match &v.fields {
                    VariantFields::Unit => VariantFieldsData::Unit,
                    VariantFields::Tuple(types) => VariantFieldsData::Tuple(
                        types.iter().map(|t| self.type_to_string(t)).collect(),
                    ),
                    VariantFields::Struct(fields) => VariantFieldsData::Struct(
                        fields
                            .iter()
                            .map(|f| (f.name.clone(), self.type_to_string(&f.ty)))
                            .collect(),
                    ),
                },
                span: v.span,
            })
            .collect();

        self.symbols.enums.insert(
            e.name.clone(),
            EnumInfo {
                name: e.name.clone(),
                variants,
                generics: e.generics.clone(),
                span: e.span,
                is_pub: e.is_pub,
            },
        );
    }

    fn extract_impl(&mut self, impl_block: &Impl) {
        let type_name = self.type_to_string(&impl_block.target);
        let methods: Vec<MethodInfo> = impl_block
            .methods
            .iter()
            .map(|m| MethodInfo {
                name: m.name.clone(),
                params: m
                    .params
                    .iter()
                    .map(|p| (p.name.clone(), self.type_to_string(&p.ty)))
                    .collect(),
                return_type: m.return_type.as_ref().map(|t| self.type_to_string(t)),
                span: m.span,
            })
            .collect();

        self.symbols.methods.entry(type_name).or_default().extend(methods);
    }

    fn extract_variables_from_block(&mut self, block: &Block, scope_start: usize, scope_end: usize) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let { name, ty, value, span, .. } => {
                    let inferred_ty = if let Some(t) = ty {
                        Some(self.type_to_string(t))
                    } else {
                        self.infer_type_from_expr(value)
                    };

                    self.symbols.variables.push(VariableInfo {
                        name: name.clone(),
                        ty: inferred_ty,
                        span: *span,
                        scope_start,
                        scope_end,
                        borrow_state: BorrowState::Owned,
                    });
                }
                Stmt::If { then_block, else_block, span, .. } => {
                    self.extract_variables_from_block(then_block, span.start, span.end);
                    if let Some(else_b) = else_block {
                        self.extract_variables_from_block(else_b, span.start, span.end);
                    }
                }
                Stmt::While { body, span, .. } => {
                    self.extract_variables_from_block(body, span.start, span.end);
                }
                Stmt::For { name, body, span, .. } => {
                    self.symbols.variables.push(VariableInfo {
                        name: name.clone(),
                        ty: None,
                        span: *span,
                        scope_start: span.start,
                        scope_end: span.end,
                        borrow_state: BorrowState::Owned,
                    });
                    self.extract_variables_from_block(body, span.start, span.end);
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        self.extract_variables_from_pattern(&arm.pattern, arm.span.start, arm.span.end);
                        self.extract_variables_from_expr(&arm.body, arm.span.start, arm.span.end);
                    }
                }
                Stmt::Expr(expr) => {
                    self.extract_variables_from_expr(expr, scope_start, scope_end);
                }
                _ => {}
            }
        }
    }

    fn extract_variables_from_pattern(&mut self, pattern: &Pattern, scope_start: usize, scope_end: usize) {
        match pattern {
            Pattern::Ident(name) if name != "_" => {
                self.symbols.variables.push(VariableInfo {
                    name: name.clone(),
                    ty: None,
                    span: Span { start: scope_start, end: scope_end, line: 0, column: 0 },
                    scope_start,
                    scope_end,
                    borrow_state: BorrowState::Owned,
                });
            }
            Pattern::Enum { fields, .. } => {
                for field in fields {
                    self.extract_variables_from_pattern(field, scope_start, scope_end);
                }
            }
            Pattern::Struct { fields, .. } => {
                for (_, field_pattern) in fields {
                    self.extract_variables_from_pattern(field_pattern, scope_start, scope_end);
                }
            }
            _ => {}
        }
    }

    fn extract_variables_from_expr(&mut self, expr: &Expr, scope_start: usize, scope_end: usize) {
        match expr {
            Expr::Block(block) => {
                self.extract_variables_from_block(block, scope_start, scope_end);
            }
            Expr::If { then_expr, else_expr, span, .. } => {
                self.extract_variables_from_expr(then_expr, span.start, span.end);
                self.extract_variables_from_expr(else_expr, span.start, span.end);
            }
            Expr::Closure { params, body, span, .. } => {
                for (param_name, param_ty) in params {
                    let ty_str = param_ty.as_ref().map(|t| self.type_to_string(t));
                    self.symbols.variables.push(VariableInfo {
                        name: param_name.clone(),
                        ty: ty_str,
                        span: *span,
                        scope_start: span.start,
                        scope_end: span.end,
                        borrow_state: BorrowState::Owned,
                    });
                }
                match body {
                    ClosureBody::Expr(expr) => {
                        self.extract_variables_from_expr(expr, span.start, span.end);
                    }
                    ClosureBody::Block(block) => {
                        self.extract_variables_from_block(block, span.start, span.end);
                    }
                }
            }
            _ => {}
        }
    }

    // ========================================================================
    // Semantic Analysis (Second Pass)
    // ========================================================================

    fn analyze_semantics(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Function(func) = item {
                self.analyze_function(func);
            }
        }
    }

    fn analyze_function(&mut self, func: &Function) {
        // Reset state for new function
        self.ownership.clear();
        self.borrows.release_borrows();
        self.current_type_params = func.generics.clone();

        // Check parameter types
        for param in &func.params {
            self.check_type(&param.ty, &param.span);
        }

        // Check return type
        if let Some(ref ret_ty) = func.return_type {
            self.check_type(ret_ty, &func.span);
        }

        // Analyze function body
        self.analyze_block(&func.body);
    }

    fn analyze_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { name, ty, value, span, .. } => {
                // Check declared type
                if let Some(declared_ty) = ty {
                    self.check_type(declared_ty, span);

                    // Check type mismatch
                    if let Some(expr_ty) = self.infer_type_from_expr(value) {
                        let declared_str = self.type_to_string(declared_ty);
                        if !self.types_compatible(&declared_str, &expr_ty) {
                            self.errors.push(SemanticError::TypeMismatch {
                                expected: declared_str,
                                found: expr_ty,
                                span: *span,
                            });
                        }
                    }
                }

                // Analyze the value expression (may move values)
                self.analyze_expr(value);

                // Track move if assigning from another variable
                if let Expr::Ident(src_name, src_span) = value {
                    if let Some(var_ty) = self.get_variable_type(src_name) {
                        if !is_copy_type(&var_ty) {
                            self.ownership.mark_moved(src_name, *src_span);
                        }
                    }
                }

                // Reassignment restores ownership
                self.ownership.restore(name);
            }

            Stmt::Expr(expr) => {
                // Save borrow state, analyze, restore (expression borrows are temporary)
                let saved_borrows = self.borrows.snapshot();
                self.analyze_expr(expr);
                self.borrows.restore(saved_borrows);
            }

            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.analyze_expr(v);
                }
            }

            Stmt::If { condition, then_block, else_block, .. } => {
                self.analyze_expr(condition);

                // Analyze branches with ownership tracking
                let ownership_before = self.ownership.snapshot();
                let borrows_before = self.borrows.snapshot();

                self.analyze_block(then_block);
                let ownership_after_then = self.ownership.snapshot();

                if let Some(else_b) = else_block {
                    self.ownership.restore_from(ownership_before.clone());
                    self.borrows.restore(borrows_before.clone());
                    self.analyze_block(else_b);

                    // Merge: variable is moved if moved in either branch
                    self.ownership.merge(&ownership_after_then);
                }
            }

            Stmt::While { condition, body, .. } => {
                self.analyze_expr(condition);
                self.analyze_block(body);
            }

            Stmt::For { iter, body, .. } => {
                self.analyze_expr(iter);
                self.analyze_block(body);
            }

            Stmt::Match { value, arms, .. } => {
                self.analyze_expr(value);
                for arm in arms {
                    self.analyze_expr(&arm.body);
                }
            }

            Stmt::Defer { expr, .. } => {
                self.analyze_expr(expr);
            }

            _ => {}
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(name, span) => {
                // Check use-after-move
                if let Err(e) = self.ownership.check_use(name, *span) {
                    self.errors.push(e);
                    return;
                }

                // Check variable exists
                let var_exists = self.symbols.variables.iter().any(|v| &v.name == name)
                    || self.symbols.functions.contains_key(name);

                if !var_exists && name != "self" && !is_builtin_function(name) {
                    // Allow struct/enum names for static method calls (e.g., Vec.new())
                    // Also allow prelude types (Vec, String, Option, etc.)
                    // Also allow imported items from `use` statements
                    let is_type_name = self.symbols.structs.contains_key(name)
                        || self.symbols.enums.contains_key(name)
                        || is_prelude_type(name)
                        || self.imported_types.contains(name)
                        || self.imported_types.contains("*"); // Glob import
                    if !is_type_name {
                        self.errors.push(SemanticError::UndefinedVariable {
                            name: name.clone(),
                            span: *span,
                        });
                    }
                }
            }

            Expr::Binary { left, right, op, span } => {
                self.analyze_expr(left);
                self.analyze_expr(right);

                let is_assignment = matches!(op,
                    BinOp::Assign | BinOp::AddAssign | BinOp::SubAssign |
                    BinOp::MulAssign | BinOp::DivAssign | BinOp::ModAssign |
                    BinOp::BitAndAssign | BinOp::BitOrAssign | BinOp::BitXorAssign |
                    BinOp::ShlAssign | BinOp::ShrAssign
                );

                if is_assignment {
                    // Reassignment restores ownership
                    if let Expr::Ident(name, _) = left.as_ref() {
                        self.ownership.restore(name);
                    }

                    // Check mutation through shared reference
                    if let Expr::Field { object, .. } = left.as_ref() {
                        if let Expr::Ident(var_name, _) = object.as_ref() {
                            if let Some(var) = self.symbols.variables.iter().find(|v| &v.name == var_name) {
                                if var.borrow_state == BorrowState::Borrowed {
                                    self.errors.push(SemanticError::MutationThroughSharedRef {
                                        name: var_name.clone(),
                                        span: *span,
                                    });
                                }
                            }
                        }
                    }
                }
            }

            Expr::Unary { operand, .. } => {
                self.analyze_expr(operand);
            }

            Expr::Call { func, args, span, .. } => {
                self.analyze_expr(func);

                // Get function info for validation
                let func_info = if let Expr::Ident(name, _) = func.as_ref() {
                    self.symbols.functions.get(name).cloned()
                } else {
                    None
                };

                // Check argument count
                if let Some(ref info) = func_info {
                    if args.len() != info.params.len() {
                        self.errors.push(SemanticError::ArgumentCountMismatch {
                            function: info.name.clone(),
                            expected: info.params.len(),
                            found: args.len(),
                            span: *span,
                        });
                    }
                }

                // Analyze arguments and check borrow/move matching
                for (i, arg) in args.iter().enumerate() {
                    self.analyze_expr(arg);

                    // Check borrow type matching at call site
                    if let Some(ref info) = func_info {
                        if let Some((_, param_ty)) = info.params.get(i) {
                            self.check_call_site_borrow_match(arg, param_ty, span);
                        }
                    }

                    // Track moves for non-copy arguments
                    if let Expr::Ident(name, arg_span) = arg {
                        if let Some(var_ty) = self.get_variable_type(name) {
                            if !is_copy_type(&var_ty) && !self.is_reference_arg(arg) {
                                self.ownership.mark_moved(name, *arg_span);
                            }
                        }
                    }
                }
            }

            Expr::MethodCall { receiver, method, args, span } => {
                self.analyze_expr(receiver);
                for arg in args {
                    self.analyze_expr(arg);
                }

                // Type check method arguments against generic type parameters
                if let Some(receiver_type) = self.infer_type_from_expr(receiver) {
                    let (base_type, type_args) = self.parse_generic_type(&receiver_type);

                    // Look up method info
                    if let Some(methods) = self.symbols.methods.get(&base_type).cloned() {
                        if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                            // Get the struct's generic parameters (e.g., ["K", "V"] for Map)
                            let type_params = self.symbols.structs.get(&base_type)
                                .map(|s| s.generics.clone())
                                .unwrap_or_default();

                            // Check each argument (skip self parameter)
                            let params_without_self: Vec<_> = method_info.params.iter()
                                .filter(|(name, _)| name != "self")
                                .collect();

                            for (i, arg) in args.iter().enumerate() {
                                if let Some((param_name, param_type)) = params_without_self.get(i) {
                                    // Substitute type parameters with concrete types
                                    let expected_type = self.substitute_type_params(
                                        param_type,
                                        &type_params,
                                        &type_args
                                    );

                                    // Infer the argument's type
                                    if let Some(arg_type) = self.infer_type_from_expr(arg) {
                                        if !self.types_compatible(&expected_type, &arg_type) {
                                            self.errors.push(SemanticError::TypeMismatch {
                                                expected: expected_type,
                                                found: arg_type,
                                                span: *span,
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Expr::Field { object, .. } => {
                self.analyze_expr(object);

                // Check use-after-move for field access
                if let Expr::Ident(name, span) = object.as_ref() {
                    if let Err(e) = self.ownership.check_use(name, *span) {
                        self.errors.push(e);
                    }
                }
            }

            Expr::Index { array, index, .. } => {
                self.analyze_expr(array);
                self.analyze_expr(index);
            }

            Expr::Ref { operand, span } => {
                if let Expr::Ident(name, _) = operand.as_ref() {
                    // Check borrow of moved value
                    if let Some(move_span) = self.ownership.is_moved(name) {
                        self.errors.push(SemanticError::BorrowOfMovedValue {
                            name: name.clone(),
                            borrow_span: *span,
                            move_span,
                        });
                        return;
                    }

                    // Check borrow conflicts
                    if let Err(e) = self.borrows.try_borrow(name, false, *span) {
                        self.errors.push(e);
                    }
                }
                self.analyze_expr(operand);
            }

            Expr::RefMut { operand, span } => {
                if let Expr::Ident(name, _) = operand.as_ref() {
                    // Check borrow of moved value
                    if let Some(move_span) = self.ownership.is_moved(name) {
                        self.errors.push(SemanticError::BorrowOfMovedValue {
                            name: name.clone(),
                            borrow_span: *span,
                            move_span,
                        });
                        return;
                    }

                    // Check borrow conflicts
                    if let Err(e) = self.borrows.try_borrow(name, true, *span) {
                        self.errors.push(e);
                    }
                }
                self.analyze_expr(operand);
            }

            Expr::Deref { operand, .. } => {
                self.analyze_expr(operand);
            }

            Expr::StructInit { name, generics, fields, span, .. } => {
                // Check struct exists (including imported types)
                let base_name = name.split('<').next().unwrap_or(name);
                let is_valid = self.symbols.structs.contains_key(base_name)
                    || self.symbols.enums.contains_key(base_name)
                    || is_prelude_type(base_name)
                    || self.imported_types.contains(base_name)
                    || self.imported_types.contains("*"); // Glob import

                if !is_valid {
                    self.errors.push(SemanticError::UndefinedType {
                        name: name.clone(),
                        span: *span,
                    });
                } else if let Some(struct_info) = self.symbols.structs.get(base_name).cloned() {
                    // Skip field checks if no fields provided AND it has generics
                    // This allows Vec<u8>.new() syntax (type with generics used for static method call)
                    if !fields.is_empty() || generics.is_empty() {
                        // Check fields
                        let provided_fields: Vec<_> = fields.iter().map(|(n, _)| n.as_str()).collect();
                        for (field_name, _, _) in &struct_info.fields {
                            if !provided_fields.contains(&field_name.as_str()) {
                                self.errors.push(SemanticError::MissingField {
                                    struct_name: name.clone(),
                                    field_name: field_name.clone(),
                                    span: *span,
                                });
                            }
                        }
                        let known_fields: Vec<_> = struct_info.fields.iter().map(|(n, _, _)| n.as_str()).collect();
                        for (field_name, _) in fields {
                            if !known_fields.contains(&field_name.as_str()) {
                                self.errors.push(SemanticError::UnknownField {
                                    struct_name: name.clone(),
                                    field_name: field_name.clone(),
                                    span: *span,
                                });
                            }
                        }
                    }
                }

                // Analyze field expressions
                for (_, expr) in fields {
                    self.analyze_expr(expr);
                }
            }

            Expr::ArrayInit { elements, .. } => {
                for elem in elements {
                    self.analyze_expr(elem);
                }
            }

            Expr::Block(block) => {
                self.analyze_block(block);
            }

            Expr::Try { operand, .. } => {
                self.analyze_expr(operand);
            }

            Expr::Range { start, end, .. } => {
                self.analyze_expr(start);
                self.analyze_expr(end);
            }

            Expr::Closure { body, .. } => {
                match body {
                    ClosureBody::Expr(expr) => self.analyze_expr(expr),
                    ClosureBody::Block(block) => self.analyze_block(block),
                }
            }

            Expr::If { condition, then_expr, else_expr, .. } => {
                self.analyze_expr(condition);
                self.analyze_expr(then_expr);
                self.analyze_expr(else_expr);
            }

            _ => {}
        }
    }

    // ========================================================================
    // Helper Methods
    // ========================================================================

    fn check_type(&mut self, ty: &Type, span: &Span) {
        match ty {
            Type::Named { name, generics } => {
                // Check if type is valid: builtin, prelude, type param, user-defined, or imported
                let is_valid = self.current_type_params.contains(name)
                    || self.symbols.structs.contains_key(name)
                    || self.symbols.enums.contains_key(name)
                    || is_builtin_type(name)
                    || is_prelude_type(name)
                    || self.imported_types.contains(name)
                    || self.imported_types.contains("*"); // Glob import - be lenient

                if !is_valid {
                    self.errors.push(SemanticError::UndefinedType {
                        name: name.clone(),
                        span: *span,
                    });
                }
                for g in generics {
                    self.check_type(g, span);
                }
            }
            Type::Ref(inner)
            | Type::RefMut(inner)
            | Type::Pointer(inner)
            | Type::Array(inner, _)
            | Type::Slice(inner) => {
                self.check_type(inner, span);
            }
            _ => {}
        }
    }

    fn check_duplicate_imports(&mut self, use_stmt: &Use) {
        match &use_stmt.items {
            ImportItems::Named(items) => {
                let mut seen: HashMap<String, Span> = HashMap::new();
                for item in items {
                    let name = item.alias.as_ref().unwrap_or(&item.name);
                    if seen.contains_key(name) {
                        self.errors.push(SemanticError::DuplicateImport {
                            name: name.clone(),
                            span: item.span,
                        });
                    } else {
                        seen.insert(name.clone(), item.span);
                    }
                    // Track this as an imported type (could be struct, enum, or function)
                    self.imported_types.insert(name.clone());
                }
            }
            ImportItems::Glob => {
                // Glob imports - we can't know the names statically without loading the module
                // Mark that we have glob imports so we're lenient with unknown types
                // Use a sentinel value to indicate glob import presence
                self.imported_types.insert("*".to_string());
            }
            ImportItems::Module => {
                // Module import (e.g., `use std.fs` for `fs.File`)
                // The module name becomes available as a namespace prefix
                if let Some(module_name) = use_stmt.path.last() {
                    self.imported_types.insert(module_name.clone());
                }
            }
        }
    }

    /// Check that borrow operators at call site match parameter expectations
    fn check_call_site_borrow_match(&mut self, arg: &Expr, param_ty: &str, call_span: &Span) {
        let param_is_mut_ref = param_ty.starts_with('~');
        let param_is_ref = param_ty.starts_with('&');

        match arg {
            Expr::RefMut { span, .. } => {
                if !param_is_mut_ref {
                    self.errors.push(SemanticError::BorrowConflict {
                        name: "argument".to_string(),
                        existing: BorrowKind::Mutable,
                        requested: if param_is_ref { BorrowKind::Shared } else { BorrowKind::Shared },
                        existing_span: *span,
                        request_span: *call_span,
                    });
                }
            }
            Expr::Ref { span, .. } => {
                if !param_is_ref {
                    self.errors.push(SemanticError::BorrowConflict {
                        name: "argument".to_string(),
                        existing: BorrowKind::Shared,
                        requested: if param_is_mut_ref { BorrowKind::Mutable } else { BorrowKind::Shared },
                        existing_span: *span,
                        request_span: *call_span,
                    });
                }
            }
            _ => {
                // Plain argument passed to reference parameter
                if param_is_ref || param_is_mut_ref {
                    // This is allowed - auto-borrow at call site
                }
            }
        }
    }

    fn is_reference_arg(&self, arg: &Expr) -> bool {
        matches!(arg, Expr::Ref { .. } | Expr::RefMut { .. })
    }

    fn get_variable_type(&self, name: &str) -> Option<String> {
        self.symbols.variables.iter()
            .find(|v| v.name == name)
            .and_then(|v| v.ty.clone())
    }

    fn types_compatible(&self, expected: &str, actual: &str) -> bool {
        if expected == actual {
            return true;
        }

        // Integer literal coercion
        let integer_types = ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"];
        if integer_types.contains(&expected) && integer_types.contains(&actual) {
            return true;
        }

        // Check base type matches for generics
        let expected_base = expected.split('<').next().unwrap_or(expected);
        let actual_base = actual.split('<').next().unwrap_or(actual);

        expected_base == actual_base
    }

    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    name.clone()
                } else {
                    let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                    format!("{}<{}>", name, generic_strs.join(", "))
                }
            }
            Type::Ref(inner) => format!("&{}", self.type_to_string(inner)),
            Type::RefMut(inner) => format!("~{}", self.type_to_string(inner)),
            Type::Pointer(inner) => format!("*{}", self.type_to_string(inner)),
            Type::Array(inner, size) => format!("{}[{}]", self.type_to_string(inner), size),
            Type::Slice(inner) => format!("Slice<{}>", self.type_to_string(inner)),
            Type::Tuple(types) => {
                let type_strs: Vec<_> = types.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})", type_strs.join(", "))
            }
            Type::Fn(params, return_type) => {
                let param_strs: Vec<_> = params.iter().map(|t| self.type_to_string(t)).collect();
                let ret = self.type_to_string(return_type);
                format!("fn({}) -> {}", param_strs.join(", "), ret)
            }
            // Handle primitive types
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
            Type::Char => "char".to_string(),
            Type::Void => "void".to_string(),
            Type::SelfType => "Self".to_string(),
        }
    }

    fn infer_type_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Literal(lit, _) => {
                let ty = match lit {
                    Literal::Int(_, suffix) => match suffix {
                        IntSuffix::I8 => "i8",
                        IntSuffix::I16 => "i16",
                        IntSuffix::I32 => "i32",
                        IntSuffix::I64 => "i64",
                        IntSuffix::U8 => "u8",
                        IntSuffix::U16 => "u16",
                        IntSuffix::U32 => "u32",
                        IntSuffix::U64 => "u64",
                        IntSuffix::None => "i32",
                    },
                    Literal::Float(_) => "f64",
                    Literal::Bool(_) => "bool",
                    Literal::Char(_) => "char",
                    Literal::String(_) => "Slice<u8>",
                };
                Some(ty.to_string())
            }
            Expr::Ident(name, _) => self.get_variable_type(name),
            Expr::Call { func, .. } => {
                // Direct function call: foo()
                if let Expr::Ident(name, _) = func.as_ref() {
                    return self.symbols.functions.get(name).and_then(|f| f.return_type.clone());
                }
                // Static method call on generic type: Vec<u8>.new()
                // Parses as Call { func: Field { object: StructInit, field: method } }
                if let Expr::Field { object, field, .. } = func.as_ref() {
                    if let Expr::StructInit { name, generics, fields, .. } = object.as_ref() {
                        if fields.is_empty() && !generics.is_empty() {
                            // Build the full generic type
                            let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                            let full_type = format!("{}<{}>", name, generic_strs.join(", "));

                            // Look up method return type
                            if let Some(methods) = self.symbols.methods.get(name) {
                                if let Some(method_info) = methods.iter().find(|m| &m.name == field) {
                                    return method_info.return_type.as_ref().map(|ty| {
                                        // Replace Self or Vec<T> with concrete type
                                        if ty == "Self" || ty.starts_with(&format!("{}<", name)) {
                                            full_type.clone()
                                        } else {
                                            ty.clone()
                                        }
                                    });
                                }
                            }
                            // Even without method info, return type is the generic struct
                            return Some(full_type);
                        }
                    }
                }
                None
            }
            Expr::MethodCall { receiver, method, .. } => {
                // Check if receiver is a generic type (e.g., Vec<u8>.new())
                if let Expr::StructInit { name, generics, fields, .. } = receiver.as_ref() {
                    if fields.is_empty() && !generics.is_empty() {
                        // Static method call on a generic type: Vec<u8>.new()
                        // Look up the method return type
                        if let Some(methods) = self.symbols.methods.get(name) {
                            if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                                let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                                let full_type = format!("{}<{}>", name, generic_strs.join(", "));
                                // Replace 'Self', 'Vec<T>', etc. with the concrete type
                                return method_info.return_type.as_ref().map(|ty| {
                                    if ty == "Self" || ty.starts_with(&format!("{}<", name)) {
                                        full_type.clone()
                                    } else {
                                        ty.clone()
                                    }
                                });
                            }
                        }
                        // Even if method not found, we know the type is the generic struct
                        let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                        return Some(format!("{}<{}>", name, generic_strs.join(", ")));
                    }
                }
                // Get receiver type
                if let Expr::Ident(type_name, _) = receiver.as_ref() {
                    // Check if it's a static method call on a type (e.g., String.from())
                    if self.symbols.structs.contains_key(type_name) || self.symbols.enums.contains_key(type_name) || is_prelude_type(type_name) {
                        // Look up the method return type
                        if let Some(methods) = self.symbols.methods.get(type_name) {
                            if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                                // Replace 'Self' with the actual type name
                                return method_info.return_type.as_ref().map(|ty| {
                                    if ty == "Self" { type_name.clone() } else { ty.clone() }
                                });
                            }
                        }
                    }
                }
                // Method call on a variable - get the receiver's type first
                if let Some(receiver_type) = self.infer_type_from_expr(receiver) {
                    let base_type = receiver_type
                        .trim_start_matches('&')
                        .trim_start_matches('~')
                        .trim_start_matches('*')
                        .to_string();
                    // Extract base type name (strip generics)
                    let base_name = base_type.split('<').next().unwrap_or(&base_type);
                    if let Some(methods) = self.symbols.methods.get(base_name) {
                        if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                            return method_info.return_type.as_ref().map(|ty| {
                                if ty == "Self" { base_name.to_string() } else { ty.clone() }
                            });
                        }
                    }
                }
                None
            }
            Expr::Field { object, field, .. } => {
                // Get the object's type and look up the field type
                if let Some(obj_type) = self.infer_type_from_expr(object) {
                    let base_type = obj_type
                        .trim_start_matches('&')
                        .trim_start_matches('~')
                        .trim_start_matches('*')
                        .to_string();
                    let base_name = base_type.split('<').next().unwrap_or(&base_type);
                    if let Some(struct_info) = self.symbols.structs.get(base_name) {
                        for (field_name, field_type, _) in &struct_info.fields {
                            if field_name == field {
                                return Some(field_type.clone());
                            }
                        }
                    }
                }
                None
            }
            Expr::StructInit { name, generics, .. } => {
                if generics.is_empty() {
                    Some(name.clone())
                } else {
                    let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                    Some(format!("{}<{}>", name, generic_strs.join(", ")))
                }
            }
            Expr::Ref { operand, .. } => {
                self.infer_type_from_expr(operand).map(|t| format!("&{}", t))
            }
            Expr::RefMut { operand, .. } => {
                self.infer_type_from_expr(operand).map(|t| format!("~{}", t))
            }
            Expr::Deref { operand, .. } => {
                if let Some(ty) = self.infer_type_from_expr(operand) {
                    // Strip one level of reference/pointer
                    if ty.starts_with('&') || ty.starts_with('~') || ty.starts_with('*') {
                        return Some(ty[1..].to_string());
                    }
                }
                None
            }
            Expr::ArrayInit { elements, .. } => {
                if let Some(first) = elements.first() {
                    if let Some(elem_ty) = self.infer_type_from_expr(first) {
                        return Some(format!("{}[{}]", elem_ty, elements.len()));
                    }
                }
                None
            }
            Expr::Index { array, .. } => {
                // Indexing into array/slice returns element type
                if let Some(obj_type) = self.infer_type_from_expr(array) {
                    // Handle array types like "i32[5]"
                    if let Some(bracket_pos) = obj_type.find('[') {
                        return Some(obj_type[..bracket_pos].to_string());
                    }
                    // Handle Slice<T> -> T
                    if obj_type.starts_with("Slice<") && obj_type.ends_with('>') {
                        return Some(obj_type[6..obj_type.len()-1].to_string());
                    }
                    // Handle Vec<T> -> T
                    if obj_type.starts_with("Vec<") && obj_type.ends_with('>') {
                        return Some(obj_type[4..obj_type.len()-1].to_string());
                    }
                }
                None
            }
            Expr::Closure { params, return_type, .. } => {
                let param_types: Vec<String> = params.iter().map(|(_, ty)| {
                    ty.as_ref().map(|t| self.type_to_string(t)).unwrap_or_else(|| "?".to_string())
                }).collect();
                let ret_type = return_type.as_ref()
                    .map(|t| self.type_to_string(t))
                    .unwrap_or_else(|| "?".to_string());
                Some(format!("({}) => {}", param_types.join(", "), ret_type))
            }
            Expr::Binary { op, left, right, .. } => {
                match op {
                    // Comparison operators return bool
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le |
                    BinOp::Gt | BinOp::Ge | BinOp::And | BinOp::Or => {
                        Some("bool".to_string())
                    }
                    // Arithmetic operators return the type of the operands
                    _ => self.infer_type_from_expr(left).or_else(|| self.infer_type_from_expr(right)),
                }
            }
            Expr::Unary { op, operand, .. } => {
                match op {
                    UnaryOp::Not => Some("bool".to_string()),
                    UnaryOp::Neg | UnaryOp::BitNot => self.infer_type_from_expr(operand),
                }
            }
            Expr::Block(block) => {
                // Return type is the type of the last expression
                if let Some(Stmt::Expr(last_expr)) = block.stmts.last() {
                    return self.infer_type_from_expr(last_expr);
                }
                None
            }
            Expr::If { then_expr, .. } => {
                // Return type is the type of the then branch
                self.infer_type_from_expr(then_expr)
            }
            _ => None,
        }
    }

    /// Inject prelude types and their methods into the symbol table
    /// This allows type inference to work for Vec.new(), String.from(), etc.
    fn inject_prelude_symbols(&mut self) {
        let prelude_span = Span { start: 0, end: 0, line: 0, column: 0 };

        // Vec<T> struct
        if !self.symbols.structs.contains_key("Vec") {
            self.symbols.structs.insert("Vec".to_string(), StructInfo {
                name: "Vec".to_string(),
                fields: vec![
                    ("ptr".to_string(), "*T".to_string(), false),
                    ("len".to_string(), "i64".to_string(), false),
                    ("capacity".to_string(), "i64".to_string(), false),
                ],
                generics: vec!["T".to_string()],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Vec<T> methods
        if !self.symbols.methods.contains_key("Vec") {
            self.symbols.methods.insert("Vec".to_string(), vec![
                MethodInfo {
                    name: "new".to_string(),
                    params: vec![],
                    return_type: Some("Vec<T>".to_string()),
                    span: prelude_span,
                },
                MethodInfo {
                    name: "with_capacity".to_string(),
                    params: vec![("cap".to_string(), "i64".to_string())],
                    return_type: Some("Vec<T>".to_string()),
                    span: prelude_span,
                },
                MethodInfo {
                    name: "len".to_string(),
                    params: vec![("self".to_string(), "&Vec<T>".to_string())],
                    return_type: Some("i64".to_string()),
                    span: prelude_span,
                },
                MethodInfo {
                    name: "push".to_string(),
                    params: vec![
                        ("self".to_string(), "~Vec<T>".to_string()),
                        ("value".to_string(), "T".to_string()),
                    ],
                    return_type: None,
                    span: prelude_span,
                },
                MethodInfo {
                    name: "pop".to_string(),
                    params: vec![("self".to_string(), "~Vec<T>".to_string())],
                    return_type: Some("Option<T>".to_string()),
                    span: prelude_span,
                },
                MethodInfo {
                    name: "get".to_string(),
                    params: vec![
                        ("self".to_string(), "&Vec<T>".to_string()),
                        ("index".to_string(), "i64".to_string()),
                    ],
                    return_type: Some("Option<T>".to_string()),
                    span: prelude_span,
                },
            ]);
        }

        // String struct
        if !self.symbols.structs.contains_key("String") {
            self.symbols.structs.insert("String".to_string(), StructInfo {
                name: "String".to_string(),
                fields: vec![("data".to_string(), "Vec<u8>".to_string(), false)],
                generics: vec![],
                span: prelude_span,
                is_pub: true,
            });
        }

        // String methods
        if !self.symbols.methods.contains_key("String") {
            self.symbols.methods.insert("String".to_string(), vec![
                MethodInfo {
                    name: "new".to_string(),
                    params: vec![],
                    return_type: Some("String".to_string()),
                    span: prelude_span,
                },
                MethodInfo {
                    name: "from".to_string(),
                    params: vec![("s".to_string(), "&Slice<u8>".to_string())],
                    return_type: Some("String".to_string()),
                    span: prelude_span,
                },
                MethodInfo {
                    name: "len".to_string(),
                    params: vec![("self".to_string(), "&String".to_string())],
                    return_type: Some("i64".to_string()),
                    span: prelude_span,
                },
            ]);
        }

        // Option<T> enum
        if !self.symbols.enums.contains_key("Option") {
            self.symbols.enums.insert("Option".to_string(), EnumInfo {
                name: "Option".to_string(),
                variants: vec![
                    VariantData {
                        name: "Some".to_string(),
                        fields: VariantFieldsData::Tuple(vec!["T".to_string()]),
                        span: prelude_span,
                    },
                    VariantData {
                        name: "None".to_string(),
                        fields: VariantFieldsData::Unit,
                        span: prelude_span,
                    },
                ],
                generics: vec!["T".to_string()],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Result<T, E> enum
        if !self.symbols.enums.contains_key("Result") {
            self.symbols.enums.insert("Result".to_string(), EnumInfo {
                name: "Result".to_string(),
                variants: vec![
                    VariantData {
                        name: "Ok".to_string(),
                        fields: VariantFieldsData::Tuple(vec!["T".to_string()]),
                        span: prelude_span,
                    },
                    VariantData {
                        name: "Err".to_string(),
                        fields: VariantFieldsData::Tuple(vec!["E".to_string()]),
                        span: prelude_span,
                    },
                ],
                generics: vec!["T".to_string(), "E".to_string()],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Error struct
        if !self.symbols.structs.contains_key("Error") {
            self.symbols.structs.insert("Error".to_string(), StructInfo {
                name: "Error".to_string(),
                fields: vec![("message".to_string(), "Slice<u8>".to_string(), true)],
                generics: vec![],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Error methods
        if !self.symbols.methods.contains_key("Error") {
            self.symbols.methods.insert("Error".to_string(), vec![
                MethodInfo {
                    name: "new".to_string(),
                    params: vec![("message".to_string(), "Slice<u8>".to_string())],
                    return_type: Some("Error".to_string()),
                    span: prelude_span,
                },
            ]);
        }
    }

    /// Parse a type string like "Map<i64, i64>" into base type and generic args
    fn parse_generic_type(&self, ty: &str) -> (String, Vec<String>) {
        if let Some(open) = ty.find('<') {
            if let Some(close) = ty.rfind('>') {
                let base = ty[..open].to_string();
                let generics_str = &ty[open + 1..close];
                // Simple parsing - split by comma but respect nested generics
                let generics = self.split_generic_args(generics_str);
                return (base, generics);
            }
        }
        (ty.to_string(), Vec::new())
    }

    /// Split generic args string, respecting nested generics
    fn split_generic_args(&self, s: &str) -> Vec<String> {
        let mut result = Vec::new();
        let mut current = String::new();
        let mut depth = 0;
        for c in s.chars() {
            match c {
                '<' => { depth += 1; current.push(c); }
                '>' => { depth -= 1; current.push(c); }
                ',' if depth == 0 => {
                    result.push(current.trim().to_string());
                    current = String::new();
                }
                _ => current.push(c),
            }
        }
        if !current.trim().is_empty() {
            result.push(current.trim().to_string());
        }
        result
    }

    /// Substitute generic type parameters in a type string
    /// e.g., substitute_type_params("K", ["K", "V"], ["i64", "String"]) -> "i64"
    fn substitute_type_params(&self, ty: &str, params: &[String], args: &[String]) -> String {
        // Direct match for type parameters
        for (i, param) in params.iter().enumerate() {
            if ty == param {
                if let Some(arg) = args.get(i) {
                    return arg.clone();
                }
            }
        }
        // Handle generic types like Vec<K>
        let (base, generics) = self.parse_generic_type(ty);
        if !generics.is_empty() {
            let substituted_generics: Vec<_> = generics
                .iter()
                .map(|g| self.substitute_type_params(g, params, args))
                .collect();
            return format!("{}<{}>", base, substituted_generics.join(", "));
        }
        ty.to_string()
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to analyze a program
pub fn analyze(program: &Program) -> AnalysisResult {
    SemanticAnalyzer::new().analyze(program)
}
