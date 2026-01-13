//! Symbol extraction and semantic analysis for the Vibelang LSP

use std::collections::HashMap;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity};
use crate::ast::{Block, BinOp, Expr, Item, Program, Stmt, Type, VariantFields, Pattern as AstPattern};
use crate::lexer::Span;

use crate::lsp::types::{
    BorrowState, EnumInfo, FunctionInfo, MethodInfo, StructInfo, SymbolTable, VariableInfo,
    VariantData, VariantFieldsData,
};
use crate::lsp::utils::{is_builtin_function, is_builtin_type, is_prelude_type};
use crate::lsp::Backend;

impl Backend {
    pub fn extract_symbols(&self, program: &Program, symbols: &mut SymbolTable) {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    let params: Vec<(String, String)> = func
                        .params
                        .iter()
                        .map(|p| (p.name.clone(), self.type_to_string(&p.ty)))
                        .collect();

                    symbols.functions.insert(
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

                    self.extract_variables_from_block(
                        &func.body,
                        symbols,
                        func.span.start,
                        func.span.end,
                    );

                    // Add function parameters as variables
                    for param in &func.params {
                        let borrow_state = match &param.ty {
                            Type::Ref(_) => BorrowState::Borrowed,
                            Type::RefMut(_) => BorrowState::MutBorrowed,
                            _ => BorrowState::Owned,
                        };
                        symbols.variables.push(VariableInfo {
                            name: param.name.clone(),
                            ty: Some(self.type_to_string(&param.ty)),
                            span: param.span,
                            scope_start: func.span.start,
                            scope_end: func.span.end,
                            borrow_state,
                        });
                    }
                }
                Item::Struct(s) => {
                    let fields: Vec<(String, String, bool)> = s
                        .fields
                        .iter()
                        .map(|f| (f.name.clone(), self.type_to_string(&f.ty), f.is_pub))
                        .collect();

                    symbols.structs.insert(
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
                Item::Enum(e) => {
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

                    symbols.enums.insert(
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
                Item::Impl(impl_block) => {
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

                    symbols.methods.entry(type_name).or_default().extend(methods);
                }
                _ => {}
            }
        }
    }

    pub fn extract_variables_from_block(
        &self,
        block: &Block,
        symbols: &mut SymbolTable,
        scope_start: usize,
        scope_end: usize,
    ) {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Let {
                    name, ty, value, span, ..
                } => {
                    let inferred_ty = if let Some(t) = ty {
                        Some(self.type_to_string(t))
                    } else {
                        self.infer_type_from_expr(value)
                    };

                    symbols.variables.push(VariableInfo {
                        name: name.clone(),
                        ty: inferred_ty,
                        span: *span,
                        scope_start,
                        scope_end,
                        borrow_state: BorrowState::Owned,
                    });
                }
                Stmt::If {
                    then_block,
                    else_block,
                    span,
                    ..
                } => {
                    self.extract_variables_from_block(then_block, symbols, span.start, span.end);
                    if let Some(else_b) = else_block {
                        self.extract_variables_from_block(else_b, symbols, span.start, span.end);
                    }
                }
                Stmt::While { body, span, .. } => {
                    self.extract_variables_from_block(body, symbols, span.start, span.end);
                }
                Stmt::For {
                    name, body, span, ..
                } => {
                    symbols.variables.push(VariableInfo {
                        name: name.clone(),
                        ty: None,
                        span: *span,
                        scope_start: span.start,
                        scope_end: span.end,
                        borrow_state: BorrowState::Owned,
                    });
                    self.extract_variables_from_block(body, symbols, span.start, span.end);
                }
                Stmt::Match { arms, span, .. } => {
                    for arm in arms {
                        self.extract_variables_from_pattern(
                            &arm.pattern,
                            symbols,
                            span.start,
                            span.end,
                        );
                    }
                }
                _ => {}
            }
        }
    }

    pub fn extract_variables_from_pattern(
        &self,
        pattern: &AstPattern,
        symbols: &mut SymbolTable,
        scope_start: usize,
        scope_end: usize,
    ) {
        match pattern {
            AstPattern::Ident(name) if name != "_" => {
                symbols.variables.push(VariableInfo {
                    name: name.clone(),
                    ty: None,
                    span: Span {
                        start: scope_start,
                        end: scope_end,
                        line: 0,
                        column: 0,
                    },
                    scope_start,
                    scope_end,
                    borrow_state: BorrowState::Owned,
                });
            }
            AstPattern::Enum { fields, .. } => {
                for field in fields {
                    self.extract_variables_from_pattern(field, symbols, scope_start, scope_end);
                }
            }
            AstPattern::Struct { fields, .. } => {
                for (_, field_pattern) in fields {
                    self.extract_variables_from_pattern(field_pattern, symbols, scope_start, scope_end);
                }
            }
            _ => {}
        }
    }

    pub fn analyze_semantics(
        &self,
        program: &Program,
        symbols: &SymbolTable,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let mut moved_vars: HashMap<String, Span> = HashMap::new();
        let mut borrowed_vars: HashMap<String, (BorrowState, Span)> = HashMap::new();

        for item in &program.items {
            if let Item::Function(func) = item {
                moved_vars.clear();
                borrowed_vars.clear();

                let type_params = &func.generics;

                for param in &func.params {
                    self.check_type(&param.ty, symbols, type_params, diagnostics, &param.span);
                }

                if let Some(ref ret_ty) = func.return_type {
                    self.check_type(ret_ty, symbols, type_params, diagnostics, &func.span);
                }

                self.analyze_block(
                    &func.body,
                    symbols,
                    type_params,
                    diagnostics,
                    &mut moved_vars,
                    &mut borrowed_vars,
                );
            }
        }
    }

    pub fn check_type(
        &self,
        ty: &Type,
        symbols: &SymbolTable,
        type_params: &[String],
        diagnostics: &mut Vec<Diagnostic>,
        span: &Span,
    ) {
        match ty {
            Type::Named { name, generics } => {
                if !type_params.contains(name)
                    && !symbols.structs.contains_key(name)
                    && !symbols.enums.contains_key(name)
                    && !is_builtin_type(name)
                    && !is_prelude_type(name)
                {
                    diagnostics.push(Diagnostic {
                        range: self.span_to_range(span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("vibelang".to_string()),
                        message: format!("Unknown type '{}'", name),
                        ..Default::default()
                    });
                }
                for g in generics {
                    self.check_type(g, symbols, type_params, diagnostics, span);
                }
            }
            Type::Ref(inner)
            | Type::RefMut(inner)
            | Type::Pointer(inner)
            | Type::Array(inner, _)
            | Type::Slice(inner) => {
                self.check_type(inner, symbols, type_params, diagnostics, span);
            }
            _ => {}
        }
    }

    pub fn analyze_block(
        &self,
        block: &Block,
        symbols: &SymbolTable,
        type_params: &[String],
        diagnostics: &mut Vec<Diagnostic>,
        moved_vars: &mut HashMap<String, Span>,
        borrowed_vars: &mut HashMap<String, (BorrowState, Span)>,
    ) {
        for stmt in &block.stmts {
            self.analyze_stmt(stmt, symbols, type_params, diagnostics, moved_vars, borrowed_vars);
        }
    }

    pub fn analyze_stmt(
        &self,
        stmt: &Stmt,
        symbols: &SymbolTable,
        type_params: &[String],
        diagnostics: &mut Vec<Diagnostic>,
        moved_vars: &mut HashMap<String, Span>,
        borrowed_vars: &mut HashMap<String, (BorrowState, Span)>,
    ) {
        match stmt {
            Stmt::Let { ty, value, span, .. } => {
                if let Some(t) = ty {
                    self.check_type(t, symbols, type_params, diagnostics, span);
                }
                let temp_borrows = borrowed_vars.clone();
                self.analyze_expr(value, symbols, diagnostics, moved_vars, borrowed_vars);
                *borrowed_vars = temp_borrows;
            }
            Stmt::Expr(expr) => {
                let temp_borrows = borrowed_vars.clone();
                self.analyze_expr(expr, symbols, diagnostics, moved_vars, borrowed_vars);
                *borrowed_vars = temp_borrows;
            }
            Stmt::Return { value, .. } => {
                if let Some(v) = value {
                    self.analyze_expr(v, symbols, diagnostics, moved_vars, borrowed_vars);
                }
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.analyze_expr(condition, symbols, diagnostics, moved_vars, borrowed_vars);

                let moved_before = moved_vars.clone();
                let borrowed_before = borrowed_vars.clone();

                self.analyze_block(then_block, symbols, type_params, diagnostics, moved_vars, borrowed_vars);

                if let Some(else_b) = else_block {
                    let moved_after_then = moved_vars.clone();
                    *moved_vars = moved_before.clone();
                    *borrowed_vars = borrowed_before.clone();

                    self.analyze_block(else_b, symbols, type_params, diagnostics, moved_vars, borrowed_vars);

                    for (name, span) in moved_after_then {
                        moved_vars.entry(name).or_insert(span);
                    }
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.analyze_expr(condition, symbols, diagnostics, moved_vars, borrowed_vars);
                self.analyze_block(body, symbols, type_params, diagnostics, moved_vars, borrowed_vars);
            }
            Stmt::For { iter, body, .. } => {
                self.analyze_expr(iter, symbols, diagnostics, moved_vars, borrowed_vars);
                self.analyze_block(body, symbols, type_params, diagnostics, moved_vars, borrowed_vars);
            }
            Stmt::Match { value, arms, .. } => {
                self.analyze_expr(value, symbols, diagnostics, moved_vars, borrowed_vars);
                for arm in arms {
                    self.analyze_expr(&arm.body, symbols, diagnostics, moved_vars, borrowed_vars);
                }
            }
            Stmt::Defer { expr, .. } => {
                self.analyze_expr(expr, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            _ => {}
        }
    }

    pub fn analyze_expr(
        &self,
        expr: &Expr,
        symbols: &SymbolTable,
        diagnostics: &mut Vec<Diagnostic>,
        moved_vars: &mut HashMap<String, Span>,
        borrowed_vars: &mut HashMap<String, (BorrowState, Span)>,
    ) {
        match expr {
            Expr::Ident(name, span) => {
                if let Some(moved_span) = moved_vars.get(name) {
                    diagnostics.push(Diagnostic {
                        range: self.span_to_range(span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("vibelang".to_string()),
                        message: format!(
                            "Use of moved value '{}' (moved at line {})",
                            name, moved_span.line
                        ),
                        ..Default::default()
                    });
                }

                let var_exists = symbols.variables.iter().any(|v| &v.name == name)
                    || symbols.functions.contains_key(name);

                if !var_exists && name != "self" && !is_builtin_function(name) {
                    if !symbols.structs.contains_key(name) && !symbols.enums.contains_key(name) {
                        diagnostics.push(Diagnostic {
                            range: self.span_to_range(span),
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("vibelang".to_string()),
                            message: format!("Unknown variable '{}'", name),
                            ..Default::default()
                        });
                    }
                }
            }
            Expr::Binary {
                left, right, op, span,
            } => {
                self.analyze_expr(left, symbols, diagnostics, moved_vars, borrowed_vars);
                self.analyze_expr(right, symbols, diagnostics, moved_vars, borrowed_vars);

                let is_assignment = matches!(op,
                    BinOp::Assign | BinOp::AddAssign | BinOp::SubAssign |
                    BinOp::MulAssign | BinOp::DivAssign | BinOp::ModAssign |
                    BinOp::BitAndAssign | BinOp::BitOrAssign | BinOp::BitXorAssign |
                    BinOp::ShlAssign | BinOp::ShrAssign
                );

                if is_assignment {
                    if let Expr::Ident(name, _) = left.as_ref() {
                        moved_vars.remove(name);
                    }

                    if let Expr::Field { object, .. } = left.as_ref() {
                        if let Expr::Ident(var_name, ident_span) = object.as_ref() {
                            if let Some(var) = symbols.variables.iter().find(|v| {
                                &v.name == var_name &&
                                ident_span.start >= v.scope_start &&
                                ident_span.start <= v.scope_end
                            }) {
                                if var.borrow_state == BorrowState::Borrowed {
                                    diagnostics.push(Diagnostic {
                                        range: self.span_to_range(span),
                                        severity: Some(DiagnosticSeverity::ERROR),
                                        source: Some("vibelang".to_string()),
                                        message: format!(
                                            "cannot mutate through read-only borrow '&{}': use '~{}' for mutable access",
                                            var_name, var_name
                                        ),
                                        ..Default::default()
                                    });
                                }
                            }
                        }
                    }
                }
            }
            Expr::Unary { operand, .. } => {
                self.analyze_expr(operand, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::Call {
                func, args, span, ..
            } => {
                self.analyze_expr(func, symbols, diagnostics, moved_vars, borrowed_vars);

                if let Expr::Ident(name, _) = func.as_ref() {
                    if let Some(func_info) = symbols.functions.get(name) {
                        let expected = func_info.params.len();
                        let actual = args.len();
                        if actual != expected {
                            let params_str: Vec<_> = func_info.params.iter()
                                .map(|(n, t)| format!("{}: {}", n, t))
                                .collect();
                            diagnostics.push(Diagnostic {
                                range: self.span_to_range(span),
                                severity: Some(DiagnosticSeverity::ERROR),
                                source: Some("vibelang".to_string()),
                                message: format!(
                                    "Function '{}' expects {} argument{}, got {}\n  Expected: fn {}({})",
                                    name,
                                    expected,
                                    if expected == 1 { "" } else { "s" },
                                    actual,
                                    name,
                                    params_str.join(", ")
                                ),
                                ..Default::default()
                            });
                        }
                    } else if !is_builtin_function(name) {
                        if !symbols.structs.contains_key(name) && !symbols.enums.contains_key(name)
                        {
                            diagnostics.push(Diagnostic {
                                range: self.span_to_range(span),
                                severity: Some(DiagnosticSeverity::ERROR),
                                source: Some("vibelang".to_string()),
                                message: format!("Unknown function '{}'", name),
                                ..Default::default()
                            });
                        }
                    }
                }

                let is_builtin = if let Expr::Ident(name, _) = func.as_ref() {
                    is_builtin_function(name)
                } else {
                    false
                };

                for arg in args {
                    self.analyze_expr(arg, symbols, diagnostics, moved_vars, borrowed_vars);

                    if !is_builtin {
                        if let Expr::Ident(name, span) = arg {
                            moved_vars.insert(name.clone(), *span);
                        }
                    }
                }
            }
            Expr::MethodCall {
                receiver, args, ..
            } => {
                self.analyze_expr(receiver, symbols, diagnostics, moved_vars, borrowed_vars);
                for arg in args {
                    self.analyze_expr(arg, symbols, diagnostics, moved_vars, borrowed_vars);
                }
            }
            Expr::Field { object, .. } => {
                self.analyze_expr(object, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::Index { array, index, .. } => {
                self.analyze_expr(array, symbols, diagnostics, moved_vars, borrowed_vars);
                self.analyze_expr(index, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::Ref { operand, span } => {
                if let Expr::Ident(name, _) = operand.as_ref() {
                    if let Some((BorrowState::MutBorrowed, existing_span)) = borrowed_vars.get(name)
                    {
                        diagnostics.push(Diagnostic {
                            range: self.span_to_range(span),
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("vibelang".to_string()),
                            message: format!(
                                "Cannot borrow '{}' as immutable because it is already mutably borrowed (at line {})",
                                name, existing_span.line
                            ),
                            ..Default::default()
                        });
                    }
                    borrowed_vars.insert(name.clone(), (BorrowState::Borrowed, *span));
                }
                self.analyze_expr(operand, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::RefMut { operand, span } => {
                if let Expr::Ident(name, _) = operand.as_ref() {
                    if let Some((state, existing_span)) = borrowed_vars.get(name) {
                        let kind = match state {
                            BorrowState::Borrowed => "immutably",
                            BorrowState::MutBorrowed => "mutably",
                            _ => "as",
                        };
                        diagnostics.push(Diagnostic {
                            range: self.span_to_range(span),
                            severity: Some(DiagnosticSeverity::ERROR),
                            source: Some("vibelang".to_string()),
                            message: format!(
                                "Cannot mutably borrow '{}' because it is already borrowed {} (at line {})",
                                name, kind, existing_span.line
                            ),
                            ..Default::default()
                        });
                    }
                    borrowed_vars.insert(name.clone(), (BorrowState::MutBorrowed, *span));
                }
                self.analyze_expr(operand, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::Deref { operand, .. } => {
                self.analyze_expr(operand, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::StructInit {
                name, fields, span, ..
            } => {
                let base_name = name.split('<').next().unwrap_or(name);
                if !symbols.structs.contains_key(base_name)
                    && !symbols.enums.contains_key(base_name)
                    && !is_prelude_type(base_name)
                {
                    diagnostics.push(Diagnostic {
                        range: self.span_to_range(span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        source: Some("vibelang".to_string()),
                        message: format!("Unknown struct '{}'", name),
                        ..Default::default()
                    });
                } else if let Some(struct_info) = symbols.structs.get(base_name) {
                    let provided_fields: Vec<_> = fields.iter().map(|(n, _)| n.as_str()).collect();
                    for (field_name, _, _) in &struct_info.fields {
                        if !provided_fields.contains(&field_name.as_str()) {
                            diagnostics.push(Diagnostic {
                                range: self.span_to_range(span),
                                severity: Some(DiagnosticSeverity::ERROR),
                                source: Some("vibelang".to_string()),
                                message: format!(
                                    "Missing field '{}' in struct '{}'",
                                    field_name, name
                                ),
                                ..Default::default()
                            });
                        }
                    }
                    let known_fields: Vec<_> = struct_info
                        .fields
                        .iter()
                        .map(|(n, _, _)| n.as_str())
                        .collect();
                    for (field_name, _) in fields {
                        if !known_fields.contains(&field_name.as_str()) {
                            diagnostics.push(Diagnostic {
                                range: self.span_to_range(span),
                                severity: Some(DiagnosticSeverity::ERROR),
                                source: Some("vibelang".to_string()),
                                message: format!(
                                    "Unknown field '{}' in struct '{}'",
                                    field_name, name
                                ),
                                ..Default::default()
                            });
                        }
                    }
                }
                for (_, expr) in fields {
                    self.analyze_expr(expr, symbols, diagnostics, moved_vars, borrowed_vars);
                }
            }
            Expr::ArrayInit { elements, .. } => {
                for elem in elements {
                    self.analyze_expr(elem, symbols, diagnostics, moved_vars, borrowed_vars);
                }
            }
            Expr::Block(block) => {
                self.analyze_block(block, symbols, &[], diagnostics, moved_vars, borrowed_vars);
            }
            Expr::Try { operand, .. } => {
                self.analyze_expr(operand, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            Expr::Range { start, end, .. } => {
                self.analyze_expr(start, symbols, diagnostics, moved_vars, borrowed_vars);
                self.analyze_expr(end, symbols, diagnostics, moved_vars, borrowed_vars);
            }
            _ => {}
        }
    }
}
