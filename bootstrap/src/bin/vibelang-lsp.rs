//! Vibelang Language Server Protocol implementation
//!
//! Provides IDE features for Vibelang:
//! - Diagnostics (parse errors, type errors, borrow errors)
//! - Completion (struct fields, methods, variables, functions, types)
//! - Hover (types, signatures, borrow state)
//! - Go to definition

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server};

// Import from the library crate
use vibec::ast::{
    Block, BinOp, Expr, Item, Literal, Program, Stmt, Type, VariantFields,
    Pattern as AstPattern,  // Renamed to avoid conflict with ls_types::Pattern
};
use vibec::lexer::{Lexer, Span};
use vibec::parser::{ParseError, Parser};

/// Represents a parsed document with semantic information
#[derive(Debug, Clone)]
struct DocumentInfo {
    /// The source text
    text: String,
    /// Parsed AST (if successful)
    ast: Option<Program>,
    /// Collected symbols for completion and goto
    symbols: SymbolTable,
    /// Collected diagnostics
    diagnostics: Vec<Diagnostic>,
}

/// Symbol table for a document
#[derive(Debug, Clone, Default)]
struct SymbolTable {
    /// Functions: name -> info
    functions: HashMap<String, FunctionInfo>,
    /// Structs: name -> info
    structs: HashMap<String, StructInfo>,
    /// Enums: name -> info
    enums: HashMap<String, EnumInfo>,
    /// Variables in scope at different positions
    variables: Vec<VariableInfo>,
    /// Methods: type_name -> [method_info]
    methods: HashMap<String, Vec<MethodInfo>>,
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    name: String,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    generics: Vec<String>,
    span: Span,
    is_pub: bool,
}

#[derive(Debug, Clone)]
struct StructInfo {
    name: String,
    fields: Vec<(String, String, bool)>,
    generics: Vec<String>,
    span: Span,
    is_pub: bool,
}

#[derive(Debug, Clone)]
struct EnumInfo {
    name: String,
    variants: Vec<VariantData>,
    generics: Vec<String>,
    span: Span,
    is_pub: bool,
}

#[derive(Debug, Clone)]
struct VariantData {
    name: String,
    fields: VariantFieldsData,
    span: Span,
}

#[derive(Debug, Clone)]
enum VariantFieldsData {
    Unit,
    Tuple(Vec<String>),
    Struct(Vec<(String, String)>),
}

#[derive(Debug, Clone)]
struct VariableInfo {
    name: String,
    ty: Option<String>,
    span: Span,
    scope_start: usize,
    scope_end: usize,
    borrow_state: BorrowState,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BorrowState {
    Owned,
    Borrowed,
    MutBorrowed,
    Moved,
}

#[derive(Debug, Clone)]
struct MethodInfo {
    name: String,
    params: Vec<(String, String)>,
    return_type: Option<String>,
    span: Span,
}

struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Uri, DocumentInfo>>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Backend {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn parse_document(&self, uri: &Uri, text: &str) -> DocumentInfo {
        let mut diagnostics = Vec::new();
        let mut symbols = SymbolTable::default();
        let ast;

        let mut lexer = Lexer::new(text);
        match lexer.tokenize() {
            Ok(tokens) => {
                let mut parser = Parser::new(tokens);
                match parser.parse_program() {
                    Ok(program) => {
                        self.extract_symbols(&program, &mut symbols);
                        self.analyze_semantics(&program, &symbols, &mut diagnostics);
                        ast = Some(program);
                    }
                    Err(e) => {
                        diagnostics.push(self.parse_error_to_diagnostic(&e));
                        ast = None;
                        // Keep symbols from last successful parse
                        if let Some(old_doc) = self.documents.read().await.get(uri) {
                            symbols = old_doc.symbols.clone();
                        }
                    }
                }
            }
            Err(e) => {
                diagnostics.push(self.lex_error_to_diagnostic(&e));
                ast = None;
                // Keep symbols from last successful parse
                if let Some(old_doc) = self.documents.read().await.get(uri) {
                    symbols = old_doc.symbols.clone();
                }
            }
        }

        DocumentInfo {
            text: text.to_string(),
            ast,
            symbols,
            diagnostics,
        }
    }

    fn lex_error_to_diagnostic(&self, err: &vibec::lexer::LexError) -> Diagnostic {
        use vibec::lexer::LexError;
        let (message, line, col) = match err {
            LexError::UnexpectedChar(c, line, col) => {
                (format!("Unexpected character '{}'", c), *line, *col)
            }
            LexError::UnterminatedString(line) => {
                ("Unterminated string literal".to_string(), *line, 1)
            }
            LexError::InvalidEscape(c, line) => {
                (format!("Invalid escape sequence '\\{}'", c), *line, 1)
            }
            LexError::InvalidNumber(line) => ("Invalid number literal".to_string(), *line, 1),
        };

        Diagnostic {
            range: Range {
                start: Position {
                    line: line.saturating_sub(1),
                    character: col.saturating_sub(1),
                },
                end: Position {
                    line: line.saturating_sub(1),
                    character: col,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("vibelang".to_string()),
            message,
            ..Default::default()
        }
    }

    fn parse_error_to_diagnostic(&self, err: &ParseError) -> Diagnostic {
        match err {
            ParseError::LexError(msg) => Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("vibelang".to_string()),
                message: msg.clone(),
                ..Default::default()
            },
            ParseError::Unexpected {
                message,
                line,
                column,
            } => Diagnostic {
                range: Range {
                    start: Position {
                        line: line.saturating_sub(1),
                        character: column.saturating_sub(1),
                    },
                    end: Position {
                        line: line.saturating_sub(1),
                        character: *column,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("vibelang".to_string()),
                message: message.clone(),
                ..Default::default()
            },
        }
    }

    fn span_to_range(&self, span: &Span) -> Range {
        Range {
            start: Position {
                line: span.line.saturating_sub(1),
                character: span.column.saturating_sub(1),
            },
            end: Position {
                line: span.line.saturating_sub(1),
                character: span.column.saturating_sub(1) + (span.end - span.start) as u32,
            },
        }
    }

    fn extract_symbols(&self, program: &Program, symbols: &mut SymbolTable) {
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
                        // Determine borrow state from parameter type
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

    fn extract_variables_from_block(
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
                    // Infer type from explicit annotation or from value expression
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

    fn extract_variables_from_pattern(
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

    fn analyze_semantics(
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

                // Pass function's generic type parameters to type checking
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

    fn check_type(
        &self,
        ty: &Type,
        symbols: &SymbolTable,
        type_params: &[String],
        diagnostics: &mut Vec<Diagnostic>,
        span: &Span,
    ) {
        match ty {
            Type::Named { name, generics } => {
                // Check if it's a type parameter in scope (e.g., T in fn foo<T>(...))
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

    fn analyze_block(
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

    fn analyze_stmt(
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
                // Temporary borrows in the expression end after the let binding
                let temp_borrows = borrowed_vars.clone();
                self.analyze_expr(value, symbols, diagnostics, moved_vars, borrowed_vars);
                // Only keep borrows if they're captured in the binding (simplified: clear them)
                *borrowed_vars = temp_borrows;
            }
            Stmt::Expr(expr) => {
                // Temporary borrows in expression statements end after the statement
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

    fn analyze_expr(
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

                if matches!(op, BinOp::Assign) {
                    if let Expr::Ident(name, _) = left.as_ref() {
                        moved_vars.remove(name);
                    }

                    // Check for mutation through read-only borrow
                    // e.g., p.x = 5 when p is &Point
                    if let Expr::Field { object, .. } = left.as_ref() {
                        if let Expr::Ident(var_name, _) = object.as_ref() {
                            if let Some(var) = symbols.variables.iter().find(|v| &v.name == var_name) {
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
                        // Check argument count
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

                // Check if this is a builtin function (they don't move their arguments)
                let is_builtin = if let Expr::Ident(name, _) = func.as_ref() {
                    is_builtin_function(name)
                } else {
                    false
                };

                for arg in args {
                    self.analyze_expr(arg, symbols, diagnostics, moved_vars, borrowed_vars);

                    // Only mark as moved if not a builtin function
                    // Builtins like print, print_int just borrow their arguments
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
                // Check if it's a known struct, enum, or prelude type
                // For generic instantiations like Option<i32>, extract the base name
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
                // Block expressions don't introduce new type parameters
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

    fn type_to_string(&self, ty: &Type) -> String {
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
            Type::Void => "void".to_string(),
            Type::Pointer(inner) => format!("*{}", self.type_to_string(inner)),
            Type::Ref(inner) => format!("&{}", self.type_to_string(inner)),
            Type::RefMut(inner) => format!("~{}", self.type_to_string(inner)),
            Type::Array(inner, size) => format!("{}[{}]", self.type_to_string(inner), size),
            Type::Slice(inner) => format!("Slice<{}>", self.type_to_string(inner)),
            Type::Named { name, generics } => {
                if generics.is_empty() {
                    name.clone()
                } else {
                    let generic_strs: Vec<_> =
                        generics.iter().map(|g| self.type_to_string(g)).collect();
                    format!("{}<{}>", name, generic_strs.join(", "))
                }
            }
            Type::SelfType => "Self".to_string(),
        }
    }

    fn get_completions(&self, doc: &DocumentInfo, position: Position) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        let offset = self.position_to_offset(&doc.text, position);

        let line = doc.text.lines().nth(position.line as usize).unwrap_or("");
        let prefix = &line[..(position.character as usize).min(line.len())];

        // After dot: show struct fields, methods, enum variants
        if prefix.trim_end().ends_with('.') {
            let before_dot = prefix.trim_end().strip_suffix('.').unwrap_or("");
            let expr_name = before_dot.split_whitespace().last().unwrap_or("");

            // Enum variants
            if let Some(enum_info) = doc.symbols.enums.get(expr_name) {
                for variant in &enum_info.variants {
                    let detail = match &variant.fields {
                        VariantFieldsData::Unit => format!("{}.{}", expr_name, variant.name),
                        VariantFieldsData::Tuple(types) => {
                            format!("{}.{}({})", expr_name, variant.name, types.join(", "))
                        }
                        VariantFieldsData::Struct(fields) => {
                            let field_strs: Vec<_> =
                                fields.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                            format!(
                                "{}.{} {{ {} }}",
                                expr_name,
                                variant.name,
                                field_strs.join(", ")
                            )
                        }
                    };
                    completions.push(CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(detail),
                        insert_text: Some(match &variant.fields {
                            VariantFieldsData::Unit => variant.name.clone(),
                            VariantFieldsData::Tuple(types) => {
                                let placeholders: Vec<_> = types
                                    .iter()
                                    .enumerate()
                                    .map(|(i, t)| format!("${{{}:{}}}", i + 1, t))
                                    .collect();
                                format!("{}({})", variant.name, placeholders.join(", "))
                            }
                            VariantFieldsData::Struct(fields) => {
                                let placeholders: Vec<_> = fields
                                    .iter()
                                    .enumerate()
                                    .map(|(i, (n, t))| format!("{}: ${{{}:{}}}", n, i + 1, t))
                                    .collect();
                                format!("{} {{ {} }}", variant.name, placeholders.join(", "))
                            }
                        }),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    });
                }
            }

            // Methods for type
            if let Some(methods) = doc.symbols.methods.get(expr_name) {
                for method in methods {
                    let params_str: Vec<_> = method
                        .params
                        .iter()
                        .map(|(n, t)| format!("{}: {}", n, t))
                        .collect();
                    let sig = format!(
                        "fn {}({}) -> {}",
                        method.name,
                        params_str.join(", "),
                        method.return_type.as_deref().unwrap_or("void")
                    );
                    completions.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(sig),
                        ..Default::default()
                    });
                }
            }

            // Struct fields for variables
            if let Some(var) = doc.symbols.variables.iter().find(|v| v.name == expr_name) {
                if let Some(ref ty_str) = var.ty {
                    // Strip reference prefixes (&, ~, *) and generic params
                    let stripped = ty_str.trim_start_matches('&').trim_start_matches('~').trim_start_matches('*');
                    let base_type = stripped.split('<').next().unwrap_or(stripped);
                    if let Some(struct_info) = doc.symbols.structs.get(base_type.trim()) {
                        for (field_name, field_type, _) in &struct_info.fields {
                            completions.push(CompletionItem {
                                label: field_name.clone(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(field_type.clone()),
                                ..Default::default()
                            });
                        }
                    }
                    if let Some(methods) = doc.symbols.methods.get(base_type.trim()) {
                        for method in methods {
                            completions.push(CompletionItem {
                                label: method.name.clone(),
                                kind: Some(CompletionItemKind::METHOD),
                                detail: Some(format!("fn {}(...)", method.name)),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        } else if let Some(struct_name) = self.find_struct_init_context(&doc.text, position) {
            // Inside struct initializer: show struct fields
            if let Some(struct_info) = doc.symbols.structs.get(&struct_name) {
                // Find already-used fields
                let line = doc.text.lines().nth(position.line as usize).unwrap_or("");
                let used_fields: Vec<&str> = line
                    .split(',')
                    .filter_map(|part| {
                        let trimmed = part.trim();
                        if trimmed.contains(':') {
                            Some(trimmed.split(':').next().unwrap().trim())
                        } else {
                            None
                        }
                    })
                    .collect();

                for (field_name, field_type, _) in &struct_info.fields {
                    if !used_fields.contains(&field_name.as_str()) {
                        completions.push(CompletionItem {
                            label: field_name.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(field_type.clone()),
                            insert_text: Some(format!("{}: ", field_name)),
                            ..Default::default()
                        });
                    }
                }
            }
            return completions;
        } else {
            // After & or ~: show borrowable variables
            let trimmed = prefix.trim_end();
            let after_borrow = trimmed.ends_with('&') || trimmed.ends_with('~');

            if after_borrow {
                for var in &doc.symbols.variables {
                    if var.scope_start <= offset && offset <= var.scope_end {
                        completions.push(CompletionItem {
                            label: var.name.clone(),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: var.ty.clone(),
                            ..Default::default()
                        });
                    }
                }
            } else {
                // General completion: variables, functions, types, keywords

                // Variables in scope
                for var in &doc.symbols.variables {
                    if var.scope_start <= offset && offset <= var.scope_end {
                        completions.push(CompletionItem {
                            label: var.name.clone(),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: var.ty.clone(),
                            ..Default::default()
                        });
                    }
                }

                // Functions
                for (name, func) in &doc.symbols.functions {
                    let params_str: Vec<_> = func
                        .params
                        .iter()
                        .map(|(n, t)| format!("{}: {}", n, t))
                        .collect();
                    let generics_str = if func.generics.is_empty() {
                        String::new()
                    } else {
                        format!("<{}>", func.generics.join(", "))
                    };
                    let sig = format!(
                        "fn {}{}({}) -> {}",
                        name,
                        generics_str,
                        params_str.join(", "),
                        func.return_type.as_deref().unwrap_or("void")
                    );
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(sig),
                        ..Default::default()
                    });
                }

                // Structs
                for (name, struct_info) in &doc.symbols.structs {
                    let generics_str = if struct_info.generics.is_empty() {
                        String::new()
                    } else {
                        format!("<{}>", struct_info.generics.join(", "))
                    };
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::STRUCT),
                        detail: Some(format!("struct {}{}", name, generics_str)),
                        ..Default::default()
                    });
                }

                // Enums
                for (name, enum_info) in &doc.symbols.enums {
                    let generics_str = if enum_info.generics.is_empty() {
                        String::new()
                    } else {
                        format!("<{}>", enum_info.generics.join(", "))
                    };
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::ENUM),
                        detail: Some(format!("enum {}{}", name, generics_str)),
                        ..Default::default()
                    });
                }

                // Keywords
                for kw in &[
                    "let", "fn", "struct", "enum", "impl", "if", "else", "match", "while", "for",
                    "in", "return", "break", "continue", "defer", "pub", "use", "mod", "and",
                    "or", "not", "true", "false",
                ] {
                    completions.push(CompletionItem {
                        label: kw.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    });
                }

                // Primitive types
                for ty in &[
                    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "bool",
                    "void",
                ] {
                    completions.push(CompletionItem {
                        label: ty.to_string(),
                        kind: Some(CompletionItemKind::TYPE_PARAMETER),
                        ..Default::default()
                    });
                }
            }
        }

        completions
    }

    fn get_hover(&self, doc: &DocumentInfo, position: Position) -> Option<Hover> {
        let offset = self.position_to_offset(&doc.text, position);
        let word = self.get_word_at_position(&doc.text, position)?;

        // Variable
        for var in &doc.symbols.variables {
            if var.name == word && var.scope_start <= offset && offset <= var.scope_end {
                let borrow_info = match var.borrow_state {
                    BorrowState::Owned => "owned",
                    BorrowState::Borrowed => "borrowed (&)",
                    BorrowState::MutBorrowed => "mutably borrowed (~)",
                    BorrowState::Moved => "moved",
                };
                let ty_str = var.ty.as_deref().unwrap_or("(inferred)");
                let content = format!(
                    "**Variable** `{}`\n\n**Type:** `{}`\n\n**State:** {}",
                    word, ty_str, borrow_info
                );
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: None,
                });
            }
        }

        // Function
        if let Some(func) = doc.symbols.functions.get(&word) {
            let params_str: Vec<_> = func
                .params
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
                .collect();
            let generics_str = if func.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", func.generics.join(", "))
            };
            let sig = format!(
                "fn {}{}({}) -> {}",
                word,
                generics_str,
                params_str.join(", "),
                func.return_type.as_deref().unwrap_or("void")
            );
            let visibility = if func.is_pub { "pub " } else { "" };
            let content = format!("**Function**\n\n```vibe\n{}{}\n```", visibility, sig);
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }

        // Struct
        if let Some(struct_info) = doc.symbols.structs.get(&word) {
            let generics_str = if struct_info.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", struct_info.generics.join(", "))
            };
            let fields_str: Vec<_> = struct_info
                .fields
                .iter()
                .map(|(n, t, is_pub)| {
                    let vis = if *is_pub { "pub " } else { "" };
                    format!("    {}{}: {}", vis, n, t)
                })
                .collect();
            let visibility = if struct_info.is_pub { "pub " } else { "" };
            let content = format!(
                "**Struct**\n\n```vibe\n{}struct {}{} {{\n{}\n}}\n```",
                visibility,
                word,
                generics_str,
                fields_str.join(",\n")
            );
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }

        // Enum
        if let Some(enum_info) = doc.symbols.enums.get(&word) {
            let generics_str = if enum_info.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", enum_info.generics.join(", "))
            };
            let variants_str: Vec<_> = enum_info
                .variants
                .iter()
                .map(|v| match &v.fields {
                    VariantFieldsData::Unit => format!("    {}", v.name),
                    VariantFieldsData::Tuple(types) => {
                        format!("    {}({})", v.name, types.join(", "))
                    }
                    VariantFieldsData::Struct(fields) => {
                        let field_strs: Vec<_> =
                            fields.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                        format!("    {} {{ {} }}", v.name, field_strs.join(", "))
                    }
                })
                .collect();
            let visibility = if enum_info.is_pub { "pub " } else { "" };
            let content = format!(
                "**Enum**\n\n```vibe\n{}enum {}{} {{\n{}\n}}\n```",
                visibility,
                word,
                generics_str,
                variants_str.join(",\n")
            );
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }

        // Primitive types
        if is_builtin_type(&word) {
            let desc = match word.as_str() {
                "i8" => "8-bit signed integer (-128 to 127)",
                "i16" => "16-bit signed integer",
                "i32" => "32-bit signed integer",
                "i64" => "64-bit signed integer",
                "u8" => "8-bit unsigned integer (0 to 255)",
                "u16" => "16-bit unsigned integer",
                "u32" => "32-bit unsigned integer",
                "u64" => "64-bit unsigned integer",
                "f32" => "32-bit floating point",
                "f64" => "64-bit floating point",
                "bool" => "Boolean type (true or false)",
                "void" => "Unit type (no value)",
                _ => "Primitive type",
            };
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("**Primitive Type** `{}`\n\n{}", word, desc),
                }),
                range: None,
            });
        }

        None
    }

    fn get_definition(&self, doc: &DocumentInfo, uri: &Uri, position: Position) -> Option<Location> {
        let word = self.get_word_at_position(&doc.text, position)?;
        let offset = self.position_to_offset(&doc.text, position);

        // Variable
        for var in &doc.symbols.variables {
            if var.name == word && var.scope_start <= offset && offset <= var.scope_end {
                return Some(Location {
                    uri: uri.clone(),
                    range: self.span_to_range(&var.span),
                });
            }
        }

        // Function
        if let Some(func) = doc.symbols.functions.get(&word) {
            return Some(Location {
                uri: uri.clone(),
                range: self.span_to_range(&func.span),
            });
        }

        // Struct
        if let Some(struct_info) = doc.symbols.structs.get(&word) {
            return Some(Location {
                uri: uri.clone(),
                range: self.span_to_range(&struct_info.span),
            });
        }

        // Enum
        if let Some(enum_info) = doc.symbols.enums.get(&word) {
            return Some(Location {
                uri: uri.clone(),
                range: self.span_to_range(&enum_info.span),
            });
        }

        None
    }

    fn infer_type_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::StructInit { name, generics, .. } => {
                if generics.is_empty() {
                    Some(name.clone())
                } else {
                    let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                    Some(format!("{}<{}>", name, generic_strs.join(", ")))
                }
            }
            Expr::Call { func, .. } => {
                // For constructor-style calls like Option.Some(x), we could infer the type
                if let Expr::Field { object, field, .. } = func.as_ref() {
                    if let Expr::Ident(enum_name, _) = object.as_ref() {
                        // Looks like EnumName.Variant(...)
                        return Some(enum_name.clone());
                    }
                    // For Option.None etc
                    Some(field.clone())
                } else {
                    None
                }
            }
            Expr::ArrayInit { elements, .. } => {
                // Try to infer element type from first element
                if let Some(first) = elements.first() {
                    if let Some(elem_ty) = self.infer_type_from_expr(first) {
                        return Some(format!("{}[{}]", elem_ty, elements.len()));
                    }
                }
                None
            }
            Expr::Ref { operand, .. } => {
                self.infer_type_from_expr(operand).map(|t| format!("&{}", t))
            }
            Expr::RefMut { operand, .. } => {
                self.infer_type_from_expr(operand).map(|t| format!("~{}", t))
            }
            Expr::Literal(lit, _) => match lit {
                Literal::Int(_) => Some("i32".to_string()),
                Literal::Float(_) => Some("f64".to_string()),
                Literal::Bool(_) => Some("bool".to_string()),
                Literal::String(_) => Some("Slice<u8>".to_string()),
            },
            _ => None,
        }
    }

    fn find_struct_init_context(&self, text: &str, position: Position) -> Option<String> {
        // Look backwards from cursor to find "StructName {" that is still open
        let lines: Vec<&str> = text.lines().collect();
        let current_line = position.line as usize;
        let col = position.character as usize;

        // Build the text before cursor
        let mut search_text = String::new();
        for (i, line) in lines.iter().enumerate() {
            if i < current_line {
                search_text.push_str(line);
                search_text.push('\n');
            } else if i == current_line {
                search_text.push_str(&line[..col.min(line.len())]);
                break;
            }
        }

        // Use a stack to track open braces - only consider braces that are still open
        let mut brace_stack: Vec<usize> = Vec::new();

        for (i, c) in search_text.char_indices() {
            match c {
                '{' => brace_stack.push(i),
                '}' => { brace_stack.pop(); }
                _ => {}
            }
        }

        // Check if the innermost open brace is a struct initializer
        if let Some(&brace_pos) = brace_stack.last() {
            let before_brace = &search_text[..brace_pos];
            // Extract the identifier before the brace
            let struct_name: String = before_brace
                .trim_end()
                .chars()
                .rev()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect::<String>()
                .chars()
                .rev()
                .collect();

            // Check if it's actually a struct (starts with uppercase)
            if !struct_name.is_empty() && struct_name.chars().next().unwrap().is_uppercase() {
                return Some(struct_name);
            }
        }

        None
    }

    fn get_signature_help(&self, doc: &DocumentInfo, position: Position) -> Option<SignatureHelp> {
        let line = doc.text.lines().nth(position.line as usize)?;
        let col = position.character as usize;
        let prefix = &line[..col.min(line.len())];

        // Find the function name by looking backwards for an open paren
        let mut paren_depth = 0;
        let mut func_end = None;
        let mut active_param = 0u32;

        for (i, c) in prefix.char_indices().rev() {
            match c {
                ')' => paren_depth += 1,
                '(' => {
                    if paren_depth == 0 {
                        func_end = Some(i);
                        break;
                    }
                    paren_depth -= 1;
                }
                ',' if paren_depth == 0 => active_param += 1,
                _ => {}
            }
        }

        let func_end = func_end?;
        let before_paren = &prefix[..func_end];

        // Extract function name
        let func_name: String = before_paren
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        if func_name.is_empty() {
            return None;
        }

        // Look up function in symbols
        if let Some(func) = doc.symbols.functions.get(&func_name) {
            let params: Vec<ParameterInformation> = func
                .params
                .iter()
                .map(|(name, ty)| ParameterInformation {
                    label: ParameterLabel::Simple(format!("{}: {}", name, ty)),
                    documentation: None,
                })
                .collect();

            let params_str: Vec<_> = func
                .params
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
                .collect();

            let signature = SignatureInformation {
                label: format!(
                    "fn {}({}) -> {}",
                    func_name,
                    params_str.join(", "),
                    func.return_type.as_deref().unwrap_or("void")
                ),
                documentation: None,
                parameters: Some(params),
                active_parameter: Some(active_param),
            };

            return Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: Some(active_param),
            });
        }

        None
    }

    fn position_to_offset(&self, text: &str, position: Position) -> usize {
        let mut offset = 0;
        for (i, line) in text.lines().enumerate() {
            if i == position.line as usize {
                return offset + (position.character as usize).min(line.len());
            }
            offset += line.len() + 1;
        }
        text.len()
    }

    fn get_word_at_position(&self, text: &str, position: Position) -> Option<String> {
        let line = text.lines().nth(position.line as usize)?;
        let col = position.character as usize;

        if col > line.len() {
            return None;
        }

        let chars: Vec<char> = line.chars().collect();
        let mut start = col;
        let mut end = col;

        while start > 0 && is_ident_char(chars[start - 1]) {
            start -= 1;
        }

        while end < chars.len() && is_ident_char(chars[end]) {
            end += 1;
        }

        if start == end {
            return None;
        }

        Some(chars[start..end].iter().collect())
    }
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "f32"
            | "f64"
            | "bool"
            | "void"
            | "Slice"
    )
}

/// Types from the prelude that are always available
fn is_prelude_type(name: &str) -> bool {
    matches!(name, "Option" | "Result" | "Vec")
}

fn is_builtin_function(name: &str) -> bool {
    matches!(
        name,
        "print" | "println" | "println_int" | "print_int" | "panic" | "malloc" | "realloc" | "free" | "sizeof" | "memcpy" | "ptr_write_i64" | "ptr_read_i64"
    )
}

/// Semantic token type indices (must match SEMANTIC_TOKEN_TYPES order)
mod semantic_tokens {
    pub const ENUM: u32 = 0;
    pub const ENUM_MEMBER: u32 = 1;
    pub const STRUCT: u32 = 2;
    pub const TYPE_PARAMETER: u32 = 3;
    pub const FUNCTION: u32 = 4;
    pub const PARAMETER: u32 = 5;
    pub const VARIABLE: u32 = 6;
    pub const PROPERTY: u32 = 7;
}

/// Returns the semantic token legend for this server
fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::ENUM,           // 0
            SemanticTokenType::ENUM_MEMBER,    // 1
            SemanticTokenType::STRUCT,         // 2
            SemanticTokenType::TYPE_PARAMETER, // 3
            SemanticTokenType::FUNCTION,       // 4
            SemanticTokenType::PARAMETER,      // 5
            SemanticTokenType::VARIABLE,       // 6
            SemanticTokenType::PROPERTY,       // 7
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::DECLARATION,
        ],
    }
}

/// A raw semantic token before delta encoding
#[derive(Debug, Clone)]
struct RawSemanticToken {
    line: u32,
    start: u32,
    length: u32,
    token_type: u32,
    modifiers: u32,
}

impl Backend {
    /// Collect semantic tokens from a parsed program
    fn collect_semantic_tokens(&self, program: &Program, text: &str) -> Vec<RawSemanticToken> {
        let mut tokens = Vec::new();

        for item in &program.items {
            match item {
                Item::Enum(e) => {
                    // Enum name
                    tokens.push(RawSemanticToken {
                        line: e.span.line.saturating_sub(1) as u32,
                        start: self.find_name_column(text, e.span.line, &e.name),
                        length: e.name.len() as u32,
                        token_type: semantic_tokens::ENUM,
                        modifiers: 1, // DEFINITION
                    });

                    // Generic parameters
                    for generic in &e.generics {
                        if let Some((line, col)) = self.find_generic_param(text, e.span.line, generic) {
                            tokens.push(RawSemanticToken {
                                line: line as u32,
                                start: col as u32,
                                length: generic.len() as u32,
                                token_type: semantic_tokens::TYPE_PARAMETER,
                                modifiers: 1,
                            });
                        }
                    }

                    // Enum variants
                    for variant in &e.variants {
                        tokens.push(RawSemanticToken {
                            line: variant.span.line.saturating_sub(1) as u32,
                            start: self.find_name_column(text, variant.span.line, &variant.name),
                            length: variant.name.len() as u32,
                            token_type: semantic_tokens::ENUM_MEMBER,
                            modifiers: 1, // DEFINITION
                        });
                    }
                }
                Item::Struct(s) => {
                    // Struct name
                    tokens.push(RawSemanticToken {
                        line: s.span.line.saturating_sub(1) as u32,
                        start: self.find_name_column(text, s.span.line, &s.name),
                        length: s.name.len() as u32,
                        token_type: semantic_tokens::STRUCT,
                        modifiers: 1,
                    });

                    // Generic parameters
                    for generic in &s.generics {
                        if let Some((line, col)) = self.find_generic_param(text, s.span.line, generic) {
                            tokens.push(RawSemanticToken {
                                line: line as u32,
                                start: col as u32,
                                length: generic.len() as u32,
                                token_type: semantic_tokens::TYPE_PARAMETER,
                                modifiers: 1,
                            });
                        }
                    }

                    // Struct fields
                    for field in &s.fields {
                        tokens.push(RawSemanticToken {
                            line: field.span.line.saturating_sub(1) as u32,
                            start: self.find_name_column(text, field.span.line, &field.name),
                            length: field.name.len() as u32,
                            token_type: semantic_tokens::PROPERTY,
                            modifiers: 1,
                        });
                    }
                }
                Item::Function(f) => {
                    // Function name
                    tokens.push(RawSemanticToken {
                        line: f.span.line.saturating_sub(1) as u32,
                        start: self.find_name_column(text, f.span.line, &f.name),
                        length: f.name.len() as u32,
                        token_type: semantic_tokens::FUNCTION,
                        modifiers: 1,
                    });

                    // Generic parameters
                    for generic in &f.generics {
                        if let Some((line, col)) = self.find_generic_param(text, f.span.line, generic) {
                            tokens.push(RawSemanticToken {
                                line: line as u32,
                                start: col as u32,
                                length: generic.len() as u32,
                                token_type: semantic_tokens::TYPE_PARAMETER,
                                modifiers: 1,
                            });
                        }
                    }

                    // Parameters
                    for param in &f.params {
                        tokens.push(RawSemanticToken {
                            line: param.span.line.saturating_sub(1) as u32,
                            start: self.find_name_column(text, param.span.line, &param.name),
                            length: param.name.len() as u32,
                            token_type: semantic_tokens::PARAMETER,
                            modifiers: 1,
                        });
                    }

                    // Collect tokens from function body
                    self.collect_tokens_from_block(&f.body, text, &mut tokens, &f.generics);
                }
                _ => {}
            }
        }

        // Sort by position
        tokens.sort_by(|a, b| {
            a.line.cmp(&b.line).then_with(|| a.start.cmp(&b.start))
        });

        tokens
    }

    /// Collect semantic tokens from a block
    fn collect_tokens_from_block(
        &self,
        block: &Block,
        text: &str,
        tokens: &mut Vec<RawSemanticToken>,
        type_params: &[String],
    ) {
        for stmt in &block.stmts {
            self.collect_tokens_from_stmt(stmt, text, tokens, type_params);
        }
    }

    /// Collect semantic tokens from a statement
    fn collect_tokens_from_stmt(
        &self,
        stmt: &Stmt,
        text: &str,
        tokens: &mut Vec<RawSemanticToken>,
        type_params: &[String],
    ) {
        match stmt {
            Stmt::Let { name, value, span, .. } => {
                tokens.push(RawSemanticToken {
                    line: span.line.saturating_sub(1) as u32,
                    start: self.find_name_column(text, span.line, name),
                    length: name.len() as u32,
                    token_type: semantic_tokens::VARIABLE,
                    modifiers: 1,
                });
                self.collect_tokens_from_expr(value, text, tokens, type_params);
            }
            Stmt::Expr(expr) => {
                self.collect_tokens_from_expr(expr, text, tokens, type_params);
            }
            Stmt::Return { value: Some(v), .. } => {
                self.collect_tokens_from_expr(v, text, tokens, type_params);
            }
            Stmt::If { condition, then_block, else_block, .. } => {
                self.collect_tokens_from_expr(condition, text, tokens, type_params);
                self.collect_tokens_from_block(then_block, text, tokens, type_params);
                if let Some(eb) = else_block {
                    self.collect_tokens_from_block(eb, text, tokens, type_params);
                }
            }
            Stmt::While { condition, body, .. } => {
                self.collect_tokens_from_expr(condition, text, tokens, type_params);
                self.collect_tokens_from_block(body, text, tokens, type_params);
            }
            Stmt::For { iter, body, .. } => {
                self.collect_tokens_from_expr(iter, text, tokens, type_params);
                self.collect_tokens_from_block(body, text, tokens, type_params);
            }
            Stmt::Match { value, arms, .. } => {
                self.collect_tokens_from_expr(value, text, tokens, type_params);
                for arm in arms {
                    // Collect enum variants from patterns
                    self.collect_tokens_from_pattern(&arm.pattern, text, tokens);
                    self.collect_tokens_from_expr(&arm.body, text, tokens, type_params);
                }
            }
            _ => {}
        }
    }

    /// Collect semantic tokens from an expression
    fn collect_tokens_from_expr(
        &self,
        expr: &Expr,
        text: &str,
        tokens: &mut Vec<RawSemanticToken>,
        type_params: &[String],
    ) {
        match expr {
            Expr::Field { object, field, span } => {
                self.collect_tokens_from_expr(object, text, tokens, type_params);

                // Check if this is an enum variant access (Type.Variant)
                if let Expr::Ident(name, _) | Expr::StructInit { name, .. } = object.as_ref() {
                    // Extract base name without generics
                    let base_name = name.split('<').next().unwrap_or(name);
                    if base_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                        // Looks like EnumName.Variant - mark field as enum member
                        tokens.push(RawSemanticToken {
                            line: span.line.saturating_sub(1) as u32,
                            start: self.find_field_column(text, span.line, field),
                            length: field.len() as u32,
                            token_type: semantic_tokens::ENUM_MEMBER,
                            modifiers: 0,
                        });
                    }
                }
            }
            Expr::Call { func, args, .. } => {
                self.collect_tokens_from_expr(func, text, tokens, type_params);
                for arg in args {
                    self.collect_tokens_from_expr(arg, text, tokens, type_params);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.collect_tokens_from_expr(left, text, tokens, type_params);
                self.collect_tokens_from_expr(right, text, tokens, type_params);
            }
            Expr::Unary { operand, .. } => {
                self.collect_tokens_from_expr(operand, text, tokens, type_params);
            }
            Expr::StructInit { fields, .. } => {
                for (_, expr) in fields {
                    self.collect_tokens_from_expr(expr, text, tokens, type_params);
                }
            }
            Expr::ArrayInit { elements, .. } => {
                for elem in elements {
                    self.collect_tokens_from_expr(elem, text, tokens, type_params);
                }
            }
            Expr::Index { array, index, .. } => {
                self.collect_tokens_from_expr(array, text, tokens, type_params);
                self.collect_tokens_from_expr(index, text, tokens, type_params);
            }
            Expr::Block(block) => {
                self.collect_tokens_from_block(block, text, tokens, type_params);
            }
            Expr::Ref { operand, .. } | Expr::RefMut { operand, .. } | Expr::Deref { operand, .. } => {
                self.collect_tokens_from_expr(operand, text, tokens, type_params);
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.collect_tokens_from_expr(receiver, text, tokens, type_params);
                for arg in args {
                    self.collect_tokens_from_expr(arg, text, tokens, type_params);
                }
            }
            _ => {}
        }
    }

    /// Collect tokens from a pattern (for match arms)
    fn collect_tokens_from_pattern(&self, pattern: &AstPattern, text: &str, tokens: &mut Vec<RawSemanticToken>) {
        match pattern {
            AstPattern::Enum { path, fields, .. } => {
                // The variant name is the last part of the path
                if path.len() >= 2 {
                    // Try to find the variant in the text
                    // This is approximate - we'd need span info for exact positioning
                }
                for field in fields {
                    self.collect_tokens_from_pattern(field, text, tokens);
                }
            }
            _ => {}
        }
    }

    /// Find the column of a name in a line
    fn find_name_column(&self, text: &str, line: u32, name: &str) -> u32 {
        if let Some(line_text) = text.lines().nth(line.saturating_sub(1) as usize) {
            if let Some(pos) = line_text.find(name) {
                return pos as u32;
            }
        }
        0
    }

    /// Find the column of a field access in a line
    fn find_field_column(&self, text: &str, line: u32, field: &str) -> u32 {
        if let Some(line_text) = text.lines().nth(line.saturating_sub(1) as usize) {
            // Look for .field pattern
            if let Some(pos) = line_text.find(&format!(".{}", field)) {
                return (pos + 1) as u32; // +1 to skip the dot
            }
        }
        0
    }

    /// Find a generic parameter in the text near the given line
    fn find_generic_param(&self, text: &str, line: u32, param: &str) -> Option<(u32, u32)> {
        if let Some(line_text) = text.lines().nth(line.saturating_sub(1) as usize) {
            // Look for <...param...> pattern
            if let Some(start) = line_text.find('<') {
                if let Some(end) = line_text[start..].find('>') {
                    let generic_part = &line_text[start..start+end+1];
                    if let Some(pos) = generic_part.find(param) {
                        // Make sure it's a whole word match
                        let abs_pos = start + pos;
                        return Some((line.saturating_sub(1), abs_pos as u32));
                    }
                }
            }
        }
        None
    }

    /// Encode raw tokens into the LSP delta format
    fn encode_semantic_tokens(&self, raw_tokens: Vec<RawSemanticToken>) -> Vec<SemanticToken> {
        let mut result = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;

        for token in raw_tokens {
            let delta_line = token.line - prev_line;
            let delta_start = if delta_line == 0 {
                token.start - prev_start
            } else {
                token.start
            };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: token.modifiers,
            });

            prev_line = token.line;
            prev_start = token.start;
        }

        result
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "vibelang-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        "&".to_string(),
                        "~".to_string(),
                    ]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: Some(vec![",".to_string()]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                        legend: semantic_token_legend(),
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        range: Some(false),
                        ..Default::default()
                    }),
                ),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Vibelang LSP initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text;

        let doc = self.parse_document(&uri, &text).await;
        let diagnostics = doc.diagnostics.clone();

        self.documents.write().await.insert(uri.clone(), doc);

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();

        if let Some(change) = params.content_changes.into_iter().last() {
            let doc = self.parse_document(&uri, &change.text).await;
            let diagnostics = doc.diagnostics.clone();

            self.documents.write().await.insert(uri.clone(), doc);

            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents
            .write()
            .await
            .remove(&params.text_document.uri);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            // Debug logging
            let line = doc.text.lines().nth(position.line as usize).unwrap_or("");
            let prefix = &line[..(position.character as usize).min(line.len())];
            self.client.log_message(
                MessageType::INFO,
                format!("Completion at line {}, col {}: prefix='{}', vars={:?}",
                    position.line, position.character, prefix,
                    doc.symbols.variables.iter().map(|v| format!("{}:{:?}", v.name, v.ty)).collect::<Vec<_>>()
                )
            ).await;

            let completions = self.get_completions(doc, position);
            self.client.log_message(
                MessageType::INFO,
                format!("Returning {} completions", completions.len())
            ).await;
            return Ok(Some(CompletionResponse::Array(completions)));
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            return Ok(self.get_hover(doc, position));
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            if let Some(location) = self.get_definition(doc, &uri, position) {
                return Ok(Some(GotoDefinitionResponse::Scalar(location)));
            }
        }

        Ok(None)
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            return Ok(self.get_signature_help(doc, position));
        }

        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let documents = self.documents.read().await;
        if let Some(doc) = documents.get(&uri) {
            if let Some(ref ast) = doc.ast {
                let raw_tokens = self.collect_semantic_tokens(ast, &doc.text);
                let encoded = self.encode_semantic_tokens(raw_tokens);

                return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                    result_id: None,
                    data: encoded,
                })));
            }
        }

        Ok(None)
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
