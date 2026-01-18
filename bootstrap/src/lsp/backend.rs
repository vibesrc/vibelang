//! Backend struct and core methods for the Vibelang LSP

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range, Uri};
use tower_lsp_server::Client;
use crate::ast::{Item, Type};
use crate::lexer::{Lexer, LexError, Span};
use crate::parser::{ParseError, Parser};
use crate::analysis;
use crate::lsp::utils::semantic_error_to_diagnostic;

use crate::lsp::types::{DocumentInfo, SymbolTable, EnumInfo, StructInfo, VariantData, VariantFieldsData};

pub struct Backend {
    pub client: Client,
    pub documents: Arc<RwLock<HashMap<Uri, DocumentInfo>>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Backend {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn parse_document(&self, uri: &Uri, text: &str) -> DocumentInfo {
        let mut diagnostics = Vec::new();
        let mut symbols = SymbolTable::default();
        let ast;

        let mut lexer = Lexer::new(text);
        match lexer.tokenize() {
            Ok(tokens) => {
                let mut parser = Parser::new(tokens);
                match parser.parse_program() {
                    Ok(program) => {
                        // Use the shared semantic analyzer
                        let analyzer = analysis::SemanticAnalyzer::new();
                        let result = analyzer.analyze(&program);

                        // Use symbols from the analyzer
                        symbols = result.symbols;

                        // Process imports to load std modules
                        for item in &program.items {
                            if let Item::Use(use_stmt) = item {
                                self.process_import(use_stmt, &mut symbols);
                            }
                        }

                        // Inject prelude symbols for LSP features
                        self.inject_prelude_symbols(&mut symbols);

                        // Convert semantic errors to LSP diagnostics
                        for error in &result.errors {
                            diagnostics.push(semantic_error_to_diagnostic(error, text));
                        }

                        ast = Some(program);
                    }
                    Err(e) => {
                        diagnostics.push(self.parse_error_to_diagnostic(&e));
                        ast = None;
                        if let Some(old_doc) = self.documents.read().await.get(uri) {
                            symbols = old_doc.symbols.clone();
                        }
                    }
                }
            }
            Err(e) => {
                diagnostics.push(self.lex_error_to_diagnostic(&e));
                ast = None;
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

    pub fn lex_error_to_diagnostic(&self, err: &LexError) -> Diagnostic {
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
            LexError::UnterminatedChar(line) => {
                ("Unterminated character literal".to_string(), *line, 1)
            }
            LexError::EmptyChar(line) => ("Empty character literal".to_string(), *line, 1),
            LexError::NonAsciiChar(line) => {
                ("Non-ASCII character in character literal".to_string(), *line, 1)
            }
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

    pub fn parse_error_to_diagnostic(&self, err: &ParseError) -> Diagnostic {
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

    pub fn span_to_range(&self, span: &Span) -> Range {
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

    pub fn type_to_string(&self, ty: &Type) -> String {
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
            Type::Char => "char".to_string(),
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
            Type::Tuple(types) => {
                let type_strs: Vec<_> = types.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})", type_strs.join(", "))
            }
            Type::Fn(params, return_type) => {
                let param_strs: Vec<_> = params.iter().map(|t| self.type_to_string(t)).collect();
                format!("fn({}) -> {}", param_strs.join(", "), self.type_to_string(return_type))
            }
            Type::SelfType => "Self".to_string(),
        }
    }

    pub fn position_to_offset(&self, text: &str, position: Position) -> usize {
        let mut offset = 0;
        for (i, line) in text.lines().enumerate() {
            if i == position.line as usize {
                return offset + (position.character as usize).min(line.len());
            }
            offset += line.len() + 1;
        }
        text.len()
    }

    /// Inject prelude symbols (Option, Result, Error) into the symbol table
    pub fn inject_prelude_symbols(&self, symbols: &mut SymbolTable) {
        let prelude_span = Span { start: 0, end: 0, line: 0, column: 0 };

        // Option<T> enum
        if !symbols.enums.contains_key("Option") {
            symbols.enums.insert("Option".to_string(), EnumInfo {
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
        if !symbols.enums.contains_key("Result") {
            symbols.enums.insert("Result".to_string(), EnumInfo {
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
        if !symbols.structs.contains_key("Error") {
            symbols.structs.insert("Error".to_string(), StructInfo {
                name: "Error".to_string(),
                fields: vec![("message".to_string(), "Slice<u8>".to_string(), true)],
                generics: vec![],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Vec<T> struct (from std.collections)
        if !symbols.structs.contains_key("Vec") {
            symbols.structs.insert("Vec".to_string(), StructInfo {
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

        // String struct (from std.string)
        if !symbols.structs.contains_key("String") {
            symbols.structs.insert("String".to_string(), StructInfo {
                name: "String".to_string(),
                fields: vec![
                    ("data".to_string(), "Vec<u8>".to_string(), false),
                ],
                generics: vec![],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Map<K, V> struct (from std.collections)
        if !symbols.structs.contains_key("Map") {
            symbols.structs.insert("Map".to_string(), StructInfo {
                name: "Map".to_string(),
                fields: vec![],  // Internal fields not exposed
                generics: vec!["K".to_string(), "V".to_string()],
                span: prelude_span,
                is_pub: true,
            });
        }

        // Set<T> struct (from std.collections)
        if !symbols.structs.contains_key("Set") {
            symbols.structs.insert("Set".to_string(), StructInfo {
                name: "Set".to_string(),
                fields: vec![],  // Internal fields not exposed
                generics: vec!["T".to_string()],
                span: prelude_span,
                is_pub: true,
            });
        }
    }
}
