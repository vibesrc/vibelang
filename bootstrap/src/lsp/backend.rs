//! Backend struct and core methods for the Vibelang LSP

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range, Uri};
use tower_lsp_server::Client;
use crate::ast::Type;
use crate::lexer::{Lexer, LexError, Span};
use crate::parser::{ParseError, Parser};

use crate::lsp::types::{DocumentInfo, SymbolTable};

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
                        self.extract_symbols(&program, &mut symbols);
                        self.analyze_semantics(&program, &symbols, &mut diagnostics);
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
}
