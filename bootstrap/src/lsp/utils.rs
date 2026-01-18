//! Utility functions for the Vibelang LSP

use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, SemanticTokenModifier, SemanticTokenType,
    SemanticTokensLegend,
};
use crate::analysis::SemanticError;

// Re-export shared type checking functions from analysis module
pub use crate::analysis::{is_builtin_function, is_builtin_type, is_copy_type, is_prelude_type};

/// Convert a SemanticError to an LSP Diagnostic
pub fn semantic_error_to_diagnostic(error: &SemanticError, source: &str) -> Diagnostic {
    let span = error.span();
    let message = error.message();

    // Convert byte offset to line/column
    let (start_line, start_col) = offset_to_position(source, span.start);
    let (end_line, end_col) = offset_to_position(source, span.end);

    Diagnostic {
        range: Range {
            start: Position {
                line: start_line,
                character: start_col,
            },
            end: Position {
                line: end_line,
                character: end_col,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        message,
        source: Some("vibelang".to_string()),
        code: None,
        code_description: None,
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Convert a byte offset to (line, column) in a source string
fn offset_to_position(source: &str, offset: usize) -> (u32, u32) {
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Semantic token type indices (must match SEMANTIC_TOKEN_TYPES order)
pub mod semantic_token_types {
    pub const ENUM: u32 = 0;
    pub const ENUM_MEMBER: u32 = 1;
    pub const STRUCT: u32 = 2;
    pub const TYPE_PARAMETER: u32 = 3;
    pub const FUNCTION: u32 = 4;
    pub const PARAMETER: u32 = 5;
    pub const VARIABLE: u32 = 6;
    pub const PROPERTY: u32 = 7;
    pub const TYPE: u32 = 8;  // Built-in types like i32, bool, etc.
}

/// Returns the semantic token legend for this server
pub fn semantic_token_legend() -> SemanticTokensLegend {
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
            SemanticTokenType::TYPE,           // 8 - Built-in types
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::DECLARATION,
        ],
    }
}

pub fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

/// Standard library modules available for import (dynamically discovered)
pub fn std_modules() -> Vec<String> {
    // Try to find and list std modules dynamically
    if let Some(std_path) = get_std_src_path() {
        if let Ok(entries) = std::fs::read_dir(&std_path) {
            return entries
                .filter_map(|e| e.ok())
                .filter(|e| e.path().is_dir())
                .filter_map(|e| e.file_name().into_string().ok())
                .filter(|name| !name.starts_with('.'))
                .collect();
        }
    }
    // Fallback to empty if std not found
    vec![]
}

/// Get the path to the std library src directory
fn get_std_src_path() -> Option<std::path::PathBuf> {
    use std::path::PathBuf;

    // First, check environment variable
    if let Ok(path) = std::env::var("VIBELANG_STD") {
        let path = PathBuf::from(path).join("src");
        if path.exists() {
            return Some(path);
        }
    }

    // Also check legacy VIBELANG_STDLIB
    if let Ok(path) = std::env::var("VIBELANG_STDLIB") {
        let path = PathBuf::from(path).join("src");
        if path.exists() {
            return Some(path);
        }
    }

    // Try relative to current executable
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            for relative in &["../std/src", "../../std/src", "../../../std/src"] {
                let stdlib = exe_dir.join(relative);
                if stdlib.exists() {
                    return stdlib.canonicalize().ok();
                }
            }
        }
    }

    None
}
