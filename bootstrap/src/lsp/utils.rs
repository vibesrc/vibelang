//! Utility functions for the Vibelang LSP

use tower_lsp_server::ls_types::{SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend};

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

pub fn is_builtin_type(name: &str) -> bool {
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
pub fn is_prelude_type(name: &str) -> bool {
    matches!(name, "Option" | "Result" | "Error")
}

pub fn is_builtin_function(name: &str) -> bool {
    matches!(
        name,
        "print" | "println" | "panic"
        | "malloc" | "realloc" | "free" | "memcpy"
        | "sizeof" | "null"
        | "ptr_null" | "ptr_is_null" | "ptr_write" | "ptr_read" | "ptr_add"
        | "ptr_write_i64" | "ptr_read_i64"
    )
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
