//! Type definitions for the Vibelang LSP
//!
//! This module re-exports shared types from the analysis module and adds
//! LSP-specific types like DocumentInfo and RawSemanticToken.

use tower_lsp_server::ls_types::Diagnostic;
use crate::ast::Program;

// Re-export shared types from analysis module
pub use crate::analysis::{
    BorrowState, EnumInfo, FunctionInfo, ImportedItem, ImportedItemKind, MethodInfo,
    StructInfo, SymbolTable, VariableInfo, VariantData, VariantFieldsData,
};

/// Represents a parsed document with semantic information
#[derive(Debug, Clone)]
pub struct DocumentInfo {
    /// The source text
    pub text: String,
    /// Parsed AST (if successful)
    pub ast: Option<Program>,
    /// Collected symbols for completion and goto
    pub symbols: SymbolTable,
    /// Collected diagnostics
    pub diagnostics: Vec<Diagnostic>,
}

/// A raw semantic token before delta encoding
#[derive(Debug, Clone)]
pub struct RawSemanticToken {
    pub line: u32,
    pub start: u32,
    pub length: u32,
    pub token_type: u32,
    pub modifiers: u32,
}
