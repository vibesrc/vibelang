//! Type definitions for the Vibelang LSP

use std::collections::HashMap;
use tower_lsp_server::ls_types::Diagnostic;
use crate::ast::Program;
use crate::lexer::Span;

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

/// Symbol table for a document
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    /// Functions: name -> info
    pub functions: HashMap<String, FunctionInfo>,
    /// Structs: name -> info
    pub structs: HashMap<String, StructInfo>,
    /// Enums: name -> info
    pub enums: HashMap<String, EnumInfo>,
    /// Variables in scope at different positions
    pub variables: Vec<VariableInfo>,
    /// Methods: type_name -> [method_info]
    pub methods: HashMap<String, Vec<MethodInfo>>,
    /// Imported items: item_name -> ImportedItem
    pub imports: HashMap<String, ImportedItem>,
    /// Module namespace aliases: alias_name -> module symbol table
    /// e.g., `use std.fs` makes "fs" -> SymbolTable of fs module
    pub module_aliases: HashMap<String, Box<SymbolTable>>,
}

/// Information about an imported item
#[derive(Debug, Clone)]
pub struct ImportedItem {
    /// The original name of the item
    pub name: String,
    /// The alias (if any)
    pub alias: Option<String>,
    /// The module path it was imported from (e.g., "std.types")
    pub module_path: String,
    /// The kind of item (struct, enum, fn)
    pub kind: ImportedItemKind,
}

#[derive(Debug, Clone)]
pub enum ImportedItemKind {
    Struct(StructInfo),
    Enum(EnumInfo),
    Function(FunctionInfo),
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub return_type: Option<String>,
    pub generics: Vec<String>,
    pub span: Span,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<(String, String, bool)>,
    pub generics: Vec<String>,
    pub span: Span,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub variants: Vec<VariantData>,
    pub generics: Vec<String>,
    pub span: Span,
    pub is_pub: bool,
}

#[derive(Debug, Clone)]
pub struct VariantData {
    pub name: String,
    pub fields: VariantFieldsData,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantFieldsData {
    Unit,
    Tuple(Vec<String>),
    Struct(Vec<(String, String)>),
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub ty: Option<String>,
    pub span: Span,
    pub scope_start: usize,
    pub scope_end: usize,
    pub borrow_state: BorrowState,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowState {
    Owned,
    Borrowed,
    MutBorrowed,
    Moved,
}

#[derive(Debug, Clone)]
pub struct MethodInfo {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub return_type: Option<String>,
    pub span: Span,
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
