//! Symbol table types for semantic analysis
//!
//! These types represent the semantic information about a program's symbols
//! (functions, structs, enums, variables, etc.) and are shared between
//! the LSP and codegen.

use std::collections::HashMap;
use crate::lexer::Span;

/// Symbol table containing all semantic information about a program
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

/// Information about a function
#[derive(Debug, Clone)]
pub struct FunctionInfo {
    /// Function name
    pub name: String,
    /// Parameters: (name, type_string)
    pub params: Vec<(String, String)>,
    /// Return type (None for void)
    pub return_type: Option<String>,
    /// Generic type parameters
    pub generics: Vec<String>,
    /// Location in source
    pub span: Span,
    /// Whether the function is public
    pub is_pub: bool,
}

/// Information about a struct
#[derive(Debug, Clone)]
pub struct StructInfo {
    /// Struct name
    pub name: String,
    /// Fields: (name, type_string, is_public)
    pub fields: Vec<(String, String, bool)>,
    /// Generic type parameters
    pub generics: Vec<String>,
    /// Location in source
    pub span: Span,
    /// Whether the struct is public
    pub is_pub: bool,
}

/// Information about an enum
#[derive(Debug, Clone)]
pub struct EnumInfo {
    /// Enum name
    pub name: String,
    /// Variants
    pub variants: Vec<VariantData>,
    /// Generic type parameters
    pub generics: Vec<String>,
    /// Location in source
    pub span: Span,
    /// Whether the enum is public
    pub is_pub: bool,
}

/// Information about an enum variant
#[derive(Debug, Clone)]
pub struct VariantData {
    /// Variant name
    pub name: String,
    /// Variant fields
    pub fields: VariantFieldsData,
    /// Location in source
    pub span: Span,
}

/// Enum variant field data
#[derive(Debug, Clone)]
pub enum VariantFieldsData {
    /// Unit variant (no data)
    Unit,
    /// Tuple variant (positional fields)
    Tuple(Vec<String>),
    /// Struct variant (named fields)
    Struct(Vec<(String, String)>),
}

/// Information about a variable
#[derive(Debug, Clone)]
pub struct VariableInfo {
    /// Variable name
    pub name: String,
    /// Type (None if not yet inferred)
    pub ty: Option<String>,
    /// Location in source
    pub span: Span,
    /// Start of the scope where this variable is valid
    pub scope_start: usize,
    /// End of the scope where this variable is valid
    pub scope_end: usize,
    /// Current borrow state (for LSP diagnostics)
    pub borrow_state: super::borrow::BorrowState,
}

/// Information about a method
#[derive(Debug, Clone)]
pub struct MethodInfo {
    /// Method name
    pub name: String,
    /// Parameters: (name, type_string)
    pub params: Vec<(String, String)>,
    /// Return type (None for void)
    pub return_type: Option<String>,
    /// Location in source
    pub span: Span,
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

/// The kind of imported item
#[derive(Debug, Clone)]
pub enum ImportedItemKind {
    /// Imported struct
    Struct(StructInfo),
    /// Imported enum
    Enum(EnumInfo),
    /// Imported function
    Function(FunctionInfo),
}

impl SymbolTable {
    /// Create a new empty symbol table
    pub fn new() -> Self {
        Self::default()
    }

    /// Find a variable by name that is in scope at the given position
    pub fn find_variable(&self, name: &str, position: usize) -> Option<&VariableInfo> {
        self.variables.iter().find(|v| {
            v.name == name && position >= v.scope_start && position <= v.scope_end
        })
    }

    /// Find all variables in scope at the given position
    pub fn variables_in_scope(&self, position: usize) -> Vec<&VariableInfo> {
        self.variables.iter().filter(|v| {
            position >= v.scope_start && position <= v.scope_end
        }).collect()
    }

    /// Check if a type name is defined (struct, enum, or generic parameter)
    pub fn type_exists(&self, name: &str, type_params: &[String]) -> bool {
        type_params.contains(&name.to_string())
            || self.structs.contains_key(name)
            || self.enums.contains_key(name)
    }
}
