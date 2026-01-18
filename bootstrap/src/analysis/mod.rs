//! Semantic analysis module for Vibelang
//!
//! This module provides shared semantic analysis functionality used by both
//! the LSP (for IDE features) and codegen (for compilation). Having a single
//! source of truth for semantic rules ensures consistency.
//!
//! # Components
//!
//! - [`analyzer`]: The main `SemanticAnalyzer` that walks the AST
//! - [`symbols`]: Symbol table types (functions, structs, enums, variables)
//! - [`borrow`]: Borrow checking (tracking &T and ~T borrows)
//! - [`ownership`]: Ownership tracking (moves and Copy types)
//! - [`types`]: Type validation and compatibility
//! - [`errors`]: Semantic error types
//!
//! # Example
//!
//! ```ignore
//! use vibelang::analysis::{analyze, SemanticAnalyzer};
//!
//! // Analyze a program
//! let result = analyze(&program);
//!
//! // Check for errors
//! if !result.errors.is_empty() {
//!     for error in &result.errors {
//!         eprintln!("Error: {}", error);
//!     }
//! }
//!
//! // Use the symbol table
//! let symbols = result.symbols;
//! ```

pub mod analyzer;
pub mod borrow;
pub mod errors;
pub mod ownership;
pub mod symbols;
pub mod types;

// Re-export commonly used types at the module level
pub use analyzer::{analyze, AnalysisResult, SemanticAnalyzer};
pub use borrow::{BorrowChecker, BorrowState};
pub use errors::{BorrowKind, SemanticError};
pub use ownership::{is_copy_type, OwnershipTracker};
pub use symbols::{
    EnumInfo, FunctionInfo, ImportedItem, ImportedItemKind, MethodInfo, StructInfo, SymbolTable,
    VariableInfo, VariantData, VariantFieldsData,
};
pub use types::{is_builtin_function, is_builtin_type, is_prelude_type, TypeResolver};
