//! Vibelang bootstrap compiler library
//!
//! This library provides the core components for the Vibelang compiler:
//! - Lexer: Tokenizes source code
//! - Parser: Builds AST from tokens
//! - AST: Type definitions for the abstract syntax tree
//! - Analysis: Shared semantic analysis (borrow checking, ownership, types)
//! - Codegen: LLVM code generation (requires inkwell)

pub mod ast;
pub mod lexer;
pub mod parser;

// Shared semantic analysis module
pub mod analysis;

// Codegen requires inkwell - only available when codegen feature is enabled
#[cfg(all(feature = "codegen"))]
pub mod codegen;

// LSP module - provides language server protocol implementation
pub mod lsp;
