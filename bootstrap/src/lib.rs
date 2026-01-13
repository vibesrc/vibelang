//! Vibelang bootstrap compiler library
//!
//! This library provides the core components for the Vibelang compiler:
//! - Lexer: Tokenizes source code
//! - Parser: Builds AST from tokens
//! - AST: Type definitions for the abstract syntax tree
//! - Codegen: LLVM code generation (requires inkwell)

pub mod ast;
pub mod lexer;
pub mod parser;

// Codegen requires inkwell - only available when codegen feature is enabled
#[cfg(all(feature = "codegen"))]
pub mod codegen;
