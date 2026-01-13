//! Vibelang Language Server Protocol implementation
//!
//! Provides IDE features for Vibelang:
//! - Diagnostics (parse errors, type errors, borrow errors)
//! - Completion (struct fields, methods, variables, functions, types)
//! - Hover (types, signatures, borrow state)
//! - Go to definition
//! - Semantic tokens for syntax highlighting

pub mod types;
pub mod utils;
pub mod backend;
pub mod analysis;
pub mod completion;
pub mod hover;
pub mod signature;
pub mod semantic_tokens;
pub mod handlers;

pub use backend::Backend;
pub use types::*;
pub use utils::semantic_token_legend;
