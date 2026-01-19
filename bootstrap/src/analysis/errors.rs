//! Semantic error types for the Vibelang compiler
//!
//! These errors are produced during semantic analysis (borrow checking,
//! type checking, ownership tracking) and used by both the LSP and codegen.

use crate::lexer::Span;

/// A semantic error detected during analysis
#[derive(Debug, Clone)]
pub enum SemanticError {
    /// Use of a value after it has been moved
    UseAfterMove {
        name: String,
        use_span: Span,
        move_span: Span,
    },

    /// Conflicting borrow (e.g., mutable borrow while already borrowed)
    BorrowConflict {
        name: String,
        existing: BorrowKind,
        requested: BorrowKind,
        existing_span: Span,
        request_span: Span,
    },

    /// Attempting to mutate through a shared reference
    MutationThroughSharedRef {
        name: String,
        span: Span,
    },

    /// Borrow of a moved value
    BorrowOfMovedValue {
        name: String,
        borrow_span: Span,
        move_span: Span,
    },

    /// Variable not found
    UndefinedVariable {
        name: String,
        span: Span,
    },

    /// Type not found
    UndefinedType {
        name: String,
        span: Span,
    },

    /// Function not found
    UndefinedFunction {
        name: String,
        span: Span,
    },

    /// Struct field not found
    UndefinedField {
        struct_name: String,
        field_name: String,
        span: Span,
    },

    /// Type mismatch
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    /// Wrong number of arguments to function
    ArgumentCountMismatch {
        function: String,
        expected: usize,
        found: usize,
        span: Span,
    },

    /// Missing field in struct initialization
    MissingField {
        struct_name: String,
        field_name: String,
        span: Span,
    },

    /// Unknown field in struct initialization
    UnknownField {
        struct_name: String,
        field_name: String,
        span: Span,
    },

    /// Duplicate import
    DuplicateImport {
        name: String,
        span: Span,
    },

    /// Operation requires unsafe block
    UnsafeRequired {
        operation: String,
        span: Span,
    },

    /// Type does not implement required trait bound
    TraitBoundNotSatisfied {
        type_name: String,
        trait_name: String,
        span: Span,
    },

    /// Trait implementation is missing required method
    MissingTraitMethod {
        trait_name: String,
        method_name: String,
        type_name: String,
        span: Span,
    },
}

/// The kind of borrow (for error messages)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorrowKind {
    Shared,
    Mutable,
}

impl std::fmt::Display for BorrowKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BorrowKind::Shared => write!(f, "immutable"),
            BorrowKind::Mutable => write!(f, "mutable"),
        }
    }
}

impl SemanticError {
    /// Get the span where the error occurred
    pub fn span(&self) -> Span {
        match self {
            SemanticError::UseAfterMove { use_span, .. } => *use_span,
            SemanticError::BorrowConflict { request_span, .. } => *request_span,
            SemanticError::MutationThroughSharedRef { span, .. } => *span,
            SemanticError::BorrowOfMovedValue { borrow_span, .. } => *borrow_span,
            SemanticError::UndefinedVariable { span, .. } => *span,
            SemanticError::UndefinedType { span, .. } => *span,
            SemanticError::UndefinedFunction { span, .. } => *span,
            SemanticError::UndefinedField { span, .. } => *span,
            SemanticError::TypeMismatch { span, .. } => *span,
            SemanticError::ArgumentCountMismatch { span, .. } => *span,
            SemanticError::MissingField { span, .. } => *span,
            SemanticError::UnknownField { span, .. } => *span,
            SemanticError::DuplicateImport { span, .. } => *span,
            SemanticError::UnsafeRequired { span, .. } => *span,
            SemanticError::TraitBoundNotSatisfied { span, .. } => *span,
            SemanticError::MissingTraitMethod { span, .. } => *span,
        }
    }

    /// Get a human-readable error message
    pub fn message(&self) -> String {
        match self {
            SemanticError::UseAfterMove { name, move_span, .. } => {
                format!(
                    "use of moved value '{}' (moved at line {})",
                    name, move_span.line
                )
            }
            SemanticError::BorrowConflict {
                name,
                existing,
                requested,
                existing_span,
                ..
            } => {
                format!(
                    "cannot borrow '{}' as {} because it is already borrowed as {} (at line {})",
                    name, requested, existing, existing_span.line
                )
            }
            SemanticError::MutationThroughSharedRef { name, .. } => {
                format!(
                    "cannot mutate through read-only borrow '&{}': use '~{}' for mutable access",
                    name, name
                )
            }
            SemanticError::BorrowOfMovedValue { name, move_span, .. } => {
                format!(
                    "cannot borrow '{}' because it has been moved (at line {})",
                    name, move_span.line
                )
            }
            SemanticError::UndefinedVariable { name, .. } => {
                format!("unknown variable '{}'", name)
            }
            SemanticError::UndefinedType { name, .. } => {
                format!("unknown type '{}'", name)
            }
            SemanticError::UndefinedFunction { name, .. } => {
                format!("unknown function '{}'", name)
            }
            SemanticError::UndefinedField { struct_name, field_name, .. } => {
                format!("unknown field '{}' on struct '{}'", field_name, struct_name)
            }
            SemanticError::TypeMismatch { expected, found, .. } => {
                format!("type mismatch: expected '{}', found '{}'", expected, found)
            }
            SemanticError::ArgumentCountMismatch { function, expected, found, .. } => {
                format!(
                    "function '{}' expects {} argument{}, got {}",
                    function,
                    expected,
                    if *expected == 1 { "" } else { "s" },
                    found
                )
            }
            SemanticError::MissingField { struct_name, field_name, .. } => {
                format!("missing field '{}' in struct '{}'", field_name, struct_name)
            }
            SemanticError::UnknownField { struct_name, field_name, .. } => {
                format!("unknown field '{}' in struct '{}'", field_name, struct_name)
            }
            SemanticError::DuplicateImport { name, .. } => {
                format!("duplicate import '{}'", name)
            }
            SemanticError::UnsafeRequired { operation, .. } => {
                format!("'{}' requires unsafe block", operation)
            }
            SemanticError::TraitBoundNotSatisfied { type_name, trait_name, .. } => {
                format!(
                    "type '{}' does not implement trait '{}'",
                    type_name, trait_name
                )
            }
            SemanticError::MissingTraitMethod { trait_name, method_name, type_name, .. } => {
                format!(
                    "impl of trait '{}' for '{}' is missing method '{}'",
                    trait_name, type_name, method_name
                )
            }
        }
    }
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl std::error::Error for SemanticError {}
