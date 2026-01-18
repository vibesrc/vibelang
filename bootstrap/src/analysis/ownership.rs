//! Ownership tracking for Vibelang
//!
//! Tracks which variables have been moved and enforces ownership rules:
//! - Values are moved by default (unless Copy type)
//! - Cannot use a value after it has been moved
//! - Reassignment restores ownership

use std::collections::HashMap;
use crate::lexer::Span;
use super::errors::SemanticError;

/// Tracks moved variables during analysis
#[derive(Debug, Clone, Default)]
pub struct OwnershipTracker {
    /// Variables that have been moved: name -> span where moved
    moved_vars: HashMap<String, Span>,
}

impl OwnershipTracker {
    /// Create a new ownership tracker
    pub fn new() -> Self {
        Self::default()
    }

    /// Mark a variable as moved
    pub fn mark_moved(&mut self, name: &str, span: Span) {
        self.moved_vars.insert(name.to_string(), span);
    }

    /// Check if a variable has been moved
    ///
    /// Returns the span where it was moved, if any.
    pub fn is_moved(&self, name: &str) -> Option<Span> {
        self.moved_vars.get(name).copied()
    }

    /// Restore ownership (e.g., on reassignment)
    pub fn restore(&mut self, name: &str) {
        self.moved_vars.remove(name);
    }

    /// Check usage of a potentially moved variable
    ///
    /// Returns an error if the variable has been moved.
    pub fn check_use(&self, name: &str, use_span: Span) -> Result<(), SemanticError> {
        if let Some(move_span) = self.moved_vars.get(name) {
            Err(SemanticError::UseAfterMove {
                name: name.to_string(),
                use_span,
                move_span: *move_span,
            })
        } else {
            Ok(())
        }
    }

    /// Clone the current state (for branch analysis)
    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    /// Restore state from a snapshot
    pub fn restore_from(&mut self, snapshot: OwnershipTracker) {
        *self = snapshot;
    }

    /// Merge two ownership states (for control flow joins)
    ///
    /// After an if/else, a variable is considered moved if it was
    /// moved in either branch.
    pub fn merge(&mut self, other: &OwnershipTracker) {
        for (name, span) in &other.moved_vars {
            self.moved_vars.entry(name.clone()).or_insert(*span);
        }
    }

    /// Clear all moved state
    pub fn clear(&mut self) {
        self.moved_vars.clear();
    }
}

/// Check if a type is a Copy type (implicitly duplicated, not moved)
///
/// Copy types:
/// - All primitives (i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, bool, char)
/// - Raw pointers (*T)
/// - Fixed arrays of Copy types (T[N])
pub fn is_copy_type(ty: &str) -> bool {
    let ty = ty.trim();

    // Primitive types are Copy
    if matches!(
        ty,
        "i8" | "i16" | "i32" | "i64"
        | "u8" | "u16" | "u32" | "u64"
        | "f32" | "f64"
        | "bool"
        | "char"
    ) {
        return true;
    }

    // Raw pointers are Copy
    if ty.starts_with('*') {
        return true;
    }

    // Fixed-size arrays of Copy types are Copy (e.g., "i32[10]")
    if let Some(bracket_pos) = ty.find('[') {
        if ty.ends_with(']') {
            let elem_type = &ty[..bracket_pos];
            return is_copy_type(elem_type);
        }
    }

    // Everything else (String, Vec, structs, enums with owned data) is not Copy
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span { start: 0, end: 10, line: 1, column: 1 }
    }

    fn test_span_2() -> Span {
        Span { start: 20, end: 30, line: 2, column: 1 }
    }

    #[test]
    fn test_move_tracking() {
        let mut tracker = OwnershipTracker::new();

        // Initially not moved
        assert!(tracker.is_moved("x").is_none());
        assert!(tracker.check_use("x", test_span()).is_ok());

        // Mark as moved
        tracker.mark_moved("x", test_span());
        assert!(tracker.is_moved("x").is_some());

        // Using after move should fail
        assert!(tracker.check_use("x", test_span_2()).is_err());

        // Restore ownership
        tracker.restore("x");
        assert!(tracker.is_moved("x").is_none());
        assert!(tracker.check_use("x", test_span_2()).is_ok());
    }

    #[test]
    fn test_merge_ownership() {
        let mut tracker1 = OwnershipTracker::new();
        let mut tracker2 = OwnershipTracker::new();

        // Move x in tracker1 only
        tracker1.mark_moved("x", test_span());

        // Move y in tracker2 only
        tracker2.mark_moved("y", test_span_2());

        // After merge, both should be moved
        tracker1.merge(&tracker2);
        assert!(tracker1.is_moved("x").is_some());
        assert!(tracker1.is_moved("y").is_some());
    }

    #[test]
    fn test_copy_types() {
        // Primitives are Copy
        assert!(is_copy_type("i32"));
        assert!(is_copy_type("u64"));
        assert!(is_copy_type("f64"));
        assert!(is_copy_type("bool"));
        assert!(is_copy_type("char"));

        // Pointers are Copy
        assert!(is_copy_type("*i32"));
        assert!(is_copy_type("*MyStruct"));

        // Arrays of Copy types are Copy
        assert!(is_copy_type("i32[10]"));

        // Non-Copy types
        assert!(!is_copy_type("String"));
        assert!(!is_copy_type("Vec<i32>"));
        assert!(!is_copy_type("MyStruct"));
    }
}
