//! Borrow checking for Vibelang
//!
//! Tracks borrow state of variables and enforces borrow rules:
//! - A value can have multiple shared borrows (&T)
//! - A value can have exactly one mutable borrow (~T)
//! - Cannot have shared and mutable borrows simultaneously
//! - Cannot borrow a moved value

use std::collections::HashMap;
use crate::lexer::Span;
use super::errors::{SemanticError, BorrowKind};

/// The borrow state of a variable
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BorrowState {
    /// No active borrows
    Owned,
    /// Has one or more shared (&T) borrows
    Borrowed,
    /// Has a mutable (~T) borrow
    MutBorrowed,
    /// Value has been moved (for LSP tracking)
    Moved,
}

impl Default for BorrowState {
    fn default() -> Self {
        BorrowState::Owned
    }
}

/// Tracks active borrows during analysis
#[derive(Debug, Clone, Default)]
pub struct BorrowChecker {
    /// Active borrows: variable name -> (borrow state, span where borrowed)
    borrowed_vars: HashMap<String, (BorrowState, Span)>,
}

impl BorrowChecker {
    /// Create a new borrow checker
    pub fn new() -> Self {
        Self::default()
    }

    /// Attempt to create a borrow of a variable
    ///
    /// Returns an error if the borrow would violate borrow rules.
    pub fn try_borrow(
        &mut self,
        name: &str,
        mutable: bool,
        span: Span,
    ) -> Result<(), SemanticError> {
        if let Some((current_state, existing_span)) = self.borrowed_vars.get(name) {
            match (current_state, mutable) {
                (BorrowState::MutBorrowed, _) => {
                    // Cannot borrow (mutable or shared) if already mutably borrowed
                    return Err(SemanticError::BorrowConflict {
                        name: name.to_string(),
                        existing: BorrowKind::Mutable,
                        requested: if mutable { BorrowKind::Mutable } else { BorrowKind::Shared },
                        existing_span: *existing_span,
                        request_span: span,
                    });
                }
                (BorrowState::Borrowed, true) => {
                    // Cannot mutably borrow if already shared borrowed
                    return Err(SemanticError::BorrowConflict {
                        name: name.to_string(),
                        existing: BorrowKind::Shared,
                        requested: BorrowKind::Mutable,
                        existing_span: *existing_span,
                        request_span: span,
                    });
                }
                (BorrowState::Borrowed, false) => {
                    // Multiple shared borrows are OK - nothing to do
                }
                (BorrowState::Moved, _) => {
                    // This shouldn't happen through try_borrow; move checking is separate
                    // But handle it just in case
                    return Err(SemanticError::BorrowOfMovedValue {
                        name: name.to_string(),
                        borrow_span: span,
                        move_span: *existing_span,
                    });
                }
                (BorrowState::Owned, _) => {
                    // Variable was marked owned in map, which shouldn't happen
                    // but we'll allow the borrow
                }
            }
        }

        // Record the borrow
        let new_state = if mutable {
            BorrowState::MutBorrowed
        } else {
            BorrowState::Borrowed
        };
        self.borrowed_vars.insert(name.to_string(), (new_state, span));
        Ok(())
    }

    /// Release all borrows (e.g., at end of scope or expression)
    pub fn release_borrows(&mut self) {
        self.borrowed_vars.clear();
    }

    /// Release borrow of a specific variable
    pub fn release_borrow(&mut self, name: &str) {
        self.borrowed_vars.remove(name);
    }

    /// Check if a variable is currently mutably borrowed
    pub fn is_mutably_borrowed(&self, name: &str) -> bool {
        matches!(
            self.borrowed_vars.get(name),
            Some((BorrowState::MutBorrowed, _))
        )
    }

    /// Check if a variable is currently borrowed (shared or mutable)
    pub fn is_borrowed(&self, name: &str) -> bool {
        matches!(
            self.borrowed_vars.get(name),
            Some((BorrowState::Borrowed | BorrowState::MutBorrowed, _))
        )
    }

    /// Get the current borrow state of a variable
    pub fn get_state(&self, name: &str) -> BorrowState {
        self.borrowed_vars
            .get(name)
            .map(|(state, _)| *state)
            .unwrap_or(BorrowState::Owned)
    }

    /// Get the span where a variable was borrowed (if any)
    pub fn get_borrow_span(&self, name: &str) -> Option<Span> {
        self.borrowed_vars.get(name).map(|(_, span)| *span)
    }

    /// Clone the current state (for branch analysis)
    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    /// Restore state from a snapshot
    pub fn restore(&mut self, snapshot: BorrowChecker) {
        *self = snapshot;
    }

    /// Merge two borrow states (for control flow joins)
    ///
    /// After an if/else, a variable is considered borrowed if it was
    /// borrowed in either branch.
    pub fn merge(&mut self, other: &BorrowChecker) {
        for (name, (state, span)) in &other.borrowed_vars {
            if let Some((existing_state, existing_span)) = self.borrowed_vars.get(name) {
                // Take the more restrictive state
                let merged_state = match (existing_state, state) {
                    (BorrowState::MutBorrowed, _) | (_, BorrowState::MutBorrowed) => {
                        BorrowState::MutBorrowed
                    }
                    (BorrowState::Borrowed, _) | (_, BorrowState::Borrowed) => {
                        BorrowState::Borrowed
                    }
                    (BorrowState::Moved, _) | (_, BorrowState::Moved) => {
                        BorrowState::Moved
                    }
                    _ => BorrowState::Owned,
                };
                // Keep the earlier span
                let merged_span = if existing_span.start < span.start {
                    *existing_span
                } else {
                    *span
                };
                self.borrowed_vars.insert(name.clone(), (merged_state, merged_span));
            } else {
                self.borrowed_vars.insert(name.clone(), (*state, *span));
            }
        }
    }
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
    fn test_shared_borrows() {
        let mut checker = BorrowChecker::new();

        // First shared borrow should succeed
        assert!(checker.try_borrow("x", false, test_span()).is_ok());

        // Second shared borrow should also succeed
        assert!(checker.try_borrow("x", false, test_span_2()).is_ok());
    }

    #[test]
    fn test_mutable_borrow_conflict() {
        let mut checker = BorrowChecker::new();

        // Mutable borrow should succeed
        assert!(checker.try_borrow("x", true, test_span()).is_ok());

        // Second mutable borrow should fail
        assert!(checker.try_borrow("x", true, test_span_2()).is_err());
    }

    #[test]
    fn test_shared_after_mutable() {
        let mut checker = BorrowChecker::new();

        // Mutable borrow first
        assert!(checker.try_borrow("x", true, test_span()).is_ok());

        // Shared borrow should fail
        assert!(checker.try_borrow("x", false, test_span_2()).is_err());
    }

    #[test]
    fn test_mutable_after_shared() {
        let mut checker = BorrowChecker::new();

        // Shared borrow first
        assert!(checker.try_borrow("x", false, test_span()).is_ok());

        // Mutable borrow should fail
        assert!(checker.try_borrow("x", true, test_span_2()).is_err());
    }

    #[test]
    fn test_release_borrows() {
        let mut checker = BorrowChecker::new();

        // Create a mutable borrow
        assert!(checker.try_borrow("x", true, test_span()).is_ok());
        assert!(checker.is_mutably_borrowed("x"));

        // Release borrows
        checker.release_borrows();
        assert!(!checker.is_mutably_borrowed("x"));

        // Now we can borrow again
        assert!(checker.try_borrow("x", true, test_span_2()).is_ok());
    }
}
