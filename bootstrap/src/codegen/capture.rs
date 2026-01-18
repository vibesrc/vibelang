//! Closure capture analysis
//!
//! This module analyzes closure bodies to find free variables (variables that
//! are used but not defined within the closure).

use crate::ast::*;
use std::collections::HashSet;

/// Represents a captured variable
#[derive(Debug, Clone)]
pub struct CapturedVar {
    pub name: String,
    /// True if the variable is mutated inside the closure
    pub is_mutated: bool,
}

/// Collect free variables from a closure body.
/// `params` are the closure parameters (bound variables).
/// Returns the list of captured variables (used but not locally defined).
pub fn collect_free_variables(
    params: &[(String, Option<Type>)],
    body: &ClosureBody,
) -> Vec<CapturedVar> {
    let mut bound: HashSet<String> = params.iter().map(|(name, _)| name.clone()).collect();
    let mut captures: Vec<CapturedVar> = Vec::new();
    let mut capture_names: HashSet<String> = HashSet::new();

    match body {
        ClosureBody::Expr(expr) => {
            collect_free_in_expr(expr, &bound, &mut captures, &mut capture_names);
        }
        ClosureBody::Block(block) => {
            collect_free_in_block(block, &mut bound, &mut captures, &mut capture_names);
        }
    }

    captures
}

/// Collect free variables in an expression
fn collect_free_in_expr(
    expr: &Expr,
    bound: &HashSet<String>,
    captures: &mut Vec<CapturedVar>,
    capture_names: &mut HashSet<String>,
) {
    match expr {
        Expr::Ident(name, _) => {
            if !bound.contains(name) && !capture_names.contains(name) {
                captures.push(CapturedVar {
                    name: name.clone(),
                    is_mutated: false,
                });
                capture_names.insert(name.clone());
            }
        }
        Expr::Literal(_, _) => {}
        Expr::Binary { op, left, right, .. } => {
            // Check if this is an assignment to a captured variable
            if matches!(op, BinOp::Assign) {
                if let Expr::Ident(name, _) = left.as_ref() {
                    if !bound.contains(name) {
                        // This is a mutation of a captured variable
                        if let Some(cap) = captures.iter_mut().find(|c| c.name == *name) {
                            cap.is_mutated = true;
                        } else if !capture_names.contains(name) {
                            captures.push(CapturedVar {
                                name: name.clone(),
                                is_mutated: true,
                            });
                            capture_names.insert(name.clone());
                        }
                    }
                }
            }
            collect_free_in_expr(left, bound, captures, capture_names);
            collect_free_in_expr(right, bound, captures, capture_names);
        }
        Expr::Unary { operand, .. } => {
            collect_free_in_expr(operand, bound, captures, capture_names);
        }
        Expr::Call { func, args, .. } => {
            collect_free_in_expr(func, bound, captures, capture_names);
            for arg in args {
                collect_free_in_expr(arg, bound, captures, capture_names);
            }
        }
        Expr::MethodCall { receiver, args, .. } => {
            collect_free_in_expr(receiver, bound, captures, capture_names);
            for arg in args {
                collect_free_in_expr(arg, bound, captures, capture_names);
            }
        }
        Expr::Field { object, .. } => {
            collect_free_in_expr(object, bound, captures, capture_names);
        }
        Expr::Index { array, index, .. } => {
            collect_free_in_expr(array, bound, captures, capture_names);
            collect_free_in_expr(index, bound, captures, capture_names);
        }
        Expr::StructInit { fields, .. } => {
            for (_, expr) in fields {
                collect_free_in_expr(expr, bound, captures, capture_names);
            }
        }
        Expr::ArrayInit { elements, .. } => {
            for elem in elements {
                collect_free_in_expr(elem, bound, captures, capture_names);
            }
        }
        Expr::ArrayRepeat { value, .. } => {
            collect_free_in_expr(value, bound, captures, capture_names);
        }
        Expr::Ref { operand, .. } | Expr::RefMut { operand, .. } | Expr::Deref { operand, .. } => {
            collect_free_in_expr(operand, bound, captures, capture_names);
        }
        Expr::Cast { expr, .. } => {
            collect_free_in_expr(expr, bound, captures, capture_names);
        }
        Expr::Try { operand, .. } => {
            collect_free_in_expr(operand, bound, captures, capture_names);
        }
        Expr::Tuple { elements, .. } => {
            for elem in elements {
                collect_free_in_expr(elem, bound, captures, capture_names);
            }
        }
        Expr::Block(block) => {
            let mut inner_bound = bound.clone();
            collect_free_in_block(block, &mut inner_bound, captures, capture_names);
        }
        Expr::Unsafe { block, .. } => {
            let mut inner_bound = bound.clone();
            collect_free_in_block(block, &mut inner_bound, captures, capture_names);
        }
        Expr::Closure { params, body, .. } => {
            // Nested closure - its params shadow outer scope
            let mut inner_bound = bound.clone();
            for (name, _) in params {
                inner_bound.insert(name.clone());
            }
            match body {
                ClosureBody::Expr(e) => {
                    collect_free_in_expr(e, &inner_bound, captures, capture_names);
                }
                ClosureBody::Block(b) => {
                    collect_free_in_block(b, &mut inner_bound, captures, capture_names);
                }
            }
        }
        Expr::InterpolatedString { parts, .. } => {
            for part in parts {
                if let StringPart::Expr(e) = part {
                    collect_free_in_expr(e, bound, captures, capture_names);
                }
            }
        }
        Expr::If { condition, then_expr, else_expr, .. } => {
            collect_free_in_expr(condition, bound, captures, capture_names);
            collect_free_in_expr(then_expr, bound, captures, capture_names);
            collect_free_in_expr(else_expr, bound, captures, capture_names);
        }
        Expr::Range { start, end, .. } => {
            collect_free_in_expr(start, bound, captures, capture_names);
            collect_free_in_expr(end, bound, captures, capture_names);
        }
    }
}

/// Collect free variables in a block, updating bound variables as we go
fn collect_free_in_block(
    block: &Block,
    bound: &mut HashSet<String>,
    captures: &mut Vec<CapturedVar>,
    capture_names: &mut HashSet<String>,
) {
    for stmt in &block.stmts {
        collect_free_in_stmt(stmt, bound, captures, capture_names);
    }
}

/// Collect free variables in a statement
fn collect_free_in_stmt(
    stmt: &Stmt,
    bound: &mut HashSet<String>,
    captures: &mut Vec<CapturedVar>,
    capture_names: &mut HashSet<String>,
) {
    match stmt {
        Stmt::Let { name, value, .. } => {
            // First collect from the value (before binding)
            collect_free_in_expr(value, bound, captures, capture_names);
            // Then add the binding
            bound.insert(name.clone());
        }
        Stmt::LetPattern { pattern, value, .. } => {
            // First collect from the value (before binding)
            collect_free_in_expr(value, bound, captures, capture_names);
            // Then add the pattern bindings
            collect_pattern_bindings(pattern, bound);
        }
        Stmt::Expr(expr) => {
            collect_free_in_expr(expr, bound, captures, capture_names);
        }
        Stmt::Return { value, .. } => {
            if let Some(expr) = value {
                collect_free_in_expr(expr, bound, captures, capture_names);
            }
        }
        Stmt::Defer { expr, .. } => {
            collect_free_in_expr(expr, bound, captures, capture_names);
        }
        Stmt::If { condition, then_block, else_block, .. } => {
            collect_free_in_expr(condition, bound, captures, capture_names);
            let mut then_bound = bound.clone();
            collect_free_in_block(then_block, &mut then_bound, captures, capture_names);
            if let Some(else_blk) = else_block {
                let mut else_bound = bound.clone();
                collect_free_in_block(else_blk, &mut else_bound, captures, capture_names);
            }
        }
        Stmt::While { condition, body, .. } => {
            collect_free_in_expr(condition, bound, captures, capture_names);
            let mut loop_bound = bound.clone();
            collect_free_in_block(body, &mut loop_bound, captures, capture_names);
        }
        Stmt::For { name, iter, body, .. } => {
            collect_free_in_expr(iter, bound, captures, capture_names);
            let mut for_bound = bound.clone();
            for_bound.insert(name.clone());
            collect_free_in_block(body, &mut for_bound, captures, capture_names);
        }
        Stmt::Match { value, arms, .. } => {
            collect_free_in_expr(value, bound, captures, capture_names);
            for arm in arms {
                let mut arm_bound = bound.clone();
                collect_pattern_bindings(&arm.pattern, &mut arm_bound);
                collect_free_in_expr(&arm.body, &arm_bound, captures, capture_names);
            }
        }
        Stmt::Break { .. } | Stmt::Continue { .. } => {}
    }
}

/// Extract variable bindings from a pattern
fn collect_pattern_bindings(pattern: &Pattern, bound: &mut HashSet<String>) {
    match pattern {
        Pattern::Ident(name) => {
            bound.insert(name.clone());
        }
        Pattern::Wildcard | Pattern::Literal(_) => {}
        Pattern::Tuple(patterns) => {
            for p in patterns {
                collect_pattern_bindings(p, bound);
            }
        }
        Pattern::Struct { fields, .. } => {
            for (_, pat) in fields {
                collect_pattern_bindings(pat, bound);
            }
        }
        Pattern::Enum { fields, .. } => {
            for p in fields {
                collect_pattern_bindings(p, bound);
            }
        }
    }
}
