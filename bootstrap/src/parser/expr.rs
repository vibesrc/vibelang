//! Expression parsing with operator precedence

use super::{ParseError, Parser};
use crate::ast::*;
use crate::lexer::{Keyword, Span, Token, TokenKind};

impl Parser {
    pub(crate) fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_or()?;

        if let Some(compound_op) = self.match_assign_op() {
            let right = self.parse_assignment()?;
            let span = self.span_from(expr.span());

            // If compound assignment, desugar: x += y -> x = x + y
            let final_right = if let Some(op) = compound_op {
                Box::new(Expr::Binary {
                    op,
                    left: Box::new(expr.clone()),
                    right: Box::new(right),
                    span,
                })
            } else {
                Box::new(right)
            };

            return Ok(Expr::Binary {
                op: BinOp::Assign,
                left: Box::new(expr),
                right: final_right,
                span,
            });
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_and()?;

        while self.match_keyword(Keyword::Or) {
            let right = self.parse_and()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op: BinOp::Or,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_range()?;

        while self.match_keyword(Keyword::And) {
            let right = self.parse_range()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op: BinOp::And,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_range(&mut self) -> Result<Expr, ParseError> {
        let left = self.parse_comparison()?;

        // Check for range operator ..
        if self.match_token(TokenKind::DotDot) {
            let right = self.parse_comparison()?;
            let span = self.span_from(left.span());
            return Ok(Expr::Range {
                start: Box::new(left),
                end: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_bitwise_or()?;

        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::EqEq) => BinOp::Eq,
                Some(TokenKind::BangEq) => BinOp::Ne,
                Some(TokenKind::Lt) => BinOp::Lt,
                Some(TokenKind::LtEq) => BinOp::Le,
                Some(TokenKind::Gt) => BinOp::Gt,
                Some(TokenKind::GtEq) => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_bitwise_or()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_bitwise_xor()?;

        while self.match_token(TokenKind::Pipe) {
            let right = self.parse_bitwise_xor()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op: BinOp::BitOr,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_bitwise_and()?;

        while self.match_token(TokenKind::Caret) {
            let right = self.parse_bitwise_and()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op: BinOp::BitXor,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_term()?;

        // Note: & is ambiguous with borrow - only treat as bitwise in binary context
        while self.check(TokenKind::Amp) {
            // Look ahead to disambiguate
            let next = self.tokens.get(self.pos + 1);
            if matches!(next.map(|t| &t.kind), Some(TokenKind::Ident(_)) | Some(TokenKind::Int(_))) {
                // Could be bitwise AND, but let's be conservative
                break;
            }
            self.advance();
            let right = self.parse_term()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op: BinOp::BitAnd,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;

        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::Plus) => BinOp::Add,
                Some(TokenKind::Minus) => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_factor()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::Star) => BinOp::Mul,
                Some(TokenKind::Slash) => BinOp::Div,
                Some(TokenKind::Percent) => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            let span = self.span_from(left.span());
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let start = self.current_span();

        // Negation
        if self.match_token(TokenKind::Minus) {
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
                span: self.span_from(start),
            });
        }

        // Logical not
        if self.match_keyword(Keyword::Not) {
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                operand: Box::new(operand),
                span: self.span_from(start),
            });
        }

        // Bitwise not
        if self.match_token(TokenKind::Bang) {
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::BitNot,
                operand: Box::new(operand),
                span: self.span_from(start),
            });
        }

        // Borrow
        if self.match_token(TokenKind::Amp) {
            let operand = self.parse_unary()?;
            return Ok(Expr::Ref {
                operand: Box::new(operand),
                span: self.span_from(start),
            });
        }

        // Mutable borrow (vibing)
        if self.match_token(TokenKind::Tilde) {
            let operand = self.parse_unary()?;
            return Ok(Expr::RefMut {
                operand: Box::new(operand),
                span: self.span_from(start),
            });
        }

        // Dereference
        if self.match_token(TokenKind::Star) {
            let operand = self.parse_unary()?;
            return Ok(Expr::Deref {
                operand: Box::new(operand),
                span: self.span_from(start),
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenKind::LParen) {
                // Function call
                let args = self.parse_args()?;
                self.expect(TokenKind::RParen)?;
                let span = self.span_from(expr.span());
                expr = Expr::Call {
                    func: Box::new(expr),
                    type_args: Vec::new(),
                    args,
                    span,
                };
            } else if self.match_token(TokenKind::LBracket) {
                // Index
                let index = self.parse_expr()?;
                self.expect(TokenKind::RBracket)?;
                let span = self.span_from(expr.span());
                expr = Expr::Index {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                };
            } else if self.match_token(TokenKind::Dot) {
                // Field access or method call
                let field = self.expect_ident()?;

                if self.match_token(TokenKind::LParen) {
                    // Method call
                    let args = self.parse_args()?;
                    self.expect(TokenKind::RParen)?;
                    let span = self.span_from(expr.span());
                    expr = Expr::MethodCall {
                        receiver: Box::new(expr),
                        method: field,
                        args,
                        span,
                    };
                } else {
                    // Field access
                    let span = self.span_from(expr.span());
                    expr = Expr::Field {
                        object: Box::new(expr),
                        field,
                        span,
                    };
                }
            } else if self.match_token(TokenKind::Question) {
                // Try operator
                let span = self.span_from(expr.span());
                expr = Expr::Try {
                    operand: Box::new(expr),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let span = self.current_span();

        match self.peek_kind() {
            Some(TokenKind::Int(n)) => {
                let n = *n;
                self.advance();
                Ok(Expr::Literal(Literal::Int(n), span))
            }
            Some(TokenKind::Float(n)) => {
                let n = *n;
                self.advance();
                Ok(Expr::Literal(Literal::Float(n), span))
            }
            Some(TokenKind::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Expr::Literal(Literal::String(s), span))
            }
            Some(TokenKind::InterpolatedStringStart(s)) => {
                self.parse_interpolated_string(s.clone(), span)
            }
            Some(TokenKind::Bool(b)) => {
                let b = *b;
                self.advance();
                Ok(Expr::Literal(Literal::Bool(b), span))
            }
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                self.advance();

                // Check for generic type arguments: name<Type, ...>
                let type_args = if self.check(TokenKind::Lt) {
                    // Look ahead to see if this is really generics or a comparison
                    // Generics: name<Type>(...)  or  Name<Type> { ... }
                    // Comparison: name < expr
                    if self.is_generic_args() {
                        self.parse_type_args()?
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                // Check for struct initialization: Name { field: value, ... } or Name<T> { ... }
                if self.check(TokenKind::LBrace) {
                    // Look ahead to see if it's a struct init (has field: value) or a block
                    // Struct init has pattern: { ident : expr, ... }
                    let mut lookahead = self.tokens[self.pos..].iter();
                    lookahead.next(); // skip {
                    let is_struct_init = matches!(
                        (lookahead.next(), lookahead.next()),
                        (Some(Token { kind: TokenKind::Ident(_), .. }), Some(Token { kind: TokenKind::Colon, .. }))
                    );

                    if is_struct_init {
                        return self.parse_struct_init_with_generics(name, type_args, span);
                    }
                }

                // Check for generic function call: name<Type>(...)
                if !type_args.is_empty() && self.check(TokenKind::LParen) {
                    self.advance(); // consume (
                    let args = self.parse_args()?;
                    self.expect(TokenKind::RParen)?;
                    return Ok(Expr::Call {
                        func: Box::new(Expr::Ident(name, span)),
                        type_args,
                        args,
                        span: self.span_from(span),
                    });
                }

                // Check for generic enum variant constructor: EnumName<Type>.Variant(args)
                // or unit variant: EnumName<Type>.Variant
                if !type_args.is_empty() && self.check(TokenKind::Dot) {
                    self.advance(); // consume .
                    let variant = self.expect_ident()?;

                    // Create a StructInit to carry the enum name and generics
                    // (it's a bit of a hack but avoids changing AST)
                    let typed_enum = Expr::StructInit {
                        name: name.clone(),
                        generics: type_args.clone(),
                        fields: Vec::new(),
                        span,
                    };

                    // Check if followed by ( for constructor call
                    if self.check(TokenKind::LParen) {
                        self.advance();
                        let args = self.parse_args()?;
                        self.expect(TokenKind::RParen)?;

                        return Ok(Expr::Call {
                            func: Box::new(Expr::Field {
                                object: Box::new(typed_enum),
                                field: variant,
                                span,
                            }),
                            type_args,
                            args,
                            span: self.span_from(span),
                        });
                    } else {
                        // Unit variant: EnumName<Type>.Variant
                        return Ok(Expr::Field {
                            object: Box::new(typed_enum),
                            field: variant,
                            span: self.span_from(span),
                        });
                    }
                }

                Ok(Expr::Ident(name, span))
            }
            Some(TokenKind::LParen) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            Some(TokenKind::LBracket) => {
                // Array literal
                self.advance();
                let mut elements = Vec::new();
                while !self.check(TokenKind::RBracket) {
                    elements.push(self.parse_expr()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
                self.expect(TokenKind::RBracket)?;
                Ok(Expr::ArrayInit {
                    elements,
                    span: self.span_from(span),
                })
            }
            Some(TokenKind::LBrace) => {
                // Block expression
                let block = self.parse_block()?;
                Ok(Expr::Block(block))
            }
            Some(TokenKind::Keyword(Keyword::SelfValue)) => {
                // 'self' used as an expression (e.g., self.field in methods)
                self.advance();
                Ok(Expr::Ident("self".to_string(), span))
            }
            _ => Err(self.error("expected expression")),
        }
    }

    pub(crate) fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        while !self.check(TokenKind::RParen) {
            args.push(self.parse_expr()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(args)
    }

    /// Parse an interpolated string: "prefix${expr}middle${expr}suffix"
    /// Called when we encounter InterpolatedStringStart
    fn parse_interpolated_string(&mut self, first_literal: String, start: Span) -> Result<Expr, ParseError> {
        self.advance(); // consume InterpolatedStringStart

        let mut parts = Vec::new();

        // Add the first literal part (may be empty)
        if !first_literal.is_empty() {
            parts.push(StringPart::Literal(first_literal));
        }

        // Parse the first expression
        let expr = self.parse_expr()?;
        parts.push(StringPart::Expr(Box::new(expr)));

        // Now we should see either InterpolatedStringMiddle or InterpolatedStringEnd
        loop {
            match self.peek_kind() {
                Some(TokenKind::InterpolatedStringMiddle(s)) => {
                    let s = s.clone();
                    self.advance();

                    // Add the middle literal (may be empty)
                    if !s.is_empty() {
                        parts.push(StringPart::Literal(s));
                    }

                    // Parse the next expression
                    let expr = self.parse_expr()?;
                    parts.push(StringPart::Expr(Box::new(expr)));
                }
                Some(TokenKind::InterpolatedStringEnd(s)) => {
                    let s = s.clone();
                    self.advance();

                    // Add the final literal (may be empty)
                    if !s.is_empty() {
                        parts.push(StringPart::Literal(s));
                    }

                    // Done!
                    break;
                }
                _ => {
                    return Err(self.error("expected continuation of interpolated string"));
                }
            }
        }

        Ok(Expr::InterpolatedString {
            parts,
            span: self.span_from(start),
        })
    }

    fn parse_struct_init_with_generics(&mut self, name: String, generics: Vec<Type>, start: Span) -> Result<Expr, ParseError> {
        self.expect(TokenKind::LBrace)?;

        let mut fields = Vec::new();

        while !self.check(TokenKind::RBrace) {
            let field_name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let value = self.parse_expr()?;
            fields.push((field_name, value));

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Expr::StructInit {
            name,
            generics,
            fields,
            span: self.span_from(start),
        })
    }

    /// Check if we're looking at generic type arguments (< Type, ... >) vs a comparison (< expr)
    fn is_generic_args(&self) -> bool {
        // Look ahead: < must be followed by a type-like thing and eventually >
        // Types start with: identifier (includes primitives like i32), &, ~, *
        let mut depth = 0;
        let mut i = self.pos;

        // Must start with <
        if self.tokens.get(i).map(|t| &t.kind) != Some(&TokenKind::Lt) {
            return false;
        }
        i += 1;
        depth += 1;

        while i < self.tokens.len() && depth > 0 {
            match self.tokens.get(i).map(|t| &t.kind) {
                Some(TokenKind::Lt) => depth += 1,
                Some(TokenKind::Gt) => {
                    depth -= 1;
                    if depth == 0 {
                        // Check what follows the >
                        // Generic calls: >( or >{ or >) or >, or >; or >.
                        let next = self.tokens.get(i + 1).map(|t| &t.kind);
                        return matches!(next,
                            Some(TokenKind::LParen) |
                            Some(TokenKind::LBrace) |
                            Some(TokenKind::RParen) |
                            Some(TokenKind::Comma) |
                            Some(TokenKind::Semicolon) |
                            Some(TokenKind::Dot) |  // For EnumName<Type>.Variant
                            None
                        );
                    }
                }
                Some(TokenKind::Ident(_)) |
                Some(TokenKind::Comma) |
                Some(TokenKind::Amp) |
                Some(TokenKind::Tilde) |
                Some(TokenKind::Star) => {}
                // Invalid token for type args - probably a comparison
                _ => return false,
            }
            i += 1;
        }

        false
    }
}

// Helper trait for Expr
impl Expr {
    pub(crate) fn span(&self) -> Span {
        match self {
            Expr::Literal(_, span) => *span,
            Expr::Ident(_, span) => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::MethodCall { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Field { span, .. } => *span,
            Expr::Ref { span, .. } => *span,
            Expr::RefMut { span, .. } => *span,
            Expr::Deref { span, .. } => *span,
            Expr::StructInit { span, .. } => *span,
            Expr::ArrayInit { span, .. } => *span,
            Expr::If { span, .. } => *span,
            Expr::Block(block) => block.span,
            Expr::Try { span, .. } => *span,
            Expr::Cast { span, .. } => *span,
            Expr::Range { span, .. } => *span,
            Expr::InterpolatedString { span, .. } => *span,
        }
    }
}
