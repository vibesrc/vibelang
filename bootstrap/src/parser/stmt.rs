//! Statement parsing

use super::{ParseError, Parser};
use crate::ast::{Block, MatchArm, Stmt};
use crate::lexer::{Keyword, TokenKind};

impl Parser {
    pub(crate) fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.current_span();
        self.expect(TokenKind::LBrace)?;
        self.parse_block_inner_from(start)
    }

    /// Parse a block's contents after the opening brace has already been consumed
    pub(crate) fn parse_block_inner(&mut self) -> Result<Block, ParseError> {
        let start = self.current_span();
        self.parse_block_inner_from(start)
    }

    fn parse_block_inner_from(&mut self, start: crate::lexer::Span) -> Result<Block, ParseError> {
        let mut stmts = Vec::new();
        while !self.check(TokenKind::RBrace) {
            stmts.push(self.parse_stmt()?);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Block {
            stmts,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Keyword(Keyword::Let)) => self.parse_let(),
            Some(TokenKind::Keyword(Keyword::Return)) => self.parse_return(),
            Some(TokenKind::Keyword(Keyword::If)) => self.parse_if_stmt(),
            Some(TokenKind::Keyword(Keyword::While)) => self.parse_while(),
            Some(TokenKind::Keyword(Keyword::For)) => self.parse_for(),
            Some(TokenKind::Keyword(Keyword::Match)) => self.parse_match_stmt(),
            Some(TokenKind::Keyword(Keyword::Defer)) => self.parse_defer(),
            Some(TokenKind::Keyword(Keyword::Break)) => {
                let span = self.current_span();
                self.advance();
                Ok(Stmt::Break { span })
            }
            Some(TokenKind::Keyword(Keyword::Continue)) => {
                let span = self.current_span();
                self.advance();
                Ok(Stmt::Continue { span })
            }
            _ => {
                let expr = self.parse_expr()?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Let)?;
        let name = self.expect_ident()?;

        let ty = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;

        Ok(Stmt::Let {
            name,
            ty,
            value,
            span: self.span_from(start),
        })
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Return)?;

        let value = if !self.check(TokenKind::RBrace) && !self.is_at_end() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Stmt::Return {
            value,
            span: self.span_from(start),
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::If)?;
        let condition = Box::new(self.parse_expr()?);
        let then_block = self.parse_block()?;

        let else_block = if self.match_keyword(Keyword::Else) {
            // Check for else if
            if self.check(TokenKind::Keyword(Keyword::If)) {
                // Parse else if as a nested if statement in a block
                let nested_if = self.parse_if_stmt()?;
                let nested_span = match &nested_if {
                    Stmt::If { span, .. } => *span,
                    _ => start,
                };
                Some(Block {
                    stmts: vec![nested_if],
                    span: nested_span,
                })
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_block,
            else_block,
            span: self.span_from(start),
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::While)?;
        let condition = Box::new(self.parse_expr()?);
        let body = self.parse_block()?;

        Ok(Stmt::While {
            condition,
            body,
            span: self.span_from(start),
        })
    }

    fn parse_for(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::For)?;
        let name = self.expect_ident()?;
        self.expect_keyword(Keyword::In)?;
        let iter = Box::new(self.parse_expr()?);
        let body = self.parse_block()?;

        Ok(Stmt::For {
            name,
            iter,
            body,
            span: self.span_from(start),
        })
    }

    fn parse_match_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Match)?;
        let value = Box::new(self.parse_expr()?);

        self.expect(TokenKind::LBrace)?;
        let mut arms = Vec::new();

        while !self.check(TokenKind::RBrace) {
            arms.push(self.parse_match_arm()?);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Stmt::Match {
            value,
            arms,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
        let start = self.current_span();
        let pattern = self.parse_pattern()?;
        self.expect(TokenKind::FatArrow)?;
        let body = self.parse_expr()?;

        // Optional comma
        self.match_token(TokenKind::Comma);

        Ok(MatchArm {
            pattern,
            body,
            span: self.span_from(start),
        })
    }

    fn parse_defer(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Defer)?;
        let expr = Box::new(self.parse_expr()?);

        Ok(Stmt::Defer {
            expr,
            span: self.span_from(start),
        })
    }
}
