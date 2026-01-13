//! Token navigation and utility functions

use super::{ParseError, Parser};
use crate::ast::BinOp;
use crate::lexer::{Keyword, Span, Token, TokenKind};

impl Parser {
    pub(crate) fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos).map(|t| &t.kind)
    }

    pub(crate) fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    pub(crate) fn check(&self, kind: TokenKind) -> bool {
        self.peek_kind() == Some(&kind)
    }

    pub(crate) fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn match_keyword(&mut self, kw: Keyword) -> bool {
        if self.peek_kind() == Some(&TokenKind::Keyword(kw)) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError> {
        if self.check(kind.clone()) {
            Ok(self.advance().unwrap())
        } else {
            Err(self.error(&format!("expected {:?}", kind)))
        }
    }

    pub(crate) fn expect_keyword(&mut self, kw: Keyword) -> Result<(), ParseError> {
        if self.match_keyword(kw) {
            Ok(())
        } else {
            Err(self.error(&format!("expected keyword {:?}", kw)))
        }
    }

    pub(crate) fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            // Allow 'self' as identifier in parameter position
            Some(TokenKind::Keyword(Keyword::SelfValue)) => {
                self.advance();
                Ok("self".to_string())
            }
            // Allow 'Self' as identifier for type position
            Some(TokenKind::Keyword(Keyword::SelfType)) => {
                self.advance();
                Ok("Self".to_string())
            }
            _ => Err(self.error("expected identifier")),
        }
    }

    /// Returns Some(None) for simple assignment (=), Some(Some(op)) for compound assignment (+=, etc.)
    pub(crate) fn match_assign_op(&mut self) -> Option<Option<BinOp>> {
        let result = match self.peek_kind() {
            Some(TokenKind::Eq) => Some(None), // simple assignment
            Some(TokenKind::PlusEq) => Some(Some(BinOp::Add)),
            Some(TokenKind::MinusEq) => Some(Some(BinOp::Sub)),
            Some(TokenKind::StarEq) => Some(Some(BinOp::Mul)),
            Some(TokenKind::SlashEq) => Some(Some(BinOp::Div)),
            Some(TokenKind::PercentEq) => Some(Some(BinOp::Mod)),
            _ => return None,
        };
        self.advance();
        result
    }

    pub(crate) fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), None | Some(TokenKind::Eof))
    }

    pub(crate) fn current_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span)
            .unwrap_or(Span {
                start: 0,
                end: 0,
                line: 0,
                column: 0,
            })
    }

    pub(crate) fn span_from(&self, start: Span) -> Span {
        let end = self
            .tokens
            .get(self.pos.saturating_sub(1))
            .map(|t| t.span.end)
            .unwrap_or(start.end);
        Span {
            start: start.start,
            end,
            line: start.line,
            column: start.column,
        }
    }

    pub(crate) fn error(&self, msg: &str) -> ParseError {
        let span = self.current_span();
        ParseError::Unexpected {
            message: msg.to_string(),
            line: span.line,
            column: span.column,
        }
    }
}
