//! Pattern parsing for match expressions

use super::{ParseError, Parser};
use crate::ast::{Literal, Pattern};
use crate::lexer::TokenKind;

impl Parser {
    pub(crate) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_kind() {
            // Tuple pattern: (a, b, c)
            Some(TokenKind::LParen) => {
                self.advance();
                let mut patterns = Vec::new();

                if !self.check(TokenKind::RParen) {
                    patterns.push(self.parse_pattern()?);
                    while self.match_token(TokenKind::Comma) {
                        if self.check(TokenKind::RParen) {
                            break; // trailing comma
                        }
                        patterns.push(self.parse_pattern()?);
                    }
                }

                self.expect(TokenKind::RParen)?;
                Ok(Pattern::Tuple(patterns))
            }
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                self.advance();

                if name == "_" {
                    return Ok(Pattern::Wildcard);
                }

                // Could be enum pattern like Option.Some(x)
                if self.match_token(TokenKind::Dot) {
                    let variant = self.expect_ident()?;
                    let path = vec![name, variant];

                    if self.match_token(TokenKind::LParen) {
                        let mut fields = Vec::new();
                        while !self.check(TokenKind::RParen) {
                            fields.push(self.parse_pattern()?);
                            if !self.match_token(TokenKind::Comma) {
                                break;
                            }
                        }
                        self.expect(TokenKind::RParen)?;
                        return Ok(Pattern::Enum { path, fields });
                    }

                    return Ok(Pattern::Enum {
                        path,
                        fields: Vec::new(),
                    });
                }

                Ok(Pattern::Ident(name))
            }
            Some(TokenKind::Int(n, suffix)) => {
                let n = *n;
                let suffix = *suffix;
                self.advance();
                Ok(Pattern::Literal(Literal::Int(n, suffix)))
            }
            Some(TokenKind::Bool(b)) => {
                let b = *b;
                self.advance();
                Ok(Pattern::Literal(Literal::Bool(b)))
            }
            Some(TokenKind::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Pattern::Literal(Literal::String(s)))
            }
            _ => Err(self.error("expected pattern")),
        }
    }
}
