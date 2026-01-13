//! Vibelang parser - builds AST from tokens

mod expr;
mod helpers;
mod item;
mod pattern;
mod stmt;
mod types;

use crate::ast::*;
use crate::lexer::{Keyword, Lexer, Token, TokenKind};

pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub fn parse(source: &str) -> Result<Program, ParseError> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(|e| ParseError::LexError(e.to_string()))?;
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_item()?)
        }

        Ok(Program { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let is_pub = self.match_keyword(Keyword::Pub);

        match self.peek_kind() {
            Some(TokenKind::Keyword(Keyword::Fn)) => {
                Ok(Item::Function(self.parse_function(is_pub)?))
            }
            Some(TokenKind::Keyword(Keyword::Struct)) => {
                Ok(Item::Struct(self.parse_struct(is_pub)?))
            }
            Some(TokenKind::Keyword(Keyword::Enum)) => {
                Ok(Item::Enum(self.parse_enum(is_pub)?))
            }
            Some(TokenKind::Keyword(Keyword::Impl)) => {
                Ok(Item::Impl(self.parse_impl()?))
            }
            Some(TokenKind::Keyword(Keyword::Static)) => {
                Ok(Item::Static(self.parse_static(is_pub)?))
            }
            Some(TokenKind::Keyword(Keyword::Use)) => {
                Ok(Item::Use(self.parse_use(is_pub)?))
            }
            Some(TokenKind::Keyword(Keyword::Mod)) => {
                Ok(Item::Mod(self.parse_mod(is_pub)?))
            }
            _ => Err(self.error("expected item (fn, struct, enum, impl, static, use, mod)")),
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    LexError(String),
    Unexpected {
        message: String,
        line: u32,
        column: u32,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::LexError(msg) => write!(f, "lex error: {}", msg),
            ParseError::Unexpected {
                message,
                line,
                column,
            } => write!(f, "parse error at {}:{}: {}", line, column, message),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_function() {
        let source = "fn main() { }";
        let program = Parser::parse(source).unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_let() {
        let source = "fn main() { let x = 42 }";
        let program = Parser::parse(source).unwrap();
        assert_eq!(program.items.len(), 1);
    }

    #[test]
    fn test_parse_interpolated_string() {
        let source = r#"fn main() { let s = "hello ${name}" }"#;
        let program = Parser::parse(source).unwrap();
        assert_eq!(program.items.len(), 1);

        // Verify it parsed as an interpolated string
        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                assert!(matches!(value, Expr::InterpolatedString { .. }));
            } else {
                panic!("Expected Let statement");
            }
        } else {
            panic!("Expected Function");
        }
    }

    #[test]
    fn test_parse_interpolated_string_multiple() {
        let source = r#"fn main() { let s = "x=${x}, y=${y}" }"#;
        let program = Parser::parse(source).unwrap();

        if let Item::Function(f) = &program.items[0] {
            if let Stmt::Let { value, .. } = &f.body.stmts[0] {
                if let Expr::InterpolatedString { parts, .. } = value {
                    // Should have: "x=", x, ", y=", y
                    assert_eq!(parts.len(), 4);
                } else {
                    panic!("Expected InterpolatedString");
                }
            } else {
                panic!("Expected Let statement");
            }
        } else {
            panic!("Expected Function");
        }
    }
}
