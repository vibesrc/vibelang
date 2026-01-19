//! Type parsing

use super::{ParseError, Parser};
use crate::ast::Type;
use crate::lexer::{Keyword, TokenKind};

impl Parser {
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParseError> {
        // Function type: fn(T1, T2) -> R
        if self.match_keyword(Keyword::Fn) {
            self.expect(TokenKind::LParen)?;

            let mut param_types = Vec::new();
            if !self.check(TokenKind::RParen) {
                loop {
                    param_types.push(self.parse_type()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    if self.check(TokenKind::RParen) {
                        break; // trailing comma
                    }
                }
            }
            self.expect(TokenKind::RParen)?;

            // Return type: -> Type (optional, defaults to void)
            let return_type = if self.match_token(TokenKind::Arrow) {
                self.parse_type()?
            } else {
                Type::Void
            };

            return Ok(Type::Fn(param_types, Box::new(return_type)));
        }

        // Tuple type: (T1, T2, ...)
        if self.match_token(TokenKind::LParen) {
            let mut types = Vec::new();
            let mut saw_comma = false;

            if !self.check(TokenKind::RParen) {
                types.push(self.parse_type()?);

                while self.match_token(TokenKind::Comma) {
                    saw_comma = true;
                    if self.check(TokenKind::RParen) {
                        break; // trailing comma
                    }
                    types.push(self.parse_type()?);
                }
            }

            self.expect(TokenKind::RParen)?;

            // Single type in parens without comma is just grouping, not a tuple
            // (i32) -> i32, (i32,) -> Tuple([i32]), (i32, i64) -> Tuple([i32, i64])
            if types.len() == 1 && !saw_comma {
                return Ok(types.pop().unwrap());
            }

            return Ok(Type::Tuple(types));
        }

        // Pointer type
        if self.match_token(TokenKind::Star) {
            let inner = self.parse_type()?;
            return Ok(Type::Pointer(Box::new(inner)));
        }

        // Read-only borrow
        if self.match_token(TokenKind::Amp) {
            let inner = self.parse_type()?;
            return Ok(Type::Ref(Box::new(inner)));
        }

        // Mutable borrow (vibing)
        if self.match_token(TokenKind::Tilde) {
            let inner = self.parse_type()?;
            return Ok(Type::RefMut(Box::new(inner)));
        }

        // Named type (possibly with generics)
        let name = self.expect_ident()?;

        // Check for primitive types
        let ty = match name.as_str() {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "void" => Type::Void,
            // str is a primitive type (UTF-8 string slice)
            "str" => Type::Str,
            _ => {
                // Generic type?
                let generics = if self.match_token(TokenKind::Lt) {
                    let mut types = Vec::new();
                    loop {
                        types.push(self.parse_type()?);
                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }
                    self.expect(TokenKind::Gt)?;
                    types
                } else {
                    Vec::new()
                };

                Type::Named { name, generics }
            }
        };

        // Check for array type T[N] or slice type T[]
        if self.match_token(TokenKind::LBracket) {
            if let Some(TokenKind::Int(n, _)) = self.peek_kind() {
                // T[N] - fixed-size array
                let size = *n as usize;
                self.advance();
                self.expect(TokenKind::RBracket)?;
                return Ok(Type::Array(Box::new(ty), size));
            } else if self.check(TokenKind::RBracket) {
                // T[] - slice (sugar for Slice<T>)
                self.expect(TokenKind::RBracket)?;
                return Ok(Type::Slice(Box::new(ty)));
            } else {
                return Err(self.error("expected array size or ]"));
            }
        }

        Ok(ty)
    }

    /// Parse type arguments: <Type, Type, ...>
    pub(crate) fn parse_type_args(&mut self) -> Result<Vec<Type>, ParseError> {
        self.expect(TokenKind::Lt)?;

        let mut types = Vec::new();

        loop {
            types.push(self.parse_type()?);

            if self.match_token(TokenKind::Gt) {
                break;
            }

            self.expect(TokenKind::Comma)?;
        }

        Ok(types)
    }
}
