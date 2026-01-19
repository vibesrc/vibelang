//! Top-level item parsing (functions, structs, enums, impl blocks, etc.)

use super::{ParseError, Parser};
use crate::ast::*;
use crate::lexer::{Keyword, TokenKind};

impl Parser {
    pub(crate) fn parse_function(&mut self, is_pub: bool) -> Result<Function, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_ident()?;
        let (generics, bounds) = self.parse_generics_with_bounds()?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;

        let return_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(Function {
            name,
            generics,
            bounds,
            params,
            return_type,
            body,
            is_pub,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();

        while !self.check(TokenKind::RParen) {
            let start = self.current_span();

            // Handle self parameters: &self, ~self, or self
            if self.check(TokenKind::Amp) {
                // &self - immutable reference
                self.advance();
                if self.peek_kind() == Some(&TokenKind::Keyword(Keyword::SelfValue)) {
                    self.advance();
                    params.push(Param {
                        name: "self".to_string(),
                        ty: Type::Ref(Box::new(Type::SelfType)),
                        span: self.span_from(start),
                    });
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    continue;
                } else {
                    return Err(self.error("expected 'self' after '&'"));
                }
            } else if self.check(TokenKind::Tilde) {
                // ~self - mutable reference
                self.advance();
                if self.peek_kind() == Some(&TokenKind::Keyword(Keyword::SelfValue)) {
                    self.advance();
                    params.push(Param {
                        name: "self".to_string(),
                        ty: Type::RefMut(Box::new(Type::SelfType)),
                        span: self.span_from(start),
                    });
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    continue;
                } else {
                    return Err(self.error("expected 'self' after '~'"));
                }
            } else if self.peek_kind() == Some(&TokenKind::Keyword(Keyword::SelfValue)) {
                // self - consuming self
                self.advance();
                params.push(Param {
                    name: "self".to_string(),
                    ty: Type::SelfType,
                    span: self.span_from(start),
                });
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                continue;
            }

            // Regular parameter: name: type
            let name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            params.push(Param {
                name,
                ty,
                span: self.span_from(start),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    pub(crate) fn parse_struct(&mut self, is_pub: bool) -> Result<Struct, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_ident()?;
        let (generics, bounds) = self.parse_generics_with_bounds()?;

        self.expect(TokenKind::LBrace)?;
        let fields = self.parse_fields()?;
        self.expect(TokenKind::RBrace)?;

        Ok(Struct {
            name,
            generics,
            bounds,
            fields,
            is_pub,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_enum(&mut self, is_pub: bool) -> Result<Enum, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();

        while !self.check(TokenKind::RBrace) {
            variants.push(self.parse_variant()?);
            // Optional comma between variants
            self.match_token(TokenKind::Comma);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Enum {
            name,
            generics,
            variants,
            is_pub,
            span: self.span_from(start),
        })
    }

    fn parse_variant(&mut self) -> Result<Variant, ParseError> {
        let start = self.current_span();
        let name = self.expect_ident()?;

        let fields = if self.match_token(TokenKind::LParen) {
            // Tuple variant
            let mut types = Vec::new();
            while !self.check(TokenKind::RParen) {
                types.push(self.parse_type()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::RParen)?;
            VariantFields::Tuple(types)
        } else if self.match_token(TokenKind::LBrace) {
            // Struct variant
            let fields = self.parse_fields()?;
            self.expect(TokenKind::RBrace)?;
            VariantFields::Struct(fields)
        } else {
            VariantFields::Unit
        };

        Ok(Variant {
            name,
            fields,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_impl(&mut self) -> Result<Impl, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Impl)?;

        // Parse optional generics with bounds: impl<T: Hash>
        let (generics, bounds) = self.parse_generics_with_bounds()?;

        // Parse first type (could be trait name or impl target)
        let first = self.parse_type()?;

        // Check for `for` keyword to distinguish trait impl from inherent impl
        let (trait_name, target) = if self.match_keyword(Keyword::For) {
            // Trait impl: impl Trait for Type
            let trait_name = self.type_to_name(&first)?;
            let target = self.parse_type()?;
            (Some(trait_name), target)
        } else {
            // Inherent impl: impl Type
            (None, first)
        };

        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();

        while !self.check(TokenKind::RBrace) {
            let is_pub = self.match_keyword(Keyword::Pub);
            methods.push(self.parse_function(is_pub)?);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Impl {
            trait_name,
            generics,
            bounds,
            target,
            methods,
            span: self.span_from(start),
        })
    }

    /// Convert a Type to a trait name string (for impl Trait for Type)
    fn type_to_name(&self, ty: &Type) -> Result<String, ParseError> {
        match ty {
            Type::Named { name, generics: _ } => Ok(name.clone()),
            _ => Err(ParseError::Unexpected {
                message: "expected trait name".to_string(),
                line: 0,
                column: 0,
            }),
        }
    }

    pub(crate) fn parse_static(&mut self, is_pub: bool) -> Result<Static, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Static)?;
        let name = self.expect_ident()?;

        let ty = if self.match_token(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;

        Ok(Static {
            name,
            ty,
            value,
            is_pub,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_use(&mut self, is_pub: bool) -> Result<Use, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Use)?;

        // Parse the prefix and first path segment
        let (prefix, mut path) = self.parse_use_prefix()?;
        let initial_path_len = path.len();

        // Parse remaining path segments (after prefix)
        // e.g., for "use src.utils.format.{x}", path becomes ["utils", "format"]
        // The items to import come after the last dot
        while self.check(TokenKind::Dot) {
            // Peek ahead to see what's after the dot
            let mut lookahead = self.pos;
            lookahead += 1; // skip current token
            if lookahead < self.tokens.len() {
                match &self.tokens[lookahead].kind {
                    TokenKind::LBrace => {
                        // Next is {, so parse grouped imports
                        self.advance(); // consume the dot
                        break;
                    }
                    TokenKind::Star => {
                        // Next is *, so parse glob import
                        self.advance(); // consume the dot
                        self.advance(); // consume the *
                        return Ok(Use {
                            prefix,
                            path,
                            items: ImportItems::Glob,
                            is_pub,
                            span: self.span_from(start),
                        });
                    }
                    TokenKind::Ident(_) => {
                        // More path segments
                        self.advance(); // consume the dot
                        path.push(self.expect_ident()?);
                    }
                    _ => {
                        return Err(self.error("expected identifier, '{', or '*' after '.'"));
                    }
                }
            } else {
                return Err(self.error("unexpected end of input in use statement"));
            }
        }

        // Parse the imported items
        let items = if self.check(TokenKind::LBrace) {
            // Grouped import: use src.foo.{bar, baz as b}
            self.parse_use_group()?
        } else if path.is_empty() {
            return Err(self.error("use statement requires at least one path segment"));
        } else if path.len() == initial_path_len {
            // Module import: use std.fs (import module as namespace)
            // No additional segments were added after the initial path from prefix parsing
            // This allows: use std.fs, then use fs.File, fs.read_file(), etc.
            ImportItems::Module
        } else {
            // Single import: use src.foo.bar or use src.foo.bar as b
            // The last path segment is the item to import
            let item_name = path.pop().unwrap();
            let alias = if self.match_keyword(Keyword::As) {
                Some(self.expect_ident()?)
            } else {
                None
            };
            ImportItems::Named(vec![ImportItem {
                name: item_name,
                alias,
                span: self.span_from(start),
            }])
        };

        Ok(Use {
            prefix,
            path,
            items,
            is_pub,
            span: self.span_from(start),
        })
    }

    /// Parse the prefix of a use statement (src, std, dep, or relative .)
    fn parse_use_prefix(&mut self) -> Result<(ImportPrefix, Vec<String>), ParseError> {
        // Check for relative import starting with .
        if self.check(TokenKind::Dot) {
            self.advance(); // consume the dot
            let first_segment = self.expect_ident()?;
            return Ok((ImportPrefix::Relative, vec![first_segment]));
        }

        // Parse the first identifier (src, std, dep, or module name for backward compat)
        let first = self.expect_ident()?;

        match first.as_str() {
            "src" => {
                self.expect(TokenKind::Dot)?;
                let next = self.expect_ident()?;
                Ok((ImportPrefix::Src, vec![next]))
            }
            "lib" => {
                self.expect(TokenKind::Dot)?;
                let next = self.expect_ident()?;
                Ok((ImportPrefix::Lib, vec![next]))
            }
            "std" => {
                self.expect(TokenKind::Dot)?;
                let next = self.expect_ident()?;
                Ok((ImportPrefix::Std, vec![next]))
            }
            "dep" => {
                self.expect(TokenKind::Dot)?;
                let next = self.expect_ident()?;
                Ok((ImportPrefix::Dep, vec![next]))
            }
            _ => {
                // Not a known prefix - this is an error in the new module system
                Err(self.error(&format!(
                    "invalid import prefix '{}'. Use 'src.', 'lib.', 'std.', 'dep.', or '.' for relative imports",
                    first
                )))
            }
        }
    }

    /// Parse a grouped import: {foo, bar as b, baz}
    fn parse_use_group(&mut self) -> Result<ImportItems, ParseError> {
        self.expect(TokenKind::LBrace)?;
        let mut items = Vec::new();

        while !self.check(TokenKind::RBrace) {
            let item_start = self.current_span();
            let name = self.expect_ident()?;
            let alias = if self.match_keyword(Keyword::As) {
                Some(self.expect_ident()?)
            } else {
                None
            };
            items.push(ImportItem {
                name,
                alias,
                span: self.span_from(item_start),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RBrace)?;
        Ok(ImportItems::Named(items))
    }

    pub(crate) fn parse_generics(&mut self) -> Result<Vec<String>, ParseError> {
        let (generics, _bounds) = self.parse_generics_with_bounds()?;
        Ok(generics)
    }

    /// Parse generics with optional trait bounds: <T: Hash, U: Eq + Clone>
    pub(crate) fn parse_generics_with_bounds(&mut self) -> Result<(Vec<String>, Vec<(String, String)>), ParseError> {
        if !self.match_token(TokenKind::Lt) {
            return Ok((Vec::new(), Vec::new()));
        }

        let mut generics = Vec::new();
        let mut bounds = Vec::new();

        loop {
            let name = self.expect_ident()?;
            generics.push(name.clone());

            // Check for bounds: T: Hash + Eq
            if self.match_token(TokenKind::Colon) {
                // Parse bounds: Hash + Eq + Clone
                loop {
                    let trait_name = self.expect_ident()?;
                    bounds.push((name.clone(), trait_name));
                    if !self.match_token(TokenKind::Plus) {
                        break;
                    }
                }
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::Gt)?;

        Ok((generics, bounds))
    }

    /// Parse a trait definition
    pub(crate) fn parse_trait(&mut self, is_pub: bool) -> Result<Trait, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Trait)?;
        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

        // Parse supertraits: trait Ord: Eq + Hash
        let supertraits = if self.match_token(TokenKind::Colon) {
            self.parse_trait_bound_list()?
        } else {
            Vec::new()
        };

        self.expect(TokenKind::LBrace)?;
        let methods = self.parse_trait_methods()?;
        self.expect(TokenKind::RBrace)?;

        Ok(Trait {
            name,
            generics,
            supertraits,
            methods,
            is_pub,
            span: self.span_from(start),
        })
    }

    /// Parse a list of trait bounds separated by +: Hash + Eq + Clone
    fn parse_trait_bound_list(&mut self) -> Result<Vec<String>, ParseError> {
        let mut traits = Vec::new();
        loop {
            traits.push(self.expect_ident()?);
            if !self.match_token(TokenKind::Plus) {
                break;
            }
        }
        Ok(traits)
    }

    /// Parse methods within a trait definition
    fn parse_trait_methods(&mut self) -> Result<Vec<TraitMethod>, ParseError> {
        let mut methods = Vec::new();

        while !self.check(TokenKind::RBrace) {
            methods.push(self.parse_trait_method()?);
        }

        Ok(methods)
    }

    /// Parse a single trait method (may have default impl or be abstract)
    fn parse_trait_method(&mut self) -> Result<TraitMethod, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_ident()?;
        let (generics, bounds) = self.parse_generics_with_bounds()?;

        self.expect(TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect(TokenKind::RParen)?;

        let return_type = if self.match_token(TokenKind::Arrow) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // Check for default implementation (block) or abstract (no body)
        let body = if self.check(TokenKind::LBrace) {
            Some(self.parse_block()?)
        } else {
            // Abstract method - no body, just consumes any trailing newlines/semicolons
            None
        };

        Ok(TraitMethod {
            name,
            generics,
            bounds,
            params,
            return_type,
            body,
            span: self.span_from(start),
        })
    }

    pub(crate) fn parse_fields(&mut self) -> Result<Vec<Field>, ParseError> {
        let mut fields = Vec::new();

        while !self.check(TokenKind::RBrace) {
            let start = self.current_span();
            let is_pub = self.match_keyword(Keyword::Pub);
            let name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            fields.push(Field {
                name,
                ty,
                is_pub,
                span: self.span_from(start),
            });

            // Optional comma
            self.match_token(TokenKind::Comma);
        }

        Ok(fields)
    }
}
