//! Vibelang parser - builds AST from tokens

use crate::ast::*;
use crate::lexer::{Keyword, Lexer, Span, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
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

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();

        while !self.is_at_end() {
            items.push(self.parse_item()?);
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

    fn parse_function(&mut self, is_pub: bool) -> Result<Function, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

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
            params,
            return_type,
            body,
            is_pub,
            span: self.span_from(start),
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();

        while !self.check(TokenKind::RParen) {
            let start = self.current_span();
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

    fn parse_struct(&mut self, is_pub: bool) -> Result<Struct, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_ident()?;
        let generics = self.parse_generics()?;

        self.expect(TokenKind::LBrace)?;
        let fields = self.parse_fields()?;
        self.expect(TokenKind::RBrace)?;

        Ok(Struct {
            name,
            generics,
            fields,
            is_pub,
            span: self.span_from(start),
        })
    }

    fn parse_enum(&mut self, is_pub: bool) -> Result<Enum, ParseError> {
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

    fn parse_impl(&mut self) -> Result<Impl, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Impl)?;
        let target = self.parse_type()?;

        self.expect(TokenKind::LBrace)?;
        let mut methods = Vec::new();

        while !self.check(TokenKind::RBrace) {
            let is_pub = self.match_keyword(Keyword::Pub);
            methods.push(self.parse_function(is_pub)?);
        }

        self.expect(TokenKind::RBrace)?;

        Ok(Impl {
            target,
            methods,
            span: self.span_from(start),
        })
    }

    fn parse_static(&mut self, is_pub: bool) -> Result<Static, ParseError> {
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

    fn parse_use(&mut self, is_pub: bool) -> Result<Use, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Use)?;

        let mut path = vec![self.expect_ident()?];
        while self.match_token(TokenKind::Dot) {
            path.push(self.expect_ident()?);
        }

        let alias = if self.match_keyword(Keyword::As) {
            Some(self.expect_ident()?)
        } else {
            None
        };

        Ok(Use {
            path,
            alias,
            is_pub,
            span: self.span_from(start),
        })
    }

    fn parse_mod(&mut self, is_pub: bool) -> Result<Mod, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Mod)?;
        let name = self.expect_ident()?;

        Ok(Mod {
            name,
            is_pub,
            span: self.span_from(start),
        })
    }

    fn parse_generics(&mut self) -> Result<Vec<String>, ParseError> {
        if !self.match_token(TokenKind::Lt) {
            return Ok(Vec::new());
        }

        let mut generics = Vec::new();
        loop {
            generics.push(self.expect_ident()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::Gt)?;

        Ok(generics)
    }

    fn parse_fields(&mut self) -> Result<Vec<Field>, ParseError> {
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

    fn parse_type(&mut self) -> Result<Type, ParseError> {
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
            "void" => Type::Void,
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

        // Check for array type T[N]
        if self.match_token(TokenKind::LBracket) {
            if let Some(TokenKind::Int(n)) = self.peek_kind() {
                let size = *n as usize;
                self.advance();
                self.expect(TokenKind::RBracket)?;
                return Ok(Type::Array(Box::new(ty), size));
            } else {
                return Err(self.error("expected array size"));
            }
        }

        Ok(ty)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.current_span();
        self.expect(TokenKind::LBrace)?;

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

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
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

    fn parse_match_arm(&mut self) -> Result<MatchArm, ParseError> {
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_kind() {
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
            Some(TokenKind::Int(n)) => {
                let n = *n;
                self.advance();
                Ok(Pattern::Literal(Literal::Int(n)))
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

    fn parse_defer(&mut self) -> Result<Stmt, ParseError> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Defer)?;
        let expr = Box::new(self.parse_expr()?);

        Ok(Stmt::Defer {
            expr,
            span: self.span_from(start),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_or()?;

        if let Some(op) = self.match_assign_op() {
            let right = self.parse_assignment()?;
            let span = self.span_from(expr.span());
            return Ok(Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
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
            _ => Err(self.error("expected expression")),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        while !self.check(TokenKind::RParen) {
            args.push(self.parse_expr()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok(args)
    }

    fn parse_struct_init(&mut self, name: String, start: Span) -> Result<Expr, ParseError> {
        self.parse_struct_init_with_generics(name, Vec::new(), start)
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

    /// Parse type arguments: <Type, Type, ...>
    fn parse_type_args(&mut self) -> Result<Vec<Type>, ParseError> {
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

    // Helper methods

    fn peek_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos).map(|t| &t.kind)
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.peek_kind() == Some(&kind)
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_keyword(&mut self, kw: Keyword) -> bool {
        if self.peek_kind() == Some(&TokenKind::Keyword(kw)) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError> {
        if self.check(kind.clone()) {
            Ok(self.advance().unwrap())
        } else {
            Err(self.error(&format!("expected {:?}", kind)))
        }
    }

    fn expect_keyword(&mut self, kw: Keyword) -> Result<(), ParseError> {
        if self.match_keyword(kw) {
            Ok(())
        } else {
            Err(self.error(&format!("expected keyword {:?}", kw)))
        }
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Ident(name)) => {
                let name = name.clone();
                self.advance();
                Ok(name)
            }
            _ => Err(self.error("expected identifier")),
        }
    }

    fn match_assign_op(&mut self) -> Option<BinOp> {
        let op = match self.peek_kind() {
            Some(TokenKind::Eq) => BinOp::Assign,
            // TODO: compound assignment operators
            _ => return None,
        };
        self.advance();
        Some(op)
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), None | Some(TokenKind::Eof))
    }

    fn current_span(&self) -> Span {
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

    fn span_from(&self, start: Span) -> Span {
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

    fn error(&self, msg: &str) -> ParseError {
        let span = self.current_span();
        ParseError::Unexpected {
            message: msg.to_string(),
            line: span.line,
            column: span.column,
        }
    }
}

// Helper trait for Expr
impl Expr {
    fn span(&self) -> Span {
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
}
