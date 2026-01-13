//! Semantic token generation for the Vibelang LSP

use tower_lsp_server::ls_types::SemanticToken;
use crate::ast::{Block, Expr, Item, Program, Stmt, Pattern as AstPattern, StringPart};

use crate::lsp::types::RawSemanticToken;
use crate::lsp::utils::semantic_token_types;
use crate::lsp::Backend;

impl Backend {
    /// Collect semantic tokens from a parsed program
    pub fn collect_semantic_tokens(&self, program: &Program, text: &str) -> Vec<RawSemanticToken> {
        let mut tokens = Vec::new();

        for item in &program.items {
            match item {
                Item::Enum(e) => {
                    tokens.push(RawSemanticToken {
                        line: e.span.line.saturating_sub(1) as u32,
                        start: self.find_name_column(text, e.span.line, &e.name),
                        length: e.name.len() as u32,
                        token_type: semantic_token_types::ENUM,
                        modifiers: 1,
                    });

                    for generic in &e.generics {
                        if let Some((line, col)) = self.find_generic_param(text, e.span.line, generic) {
                            tokens.push(RawSemanticToken {
                                line: line as u32,
                                start: col as u32,
                                length: generic.len() as u32,
                                token_type: semantic_token_types::TYPE_PARAMETER,
                                modifiers: 1,
                            });
                        }
                    }

                    for variant in &e.variants {
                        tokens.push(RawSemanticToken {
                            line: variant.span.line.saturating_sub(1) as u32,
                            start: self.find_name_column(text, variant.span.line, &variant.name),
                            length: variant.name.len() as u32,
                            token_type: semantic_token_types::ENUM_MEMBER,
                            modifiers: 1,
                        });
                    }
                }
                Item::Struct(s) => {
                    tokens.push(RawSemanticToken {
                        line: s.span.line.saturating_sub(1) as u32,
                        start: self.find_name_column(text, s.span.line, &s.name),
                        length: s.name.len() as u32,
                        token_type: semantic_token_types::STRUCT,
                        modifiers: 1,
                    });

                    for generic in &s.generics {
                        if let Some((line, col)) = self.find_generic_param(text, s.span.line, generic) {
                            tokens.push(RawSemanticToken {
                                line: line as u32,
                                start: col as u32,
                                length: generic.len() as u32,
                                token_type: semantic_token_types::TYPE_PARAMETER,
                                modifiers: 1,
                            });
                        }
                    }

                    for field in &s.fields {
                        tokens.push(RawSemanticToken {
                            line: field.span.line.saturating_sub(1) as u32,
                            start: self.find_name_column(text, field.span.line, &field.name),
                            length: field.name.len() as u32,
                            token_type: semantic_token_types::PROPERTY,
                            modifiers: 1,
                        });
                    }
                }
                Item::Function(f) => {
                    tokens.push(RawSemanticToken {
                        line: f.span.line.saturating_sub(1) as u32,
                        start: self.find_name_column(text, f.span.line, &f.name),
                        length: f.name.len() as u32,
                        token_type: semantic_token_types::FUNCTION,
                        modifiers: 1,
                    });

                    for generic in &f.generics {
                        if let Some((line, col)) = self.find_generic_param(text, f.span.line, generic) {
                            tokens.push(RawSemanticToken {
                                line: line as u32,
                                start: col as u32,
                                length: generic.len() as u32,
                                token_type: semantic_token_types::TYPE_PARAMETER,
                                modifiers: 1,
                            });
                        }
                    }

                    for param in &f.params {
                        tokens.push(RawSemanticToken {
                            line: param.span.line.saturating_sub(1) as u32,
                            start: self.find_name_column(text, param.span.line, &param.name),
                            length: param.name.len() as u32,
                            token_type: semantic_token_types::PARAMETER,
                            modifiers: 1,
                        });
                    }

                    self.collect_tokens_from_block(&f.body, text, &mut tokens, &f.generics);
                }
                _ => {}
            }
        }

        tokens.sort_by(|a, b| {
            a.line.cmp(&b.line).then_with(|| a.start.cmp(&b.start))
        });

        tokens
    }

    pub fn collect_tokens_from_block(
        &self,
        block: &Block,
        text: &str,
        tokens: &mut Vec<RawSemanticToken>,
        type_params: &[String],
    ) {
        for stmt in &block.stmts {
            self.collect_tokens_from_stmt(stmt, text, tokens, type_params);
        }
    }

    pub fn collect_tokens_from_stmt(
        &self,
        stmt: &Stmt,
        text: &str,
        tokens: &mut Vec<RawSemanticToken>,
        type_params: &[String],
    ) {
        match stmt {
            Stmt::Let { name, value, span, .. } => {
                tokens.push(RawSemanticToken {
                    line: span.line.saturating_sub(1) as u32,
                    start: self.find_name_column(text, span.line, name),
                    length: name.len() as u32,
                    token_type: semantic_token_types::VARIABLE,
                    modifiers: 1,
                });
                self.collect_tokens_from_expr(value, text, tokens, type_params);
            }
            Stmt::Expr(expr) => {
                self.collect_tokens_from_expr(expr, text, tokens, type_params);
            }
            Stmt::Return { value: Some(v), .. } => {
                self.collect_tokens_from_expr(v, text, tokens, type_params);
            }
            Stmt::If { condition, then_block, else_block, .. } => {
                self.collect_tokens_from_expr(condition, text, tokens, type_params);
                self.collect_tokens_from_block(then_block, text, tokens, type_params);
                if let Some(eb) = else_block {
                    self.collect_tokens_from_block(eb, text, tokens, type_params);
                }
            }
            Stmt::While { condition, body, .. } => {
                self.collect_tokens_from_expr(condition, text, tokens, type_params);
                self.collect_tokens_from_block(body, text, tokens, type_params);
            }
            Stmt::For { iter, body, .. } => {
                self.collect_tokens_from_expr(iter, text, tokens, type_params);
                self.collect_tokens_from_block(body, text, tokens, type_params);
            }
            Stmt::Match { value, arms, .. } => {
                self.collect_tokens_from_expr(value, text, tokens, type_params);
                for arm in arms {
                    self.collect_tokens_from_pattern(&arm.pattern, text, tokens);
                    self.collect_tokens_from_expr(&arm.body, text, tokens, type_params);
                }
            }
            _ => {}
        }
    }

    pub fn collect_tokens_from_expr(
        &self,
        expr: &Expr,
        text: &str,
        tokens: &mut Vec<RawSemanticToken>,
        type_params: &[String],
    ) {
        match expr {
            Expr::Field { object, field, span } => {
                self.collect_tokens_from_expr(object, text, tokens, type_params);

                if let Expr::Ident(name, _) | Expr::StructInit { name, .. } = object.as_ref() {
                    let base_name = name.split('<').next().unwrap_or(name);
                    if base_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                        tokens.push(RawSemanticToken {
                            line: span.line.saturating_sub(1) as u32,
                            start: self.find_field_column(text, span.line, field),
                            length: field.len() as u32,
                            token_type: semantic_token_types::ENUM_MEMBER,
                            modifiers: 0,
                        });
                    }
                }
            }
            Expr::Call { func, args, .. } => {
                self.collect_tokens_from_expr(func, text, tokens, type_params);
                for arg in args {
                    self.collect_tokens_from_expr(arg, text, tokens, type_params);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.collect_tokens_from_expr(left, text, tokens, type_params);
                self.collect_tokens_from_expr(right, text, tokens, type_params);
            }
            Expr::Unary { operand, .. } => {
                self.collect_tokens_from_expr(operand, text, tokens, type_params);
            }
            Expr::StructInit { fields, .. } => {
                for (_, expr) in fields {
                    self.collect_tokens_from_expr(expr, text, tokens, type_params);
                }
            }
            Expr::ArrayInit { elements, .. } => {
                for elem in elements {
                    self.collect_tokens_from_expr(elem, text, tokens, type_params);
                }
            }
            Expr::Index { array, index, .. } => {
                self.collect_tokens_from_expr(array, text, tokens, type_params);
                self.collect_tokens_from_expr(index, text, tokens, type_params);
            }
            Expr::Block(block) => {
                self.collect_tokens_from_block(block, text, tokens, type_params);
            }
            Expr::Ref { operand, .. } | Expr::RefMut { operand, .. } | Expr::Deref { operand, .. } => {
                self.collect_tokens_from_expr(operand, text, tokens, type_params);
            }
            Expr::MethodCall { receiver, args, .. } => {
                self.collect_tokens_from_expr(receiver, text, tokens, type_params);
                for arg in args {
                    self.collect_tokens_from_expr(arg, text, tokens, type_params);
                }
            }
            Expr::InterpolatedString { parts, .. } => {
                for part in parts {
                    if let StringPart::Expr(expr) = part {
                        self.collect_tokens_from_expr(expr, text, tokens, type_params);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn collect_tokens_from_pattern(&self, pattern: &AstPattern, text: &str, tokens: &mut Vec<RawSemanticToken>) {
        match pattern {
            AstPattern::Enum { path: _, fields, .. } => {
                for field in fields {
                    self.collect_tokens_from_pattern(field, text, tokens);
                }
            }
            _ => {}
        }
    }

    pub fn find_name_column(&self, text: &str, line: u32, name: &str) -> u32 {
        if let Some(line_text) = text.lines().nth(line.saturating_sub(1) as usize) {
            if let Some(pos) = line_text.find(name) {
                return pos as u32;
            }
        }
        0
    }

    pub fn find_field_column(&self, text: &str, line: u32, field: &str) -> u32 {
        if let Some(line_text) = text.lines().nth(line.saturating_sub(1) as usize) {
            if let Some(pos) = line_text.find(&format!(".{}", field)) {
                return (pos + 1) as u32;
            }
        }
        0
    }

    pub fn find_generic_param(&self, text: &str, line: u32, param: &str) -> Option<(u32, u32)> {
        if let Some(line_text) = text.lines().nth(line.saturating_sub(1) as usize) {
            if let Some(start) = line_text.find('<') {
                if let Some(end) = line_text[start..].find('>') {
                    let generic_part = &line_text[start..start+end+1];
                    if let Some(pos) = generic_part.find(param) {
                        let abs_pos = start + pos;
                        return Some((line.saturating_sub(1), abs_pos as u32));
                    }
                }
            }
        }
        None
    }

    pub fn encode_semantic_tokens(&self, raw_tokens: Vec<RawSemanticToken>) -> Vec<SemanticToken> {
        let mut result = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;

        for token in raw_tokens {
            let delta_line = token.line - prev_line;
            let delta_start = if delta_line == 0 {
                token.start - prev_start
            } else {
                token.start
            };

            result.push(SemanticToken {
                delta_line,
                delta_start,
                length: token.length,
                token_type: token.token_type,
                token_modifiers_bitset: token.modifiers,
            });

            prev_line = token.line;
            prev_start = token.start;
        }

        result
    }
}
