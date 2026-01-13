//! Hover and go-to-definition handling for the Vibelang LSP

use tower_lsp_server::ls_types::{
    Hover, HoverContents, Location, MarkupContent, MarkupKind, Position, Uri,
};
use crate::ast::{Expr, Literal};

use crate::lsp::types::{BorrowState, DocumentInfo, VariantFieldsData};
use crate::lsp::utils::{is_builtin_type, is_ident_char};
use crate::lsp::Backend;

impl Backend {
    pub fn get_hover(&self, doc: &DocumentInfo, position: Position) -> Option<Hover> {
        let offset = self.position_to_offset(&doc.text, position);
        let word = self.get_word_at_position(&doc.text, position)?;

        // Variable
        for var in &doc.symbols.variables {
            if var.name == word && var.scope_start <= offset && offset <= var.scope_end {
                let borrow_info = match var.borrow_state {
                    BorrowState::Owned => "owned",
                    BorrowState::Borrowed => "borrowed (&)",
                    BorrowState::MutBorrowed => "mutably borrowed (~)",
                    BorrowState::Moved => "moved",
                };
                let ty_str = var.ty.as_deref().unwrap_or("(inferred)");
                let content = format!(
                    "**Variable** `{}`\n\n**Type:** `{}`\n\n**State:** {}",
                    word, ty_str, borrow_info
                );
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: None,
                });
            }
        }

        // Function
        if let Some(func) = doc.symbols.functions.get(&word) {
            let params_str: Vec<_> = func
                .params
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
                .collect();
            let generics_str = if func.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", func.generics.join(", "))
            };
            let sig = format!(
                "fn {}{}({}) -> {}",
                word,
                generics_str,
                params_str.join(", "),
                func.return_type.as_deref().unwrap_or("void")
            );
            let visibility = if func.is_pub { "pub " } else { "" };
            let content = format!("**Function**\n\n```vibe\n{}{}\n```", visibility, sig);
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }

        // Struct
        if let Some(struct_info) = doc.symbols.structs.get(&word) {
            let generics_str = if struct_info.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", struct_info.generics.join(", "))
            };
            let fields_str: Vec<_> = struct_info
                .fields
                .iter()
                .map(|(n, t, is_pub)| {
                    let vis = if *is_pub { "pub " } else { "" };
                    format!("    {}{}: {}", vis, n, t)
                })
                .collect();
            let visibility = if struct_info.is_pub { "pub " } else { "" };
            let content = format!(
                "**Struct**\n\n```vibe\n{}struct {}{} {{\n{}\n}}\n```",
                visibility,
                word,
                generics_str,
                fields_str.join(",\n")
            );
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }

        // Enum
        if let Some(enum_info) = doc.symbols.enums.get(&word) {
            let generics_str = if enum_info.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", enum_info.generics.join(", "))
            };
            let variants_str: Vec<_> = enum_info
                .variants
                .iter()
                .map(|v| match &v.fields {
                    VariantFieldsData::Unit => format!("    {}", v.name),
                    VariantFieldsData::Tuple(types) => {
                        format!("    {}({})", v.name, types.join(", "))
                    }
                    VariantFieldsData::Struct(fields) => {
                        let field_strs: Vec<_> =
                            fields.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                        format!("    {} {{ {} }}", v.name, field_strs.join(", "))
                    }
                })
                .collect();
            let visibility = if enum_info.is_pub { "pub " } else { "" };
            let content = format!(
                "**Enum**\n\n```vibe\n{}enum {}{} {{\n{}\n}}\n```",
                visibility,
                word,
                generics_str,
                variants_str.join(",\n")
            );
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: content,
                }),
                range: None,
            });
        }

        // Primitive types
        if is_builtin_type(&word) {
            let desc = match word.as_str() {
                "i8" => "8-bit signed integer (-128 to 127)",
                "i16" => "16-bit signed integer",
                "i32" => "32-bit signed integer",
                "i64" => "64-bit signed integer",
                "u8" => "8-bit unsigned integer (0 to 255)",
                "u16" => "16-bit unsigned integer",
                "u32" => "32-bit unsigned integer",
                "u64" => "64-bit unsigned integer",
                "f32" => "32-bit floating point",
                "f64" => "64-bit floating point",
                "bool" => "Boolean type (true or false)",
                "void" => "Unit type (no value)",
                _ => "Primitive type",
            };
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("**Primitive Type** `{}`\n\n{}", word, desc),
                }),
                range: None,
            });
        }

        None
    }

    pub fn get_definition(&self, doc: &DocumentInfo, uri: &Uri, position: Position) -> Option<Location> {
        let word = self.get_word_at_position(&doc.text, position)?;
        let offset = self.position_to_offset(&doc.text, position);

        // Variable
        for var in &doc.symbols.variables {
            if var.name == word && var.scope_start <= offset && offset <= var.scope_end {
                return Some(Location {
                    uri: uri.clone(),
                    range: self.span_to_range(&var.span),
                });
            }
        }

        // Function
        if let Some(func) = doc.symbols.functions.get(&word) {
            return Some(Location {
                uri: uri.clone(),
                range: self.span_to_range(&func.span),
            });
        }

        // Struct
        if let Some(struct_info) = doc.symbols.structs.get(&word) {
            return Some(Location {
                uri: uri.clone(),
                range: self.span_to_range(&struct_info.span),
            });
        }

        // Enum
        if let Some(enum_info) = doc.symbols.enums.get(&word) {
            return Some(Location {
                uri: uri.clone(),
                range: self.span_to_range(&enum_info.span),
            });
        }

        None
    }

    pub fn infer_type_from_expr(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::StructInit { name, generics, .. } => {
                if generics.is_empty() {
                    Some(name.clone())
                } else {
                    let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                    Some(format!("{}<{}>", name, generic_strs.join(", ")))
                }
            }
            Expr::Call { func, .. } => {
                if let Expr::Field { object, field, .. } = func.as_ref() {
                    if let Expr::Ident(enum_name, _) = object.as_ref() {
                        return Some(enum_name.clone());
                    }
                    Some(field.clone())
                } else {
                    None
                }
            }
            Expr::ArrayInit { elements, .. } => {
                if let Some(first) = elements.first() {
                    if let Some(elem_ty) = self.infer_type_from_expr(first) {
                        return Some(format!("{}[{}]", elem_ty, elements.len()));
                    }
                }
                None
            }
            Expr::Ref { operand, .. } => {
                self.infer_type_from_expr(operand).map(|t| format!("&{}", t))
            }
            Expr::RefMut { operand, .. } => {
                self.infer_type_from_expr(operand).map(|t| format!("~{}", t))
            }
            Expr::Literal(lit, _) => match lit {
                Literal::Int(_) => Some("i32".to_string()),
                Literal::Float(_) => Some("f64".to_string()),
                Literal::Bool(_) => Some("bool".to_string()),
                Literal::String(_) => Some("Slice<u8>".to_string()),
            },
            _ => None,
        }
    }

    pub fn get_word_at_position(&self, text: &str, position: Position) -> Option<String> {
        let line = text.lines().nth(position.line as usize)?;
        let col = position.character as usize;

        if col > line.len() {
            return None;
        }

        let chars: Vec<char> = line.chars().collect();
        let mut start = col;
        let mut end = col;

        while start > 0 && is_ident_char(chars[start - 1]) {
            start -= 1;
        }

        while end < chars.len() && is_ident_char(chars[end]) {
            end += 1;
        }

        if start == end {
            return None;
        }

        Some(chars[start..end].iter().collect())
    }
}
