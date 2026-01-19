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

        // Method - check if preceded by a dot
        if let Some(method_hover) = self.get_method_hover(doc, position, &word) {
            return Some(method_hover);
        }

        // Variable
        for var in &doc.symbols.variables {
            if var.name == word && var.scope_start <= offset && offset <= var.scope_end {
                let borrow_info = match var.borrow_state {
                    BorrowState::Owned => "owned",
                    BorrowState::Borrowed => "borrowed (&)",
                    BorrowState::MutBorrowed => "mutably borrowed (~)",
                    BorrowState::Moved => "moved",
                };
                let ty_str = var.ty.as_deref().unwrap_or("unknown");
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
                "char" => "8-bit character (ASCII)",
                "str" => "UTF-8 string slice (fat pointer: ptr + len)",
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

    /// Get hover info for a method call (word preceded by a dot)
    fn get_method_hover(&self, doc: &DocumentInfo, position: Position, word: &str) -> Option<Hover> {
        // Get the line and check if word is preceded by a dot
        let line = doc.text.lines().nth(position.line as usize)?;
        let col = position.character as usize;
        let chars: Vec<char> = line.chars().collect();

        // Find start of the word
        let mut start = col;
        while start > 0 && is_ident_char(chars[start - 1]) {
            start -= 1;
        }

        // Check if preceded by a dot
        if start == 0 || chars[start - 1] != '.' {
            return None;
        }

        // Find the receiver name (before the dot)
        let mut receiver_end = start - 1;
        let mut receiver_start = receiver_end;

        // Skip whitespace
        while receiver_start > 0 && chars[receiver_start - 1].is_whitespace() {
            receiver_start -= 1;
            receiver_end = receiver_start;
        }

        // Get the receiver identifier or closing paren/bracket
        while receiver_start > 0 && is_ident_char(chars[receiver_start - 1]) {
            receiver_start -= 1;
        }

        let receiver_name: String = chars[receiver_start..receiver_end].iter().collect();

        if receiver_name.is_empty() {
            // Search all methods in the symbol table
            return self.find_method_in_all_types(doc, word);
        }

        // Try to find the receiver's type
        let receiver_type = self.find_receiver_type(doc, &receiver_name, position);

        if let Some(ref ty) = receiver_type {
            // Extract base type name (strip generics and refs)
            let clean_ty = ty
                .trim_start_matches('&')
                .trim_start_matches('~')
                .trim_start_matches('*');
            let base_type = clean_ty
                .split('<')
                .next()
                .unwrap_or(clean_ty);

            // Look up method in the type's methods
            if let Some(methods) = doc.symbols.methods.get(base_type) {
                if let Some(method_info) = methods.iter().find(|m| m.name == word) {
                    return Some(self.format_method_hover_with_generics(doc, base_type, clean_ty, method_info));
                }
            }
        }

        // Fallback: search all types for this method
        self.find_method_in_all_types(doc, word)
    }

    /// Find receiver's type by looking it up in variables
    fn find_receiver_type(&self, doc: &DocumentInfo, name: &str, position: Position) -> Option<String> {
        let offset = self.position_to_offset(&doc.text, position);

        // Look up variable
        for var in &doc.symbols.variables {
            if var.name == name && var.scope_start <= offset && offset <= var.scope_end {
                return var.ty.clone();
            }
        }

        // Check if it's a type name (static method call)
        if doc.symbols.structs.contains_key(name) || doc.symbols.enums.contains_key(name) {
            return Some(name.to_string());
        }

        None
    }

    /// Search for a method name in all types
    fn find_method_in_all_types(&self, doc: &DocumentInfo, method_name: &str) -> Option<Hover> {
        for (type_name, methods) in &doc.symbols.methods {
            if let Some(method_info) = methods.iter().find(|m| m.name == method_name) {
                return Some(self.format_method_hover(type_name, method_info));
            }
        }
        None
    }

    /// Format a method as hover info with generic type substitution
    fn format_method_hover_with_generics(
        &self,
        doc: &DocumentInfo,
        base_type: &str,
        full_type: &str,
        method_info: &crate::lsp::types::MethodInfo
    ) -> Hover {
        // Build substitution map from generic params to concrete types
        let substitutions = self.build_generic_substitutions(doc, base_type, full_type);

        let params_str: Vec<_> = method_info
            .params
            .iter()
            .map(|(n, t)| {
                let resolved_type = self.substitute_generics(t, &substitutions);
                format!("{}: {}", n, resolved_type)
            })
            .collect();

        let return_type = method_info.return_type.as_deref().unwrap_or("void");
        let resolved_return = self.substitute_generics(return_type, &substitutions);

        let sig = format!(
            "fn {}({}) -> {}",
            method_info.name,
            params_str.join(", "),
            resolved_return
        );

        let content = format!("**Method** on `{}`\n\n```vibe\n{}\n```", full_type, sig);

        Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }
    }

    /// Format a method as hover info (without generic resolution)
    fn format_method_hover(&self, type_name: &str, method_info: &crate::lsp::types::MethodInfo) -> Hover {
        let params_str: Vec<_> = method_info
            .params
            .iter()
            .map(|(n, t)| format!("{}: {}", n, t))
            .collect();

        let return_type = method_info.return_type.as_deref().unwrap_or("void");

        let sig = format!(
            "fn {}({}) -> {}",
            method_info.name,
            params_str.join(", "),
            return_type
        );

        let content = format!("**Method** on `{}`\n\n```vibe\n{}\n```", type_name, sig);

        Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: content,
            }),
            range: None,
        }
    }

    /// Build a map from generic type parameters to concrete types
    /// e.g., for Map<str, i64> with generics [K, V], returns {K -> str, V -> i64}
    fn build_generic_substitutions(
        &self,
        doc: &DocumentInfo,
        base_type: &str,
        full_type: &str,
    ) -> std::collections::HashMap<String, String> {
        let mut subs = std::collections::HashMap::new();

        // Get the generic parameter names from the struct/enum definition
        let generic_params = if let Some(struct_info) = doc.symbols.structs.get(base_type) {
            &struct_info.generics
        } else if let Some(enum_info) = doc.symbols.enums.get(base_type) {
            &enum_info.generics
        } else {
            return subs;
        };

        if generic_params.is_empty() {
            return subs;
        }

        // Parse the concrete type arguments from full_type (e.g., "Map<str, i64>")
        let concrete_args = self.parse_generic_args(full_type);

        // Build the substitution map
        for (i, param) in generic_params.iter().enumerate() {
            if let Some(concrete) = concrete_args.get(i) {
                subs.insert(param.clone(), concrete.clone());
            }
        }

        subs
    }

    /// Parse generic arguments from a type string like "Map<str, i64>" -> ["str", "i64"]
    fn parse_generic_args(&self, type_str: &str) -> Vec<String> {
        let mut args = Vec::new();

        // Find the first '<' and matching '>'
        if let Some(start) = type_str.find('<') {
            let inner = &type_str[start + 1..];
            if let Some(end) = inner.rfind('>') {
                let args_str = &inner[..end];

                // Split by commas, but respect nested generics
                let mut depth = 0;
                let mut current = String::new();

                for ch in args_str.chars() {
                    match ch {
                        '<' => {
                            depth += 1;
                            current.push(ch);
                        }
                        '>' => {
                            depth -= 1;
                            current.push(ch);
                        }
                        ',' if depth == 0 => {
                            args.push(current.trim().to_string());
                            current = String::new();
                        }
                        _ => current.push(ch),
                    }
                }

                if !current.trim().is_empty() {
                    args.push(current.trim().to_string());
                }
            }
        }

        args
    }

    /// Substitute generic type parameters in a type string
    fn substitute_generics(
        &self,
        type_str: &str,
        subs: &std::collections::HashMap<String, String>,
    ) -> String {
        if subs.is_empty() {
            return type_str.to_string();
        }

        let mut result = type_str.to_string();

        // Sort by length descending to avoid partial replacements (e.g., "K" in "Key")
        let mut keys: Vec<_> = subs.keys().collect();
        keys.sort_by(|a, b| b.len().cmp(&a.len()));

        for key in keys {
            if let Some(value) = subs.get(key) {
                // Replace only whole words (type parameters are usually single letters)
                // Use a simple approach: replace "K" but not "Key"
                result = self.replace_type_param(&result, key, value);
            }
        }

        result
    }

    /// Replace a type parameter with a concrete type, being careful about word boundaries
    fn replace_type_param(&self, type_str: &str, param: &str, replacement: &str) -> String {
        let mut result = String::new();
        let mut chars = type_str.chars().peekable();
        let param_chars: Vec<char> = param.chars().collect();

        while let Some(ch) = chars.next() {
            // Check if this could be the start of the parameter
            if ch == param_chars[0] {
                // Try to match the full parameter
                let mut matched = true;
                let mut temp = String::new();
                temp.push(ch);

                for &p in param_chars.iter().skip(1) {
                    if let Some(&next) = chars.peek() {
                        if next == p {
                            temp.push(chars.next().unwrap());
                        } else {
                            matched = false;
                            break;
                        }
                    } else {
                        matched = false;
                        break;
                    }
                }

                if matched {
                    // Check that it's not part of a larger identifier
                    let is_word_boundary = chars.peek()
                        .map(|&c| !c.is_alphanumeric() && c != '_')
                        .unwrap_or(true);

                    // Also check what came before (we need to look at result)
                    let prev_is_boundary = result.chars().last()
                        .map(|c| !c.is_alphanumeric() && c != '_')
                        .unwrap_or(true);

                    if is_word_boundary && prev_is_boundary {
                        result.push_str(replacement);
                    } else {
                        result.push_str(&temp);
                    }
                } else {
                    result.push_str(&temp);
                }
            } else {
                result.push(ch);
            }
        }

        result
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
        // Wrapper without symbols - limited inference
        self.infer_type_from_expr_with_symbols(expr, None)
    }

    pub fn infer_type_from_expr_with_symbols(
        &self,
        expr: &Expr,
        symbols: Option<&crate::lsp::types::SymbolTable>,
    ) -> Option<String> {
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
                    // Static method call on generic type: Vec<u8>.new()
                    if let Expr::StructInit { name, generics, fields, .. } = object.as_ref() {
                        if fields.is_empty() && !generics.is_empty() {
                            let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                            let full_type = format!("{}<{}>", name, generic_strs.join(", "));

                            // Look up method return type
                            if let Some(syms) = symbols {
                                if let Some(methods) = syms.methods.get(name.as_str()) {
                                    if let Some(method_info) = methods.iter().find(|m| &m.name == field) {
                                        if let Some(ref ret_ty) = method_info.return_type {
                                            if ret_ty == "Self" || ret_ty.starts_with(&format!("{}<", name)) {
                                                return Some(full_type);
                                            }
                                            return Some(ret_ty.clone());
                                        }
                                    }
                                }
                            }
                            return Some(full_type);
                        }
                    }
                    // Enum variant constructor
                    if let Expr::Ident(enum_name, _) = object.as_ref() {
                        return Some(enum_name.clone());
                    }
                    Some(field.clone())
                } else {
                    None
                }
            }
            Expr::MethodCall { receiver, method, .. } => {
                // Static method call on a generic type (e.g., Vec<u8>.new())
                if let Expr::StructInit { name, generics, fields, .. } = receiver.as_ref() {
                    if fields.is_empty() && !generics.is_empty() {
                        // Build the full generic type name
                        let generic_strs: Vec<_> = generics.iter().map(|g| self.type_to_string(g)).collect();
                        let full_type = format!("{}<{}>", name, generic_strs.join(", "));

                        // Look up method return type
                        if let Some(syms) = symbols {
                            if let Some(methods) = syms.methods.get(name.as_str()) {
                                if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                                    if let Some(ref ret_ty) = method_info.return_type {
                                        // Replace Self or generic return types with concrete type
                                        if ret_ty == "Self" || ret_ty.starts_with(&format!("{}<", name)) {
                                            return Some(full_type);
                                        }
                                        return Some(ret_ty.clone());
                                    }
                                }
                            }
                        }
                        // Even without method info, the return type is the generic struct
                        return Some(full_type);
                    }
                }
                // Static method call on a type (e.g., String.from(), Vec.new())
                if let Expr::Ident(type_name, _) = receiver.as_ref() {
                    // Look up the method return type from symbols
                    if let Some(syms) = symbols {
                        if let Some(methods) = syms.methods.get(type_name) {
                            if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                                if let Some(ref ret_ty) = method_info.return_type {
                                    return Some(ret_ty.clone());
                                }
                            }
                        }
                    }
                }
                // For chained method calls on variables, get receiver type first
                if let Some(syms) = symbols {
                    if let Some(receiver_type) = self.infer_type_from_expr_with_symbols(receiver, Some(syms)) {
                        // Strip reference markers to get base type
                        let base_type = receiver_type
                            .trim_start_matches('&')
                            .trim_start_matches('~')
                            .trim_start_matches('*')
                            .to_string();
                        // Look up method on the receiver type
                        if let Some(methods) = syms.methods.get(&base_type) {
                            if let Some(method_info) = methods.iter().find(|m| &m.name == method) {
                                if let Some(ref ret_ty) = method_info.return_type {
                                    return Some(ret_ty.clone());
                                }
                            }
                        }
                    }
                }
                None
            }
            Expr::Ident(name, _) => {
                // Look up variable type from symbols
                if let Some(syms) = symbols {
                    if let Some(var) = syms.variables.iter().find(|v| &v.name == name) {
                        return var.ty.clone();
                    }
                }
                None
            }
            Expr::ArrayInit { elements, .. } => {
                if let Some(first) = elements.first() {
                    if let Some(elem_ty) = self.infer_type_from_expr_with_symbols(first, symbols) {
                        return Some(format!("{}[{}]", elem_ty, elements.len()));
                    }
                }
                None
            }
            Expr::Ref { operand, .. } => {
                self.infer_type_from_expr_with_symbols(operand, symbols).map(|t| format!("&{}", t))
            }
            Expr::RefMut { operand, .. } => {
                self.infer_type_from_expr_with_symbols(operand, symbols).map(|t| format!("~{}", t))
            }
            Expr::Literal(lit, _) => match lit {
                Literal::Int(_, suffix) => {
                    use crate::ast::IntSuffix;
                    Some(match suffix {
                        IntSuffix::I8 => "i8".to_string(),
                        IntSuffix::I16 => "i16".to_string(),
                        IntSuffix::I32 => "i32".to_string(),
                        IntSuffix::I64 => "i64".to_string(),
                        IntSuffix::U8 => "u8".to_string(),
                        IntSuffix::U16 => "u16".to_string(),
                        IntSuffix::U32 => "u32".to_string(),
                        IntSuffix::U64 => "u64".to_string(),
                        IntSuffix::None => "i32".to_string(),
                    })
                }
                Literal::Float(_) => Some("f64".to_string()),
                Literal::Bool(_) => Some("bool".to_string()),
                Literal::Char(_) => Some("char".to_string()),
                Literal::String(_) => Some("str".to_string()),
            },
            Expr::Closure { params, return_type, .. } => {
                // Build closure type signature: (T1, T2) => R
                let param_types: Vec<String> = params.iter().map(|(_, ty)| {
                    ty.as_ref().map(|t| self.type_to_string(t)).unwrap_or_else(|| "?".to_string())
                }).collect();
                let ret_type = return_type.as_ref()
                    .map(|t| self.type_to_string(t))
                    .unwrap_or_else(|| "?".to_string());
                Some(format!("({}) => {}", param_types.join(", "), ret_type))
            }
            Expr::InterpolatedString { .. } => Some("str".to_string()),
            Expr::Match { arms, .. } => {
                // Return type is the type of the first arm body
                if let Some(arm) = arms.first() {
                    return self.infer_type_from_expr_with_symbols(&arm.body, symbols);
                }
                None
            }
            Expr::If { then_expr, .. } => {
                // Return type is the type of the then branch
                self.infer_type_from_expr_with_symbols(then_expr, symbols)
            }
            Expr::Block(block) => {
                // Return type is the type of the last expression
                if let Some(crate::ast::Stmt::Expr(last_expr)) = block.stmts.last() {
                    return self.infer_type_from_expr_with_symbols(last_expr, symbols);
                }
                // Also handle Stmt::Match in last position
                if let Some(crate::ast::Stmt::Match { arms, .. }) = block.stmts.last() {
                    if let Some(arm) = arms.first() {
                        return self.infer_type_from_expr_with_symbols(&arm.body, symbols);
                    }
                }
                None
            }
            Expr::Try { operand, .. } => {
                // Try operator unwraps Result<T, E> or Option<T> to T
                if let Some(ty) = self.infer_type_from_expr_with_symbols(operand, symbols) {
                    if ty.starts_with("Result<") || ty.starts_with("Option<") {
                        if let Some(start) = ty.find('<') {
                            let inner = &ty[start + 1..ty.len() - 1];
                            if let Some(comma) = inner.find(',') {
                                return Some(inner[..comma].trim().to_string());
                            }
                            return Some(inner.to_string());
                        }
                    }
                }
                None
            }
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
