//! Completion handling for the Vibelang LSP

use tower_lsp_server::ls_types::{
    CompletionItem, CompletionItemKind, InsertTextFormat, Position,
};

use crate::lsp::types::{DocumentInfo, VariantFieldsData};
use crate::lsp::Backend;

impl Backend {
    /// Check if position is inside a string interpolation ${...} expression.
    /// Returns Some(prefix_inside_interpolation) if inside, None otherwise.
    pub fn get_interpolation_context(&self, line: &str, column: usize) -> Option<String> {
        let prefix = &line[..column.min(line.len())];

        let mut in_string = false;
        let mut interpolation_start: Option<usize> = None;
        let mut brace_depth = 0;
        let mut i = 0;
        let chars: Vec<char> = prefix.chars().collect();

        while i < chars.len() {
            let c = chars[i];

            if !in_string {
                if c == '"' {
                    in_string = true;
                }
            } else {
                if interpolation_start.is_some() {
                    if c == '{' {
                        brace_depth += 1;
                    } else if c == '}' {
                        if brace_depth > 0 {
                            brace_depth -= 1;
                        } else {
                            interpolation_start = None;
                        }
                    }
                } else {
                    if c == '"' {
                        in_string = false;
                    } else if c == '$' && i + 1 < chars.len() && chars[i + 1] == '{' {
                        interpolation_start = Some(i + 2);
                        i += 1;
                    } else if c == '$' && i + 1 < chars.len() && chars[i + 1] == '$' {
                        i += 1;
                    }
                }
            }
            i += 1;
        }

        if let Some(start) = interpolation_start {
            Some(prefix[start..].to_string())
        } else {
            None
        }
    }

    /// Get expression completions (used inside string interpolation)
    pub fn get_expression_completions(
        &self,
        doc: &DocumentInfo,
        offset: usize,
        _prefix: &str,
    ) -> Vec<CompletionItem> {
        let mut completions = Vec::new();

        // Variables in scope
        for var in &doc.symbols.variables {
            if var.scope_start <= offset && offset <= var.scope_end {
                completions.push(CompletionItem {
                    label: var.name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: var.ty.clone(),
                    ..Default::default()
                });
            }
        }

        // Functions
        for (name, func) in &doc.symbols.functions {
            let params_str: Vec<_> = func
                .params
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
                .collect();
            let sig = format!(
                "fn {}({}) -> {}",
                name,
                params_str.join(", "),
                func.return_type.as_deref().unwrap_or("void")
            );
            completions.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(sig),
                ..Default::default()
            });
        }

        completions
    }

    pub fn get_completions(&self, doc: &DocumentInfo, position: Position) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        let offset = self.position_to_offset(&doc.text, position);

        let line = doc.text.lines().nth(position.line as usize).unwrap_or("");
        let column = position.character as usize;
        let prefix = &line[..column.min(line.len())];

        // Check if we're inside a string interpolation ${...}
        if let Some(interp_prefix) = self.get_interpolation_context(line, column) {
            return self.get_expression_completions(doc, offset, &interp_prefix);
        }

        // After dot: show struct fields, methods, enum variants
        if prefix.trim_end().ends_with('.') {
            let before_dot = prefix.trim_end().strip_suffix('.').unwrap_or("");
            let expr_name = before_dot.split_whitespace().last().unwrap_or("");

            // Enum variants
            if let Some(enum_info) = doc.symbols.enums.get(expr_name) {
                for variant in &enum_info.variants {
                    let detail = match &variant.fields {
                        VariantFieldsData::Unit => format!("{}.{}", expr_name, variant.name),
                        VariantFieldsData::Tuple(types) => {
                            format!("{}.{}({})", expr_name, variant.name, types.join(", "))
                        }
                        VariantFieldsData::Struct(fields) => {
                            let field_strs: Vec<_> =
                                fields.iter().map(|(n, t)| format!("{}: {}", n, t)).collect();
                            format!(
                                "{}.{} {{ {} }}",
                                expr_name,
                                variant.name,
                                field_strs.join(", ")
                            )
                        }
                    };
                    completions.push(CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail: Some(detail),
                        insert_text: Some(match &variant.fields {
                            VariantFieldsData::Unit => variant.name.clone(),
                            VariantFieldsData::Tuple(types) => {
                                let placeholders: Vec<_> = types
                                    .iter()
                                    .enumerate()
                                    .map(|(i, t)| format!("${{{}:{}}}", i + 1, t))
                                    .collect();
                                format!("{}({})", variant.name, placeholders.join(", "))
                            }
                            VariantFieldsData::Struct(fields) => {
                                let placeholders: Vec<_> = fields
                                    .iter()
                                    .enumerate()
                                    .map(|(i, (n, t))| format!("{}: ${{{}:{}}}", n, i + 1, t))
                                    .collect();
                                format!("{} {{ {} }}", variant.name, placeholders.join(", "))
                            }
                        }),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        ..Default::default()
                    });
                }
            }

            // Methods for type
            if let Some(methods) = doc.symbols.methods.get(expr_name) {
                for method in methods {
                    let params_str: Vec<_> = method
                        .params
                        .iter()
                        .map(|(n, t)| format!("{}: {}", n, t))
                        .collect();
                    let sig = format!(
                        "fn {}({}) -> {}",
                        method.name,
                        params_str.join(", "),
                        method.return_type.as_deref().unwrap_or("void")
                    );
                    completions.push(CompletionItem {
                        label: method.name.clone(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail: Some(sig),
                        ..Default::default()
                    });
                }
            }

            // Struct fields for variables
            if let Some(var) = doc.symbols.variables.iter().find(|v| v.name == expr_name) {
                if let Some(ref ty_str) = var.ty {
                    let stripped = ty_str.trim_start_matches('&').trim_start_matches('~').trim_start_matches('*');
                    let base_type = stripped.split('<').next().unwrap_or(stripped);
                    if let Some(struct_info) = doc.symbols.structs.get(base_type.trim()) {
                        for (field_name, field_type, _) in &struct_info.fields {
                            completions.push(CompletionItem {
                                label: field_name.clone(),
                                kind: Some(CompletionItemKind::FIELD),
                                detail: Some(field_type.clone()),
                                ..Default::default()
                            });
                        }
                    }
                    if let Some(methods) = doc.symbols.methods.get(base_type.trim()) {
                        for method in methods {
                            completions.push(CompletionItem {
                                label: method.name.clone(),
                                kind: Some(CompletionItemKind::METHOD),
                                detail: Some(format!("fn {}(...)", method.name)),
                                ..Default::default()
                            });
                        }
                    }
                }
            }
        } else if let Some(struct_name) = self.find_struct_init_context(&doc.text, position) {
            // Inside struct initializer: show struct fields
            if let Some(struct_info) = doc.symbols.structs.get(&struct_name) {
                let line = doc.text.lines().nth(position.line as usize).unwrap_or("");
                let used_fields: Vec<&str> = line
                    .split(',')
                    .filter_map(|part| {
                        let trimmed = part.trim();
                        if trimmed.contains(':') {
                            Some(trimmed.split(':').next().unwrap().trim())
                        } else {
                            None
                        }
                    })
                    .collect();

                for (field_name, field_type, _) in &struct_info.fields {
                    if !used_fields.contains(&field_name.as_str()) {
                        completions.push(CompletionItem {
                            label: field_name.clone(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some(field_type.clone()),
                            insert_text: Some(format!("{}: ", field_name)),
                            ..Default::default()
                        });
                    }
                }
            }
            return completions;
        } else {
            // After & or ~: show borrowable variables
            let trimmed = prefix.trim_end();
            let after_borrow = trimmed.ends_with('&') || trimmed.ends_with('~');

            if after_borrow {
                for var in &doc.symbols.variables {
                    if var.scope_start <= offset && offset <= var.scope_end {
                        completions.push(CompletionItem {
                            label: var.name.clone(),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: var.ty.clone(),
                            ..Default::default()
                        });
                    }
                }
            } else {
                // General completion: variables, functions, types, keywords

                // Variables in scope
                for var in &doc.symbols.variables {
                    if var.scope_start <= offset && offset <= var.scope_end {
                        completions.push(CompletionItem {
                            label: var.name.clone(),
                            kind: Some(CompletionItemKind::VARIABLE),
                            detail: var.ty.clone(),
                            ..Default::default()
                        });
                    }
                }

                // Functions
                for (name, func) in &doc.symbols.functions {
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
                        name,
                        generics_str,
                        params_str.join(", "),
                        func.return_type.as_deref().unwrap_or("void")
                    );
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(sig),
                        ..Default::default()
                    });
                }

                // Structs
                for (name, struct_info) in &doc.symbols.structs {
                    let generics_str = if struct_info.generics.is_empty() {
                        String::new()
                    } else {
                        format!("<{}>", struct_info.generics.join(", "))
                    };
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::STRUCT),
                        detail: Some(format!("struct {}{}", name, generics_str)),
                        ..Default::default()
                    });
                }

                // Enums
                for (name, enum_info) in &doc.symbols.enums {
                    let generics_str = if enum_info.generics.is_empty() {
                        String::new()
                    } else {
                        format!("<{}>", enum_info.generics.join(", "))
                    };
                    completions.push(CompletionItem {
                        label: name.clone(),
                        kind: Some(CompletionItemKind::ENUM),
                        detail: Some(format!("enum {}{}", name, generics_str)),
                        ..Default::default()
                    });
                }

                // Keywords
                for kw in &[
                    "let", "fn", "struct", "enum", "impl", "if", "else", "match", "while", "for",
                    "in", "return", "break", "continue", "defer", "pub", "use", "mod", "and",
                    "or", "not", "true", "false",
                ] {
                    completions.push(CompletionItem {
                        label: kw.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    });
                }

                // Primitive types
                for ty in &[
                    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "bool",
                    "void",
                ] {
                    completions.push(CompletionItem {
                        label: ty.to_string(),
                        kind: Some(CompletionItemKind::TYPE_PARAMETER),
                        ..Default::default()
                    });
                }
            }
        }

        completions
    }

    pub fn find_struct_init_context(&self, text: &str, position: Position) -> Option<String> {
        let lines: Vec<&str> = text.lines().collect();
        let current_line = position.line as usize;
        let col = position.character as usize;

        let mut search_text = String::new();
        for (i, line) in lines.iter().enumerate() {
            if i < current_line {
                search_text.push_str(line);
                search_text.push('\n');
            } else if i == current_line {
                search_text.push_str(&line[..col.min(line.len())]);
                break;
            }
        }

        let mut brace_stack: Vec<usize> = Vec::new();

        for (i, c) in search_text.char_indices() {
            match c {
                '{' => brace_stack.push(i),
                '}' => { brace_stack.pop(); }
                _ => {}
            }
        }

        if let Some(&brace_pos) = brace_stack.last() {
            let before_brace = &search_text[..brace_pos];
            let struct_name: String = before_brace
                .trim_end()
                .chars()
                .rev()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect::<String>()
                .chars()
                .rev()
                .collect();

            if !struct_name.is_empty() && struct_name.chars().next().unwrap().is_uppercase() {
                return Some(struct_name);
            }
        }

        None
    }
}
