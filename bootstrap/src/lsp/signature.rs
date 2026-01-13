//! Signature help for the Vibelang LSP

use tower_lsp_server::ls_types::{
    ParameterInformation, ParameterLabel, Position, SignatureHelp, SignatureInformation,
};

use crate::lsp::types::DocumentInfo;
use crate::lsp::Backend;

impl Backend {
    pub fn get_signature_help(&self, doc: &DocumentInfo, position: Position) -> Option<SignatureHelp> {
        let line = doc.text.lines().nth(position.line as usize)?;
        let col = position.character as usize;
        let prefix = &line[..col.min(line.len())];

        let mut paren_depth = 0;
        let mut func_end = None;
        let mut active_param = 0u32;

        for (i, c) in prefix.char_indices().rev() {
            match c {
                ')' => paren_depth += 1,
                '(' => {
                    if paren_depth == 0 {
                        func_end = Some(i);
                        break;
                    }
                    paren_depth -= 1;
                }
                ',' if paren_depth == 0 => active_param += 1,
                _ => {}
            }
        }

        let func_end = func_end?;
        let before_paren = &prefix[..func_end];

        let func_name: String = before_paren
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        if func_name.is_empty() {
            return None;
        }

        if let Some(func) = doc.symbols.functions.get(&func_name) {
            let params: Vec<ParameterInformation> = func
                .params
                .iter()
                .map(|(name, ty)| ParameterInformation {
                    label: ParameterLabel::Simple(format!("{}: {}", name, ty)),
                    documentation: None,
                })
                .collect();

            let params_str: Vec<_> = func
                .params
                .iter()
                .map(|(n, t)| format!("{}: {}", n, t))
                .collect();

            let signature = SignatureInformation {
                label: format!(
                    "fn {}({}) -> {}",
                    func_name,
                    params_str.join(", "),
                    func.return_type.as_deref().unwrap_or("void")
                ),
                documentation: None,
                parameters: Some(params),
                active_parameter: Some(active_param),
            };

            return Some(SignatureHelp {
                signatures: vec![signature],
                active_signature: Some(0),
                active_parameter: Some(active_param),
            });
        }

        None
    }
}
