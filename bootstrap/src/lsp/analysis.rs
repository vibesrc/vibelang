//! Import processing for the Vibelang LSP
//!
//! This module handles loading and resolving imports from the std library.
//! Semantic analysis is now handled by the shared `analysis` module.

use std::path::PathBuf;
use crate::ast::{ImportPrefix, ImportItems};
use crate::lexer::Lexer;
use crate::parser::Parser;

use crate::lsp::types::{
    ImportedItem, ImportedItemKind, SymbolTable,
};
use crate::lsp::Backend;

impl Backend {
    /// Process an import statement and load symbols from the imported module
    pub fn process_import(&self, use_stmt: &crate::ast::Use, symbols: &mut SymbolTable) {
        // Only handle std imports for now
        if use_stmt.prefix != ImportPrefix::Std {
            return;
        }

        let module_path = format!("std.{}", use_stmt.path.join("."));

        // Try to load the module file
        if let Some(module_symbols) = self.load_std_module(&use_stmt.path) {
            match &use_stmt.items {
                ImportItems::Named(items) => {
                    for item in items {
                        let local_name = item.alias.clone().unwrap_or_else(|| item.name.clone());

                        // Check if this item exists in the module
                        if let Some(struct_info) = module_symbols.structs.get(&item.name) {
                            if struct_info.is_pub {
                                symbols.imports.insert(local_name.clone(), ImportedItem {
                                    name: item.name.clone(),
                                    alias: item.alias.clone(),
                                    module_path: module_path.clone(),
                                    kind: ImportedItemKind::Struct(struct_info.clone()),
                                });
                                // Also add to structs for type checking
                                symbols.structs.insert(local_name.clone(), struct_info.clone());
                                // Also copy methods for this struct type
                                if let Some(methods) = module_symbols.methods.get(&item.name) {
                                    symbols.methods.entry(local_name.clone()).or_default().extend(methods.clone());
                                }
                            }
                        } else if let Some(enum_info) = module_symbols.enums.get(&item.name) {
                            if enum_info.is_pub {
                                symbols.imports.insert(local_name.clone(), ImportedItem {
                                    name: item.name.clone(),
                                    alias: item.alias.clone(),
                                    module_path: module_path.clone(),
                                    kind: ImportedItemKind::Enum(enum_info.clone()),
                                });
                                // Also add to enums for type checking
                                symbols.enums.insert(local_name.clone(), enum_info.clone());
                                // Also copy methods for this enum type
                                if let Some(methods) = module_symbols.methods.get(&item.name) {
                                    symbols.methods.entry(local_name.clone()).or_default().extend(methods.clone());
                                }
                            }
                        } else if let Some(func_info) = module_symbols.functions.get(&item.name) {
                            if func_info.is_pub {
                                symbols.imports.insert(local_name.clone(), ImportedItem {
                                    name: item.name.clone(),
                                    alias: item.alias.clone(),
                                    module_path: module_path.clone(),
                                    kind: ImportedItemKind::Function(func_info.clone()),
                                });
                                // Also add to functions for call resolution
                                symbols.functions.insert(local_name.clone(), func_info.clone());
                            }
                        }
                    }
                }
                ImportItems::Glob => {
                    // Import all public items
                    for (name, struct_info) in &module_symbols.structs {
                        if struct_info.is_pub {
                            symbols.imports.insert(name.clone(), ImportedItem {
                                name: name.clone(),
                                alias: None,
                                module_path: module_path.clone(),
                                kind: ImportedItemKind::Struct(struct_info.clone()),
                            });
                            symbols.structs.insert(name.clone(), struct_info.clone());
                        }
                    }
                    for (name, enum_info) in &module_symbols.enums {
                        if enum_info.is_pub {
                            symbols.imports.insert(name.clone(), ImportedItem {
                                name: name.clone(),
                                alias: None,
                                module_path: module_path.clone(),
                                kind: ImportedItemKind::Enum(enum_info.clone()),
                            });
                            symbols.enums.insert(name.clone(), enum_info.clone());
                        }
                    }
                    for (name, func_info) in &module_symbols.functions {
                        if func_info.is_pub {
                            symbols.imports.insert(name.clone(), ImportedItem {
                                name: name.clone(),
                                alias: None,
                                module_path: module_path.clone(),
                                kind: ImportedItemKind::Function(func_info.clone()),
                            });
                            symbols.functions.insert(name.clone(), func_info.clone());
                        }
                    }
                    // Also copy methods
                    for (type_name, methods) in &module_symbols.methods {
                        symbols.methods.entry(type_name.clone()).or_default().extend(methods.clone());
                    }
                }
                ImportItems::Module => {
                    // Import the module as a namespace (e.g., `use std.fs` allows `fs.File`)
                    if let Some(module_name) = use_stmt.path.last() {
                        // Store the module's symbol table under its name
                        symbols.module_aliases.insert(module_name.clone(), Box::new(module_symbols.clone()));
                    }
                }
            }
        }
    }

    /// Load and parse a std library module, returning its symbol table
    fn load_std_module(&self, path: &[String]) -> Option<SymbolTable> {
        let std_src_path = self.get_std_library_path()?;
        // Get the std root (parent of src)
        let stdlib_path = std_src_path.parent().map(|p| p.to_path_buf());

        // Build the file path: std/src/module/mod.vibe
        let mut file_path = std_src_path.clone();
        for component in path {
            file_path.push(component);
        }
        file_path.push("mod.vibe");

        // Read and parse the file
        let content = std::fs::read_to_string(&file_path).ok()?;
        let mut lexer = Lexer::new(&content);
        let tokens = lexer.tokenize().ok()?;
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().ok()?;

        // Use the shared analyzer with the stdlib path for import resolution
        let source_dir = file_path.parent().map(|p| p.to_path_buf());
        let analyzer = crate::analysis::SemanticAnalyzer::with_paths(
            stdlib_path,
            source_dir,
            None, // No project root for std modules
        );
        let result = analyzer.analyze(&program);

        Some(result.symbols)
    }

    /// Get the path to the std library src directory
    fn get_std_library_path(&self) -> Option<PathBuf> {
        // First, check environment variable (highest priority)
        if let Ok(path) = std::env::var("VIBELANG_STD") {
            let path = PathBuf::from(path).join("src");
            if path.exists() {
                return Some(path);
            }
        }

        // Also check legacy VIBELANG_STDLIB for backwards compatibility
        if let Ok(path) = std::env::var("VIBELANG_STDLIB") {
            let path = PathBuf::from(path).join("src");
            if path.exists() {
                return Some(path);
            }
        }

        // Try relative to current executable
        if let Ok(exe) = std::env::current_exe() {
            if let Some(exe_dir) = exe.parent() {
                // Check ../std (installed layout)
                let stdlib = exe_dir.join("../std/src");
                if stdlib.exists() {
                    return stdlib.canonicalize().ok();
                }

                // Check ../../std (dev layout - from bootstrap/target/release)
                let stdlib = exe_dir.join("../../std/src");
                if stdlib.exists() {
                    return stdlib.canonicalize().ok();
                }

                // Check ../../../std (dev layout - from bootstrap/target/debug or release)
                let stdlib = exe_dir.join("../../../std/src");
                if stdlib.exists() {
                    return stdlib.canonicalize().ok();
                }
            }
        }

        None
    }
}
