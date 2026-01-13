//! Module system handling for code generation

use super::{Codegen, CodegenError};
use crate::ast::*;
use crate::parser::Parser;

impl<'ctx> Codegen<'ctx> {
    /// Load a module from a file
    pub(crate) fn load_module(&mut self, mod_name: &str, _is_pub: bool) -> Result<(), CodegenError> {
        // Build module path string for tracking
        let mut module_path = self.current_module_path.clone();
        module_path.push(mod_name.to_string());
        let module_path_str = module_path.join(".");

        // Check if already loaded
        if self.loaded_modules.contains(&module_path_str) {
            return Ok(());
        }
        self.loaded_modules.insert(module_path_str.clone());

        // Find the module file
        let source_dir = self.source_dir.clone()
            .ok_or_else(|| CodegenError::NotImplemented("source_dir not set for module loading".to_string()))?;

        // Try <mod_name>.vibe first, then <mod_name>/mod.vibe
        let file_path = source_dir.join(format!("{}.vibe", mod_name));
        let dir_mod_path = source_dir.join(mod_name).join("mod.vibe");

        let is_dir_module = dir_mod_path.exists();
        let actual_path = if file_path.exists() {
            file_path.clone()
        } else if is_dir_module {
            dir_mod_path.clone()
        } else {
            return Err(CodegenError::NotImplemented(
                format!("module '{}' not found (tried {} and {})",
                    mod_name, file_path.display(), dir_mod_path.display())
            ));
        };

        // Read and parse the module file
        let source = std::fs::read_to_string(&actual_path)
            .map_err(|e| CodegenError::NotImplemented(format!("failed to read module '{}': {}", mod_name, e)))?;

        let module_program = Parser::parse(&source)
            .map_err(|e| CodegenError::NotImplemented(format!("failed to parse module '{}': {}", mod_name, e)))?;

        // Save current state
        let old_module_path = self.current_module_path.clone();
        let old_source_dir = self.source_dir.clone();

        // Set new module context
        self.current_module_path = module_path.clone();
        // Update source_dir for nested modules (directory modules)
        if is_dir_module {
            self.source_dir = Some(source_dir.join(mod_name));
        }

        // Track public items from this module
        let mut public_items = Vec::new();
        for item in &module_program.items {
            match item {
                Item::Function(f) if f.is_pub => public_items.push(f.name.clone()),
                Item::Struct(s) if s.is_pub => public_items.push(s.name.clone()),
                Item::Enum(e) if e.is_pub => public_items.push(e.name.clone()),
                Item::Static(s) if s.is_pub => public_items.push(s.name.clone()),
                _ => {}
            }
        }
        self.module_items.insert(module_path_str.clone(), public_items);

        // Compile the module items
        self.compile(&module_program)?;

        // Restore state
        self.current_module_path = old_module_path;
        self.source_dir = old_source_dir;

        Ok(())
    }

    /// Process a use declaration
    pub(crate) fn process_use(&mut self, use_decl: &Use) -> Result<(), CodegenError> {
        // The use path is like ["module", "submodule", "Item"]
        // We import the last element as an alias to the full path
        if use_decl.path.is_empty() {
            return Ok(());
        }

        let item_name = use_decl.path.last().unwrap();
        let alias = use_decl.alias.as_ref().unwrap_or(item_name);

        // For now, we'll use the full path with underscores for qualified lookups
        let qualified_name = use_decl.path.join("_");

        self.imports.insert(alias.clone(), qualified_name);
        Ok(())
    }
}
