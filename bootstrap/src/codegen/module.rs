//! Module system handling for code generation
//!
//! Implements prefix-based module resolution:
//! - src.* - absolute path from project root's src/ directory
//! - lib.* - local workspace packages from project root's lib/ directory
//! - std.* - standard library
//! - dep.* - vendored external dependencies from project root's dep/ directory
//! - .* - relative to current file's directory

use super::{Codegen, CodegenError};
use crate::ast::*;
use crate::parser::Parser;
use std::path::PathBuf;

impl<'ctx> Codegen<'ctx> {
    /// Process a use declaration with the new prefix-based system
    pub(crate) fn process_use(&mut self, use_decl: &Use) -> Result<(), CodegenError> {
        // Resolve the module file path based on prefix
        let module_path = self.resolve_module_path(use_decl)?;

        // Load the module if not already loaded
        let module_path_str = module_path.to_string_lossy().to_string();
        if !self.loaded_modules.contains(&module_path_str) {
            self.load_module_from_path(&module_path, use_decl.is_pub)?;
        }

        // Register the imported items
        self.register_imports(use_decl, &module_path)?;

        Ok(())
    }

    /// Resolve a module path based on the import prefix
    fn resolve_module_path(&self, use_decl: &Use) -> Result<PathBuf, CodegenError> {
        match use_decl.prefix {
            ImportPrefix::Src => self.resolve_src_import(&use_decl.path),
            ImportPrefix::Lib => self.resolve_lib_import(&use_decl.path),
            ImportPrefix::Std => self.resolve_std_import(&use_decl.path),
            ImportPrefix::Dep => self.resolve_dep_import(&use_decl.path),
            ImportPrefix::Relative => self.resolve_relative_import(&use_decl.path),
        }
    }

    /// Resolve a src.* import (absolute from project root)
    fn resolve_src_import(&self, path: &[String]) -> Result<PathBuf, CodegenError> {
        // Require vibe.toml project
        let src_dir = self.project.src_dir().ok_or_else(|| {
            CodegenError::NotImplemented(
                "src. imports require a vibe.toml project".to_string()
            )
        })?;

        self.find_module_file(&src_dir, path)
    }

    /// Resolve a lib.* import (local workspace packages)
    /// lib.X.Y resolves to <project_root>/lib/X/src/Y.vibe
    fn resolve_lib_import(&self, path: &[String]) -> Result<PathBuf, CodegenError> {
        if path.is_empty() {
            return Err(CodegenError::NotImplemented(
                "lib. import requires a package name".to_string()
            ));
        }

        // Require vibe.toml project
        let project_root = self.project.project_root.as_ref().ok_or_else(|| {
            CodegenError::NotImplemented(
                "lib. imports require a vibe.toml project".to_string()
            )
        })?;

        let pkg_name = &path[0];
        let lib_dir = project_root.join("lib").join(pkg_name);

        if !lib_dir.exists() {
            return Err(CodegenError::NotImplemented(
                format!("lib package '{}' not found at {}", pkg_name, lib_dir.display())
            ));
        }

        // Find module within the package's src/ directory
        let pkg_src_dir = lib_dir.join("src");
        let remaining_path = &path[1..];

        if remaining_path.is_empty() {
            // Import from root of package - look for lib.vibe
            let lib_vibe = pkg_src_dir.join("lib.vibe");
            if lib_vibe.exists() {
                return Ok(lib_vibe);
            }
            // Fallback to mod.vibe
            let mod_vibe = pkg_src_dir.join("mod.vibe");
            if mod_vibe.exists() {
                return Ok(mod_vibe);
            }
            Err(CodegenError::NotImplemented(
                format!("lib package '{}' has no src/lib.vibe or src/mod.vibe", pkg_name)
            ))
        } else {
            self.find_module_file(&pkg_src_dir, remaining_path)
        }
    }

    /// Resolve a std.* import (standard library)
    /// std.X resolves to <std_root>/src/X/mod.vibe
    /// std.X.Y resolves to <std_root>/src/X/Y.vibe or <std_root>/src/X/Y/mod.vibe
    fn resolve_std_import(&self, path: &[String]) -> Result<PathBuf, CodegenError> {
        if path.is_empty() {
            return Err(CodegenError::NotImplemented(
                "std. import requires a module name".to_string()
            ));
        }

        let stdlib_path = self.project.stdlib_path.clone().ok_or_else(|| {
            CodegenError::NotImplemented(
                "stdlib not found. Set VIBELANG_STD environment variable".to_string()
            )
        })?;

        // std is a package with src/ directory
        let src_dir = stdlib_path.join("src");

        let mod_name = &path[0];
        let mod_dir = src_dir.join(mod_name);

        // Check if module directory exists (std/src/X/)
        if mod_dir.exists() && mod_dir.is_dir() {
            let remaining_path = &path[1..];

            if remaining_path.is_empty() {
                // Import from root of module - look for mod.vibe
                let mod_vibe = mod_dir.join("mod.vibe");
                if mod_vibe.exists() {
                    return Ok(mod_vibe);
                }
            } else {
                // Look for specific file within the module directory
                let result = self.find_module_file(&mod_dir, remaining_path);
                if result.is_ok() {
                    return result;
                }
            }
        }

        // Fallback to flat file: std/src/X.vibe
        self.find_module_file(&src_dir, path)
    }

    /// Resolve a dep.* import (vendored external dependencies)
    /// dep.X.Y resolves to <project_root>/dep/X/src/Y.vibe
    fn resolve_dep_import(&self, path: &[String]) -> Result<PathBuf, CodegenError> {
        if path.is_empty() {
            return Err(CodegenError::NotImplemented(
                "dep. import requires a dependency name".to_string()
            ));
        }

        // Require vibe.toml project
        let project_root = self.project.project_root.as_ref().ok_or_else(|| {
            CodegenError::NotImplemented(
                "dep. imports require a vibe.toml project".to_string()
            )
        })?;

        let dep_name = &path[0];
        let dep_dir = project_root.join("dep").join(dep_name);

        if !dep_dir.exists() {
            return Err(CodegenError::NotImplemented(
                format!("vendored dependency '{}' not found at {}", dep_name, dep_dir.display())
            ));
        }

        // Find module within the dependency's src/ directory
        let dep_src_dir = dep_dir.join("src");
        let remaining_path = &path[1..];

        if remaining_path.is_empty() {
            // Import from root of dependency - look for lib.vibe
            let lib_vibe = dep_src_dir.join("lib.vibe");
            if lib_vibe.exists() {
                return Ok(lib_vibe);
            }
            // Fallback to mod.vibe
            let mod_vibe = dep_src_dir.join("mod.vibe");
            if mod_vibe.exists() {
                return Ok(mod_vibe);
            }
            Err(CodegenError::NotImplemented(
                format!("vendored dependency '{}' has no src/lib.vibe or src/mod.vibe", dep_name)
            ))
        } else {
            self.find_module_file(&dep_src_dir, remaining_path)
        }
    }

    /// Resolve a .* import (relative to current file)
    fn resolve_relative_import(&self, path: &[String]) -> Result<PathBuf, CodegenError> {
        let source_dir = self.source_dir.clone().ok_or_else(|| {
            CodegenError::NotImplemented(
                "relative imports require source directory context".to_string()
            )
        })?;

        self.find_module_file(&source_dir, path)
    }

    /// Find a module file given a base directory and path segments
    fn find_module_file(&self, base_dir: &PathBuf, path: &[String]) -> Result<PathBuf, CodegenError> {
        if path.is_empty() {
            return Err(CodegenError::NotImplemented(
                "module path cannot be empty".to_string()
            ));
        }

        // Build the path to the module
        let mut current_dir = base_dir.clone();
        for segment in &path[..path.len()-1] {
            current_dir = current_dir.join(segment);
        }

        let last_segment = &path[path.len()-1];

        // Try <name>.vibe first
        let file_path = current_dir.join(format!("{}.vibe", last_segment));
        if file_path.exists() {
            return Ok(file_path);
        }

        // Try <name>/mod.vibe for directory modules
        let dir_mod_path = current_dir.join(last_segment).join("mod.vibe");
        if dir_mod_path.exists() {
            return Ok(dir_mod_path);
        }

        Err(CodegenError::NotImplemented(
            format!(
                "module '{}' not found (tried {} and {})",
                path.join("."),
                file_path.display(),
                dir_mod_path.display()
            )
        ))
    }

    /// Load a module from a file path
    pub(crate) fn load_module_from_path(&mut self, path: &PathBuf, _is_pub: bool) -> Result<(), CodegenError> {
        let path_str = path.to_string_lossy().to_string();

        // Check if already loaded
        if self.loaded_modules.contains(&path_str) {
            return Ok(());
        }
        self.loaded_modules.insert(path_str.clone());

        // Read and parse the module file
        let source = std::fs::read_to_string(path)
            .map_err(|e| CodegenError::NotImplemented(format!("failed to read module '{}': {}", path.display(), e)))?;

        let module_program = Parser::parse(&source)
            .map_err(|e| CodegenError::NotImplemented(format!("failed to parse module '{}': {}", path.display(), e)))?;

        // Save current state
        let old_module_path = self.current_module_path.clone();
        let old_source_dir = self.source_dir.clone();
        let old_file_path = self.current_file_path.clone();

        // Set new module context
        let module_name = path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        self.current_module_path.push(module_name.clone());
        self.source_dir = path.parent().map(|p| p.to_path_buf());
        self.current_file_path = Some(path.clone());

        // Track public items from this module
        let mut public_items = std::collections::HashSet::new();
        for item in &module_program.items {
            match item {
                Item::Function(f) if f.is_pub => { public_items.insert(f.name.clone()); }
                Item::Struct(s) if s.is_pub => { public_items.insert(s.name.clone()); }
                Item::Enum(e) if e.is_pub => { public_items.insert(e.name.clone()); }
                Item::Static(s) if s.is_pub => { public_items.insert(s.name.clone()); }
                // Handle pub use re-exports
                Item::Use(u) if u.is_pub => {
                    match &u.items {
                        ImportItems::Named(items) => {
                            for import_item in items {
                                let name = import_item.alias.as_ref().unwrap_or(&import_item.name);
                                public_items.insert(name.clone());
                            }
                        }
                        ImportItems::Glob => {
                            // For glob re-exports, we'd need to resolve the source module
                            // For now, mark that everything from this module could be public
                        }
                        ImportItems::Module => {
                            // Module re-export - the module name itself is public
                            if let Some(module_name) = u.path.last() {
                                public_items.insert(module_name.clone());
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        self.module_public_items.insert(path_str.clone(), public_items);

        // Compile the module items
        self.compile(&module_program)?;

        // Restore state
        self.current_module_path = old_module_path;
        self.source_dir = old_source_dir;
        self.current_file_path = old_file_path;

        Ok(())
    }

    /// Register imported items as aliases
    fn register_imports(&mut self, use_decl: &Use, module_path: &PathBuf) -> Result<(), CodegenError> {
        let module_path_str = module_path.to_string_lossy().to_string();

        // Get the public items from this module
        let public_items = self.module_public_items.get(&module_path_str).cloned().unwrap_or_default();

        // Build qualified name prefix (for mangled names)
        let qualified_prefix = use_decl.path.join("_");

        match &use_decl.items {
            ImportItems::Named(items) => {
                for item in items {
                    // Check if item is public
                    if !public_items.contains(&item.name) {
                        return Err(CodegenError::NotImplemented(
                            format!("'{}' is not public in module '{}'", item.name, use_decl.path.join("."))
                        ));
                    }

                    // Register alias
                    let alias = item.alias.as_ref().unwrap_or(&item.name);
                    let qualified_name = if qualified_prefix.is_empty() {
                        item.name.clone()
                    } else {
                        format!("{}_{}", qualified_prefix, item.name)
                    };
                    self.imports.insert(alias.clone(), qualified_name);
                }
            }
            ImportItems::Glob => {
                // Import all public items
                for item_name in &public_items {
                    let qualified_name = if qualified_prefix.is_empty() {
                        item_name.clone()
                    } else {
                        format!("{}_{}", qualified_prefix, item_name)
                    };
                    self.imports.insert(item_name.clone(), qualified_name);
                }
            }
            ImportItems::Module => {
                // Import the module as a namespace
                // e.g., `use std.fs` allows `fs.File`, `fs.read_file()`, etc.
                if let Some(module_name) = use_decl.path.last() {
                    // Store the module alias -> qualified prefix mapping
                    self.module_aliases.insert(module_name.clone(), qualified_prefix.clone());

                    // Register all public items: fs_exists -> exists
                    // This maps the namespace-qualified lookup name to the actual function name
                    for item_name in &public_items {
                        let prefixed_name = format!("{}_{}", module_name, item_name);
                        // The actual function is declared with just its name, not prefixed
                        self.imports.insert(prefixed_name, item_name.clone());
                    }
                }
            }
        }

        Ok(())
    }
}
