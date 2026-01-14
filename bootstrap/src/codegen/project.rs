//! Project detection and configuration
//!
//! Handles vibe.toml parsing and project root detection.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Project configuration from vibe.toml
#[derive(Debug, Clone, Default)]
pub struct ProjectConfig {
    /// Project name
    pub name: String,
    /// Project version
    pub version: String,
    /// Dependencies: name -> path/version
    pub dependencies: HashMap<String, DepConfig>,
}

/// Dependency configuration
#[derive(Debug, Clone)]
pub struct DepConfig {
    /// Path to dependency (local path)
    pub path: Option<PathBuf>,
    /// Version requirement (for future package registry)
    pub version: Option<String>,
}

/// Information about the current project context
#[derive(Debug, Clone)]
pub struct ProjectContext {
    /// Path to vibe.toml if found
    pub config_path: Option<PathBuf>,
    /// Project root directory (where vibe.toml is)
    pub project_root: Option<PathBuf>,
    /// Parsed project configuration
    pub config: Option<ProjectConfig>,
    /// Path to stdlib
    pub stdlib_path: Option<PathBuf>,
}

impl ProjectContext {
    /// Create a new project context by searching for vibe.toml starting from the given path
    pub fn discover(start_path: &Path) -> Self {
        // Try to find vibe.toml by walking up directories
        let (config_path, project_root) = find_project_root(start_path);

        // Parse config if found
        let config = config_path.as_ref().and_then(|p| parse_vibe_toml(p).ok());

        // Look for stdlib relative to the compiler binary or in known locations
        let stdlib_path = find_stdlib();

        ProjectContext {
            config_path,
            project_root,
            config,
            stdlib_path,
        }
    }

    /// Check if we're in a project (have vibe.toml)
    pub fn is_project(&self) -> bool {
        self.project_root.is_some()
    }

    /// Get the src directory path
    pub fn src_dir(&self) -> Option<PathBuf> {
        self.project_root.as_ref().map(|root| root.join("src"))
    }

    /// Resolve a dependency path
    pub fn resolve_dep(&self, dep_name: &str) -> Option<PathBuf> {
        self.config.as_ref()
            .and_then(|c| c.dependencies.get(dep_name))
            .and_then(|dep| dep.path.clone())
    }
}

/// Walk up directories to find vibe.toml
fn find_project_root(start: &Path) -> (Option<PathBuf>, Option<PathBuf>) {
    let mut current = if start.is_file() {
        start.parent().map(|p| p.to_path_buf())
    } else {
        Some(start.to_path_buf())
    };

    while let Some(dir) = current {
        let vibe_toml = dir.join("vibe.toml");
        if vibe_toml.exists() {
            return (Some(vibe_toml), Some(dir));
        }
        current = dir.parent().map(|p| p.to_path_buf());
    }

    (None, None)
}

/// Parse a vibe.toml file
fn parse_vibe_toml(path: &Path) -> Result<ProjectConfig, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("failed to read vibe.toml: {}", e))?;

    // Simple TOML parser for our needs
    // Format:
    // [project]
    // name = "myproject"
    // version = "0.1.0"
    //
    // [dependencies]
    // postgres = { path = "../postgres-vibe" }

    let mut config = ProjectConfig::default();
    let mut current_section = String::new();

    for line in content.lines() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Section header
        if line.starts_with('[') && line.ends_with(']') {
            current_section = line[1..line.len()-1].to_string();
            continue;
        }

        // Key-value pair
        if let Some((key, value)) = line.split_once('=') {
            let key = key.trim();
            let value = value.trim();

            match current_section.as_str() {
                "project" => {
                    match key {
                        "name" => config.name = unquote(value),
                        "version" => config.version = unquote(value),
                        _ => {}
                    }
                }
                "dependencies" | "dep" => {
                    // Parse dependency: name = { path = "..." } or name = "version"
                    let dep_config = parse_dep_value(value);
                    config.dependencies.insert(key.to_string(), dep_config);
                }
                _ => {}
            }
        }
    }

    Ok(config)
}

/// Remove quotes from a string value
fn unquote(s: &str) -> String {
    let s = s.trim();
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        s[1..s.len()-1].to_string()
    } else {
        s.to_string()
    }
}

/// Parse a dependency value like { path = "..." } or "version"
fn parse_dep_value(value: &str) -> DepConfig {
    let value = value.trim();

    if value.starts_with('{') && value.ends_with('}') {
        // Inline table: { path = "..." }
        let inner = &value[1..value.len()-1];
        let mut dep = DepConfig { path: None, version: None };

        for part in inner.split(',') {
            if let Some((k, v)) = part.split_once('=') {
                let k = k.trim();
                let v = unquote(v.trim());
                match k {
                    "path" => dep.path = Some(PathBuf::from(v)),
                    "version" => dep.version = Some(v),
                    _ => {}
                }
            }
        }
        dep
    } else {
        // Simple version string
        DepConfig {
            path: None,
            version: Some(unquote(value)),
        }
    }
}

/// Find the stdlib path
fn find_stdlib() -> Option<PathBuf> {
    // First, check environment variable (highest priority)
    if let Ok(path) = std::env::var("VIBELANG_STD") {
        let path = PathBuf::from(path);
        if path.exists() {
            return Some(path);
        }
    }

    // Also check legacy VIBELANG_STDLIB for backwards compatibility
    if let Ok(path) = std::env::var("VIBELANG_STDLIB") {
        let path = PathBuf::from(path);
        if path.exists() {
            return Some(path);
        }
    }

    // Try relative to current executable
    if let Ok(exe) = std::env::current_exe() {
        if let Some(exe_dir) = exe.parent() {
            // Check ../std (installed layout)
            let stdlib = exe_dir.join("../std");
            if stdlib.exists() {
                return Some(stdlib.canonicalize().ok()?);
            }

            // Check ../../std (dev layout - from bootstrap/target/release)
            let stdlib = exe_dir.join("../../std");
            if stdlib.exists() {
                return Some(stdlib.canonicalize().ok()?);
            }

            // Check ../../../std (dev layout - from bootstrap/target/debug or release)
            let stdlib = exe_dir.join("../../../std");
            if stdlib.exists() {
                return Some(stdlib.canonicalize().ok()?);
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unquote() {
        assert_eq!(unquote("\"hello\""), "hello");
        assert_eq!(unquote("'hello'"), "hello");
        assert_eq!(unquote("hello"), "hello");
    }

    #[test]
    fn test_parse_dep_value() {
        let dep = parse_dep_value("\"0.1.0\"");
        assert_eq!(dep.version, Some("0.1.0".to_string()));

        let dep = parse_dep_value("{ path = \"../mylib\" }");
        assert_eq!(dep.path, Some(PathBuf::from("../mylib")));
    }
}
