//! Type resolution and validation for Vibelang
//!
//! Provides utilities for checking type validity and compatibility.

use super::symbols::SymbolTable;

/// Check if a type name is a built-in primitive type
pub fn is_builtin_type(name: &str) -> bool {
    matches!(
        name,
        "i8" | "i16" | "i32" | "i64"
        | "u8" | "u16" | "u32" | "u64"
        | "f32" | "f64"
        | "bool"
        | "void"
        | "Slice"
    )
}

/// Check if a type name is from the prelude (always available)
/// Note: Vec and String are in the prelude but require explicit import for codegen
/// due to their dependencies on std.mem. The analyzer accepts them as valid types.
pub fn is_prelude_type(name: &str) -> bool {
    matches!(name, "Option" | "Result" | "Error" | "Vec" | "String" | "Map" | "Set")
}

/// Check if a function name is a built-in function
pub fn is_builtin_function(name: &str) -> bool {
    matches!(
        name,
        "print" | "println" | "panic"
        | "malloc" | "realloc" | "free" | "memcpy"
        | "sizeof" | "null"
        | "ptr_null" | "ptr_is_null" | "ptr_write" | "ptr_read" | "ptr_add"
        | "ptr_write_i64" | "ptr_read_i64"
    )
}

/// Type resolution context for checking type validity
pub struct TypeResolver<'a> {
    symbols: &'a SymbolTable,
    type_params: Vec<String>,
}

impl<'a> TypeResolver<'a> {
    /// Create a new type resolver with the given symbol table
    pub fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            type_params: Vec::new(),
        }
    }

    /// Create a type resolver with additional type parameters in scope
    pub fn with_type_params(symbols: &'a SymbolTable, type_params: &[String]) -> Self {
        Self {
            symbols,
            type_params: type_params.to_vec(),
        }
    }

    /// Check if a type name is valid (exists as builtin, prelude, type param, or user-defined)
    pub fn is_valid_type(&self, name: &str) -> bool {
        is_builtin_type(name)
            || is_prelude_type(name)
            || self.type_params.contains(&name.to_string())
            || self.symbols.structs.contains_key(name)
            || self.symbols.enums.contains_key(name)
    }

    /// Check if two types are compatible
    ///
    /// This performs basic compatibility checking with some flexibility:
    /// - Exact match
    /// - Integer literal coercion (i32 can be assigned to any integer type)
    /// - Generic type matching (base names must match)
    pub fn types_compatible(&self, expected: &str, actual: &str) -> bool {
        // Exact match
        if expected == actual {
            return true;
        }

        // Integer literal coercion: i32 literals can be assigned to any integer type
        // This matches the compiler's behavior where `let x: u8 = 0` is valid
        let integer_types = ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"];
        if integer_types.contains(&expected) && integer_types.contains(&actual) {
            // Allow integer literal coercion (compiler handles overflow checks)
            return true;
        }

        // Handle generic types - check if base type matches
        let expected_base = expected.split('<').next().unwrap_or(expected);
        let actual_base = actual.split('<').next().unwrap_or(actual);

        if expected_base != actual_base {
            return false;
        }

        // If bases match but generics differ, they're incompatible
        // (unless we add more sophisticated generic handling)
        expected == actual
    }
}

/// Parse a type string to extract the base name and any generic arguments
///
/// Examples:
/// - "Vec<i32>" -> ("Vec", vec!["i32"])
/// - "HashMap<String, i32>" -> ("HashMap", vec!["String", "i32"])
/// - "i32" -> ("i32", vec![])
pub fn parse_generic_type(ty: &str) -> (&str, Vec<&str>) {
    if let Some(lt_pos) = ty.find('<') {
        if ty.ends_with('>') {
            let base = &ty[..lt_pos];
            let generics_str = &ty[lt_pos + 1..ty.len() - 1];
            // Simple split - doesn't handle nested generics properly
            let generics: Vec<&str> = generics_str
                .split(',')
                .map(|s| s.trim())
                .collect();
            return (base, generics);
        }
    }
    (ty, Vec::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_types() {
        assert!(is_builtin_type("i32"));
        assert!(is_builtin_type("bool"));
        assert!(is_builtin_type("f64"));
        assert!(!is_builtin_type("MyStruct"));
        assert!(!is_builtin_type("Option"));
    }

    #[test]
    fn test_prelude_types() {
        assert!(is_prelude_type("Option"));
        assert!(is_prelude_type("Result"));
        assert!(is_prelude_type("Vec"));
        assert!(is_prelude_type("String"));
        assert!(is_prelude_type("Map"));
        assert!(is_prelude_type("Set"));
        assert!(!is_prelude_type("i32"));
        assert!(!is_prelude_type("MyType"));
    }

    #[test]
    fn test_builtin_functions() {
        assert!(is_builtin_function("print"));
        assert!(is_builtin_function("malloc"));
        assert!(!is_builtin_function("my_function"));
    }

    #[test]
    fn test_parse_generic_type() {
        assert_eq!(parse_generic_type("i32"), ("i32", vec![]));
        assert_eq!(parse_generic_type("Vec<i32>"), ("Vec", vec!["i32"]));
        assert_eq!(
            parse_generic_type("HashMap<String, i32>"),
            ("HashMap", vec!["String", "i32"])
        );
    }

    #[test]
    fn test_type_compatibility() {
        let symbols = SymbolTable::default();
        let resolver = TypeResolver::new(&symbols);

        // Exact match
        assert!(resolver.types_compatible("i32", "i32"));

        // Integer coercion
        assert!(resolver.types_compatible("i32", "u64"));
        assert!(resolver.types_compatible("u8", "i64"));

        // Different base types
        assert!(!resolver.types_compatible("i32", "bool"));
        assert!(!resolver.types_compatible("Vec<i32>", "Option<i32>"));
    }
}
