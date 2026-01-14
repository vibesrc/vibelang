# Vibelang Design Decisions

This document tracks key design decisions for the Vibelang compiler. Claude Code should reference this during implementation.

## Language Design

| Topic | Decision | Notes |
|-------|----------|-------|
| Mutable borrow | `~T` ("vibing") | Distinct from Rust's `&mut` |
| Read-only borrow | `&T` | Like `const` in function |
| Bitwise NOT | `!` | Freed up `~` for vibing |
| Logical operators | `and`, `or`, `not` | Words, not symbols |
| Semicolons | Optional (Go-style ASI) | Newlines end statements |
| Self parameter | Explicit always | `fn foo(self: &Type)` not `fn foo(&self)` |
| String literals | `Slice<u8>` | Static memory, use `String.from()` for owned |
| Integer overflow | Debug panic, release wrap | Like Rust |
| File extension | `.vibe` | |

## Not in v1

| Feature | Reason |
|---------|--------|
| Concurrency | Needs runtime or async machinery |
| Traits | Generics work without them via monomorphization |
| Macros | Complexity, defer to v2 |
| async/await | Defer to v2, easier to add later than goroutines |

## Implementation

| Topic | Decision | Notes |
|-------|----------|-------|
| Bootstrap language | Rust | inkwell 0.8.0, LLVM 21.1 |
| MVP scope | Full RFC | Not a subset, build it all |
| Output targets | Native + LLVM IR | `--emit=llvm` for debugging |
| Testing | Unit + snapshot + e2e | All three levels |
| FFI | Intrinsics + extern blocks | `sys_*` built-in, `extern "C"` for user |

## Stdlib Implementation

The standard library uses a modular structure under `std/`:

```
std/
├── vibe.toml
└── src/
    ├── lib.vibe
    ├── types/
    │   ├── mod.vibe          # Option, Result, Error (prelude)
    │   ├── option.vibe
    │   ├── result.vibe
    │   └── error.vibe
    ├── collections/
    │   ├── mod.vibe          # Array<T>
    │   └── array.vibe
    ├── string/mod.vibe       # String type + utilities
    ├── io/mod.vibe           # print, println
    └── fs/mod.vibe           # file operations
```

**Prelude (auto-imported):**
- Option, Result, Error from `std/src/types/mod.vibe`
- `print`, `println` are compiler intrinsics

**Resolution:** `use std.X` resolves to `<VIBELANG_STD>/src/X/mod.vibe`.

Environment variable: `VIBELANG_STD` (or legacy `VIBELANG_STDLIB`).

**Priority:**
1. ✅ Types (Option, Result, Error)
2. ✅ Collections (Array, String basics)
3. ✅ String utilities (split, join, trim)
4. 🚧 File I/O (intrinsics needed)
5. Future: Networking

## Error Handling

- `Result<T, E>` with `?` propagation
- `panic()` for unrecoverable bugs
- No try/catch, no exceptions

## Module System

| Prefix | Resolves to | Notes |
|--------|-------------|-------|
| `use src.foo` | `<project_root>/src/foo.vibe` | Requires vibe.toml project |
| `use lib.pkg.foo` | `<project_root>/lib/pkg/src/foo.vibe` | Local workspace packages |
| `use dep.pkg.foo` | `<project_root>/dep/pkg/src/foo.vibe` | Vendored external deps |
| `use std.foo` | Standard library | Requires VIBELANG_STD env var |
| `use .foo` | Relative to current file | Works in single-file mode |

Key decisions:
- Prefix-based imports (no ambiguous bare paths)
- `vibe.toml` for project configuration
- Private by default, `pub` to export
- `mod.vibe` optional for directory modules (re-exports only)
- Clear error messages for missing project config
- `lib/` for local workspace packages (your code)
- `dep/` for vendored external dependencies (managed by tooling)

See `docs/rfc/13-modules.md` for full specification.

## Package Management (Post-v1)

- Config: `vibe.toml`
- Distribution: GitHub releases
- No central registry

## Testing Architecture

| Location | Purpose | Lifespan |
|----------|---------|----------|
| `bootstrap/src/*.rs` | Rust unit tests (`#[cfg(test)]`) | Dies with bootstrap |
| `bootstrap/tests/` | Rust integration tests | Dies with bootstrap |
| `tests/` | **Conformance tests** (`.vibe` files) | Forever |

Conformance tests define language behavior and transfer to self-hosted compiler:
- `tests/compile/` - Should compile successfully
- `tests/error/` - Should produce specific compile errors
- `tests/run/` - Compile, execute, check stdout/exit code

## Open Questions

Track in `docs/questions/` as separate files.

## Implementation Order

```
1. Lexer/Parser
2. AST + Type system
3. Primitives (i32, bool, etc.)
4. Functions + control flow
5. Structs
6. Enums + match
7. Generics (monomorphization)
8. Ownership + borrowing (&, ~)
9. Arrays, Slices, Strings
10. LLVM codegen (throughout)
```

Each step should be testable independently.

## Project Structure

```
vibelang/
├── bootstrap/              # Rust bootstrap compiler (temporary)
│   ├── Cargo.toml
│   ├── src/
│   │   ├── main.rs
│   │   ├── lexer.rs
│   │   ├── parser.rs
│   │   ├── ast.rs
│   │   ├── types.rs
│   │   ├── ownership.rs
│   │   └── codegen/
│   │       ├── mod.rs
│   │       └── llvm.rs
│   └── tests/              # Rust-specific integration tests
├── docs/
│   ├── decisions.md        # This file
│   ├── rfc/                # Language specification
│   ├── questions/          # Open design questions
│   └── notes/              # Session notes for Claude
├── tests/                  # Conformance tests (forever)
│   ├── compile/            # Should compile
│   ├── error/              # Should error
│   └── run/                # Compile + execute + verify
├── examples/
│   └── hello.vibe
└── std/                    # Standard library (package)
    ├── vibe.toml
    └── src/
        ├── lib.vibe
        ├── types/mod.vibe  # Option, Result, Error
        ├── collections/mod.vibe  # Array
        ├── string/mod.vibe # String + utilities
        ├── io/mod.vibe     # print, println
        └── fs/mod.vibe     # File operations
```