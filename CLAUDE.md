# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the bootstrap compiler
cd bootstrap && cargo build --release

# Compile a .vibe file to native binary (outputs to bin/)
./bootstrap/target/release/vibec examples/hello.vibe

# Compile with options
vibec file.vibe --emit=llvm    # Output LLVM IR (.ll)
vibec file.vibe --emit=obj     # Output object file (.o)
vibec file.vibe -o name        # Specify output name

# Run tests (compile and execute)
./bootstrap/target/release/vibec tests/run/hello.vibe && ./bin/hello
```

## Project Structure

Vibelang is a statically-typed systems language with Rust-like ownership semantics, compiling to native code via LLVM. Currently in bootstrap phase (compiler written in Rust, targeting self-hosting).

```
bootstrap/src/     # Rust bootstrap compiler (~4,800 lines)
├── main.rs        # CLI entry point, orchestrates compilation
├── lexer.rs       # Tokenizer: source → Token stream
├── parser.rs      # Parser: tokens → AST
├── ast.rs         # AST type definitions
└── codegen/
    └── llvm.rs    # LLVM IR generation, monomorphization, ownership tracking

docs/rfc/          # Language specification (~9,300 lines of RFC docs)
stdlib/            # Standard library (prelude.vibe with Option, Result)
examples/          # Example .vibe programs
tests/             # Conformance tests
├── compile/       # Programs that should compile
├── error/         # Programs that should produce specific errors
└── run/           # Programs to compile, execute, verify output
bin/               # Compiled output (gitignored)
```

## Compiler Pipeline

1. **Lexer** (`lexer.rs`) - Tokenizes source, handles keywords (`fn`, `struct`, `enum`, `impl`, `let`, `match`, `defer`), operators, literals
2. **Parser** (`parser.rs`) - Builds AST from tokens, supports generics, impl blocks, pattern matching
3. **Codegen** (`codegen/llvm.rs`) - Three-pass compilation:
   - Pass 1: Store generics, define concrete structs/enums
   - Pass 2: Declare function signatures
   - Pass 3: Implement function bodies with ownership tracking

## Key Language Features

- **Ownership model**: `&T` (immutable borrow), `~T` (mutable/"vibing" borrow)
- **Monomorphization**: Generics specialized at compile time (no traits yet)
- **Logical operators**: Words (`and`, `or`, `not`) instead of symbols
- **Semicolons**: Optional (newlines end statements)
- **String literals**: `Slice<u8>` pointing to static memory

## Codegen Internals

The `VarInfo` struct tracks variable state during codegen:
- `ptr` - LLVM pointer to storage
- `is_ref`/`is_mut_ref` - Whether variable is a reference
- `slice_elem_type` - Element type for slices

Ownership tracking uses `moved_vars` (consumed variables) and `borrowed_vars` (active borrows) to enforce single-mutable-borrow rules.

Enums are represented as `{i32 tag, payload}` structs. Slices are fat pointers `{*T, length}`.
