# Vibelang VSCode Extension

Language support for Vibelang - a statically-typed systems language with Rust-like ownership semantics.

## Features

- **Syntax Highlighting**: Full TextMate grammar for `.vibe` files
- **Language Configuration**: Bracket matching, auto-closing pairs, comment toggling
- **LSP Integration**: Powered by `vibelang-lsp` for advanced features:
  - Diagnostics (syntax errors, type errors, borrow checker errors)
  - Code completion (struct fields, methods, variables, functions)
  - Hover information (types, function signatures, borrow state)
  - Go to definition

## Installation

### Building the LSP Server

The extension requires the `vibelang-lsp` binary. Build it from the vibelang repository:

```bash
cd bootstrap
cargo build --release --bin vibelang-lsp
```

The extension automatically looks for the binary in:
1. User-configured path (via `vibelang.lsp.path` setting)
2. `bootstrap/target/release/vibelang-lsp` relative to workspace
3. `~/.cargo/bin/vibelang-lsp`
4. System PATH

### Installing the Extension

For development:
```bash
cd vscode-vibelang
npm install
npm run compile
```

Then press F5 in VSCode to launch the extension development host.

## Configuration

| Setting | Default | Description |
|---------|---------|-------------|
| `vibelang.lsp.path` | `""` | Path to vibelang-lsp binary |
| `vibelang.lsp.trace.server` | `"off"` | Trace LSP communication |

## Language Overview

Vibelang uses Rust-like ownership with unique syntax:

```vibe
// Ownership primitives
let x = 42          // Owned value
let r = &x          // Immutable borrow
let m = ~x          // Mutable borrow ("vibing")

// Logical operators are words
if x > 0 and y < 10 or not done {
    // ...
}

// Everything uses dot notation
Option.Some(value)  // Enum variant
module.function()   // Module access
obj.method()        // Method call
```
