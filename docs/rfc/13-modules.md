# Section 13: Modules

Modules organize code into namespaces and control visibility.

## 13.1 Module Basics

### File as Module

Each `.vibe` file is a module:

```
src/
├── main.vibe           // main module
├── lexer.vibe          // lexer module
├── parser.vibe         // parser module
└── types.vibe          // types module
```

### Module Declaration

In `main.vibe`:

```vibelang
mod lexer               // declares lexer module (from lexer.vibe)
mod parser              // declares parser module (from parser.vibe)
mod types               // declares types module (from types.vibe)

fn main() {
    let tokens = lexer.tokenize(source)
    let ast = parser.parse(&tokens)
}
```

## 13.2 Directory Modules

### Directory Structure

```
src/
├── main.vibe
└── compiler/
    ├── mod.vibe        // module root
    ├── lexer.vibe
    ├── parser.vibe
    └── codegen.vibe
```

### mod.vibe

```vibelang
// compiler/mod.vibe
pub mod lexer
pub mod parser
pub mod codegen

// Re-exports
pub use lexer.Token
pub use parser.Ast
```

### Using Directory Module

```vibelang
// main.vibe
mod compiler

fn main() {
    let tokens = compiler.lexer.tokenize(source)
    let ast = compiler.parser.parse(&tokens)
    let code = compiler.codegen.generate(&ast)
}
```

## 13.3 Visibility

### Private by Default

Items are private to their module by default:

```vibelang
// lexer.vibe
struct Lexer { ... }            // private
fn helper() { ... }             // private

pub struct Token { ... }        // public
pub fn tokenize() { ... }       // public
```

### Pub Keyword

```vibelang
pub struct Token { ... }        // type is public
pub enum TokenKind { ... }      // enum is public
pub fn tokenize() { ... }       // function is public
pub static VERSION = "1.0"      // constant is public
```

### Public Fields

```vibelang
pub struct Point {
    pub x: f64                  // public field
    pub y: f64                  // public field
}

pub struct Counter {
    count: u64                  // private field (encapsulated)
}

impl Counter {
    pub fn new() -> Counter { ... }
    pub fn value(self: &Counter) -> u64 { self.count }
    pub fn increment(self: &Counter) { self.count += 1 }
}
```

## 13.4 Use Declarations

### Basic Use

```vibelang
use lexer.Token
use lexer.TokenKind

fn process(token: Token) { ... }
```

### Multiple Imports

```vibelang
use lexer.{Token, TokenKind, Lexer}
use parser.{Ast, Parser, ParseError}
```

### Glob Import

```vibelang
use lexer.*             // import all public items
```

### Aliased Import

```vibelang
use lexer.Token as Tok
use parser.Ast as SyntaxTree
```

### Nested Paths

```vibelang
use compiler.lexer.Token
use compiler.parser.{Ast, Parser}
use compiler.codegen.*
```

## 13.5 Re-exports

### Public Use

```vibelang
// lib.vibe
mod lexer
mod parser

// Re-export for convenience
pub use lexer.Token
pub use lexer.TokenKind
pub use parser.Ast
pub use parser.Parser
```

Users can then:
```vibelang
use mylib.Token         // instead of mylib.lexer.Token
```

### Glob Re-export

```vibelang
pub use lexer.*         // re-export everything from lexer
```

## 13.6 Module Paths

### Absolute Paths

```vibelang
// From crate root
use crate.lexer.Token
use crate.parser.Ast
```

### Relative Paths

```vibelang
// From current module
use self.helper
use super.Token         // parent module
use super.super.Config  // grandparent module
```

### Self and Super

```vibelang
// compiler/parser.vibe
use self.expression.parse_expr      // submodule
use super.lexer.Token               // sibling module
use super.super.Config              // parent's parent
```

## 13.7 Module Example

### Project Structure

```
mycompiler/
├── main.vibe
├── config.vibe
├── compiler/
│   ├── mod.vibe
│   ├── lexer.vibe
│   ├── parser.vibe
│   └── codegen/
│       ├── mod.vibe
│       ├── ir.vibe
│       └── llvm.vibe
└── util/
    ├── mod.vibe
    └── error.vibe
```

### main.vibe

```vibelang
mod config
mod compiler
mod util

use config.Config
use compiler.{Lexer, Parser, Codegen}
use util.error.Error

fn main() -> Result<void, Error> {
    let config = Config.load("config.toml")?
    
    let source = read_file(&config.input)?
    
    let lexer = Lexer.new(&source)
    let tokens = lexer.tokenize()?
    
    let parser = Parser.new(&tokens)
    let ast = parser.parse()?
    
    let codegen = Codegen.new(&config)
    let output = codegen.generate(&ast)?
    
    write_file(&config.output, &output)?
    
    return Result.Ok(void)
}
```

### compiler/mod.vibe

```vibelang
pub mod lexer
pub mod parser
pub mod codegen

// Re-export main types
pub use lexer.Lexer
pub use lexer.Token
pub use parser.Parser
pub use parser.Ast
pub use codegen.Codegen
```

### compiler/lexer.vibe

```vibelang
use super.super.util.error.Error

pub struct Lexer {
    source: String
    pos: u64
}

pub struct Token {
    pub kind: TokenKind
    pub lexeme: Slice<u8>
    pub line: u32
}

pub enum TokenKind {
    Ident
    Number
    String
    // ...
}

impl Lexer {
    pub fn new(source: &String) -> Lexer {
        return Lexer {
            source: source.copy(),
            pos: 0
        }
    }
    
    pub fn tokenize(self: &Lexer) -> Result<Array<Token>, Error> {
        let tokens = Array<Token>()
        
        while self.pos < self.source.len() {
            let token = self.next_token()?
            tokens.push(token)
        }
        
        return Result.Ok(tokens)
    }
    
    fn next_token(self: &Lexer) -> Result<Token, Error> {
        // private helper
    }
}
```

### util/error.vibe

```vibelang
pub struct Error {
    pub kind: ErrorKind
    pub message: String
    pub location: Option<Location>
}

pub enum ErrorKind {
    Io
    Parse
    Type
    Codegen
}

pub struct Location {
    pub file: String
    pub line: u32
    pub column: u32
}

impl Error {
    pub fn new(kind: ErrorKind, message: String) -> Error {
        return Error {
            kind: kind,
            message: message,
            location: Option.None
        }
    }
    
    pub fn with_location(kind: ErrorKind, message: String, loc: Location) -> Error {
        return Error {
            kind: kind,
            message: message,
            location: Option.Some(loc)
        }
    }
}
```

## 13.8 Prelude

### Implicit Imports

Some items are automatically available:

```vibelang
// Always available without import:
Option
Option.Some
Option.None
Result
Result.Ok
Result.Err
Array
Slice
String
// ... basic types
```

## 13.9 Circular Dependencies

Circular module dependencies are NOT allowed:

```vibelang
// a.vibe
mod b               // a depends on b
use b.Foo

// b.vibe
mod a               // ERROR: b depends on a, creating cycle
use a.Bar
```

### Breaking Cycles

Extract shared types to a third module:

```vibelang
// shared.vibe
pub struct SharedType { ... }

// a.vibe
use shared.SharedType

// b.vibe  
use shared.SharedType
```

## 13.10 Conditional Compilation

### Platform-Specific Code

```vibelang
#[cfg(os = "linux")]
mod linux_impl

#[cfg(os = "windows")]
mod windows_impl

#[cfg(os = "macos")]
mod macos_impl
```

### Feature Flags

```vibelang
#[cfg(feature = "json")]
mod json_support

#[cfg(feature = "async")]
mod async_runtime
```

### Conditional Items

```vibelang
#[cfg(debug)]
fn debug_print(msg: &String) {
    print(msg)
}

#[cfg(not(debug))]
fn debug_print(msg: &String) {
    // no-op in release
}
```

## 13.11 Best Practices

### Module Organization

1. **One concept per module** - lexer.vibe for lexing, parser.vibe for parsing
2. **Minimize public API** - only expose what's needed
3. **Re-export for convenience** - users shouldn't need deep paths
4. **Group related modules** - use directories for subsystems

### Naming Conventions

```vibelang
// Modules: snake_case
mod token_stream
mod ast_builder

// Types: PascalCase  
pub struct TokenStream
pub enum AstNode

// Functions: snake_case
pub fn parse_expression()
```

### API Design

```vibelang
// Good: clear, minimal public API
pub struct Parser { ... }

impl Parser {
    pub fn new(tokens: &Array<Token>) -> Parser
    pub fn parse(self: &Parser) -> Result<Ast, Error>
}

// Internal helpers are private
impl Parser {
    fn parse_expression(self: &Parser) -> Result<Expr, Error>
    fn parse_statement(self: &Parser) -> Result<Stmt, Error>
    fn expect_token(self: &Parser, kind: TokenKind) -> Result<Token, Error>
}
```
