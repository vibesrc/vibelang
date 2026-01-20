# Vibelang Language Specification

**Version:** 0.1.0-draft  
**Status:** Draft  
**Last Updated:** 2026-01-12

## Abstract

Vibelang is a systems programming language designed for safety, simplicity, and self-hosting. It combines Rust-like ownership semantics with TypeScript-like ergonomics, compiling to LLVM IR. The language prioritizes correctness-first code that the compiler optimizes into efficient implementations through escape analysis and copy elision.

## Design Philosophy

1. **Write for correctness, compile for speed** — Simple owned/copied code that the compiler optimizes into zero-copy implementations where safe
2. **Explicit ownership, implicit optimization** — Ownership is clear at boundaries; storage decisions (stack/heap) are compiler's job
3. **Minimal primitives, rich stdlib** — Language provides LLVM-mapped primitives; containers and utilities are stdlib

## Table of Contents

1. [Abstract](./00-abstract.md)
2. [Introduction](./01-introduction.md)
3. [Terminology](./02-terminology.md)
4. [Lexical Structure](./03-lexical.md)
5. [Types](./04-types.md)
   - 5.1 [Primitive Types](./04.01-primitives.md)
   - 5.2 [Composite Types](./04.02-composites.md)
   - 5.3 [Generic Types](./04.03-generics.md)
6. [Declarations](./05-declarations.md)
7. [Ownership and Memory](./06-ownership.md)
8. [Expressions](./07-expressions.md)
9. [Statements](./08-statements.md)
10. [Functions](./09-functions.md)
11. [Structs and Enums](./10-structs-enums.md)
12. [Pattern Matching](./11-pattern-matching.md)
13. [Error Handling](./12-error-handling.md)
14. [Modules](./13-modules.md)
15. [Standard Library](./14-stdlib.md)
16. [Compiler Optimizations](./15-optimizations.md)
17. [LLVM Code Generation](./16-codegen.md)
18. [Security Considerations](./17-security.md)
19. [References](./18-references.md)
20. [Closures](./19-closures.md)
21. [Compiler-Coupled Traits](./20-compiler-traits.md)

## Document Index

| Section | File | Description | Lines |
|---------|------|-------------|-------|
| Abstract | [00-abstract.md](./00-abstract.md) | Status and summary | ~50 |
| Introduction | [01-introduction.md](./01-introduction.md) | Goals and motivation | ~150 |
| Terminology | [02-terminology.md](./02-terminology.md) | Definitions | ~100 |
| Lexical Structure | [03-lexical.md](./03-lexical.md) | Tokens and grammar | ~300 |
| Types | [04-types.md](./04-types.md) | Type system overview | ~100 |
| Primitive Types | [04.01-primitives.md](./04.01-primitives.md) | LLVM-mapped types | ~200 |
| Composite Types | [04.02-composites.md](./04.02-composites.md) | Arrays, slices, structs | ~300 |
| Generic Types | [04.03-generics.md](./04.03-generics.md) | Parameterized types | ~200 |
| Declarations | [05-declarations.md](./05-declarations.md) | const, let, static | ~250 |
| Ownership | [06-ownership.md](./06-ownership.md) | Memory model | ~400 |
| Expressions | [07-expressions.md](./07-expressions.md) | Operators and evaluation | ~300 |
| Statements | [08-statements.md](./08-statements.md) | Control flow | ~250 |
| Functions | [09-functions.md](./09-functions.md) | Function definitions | ~300 |
| Structs/Enums | [10-structs-enums.md](./10-structs-enums.md) | User-defined types | ~350 |
| Pattern Matching | [11-pattern-matching.md](./11-pattern-matching.md) | match expressions | ~250 |
| Error Handling | [12-error-handling.md](./12-error-handling.md) | Result and Option | ~200 |
| Modules | [13-modules.md](./13-modules.md) | Code organization | ~200 |
| Standard Library | [14-stdlib.md](./14-stdlib.md) | Core types | ~400 |
| Optimizations | [15-optimizations.md](./15-optimizations.md) | Compiler passes | ~300 |
| Code Generation | [16-codegen.md](./16-codegen.md) | LLVM IR output | ~300 |
| Security | [17-security.md](./17-security.md) | Safety guarantees | ~150 |
| References | [18-references.md](./18-references.md) | Citations | ~50 |
| Closures | [19-closures.md](./19-closures.md) | Anonymous functions | ~250 |
| Compiler Traits | [20-compiler-traits.md](./20-compiler-traits.md) | Lang items, operator traits | ~400 |

## Quick Navigation

- **Language overview**: Start with [Introduction](./01-introduction.md)
- **Type system**: See [Types](./04-types.md)
- **Memory model**: See [Ownership and Memory](./06-ownership.md)
- **Implementers**: See [LLVM Code Generation](./16-codegen.md)
- **Stdlib reference**: See [Standard Library](./14-stdlib.md)

## Example

```vibelang
// A taste of Vibelang

static VERSION = "0.1.0"

struct Token {
    lexeme: Slice<u8>
    line: u32
    kind: TokenKind
}

enum TokenKind {
    Ident
    Number(i64)
    String
    Plus
    Minus
    LParen
    RParen
    Eof
}

fn main() -> Result<void, Error> {
    let source = File.read("input.vibe")?
    let tokens = lex(&source)
    let ast = parse(&tokens)?
    let ir = codegen(&ast)
    File.write("output.ll", &ir)?
    return Ok(void)
}

fn lex(src: &String) -> Vec<Token> {
    let tokens = Vec<Token>()
    let pos = 0
    
    while pos < src.len() {
        const start = pos
        const c = src.byte_at(pos)
        
        const kind = match c {
            '+' => TokenKind.Plus
            '-' => TokenKind.Minus
            '(' => TokenKind.LParen
            ')' => TokenKind.RParen
            _ if is_alpha(c) => {
                while pos < src.len() and is_alnum(src.byte_at(pos)) {
                    pos += 1
                }
                TokenKind.Ident
            }
            _ if is_digit(c) => {
                while pos < src.len() and is_digit(src.byte_at(pos)) {
                    pos += 1
                }
                TokenKind.Number(parse_int(src.slice(start, pos)))
            }
            _ => continue
        }
        
        tokens.push(Token {
            lexeme: src.slice(start, pos),
            line: current_line,
            kind: kind
        })
    }
    
    tokens.push(Token { lexeme: Slice.empty(), line: current_line, kind: TokenKind.Eof })
    return tokens
}
```
