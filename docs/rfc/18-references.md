# Section 18: References

## 18.1 Normative References

These documents are essential for understanding and implementing Vibelang.

### Language Design

- **RFC 2119** - Key words for use in RFCs to Indicate Requirement Levels
  - https://www.rfc-editor.org/rfc/rfc2119

### LLVM

- **LLVM Language Reference Manual**
  - https://llvm.org/docs/LangRef.html
  - Defines LLVM IR syntax and semantics

- **LLVM-C API Documentation**
  - https://llvm.org/doxygen/group__LLVMC.html
  - C bindings used by bootstrap compiler

### Unicode

- **The Unicode Standard, Version 15.0**
  - https://www.unicode.org/versions/Unicode15.0.0/
  - UTF-8 encoding specification

## 18.2 Informative References

These documents provide background and inspiration.

### Related Languages

- **The Rust Programming Language**
  - https://doc.rust-lang.org/book/
  - Ownership, borrowing, and memory safety concepts

- **The Go Programming Language Specification**
  - https://go.dev/ref/spec
  - Simple syntax, defer statement

- **TypeScript Handbook**
  - https://www.typescriptlang.org/docs/handbook/
  - Generic syntax inspiration

- **Zig Language Reference**
  - https://ziglang.org/documentation/master/
  - Compile-time evaluation, no hidden control flow

- **Lobster Language**
  - http://strlen.com/lobster/
  - Lifetime inference without annotations

### Compiler Design

- **Engineering a Compiler (2nd Edition)**
  - Cooper, Keith D. and Torczon, Linda
  - ISBN: 978-0120884780

- **Modern Compiler Implementation in ML**
  - Appel, Andrew W.
  - ISBN: 978-0521607643

- **Crafting Interpreters**
  - Nystrom, Robert
  - https://craftinginterpreters.com/

### Memory Management

- **A Unified Theory of Garbage Collection**
  - Bacon, David F., Cheng, Perry, and Rajan, V.T.
  - OOPSLA 2004

- **Region-Based Memory Management**
  - Tofte, Mads and Talpin, Jean-Pierre
  - Information and Computation, 1997

### Type Systems

- **Types and Programming Languages**
  - Pierce, Benjamin C.
  - ISBN: 978-0262162098

- **Advanced Topics in Types and Programming Languages**
  - Pierce, Benjamin C. (editor)
  - ISBN: 978-0262162289

### Optimization

- **Static Single Assignment Book**
  - Multiple authors
  - http://ssabook.gforge.inria.fr/latest/book.pdf

- **LLVM: A Compilation Framework for Lifelong Program Analysis & Transformation**
  - Lattner, Chris and Adve, Vikram
  - CGO 2004

## 18.3 Tools and Infrastructure

### Build Tools

- **LLVM Project**
  - https://llvm.org/
  - Compiler infrastructure

- **Clang**
  - https://clang.llvm.org/
  - C compiler for bootstrap phase

### Testing

- **LLVM lit (LLVM Integrated Tester)**
  - https://llvm.org/docs/CommandGuide/lit.html

- **FileCheck**
  - https://llvm.org/docs/CommandGuide/FileCheck.html

## 18.4 Glossary

| Term | Definition |
|------|------------|
| AST | Abstract Syntax Tree - tree representation of source code |
| Borrow | Temporary access to a value without ownership |
| Copy type | Type whose values are implicitly duplicated |
| Drop | Cleanup action when a value goes out of scope |
| Escape | When a value or reference leaves its original scope |
| FFI | Foreign Function Interface - calling external code |
| IR | Intermediate Representation - code between source and machine |
| Lifetime | Duration during which a value or reference is valid |
| LLVM | Low Level Virtual Machine - compiler infrastructure |
| Monomorphization | Generating specialized code for each generic instantiation |
| Move | Transfer of ownership from one binding to another |
| Owned type | Type whose values have a single owner responsible for cleanup |
| Ownership | Responsibility for freeing a value |
| Slice | Borrowed view into contiguous data |
| SSA | Static Single Assignment - IR form where each variable assigned once |

## 18.5 Document History

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0-draft | 2026-01-12 | Initial specification |

## 18.6 Authors and Acknowledgments

### Authors

- **Claude** (Anthropic) - Initial design and specification
- **Brennen Herbruck** - Design direction and review

### Acknowledgments

Vibelang draws inspiration from many languages and their communities:

- The Rust community for pioneering practical ownership systems
- The Go team for demonstrating that simplicity scales
- The LLVM project for excellent compiler infrastructure
- The TypeScript team for making generics approachable

## 18.7 License

This specification is released under [LICENSE TBD].

The Vibelang compiler and standard library will be released under [LICENSE TBD].
