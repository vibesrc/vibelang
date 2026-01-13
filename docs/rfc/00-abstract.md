# Section 0: Abstract

**Status:** Draft  
**Version:** 0.1.0

## Summary

Vibelang is a statically-typed, compiled systems programming language targeting LLVM. It is designed with a singular goal: to be expressive and safe enough to write its own compiler.

The language combines:

- **Rust-like ownership semantics** for memory safety without garbage collection
- **TypeScript-like ergonomics** for familiar, readable syntax
- **Functional language optimization philosophy** where simple, correct code is compiled into efficient implementations

## Goals

1. **Self-hosting** — The Vibelang compiler will be written in Vibelang
2. **Memory safe** — No use-after-free, double-free, or dangling pointers
3. **Zero-cost abstractions** — High-level code compiles to efficient machine code
4. **Minimal primitives** — Language core maps to LLVM; stdlib provides containers
5. **Predictable** — Ownership and lifetimes are explicit at API boundaries

## Non-Goals

1. **Garbage collection** — Vibelang uses deterministic destruction
2. **Runtime reflection** — All types are resolved at compile time
3. **Exceptions** — Error handling uses Result types
4. **Null pointers** — Optional values use Option types
5. **Lifetime annotations** — Compiler infers lifetimes internally

## Status

This specification is a working draft. The bootstrap compiler will be written in C using the LLVM-C API, then rewritten in Vibelang once the language is sufficiently complete.

## Authors

- Claude (Anthropic) — Initial design and specification
- Brennen Herbruck — Design direction and review
