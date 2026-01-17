# Section 1: Introduction

## 1.1 Motivation

Modern systems programming presents a tension between safety and simplicity. Rust offers memory safety but requires lifetime annotations that steepen the learning curve. Go offers simplicity but relies on garbage collection. C offers control but provides no safety guarantees.

Vibelang aims to occupy a new point in this design space:

```
                    Safe
                     │
              Rust ──┼── Vibelang
                     │
        Simple ──────┼────── Complex
                     │
                Go ──┼── C++
                     │
                   Unsafe
```

## 1.2 Design Principles

### 1.2.1 Correctness First, Speed Second

Write simple, obvious code. Let the compiler optimize:

```vibelang
// You write this (clear ownership, explicit copies)
fn process(data: &String) -> String {
    let result = String.new()
    for line in data.split("\n") {
        result.push(line.trim().copy())
    }
    return result
}

// Compiler may optimize to (zero-copy where safe)
// - Elide copies when data outlives result
// - Stack allocate result if it doesn't escape
// - Inline small strings
```

### 1.2.2 Explicit Ownership at Boundaries

Function signatures define the contract. Inside functions, the compiler optimizes:

```vibelang
// Ownership is clear at the API boundary
fn parse(src: String) -> Ast          // takes ownership of src
fn parse(src: &String) -> Ast         // borrows src (read-only)
fn parse(src: ~String) -> Ast         // borrows src (mutable)
fn parse(src: &String) -> &Token      // borrows src, returns borrow

// Inside the function, compiler decides:
// - Stack vs heap allocation
// - Copy vs borrow for temporaries
// - Inline vs indirect storage
```

### 1.2.3 Minimal Core, Rich Standard Library

The language provides only primitives that map to LLVM:

```vibelang
// Language primitives (LLVM-backed)
i8 i16 i32 i64          // Signed integers
u8 u16 u32 u64          // Unsigned integers
f32 f64                 // Floating point
bool                    // Boolean (LLVM i1)
void                    // Unit type
*T                      // Raw pointer
T[N]                    // Fixed-size array

// Everything else is stdlib (written in Vibelang)
Vec<T>                // Dynamic array
Slice<T>                // Borrowed view
String                  // UTF-8 string
Map<K, V>               // Hash map
Option<T>               // Nullable
Result<T, E>            // Error handling
```

## 1.3 Language Overview

### 1.3.1 Hello World

```vibelang
fn main() {
    print("Hello, Vibelang!")
}
```

### 1.3.2 Variables

```vibelang
static VERSION = "0.1.0"      // Global, compile-time, immutable

fn example() {
    const len = src.len()     // Local, runtime ok, immutable
    let pos = 0               // Local, mutable
}
```

### 1.3.3 Functions

```vibelang
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn greet(name: &String) {
    print("Hello, ")
    print(name)
}
```

### 1.3.4 Structs

```vibelang
struct Point {
    x: f64
    y: f64
}

impl Point {
    fn new(x: f64, y: f64) -> Point {
        return Point { x: x, y: y }
    }
    
    fn distance(self: &Point, other: &Point) -> f64 {
        const dx = self.x - other.x
        const dy = self.y - other.y
        return sqrt(dx * dx + dy * dy)
    }
}
```

### 1.3.5 Enums

```vibelang
enum Result<T, E> {
    Ok(T)
    Err(E)
}

enum Option<T> {
    Some(T)
    None
}

fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        return Option.None
    }
    return Option.Some(a / b)
}
```

### 1.3.6 Pattern Matching

```vibelang
fn handle_result(r: Result<i32, String>) {
    match r {
        Result.Ok(value) => println("${value}")
        Result.Err(msg) => println("Error: ${msg}")
    }
}
```

### 1.3.7 Ownership

```vibelang
fn example() {
    let a = String.from("hello")
    let b = a                       // a moved to b
    print(&a)                       // ERROR: a was moved
    print(&b)                       // OK
}
```

### 1.3.8 Borrowing

```vibelang
fn example() {
    let data = String.from("hello")
    print_string(&data)             // borrow
    print_string(&data)             // can borrow again
    consume_string(data)            // move
    print_string(&data)             // ERROR: data was moved
}

fn print_string(s: &String) {
    // borrows s, doesn't own it
}

fn consume_string(s: String) {
    // owns s, will free it
}
```

## 1.4 Comparison to Other Languages

### 1.4.1 vs Rust

| Aspect | Rust | Vibelang |
|--------|------|----------|
| Lifetime annotations | Explicit (`'a`) | Inferred by compiler |
| Mutability | `let mut` | `let` (mutable by default) |
| Immutability | `let` (default) | `const` |
| String types | `String`, `&str`, `&'a str` | `String`, `Slice<u8>` |
| Array types | `Vec<T>`, `[T; N]`, `&[T]` | `Vec<T>`, `T[N]`, `Slice<T>` |

### 1.4.2 vs Go

| Aspect | Go | Vibelang |
|--------|-----|----------|
| Memory management | GC | Ownership |
| Generics | Yes (1.18+) | Yes |
| Error handling | Multiple returns | Result type |
| Null safety | nil exists | Option type |

### 1.4.3 vs TypeScript

| Aspect | TypeScript | Vibelang |
|--------|------------|----------|
| Runtime | JavaScript | Native (LLVM) |
| Type system | Structural | Nominal |
| Null handling | Optional chaining | Option/Result |
| Generics | Yes | Yes (similar syntax) |

## 1.5 Self-Hosting Roadmap

```
Phase 1: Bootstrap Compiler (C)
├── Lexer
├── Parser
├── Type Checker
├── LLVM IR Codegen
└── Can compile Vibelang v0.1

Phase 2: Standard Library (Vibelang)
├── Vec<T>
├── Slice<T>
├── String
├── Map<K, V>
├── Option<T>
├── Result<T, E>
└── File I/O

Phase 3: Self-Hosting Compiler (Vibelang)
├── Rewrite lexer in Vibelang
├── Rewrite parser in Vibelang
├── Rewrite type checker in Vibelang
├── Rewrite codegen in Vibelang
└── Vibelang compiles itself
```
