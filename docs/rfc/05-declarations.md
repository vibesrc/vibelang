# Section 5: Declarations

Vibelang has three declaration keywords: `static`, `const`, and `let`. Each serves a distinct purpose in the ownership and mutability model.

## 5.1 Declaration Overview

| Keyword | Scope | Mutable | Evaluated | Storage |
|---------|-------|---------|-----------|---------|
| `static` | Global | No | Compile-time | Data segment |
| `const` | Local | No | Runtime OK | Stack |
| `let` | Local | Yes | Runtime | Stack or Heap |

## 5.2 Static Declarations

`static` declares global compile-time constants:

```vibelang
static VERSION = "0.1.0"
static MAX_TOKENS = 10000
static PI = 3.14159265358979
static DEBUG = false
```

### Static Rules

1. `static` MUST appear at module/file scope
2. `static` values MUST be computable at compile time
3. `static` values are immutable
4. `static` values live for the entire program

```vibelang
// OK: compile-time computable
static SIZE = 1024
static DOUBLED = SIZE * 2
static NAME = "vibelang"

// ERROR: not compile-time computable
static CONFIG = load_config()           // ERROR: function call
static NOW = get_time()                 // ERROR: function call
```

### Static Arrays

```vibelang
static KEYWORDS = ["fn", "let", "const", "static", "if", "else", "match"]
static PRIMES = [2, 3, 5, 7, 11, 13, 17, 19]
static IDENTITY_3X3 = [
    [1, 0, 0],
    [0, 1, 0],
    [0, 0, 1]
]
```

### Static Visibility

By default, statics are module-private. Use `pub` for public:

```vibelang
pub static VERSION = "0.1.0"            // accessible from other modules
static INTERNAL_LIMIT = 1000            // private to this module
```

## 5.3 Const Declarations

`const` declares local immutable bindings:

```vibelang
fn example(src: &String) {
    const len = src.len()               // runtime value, immutable
    const first_char = src.byte_at(0)   // runtime value, immutable
    const threshold = 100               // can also be literal
}
```

### Const Rules

1. `const` MUST appear at local (function/block) scope
2. `const` values can be computed at runtime
3. `const` values are immutable (cannot be reassigned)
4. `const` values MUST be Copy types or borrowed types

```vibelang
fn process(data: &String) {
    const len = data.len()              // OK: u64 is Copy
    const slice = &data[0..10]          // OK: borrowing
    const greeting = "hello"            // OK: string literal (static borrow)
    
    const owned = String.from("hi")     // ERROR: const cannot hold owned types
    const vec = Vec<i32>()            // ERROR: const cannot hold owned types
}
```

### Const Cannot Be Reassigned

```vibelang
fn example() {
    const x = 42
    x = 100                             // ERROR: cannot reassign const
}
```

### Const Shadowing

A new `const` can shadow a previous one:

```vibelang
fn process(input: &String) {
    const text = input
    const text = text.trim()            // shadows previous text
    const text = text.lowercase()       // shadows again
    // each 'text' is a new immutable binding
}
```

## 5.4 Let Declarations

`let` declares local mutable bindings:

```vibelang
fn example() {
    let x = 42                          // mutable
    x = 100                             // OK: reassignment
    
    let name = String.from("Alice")     // owned type
    name.push(" Smith")                 // OK: mutation
}
```

### Let Rules

1. `let` MUST appear at local (function/block) scope
2. `let` values can be reassigned
3. `let` values can be any type (Copy or Owned)
4. Owned values are freed when `let` binding goes out of scope

```vibelang
fn example() {
    let x = 42                          // Copy type, mutable
    let s = String.from("hello")        // Owned type, mutable
    
    x = 100                             // OK
    s.push(" world")                    // OK
    s = String.from("goodbye")          // OK: old value freed, new value assigned
}   // s freed here
```

### Let with Owned Types

When a `let` binding holds an owned type, it is responsible for freeing:

```vibelang
fn example() {
    let a = String.from("hello")        // a owns the string
    let b = a                           // MOVE: b now owns, a is invalid
    
    print(&a)                           // ERROR: a was moved
    print(&b)                           // OK
}   // b freed here (a was already moved)
```

### Let with Explicit Types

```vibelang
let x: i64 = 42                         // explicit type
let y: f32 = 3.14                       // explicit type
let s: String = String.from("hi")       // explicit type
```

## 5.5 Declaration Comparison

### Mutability

```vibelang
static X = 1                    // immutable (always)
const x = 1                     // immutable (always)
let x = 1                       // mutable
```

### Scope

```vibelang
static GLOBAL = 1               // file/module scope

fn example() {
    const LOCAL_CONST = 2       // function scope
    let local_var = 3           // function scope
    
    {
        const BLOCK_CONST = 4   // block scope
        let block_var = 5       // block scope
    }   // BLOCK_CONST and block_var out of scope
}
```

### Type Restrictions

```vibelang
// static: compile-time only
static S = "hello"              // OK: string literal
static N = 42                   // OK: integer literal
static F = load()               // ERROR: not compile-time

// const: Copy and borrowed types only
const n = get_number()          // OK: i32 is Copy
const s = &owned_string         // OK: borrow
const v = Vec<i32>()          // ERROR: owned type

// let: any type
let n = 42                      // OK: Copy
let s = String.from("hi")       // OK: Owned
let r = &other                  // OK: borrow
```

## 5.6 Initialization

All declarations MUST be initialized:

```vibelang
let x: i32                      // ERROR: must initialize
let x: i32 = 42                 // OK

const y: f64                    // ERROR: must initialize
const y: f64 = 3.14             // OK
```

### Deferred Initialization (Not Supported)

Unlike some languages, Vibelang does not support deferred initialization:

```vibelang
// NOT SUPPORTED
let x: i32
if condition {
    x = 1
} else {
    x = 2
}

// Instead, use if expression
let x: i32 = if condition { 1 } else { 2 }
```

## 5.7 Destructuring

Declarations can destructure composite values:

### Tuple Destructuring

```vibelang
let (x, y) = (1, 2)
let (a, b, c) = get_triple()

const (first, second) = pair
```

### Struct Destructuring

```vibelang
struct Point { x: f64, y: f64 }

let point = Point { x: 1.0, y: 2.0 }
let Point { x, y } = point              // x = 1.0, y = 2.0
let Point { x: a, y: b } = point        // a = 1.0, b = 2.0
```

### Array Destructuring

```vibelang
let [a, b, c] = [1, 2, 3]               // a=1, b=2, c=3
let [first, ..rest] = array             // first element and rest (if supported)
```

## 5.8 Type Inference

The compiler infers types when possible:

```vibelang
let x = 42                      // inferred: i32
let y = 3.14                    // inferred: f64
let s = "hello"                 // inferred: Slice<u8>
let arr = [1, 2, 3]             // inferred: i32[3]
let vec = Vec<i32>()          // explicit generic required

const len = s.len()             // inferred: u64
const flag = true               // inferred: bool
```

### When Inference Fails

```vibelang
let vec = Array()               // ERROR: can't infer T
let vec = Vec<i32>()          // OK: explicit

let opt = Option.None           // ERROR: can't infer T
let opt: Option<i32> = Option.None  // OK: explicit type
```

## 5.9 Examples

### Complete Function Example

```vibelang
static MAX_ITERATIONS = 1000

fn process(input: &String) -> Result<Vec<Token>, Error> {
    const len = input.len()             // const: won't change
    let tokens = Vec<Token>()         // let: will be mutated
    let pos = 0                         // let: will be incremented
    
    while pos < len {
        const char = input.byte_at(pos) // const: loop-local
        
        let token = match char {
            ' ' | '\n' | '\t' => {
                pos += 1
                continue
            }
            '(' => Token { kind: TokenKind.LParen, pos: pos }
            ')' => Token { kind: TokenKind.RParen, pos: pos }
            _ => {
                const start = pos       // const: won't change in this branch
                while pos < len and is_ident_char(input.byte_at(pos)) {
                    pos += 1
                }
                Token { kind: TokenKind.Ident, lexeme: input.slice(start, pos) }
            }
        }
        
        tokens.push(token)
        pos += 1
    }
    
    return Result.Ok(tokens)
}
```
