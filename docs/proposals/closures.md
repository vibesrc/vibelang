# Proposal: Closures (Anonymous Functions)

## Summary

This proposal introduces closures (anonymous functions) to Vibelang using `fn` syntax with smart capture semantics.

## Motivation

Closures are essential for:
- Higher-order functions (`map`, `filter`, `reduce`)
- Callbacks and event handlers
- Deferred execution
- Building abstractions (iterators, builders, etc.)
- Macro APIs (e.g., `quote!` templating)

## Syntax

### Basic Syntax

Closures use the same `fn` keyword as regular functions, making anonymous functions feel consistent with named functions:

```vibe
// Single parameter
fn(x) x * 2

// Multiple parameters
fn(a, b) a + b

// No parameters
fn() 42

// With type annotations
fn(x: i32) x * 2
fn(a: i32, b: i32) a + b

// With return type
fn(x: i32) -> i32 x * 2
```

### Expression vs Block Body

**Expression body - implicit return:**
```vibe
let double = fn(x) x * 2
let add = fn(a, b) a + b
let greet = fn(name) "Hello, " + name
```

**Block body - explicit return required:**
```vibe
let parse = fn(s) {
    let trimmed = s.trim()
    let value = parse_int(trimmed)
    return value
}

let validate = fn(x) {
    if x < 0 {
        return Result.Err("negative")
    }
    if x > 100 {
        return Result.Err("too large")
    }
    return Result.Ok(x)
}
```

**Warning for missing return:**
```vibe
let bad = fn(x) {
    x * 2  // warning: expression result unused, closure returns ()
}
```

## Capture Semantics

Closures use **smart capture** - the compiler automatically infers the best capture mode based on usage:

```vibe
let multiplier = 10
let scale = fn(x) x * multiplier  // borrows multiplier automatically
print(scale(5))  // 50
print(multiplier)  // 10 - still valid
```

### Smart Capture Inference

The compiler infers capture mode based on usage:

| Usage in Closure | Capture Mode |
|------------------|--------------|
| Read only | Immutable borrow (`&T`) |
| Mutate | Mutable borrow (`~T`) |
| Move/consume | Move (ownership) |

```vibe
let a = 1
let b = vec![1, 2, 3]
let c = String.from("hello")

let closure = fn() {
    print(a)        // a: borrowed (read only)
    b.push(4)       // b: mutably borrowed
    consume(c)      // c: moved
}
```

### Mutable Capture

Closures can mutate captured variables when needed:

```vibe
let counter = 0
let increment = fn() {
    counter = counter + 1  // mutates captured variable
    return counter
}

print(increment())  // 1
print(increment())  // 2
print(counter)      // 2
```

## Closure Types

Use `fn(Params) -> Return` syntax for closure types - mirrors the value syntax:

```vibe
// Basic closure types
fn(i32) -> i32              // takes i32, returns i32
fn(i32, i32) -> bool        // takes two i32s, returns bool
fn() -> void                // takes nothing, returns nothing
fn(String) -> Option<i32>   // takes String, returns Option<i32>

// In variable declarations
let double: fn(i32) -> i32 = fn(x) x * 2

// In function parameters
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

// In struct fields
struct Handler {
    on_click: fn() -> void
    on_value: fn(i32) -> void
}
```

### Examples

```vibe
// Immutable closure - can be called multiple times
let double: fn(i32) -> i32 = fn(x) x * 2
print(double(5))  // 10
print(double(6))  // 12

// Stateful closure
let count = 0
let counter: fn() -> i32 = fn() {
    count = count + 1
    return count
}
print(counter())  // 1
print(counter())  // 2
```

## Type Inference

Types are usually inferred from context:

```vibe
// Inferred from usage
items.map(fn(x) x * 2)  // x inferred as item type

// Inferred from assignment
let f = fn(x) x + 1  // f: fn(i32) -> i32 if used with i32

// Explicit when needed
let f = fn(x: i32) x + 1
let g: fn(i32) -> i32 = fn(x) x + 1
```

## Higher-Order Functions

### Standard Library Functions

```vibe
impl<T> Array<T> {
    fn map<U>(self: &Self, f: fn(T) -> U) -> Array<U>
    fn filter(self: &Self, f: fn(T) -> bool) -> Array<T>
    fn find(self: &Self, f: fn(T) -> bool) -> Option<T>
    fn any(self: &Self, f: fn(T) -> bool) -> bool
    fn all(self: &Self, f: fn(T) -> bool) -> bool
    fn fold<U>(self: &Self, init: U, f: fn(U, T) -> U) -> U
    fn for_each(self: &Self, f: fn(T) -> void)
    fn sort_by(self: ~Self, f: fn(T, T) -> Ordering)
}

impl<T> Option<T> {
    fn map<U>(self: Self, f: fn(T) -> U) -> Option<U>
    fn and_then<U>(self: Self, f: fn(T) -> Option<U>) -> Option<U>
    fn unwrap_or_else(self: Self, f: fn() -> T) -> T
}

impl<T, E> Result<T, E> {
    fn map<U>(self: Self, f: fn(T) -> U) -> Result<U, E>
    fn map_err<F>(self: Self, f: fn(E) -> F) -> Result<T, F>
    fn and_then<U>(self: Self, f: fn(T) -> Result<U, E>) -> Result<U, E>
}
```

### Chaining

```vibe
let result = items
    .filter(fn(x) x.active)
    .map(fn(x) x.value)
    .filter(fn(v) v > 0)
    .fold(0, fn(acc, v) acc + v)
```

### Returning Closures

```vibe
fn make_adder(n: i32) -> fn(i32) -> i32 {
    return fn(x) x + n  // captures n
}

let add5 = make_adder(5)
print(add5(10))  // 15
```

### Storing Closures

```vibe
struct Handler {
    on_click: Option<fn() -> void>
    on_hover: Option<fn(i32, i32) -> void>
}

let handler = Handler {
    on_click: Option.Some(fn() print("clicked")),
    on_hover: Option.Some(fn(x, y) print("hover at ${x}, ${y}")),
}
```

## Memory and Lifetime

### Heap-Allocated Environments

Closure environments are **heap-allocated** to support escaping closures (closures that outlive their creating function):

```vibe
fn make_adder(n: i32) -> fn(i32) -> i32 {
    return fn(x) x + n  // 'n' must outlive make_adder's stack frame
}

let add5 = make_adder(5)  // closure escapes, environment lives on heap
add5(10)  // 15 - environment still valid
```

If environments were stack-allocated, the `env_ptr` would become a dangling pointer when `make_adder` returns.

### Non-Escaping Closures

Even closures that don't escape use heap allocation for simplicity:

```vibe
fn process(items: &Array<i32>) {
    let threshold = 10
    let filtered = items.filter(fn(x) x > threshold)  // heap-allocated env
}
// Environment freed when no longer referenced
```

**Future optimization:** Escape analysis could stack-allocate non-escaping closures.

### Storing Closures in Structs

Since all `fn(T) -> R` types are fat pointers, closures can be stored directly:

```vibe
struct EventSystem {
    handlers: Array<fn(Event) -> void>
}

fn register(self: ~Self, handler: fn(Event) -> void) {
    self.handlers.push(handler)
}
```

No boxing needed - the fat pointer carries everything.

## Implementation

### Fat Pointer Representation

All function types use fat pointers (16 bytes): `{fn_ptr, env_ptr}`

```
fn(T) -> R = {
    fn_ptr: *fn(env_ptr, T) -> R,  // pointer to the actual function
    env_ptr: *void                  // pointer to captured environment (null if none)
}
```

**Non-capturing closures:** `env_ptr` is null, no allocation needed.

**Capturing closures:** `env_ptr` points to a **heap-allocated** struct containing captured values.

This unified representation means:
- All callables have the same type
- No distinction needed between function pointers and closures
- Closures can escape their defining scope safely
- Slight overhead (8 extra bytes + heap allocation) but simpler mental model

### Closure Compilation

Each closure is compiled to a function that takes `env_ptr` as its first parameter:

```vibe
// Source:
let x = 10
let f = fn(y) x + y

// Compiled to:
// 1. Environment struct (heap-allocated via malloc)
struct __Env_1 {
    x: i32  // VALUE of captured x (not pointer)
}

// 2. Closure function (takes env_ptr first)
fn __closure_1(env: *__Env_1, y: i32) -> i32 {
    return env.x + y
}

// 3. Fat pointer construction
let env = malloc(sizeof(__Env_1))
env.x = x  // copy value into environment
let f = {
    fn_ptr: __closure_1,
    env_ptr: env
}

// f(5) becomes __closure_1(f.env_ptr, 5)
```

For non-capturing closures:
```vibe
let double = fn(x) x * 2

// Compiled to:
fn __closure_2(env: *void, x: i32) -> i32 {
    return x * 2  // env is ignored
}

let double = {
    fn_ptr: __closure_2,
    env_ptr: null
}
```

### Mutable Captures

When a closure mutates a captured variable, both the closure and outer scope must share the same storage. This is achieved by "boxing" the variable:

```vibe
// Source:
let count = 0
let increment = fn() {
    count = count + 1
    return count
}

// Compiled to:
// 1. Box the mutable variable on heap
let count_box = malloc(sizeof(i32))
*count_box = 0

// 2. Environment stores pointer to box
struct __Env_1 {
    count: *i32  // pointer to the boxed value
}

// 3. Outer scope's 'count' now points to count_box
// 4. Closure reads/writes through the pointer

increment()  // modifies *count_box
print(count) // reads *count_box - sees the mutation
```

This ensures mutations inside the closure are visible in the outer scope.

### Monomorphization

Generic functions with closure parameters are monomorphized:

```vibe
// Each call site gets a specialized version
items.map(fn(x) x * 2)     // map<i32, i32, __Closure_1>
items.map(fn(x) x.name)    // map<Item, String, __Closure_2>
```

### Function-to-Closure Coercion

Named functions can be used wherever a closure type is expected:

```vibe
fn double(x: i32) -> i32 {
    return x * 2
}

fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

// Named function used as closure argument
apply(double, 21)  // 42

// Store function in closure-typed variable
let f: fn(i32) -> i32 = double
f(10)  // 20
```

**How it works:** The compiler generates a thunk function with the closure calling convention (env_ptr as first parameter), which ignores the environment and calls the original function:

```vibe
// double used as value becomes:
fn __thunk_double(env: *void, x: i32) -> i32 {
    return double(x)  // env ignored
}

// Fat pointer: {fn_ptr: __thunk_double, env_ptr: null}
```

This matches the behavior of Go, Swift, and C# where functions and closures are interchangeable. Thunks are generated once per function and reused.

## LSP Integration

### Syntax Highlighting

| Element | Scope | Color (typical) |
|---------|-------|-----------------|
| `fn` | `keyword.function` | Purple/Keyword |
| `->` | `keyword.operator.arrow` | Purple/Keyword |
| Parameter names | `variable.parameter` | Orange |
| Parameter types | `entity.name.type` | Cyan |
| Captured variables | `variable.other.capture` | Italic |

### Hover Information

**On closure parameter:**
```
x
─
Closure parameter

Type: i32 (inferred)
```

**On closure expression:**
```
fn(x) x * 2
───────────
Closure expression

Type: fn(i32) -> i32
Captures: none
```

**On captured variable:**
```
threshold
─────────
Captured variable

Type: i32
Capture mode: Borrowed (&i32)
Defined at: line 5, column 9
```

### Diagnostics

**Warning: Unused closure result:**
```
warning: closure result unused
 --> src/main.vibe:5:5
  |
5 |     fn(x) { x * 2 }
  |     ^^^^^^^^^^^^^^^ expression result unused
  |
  = help: add explicit `return` or remove the braces
```

**Error: Missing return in block:**
```
error: missing return in closure block
 --> src/main.vibe:5:5
  |
5 | let f = fn(x) {
6 |     let y = x * 2
7 |     y + 1
  |     ^^^^^ expression result unused
8 | }
  |
  = help: add `return y + 1` or remove braces for expression form
```

### Code Actions

**Convert expression to block:**
```vibe
fn(x) x * 2
// Quick action: Convert to block form
fn(x) {
    return x * 2
}
```

**Convert block to expression:**
```vibe
fn(x) {
    return x * 2
}
// Quick action: Convert to expression form
fn(x) x * 2
```

### Inlay Hints

```vibe
let f = fn(x) x * 2
//        ^ : i32  (inlay hint for parameter type)
//              ^ : i32 (inlay hint for return type)

items.map(fn(item) item.value)
//           ^^^^ : &Item (inlay hint)
```

## Design Tradeoffs

### Implicit Capture vs Explicit `move`

Vibelang uses **implicit smart capture** rather than Rust's explicit `move` keyword:

**Rust approach:**
```rust
let x = vec![1, 2, 3];
let f = || x;           // borrows x - cannot escape
let g = move || x;      // moves x into closure - can escape
```

**Vibelang approach:**
```vibe
let x = [1, 2, 3]
let f = fn() x          // automatically works whether escaping or not
```

### Why We Chose Implicit Capture

| Aspect | Rust (`move`) | Vibelang (implicit) |
|--------|---------------|---------------------|
| Syntax | Explicit annotation required | No annotation needed |
| Learning curve | Must understand ownership + closures | Just closures |
| Borrow checker errors | Common source of confusion | None for closures |
| Performance | Zero-cost (stack allocation possible) | Heap allocation overhead |
| Flexibility | Fine-grained control | Simpler mental model |

The tradeoff: **simplicity over performance**. We accept heap allocation overhead to eliminate the cognitive burden of `move` semantics.

### Performance Implications

1. **Fat pointer overhead (minor)**: 16 bytes vs 8 bytes for plain function pointers
2. **Indirect call (moderate)**: Every call goes through a pointer, preventing automatic inlining
3. **Heap allocation (significant)**: `malloc` for every capturing closure

### Comparison with Other Languages

| Language | Capture Model | Allocation | Performance |
|----------|--------------|------------|-------------|
| **Rust** | Explicit `move`, `Fn`/`FnMut`/`FnOnce` traits | Stack or heap (explicit) | Best |
| **Go** | Implicit capture by reference | Heap (escaping) | Good |
| **Swift** | Implicit capture, `@escaping` annotation | Heap + reference counting | Moderate |
| **C#** | Implicit capture | Heap (delegates) | Moderate |
| **Vibelang** | Implicit smart capture | Heap | On par with Go/Swift/C# |

Vibelang is on par with Go, Swift, and C# in closure performance. Rust is faster but requires more annotations.

### Future Optimizations

These optimizations could improve performance while keeping implicit capture:

1. **Escape analysis**: Detect non-escaping closures and stack-allocate their environments
2. **Closure inlining**: Inline small closures at call sites (especially in `map`/`filter`)
3. **Monomorphization**: Specialize generic functions for each closure type
4. **Non-capturing optimization**: Use plain function pointers when `env_ptr` is always null

## Implementation Phases

### Phase 1: Basic Closures (DONE)
- Parse `fn(args) expr` syntax
- Parse `fn(args) { block }` syntax
- Parse `fn(args) -> Type expr` for return type annotation
- Parse `fn(Params) -> Return` as a type
- Fat pointer representation: `{fn_ptr, env_ptr}`
- Non-capturing closures work (env_ptr = null)

### Phase 2: Variable Capture (DONE)
- Free variable analysis
- Heap-allocated environment structs
- Immutable captures (copy value into environment)
- Mutable captures (box variable, share pointer)
- Returning closures from functions
- Storing closures in structs
- Function-to-closure coercion (thunk generation)

### Phase 3: Advanced Features (Planned)
- Generic functions with closure parameters
- Higher-order function patterns (map, filter, fold)
- Recursive closures

### Phase 4: LSP Support (Planned)
- Syntax highlighting
- Hover for closures and captures
- Inlay hints for inferred types
- Diagnostics for capture errors
- Code actions

### Phase 5: Optimization (Planned)
- Escape analysis for stack allocation
- Inline small closures
- Monomorphization for closure parameters
- Non-capturing closures → direct function calls

## Examples

### Iterator Pipeline

```vibe
fn process_orders(orders: Array<Order>) -> Array<Summary> {
    return orders
        .filter(fn(o) o.status == Status.Complete)
        .map(fn(o) {
            let total = o.items.fold(0, fn(sum, item) sum + item.price)
            return Summary {
                order_id: o.id,
                total: total,
                item_count: o.items.len(),
            }
        })
        .filter(fn(s) s.total > 0)
}
```

### Event Handler

```vibe
struct Button {
    on_click: Option<fn() -> void>
}

fn main() {
    let message = "Hello!"

    let button = Button {
        on_click: Option.Some(fn() {
            print(message)
        }),
    }

    if let Option.Some(handler) = button.on_click {
        handler()
    }
}
```

### Lazy Evaluation

```vibe
fn expensive_default() -> Config {
    print("Computing default config...")
    return Config.load_from_disk()
}

fn get_config(cached: Option<Config>) -> Config {
    return cached.unwrap_or_else(fn() expensive_default())
}

// expensive_default only called if cached is None
```

### Stateful Closure

```vibe
fn make_counter() -> fn() -> i32 {
    let count = 0
    return fn() {
        count = count + 1
        return count
    }
}

fn main() {
    let counter = make_counter()
    print(counter())  // 1
    print(counter())  // 2
    print(counter())  // 3
}
```

## Open Questions

1. **Closure size limit**: Should there be a limit on closure capture size before requiring Box?
2. **Recursive closures**: How to handle closures that call themselves?
3. **Async closures**: How will closures interact with async/await (future feature)?

## Resolved Questions

1. **Function-to-closure coercion**: ✓ Named functions automatically coerce to closure types via thunk generation.
2. **Escaping closures**: ✓ Heap allocation ensures closures can outlive their creating scope.
