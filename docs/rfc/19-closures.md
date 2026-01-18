# Section 19: Closures (Anonymous Functions)

Closures are anonymous functions that can capture variables from their enclosing scope.

## 19.1 Syntax

### Expression Body

Single-expression closures with implicit return:

```vibelang
let double = fn(x) x * 2
let add = fn(a, b) a + b
let greet = fn(name) "Hello, " + name
```

### Block Body

Multi-statement closures require explicit `return`:

```vibelang
let complex = fn(x) {
    let y = x * 2
    return y + 1
}
```

### Type Annotations

Parameters and return types can be explicitly annotated:

```vibelang
// Parameter types
let double = fn(x: i32) x * 2

// Return type
let typed = fn(x: i32) -> i32 x * 2

// Both
let full = fn(a: i32, b: i32) -> i32 a + b
```

### No Parameters

```vibelang
let get_ten = fn() 10
let greet = fn() print("Hello!")
```

## 19.2 Closure Types

The type of a closure is written as `fn(Params) -> Return`:

```vibelang
let f: fn(i32) -> i32 = fn(x) x * 2
let g: fn(i32, i32) -> bool = fn(a, b) a < b
let h: fn() -> void = fn() print("hi")
```

## 19.3 Capture Semantics

Closures automatically capture variables from their enclosing scope:

```vibelang
let multiplier = 10
let scale = fn(x) x * multiplier  // captures 'multiplier'

scale(5)  // 50
```

### Multiple Captures

```vibelang
let a = 3
let b = 4
let add_both = fn(x) x + a + b  // captures 'a' and 'b'

add_both(10)  // 17
```

### Smart Capture Inference

The compiler infers the capture mode based on usage:

| Usage in Closure | Capture Mode |
|------------------|--------------|
| Read only | Immutable borrow (`&T`) |
| Mutate | Mutable borrow (`~T`) |
| Move/consume | Move (ownership) |

```vibelang
let a = 1              // a: borrowed (read only)
let b = vec![1, 2, 3]  // b: mutably borrowed if modified
let c = String.new()   // c: moved if consumed

let closure = fn() {
    print(a)        // a captured by reference
    b.push(4)       // b captured mutably
    consume(c)      // c moved into closure
}
```

## 19.4 Implementation

### Fat Pointer Representation

All function types (including closures) use a 16-byte fat pointer:

```
fn(T) -> R = {
    fn_ptr: *fn(env_ptr, T) -> R,  // pointer to the function code
    env_ptr: *void                  // pointer to captured environment (null if none)
}
```

**Non-capturing closures:** `env_ptr` is null, no allocation needed.

**Capturing closures:** `env_ptr` points to a **heap-allocated** struct containing captured values.

### Environment Struct

For a closure that captures variables, the compiler generates an environment struct:

```vibelang
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
```

### Mutable Captures

When a closure mutates a captured variable, both the closure and outer scope must share the same storage. This is achieved by "boxing" the variable on the heap:

```vibelang
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

// 3. Both closure and outer scope access through count_box
increment()  // modifies *count_box
print(count) // reads *count_box - sees the mutation
```

### Unified Representation

This unified representation means:
- All callables have the same type and calling convention
- No distinction needed between function pointers and closures
- Functions and closures are interchangeable where types match
- Closures can escape their defining scope safely (heap allocation)

### Function-to-Closure Coercion

Named functions automatically coerce to closure types:

```vibelang
fn double(x: i32) -> i32 {
    return x * 2
}

fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

apply(double, 21)  // 42 - named function used as closure
```

The compiler generates a thunk with the closure calling convention:

```vibelang
// Compiler generates:
fn __thunk_double(env: *void, x: i32) -> i32 {
    return double(x)  // env ignored
}

// Fat pointer: {fn_ptr: __thunk_double, env_ptr: null}
```

This matches Go, Swift, and C# where functions and closures are interchangeable.

## 19.5 Higher-Order Functions

Closures work seamlessly with higher-order functions:

### Passing Closures

```vibelang
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

let result = apply(fn(x) x * 2, 21)  // 42
```

### Standard Library Functions

```vibelang
impl<T> Array<T> {
    fn map<U>(self: &Self, f: fn(T) -> U) -> Array<U>
    fn filter(self: &Self, f: fn(T) -> bool) -> Array<T>
    fn fold<U>(self: &Self, init: U, f: fn(U, T) -> U) -> U
    fn for_each(self: &Self, f: fn(T) -> void)
}
```

### Chaining

```vibelang
let result = items
    .filter(fn(x) x > 0)
    .map(fn(x) x * 2)
    .fold(0, fn(acc, x) acc + x)
```

## 19.6 Returning Closures

Functions can return closures that capture their local variables:

```vibelang
fn make_adder(n: i32) -> fn(i32) -> i32 {
    return fn(x) x + n  // captures n
}

let add5 = make_adder(5)
add5(10)  // 15
```

## 19.7 Storing Closures

Closures can be stored in structs and collections:

```vibelang
struct Handler {
    on_click: Option<fn() -> void>
    on_value: fn(i32) -> void
}

let handler = Handler {
    on_click: Option.Some(fn() print("clicked")),
    on_value: fn(x) print("value: ${x}"),
}
```

## 19.8 Nested Closures

Closures can be nested, with inner closures capturing from outer closures:

```vibelang
let x = 1
let outer = fn(a) {
    let inner = fn(b) a + b + x  // captures 'a' from outer, 'x' from main
    return inner(10)
}
```

## 19.9 Type Inference

Types are usually inferred from context:

```vibelang
// Inferred from usage
items.map(fn(x) x * 2)  // x inferred as item type

// Inferred from assignment
let f = fn(x) x + 1  // f: fn(i32) -> i32 if used with i32

// Explicit when needed
let f = fn(x: i32) x + 1
let g: fn(i32) -> i32 = fn(x) x + 1
```

## 19.10 Examples

### Counter

```vibelang
fn make_counter() -> fn() -> i32 {
    let count = 0
    return fn() {
        count = count + 1
        return count
    }
}

let counter = make_counter()
counter()  // 1
counter()  // 2
counter()  // 3
```

### Event Handler

```vibelang
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

```vibelang
fn expensive_default() -> Config {
    print("Computing default config...")
    return Config.load_from_disk()
}

fn get_config(cached: Option<Config>) -> Config {
    return cached.unwrap_or_else(fn() expensive_default())
}

// expensive_default only called if cached is None
```

### Iterator Pipeline

```vibelang
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

## 19.11 Design Tradeoffs

### Implicit Capture vs Explicit `move`

Vibelang uses **implicit smart capture** rather than Rust's explicit `move` keyword:

```rust
// Rust - explicit control
let x = vec![1, 2, 3];
let f = || x;           // borrows x - cannot escape
let g = move || x;      // moves x into closure - can escape
```

```vibelang
// Vibelang - implicit capture
let x = [1, 2, 3]
let f = fn() x          // automatically works whether escaping or not
```

### Why Implicit Capture

| Aspect | Rust (`move`) | Vibelang (implicit) |
|--------|---------------|---------------------|
| Syntax | Explicit annotation required | No annotation needed |
| Learning curve | Must understand ownership + closures | Just closures |
| Borrow checker errors | Common source of confusion | None for closures |
| Performance | Zero-cost (stack allocation possible) | Heap allocation overhead |

The tradeoff: **simplicity over performance**. We accept heap allocation overhead to eliminate the cognitive burden of `move` semantics.

### Performance Comparison

| Language | Capture Model | Allocation | Performance |
|----------|--------------|------------|-------------|
| **Rust** | Explicit `move`, `Fn`/`FnMut`/`FnOnce` traits | Stack or heap (explicit) | Best |
| **Go** | Implicit capture by reference | Heap (escaping) | Good |
| **Swift** | Implicit capture, `@escaping` annotation | Heap + reference counting | Moderate |
| **C#** | Implicit capture | Heap (delegates) | Moderate |
| **Vibelang** | Implicit smart capture | Heap | On par with Go/Swift/C# |

### Future Optimizations

These could improve performance while keeping implicit capture:

1. **Escape analysis**: Stack-allocate non-escaping closures
2. **Closure inlining**: Inline small closures at call sites
3. **Monomorphization**: Specialize generic functions for each closure type
4. **Non-capturing optimization**: Use plain function pointers when `env_ptr` is always null

## 19.12 Implementation Status

### Phase 1: Basic Closures ✓
- Parse `fn(args) expr` syntax
- Parse `fn(args) { block }` syntax
- Parse `fn(args) -> Type expr` for return type annotation
- Parse `fn(Params) -> Return` as a type
- Fat pointer representation: `{fn_ptr, env_ptr}`
- Non-capturing closures work (env_ptr = null)

### Phase 2: Variable Capture ✓
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
