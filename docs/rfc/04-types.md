# Section 4: Types

Vibelang has a minimal set of primitive types that map directly to LLVM, plus a rich set of standard library types built from those primitives.

## 4.1 Type System Overview

```
Types
├── Primitive Types (LLVM-backed)
│   ├── Integer: i8, i16, i32, i64, u8, u16, u32, u64
│   ├── Float: f32, f64
│   ├── Boolean: bool
│   ├── Void: void
│   └── Pointer: *T
│
├── Composite Types
│   ├── Fixed Array: T[N]
│   ├── Struct: struct { ... }
│   └── Enum: enum { ... }
│
└── Standard Library Types (written in Vibelang)
    ├── Array<T>: Dynamic array
    ├── Slice<T>: Borrowed view
    ├── String: UTF-8 string
    ├── Map<K, V>: Hash map
    ├── Set<T>: Hash set
    ├── Option<T>: Nullable
    └── Result<T, E>: Error handling
```

## 4.2 Type Categories

### Copy Types

Copy types can be implicitly duplicated. The compiler will copy them when needed:

```vibelang
let a: i32 = 42
let b = a           // a is copied, both a and b are valid
print(a)            // OK: a still valid
print(b)            // OK: b has its own copy
```

All primitive types are Copy:
- All integers: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- All floats: `f32`, `f64`
- Boolean: `bool`
- Raw pointers: `*T`

Fixed arrays of Copy types are also Copy:
```vibelang
let a: i32[3] = [1, 2, 3]
let b = a           // copies all 12 bytes
```

### Owned Types

Owned types have single ownership. Assignment moves the value:

```vibelang
let a = String.from("hello")
let b = a           // a is MOVED to b
print(&a)           // ERROR: a was moved
print(&b)           // OK: b owns the string
```

Owned types include:
- `Array<T>` — Dynamic array
- `String` — UTF-8 string
- `Map<K, V>` — Hash map
- Any struct containing owned types

### Borrowed Types

Borrowed types provide temporary access without ownership:

```vibelang
let s = String.from("hello")
let slice: Slice<u8> = &s       // borrow as slice
print(slice.len)                 // OK: using borrowed data
// s is still valid and will be freed at scope end
```

Borrowed types include:
- `Slice<T>` — View into contiguous data
- `&T` — Reference to any type (read-only borrow)
- `~T` — Mutable reference to any type (mutable borrow)

## 4.3 Type Inference

The compiler infers types when possible:

```vibelang
let x = 42                      // inferred: i32 (default integer)
let y = 3.14                    // inferred: f64 (default float)
let s = "hello"                 // inferred: Slice<u8> (string literal)
let arr = [1, 2, 3]             // inferred: i32[3]
let vec = Array<i32>()          // explicit generic required
```

Type annotations can be added for clarity or disambiguation:

```vibelang
let x: i64 = 42                 // explicit: i64
let y: f32 = 3.14               // explicit: f32
let arr: u8[4] = [1, 2, 3, 4]   // explicit: u8[4]
```

## 4.4 Type Compatibility

### Equality

Two types are equal if they have the same structure:

```vibelang
i32 == i32                      // equal
i32[3] == i32[3]                // equal
i32[3] != i32[4]                // not equal (different size)
Array<i32> == Array<i32>        // equal
Array<i32> != Array<i64>        // not equal (different element type)
```

### Coercion

Vibelang has minimal implicit coercion:

```vibelang
// No implicit numeric coercion
let x: i64 = 42i32              // ERROR: type mismatch
let x: i64 = 42i32 as i64       // OK: explicit cast

// Slice coercion from arrays
let arr = [1, 2, 3]
let slice: Slice<i32> = &arr    // OK: array to slice

// String to slice
let s = String.from("hello")
let slice: Slice<u8> = &s       // OK: String to Slice<u8>
```

## 4.5 Generic Types

Types can be parameterized:

```vibelang
struct Pair<T, U> {
    first: T
    second: U
}

let p = Pair<i32, String> {
    first: 42,
    second: String.from("hello")
}
```

See [Section 4.3: Generic Types](./04.03-generics.md) for details.

## 4.6 Type Layout

The compiler determines memory layout based on type category:

| Type | Layout |
|------|--------|
| `i8`, `u8`, `bool` | 1 byte |
| `i16`, `u16` | 2 bytes |
| `i32`, `u32`, `f32` | 4 bytes |
| `i64`, `u64`, `f64`, `*T` | 8 bytes |
| `T[N]` | `N * sizeof(T)` bytes, inline |
| `struct` | Sum of fields with alignment padding |
| `Array<T>` | 24 bytes (ptr + len + cap) |
| `Slice<T>` | 16 bytes (ptr + len) |
