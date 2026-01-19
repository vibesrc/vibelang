# Proposal: Traits

## Summary

This proposal introduces traits to Vibelang - a mechanism for defining shared behavior that types can implement. Traits enable:

1. **Type constraints** - Restrict generic parameters to types with specific capabilities
2. **Derive macros** - Auto-generate trait implementations via `@derive`
3. **Polymorphism** - Define interfaces that multiple types can implement

## Motivation

Currently, Vibelang has generics but no way to constrain them. This leads to:

```vibe
// Today: This compiles but crashes at codegen if T doesn't have `hash()`
fn insert<K, V>(map: ~Map<K, V>, key: K, value: V) {
    let h = key.hash()  // What if K doesn't have hash()?
    // ...
}

// With traits: Compile-time guarantee that K is hashable
fn insert<K: Hash, V>(map: ~Map<K, V>, key: K, value: V) {
    let h = key.hash()  // Guaranteed to exist
    // ...
}
```

Traits also enable the `@derive` macro system to generate implementations:

```vibe
@derive(Hash, Eq, Debug)
struct Point {
    x: i32
    y: i32
}
// Compiler generates Hash, Eq, and Debug implementations
```

## Syntax

### Trait Definition

```vibe
trait Hash {
    fn hash(&self) -> u64
}

trait Eq {
    fn eq(&self, other: &Self) -> bool
}

trait Clone {
    fn clone(&self) -> Self
}

trait Debug {
    fn debug(&self) -> String
}

trait Default {
    fn default() -> Self
}
```

### Trait with Multiple Methods

```vibe
trait Iterator<T> {
    fn next(&self) -> Option<T>
    fn has_next(&self) -> bool
}

trait Ord: Eq {  // Ord requires Eq (supertrait)
    fn cmp(&self, other: &Self) -> Ordering

    // Default implementations
    fn lt(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering.Less
    }

    fn gt(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering.Greater
    }

    fn le(&self, other: &Self) -> bool {
        not self.gt(other)
    }

    fn ge(&self, other: &Self) -> bool {
        not self.lt(other)
    }
}
```

### Trait Implementation

```vibe
impl Hash for Point {
    fn hash(&self) -> u64 {
        let mut h = 0u64
        h = h ^ hash_i64(self.x as i64)
        h = h ^ hash_i64(self.y as i64)
        h
    }
}

impl Eq for Point {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x and self.y == other.y
    }
}

impl Debug for Point {
    fn debug(&self) -> String {
        format!("Point {{ x: {}, y: {} }}", self.x, self.y)
    }
}
```

### Generic Trait Implementation

```vibe
impl<T: Debug> Debug for Vec<T> {
    fn debug(&self) -> String {
        let parts = self.iter().map(fn(x) x.debug())
        "[" + parts.join(", ") + "]"
    }
}

impl<T: Clone> Clone for Vec<T> {
    fn clone(&self) -> Self {
        let result = Vec<T>.new()
        for item in self {
            result.push(item.clone())
        }
        result
    }
}
```

## Type Constraints

### Single Constraint

```vibe
fn print_debug<T: Debug>(value: &T) {
    println(value.debug())
}

fn sort<T: Ord>(arr: ~Vec<T>) {
    // Can use <, >, == on T
}
```

### Multiple Constraints

```vibe
// Using + syntax
fn hash_and_compare<T: Hash + Eq>(a: &T, b: &T) -> bool {
    a.hash() == b.hash() and a.eq(b)
}

// Using where clause for complex bounds
fn complex<K, V>(map: &Map<K, V>) -> String
where
    K: Hash + Eq + Debug,
    V: Debug + Clone,
{
    // ...
}
```

### Constraint on Struct

```vibe
struct SortedVec<T: Ord> {
    items: Vec<T>
}

impl<T: Ord> SortedVec<T> {
    fn insert(&self, item: T) {
        // Can use comparison operators
    }
}
```

## Built-in Traits

### Marker Traits

| Trait  | Meaning                                      |
| ------ | -------------------------------------------- |
| `Copy` | Type is implicitly copied (bitwise)          |
| `Sync` | Type is safe to share between threads        |
| `Send` | Type can be transferred between threads      |

```vibe
// Copy is a marker - no methods, just compiler behavior
trait Copy {}

// Only types with all Copy fields can implement Copy
@derive(Copy)
struct Point { x: i32, y: i32 }  // OK: i32 is Copy

@derive(Copy)
struct Buffer { data: Vec<u8> }  // ERROR: Vec is not Copy
```

### Standard Traits

| Trait     | Method(s)                          | Purpose                   |
| --------- | ---------------------------------- | ------------------------- |
| `Clone`   | `fn clone(&self) -> Self`          | Explicit deep copy        |
| `Debug`   | `fn debug(&self) -> String`        | Debug representation      |
| `Default` | `fn default() -> Self`             | Default value             |
| `Eq`      | `fn eq(&self, other: &Self) -> bool` | Equality comparison     |
| `Hash`    | `fn hash(&self) -> u64`            | Hash value                |
| `Ord`     | `fn cmp(&self, other: &Self) -> Ordering` | Total ordering     |

### Collection Traits

| Trait            | Methods                              | Purpose              |
| ---------------- | ------------------------------------ | -------------------- |
| `Iterator<T>`    | `next`, `has_next`                   | Iteration            |
| `IntoIterator<T>`| `fn into_iter(self) -> Iterator<T>` | Convert to iterator  |
| `FromIterator<T>`| `fn from_iter(iter: Iterator<T>) -> Self` | Build from iterator |

## Integration with @derive

Traits power the `@derive` macro system. When you write:

```vibe
@derive(Hash, Eq, Clone, Debug)
struct User {
    id: u64
    name: String
    email: String
}
```

The compiler generates:

```vibe
impl Hash for User {
    fn hash(&self) -> u64 {
        let mut h = 0u64
        h = h ^ self.id.hash()
        h = h ^ self.name.hash()
        h = h ^ self.email.hash()
        h
    }
}

impl Eq for User {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id and self.name == other.name and self.email == other.email
    }
}

impl Clone for User {
    fn clone(&self) -> Self {
        User {
            id: self.id,
            name: self.name.clone(),
            email: self.email.clone(),
        }
    }
}

impl Debug for User {
    fn debug(&self) -> String {
        format!("User {{ id: {}, name: \"{}\", email: \"{}\" }}",
                self.id, self.name, self.email)
    }
}
```

### Derive Requirements

For `@derive` to work, all fields must implement the trait:

```vibe
@derive(Hash)
struct Container {
    value: CustomType  // ERROR if CustomType doesn't implement Hash
}
```

### Custom Derive Macros

Users can create new derivable traits:

```vibe
trait JsonCodec {
    fn to_json(&self) -> String
    fn from_json(json: &str) -> Result<Self, JsonError>
}

@macro_derive(JsonCodec)
fn derive_json_codec(input: DeriveInput) -> TokenStream {
    // Generate implementation based on fields
    // Field attributes like @json(name="...") are accessible
}
```

## Trait Objects (Dynamic Dispatch)

For runtime polymorphism, use `dyn Trait`:

```vibe
trait Drawable {
    fn draw(&self)
}

impl Drawable for Circle {
    fn draw(&self) { /* ... */ }
}

impl Drawable for Rectangle {
    fn draw(&self) { /* ... */ }
}

// Static dispatch (monomorphized)
fn draw_static<T: Drawable>(shape: &T) {
    shape.draw()
}

// Dynamic dispatch (vtable)
fn draw_dynamic(shape: &dyn Drawable) {
    shape.draw()
}

// Vec of different types
let shapes: Vec<&dyn Drawable> = vec![&circle, &rectangle]
for shape in shapes {
    shape.draw()
}
```

### Object Safety

A trait is object-safe (can be used as `dyn Trait`) if:

- No methods return `Self`
- No methods have generic type parameters
- All methods have `&self` or `~self` receiver

```vibe
// Object-safe
trait Draw {
    fn draw(&self)
}

// NOT object-safe (returns Self)
trait Clone {
    fn clone(&self) -> Self  // Can't know size at runtime
}
```

## Associated Types

Traits can have associated types:

```vibe
trait Iterator {
    type Item

    fn next(&self) -> Option<Self.Item>
}

impl Iterator for Range {
    type Item = i64

    fn next(&self) -> Option<i64> {
        if self.current < self.end {
            let val = self.current
            self.current += 1
            Option.Some(val)
        } else {
            Option.None
        }
    }
}
```

### In Bounds

```vibe
fn sum<I: Iterator>(iter: I) -> I.Item
where
    I.Item: Add,
{
    // ...
}
```

## Blanket Implementations

Implement a trait for all types matching a bound:

```vibe
// Any type that implements Debug can be printed
impl<T: Debug> Printable for T {
    fn print(&self) {
        println(self.debug())
    }
}

// Now any Debug type has print()
let p = Point { x: 1, y: 2 }
p.print()  // Works because Point: Debug
```

## Operator Traits

Operators map to trait methods:

| Operator | Trait   | Method                           |
| -------- | ------- | -------------------------------- |
| `==`     | `Eq`    | `fn eq(&self, other: &Self) -> bool` |
| `<`      | `Ord`   | `fn cmp(&self, other: &Self) -> Ordering` |
| `+`      | `Add`   | `fn add(self, other: Self) -> Self` |
| `-`      | `Sub`   | `fn sub(self, other: Self) -> Self` |
| `*`      | `Mul`   | `fn mul(self, other: Self) -> Self` |
| `/`      | `Div`   | `fn div(self, other: Self) -> Self` |
| `[]`     | `Index` | `fn index(&self, idx: I) -> &Self.Output` |

```vibe
trait Add<Rhs = Self> {
    type Output
    fn add(self, rhs: Rhs) -> Self.Output
}

impl Add for Point {
    type Output = Point
    fn add(self, other: Point) -> Point {
        Point { x: self.x + other.x, y: self.y + other.y }
    }
}

let p1 = Point { x: 1, y: 2 }
let p2 = Point { x: 3, y: 4 }
let p3 = p1 + p2  // Uses Add::add
```

## Implementation Strategy

### Phase 1: Core Trait System

- Trait definition syntax
- `impl Trait for Type` syntax
- Basic type constraints `<T: Trait>`
- Built-in traits: `Copy`, `Clone`, `Eq`, `Hash`, `Debug`, `Default`

### Phase 2: Derive Integration

- Wire `@derive(Trait)` to generate `impl Trait`
- Derive implementations for built-in traits
- Error messages for missing trait implementations on fields

### Phase 3: Advanced Features

- Associated types
- Supertraits (`trait Ord: Eq`)
- Default method implementations
- Where clauses

### Phase 4: Dynamic Dispatch

- `dyn Trait` syntax
- Vtable generation
- Object safety checking

### Phase 5: Operator Traits

- Operator overloading via traits
- Standard operator traits in prelude

## LSP Integration

### Diagnostics

**Missing trait bound:**
```
error[E0277]: the trait bound `K: Hash` is not satisfied
 --> src/main.vibe:5:18
  |
5 |     let h = key.hash()
  |                  ^^^^ the method `hash` exists but `K` doesn't implement `Hash`
  |
help: consider adding a trait bound
  |
3 | fn insert<K: Hash, V>(map: ~Map<K, V>, key: K, value: V) {
  |            ++++++
```

**Cannot derive:**
```
error[E0277]: cannot derive `Hash` for `Container`
 --> src/main.vibe:1:10
  |
1 | @derive(Hash)
  |         ^^^^ `CustomType` doesn't implement `Hash`
  |
3 |     value: CustomType
  |     ----------------- this field doesn't implement `Hash`
```

### Autocomplete

- Show trait methods on types that implement them
- Suggest `@derive(...)` for common traits
- Suggest trait bounds when calling trait methods on generics

### Hover

On trait method call:
```
point.hash()
────────────
(method) Hash::hash(&self) -> u64

Implemented by: Point
Trait: std.hash.Hash
```

On `@derive`:
```
@derive(Clone)
──────────────
Generates: impl Clone for Point

Generated method:
  fn clone(&self) -> Self
```

## Open Questions

1. **Coherence rules**: How do we prevent conflicting trait implementations?
   - Rust's orphan rules? Or something simpler?

2. **Negative bounds**: Should we support `T: !Copy` (T does NOT implement Copy)?

3. **Const traits**: Should we have traits for compile-time evaluation?

4. **Specialization**: Can a more specific impl override a blanket impl?

5. **Auto traits**: Should `Send`/`Sync` be automatically derived based on fields?

## Alternatives Considered

### Go-style Interfaces

Implicit implementation (if methods match, type implements interface):

```go
type Reader interface {
    Read(p []byte) (n int, err error)
}
// Any type with matching Read method implements Reader
```

**Rejected**: Explicit `impl` is clearer and catches accidental implementations.

### C++ Concepts

More powerful but more complex constraint system:

```cpp
template<typename T>
concept Hashable = requires(T a) {
    { a.hash() } -> std::convertible_to<uint64_t>;
};
```

**Rejected**: Rust-style traits are simpler and sufficient.

### No Traits (Duck Typing)

Just require methods exist, check at monomorphization:

**Rejected**: Error messages would be terrible, and we want explicit contracts.
