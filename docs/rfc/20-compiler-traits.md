# Section 20: Compiler-Coupled Traits

Certain traits have special meaning to the compiler. These "lang items" enable syntax sugar and operator overloading while keeping the actual implementations in user-definable code.

## 20.1 Design Philosophy

Following Rust's approach, Vibelang couples the compiler to specific well-known traits rather than using ad-hoc special cases:

- **Syntax maps to traits**: Operators like `+`, `==`, `[]` call trait methods
- **User-implementable**: Any type can implement these traits
- **Zero runtime overhead**: Monomorphization eliminates indirection
- **Consistent semantics**: Same syntax always means same trait method

### Implementation Status

| Feature | Status | Notes |
|---------|--------|-------|
| `==`, `!=` | ✅ Implemented | Dispatches to `Eq::eq()` for structs |
| `<`, `>`, `<=`, `>=` | ✅ Implemented | Dispatches to `Ord::cmp()` for structs |
| `+`, `-`, `*`, `/`, `%` | ✅ Implemented | Dispatches to `Add`/`Sub`/`Mul`/`Div`/`Rem` for structs |
| `a[i]` | ⏳ Planned | Currently built-in for arrays/slices |
| `for x in iter` | ⏳ Planned | Currently shape-based struct check |
| `?` operator | ⏳ Planned | Currently same-type return only |
| `as` | ✅ Primitives only | Matches Rust's design |
| `From`/`Into` | ⏳ Traits defined | Dispatch not yet implemented |

### How It Works

For struct types, operators dispatch to trait methods:
```vibelang
let p1 = Point { x: 1, y: 2 }
let p2 = Point { x: 3, y: 4 }

p1 == p2    // calls Point::eq(&p1, &p2)
p1 < p2     // calls Point::cmp(&p1, &p2), compares to Ordering.Less
p1 + p2     // calls Point::add(p1, p2)
```

For primitive types, operators use raw LLVM instructions (no trait dispatch) to avoid infinite recursion.

## 20.2 Comparison Traits

### 20.2.1 Eq

Equality comparison. Already implemented.

```vibelang
trait Eq {
    fn eq(&self, other: &Self) -> bool
}

// Compiler desugars:
a == b    // becomes: a.eq(&b)
a != b    // becomes: not a.eq(&b)
```

### 20.2.2 Ord

Ordering comparison for totally ordered types.

```vibelang
enum Ordering {
    Less
    Equal
    Greater
}

trait Ord {
    fn cmp(&self, other: &Self) -> Ordering
}

// Compiler desugars:
a < b     // becomes: a.cmp(&b) == Ordering.Less
a <= b    // becomes: a.cmp(&b) != Ordering.Greater
a > b     // becomes: a.cmp(&b) == Ordering.Greater
a >= b    // becomes: a.cmp(&b) != Ordering.Less
```

### 20.2.3 PartialOrd (Optional)

For types with partial ordering (e.g., floats with NaN):

```vibelang
trait PartialOrd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
}
```

If a type implements `Ord`, comparison operators use `cmp()`. If only `PartialOrd`, they use `partial_cmp()` and propagate `None` as `false`.

## 20.3 Arithmetic Traits

### 20.3.1 Add, Sub, Mul, Div, Rem

```vibelang
trait Add<Rhs = Self> {
    type Output
    fn add(self, rhs: Rhs) -> Self::Output
}

trait Sub<Rhs = Self> {
    type Output
    fn sub(self, rhs: Rhs) -> Self::Output
}

trait Mul<Rhs = Self> {
    type Output
    fn mul(self, rhs: Rhs) -> Self::Output
}

trait Div<Rhs = Self> {
    type Output
    fn div(self, rhs: Rhs) -> Self::Output
}

trait Rem<Rhs = Self> {
    type Output
    fn rem(self, rhs: Rhs) -> Self::Output
}

// Compiler desugars:
a + b     // becomes: a.add(b)
a - b     // becomes: a.sub(b)
a * b     // becomes: a.mul(b)
a / b     // becomes: a.div(b)
a % b     // becomes: a.rem(b)
```

### 20.3.2 Neg

Unary negation:

```vibelang
trait Neg {
    type Output
    fn neg(self) -> Self::Output
}

// Compiler desugars:
-a        // becomes: a.neg()
```

### 20.3.3 Compound Assignment Traits

```vibelang
trait AddAssign<Rhs = Self> {
    fn add_assign(&mut self, rhs: Rhs)
}

// Similar for SubAssign, MulAssign, DivAssign, RemAssign

// Compiler desugars:
a += b    // becomes: a.add_assign(b)
```

### 20.3.4 Primitive Implementations

The standard library provides implementations for all primitive numeric types:

```vibelang
impl Add for i32 {
    type Output = i32
    fn add(self, rhs: i32) -> i32 {
        // intrinsic: llvm.add
    }
}

impl Add for i64 { ... }
impl Add for f32 { ... }
impl Add for f64 { ... }
// etc.
```

## 20.4 Indexing Traits

### 20.4.1 Index

Read-only indexing:

```vibelang
trait Index<Idx> {
    type Output
    fn index(&self, idx: Idx) -> &Self::Output
}

// Compiler desugars:
a[i]      // becomes: *a.index(i)  (in read context)
```

### 20.4.2 IndexMut

Mutable indexing:

```vibelang
trait IndexMut<Idx>: Index<Idx> {
    fn index_mut(&mut self, idx: Idx) -> &mut Self::Output
}

// Compiler desugars:
a[i] = v  // becomes: *a.index_mut(i) = v
```

### 20.4.3 Example Implementation

```vibelang
impl<T> Index<i64> for Vec<T> {
    type Output = T
    fn index(&self, idx: i64) -> &T {
        if idx < 0 or idx >= self.len {
            panic("index out of bounds")
        }
        unsafe { return &*ptr_add(self.ptr, idx) }
    }
}

impl<T> IndexMut<i64> for Vec<T> {
    fn index_mut(&mut self, idx: i64) -> &mut T {
        if idx < 0 or idx >= self.len {
            panic("index out of bounds")
        }
        unsafe { return &mut *ptr_add(self.ptr, idx) }
    }
}
```

## 20.5 Iterator Traits

### 20.5.1 Iterator

```vibelang
trait Iterator {
    type Item
    fn next(&mut self) -> Option<Self::Item>
}
```

### 20.5.2 IntoIterator

Converts a collection into an iterator:

```vibelang
trait IntoIterator {
    type Item
    type IntoIter: Iterator<Item = Self::Item>
    fn into_iter(self) -> Self::IntoIter
}

// Compiler desugars for-in loops:
for x in collection {
    body
}

// becomes:
let mut iter = collection.into_iter()
loop {
    match iter.next() {
        Option.Some(x) => { body }
        Option.None => break
    }
}
```

### 20.5.3 Reference Iteration

By convention, collections implement `IntoIterator` for both owned and borrowed forms:

```vibelang
impl<T> IntoIterator for Vec<T> {
    type Item = T
    type IntoIter = VecIterator<T>
    fn into_iter(self) -> VecIterator<T> { ... }
}

impl<'a, T> IntoIterator for &Vec<T> {
    type Item = &T
    type IntoIter = VecRefIterator<T>
    fn into_iter(self) -> VecRefIterator<T> { ... }
}

// Usage:
for item in vec { }           // consumes vec, item: T
for item in &vec { }          // borrows vec, item: &T
```

## 20.6 Try Traits

### 20.6.1 Try

For the `?` operator:

```vibelang
trait Try {
    type Output
    type Residual

    fn from_output(output: Self::Output) -> Self
    fn branch(self) -> ControlFlow<Self::Residual, Self::Output>
}

enum ControlFlow<B, C> {
    Continue(C)
    Break(B)
}
```

### 20.6.2 FromResidual

Converts residuals (errors) between types:

```vibelang
trait FromResidual<R> {
    fn from_residual(residual: R) -> Self
}
```

### 20.6.3 ? Operator Desugaring

```vibelang
// This:
let value = expression?

// Desugars to:
let value = match Try::branch(expression) {
    ControlFlow.Continue(v) => v
    ControlFlow.Break(r) => return FromResidual::from_residual(r)
}
```

### 20.6.4 Result and Option Implementations

```vibelang
impl<T, E> Try for Result<T, E> {
    type Output = T
    type Residual = Result<!, E>  // never type for Ok

    fn from_output(output: T) -> Result<T, E> {
        return Result.Ok(output)
    }

    fn branch(self) -> ControlFlow<Result<!, E>, T> {
        match self {
            Result.Ok(v) => ControlFlow.Continue(v)
            Result.Err(e) => ControlFlow.Break(Result.Err(e))
        }
    }
}

impl<T, E, F: From<E>> FromResidual<Result<!, E>> for Result<T, F> {
    fn from_residual(residual: Result<!, E>) -> Result<T, F> {
        match residual {
            Result.Err(e) => Result.Err(F::from(e))
        }
    }
}
```

This enables automatic error conversion:

```vibelang
fn outer() -> Result<i32, AppError> {
    let x = inner()?  // IoError automatically converts to AppError
    return Result.Ok(x)
}

fn inner() -> Result<i32, IoError> { ... }
```

## 20.7 Conversion Traits

### 20.7.1 From and Into

```vibelang
trait From<T> {
    fn from(value: T) -> Self
}

trait Into<T> {
    fn into(self) -> T
}

// Blanket implementation: From implies Into
impl<T, U: From<T>> Into<U> for T {
    fn into(self) -> U {
        return U::from(self)
    }
}
```

### 20.7.2 TryFrom and TryInto

Fallible conversions:

```vibelang
trait TryFrom<T> {
    type Error
    fn try_from(value: T) -> Result<Self, Self::Error>
}

trait TryInto<T> {
    type Error
    fn try_into(self) -> Result<T, Self::Error>
}
```

### 20.7.3 as vs From/Into

| Feature | `as` | `From`/`Into` |
|---------|------|---------------|
| Scope | Primitive types only | Any types |
| Safety | May truncate/lose precision | Type-checked |
| Fallibility | Always succeeds | `TryFrom` for fallible |
| Syntax | `x as T` | `T::from(x)` or `x.into()` |

```vibelang
// as: primitive casts only
let i: i32 = 42
let f: f64 = i as f64       // OK: built-in primitive cast
let s: String = i as String // ERROR: as only works on primitives

// From/Into: custom conversions
let s: String = String::from(i)  // OK: uses From trait
let s: String = i.into()         // OK: uses Into trait
```

## 20.8 Default Trait

```vibelang
trait Default {
    fn default() -> Self
}

// Usage
let v: Vec<i32> = Default::default()
let config = Config::default()
```

## 20.9 Clone and Copy Traits

### 20.9.1 Clone

Explicit duplication:

```vibelang
trait Clone {
    fn clone(&self) -> Self
}

let a = vec![1, 2, 3]
let b = a.clone()  // explicit copy
```

### 20.9.2 Copy

Marker trait for types that can be implicitly copied:

```vibelang
trait Copy: Clone { }

// Primitives are Copy
let a: i32 = 42
let b = a  // copy, a still usable

// Heap types are not Copy
let s = String::from("hello")
let t = s  // move, s no longer usable
```

## 20.10 Implementation Priority

### Phase 1: Core Operations ✅ Complete
1. ✅ `Eq` for equality operators (`==`, `!=`)
2. ✅ `Ord` for comparison operators (`<`, `>`, `<=`, `>=`)
3. ✅ `Add`, `Sub`, `Mul`, `Div`, `Rem` for arithmetic (`+`, `-`, `*`, `/`, `%`)

### Phase 2: Indexing and Iteration (Planned)
4. `Index`/`IndexMut` for subscript syntax (`a[i]`)
5. `Iterator`/`IntoIterator` for for-loops

### Phase 3: Error Handling (Planned)
6. `Try`/`FromResidual` for `?` operator with error conversion
7. `From`/`Into`, `TryFrom`/`TryInto` for conversions

### Phase 4: Utilities (Planned)
8. `Neg` for unary negation
9. `Default`, `Clone`, `Copy`

## 20.11 Working Example

Here's a complete example showing custom trait implementations:

```vibelang
struct Point {
    x: i64
    y: i64
}

// Custom equality - compare fields
impl Eq for Point {
    fn eq(&self, other: &Point) -> bool {
        return self.x == other.x and self.y == other.y
    }
}

// Custom ordering - compare by distance from origin
impl Ord for Point {
    fn cmp(&self, other: &Point) -> Ordering {
        let self_dist = self.x * self.x + self.y * self.y
        let other_dist = other.x * other.x + other.y * other.y
        if self_dist < other_dist {
            return Ordering.Less
        }
        if self_dist > other_dist {
            return Ordering.Greater
        }
        return Ordering.Equal
    }
}

// Custom addition - vector addition
impl Add for Point {
    fn add(self, other: Point) -> Point {
        return Point {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

fn main() {
    let p1 = Point { x: 1, y: 2 }
    let p2 = Point { x: 1, y: 2 }
    let p3 = Point { x: 3, y: 4 }

    // Uses Eq trait
    if p1 == p2 { println("equal") }      // prints "equal"

    // Uses Ord trait
    if p1 < p3 { println("p1 closer") }   // prints "p1 closer"

    // Uses Add trait
    let p4 = p1 + p3                       // Point { x: 4, y: 6 }
}
```

## 20.12 Compiler Recognition

The compiler recognizes these traits by their fully-qualified names in the standard library:

```vibelang
// std/src/ops/mod.vibe
pub trait Add<Rhs = Self> { ... }
pub trait Sub<Rhs = Self> { ... }
// ...

// std/src/cmp/mod.vibe
pub trait Eq { ... }
pub trait Ord { ... }
pub trait PartialOrd { ... }

// std/src/iter/mod.vibe
pub trait Iterator { ... }
pub trait IntoIterator { ... }

// std/src/ops/try.vibe
pub trait Try { ... }
pub trait FromResidual<R> { ... }

// std/src/convert/mod.vibe
pub trait From<T> { ... }
pub trait Into<T> { ... }
pub trait TryFrom<T> { ... }
pub trait TryInto<T> { ... }
```

The compiler maintains a mapping of language items to trait paths, enabling it to desugar operators and syntax into trait method calls during semantic analysis.
