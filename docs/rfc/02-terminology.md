# Section 2: Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in RFC 2119.

## 2.1 Definitions

### Ownership Terms

**Owner**
: The binding that is responsible for freeing a value. Every heap-allocated value MUST have exactly one owner at any point in time.

**Move**
: Transfer of ownership from one binding to another. After a move, the source binding is no longer valid.

**Borrow**
: Temporary access to a value without taking ownership. The borrower MUST NOT outlive the owner.

**Copy**
: Creating a new, independent instance of a value. Copy types can be implicitly duplicated.

### Type Terms

**Primitive Type**
: A type that maps directly to an LLVM type. Includes integers, floats, booleans, and pointers.

**Composite Type**
: A type composed of other types. Includes structs, enums, arrays, and slices.

**Generic Type**
: A type parameterized by one or more type variables. Written as `Type<T>`.

**Copy Type**
: A type whose values can be duplicated by copying bits. All primitive types are Copy types.

**Owned Type**
: A type whose values have a single owner responsible for cleanup. Includes `Array<T>`, `String`, and structs containing owned types.

### Storage Terms

**Stack**
: Memory region for local variables with known size. Automatically reclaimed when scope exits.

**Heap**
: Memory region for dynamically-sized or long-lived data. Must be explicitly allocated and freed.

**Static**
: Memory region for compile-time constants. Lives for the entire program duration.

**Inline**
: Storage where data is embedded directly, not through a pointer.

### Lifetime Terms

**Scope**
: A region of code delimited by `{` and `}`. Variables declared in a scope are freed when the scope exits.

**Lifetime**
: The span during which a value is valid. For owned values, this extends from creation to the end of the owner's scope or until moved.

**Escape**
: When a value or reference leaves its original scope, typically by being returned from a function.

## 2.2 Notation

### Type Notation

```
T           Type variable
T[N]        Fixed array of N elements of type T
Array<T>    Dynamic array of type T
Slice<T>    Borrowed view into contiguous T elements
*T          Raw pointer to T
&T          Borrow of T (read-only)
~T          Mutable borrow of T (vibing)
```

### Grammar Notation

```
name        Non-terminal symbol
'text'      Literal text
[x]         Optional x
{x}         Zero or more x
x | y       x or y
(x)         Grouping
```

## 2.3 Type Categories

### Copy Types

Types that are implicitly copyable:

- All integer types: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- All float types: `f32`, `f64`
- Boolean: `bool`
- Raw pointers: `*T`
- Fixed arrays of Copy types: `T[N]` where T is Copy
- Tuples of Copy types

### Owned Types

Types that have single ownership:

- `Array<T>` — Dynamic array
- `String` — UTF-8 string
- `Map<K, V>` — Hash map
- Structs containing any Owned type
- Enums containing any Owned type

### Borrowed Types

Types that represent borrowed access:

- `Slice<T>` — View into array or fixed array
- `&T` — Reference to any type (read-only)
- `~T` — Mutable reference to any type
- String literals (borrow static memory)

## 2.4 Value Categories

### Owned Values

A value is owned when:

1. It was created by allocation (`Array.new()`, `String.from()`)
2. It was received by value in a function parameter
3. It was returned from a function
4. Ownership was transferred via move

### Borrowed Values

A value is borrowed when:

1. It was received by reference in a function parameter (`&T` for read-only, `~T` for mutable)
2. It was created by taking a reference (`&x`)
3. It is a slice into owned data (`&arr[0..10]`)
4. It is a string literal (`"hello"`)

## 2.5 Conventions

### Naming Conventions

| Entity | Convention | Example |
|--------|------------|---------|
| Variables | snake_case | `token_count` |
| Functions | snake_case | `parse_expression` |
| Types | PascalCase | `TokenKind` |
| Constants | SCREAMING_SNAKE | `MAX_TOKENS` |
| Generics | Single uppercase | `T`, `E`, `K`, `V` |

### File Conventions

| Extension | Meaning |
|-----------|---------|
| `.vibe` | Vibelang source file |
| `.ll` | LLVM IR output |
| `.o` | Object file |

### Comment Conventions

```vibelang
// Single line comment

/* 
   Multi-line
   comment
*/

/// Documentation comment for the following item
fn documented_function() { }
```
