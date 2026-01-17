# Proposal: Attributes and Macros

## Summary

This proposal introduces two syntactic constructs for metaprogramming in Vibelang:

1. **Attributes (`@`)** - Decorators that attach metadata to declarations
2. **Function-like macros (`!`)** - Code generation invoked like function calls

## Motivation

Vibelang needs mechanisms for:
- Compile-time code generation (derive Clone, Debug, serialization)
- Conditional compilation (`@cfg`)
- Testing infrastructure (`@test`)
- Field metadata for serialization, protocols, database mapping
- Reducing boilerplate without runtime cost

## Syntax

### Attributes (`@`)

Attributes decorate declarations. They attach metadata or trigger compiler behavior.

```vibe
@derive(Clone, Debug)
@repr(C)
struct Packet {
    @json(name="deviceId")
    @protocol(offset=0, endian=Big)
    id: u32

    @json(name="payload")
    @protocol(offset=4)
    data: Slice<u8>
}

@test
fn test_packet_parsing() {
    // ...
}

@cfg(target_os="linux")
fn linux_only() {
    // ...
}

@inline
fn hot_loop() {
    // ...
}
```

**Attribute placement:**
- Before `struct`, `enum`, `fn`, `impl` - item-level attribute
- Before field definition - field-level attribute

**Attribute syntax:**
```
@name                    // simple attribute
@name(value)             // single argument
@name(key=value)         // named argument
@name(a, b, c)           // multiple arguments
@name(a, key=value)      // mixed arguments
```

### Function-like Macros (`!`)

Macros are invoked like functions but with `!` suffix. They operate on tokens at compile time.

```vibe
println!("Hello, {}!", name)
let v = vec![1, 2, 3, 4, 5]
let s = format!("{} + {} = {}", a, b, a + b)
assert!(x > 0, "x must be positive")
sql!(SELECT * FROM users WHERE id = {user_id})
```

**Bracket variants:**
```vibe
name!(...)    // parentheses - expression-like
name![...]    // brackets - collection-like
name!{...}    // braces - block-like
```

## Built-in Attributes

### Derive Attributes

| Attribute | Effect |
|-----------|--------|
| `@derive(Copy)` | Enable implicit copy semantics (struct must contain only Copy types) |
| `@derive(Clone)` | Generate `fn clone(&self) -> Self` method |
| `@derive(Debug)` | Generate `fn debug(&self) -> String` method |
| `@derive(Default)` | Generate `fn default() -> Self` with default values |
| `@derive(Eq)` | Generate `fn eq(&self, other: &Self) -> bool` method |
| `@derive(Hash)` | Generate `fn hash(&self) -> u64` method |

Multiple derives can be combined:
```vibe
@derive(Copy, Clone, Debug, Eq)
struct Point {
    x: i32
    y: i32
}
```

### Compiler Attributes

| Attribute | Level | Effect |
|-----------|-------|--------|
| `@repr(C)` | struct | C-compatible memory layout |
| `@repr(packed)` | struct | No padding between fields |
| `@repr(align=N)` | struct | Minimum alignment of N bytes |
| `@inline` | fn | Hint to inline function |
| `@inline(always)` | fn | Force inlining |
| `@inline(never)` | fn | Never inline |
| `@cold` | fn | Unlikely to be called (optimize for size) |
| `@deprecated` | any | Warn on use |
| `@deprecated(msg)` | any | Warn with custom message |
| `@must_use` | fn | Warn if return value is ignored |

### Conditional Compilation

```vibe
@cfg(target_os="linux")
fn use_epoll() { ... }

@cfg(target_os="macos")
fn use_kqueue() { ... }

@cfg(debug)
fn debug_assert(cond: bool) { ... }

@cfg(not(debug))
fn debug_assert(cond: bool) { }  // no-op in release

@cfg(any(target_os="linux", target_os="freebsd"))
fn use_unix_sockets() { ... }

@cfg(all(target_arch="x86_64", feature="simd"))
fn fast_path() { ... }
```

**Cfg predicates:**
- `target_os` - "linux", "macos", "windows"
- `target_arch` - "x86_64", "aarch64", "wasm32"
- `debug` / `release` - build mode
- `feature` - enabled features
- `not(...)`, `any(...)`, `all(...)` - boolean combinators

### Testing Attributes

```vibe
@test
fn test_basic() {
    assert!(1 + 1 == 2)
}

@test
@should_panic
fn test_divide_by_zero() {
    let _ = 1 / 0
}

@test
@ignore
fn test_slow() {
    // skipped unless explicitly requested
}

@bench
fn bench_parsing() {
    // benchmark
}
```

### Field Attributes

Field attributes attach metadata used by derive macros or reflection:

```vibe
@derive(JsonCodec, DbModel)
struct User {
    @json(name="user_id", skip_if_none=true)
    @db(column="id", primary_key=true, auto_increment=true)
    id: Option<u64>

    @json(name="userName")
    @db(column="username", unique=true)
    name: String

    @json(skip=true)
    @db(column="password_hash")
    password: String

    @json(name="createdAt", format="iso8601")
    @db(column="created_at", default="now()")
    created: DateTime
}
```

## User-Defined Macros

### Declaring a Derive Macro

```vibe
@macro_derive(JsonCodec)
fn json_codec_derive(input: DeriveInput) -> TokenStream {
    let name = input.name
    let fields = input.fields

    let to_json_fields = fields.map(|f| {
        let json_name = f.get_attr("json", "name").unwrap_or(f.name)
        let skip = f.get_attr("json", "skip").unwrap_or(false)
        if skip {
            quote! {}
        } else {
            quote! { pairs.push(format!("\"{}\": {}", ${json_name}, self.${f.name}.to_json())) }
        }
    })

    quote! {
        impl ${name} {
            fn to_json(self: &Self) -> String {
                let pairs: Array<String> = []
                ${to_json_fields}
                "{" + pairs.join(", ") + "}"
            }
        }
    }
}
```

### Declaring an Attribute Macro

```vibe
@macro_attribute(test)
fn test_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    let fn_def = parse_fn(item)
    let fn_name = fn_def.name

    quote! {
        ${item}

        @test_registration
        fn __register_${fn_name}() {
            TEST_REGISTRY.push(TestCase {
                name: stringify!(${fn_name}),
                func: ${fn_name},
            })
        }
    }
}
```

### Declaring a Function-like Macro

```vibe
@macro
fn vec(input: TokenStream) -> TokenStream {
    let elements = parse_comma_separated(input)
    let len = elements.len()

    quote! {
        {
            let arr: Array<_> = Array.with_capacity(${len})
            ${elements.map(|e| quote! { arr.push(${e}) })}
            arr
        }
    }
}

// Usage: vec![1, 2, 3]
```

### Declaring a Declarative Macro (simpler)

For simpler pattern-based macros:

```vibe
@macro_rules
macro max {
    ($a:expr, $b:expr) => {
        if $a > $b { $a } else { $b }
    }
    ($a:expr, $b:expr, $($rest:expr),+) => {
        max!($a, max!($b, $($rest),+))
    }
}

// Usage: max!(1, 2, 3, 4)
```

## DeriveInput Structure

The `DeriveInput` type passed to derive macros:

```vibe
struct DeriveInput {
    name: String                    // struct/enum name
    generics: Array<Generic>        // type parameters
    fields: Array<FieldInfo>        // struct fields
    variants: Array<VariantInfo>    // enum variants (if enum)
    attrs: Array<Attribute>         // attributes on the type
}

struct FieldInfo {
    name: String
    ty: Type
    attrs: Array<Attribute>

    fn get_attr(self: &Self, namespace: &str, key: &str) -> Option<AttrValue>
}

struct Attribute {
    name: String
    args: Map<String, AttrValue>
}

enum AttrValue {
    Bool(bool)
    Int(i64)
    Float(f64)
    String(String)
    Ident(String)
    Array(Array<AttrValue>)
}
```

## Copy and Clone Semantics

### Copy

`@derive(Copy)` marks a type as implicitly copyable:
- All assignments copy instead of move
- Original value remains valid after assignment
- Only allowed if all fields are also Copy types

```vibe
@derive(Copy, Clone)
struct Point {
    x: i32    // i32 is Copy
    y: i32    // i32 is Copy
}

let p1 = Point { x: 1, y: 2 }
let p2 = p1      // Copy - p1 still valid
let p3 = p1      // Copy - p1 still valid
```

**Implicitly Copy types:**
- All primitives: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `bool`, `char`
- References: `&T`, `~T`
- Tuples of Copy types: `(i32, i32)`
- Arrays of Copy types with known size: `[i32; 4]`

**Not Copy (must move):**
- Structs without `@derive(Copy)`
- `String`, `Array<T>`, `Slice<T>`
- Any type containing non-Copy fields

### Clone

`@derive(Clone)` generates an explicit clone method:

```vibe
@derive(Clone)
struct Buffer {
    data: Array<u8>    // Array is not Copy
    len: u64
}

let b1 = Buffer { data: [1, 2, 3], len: 3 }
let b2 = b1          // Move - b1 now invalid
// let b3 = b1       // Error: use of moved value

let b4 = Buffer { data: [4, 5, 6], len: 3 }
let b5 = b4.clone()  // Clone - b4 still valid
let b6 = b4.clone()  // Clone - b4 still valid
```

### Destructuring Semantics

Destructuring follows move/copy semantics:

```vibe
@derive(Copy, Clone)
struct Point { x: i32, y: i32 }

let p = Point { x: 1, y: 2 }
let {x, y} = p       // Copy - p still valid (Point is Copy)
let z = p.x          // OK

struct Line { start: Point, end: Point }  // Line is NOT Copy

let line = Line { start: Point{x:0,y:0}, end: Point{x:1,y:1} }
let {start, end} = line   // Move - line now invalid
// let s = line.start     // Error: use of moved value
```

## Implementation Phases

### Phase 1: Attribute Parsing
- Parse `@name` and `@name(...)` syntax
- Store attributes in AST for structs, enums, functions, fields
- No semantic effects yet

### Phase 2: Built-in Derives
- Implement `@derive(Copy)` - compiler flag for copy semantics
- Implement `@derive(Clone)` - generate clone method
- Implement `@derive(Debug)` - generate debug method

### Phase 3: Compiler Attributes
- Implement `@test` - test registration
- Implement `@cfg(...)` - conditional compilation
- Implement `@repr(...)` - memory layout control

### Phase 4: User-Defined Macros
- Token stream types
- Quote syntax for code generation
- `@macro_derive`, `@macro_attribute`, `@macro`
- Macro hygiene

### Phase 5: Declarative Macros
- `@macro_rules` for pattern-based macros
- Standard macro patterns

## Alternatives Considered

### Rust-style `#[...]`
Rejected: `@` is more visually distinct and familiar from other languages (Python, TypeScript, Java annotations).

### Attributes inside struct body
```vibe
struct Packet {
    @derive(Copy)  // struct-level

    header: u8
}
```
Rejected: Ambiguous whether attribute applies to struct or first field. Before-declaration is clearer.

### `#` for macros instead of `!`
Rejected: `!` is familiar from Rust and clearly marks macro invocation.

## Field Attribute Behavior

Field attributes are metadata - they have no effect unless a derive macro reads them:

```vibe
// No @derive(JsonCodec) - @json attributes are just stored metadata
struct Packet {
    @json(name="deviceId")  // no effect without derive
    id: u32
}
```

**Behavior:**
- Attributes are always parsed and stored in the AST
- Derive macros read field attributes via `field.get_attr("json", "name")`
- Unused field attributes trigger a warning:
  ```
  warning: @json attribute on field 'id' has no effect without @derive(JsonCodec)
  ```

**No implicit derives** - all code generation must be explicitly requested at the struct level. This keeps behavior predictable and visible.

## LSP Integration

The language server must fully support attributes and macros for a good developer experience.

### Syntax Highlighting

| Element | Scope | Color (typical) |
|---------|-------|-----------------|
| `@` | `punctuation.definition.attribute` | Yellow/Gold |
| `derive`, `test`, `cfg` | `entity.name.attribute` | Yellow/Gold |
| `Copy`, `Clone`, `Debug` | `entity.name.type.derive` | Cyan/Type color |
| `name=`, `offset=` | `variable.parameter.attribute` | Orange/Parameter |
| `"deviceId"` | `string.quoted` | Green/String |
| `!` in `println!` | `punctuation.definition.macro` | Yellow/Gold |
| `println`, `vec` | `entity.name.function.macro` | Yellow/Gold |

**TextMate grammar additions:**
```json
{
  "patterns": [
    {
      "name": "meta.attribute.vibe",
      "begin": "@",
      "end": "(?=\\s|$|\\{|fn|struct|enum|let)",
      "patterns": [
        {
          "name": "entity.name.attribute.vibe",
          "match": "\\b(derive|test|cfg|repr|inline|deprecated|must_use)\\b"
        },
        {
          "name": "entity.name.type.derive.vibe",
          "match": "\\b(Copy|Clone|Debug|Default|Eq|Hash)\\b"
        }
      ]
    },
    {
      "name": "entity.name.function.macro.vibe",
      "match": "\\b([a-z_][a-z0-9_]*)!"
    }
  ]
}
```

### Autocomplete

**Struct-level attributes:**
```
@  →  @derive(...)
      @repr(...)
      @cfg(...)
      @deprecated
```

**Inside @derive():**
```
@derive(  →  Copy
             Clone
             Debug
             Default
             Eq
             Hash
             JsonCodec  (if available)
```

**Field-level attributes:**
```
@  →  @json(...)
      @protocol(...)
      @db(...)
      (context-aware based on struct derives)
```

**Inside @json():**
```
@json(  →  name="..."
           skip=true
           skip_if_none=true
           flatten=true
           default="..."
```

**Macro invocations:**
```
println  →  println!(...)
vec      →  vec![...]
format   →  format!(...)
assert   →  assert!(...)
```

### Hover Information

**On @derive:**
```
@derive(Clone)
────────────────
Generates a `clone` method that creates a deep copy of the struct.

Generated method:
  fn clone(&self) -> Self
```

**On field attribute:**
```
@json(name="deviceId")
────────────────────────
JSON serialization options for this field.

• name: Field name in JSON output (default: field name)
• skip: Exclude from serialization (default: false)
• skip_if_none: Omit if Option::None (default: false)
```

**On macro invocation:**
```
println!("Hello, {}!", name)
────────────────────────────
Prints formatted text to stdout with newline.

Arguments:
  format: Format string with {} placeholders
  args: Values to interpolate

Expands to:
  print(format!("Hello, {}!\n", name))
```

### Go to Definition

| Element | Jumps to |
|---------|----------|
| `@derive(JsonCodec)` | `@macro_derive(JsonCodec)` function |
| `println!` | `@macro fn println(...)` definition |
| `@test` | `@macro_attribute(test)` function |
| Built-in derives | Documentation / compiler intrinsic |

### Diagnostics

**Error: Invalid attribute:**
```
error[E0401]: unknown attribute `@jsn`
 --> src/main.vibe:5:5
  |
5 |     @jsn(name="id")
  |     ^^^^ did you mean `@json`?
```

**Warning: Unused attribute:**
```
warning[W0201]: unused attribute `@json`
 --> src/main.vibe:5:5
  |
5 |     @json(name="deviceId")
  |     ^^^^^^^^^^^^^^^^^^^^^^ this attribute has no effect
  |
  = help: add `@derive(JsonCodec)` to the struct
```

**Error: Invalid derive for type:**
```
error[E0402]: cannot derive `Copy` for `Buffer`
 --> src/main.vibe:1:10
  |
1 | @derive(Copy)
  |         ^^^^ `Copy` requires all fields to be `Copy`
  |
3 |     data: Array<u8>
  |     --------------- `Array<u8>` is not `Copy`
```

**Error: Missing required attribute argument:**
```
error[E0403]: missing required argument `offset` for `@protocol`
 --> src/main.vibe:5:5
  |
5 |     @protocol(endian=Big)
  |     ^^^^^^^^^^^^^^^^^^^^^ missing `offset`
```

### Code Actions

**Add missing derive:**
```
@json(name="deviceId")  // warning: unused without derive
id: u32

Quick fix: Add @derive(JsonCodec) to struct
```

**Generate derive implementation:**
```
@derive(Clone)
struct Point { ... }

Code action: Expand derive to explicit impl
→ Generates:
  impl Point {
      fn clone(&self) -> Point {
          Point { x: self.x, y: self.y }
      }
  }
```

### Semantic Tokens

Provide semantic token types for enhanced highlighting:

| Token | Semantic Type | Modifiers |
|-------|--------------|-----------|
| `@derive` | `decorator` | `declaration` |
| `Copy` | `type` | `defaultLibrary` |
| `@json` | `decorator` | - |
| `name` in `name=` | `parameter` | - |
| `println!` | `macro` | - |

## Open Questions

1. **Macro hygiene**: How do we handle identifier hygiene in generated code?
2. **Compile-time evaluation**: Should macros have access to type information or only tokens?
3. **Error messages**: How do we provide good error messages for macro-generated code?
4. **Macro ordering**: Can macros depend on other macros? How to handle ordering?
5. **Reflection**: Should field attributes be available at runtime via reflection?
6. **LSP macro expansion**: Should the LSP show expanded macro code on hover/go-to-definition?
