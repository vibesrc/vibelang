# Section 7: Expressions

Expressions evaluate to values. In Vibelang, most constructs are expressions, including `if`, `match`, and blocks.

## 7.1 Literal Expressions

### Integer Literals

```vibelang
42                      // i32 (default)
42i8                    // i8
42i16                   // i16
42i32                   // i32
42i64                   // i64
42u8                    // u8
42u16                   // u16
42u32                   // u32
42u64                   // u64

0xFF                    // hexadecimal
0o77                    // octal
0b1010                  // binary
1_000_000               // underscores for readability
```

### Float Literals

```vibelang
3.14                    // f64 (default)
3.14f32                 // f32
3.14f64                 // f64
1.0e10                  // scientific notation
2.5E-3                  // scientific notation
```

### Boolean Literals

```vibelang
true
false
```

### String Literals

```vibelang
"hello"                 // Slice<u8>, points to static data
"hello\nworld"          // with escape sequences
r"no\escapes"           // raw string
```

### Character Literals

```vibelang
'a'                     // u8
'\n'                    // newline
'\x41'                  // hex escape (65 = 'A')
```

### Array Literals

```vibelang
[1, 2, 3]               // i32[3]
[0; 100]                // i32[100], all zeros
[1.0, 2.0, 3.0]         // f64[3]
```

## 7.2 Arithmetic Expressions

### Binary Arithmetic

```vibelang
a + b                   // addition
a - b                   // subtraction
a * b                   // multiplication
a / b                   // division
a % b                   // modulo (remainder)
```

### Unary Arithmetic

```vibelang
-x                      // negation
```

### Arithmetic Type Rules

- Both operands MUST have the same type
- Result has the same type as operands
- No implicit numeric coercion

```vibelang
let a: i32 = 10
let b: i64 = 20
let c = a + b           // ERROR: type mismatch

let c = a as i64 + b    // OK: explicit cast
```

### Integer Division

Integer division truncates toward zero:

```vibelang
7 / 3                   // 2
-7 / 3                  // -2 (not -3)
7 / -3                  // -2
```

### Division by Zero

Division by zero is undefined behavior for integers. Use checked division for safety:

```vibelang
let result = checked_div(a, b)  // returns Option<i32>
```

## 7.3 Comparison Expressions

### Equality

```vibelang
a == b                  // equal
a != b                  // not equal
```

### Ordering

```vibelang
a < b                   // less than
a <= b                  // less than or equal
a > b                   // greater than
a >= b                  // greater than or equal
```

### Comparison Type Rules

- Both operands MUST have the same type
- Result is always `bool`
- Signed vs unsigned uses appropriate comparison

```vibelang
let s: i32 = -1
let u: u32 = 1
s < u                   // ERROR: type mismatch

let s: i32 = -1
let t: i32 = 1
s < t                   // true (signed comparison)
```

### Chained Comparisons (Not Supported)

```vibelang
a < b < c               // ERROR: not supported
a < b and b < c         // OK: use explicit 'and'
```

## 7.4 Logical Expressions

### Logical Operators

```vibelang
a and b                 // logical AND
a or b                  // logical OR
not a                   // logical NOT
```

### Short-Circuit Evaluation

Logical operators short-circuit:

```vibelang
false and expensive()   // expensive() not called
true or expensive()     // expensive() not called
```

### Logical Type Rules

- Operands MUST be `bool`
- Result is `bool`

```vibelang
let x = 1
if x { }                // ERROR: expected bool, got i32
if x != 0 { }           // OK
```

## 7.5 Bitwise Expressions

### Bitwise Operators

```vibelang
a & b                   // bitwise AND
a | b                   // bitwise OR
a ^ b                   // bitwise XOR
!a                      // bitwise NOT
a << n                  // left shift
a >> n                  // right shift
```

### Shift Behavior

- Left shift fills with zeros
- Right shift: arithmetic (sign-extending) for signed, logical (zero-filling) for unsigned

```vibelang
let s: i32 = -8
let u: u32 = 0xFFFFFFFF

s >> 1                  // -4 (sign preserved)
u >> 1                  // 0x7FFFFFFF (zero filled)
```

### Shift Amount

Shift amount MUST be non-negative and less than bit width:

```vibelang
1 << 31                 // OK for i32
1 << 32                 // undefined behavior for i32
1 << -1                 // undefined behavior
```

## 7.6 Assignment Expressions

### Simple Assignment

```vibelang
x = 10                  // assigns 10 to x
```

### Compound Assignment

```vibelang
x += 1                  // x = x + 1
x -= 1                  // x = x - 1
x *= 2                  // x = x * 2
x /= 2                  // x = x / 2
x %= 3                  // x = x % 3
x &= mask               // x = x & mask
x |= flag               // x = x | flag
x ^= bits               // x = x ^ bits
x <<= n                 // x = x << n
x >>= n                 // x = x >> n
```

### Assignment is Not an Expression

Unlike C, assignment does not return a value:

```vibelang
let y = (x = 10)        // ERROR: assignment is not an expression
if (x = 10) { }         // ERROR: prevents accidental = vs ==
```

## 7.7 Call Expressions

### Function Calls

```vibelang
foo()                   // no arguments
bar(1, 2, 3)            // with arguments
baz(a, b, c: 10)        // named argument (if supported)
```

### Method Calls

```vibelang
s.len()                 // method on s
s.push("hello")         // method with argument
point.distance(&other)  // method with reference argument
```

### Chained Calls

```vibelang
text.trim().split(",").first()
```

### Associated Function Calls

```vibelang
String.new()
Array<i32>.with_capacity(100)
Option.Some(42)
```

## 7.8 Index Expressions

### Array Indexing

```vibelang
arr[0]                  // first element
arr[n]                  // nth element
arr[arr.len() - 1]      // last element
```

### Slice Indexing

```vibelang
slice[i]                // element at index i
```

### Bounds Checking

Index out of bounds causes a panic:

```vibelang
let arr = [1, 2, 3]
arr[10]                 // PANIC: index out of bounds
```

Use `get()` for safe access:

```vibelang
arr.get(10)             // returns Option<&T>
```

## 7.9 Slice Expressions

### Range Slicing

```vibelang
arr[start..end]         // elements from start to end-1
arr[start..]            // elements from start to end
arr[..end]              // elements from 0 to end-1
arr[..]                 // all elements
```

### Slice Result

Slicing produces a `Slice<T>`:

```vibelang
let arr = [1, 2, 3, 4, 5]
let s: Slice<i32> = &arr[1..4]  // [2, 3, 4]
```

## 7.10 Field Access Expressions

### Struct Fields

```vibelang
point.x
point.y
person.name
```

### Nested Fields

```vibelang
company.ceo.name
rect.top_left.x
```

### Pointer Field Access

```vibelang
ptr.field               // auto-dereference
(*ptr).field            // explicit dereference
```

## 7.11 Reference Expressions

### Taking References

```vibelang
&x                      // read-only borrow of x
~x                      // mutable borrow of x
&arr[0]                 // read-only borrow of first element
~arr[0]                 // mutable borrow of first element
&point.x                // read-only borrow of field
~point.x                // mutable borrow of field
```

### Dereference

```vibelang
*ptr                    // dereference pointer
*ref                    // dereference reference
```

## 7.12 Cast Expressions

### Type Casts

```vibelang
x as i64                // cast x to i64
f as i32                // cast float to int (truncates)
p as *u8                // cast pointer type
```

### Valid Casts

| From | To | Notes |
|------|-----|-------|
| Integer | Integer | May truncate or sign-extend |
| Integer | Float | May lose precision |
| Float | Integer | Truncates toward zero |
| Float | Float | May lose precision |
| Pointer | Pointer | Reinterprets |
| Integer | Pointer | Unsafe |
| Pointer | Integer | Unsafe |

## 7.13 Block Expressions

Blocks are expressions that evaluate to their last expression:

```vibelang
let x = {
    let a = 1
    let b = 2
    a + b               // block evaluates to 3
}
// x == 3
```

### Empty Blocks

Empty blocks evaluate to `void`:

```vibelang
let x = { }             // x has type void
```

### Blocks with Semicolons

A trailing semicolon makes the block evaluate to `void`:

```vibelang
let x = {
    let a = 1
    a + 1;              // semicolon: block evaluates to void
}
// x has type void
```

## 7.14 If Expressions

`if` is an expression that returns a value:

```vibelang
let max = if a > b { a } else { b }
```

### If-Else Chains

```vibelang
let grade = if score >= 90 {
    "A"
} else if score >= 80 {
    "B"
} else if score >= 70 {
    "C"
} else {
    "F"
}
```

### Type Rules

- All branches MUST have the same type
- If no `else`, the type is `void`

```vibelang
let x = if cond { 1 } else { 2 }        // OK: both i32
let y = if cond { 1 } else { "hi" }     // ERROR: type mismatch
let z = if cond { 1 }                   // z has type void (no else)
```

## 7.15 Match Expressions

`match` is an expression:

```vibelang
let name = match token.kind {
    TokenKind.Ident => "identifier"
    TokenKind.Number(_) => "number"
    _ => "other"
}
```

### Match with Blocks

```vibelang
let result = match opt {
    Option.Some(x) => {
        let doubled = x * 2
        doubled + 1
    }
    Option.None => 0
}
```

See [Section 11: Pattern Matching](./11-pattern-matching.md) for full details.

## 7.16 Closure Expressions

Anonymous functions:

```vibelang
let add = fn(a: i32, b: i32) -> i32 { return a + b }
let double = fn(x: i32) -> i32 { return x * 2 }
```

### Closure as Arguments

```vibelang
let nums = [1, 2, 3, 4, 5]
let doubled = nums.map(fn(x: &i32) -> i32 { return *x * 2 })
let evens = nums.filter(fn(x: &i32) -> bool { return *x % 2 == 0 })
```

### Closure Captures

Closures capture variables from their environment:

```vibelang
let multiplier = 3
let scale = fn(x: i32) -> i32 { return x * multiplier }
scale(10)               // 30
```

Captures are by reference by default. Use `move` for ownership:

```vibelang
let s = String.from("hello")
let closure = move fn() { print(&s) }   // s moved into closure
```

## 7.17 Operator Precedence

From highest to lowest:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 | `.` `()` `[]` | Left |
| 2 | Unary `-` `not` `!` `&` `~` `*` | Right |
| 3 | `as` | Left |
| 4 | `*` `/` `%` | Left |
| 5 | `+` `-` | Left |
| 6 | `<<` `>>` | Left |
| 7 | `&` | Left |
| 8 | `^` | Left |
| 9 | `\|` | Left |
| 10 | `==` `!=` `<` `<=` `>` `>=` | Left |
| 11 | `and` | Left |
| 12 | `or` | Left |

### Parentheses

Use parentheses to override precedence:

```vibelang
a + b * c               // a + (b * c)
(a + b) * c             // (a + b) * c

a and b or c            // (a and b) or c
a and (b or c)          // a and (b or c)
```

## 7.18 Expression Examples

### Complex Expressions

```vibelang
// Conditional with method calls
let result = if data.is_empty() {
    default_value()
} else {
    data.first().unwrap().process()
}

// Chained operations
let total = items
    .filter(fn(i: &Item) -> bool { return i.active })
    .map(fn(i: &Item) -> i64 { return i.value })
    .fold(0i64, fn(a: i64, b: &i64) -> i64 { return a + *b })

// Match with computation
let category = match score / 10 {
    9 | 10 => Category.Excellent
    7 | 8 => Category.Good
    5 | 6 => Category.Average
    _ => Category.Poor
}
```
