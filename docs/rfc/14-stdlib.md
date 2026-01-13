# Section 14: Standard Library

The standard library provides essential types and functions built on top of Vibelang's primitives. All stdlib types are written in Vibelang itself (once self-hosting is achieved).

## 14.1 Core Types

### Array<T>

Dynamic, growable array:

```vibelang
struct Array<T> {
    ptr: *T
    len: u64
    cap: u64
}

impl<T> Array<T> {
    /// Create empty array
    fn new() -> Array<T>
    
    /// Create with initial capacity
    fn with_capacity(cap: u64) -> Array<T>
    
    /// Number of elements
    fn len(self: &Array<T>) -> u64
    
    /// Current capacity
    fn capacity(self: &Array<T>) -> u64
    
    /// Is empty?
    fn is_empty(self: &Array<T>) -> bool
    
    /// Add element to end
    fn push(self: ~Array<T>, item: T)
    
    /// Remove and return last element
    fn pop(self: ~Array<T>) -> Option<T>
    
    /// Get element by index
    fn get(self: &Array<T>, index: u64) -> Option<&T>
    
    /// Get mutable element by index
    fn get_mut(self: ~Array<T>, index: u64) -> Option<~T>
    
    /// First element
    fn first(self: &Array<T>) -> Option<&T>
    
    /// Last element
    fn last(self: &Array<T>) -> Option<&T>
    
    /// Clear all elements
    fn clear(self: ~Array<T>)
    
    /// Insert at index
    fn insert(self: ~Array<T>, index: u64, item: T)
    
    /// Remove at index
    fn remove(self: ~Array<T>, index: u64) -> T
    
    /// Borrow as slice
    fn as_slice(self: &Array<T>) -> Slice<T>
    
    /// Iterate
    fn iter(self: &Array<T>) -> ArrayIter<T>
}
```

#### Array Usage

```vibelang
let numbers = Array<i32>()
numbers.push(1)
numbers.push(2)
numbers.push(3)

for n in &numbers {
    println("${n}")
}

const len = numbers.len()           // 3
const last = numbers.pop()          // Some(3)
```

### Slice<T>

Borrowed view into contiguous data:

```vibelang
struct Slice<T> {
    ptr: *T
    len: u64
}

impl<T> Slice<T> {
    /// Create empty slice
    fn empty() -> Slice<T>
    
    /// Number of elements
    fn len(self: &Slice<T>) -> u64
    
    /// Is empty?
    fn is_empty(self: &Slice<T>) -> bool
    
    /// Get element by index
    fn get(self: &Slice<T>, index: u64) -> Option<&T>
    
    /// First element
    fn first(self: &Slice<T>) -> Option<&T>
    
    /// Last element
    fn last(self: &Slice<T>) -> Option<&T>
    
    /// Sub-slice
    fn slice(self: &Slice<T>, start: u64, end: u64) -> Slice<T>
    
    /// Split at index
    fn split_at(self: &Slice<T>, index: u64) -> (Slice<T>, Slice<T>)
    
    /// Iterate
    fn iter(self: &Slice<T>) -> SliceIter<T>
    
    /// Copy to owned Array
    fn to_array(self: &Slice<T>) -> Array<T>
}
```

#### Slice Usage

```vibelang
let arr: i32[5] = [1, 2, 3, 4, 5]
let slice: Slice<i32> = &arr[1..4]      // [2, 3, 4]

for x in slice {
    println("${x}")
}

const first = slice.first()              // Some(&2)
const sub = slice.slice(0, 2)            // [2, 3]
```

### String

UTF-8 encoded, growable string:

```vibelang
struct String {
    bytes: Array<u8>
}

impl String {
    /// Create empty string
    fn new() -> String
    
    /// Create from string literal (copies)
    fn from(s: Slice<u8>) -> String
    
    /// Create from slice (copies)
    fn from_slice(s: Slice<u8>) -> String
    
    /// Length in bytes
    fn len(self: &String) -> u64
    
    /// Length in characters (UTF-8 aware)
    fn char_len(self: &String) -> u64
    
    /// Is empty?
    fn is_empty(self: &String) -> bool
    
    /// Get byte at index
    fn byte_at(self: &String, index: u64) -> u8
    
    /// Append string
    fn push(self: ~String, s: Slice<u8>)
    
    /// Append character
    fn push_char(self: ~String, c: u32)
    
    /// Clear contents
    fn clear(self: ~String)
    
    /// Borrow as byte slice
    fn as_bytes(self: &String) -> Slice<u8>
    
    /// Substring (by byte index)
    fn slice(self: &String, start: u64, end: u64) -> Slice<u8>
    
    /// Split by delimiter
    fn split(self: &String, delim: Slice<u8>) -> Array<Slice<u8>>
    
    /// Trim whitespace
    fn trim(self: &String) -> Slice<u8>
    
    /// Check if starts with prefix
    fn starts_with(self: &String, prefix: Slice<u8>) -> bool
    
    /// Check if ends with suffix
    fn ends_with(self: &String, suffix: Slice<u8>) -> bool
    
    /// Find substring
    fn find(self: &String, needle: Slice<u8>) -> Option<u64>
    
    /// Replace all occurrences (returns new string)
    fn replace(self: &String, from: Slice<u8>, to: Slice<u8>) -> String
    
    /// Convert to uppercase (returns new string)
    fn to_upper(self: &String) -> String
    
    /// Convert to lowercase (returns new string)
    fn to_lower(self: &String) -> String
    
    /// Copy
    fn copy(self: &String) -> String
}
```

#### String Usage

```vibelang
let greeting = String.from("Hello, ")
greeting.push("World!")

const len = greeting.len()              // 13
const upper = greeting.to_upper()       // "HELLO, WORLD!"

let parts = greeting.split(", ")        // ["Hello", "World!"]
```

### Option<T>

Represents an optional value:

```vibelang
enum Option<T> {
    Some(T)
    None
}

impl<T> Option<T> {
    /// Is Some?
    fn is_some(self: &Option<T>) -> bool
    
    /// Is None?
    fn is_none(self: &Option<T>) -> bool
    
    /// Unwrap or panic
    fn unwrap(self: Option<T>) -> T
    
    /// Unwrap or return default
    fn unwrap_or(self: Option<T>, default: T) -> T
    
    /// Unwrap or compute default
    fn unwrap_or_else(self: Option<T>, f: fn() -> T) -> T
    
    /// Map inner value
    fn map<U>(self: Option<T>, f: fn(T) -> U) -> Option<U>
    
    /// Flat map
    fn and_then<U>(self: Option<T>, f: fn(T) -> Option<U>) -> Option<U>
    
    /// Return None if predicate fails
    fn filter(self: Option<T>, pred: fn(&T) -> bool) -> Option<T>
    
    /// Convert to Result
    fn ok_or<E>(self: Option<T>, err: E) -> Result<T, E>
}
```

#### Option Usage

```vibelang
fn find_user(id: u64) -> Option<User> {
    // ...
}

let user = find_user(123)

match user {
    Option.Some(u) => print(&u.name)
    Option.None => print("Not found")
}

// Or with combinators
let name = find_user(123)
    .map(fn(u: User) -> String { return u.name })
    .unwrap_or(String.from("Anonymous"))
```

### Result<T, E>

Represents success or error:

```vibelang
enum Result<T, E> {
    Ok(T)
    Err(E)
}

impl<T, E> Result<T, E> {
    /// Is Ok?
    fn is_ok(self: &Result<T, E>) -> bool
    
    /// Is Err?
    fn is_err(self: &Result<T, E>) -> bool
    
    /// Unwrap or panic
    fn unwrap(self: Result<T, E>) -> T
    
    /// Unwrap error or panic
    fn unwrap_err(self: Result<T, E>) -> E
    
    /// Unwrap or return default
    fn unwrap_or(self: Result<T, E>, default: T) -> T
    
    /// Map success value
    fn map<U>(self: Result<T, E>, f: fn(T) -> U) -> Result<U, E>
    
    /// Map error value
    fn map_err<F>(self: Result<T, E>, f: fn(E) -> F) -> Result<T, F>
    
    /// Flat map
    fn and_then<U>(self: Result<T, E>, f: fn(T) -> Result<U, E>) -> Result<U, E>
    
    /// Convert to Option
    fn ok(self: Result<T, E>) -> Option<T>
    
    /// Convert error to Option
    fn err(self: Result<T, E>) -> Option<E>
}
```

#### Result Usage

```vibelang
fn read_file(path: Slice<u8>) -> Result<String, IoError> {
    // ...
}

let contents = read_file("data.txt")

match contents {
    Result.Ok(data) => process(&data)
    Result.Err(e) => print_error(&e)
}

// Error propagation with ?
fn load_config() -> Result<Config, Error> {
    let text = read_file("config.json")?    // returns early if Err
    let config = parse_config(&text)?       // returns early if Err
    return Result.Ok(config)
}
```

### Map<K, V>

Hash map:

```vibelang
struct Map<K, V> {
    buckets: Array<Bucket<K, V>>
    len: u64
}

impl<K, V> Map<K, V> {
    /// Create empty map
    fn new() -> Map<K, V>
    
    /// Number of entries
    fn len(self: &Map<K, V>) -> u64
    
    /// Is empty?
    fn is_empty(self: &Map<K, V>) -> bool
    
    /// Insert or update
    fn insert(self: ~Map<K, V>, key: K, value: V) -> Option<V>
    
    /// Get value by key
    fn get(self: &Map<K, V>, key: &K) -> Option<&V>
    
    /// Get mutable value by key
    fn get_mut(self: ~Map<K, V>, key: &K) -> Option<~V>
    
    /// Remove by key
    fn remove(self: ~Map<K, V>, key: &K) -> Option<V>
    
    /// Check if key exists
    fn contains(self: &Map<K, V>, key: &K) -> bool
    
    /// Clear all entries
    fn clear(self: ~Map<K, V>)
    
    /// Iterate over keys
    fn keys(self: &Map<K, V>) -> MapKeys<K, V>
    
    /// Iterate over values
    fn values(self: &Map<K, V>) -> MapValues<K, V>
    
    /// Iterate over entries
    fn iter(self: &Map<K, V>) -> MapIter<K, V>
}
```

#### Map Usage

```vibelang
let scores = Map<String, i32>()
scores.insert(String.from("Alice"), 100)
scores.insert(String.from("Bob"), 85)

match scores.get(&String.from("Alice")) {
    Option.Some(score) => println("Score: ${*score}")
    Option.None => println("Not found")
}

for (name, score) in &scores {
    println("${name}: ${score}")
}
```

## 14.2 I/O Types

### File

File handle for reading and writing:

```vibelang
struct File {
    handle: i32
}

impl File {
    /// Open for reading
    fn open(path: Slice<u8>) -> Result<File, IoError>
    
    /// Create or truncate for writing
    fn create(path: Slice<u8>) -> Result<File, IoError>
    
    /// Open for appending
    fn append(path: Slice<u8>) -> Result<File, IoError>
    
    /// Read all contents
    fn read_all(self: &File) -> Result<String, IoError>
    
    /// Read into buffer
    fn read(self: &File, buf: &Array<u8>) -> Result<u64, IoError>
    
    /// Write buffer
    fn write(self: &File, data: Slice<u8>) -> Result<u64, IoError>
    
    /// Write all data
    fn write_all(self: &File, data: Slice<u8>) -> Result<void, IoError>
    
    /// Flush to disk
    fn flush(self: &File) -> Result<void, IoError>
    
    /// Close file (automatic on drop)
    fn close(self: File) -> Result<void, IoError>
}

/// Convenience functions
fn read_file(path: Slice<u8>) -> Result<String, IoError>
fn write_file(path: Slice<u8>, data: Slice<u8>) -> Result<void, IoError>
```

#### File Usage

```vibelang
// Read entire file
let contents = read_file("input.txt")?

// Write file
write_file("output.txt", &contents)?

// Manual file handling
let file = File.open("data.txt")?
defer file.close()

let data = file.read_all()?
```

## 14.3 Formatting and String Interpolation

### String Interpolation

Vibelang supports string interpolation using `${expr}` syntax inside string literals. This allows embedding expressions directly in strings:

```vibelang
let name = "Alice"
let age = 30

println("Hello, ${name}!")              // Hello, Alice!
println("You are ${age} years old")     // You are 30 years old
println("Next year: ${age + 1}")        // Next year: 31
```

**Supported Types in Interpolation:**
- Integers (i8, i16, i32, i64, u8, u16, u32, u64) → decimal string
- Booleans → "true" or "false"
- Floats (f32, f64) → decimal string
- Strings (Slice<u8>) → themselves

**Escaping:** Use `$$` to include a literal `$` character:
```vibelang
println("Cost: $$50")  // Output: Cost: $50
```

### Print Functions

```vibelang
/// Print string to stdout (no newline)
fn print(s: Slice<u8>)

/// Print string to stdout (with newline)
fn println(s: Slice<u8>)
```

**Type Safety:** Print functions only accept string types (`Slice<u8>`). To print other types, use string interpolation:
```vibelang
let x = 42
println("${x}")        // Prints: 42
println("x = ${x}")    // Prints: x = 42
```

### Format Strings (Future)

```vibelang
/// Format with arguments
fn format(fmt: Slice<u8>, args: ...) -> String

/// Print formatted
fn printf(fmt: Slice<u8>, args: ...)
```

#### Format Usage

```vibelang
let name = "Alice"
let age = 30

// Using string interpolation (preferred)
println("Hello, ${name}! You are ${age} years old.")

// Using format function (future)
let msg = format("Hello, {}! You are {} years old.", name, age)
println(&msg)
```

## 14.4 Memory Functions

### Allocation

```vibelang
/// Allocate bytes (wrapper around C malloc)
fn malloc(size: i64) -> *u8

/// Allocate bytes (alias for malloc)
fn alloc(size: u64) -> *u8

/// Allocate and zero
fn alloc_zeroed(size: u64) -> *u8

/// Reallocate
fn realloc(ptr: *u8, new_size: i64) -> *u8

/// Free memory
fn free(ptr: *u8)

/// Copy memory
fn memcpy(dst: *u8, src: *u8, len: i64) -> *u8

/// Set memory
fn memset(ptr: *u8, value: u8, len: u64)

/// Compare memory
fn memcmp(a: *u8, b: *u8, len: u64) -> i32
```

**Type Safety:** Memory functions enforce strict type checking:
- `malloc` requires an integer size argument
- `realloc` requires a pointer and integer size
- `free` requires a pointer argument
- `memcpy` requires two pointers and an integer size

### Low-Level Pointer Intrinsics

For implementing data structures, these intrinsics provide direct memory access:

```vibelang
/// Write i64 value at ptr + (index * 8)
fn ptr_write_i64(ptr: *u8, index: i64, value: i64)

/// Read i64 value from ptr + (index * 8)
fn ptr_read_i64(ptr: *u8, index: i64) -> i64
```

**Type Safety:** Pointer intrinsics enforce strict type checking:
- First argument must be a pointer (or integer for int-to-ptr conversion)
- Index must be an integer
- Value (for write) must be an integer

## 14.5 Math Functions

```vibelang
/// Absolute value
fn abs(x: i64) -> i64
fn fabs(x: f64) -> f64

/// Min/Max
fn min<T>(a: T, b: T) -> T
fn max<T>(a: T, b: T) -> T

/// Clamp
fn clamp<T>(x: T, lo: T, hi: T) -> T

/// Power
fn pow(base: f64, exp: f64) -> f64
fn powi(base: f64, exp: i32) -> f64

/// Square root
fn sqrt(x: f64) -> f64

/// Trigonometry
fn sin(x: f64) -> f64
fn cos(x: f64) -> f64
fn tan(x: f64) -> f64
fn asin(x: f64) -> f64
fn acos(x: f64) -> f64
fn atan(x: f64) -> f64
fn atan2(y: f64, x: f64) -> f64

/// Exponential/Logarithm
fn exp(x: f64) -> f64
fn ln(x: f64) -> f64
fn log10(x: f64) -> f64
fn log2(x: f64) -> f64

/// Rounding
fn floor(x: f64) -> f64
fn ceil(x: f64) -> f64
fn round(x: f64) -> f64
fn trunc(x: f64) -> f64
```

## 14.6 Conversion Functions

```vibelang
/// Parse integer from string
fn parse_int(s: Slice<u8>) -> Result<i64, ParseError>
fn parse_uint(s: Slice<u8>) -> Result<u64, ParseError>

/// Parse float from string
fn parse_float(s: Slice<u8>) -> Result<f64, ParseError>

/// Integer to string
fn int_to_string(n: i64) -> String
fn uint_to_string(n: u64) -> String

/// Float to string
fn float_to_string(f: f64) -> String
fn float_to_string_precision(f: f64, precision: u32) -> String
```

## 14.7 Assertions and Panics

```vibelang
/// Panic with message
fn panic(msg: Slice<u8>) -> !

/// Assert condition
fn assert(cond: bool)
fn assert_msg(cond: bool, msg: Slice<u8>)

/// Debug assert (only in debug builds)
fn debug_assert(cond: bool)

/// Unreachable code marker
fn unreachable() -> !
```

#### Assert Usage

```vibelang
fn divide(a: i64, b: i64) -> i64 {
    assert_msg(b != 0, "division by zero")
    return a / b
}

fn process(kind: TokenKind) {
    match kind {
        TokenKind.Plus => handle_plus()
        TokenKind.Minus => handle_minus()
        _ => unreachable()
    }
}
```
