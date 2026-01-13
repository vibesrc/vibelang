# Section 6: Ownership and Memory

Vibelang uses an ownership system to manage memory without garbage collection. Every value has exactly one owner, and memory is freed when the owner goes out of scope.

## 6.1 Ownership Principles

### The Three Rules

1. Every value has exactly one owner
2. When the owner goes out of scope, the value is dropped (freed)
3. Ownership can be transferred (moved) or temporarily lent (borrowed)

### Value Categories

| Category | Description | Example |
|----------|-------------|---------|
| Copy | Implicitly duplicated on assignment | `i32`, `f64`, `bool` |
| Owned | Moved on assignment, single owner | `String`, `Array<T>` |
| Borrowed | Temporary access, no ownership | `&String`, `Slice<T>` |

## 6.2 Copy Types

Copy types are implicitly duplicated. Both source and destination remain valid:

```vibelang
let a: i32 = 42
let b = a                       // a is COPIED to b
print(a)                        // OK: a is still valid
print(b)                        // OK: b has its own copy
```

All primitives are Copy:
- Integers: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- Floats: `f32`, `f64`
- Boolean: `bool`
- Raw pointers: `*T`
- Fixed arrays of Copy types: `T[N]`

## 6.3 Owned Types

Owned types have single ownership. Assignment moves the value:

```vibelang
let a = String.from("hello")
let b = a                       // a is MOVED to b

print(&a)                       // ERROR: a was moved
print(&b)                       // OK: b owns the string
```

### Move Semantics

When a value is moved:
1. The destination takes ownership
2. The source becomes invalid
3. No memory is copied (just pointer/metadata)
4. No memory is freed yet

```vibelang
fn example() {
    let s1 = String.from("hello")   // s1 owns heap data
    let s2 = s1                      // ownership moves to s2
    // s1 is now invalid
    // heap data is NOT copied, just the pointer
}   // s2 is freed here
```

### Move in Function Calls

Passing an owned value to a function moves it:

```vibelang
fn consume(s: String) {
    print(&s)
}   // s freed here

fn main() {
    let msg = String.from("hello")
    consume(msg)                    // msg moved into consume
    print(&msg)                     // ERROR: msg was moved
}
```

### Move in Returns

Returning a value transfers ownership to the caller:

```vibelang
fn create() -> String {
    let s = String.from("hello")
    return s                        // ownership transfers to caller
}   // s NOT freed (moved out)

fn main() {
    let msg = create()              // main owns the string
    print(&msg)
}   // msg freed here
```

## 6.4 Borrowing

Borrowing provides temporary access without taking ownership. There are two kinds:

- `&T` — read-only borrow (like `const`)
- `~T` — mutable borrow (like `let`)

### Read-Only Borrow

```vibelang
fn print_length(s: &String) {       // borrows s (read-only)
    print(s.len())
    s.push("!")                     // ERROR: cannot mutate through &
}

fn main() {
    let msg = String.from("hello")
    print_length(&msg)              // lend msg (read-only)
    print_length(&msg)              // can lend again
}
```

### Mutable Borrow

```vibelang
fn add_exclaim(s: ~String) {        // borrows s (mutable)
    s.push("!")                     // OK: can mutate through ~
}

fn main() {
    let msg = String.from("hello")
    add_exclaim(~msg)               // lend msg (mutable)
    print(&msg)                     // prints "hello!"
}
```

### Borrow Rules

1. Borrows cannot outlive the owner
2. Cannot move a value while it's borrowed
3. Can have many `&T` OR one `~T`, but not both at the same time

### Borrow Syntax

```vibelang
&x                                  // read-only borrow of x
~x                                  // mutable borrow of x
&arr[0..10]                         // read-only borrow of slice
~arr[0..10]                         // mutable borrow of slice
```

### Borrow Errors

```vibelang
fn bad_borrow() -> &String {
    let s = String.from("hello")
    return &s                       // ERROR: s dies, borrow would dangle
}

fn main() {
    let s = String.from("hello")
    let r = &s                      // borrow s
    let t = s                       // ERROR: can't move s while borrowed
    print(r)
}
```

## 6.5 Scope-Based Freeing

Values are freed at the end of their owner's scope:

```vibelang
fn example() {
    let a = String.from("a")        // a allocated
    
    {
        let b = String.from("b")    // b allocated
        print(&b)
    }   // b freed here
    
    let c = String.from("c")        // c allocated
    
}   // c freed, then a freed (reverse order)
```

### Freeing Order

Values are freed in reverse declaration order:

```vibelang
fn example() {
    let first = Resource.new("1")
    let second = Resource.new("2")
    let third = Resource.new("3")
}
// Freed: third, second, first
```

### Early Return

All live values are freed on any exit path:

```vibelang
fn example(condition: bool) -> String {
    let a = String.from("a")
    let b = String.from("b")
    
    if condition {
        return a                    // b freed here, a moved out
    }
    
    let c = String.from("c")
    return c                        // a and b freed here, c moved out
}
```

## 6.6 Partial Moves

Moving a field out of a struct partially invalidates the struct:

```vibelang
struct Pair {
    first: String
    second: String
}

fn example() {
    let p = Pair {
        first: String.from("hello"),
        second: String.from("world")
    }
    
    let f = p.first                 // move first out
    print(&p.first)                 // ERROR: p.first was moved
    print(&p.second)                // OK: p.second still valid
    print(&p)                       // ERROR: p is partially moved
}
```

## 6.7 Clone and Copy

### Explicit Clone

For owned types, use `.copy()` for explicit duplication:

```vibelang
let a = String.from("hello")
let b = a.copy()                    // explicit heap copy
print(&a)                           // OK: a still valid
print(&b)                           // OK: b is independent copy
```

### Implicit Copy

Copy types are automatically duplicated:

```vibelang
let a: i32 = 42
let b = a                           // implicit copy
// both valid
```

## 6.8 Defer

`defer` schedules cleanup to run at scope exit:

```vibelang
fn process() {
    let file = open("data.txt")
    defer close(file)               // will run at scope exit
    
    let buffer = alloc(1024)
    defer free(buffer)              // will run at scope exit
    
    // work with file and buffer...
    
    if error {
        return                      // defers still run!
    }
    
    // more work...
}   // close(file) and free(buffer) run here, in reverse order
```

### Defer Order

Defers execute in reverse order (LIFO):

```vibelang
fn example() {
    defer print("first")
    defer print("second")
    defer print("third")
}
// Output: third, second, first
```

### Defer with Values

Defer captures values at declaration time:

```vibelang
fn example() {
    let x = 1
    defer print(x)                  // captures x = 1
    x = 2
}
// Output: 1 (not 2)
```

## 6.9 Memory Layout

### Stack Allocation

Copy types and borrows live on the stack:

```vibelang
let x: i32 = 42                     // 4 bytes on stack
let y: f64 = 3.14                   // 8 bytes on stack
let arr: i32[10] = [0; 10]          // 40 bytes on stack
let slice: Slice<i32> = &arr        // 16 bytes on stack (ptr + len)
```

### Heap Allocation

Owned container types use heap storage:

```vibelang
let s = String.from("hello")
// Stack: String struct (24 bytes: ptr + len + cap)
// Heap: "hello" bytes (5 bytes + possible extra capacity)

let v = Array<i32>()
// Stack: Array struct (24 bytes: ptr + len + cap)
// Heap: nothing yet (empty)

v.push(1)
v.push(2)
// Heap: [1, 2] with some capacity
```

## 6.10 Compiler Optimizations

The compiler MAY optimize memory usage while preserving semantics:

### Copy Elision

```vibelang
// You write:
let s = get_string().copy()

// Compiler may elide if safe:
// - s doesn't escape
// - source outlives s
// → just borrow instead of copy
```

### Escape Analysis

```vibelang
// You write:
let arr = Array<i32>()
arr.push(1)
arr.push(2)
return arr.len()                    // arr doesn't escape

// Compiler may optimize:
// → stack allocate arr (no heap)
```

### Borrow Inference

```vibelang
// You write:
fn process(data: &String) {
    let slice = data.slice(0, 10).copy()
    // use slice...
}

// Compiler may infer:
// → slice can borrow instead of copy
// → data outlives slice
```

These optimizations are invisible to the programmer. The ownership semantics remain the same.

## 6.11 Function Signature Conventions

### Taking Ownership

```vibelang
fn consume(s: String)               // takes ownership, will free
fn sink(v: Array<i32>)              // takes ownership, will free
```

Use when:
- The function needs to store the value long-term
- The function will modify and return a new value
- The caller doesn't need the value anymore

### Borrowing (Read-Only)

```vibelang
fn read(s: &String)                 // borrows, read-only
fn inspect(v: &Array<i32>)          // borrows, read-only
```

Use when:
- The function only needs to read
- The caller needs the value after the call

### Borrowing (Mutable)

```vibelang
fn modify(s: ~String)               // borrows, can mutate
fn append(v: ~Array<i32>)           // borrows, can mutate
```

Use when:
- The function needs to mutate
- The caller needs the value after the call

### Returning Owned Values

```vibelang
fn create() -> String               // caller takes ownership
fn transform(s: String) -> String   // takes and returns ownership
```

### Returning Borrows

```vibelang
fn first(s: &String) -> Slice<u8>   // returns borrow into s
fn get(m: &Map<K, V>, k: &K) -> Option<&V>  // returns borrow into m
```

The returned borrow cannot outlive the input borrow.

## 6.12 Common Patterns

### Builder Pattern

```vibelang
fn build_config() -> Config {
    let config = Config.new()
    config.set_name("MyApp")
    config.set_port(8080)
    config.set_debug(true)
    return config                   // ownership transfers to caller
}
```

### Transform Pattern

```vibelang
fn uppercase(s: String) -> String {
    let result = String.new()
    for c in &s {
        result.push(to_upper(c))
    }
    return result
}   // s freed here, result moved out

// Usage
let name = String.from("alice")
let upper = uppercase(name)         // name moved, get back new string
```

### Borrow and Return Pattern

```vibelang
fn find_longest<'a>(a: &String, b: &String) -> Slice<u8> {
    if a.len() > b.len() {
        return &a
    }
    return &b
}

// Usage
let s1 = String.from("short")
let s2 = String.from("much longer")
let longest = find_longest(&s1, &s2)    // borrows, doesn't copy
```

### Split and Process Pattern

```vibelang
fn process_lines(text: &String) -> Array<Result> {
    let results = Array<Result>()
    
    for line in text.split("\n") {      // line is Slice<u8>, borrows text
        let result = process_line(line)  // process each borrowed slice
        results.push(result)
    }
    
    return results
}
```

### Resource Acquisition Is Initialization (RAII)

```vibelang
struct File {
    handle: i32
}

impl File {
    fn open(path: &Slice<u8>) -> Result<File, Error> {
        let handle = sys_open(path)
        if handle < 0 {
            return Result.Err(Error.from_errno())
        }
        return Result.Ok(File { handle: handle })
    }
    
    fn drop(self: File) {
        sys_close(self.handle)
    }
}

fn read_file(path: &Slice<u8>) -> Result<String, Error> {
    let file = File.open(path)?         // open file
    let contents = file.read_all()?     // use file
    return Result.Ok(contents)
}   // file.drop() called automatically
```

## 6.13 Error Messages

The compiler provides helpful error messages for ownership violations:

### Use After Move

```
error: use of moved value `name`
  --> src/main.vibe:5:11
   |
3  |     let other = name
   |                 ---- value moved here
4  |     
5  |     print(&name)
   |           ^^^^^ value used after move
   |
   = help: consider using `name.copy()` if you need both values
```

### Borrow Outlives Owner

```
error: borrowed value does not live long enough
  --> src/main.vibe:4:12
   |
3  |     let s = String.from("hello")
   |         - binding `s` declared here
4  |     return &s
   |            ^^ borrowed value escapes function
   |
   = help: consider returning an owned value instead
```

### Move While Borrowed

```
error: cannot move `data` while borrowed
  --> src/main.vibe:6:15
   |
4  |     let slice = &data[0..5]
   |                 ----------- borrow of `data` occurs here
5  |     
6  |     let moved = data
   |                 ^^^^ move occurs here
7  |     print(slice)
   |           ----- borrow later used here
```
