# Section 8: Statements

Statements perform actions but do not produce values. Vibelang uses statements for declarations, control flow, and side effects.

## 8.1 Declaration Statements

### Variable Declarations

```vibelang
let x = 42
let name = String.from("Alice")
let arr: i32[10] = [0; 10]
```

### Constant Declarations

```vibelang
const MAX = 100
const len = data.len()
```

See [Section 5: Declarations](./05-declarations.md) for full details.

## 8.2 Expression Statements

Any expression followed by a semicolon (or at statement position) is a statement:

```vibelang
print("hello");
x + 1;                  // computed but discarded
do_something();
```

### Implicit Semicolons

Vibelang uses significant newlines. Semicolons are optional when the newline is unambiguous:

```vibelang
// These are equivalent:
print("hello");
print("hello")

// Multiline expressions use continuation:
let result = long_function_name(
    arg1,
    arg2,
    arg3
)
```

## 8.3 Return Statement

### Basic Return

```vibelang
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

### Return Without Value

```vibelang
fn print_hello() {
    print("hello")
    return              // explicit return void
}

fn print_hello() {
    print("hello")
    // implicit return at end
}
```

### Early Return

```vibelang
fn find(arr: &Array<i32>, target: i32) -> Option<u64> {
    let i = 0
    while i < arr.len() {
        if arr[i] == target {
            return Option.Some(i)   // early return
        }
        i += 1
    }
    return Option.None
}
```

## 8.4 If Statement

### Basic If

```vibelang
if condition {
    do_something()
}
```

### If-Else

```vibelang
if condition {
    do_this()
} else {
    do_that()
}
```

### If-Else-If Chain

```vibelang
if x < 0 {
    print("negative")
} else if x == 0 {
    print("zero")
} else {
    print("positive")
}
```

### If as Expression vs Statement

When used as an expression, all branches must return values:

```vibelang
// Statement (no value needed)
if condition {
    print("yes")
}

// Expression (value returned)
let msg = if condition { "yes" } else { "no" }
```

## 8.5 While Statement

### Basic While

```vibelang
let i = 0
while i < 10 {
    print_int(i)
    i += 1
}
```

### While with Break

```vibelang
let i = 0
while true {
    if i >= 10 {
        break
    }
    print_int(i)
    i += 1
}
```

### While with Continue

```vibelang
let i = 0
while i < 10 {
    i += 1
    if i % 2 == 0 {
        continue        // skip even numbers
    }
    print_int(i)
}
```

## 8.6 For Statement

### For-In Loop

```vibelang
for item in collection {
    process(item)
}
```

### Iterating Arrays

```vibelang
let arr = [1, 2, 3, 4, 5]

// By reference (borrowing)
for x in &arr {
    print_int(*x)
}

// By value (copies for Copy types, moves for owned types)
for x in arr {
    print_int(x)
}
```

### Iterating Ranges

```vibelang
// 0 to 9
for i in 0..10 {
    print_int(i)
}

// 1 to 10 inclusive
for i in 1..=10 {
    print_int(i)
}

// Reverse
for i in (0..10).rev() {
    print_int(i)
}
```

### Iterating with Index

```vibelang
for (i, item) in arr.enumerate() {
    print_int(i)
    print(": ")
    print(item)
}
```

### For with Break and Continue

```vibelang
for item in collection {
    if should_skip(item) {
        continue
    }
    if should_stop(item) {
        break
    }
    process(item)
}
```

## 8.7 Match Statement

### Basic Match

```vibelang
match value {
    0 => print("zero")
    1 => print("one")
    _ => print("other")
}
```

### Match with Blocks

```vibelang
match command {
    Command.Quit => {
        save_state()
        cleanup()
    }
    Command.Run(name) => {
        let result = execute(&name)
        print_result(&result)
    }
    _ => print("unknown command")
}
```

See [Section 11: Pattern Matching](./11-pattern-matching.md) for full details.

## 8.8 Break Statement

### Breaking Loops

```vibelang
while true {
    if done {
        break
    }
}
```

### Breaking Nested Loops (Labels)

```vibelang
'outer: while true {
    while true {
        if condition {
            break 'outer    // breaks outer loop
        }
    }
}
```

### Break with Value (Not Supported)

Unlike Rust, `break` cannot return a value. Use a variable instead:

```vibelang
let result = 0
while true {
    if found {
        result = value
        break
    }
}
```

## 8.9 Continue Statement

### Skipping Iterations

```vibelang
for i in 0..100 {
    if i % 2 == 0 {
        continue        // skip even numbers
    }
    process(i)
}
```

### Continue with Labels

```vibelang
'outer: for i in 0..10 {
    for j in 0..10 {
        if should_skip_outer(i, j) {
            continue 'outer
        }
    }
}
```

## 8.10 Defer Statement

### Basic Defer

```vibelang
fn process() {
    let file = open("data.txt")
    defer close(file)
    
    // ... use file ...
    
}   // close(file) called here
```

### Multiple Defers

Defers execute in reverse order (LIFO):

```vibelang
fn example() {
    defer print("1")
    defer print("2")
    defer print("3")
}
// Output: 3, 2, 1
```

### Defer with Early Return

Defers run on any exit path:

```vibelang
fn process() -> Result<Data, Error> {
    let resource = acquire()
    defer release(resource)
    
    if error_condition {
        return Result.Err(Error.new())  // defer still runs
    }
    
    return Result.Ok(data)              // defer still runs
}
```

### Defer Captures

Defer captures values at declaration time:

```vibelang
fn example() {
    let x = 1
    defer print_int(x)  // captures x = 1
    x = 2
}
// Output: 1
```

## 8.11 Block Statement

### Nested Blocks

```vibelang
{
    let x = 1
    {
        let y = 2
        print_int(x + y)
    }
    // y out of scope here
}
// x out of scope here
```

### Blocks for Scoping

```vibelang
let result = {
    let temp = compute()
    let processed = transform(temp)
    validate(processed)
}   // temp and processed dropped here
```

## 8.12 Empty Statement

An empty statement does nothing:

```vibelang
;                       // empty statement (rarely used)
```

## 8.13 Statement Examples

### Complete Function

```vibelang
fn process_items(items: &Array<Item>) -> Summary {
    let processed = 0
    let failed = 0
    let results = Array<r>()
    
    for item in items {
        if not item.is_valid() {
            failed += 1
            continue
        }
        
        match item.process() {
            Result.Ok(r) => {
                results.push(r)
                processed += 1
            }
            Result.Err(e) => {
                log_error(&e)
                failed += 1
            }
        }
        
        if processed >= MAX_ITEMS {
            break
        }
    }
    
    return Summary {
        processed: processed,
        failed: failed,
        results: results
    }
}
```

### Resource Management Pattern

```vibelang
fn with_transaction(db: &Database) -> Result<void, Error> {
    let tx = db.begin_transaction()?
    defer {
        if tx.is_active() {
            tx.rollback()
        }
    }
    
    // Perform operations
    tx.execute("INSERT INTO ...")?
    tx.execute("UPDATE ...")?
    
    // Commit on success
    tx.commit()?
    
    return Result.Ok(void)
}
```

### Loop with Complex Control Flow

```vibelang
fn find_match(data: &Array<Array<Item>>) -> Option<(u64, u64)> {
    'outer: for (i, row) in data.enumerate() {
        for (j, item) in row.enumerate() {
            if item.is_empty() {
                continue
            }
            
            if item.matches_criteria() {
                return Option.Some((i, j))
            }
            
            if item.is_terminal() {
                break 'outer
            }
        }
    }
    
    return Option.None
}
```
