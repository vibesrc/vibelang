# Section 11: Pattern Matching

Pattern matching is a powerful feature for destructuring and conditional logic.

## 11.1 Match Expression

### Basic Syntax

```vibelang
match value {
    pattern1 => expression1
    pattern2 => expression2
    _ => default_expression
}
```

### Match is an Expression

Match returns a value:

```vibelang
let name = match direction {
    Direction.North => "up"
    Direction.South => "down"
    Direction.East => "right"
    Direction.West => "left"
}
```

### Match with Blocks

```vibelang
let result = match opt {
    Option.Some(x) => {
        let doubled = x * 2
        let incremented = doubled + 1
        incremented
    }
    Option.None => 0
}
```

## 11.2 Pattern Types

### Literal Patterns

```vibelang
match x {
    0 => "zero"
    1 => "one"
    2 => "two"
    _ => "other"
}

match c {
    'a' => "letter a"
    'b' => "letter b"
    _ => "other"
}

match flag {
    true => "yes"
    false => "no"
}
```

### Variable Patterns

Bind a value to a variable:

```vibelang
match opt {
    Option.Some(x) => use(x)     // x bound to inner value
    Option.None => default()
}
```

### Wildcard Pattern

Match anything, discard the value:

```vibelang
match value {
    ImportantCase => handle()
    _ => ignore()               // matches anything else
}
```

### Struct Patterns

```vibelang
match point {
    Point { x: 0.0, y: 0.0 } => "origin"
    Point { x: 0.0, y } => format("y-axis at {}", y)
    Point { x, y: 0.0 } => format("x-axis at {}", x)
    Point { x, y } => format("({}, {})", x, y)
}
```

### Enum Patterns

```vibelang
match token {
    Token.Ident(name) => handle_ident(name)
    Token.Number(n) => handle_number(n)
    Token.Plus => handle_plus()
    Token.Minus => handle_minus()
    _ => handle_other()
}
```

### Tuple Patterns

```vibelang
match pair {
    (0, 0) => "origin"
    (0, y) => format("y = {}", y)
    (x, 0) => format("x = {}", x)
    (x, y) => format("({}, {})", x, y)
}
```

### Array Patterns

```vibelang
match arr {
    [] => "empty"
    [x] => format("single: {}", x)
    [x, y] => format("pair: {}, {}", x, y)
    [x, y, z] => format("triple: {}, {}, {}", x, y, z)
    _ => "many elements"
}
```

### Slice Patterns

```vibelang
match slice {
    [] => "empty"
    [first, ..] => format("starts with {}", first)
    [.., last] => format("ends with {}", last)
    [first, .., last] => format("first: {}, last: {}", first, last)
}
```

## 11.3 Pattern Guards

Add conditions to patterns:

```vibelang
match x {
    n if n < 0 => "negative"
    n if n == 0 => "zero"
    n if n > 0 => "positive"
    _ => unreachable()
}

match point {
    Point { x, y } if x == y => "on diagonal"
    Point { x, y } if x > y => "below diagonal"
    Point { x, y } => "above diagonal"
}
```

### Guards with Bindings

```vibelang
match opt {
    Option.Some(x) if x > 100 => "large value"
    Option.Some(x) if x > 0 => "positive value"
    Option.Some(x) => "non-positive value"
    Option.None => "no value"
}
```

## 11.4 Or Patterns

Match multiple patterns:

```vibelang
match c {
    'a' | 'e' | 'i' | 'o' | 'u' => "vowel"
    'A' | 'E' | 'I' | 'O' | 'U' => "uppercase vowel"
    _ => "consonant or other"
}

match direction {
    Direction.North | Direction.South => "vertical"
    Direction.East | Direction.West => "horizontal"
}
```

### Or Patterns with Bindings

All alternatives must bind the same variables:

```vibelang
match result {
    Result.Ok(x) | Result.Err(x) => use(x)
    // x is bound in both cases
}
```

## 11.5 Range Patterns

```vibelang
match score {
    0..=59 => "F"
    60..=69 => "D"
    70..=79 => "C"
    80..=89 => "B"
    90..=100 => "A"
    _ => "invalid"
}

match c {
    'a'..='z' => "lowercase"
    'A'..='Z' => "uppercase"
    '0'..='9' => "digit"
    _ => "other"
}
```

## 11.6 Nested Patterns

```vibelang
match expr {
    Expr.Binary {
        op: BinOp.Add,
        left: Expr.Literal(0),
        right
    } => right.clone()          // 0 + x => x
    
    Expr.Binary {
        op: BinOp.Mul,
        left: Expr.Literal(1),
        right
    } => right.clone()          // 1 * x => x
    
    Expr.Binary {
        op: BinOp.Mul,
        left: Expr.Literal(0),
        ..
    } => Expr.Literal(0)        // 0 * x => 0
    
    other => other.clone()
}
```

## 11.7 Binding Modes

### By Value (Move)

```vibelang
match opt {
    Option.Some(x) => consume(x)    // x moved out of opt
    Option.None => {}
}
// opt partially moved
```

### By Reference

```vibelang
match &opt {
    Option.Some(x) => use(x)        // x is &T
    Option.None => {}
}
// opt still valid
```

### Explicit Reference

```vibelang
match opt {
    Option.Some(ref x) => use(x)    // x is &T even though opt not borrowed
    Option.None => {}
}
```

## 11.8 Exhaustiveness

Match must be exhaustive:

```vibelang
// ERROR: non-exhaustive match
match direction {
    Direction.North => "north"
    Direction.South => "south"
    // missing East and West!
}

// OK: exhaustive
match direction {
    Direction.North => "north"
    Direction.South => "south"
    Direction.East => "east"
    Direction.West => "west"
}

// OK: wildcard covers remaining
match direction {
    Direction.North => "north"
    _ => "other"
}
```

### Boolean Exhaustiveness

```vibelang
// Must cover both cases
match flag {
    true => "yes"
    false => "no"
}
```

### Integer Exhaustiveness

Use wildcard for integers:

```vibelang
match n {
    0 => "zero"
    1 => "one"
    _ => "other"    // required for i32
}
```

## 11.9 If Let

Shorthand for single-pattern match:

```vibelang
// Instead of:
match opt {
    Option.Some(x) => use(x)
    _ => {}
}

// Write:
if let Option.Some(x) = opt {
    use(x)
}

// With else:
if let Option.Some(x) = opt {
    use(x)
} else {
    handle_none()
}
```

## 11.10 While Let

Loop while pattern matches:

```vibelang
while let Option.Some(item) = iter.next() {
    process(item)
}

while let Result.Ok(line) = reader.read_line() {
    print(&line)
}
```

## 11.11 Let Else

Destructure or else diverge:

```vibelang
let Option.Some(value) = opt else {
    return default()
}
// value is now in scope

let Result.Ok(data) = fetch() else {
    panic("fetch failed")
}
// data is now in scope
```

## 11.12 Pattern Matching in Function Parameters

```vibelang
fn handle_point(Point { x, y }: Point) {
    // x and y directly available
}

fn process_pair((first, second): (i32, i32)) {
    // first and second directly available
}
```

## 11.13 @ Bindings

Bind a value while also testing its structure:

```vibelang
match msg {
    Message.Data { payload: p @ [_, _, ..] } => {
        // p is bound AND we know it has at least 2 elements
        process_with_payload(p)
    }
    _ => {}
}

match opt {
    some @ Option.Some(_) => use_some(some)
    Option.None => {}
}

match n {
    x @ 1..=100 => format("small number: {}", x)
    x @ 101..=1000 => format("medium number: {}", x)
    x => format("large number: {}", x)
}
```

## 11.14 Common Patterns

### Option Handling

```vibelang
fn process(opt: Option<Data>) -> Result<Output, Error> {
    match opt {
        Option.Some(data) => transform(data)
        Option.None => Result.Err(Error.NotFound)
    }
}

// Or with if let
fn process(opt: Option<Data>) -> Result<Output, Error> {
    if let Option.Some(data) = opt {
        return transform(data)
    }
    return Result.Err(Error.NotFound)
}
```

### Result Handling

```vibelang
fn process(result: Result<Data, E>) -> Output {
    match result {
        Result.Ok(data) => transform(data)
        Result.Err(e) => {
            log_error(&e)
            default_output()
        }
    }
}
```

### State Machines

```vibelang
enum State {
    Idle
    Running { progress: u32 }
    Paused { progress: u32 }
    Completed { result: Output }
    Failed { error: Error }
}

fn transition(state: State, event: Event) -> State {
    match (state, event) {
        (State.Idle, Event.Start) => State.Running { progress: 0 }
        (State.Running { progress }, Event.Pause) => State.Paused { progress }
        (State.Paused { progress }, Event.Resume) => State.Running { progress }
        (State.Running { progress }, Event.Progress(n)) => {
            if progress + n >= 100 {
                State.Completed { result: compute_result() }
            } else {
                State.Running { progress: progress + n }
            }
        }
        (State.Running { .. }, Event.Error(e)) => State.Failed { error: e }
        (state, _) => state     // ignore invalid transitions
    }
}
```

### Command Dispatch

```vibelang
fn dispatch(cmd: Command) {
    match cmd {
        Command.Help => show_help()
        Command.Version => show_version()
        Command.Run { file, args } => run_file(&file, &args)
        Command.Build { target, release } => build(target, release)
        Command.Test { filter: Option.Some(f) } => run_tests_filtered(&f)
        Command.Test { filter: Option.None } => run_all_tests()
    }
}
```

## 11.15 Pattern Match Compilation

The compiler transforms pattern matching into efficient code:

```vibelang
match opt {
    Option.Some(x) if x > 0 => positive(x)
    Option.Some(x) => non_positive(x)
    Option.None => none()
}
```

Compiles to something like:

```vibelang
// Pseudocode of compiled form
let tag = opt.tag
if tag == SOME_TAG {
    let x = opt.payload
    if x > 0 {
        positive(x)
    } else {
        non_positive(x)
    }
} else {
    none()
}
```

The compiler:
- Checks exhaustiveness at compile time
- Optimizes redundant checks
- Orders tests for efficiency
- Warns about unreachable patterns
