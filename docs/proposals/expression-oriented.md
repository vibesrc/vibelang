# Proposal: Expression-Oriented Semantics

## Summary

Allow `match`, `if/else`, and blocks to be used as expressions that produce values, enabling more concise and functional programming patterns.

## Motivation

Currently, extracting values from control flow requires explicit mutable variables:

```vibe
// Current: verbose, requires pre-declaration
let result: str
match scores.get(&"bob") {
    Option.Some(val) => result = "Score for bob: ${val}"
    Option.None => result = "bob not found"
}

// Current: verbose if/else
let status: str
if count > 0 {
    status = "has items"
} else {
    status = "empty"
}
```

With expression-oriented semantics:

```vibe
// Proposed: clean, functional
let result = match scores.get(&"bob") {
    Option.Some(val) => "Score for bob: ${val}"
    Option.None => "bob not found"
}

// Proposed: concise conditional
let status = if count > 0 { "has items" } else { "empty" }
```

## Design

### Challenge: No Semicolons

Vibelang uses newlines as statement terminators, not semicolons. Languages like Rust use "no trailing semicolon" to indicate an expression returns a value:

```rust
// Rust: semicolon = statement, no semicolon = expression
let x = {
    let temp = compute();
    temp + 1  // no semicolon - this is the return value
};
```

We need a different approach.

### Option A: Implicit Last Expression

The last expression in a block is implicitly its value:

```vibe
let x = {
    let temp = compute()
    temp + 1              // last expression = block value
}

let result = match opt {
    Option.Some(v) => v * 2    // each arm's last expr is its value
    Option.None => 0
}
```

**Pros:**
- Clean syntax, minimal noise
- Familiar to Rust/Kotlin/Scala users
- Works naturally with existing newline-terminated statements

**Cons:**
- Ambiguity: is `println("hi")` a returned value or side effect?
- Every expression would need to consider "am I the last one?"

**Resolution for ambiguity:** In expression context (assignment, return, function arg), the last expression is the value. Void-returning expressions like `println` would cause a type error if assigned to a non-void variable.

### Option B: Explicit `yield` Keyword

Require explicit `yield` to return a value from a block:

```vibe
let x = {
    let temp = compute()
    yield temp + 1
}

let result = match opt {
    Option.Some(v) => yield v * 2
    Option.None => yield 0
}
```

**Pros:**
- Unambiguous - clear what returns a value
- Easy to implement

**Cons:**
- Verbose, especially for simple cases
- `yield` typically implies generators/coroutines in other languages

### Option C: Arrow Syntax for Expression Arms

Use `=>` consistently to indicate "this produces a value":

```vibe
// Match arms already use =>
let result = match opt {
    Option.Some(v) => v * 2
    Option.None => 0
}

// If/else with => for expression form
let status = if count > 0 => "has items" else => "empty"

// Or single-line form
let status = if count > 0 then "has items" else "empty"
```

**Pros:**
- Consistent with existing match arm syntax
- Clear visual distinction between statement and expression forms

**Cons:**
- Two syntaxes for if/else (block vs expression)
- `then` keyword adds complexity

### Recommendation: Option A with Contextual Interpretation

Use implicit last-expression semantics with these rules:

1. **Expression context:** When a `match`, `if/else`, or block appears where a value is expected (assignment RHS, function argument, return value), the last expression of each branch is the produced value.

2. **Statement context:** When appearing as a standalone statement, no value is produced.

3. **Type consistency:** All branches must produce the same type (or be exhaustively void).

```vibe
// Expression context - value expected
let x = match opt {
    Option.Some(v) => v * 2    // i32
    Option.None => 0           // i32
}  // x: i32

// Statement context - no value expected
match opt {
    Option.Some(v) => println("Got ${v}")
    Option.None => println("Nothing")
}  // no assignment, branches are void

// Type error - branches don't match
let x = match opt {
    Option.Some(v) => v        // i32
    Option.None => "missing"   // str - ERROR!
}
```

## Detailed Design

### Match Expressions

```vibe
// Simple value extraction
let name = match user {
    User.Admin(info) => info.name
    User.Guest => "anonymous"
}

// Nested in function calls
println(match status {
    Status.Ok => "success"
    Status.Err(e) => "error: ${e}"
})

// With complex expressions in arms
let score = match difficulty {
    Difficulty.Easy => base_score * 1
    Difficulty.Medium => base_score * 2
    Difficulty.Hard => {
        let bonus = calculate_bonus()
        base_score * 3 + bonus
    }
}
```

### If/Else Expressions

```vibe
// Simple ternary-style
let max = if a > b { a } else { b }

// Multi-line branches
let result = if validate(input) {
    let processed = transform(input)
    process(processed)
} else {
    default_value()
}

// Chained if/else
let grade = if score >= 90 { "A" }
    else if score >= 80 { "B" }
    else if score >= 70 { "C" }
    else { "F" }
```

### Block Expressions

```vibe
// Inline computation
let hypotenuse = {
    let a_sq = a * a
    let b_sq = b * b
    sqrt(a_sq + b_sq)
}

// Complex initialization
let config = {
    let mut c = Config.default()
    c.timeout = 30
    c.retries = 3
    c  // return the configured value
}
```

### Void Branches

When all branches are void (no meaningful return), the construct is a statement:

```vibe
// All branches are void - this is fine as a statement
match event {
    Event.Click(pos) => handle_click(pos)
    Event.Key(k) => handle_key(k)
    Event.Quit => shutdown()
}

// ERROR: mixing void and non-void
let x = match event {
    Event.Click(pos) => pos.x      // i32
    Event.Key(k) => handle_key(k)  // void - TYPE ERROR
}
```

## Implementation

### Parser Changes

1. Allow `match` and `if/else` in expression position
2. Track whether we're in expression or statement context
3. Parse block expressions `{ ... }` that can appear in expression position

### Type Checker Changes

1. Infer type of `match`/`if` from branch types
2. Verify all branches produce compatible types
3. Handle void branches (all must be void, or error)

### Codegen Changes

1. For match expressions: each arm compiles to a block that produces a value, phi node joins results
2. For if expressions: similar phi node structure
3. For block expressions: last expression's value is the block's value

### Example LLVM IR

```vibe
let x = if cond { 1 } else { 2 }
```

Compiles to:

```llvm
  br i1 %cond, label %then, label %else

then:
  br label %merge

else:
  br label %merge

merge:
  %x = phi i32 [ 1, %then ], [ 2, %else ]
```

## Future Extensions

### Loop Expressions with `break value`

```vibe
let found = loop {
    let item = next()
    if item.matches(query) {
        break item  // break with value
    }
}
```

### Try Expressions

```vibe
let data = try {
    let file = File.open(path)?
    file.read_all()?
}
```

## Alternatives Considered

1. **Ternary operator** (`cond ? a : b`) - Adds new syntax, less powerful than if-expressions
2. **Explicit return in blocks** - Too verbose for simple cases
3. **Statement-only** (current) - Requires mutable temporaries, less functional

## References

- Rust: Expression-oriented, semicolon distinguishes
- Kotlin: Everything is an expression, `if`/`when` return values
- Scala: Expression-oriented, last expression is value
- Swift: Limited expression forms, mostly statement-oriented
