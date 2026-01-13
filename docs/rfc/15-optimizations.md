# Section 15: Compiler Optimizations

Vibelang's compiler performs optimizations to transform simple, correct code into efficient executables.

## 15.1 Optimization Philosophy

### Write for Correctness, Compile for Speed

```vibelang
// You write clear, safe code
fn process(data: &String) -> Array<Token> {
    let tokens = Array<Token>()
    for line in data.split("\n") {
        let token = parse_line(line.copy())
        tokens.push(token)
    }
    return tokens
}

// Compiler optimizes:
// - Elides copies where safe
// - Stack allocates where possible
// - Inlines small functions
// - Removes dead code
```

### Optimization Levels

| Level | Flag | Description |
|-------|------|-------------|
| 0 | `-O0` | No optimization, fast compilation |
| 1 | `-O1` | Basic optimizations |
| 2 | `-O2` | Standard optimizations (default) |
| 3 | `-O3` | Aggressive optimizations |
| s | `-Os` | Optimize for size |

## 15.2 Copy Elision

### Automatic Copy Removal

The compiler removes unnecessary copies:

```vibelang
// Source
let s = get_string().copy()
use(s)

// If get_string()'s result outlives s and isn't mutated,
// compiler may elide the copy entirely
```

### Return Value Optimization (RVO)

```vibelang
fn create_large() -> LargeStruct {
    let result = LargeStruct { ... }
    return result
}

let x = create_large()

// Compiler constructs directly into x's memory
// No copy from function to caller
```

### Named Return Value Optimization (NRVO)

```vibelang
fn build() -> String {
    let s = String.new()    // s constructed in caller's space
    s.push("hello")
    s.push(" world")
    return s                 // no copy
}
```

## 15.3 Escape Analysis

### Stack Allocation Promotion

```vibelang
fn process() -> i64 {
    let arr = Array<i32>()      // normally heap
    arr.push(1)
    arr.push(2)
    arr.push(3)
    return arr.len() as i64     // arr doesn't escape
}

// Compiler may stack-allocate arr since it doesn't escape
```

### Borrow Inference

```vibelang
fn analyze(data: &String) {
    let slice = data.slice(0, 10).copy()
    process(&slice)
}

// If data outlives the function and slice doesn't escape,
// compiler may replace copy() with a borrow
```

### What Causes Escape

A value escapes when:
- Returned from function
- Stored in heap-allocated structure
- Passed to function that stores it
- Captured by closure that escapes

```vibelang
fn escapes() -> String {
    let s = String.from("hi")
    return s                    // escapes via return
}

fn does_not_escape() {
    let s = String.from("hi")
    print(&s)                   // borrowed, doesn't escape
}   // s freed here
```

## 15.4 Inlining

### Automatic Inlining

Small functions are inlined:

```vibelang
fn square(x: i32) -> i32 {
    return x * x
}

fn process() {
    let result = square(5)      // inlined to: let result = 5 * 5
}
```

### Inline Hints

```vibelang
#[inline]
fn small_helper() { ... }

#[inline(always)]
fn critical_path() { ... }

#[inline(never)]
fn rarely_called() { ... }
```

### Inlining Heuristics

The compiler considers:
- Function size (small = more likely to inline)
- Call frequency (hot = more likely to inline)
- Optimization level (higher = more aggressive)
- Resulting code size

## 15.5 Dead Code Elimination

### Unused Variables

```vibelang
fn example() {
    let unused = expensive()    // removed if side-effect free
    let used = compute()
    return used
}
```

### Unreachable Code

```vibelang
fn example(x: i32) -> i32 {
    if x > 0 {
        return x
    }
    return -x
    
    // This code is unreachable, removed:
    print("never executed")
}
```

### Dead Branches

```vibelang
static DEBUG = false

fn example() {
    if DEBUG {
        // entire block removed in release builds
        expensive_debug_check()
    }
}
```

## 15.6 Constant Folding

### Compile-Time Evaluation

```vibelang
static SIZE = 1024
static DOUBLED = SIZE * 2       // computed at compile time: 2048

fn example() {
    let x = 3 + 4               // folded to: let x = 7
    let y = SIZE * 4            // folded to: let y = 4096
}
```

### Constant Propagation

```vibelang
fn example() {
    let x = 10
    let y = x + 5               // propagated to: let y = 15
    let z = y * 2               // propagated to: let z = 30
}
```

## 15.7 Loop Optimizations

### Loop Invariant Code Motion

```vibelang
// Before
for i in 0..n {
    let len = arr.len()         // computed every iteration
    process(i, len)
}

// After (optimized)
let len = arr.len()             // hoisted out of loop
for i in 0..n {
    process(i, len)
}
```

### Loop Unrolling

```vibelang
// Before
for i in 0..4 {
    arr[i] = i * 2
}

// After (unrolled)
arr[0] = 0
arr[1] = 2
arr[2] = 4
arr[3] = 6
```

### Loop Fusion

```vibelang
// Before
for i in 0..n {
    a[i] = compute1(i)
}
for i in 0..n {
    b[i] = compute2(i)
}

// After (fused)
for i in 0..n {
    a[i] = compute1(i)
    b[i] = compute2(i)
}
```

## 15.8 Monomorphization Optimization

### Specialized Code Generation

```vibelang
fn process<T>(items: &Array<T>) { ... }

process(&int_array)     // generates process_i32
process(&string_array)  // generates process_String
```

### Dead Generic Elimination

Unused generic instantiations are not generated:

```vibelang
fn unused<T>(x: T) -> T { x }

// If never called, no code generated
```

## 15.9 Memory Optimizations

### Struct Layout Optimization

```vibelang
// Source
struct Example {
    a: u8
    b: u64
    c: u8
}

// Naive: 24 bytes (1 + 7 padding + 8 + 1 + 7 padding)
// Optimized: 16 bytes (reorder: b, a, c, 6 padding)
```

### Small String Optimization

```vibelang
// Short strings stored inline (no heap allocation)
let s = String.from("hi")   // may be stored inline

// Long strings use heap
let s = String.from("this is a much longer string")  // heap
```

### Empty Collection Optimization

```vibelang
let arr = Array<i32>()      // no heap allocation until first push
```

## 15.10 LLVM Optimizations

Vibelang leverages LLVM's optimization passes:

### LLVM Passes Used

- **mem2reg**: Promote memory to registers
- **instcombine**: Combine instructions
- **simplifycfg**: Simplify control flow
- **gvn**: Global value numbering
- **licm**: Loop invariant code motion
- **indvars**: Induction variable simplification
- **loop-unroll**: Loop unrolling
- **sroa**: Scalar replacement of aggregates
- **inline**: Function inlining
- **dce**: Dead code elimination
- **dse**: Dead store elimination
- **adce**: Aggressive dead code elimination

### Pass Pipeline

```
-O0: No LLVM passes (debug)
-O1: Basic passes (mem2reg, instcombine, simplifycfg)
-O2: Standard passes (all above + inline, gvn, licm, etc.)
-O3: Aggressive (all above + loop-unroll, vectorize)
```

## 15.11 Profile-Guided Optimization (Future)

### Collecting Profiles

```bash
# Build instrumented binary
vibec -O2 --profile-generate main.vibe

# Run to collect profile
./main < typical_input.txt

# Rebuild with profile
vibec -O2 --profile-use=profile.data main.vibe
```

### Profile-Based Decisions

- Branch prediction hints
- Hot/cold code separation
- Inline decisions
- Loop unroll factors

## 15.12 Optimization Examples

### Before and After

```vibelang
// Source code
fn sum_squares(arr: &Array<i32>) -> i64 {
    let total: i64 = 0
    for x in arr {
        let squared = (*x as i64) * (*x as i64)
        total += squared
    }
    return total
}
```

```llvm
; Optimized LLVM IR (conceptual)
define i64 @sum_squares(ptr %arr) {
entry:
    %len = ; load length
    %data = ; load data pointer
    br label %loop

loop:
    %i = phi i64 [0, %entry], [%i.next, %loop]
    %sum = phi i64 [0, %entry], [%sum.next, %loop]
    %ptr = getelementptr i32, ptr %data, i64 %i
    %x = load i32, ptr %ptr
    %x64 = sext i32 %x to i64
    %sq = mul i64 %x64, %x64
    %sum.next = add i64 %sum, %sq
    %i.next = add i64 %i, 1
    %cond = icmp ult i64 %i.next, %len
    br i1 %cond, label %loop, label %exit

exit:
    ret i64 %sum
}
```

The optimized version:
- Uses phi nodes instead of memory
- No allocations
- Tight loop with minimal overhead
