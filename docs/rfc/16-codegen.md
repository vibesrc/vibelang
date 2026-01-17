# Section 16: LLVM Code Generation

Vibelang compiles to LLVM IR, leveraging LLVM's optimization passes and backend code generation.

## 16.1 Type Mapping

### Primitive Types to LLVM

| Vibelang | LLVM IR | Notes |
|----------|---------|-------|
| `i8` | `i8` | |
| `i16` | `i16` | |
| `i32` | `i32` | |
| `i64` | `i64` | |
| `u8` | `i8` | signedness in ops |
| `u16` | `i16` | signedness in ops |
| `u32` | `i32` | signedness in ops |
| `u64` | `i64` | signedness in ops |
| `f32` | `float` | |
| `f64` | `double` | |
| `bool` | `i1` | |
| `void` | `void` | |
| `*T` | `ptr` | opaque pointer |

### Composite Types to LLVM

```llvm
; Fixed array: i32[4]
[4 x i32]

; Struct
%Point = type { double, double }

; Slice<T>
%Slice = type { ptr, i64 }      ; ptr + len

; Vec<T>
%Array = type { ptr, i64, i64 } ; ptr + len + cap

; String (same as Vec<u8>)
%String = type { ptr, i64, i64 }
```

### Enum Representation

Enums use tagged unions:

```vibelang
enum Option<T> {
    Some(T)
    None
}
```

```llvm
; Option<i32> - tag + payload
%Option_i32 = type { i8, [4 x i8] }  ; tag + max payload size

; Tag values:
; 0 = Some
; 1 = None
```

```vibelang
enum Token {
    Ident(String)
    Number(i64)
    Plus
}
```

```llvm
; Largest variant is String (24 bytes)
%Token = type { i8, [24 x i8] }  ; tag + payload
```

## 16.2 Function Codegen

### Simple Function

```vibelang
fn add(a: i32, b: i32) -> i32 {
    return a + b
}
```

```llvm
define i32 @add(i32 %a, i32 %b) {
entry:
    %result = add i32 %a, %b
    ret i32 %result
}
```

### Function with Struct Parameter

```vibelang
fn distance(p1: &Point, p2: &Point) -> f64 {
    const dx = p1.x - p2.x
    const dy = p1.y - p2.y
    return sqrt(dx * dx + dy * dy)
}
```

```llvm
define double @distance(ptr %p1, ptr %p2) {
entry:
    ; Load p1.x
    %p1_x_ptr = getelementptr %Point, ptr %p1, i32 0, i32 0
    %p1_x = load double, ptr %p1_x_ptr
    
    ; Load p2.x
    %p2_x_ptr = getelementptr %Point, ptr %p2, i32 0, i32 0
    %p2_x = load double, ptr %p2_x_ptr
    
    ; dx = p1.x - p2.x
    %dx = fsub double %p1_x, %p2_x
    
    ; Load p1.y, p2.y (similar)
    ; ...
    
    ; dy = p1.y - p2.y
    %dy = fsub double %p1_y, %p2_y
    
    ; dx * dx + dy * dy
    %dx2 = fmul double %dx, %dx
    %dy2 = fmul double %dy, %dy
    %sum = fadd double %dx2, %dy2
    
    ; sqrt
    %result = call double @llvm.sqrt.f64(double %sum)
    ret double %result
}
```

## 16.3 Control Flow

### If Expression

```vibelang
let x = if condition { 1 } else { 2 }
```

```llvm
define i32 @example(i1 %condition) {
entry:
    br i1 %condition, label %then, label %else

then:
    br label %merge

else:
    br label %merge

merge:
    %x = phi i32 [ 1, %then ], [ 2, %else ]
    ret i32 %x
}
```

### While Loop

```vibelang
let i = 0
while i < 10 {
    i += 1
}
```

```llvm
define void @example() {
entry:
    %i = alloca i32
    store i32 0, ptr %i
    br label %while.cond

while.cond:
    %i.val = load i32, ptr %i
    %cmp = icmp slt i32 %i.val, 10
    br i1 %cmp, label %while.body, label %while.end

while.body:
    %i.val2 = load i32, ptr %i
    %inc = add i32 %i.val2, 1
    store i32 %inc, ptr %i
    br label %while.cond

while.end:
    ret void
}
```

### Match Expression

```vibelang
match opt {
    Option.Some(x) => x * 2
    Option.None => 0
}
```

```llvm
define i32 @example(ptr %opt) {
entry:
    ; Load tag
    %tag_ptr = getelementptr %Option_i32, ptr %opt, i32 0, i32 0
    %tag = load i8, ptr %tag_ptr
    
    switch i8 %tag, label %unreachable [
        i8 0, label %some
        i8 1, label %none
    ]

some:
    ; Extract payload
    %payload_ptr = getelementptr %Option_i32, ptr %opt, i32 0, i32 1
    %x = load i32, ptr %payload_ptr
    %result_some = mul i32 %x, 2
    br label %merge

none:
    br label %merge

merge:
    %result = phi i32 [ %result_some, %some ], [ 0, %none ]
    ret i32 %result

unreachable:
    unreachable
}
```

## 16.4 Memory Management

### Stack Allocation

```vibelang
let x: i32 = 42
let arr: i32[10] = [0; 10]
```

```llvm
define void @example() {
entry:
    %x = alloca i32
    store i32 42, ptr %x
    
    %arr = alloca [10 x i32]
    ; Zero initialize
    call void @llvm.memset.p0.i64(ptr %arr, i8 0, i64 40, i1 false)
    
    ret void
}
```

### Heap Allocation

```vibelang
let s = String.from("hello")
```

```llvm
define void @example() {
entry:
    ; Allocate String struct on stack
    %s = alloca %String
    
    ; Call String.from
    ; This internally calls malloc for the byte buffer
    call void @String_from(ptr %s, ptr @.str.hello, i64 5)
    
    ; ... use s ...
    
    ; Free at scope end
    call void @String_drop(ptr %s)
    ret void
}

@.str.hello = private constant [5 x i8] c"hello"
```

### Drop Insertion

The compiler inserts drop calls at:
- End of scope for owned values
- Before reassignment of owned values
- At early return points

```vibelang
fn example(cond: bool) {
    let a = String.from("a")
    let b = String.from("b")
    
    if cond {
        return              // drop b, drop a
    }
    
    let c = String.from("c")
}   // drop c, drop b, drop a
```

```llvm
define void @example(i1 %cond) {
entry:
    %a = alloca %String
    call void @String_from(ptr %a, ...)
    
    %b = alloca %String
    call void @String_from(ptr %b, ...)
    
    br i1 %cond, label %early_return, label %continue

early_return:
    call void @String_drop(ptr %b)
    call void @String_drop(ptr %a)
    ret void

continue:
    %c = alloca %String
    call void @String_from(ptr %c, ...)
    
    ; Normal exit
    call void @String_drop(ptr %c)
    call void @String_drop(ptr %b)
    call void @String_drop(ptr %a)
    ret void
}
```

## 16.5 Signed vs Unsigned Operations

LLVM uses the same integer types for signed and unsigned. The difference is in the operations:

| Operation | Signed | Unsigned |
|-----------|--------|----------|
| Division | `sdiv` | `udiv` |
| Remainder | `srem` | `urem` |
| Right shift | `ashr` | `lshr` |
| Compare < | `icmp slt` | `icmp ult` |
| Compare > | `icmp sgt` | `icmp ugt` |
| Compare <= | `icmp sle` | `icmp ule` |
| Compare >= | `icmp sge` | `icmp uge` |

```vibelang
let s: i32 = -10
let u: u32 = 10

s / 3       // sdiv
u / 3       // udiv

s > 0       // icmp sgt
u > 0       // icmp ugt
```

## 16.6 Generic Monomorphization

Each generic instantiation generates specialized code:

```vibelang
fn identity<T>(x: T) -> T {
    return x
}

identity(42)        // generates identity_i32
identity(3.14)      // generates identity_f64
identity("hi")      // generates identity_slice_u8
```

```llvm
define i32 @identity_i32(i32 %x) {
    ret i32 %x
}

define double @identity_f64(double %x) {
    ret double %x
}

define { ptr, i64 } @identity_slice_u8({ ptr, i64 } %x) {
    ret { ptr, i64 } %x
}
```

## 16.7 LLVM Intrinsics Used

```llvm
; Memory operations
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)

; Math
declare double @llvm.sqrt.f64(double)
declare double @llvm.pow.f64(double, double)
declare double @llvm.sin.f64(double)
declare double @llvm.cos.f64(double)
declare double @llvm.floor.f64(double)
declare double @llvm.ceil.f64(double)

; Overflow checking (for safe arithmetic)
declare { i32, i1 } @llvm.sadd.with.overflow.i32(i32, i32)
declare { i32, i1 } @llvm.ssub.with.overflow.i32(i32, i32)
declare { i32, i1 } @llvm.smul.with.overflow.i32(i32, i32)
```

## 16.8 Runtime Functions

The minimal runtime provides:

```llvm
; Memory allocation
declare ptr @vibe_alloc(i64)
declare ptr @vibe_realloc(ptr, i64)
declare void @vibe_free(ptr)

; Panic
declare void @vibe_panic(ptr, i64) noreturn

; Main entry point wrapper
define i32 @main() {
    call void @vibe_main()
    ret i32 0
}
```

## 16.9 Example: Complete Function

```vibelang
fn sum_positive(numbers: &Vec<i32>) -> i64 {
    let total: i64 = 0
    for n in numbers {
        if n > 0 {
            total += n as i64
        }
    }
    return total
}
```

```llvm
define i64 @sum_positive(ptr %numbers) {
entry:
    %total = alloca i64
    store i64 0, ptr %total
    
    ; Get array length
    %len_ptr = getelementptr %Array, ptr %numbers, i32 0, i32 1
    %len = load i64, ptr %len_ptr
    
    ; Get data pointer
    %data_ptr = getelementptr %Array, ptr %numbers, i32 0, i32 0
    %data = load ptr, ptr %data_ptr
    
    ; Loop index
    %i = alloca i64
    store i64 0, ptr %i
    br label %loop.cond

loop.cond:
    %i.val = load i64, ptr %i
    %cmp = icmp ult i64 %i.val, %len
    br i1 %cmp, label %loop.body, label %loop.end

loop.body:
    ; Get numbers[i]
    %elem_ptr = getelementptr i32, ptr %data, i64 %i.val
    %n = load i32, ptr %elem_ptr
    
    ; if n > 0
    %positive = icmp sgt i32 %n, 0
    br i1 %positive, label %add, label %loop.next

add:
    ; total += n as i64
    %n64 = sext i32 %n to i64
    %total.val = load i64, ptr %total
    %new_total = add i64 %total.val, %n64
    store i64 %new_total, ptr %total
    br label %loop.next

loop.next:
    ; i++
    %i.val2 = load i64, ptr %i
    %i.inc = add i64 %i.val2, 1
    store i64 %i.inc, ptr %i
    br label %loop.cond

loop.end:
    %result = load i64, ptr %total
    ret i64 %result
}
```
