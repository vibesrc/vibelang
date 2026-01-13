# Section 17: Security Considerations

Vibelang is designed with memory safety as a core principle. This section documents the security guarantees and remaining risks.

## 17.1 Memory Safety Guarantees

### What Vibelang Prevents

| Vulnerability | Prevention Mechanism |
|--------------|----------------------|
| Use-after-free | Ownership system, borrow checker |
| Double-free | Single ownership rule |
| Dangling pointers | Borrows cannot outlive owner |
| Buffer overflow | Bounds-checked array access |
| Null dereference | Option type instead of null |
| Data races | Single ownership (no shared mutable state) |
| Uninitialized memory | All variables must be initialized |

### Use-After-Free Prevention

```vibelang
let a = String.from("hello")
let b = a                       // ownership moves to b
print(&a)                       // COMPILE ERROR: a was moved
```

### Double-Free Prevention

```vibelang
let s = String.from("hello")
drop(s)                         // s freed
drop(s)                         // COMPILE ERROR: s already moved
```

### Dangling Pointer Prevention

```vibelang
fn bad() -> &String {
    let s = String.from("hello")
    return &s                   // COMPILE ERROR: s does not live long enough
}
```

### Buffer Overflow Prevention

```vibelang
let arr = [1, 2, 3]
let x = arr[10]                 // RUNTIME PANIC: index out of bounds

// Safe alternative
match arr.get(10) {
    Option.Some(x) => use(x)
    Option.None => handle_missing()
}
```

## 17.2 Unsafe Operations

### Raw Pointers

Raw pointers bypass safety checks:

```vibelang
let x = 42
let ptr: *i32 = &x

// These operations are unsafe:
let value = *ptr                // dereference
let next = ptr_add(ptr, 1)      // pointer arithmetic
```

### When Unsafe is Needed

- FFI (Foreign Function Interface)
- Low-level memory manipulation
- Performance-critical code with proven correctness
- Implementing safe abstractions

### Unsafe Blocks (If Supported)

```vibelang
fn safe_abstraction(ptr: *u8, len: u64) -> Slice<u8> {
    unsafe {
        // Raw pointer operations
        // Programmer responsible for safety
    }
}
```

## 17.3 Integer Safety

### Overflow Behavior

Integer overflow is undefined behavior by default (like C):

```vibelang
let x: u8 = 255
let y = x + 1                   // UNDEFINED BEHAVIOR
```

### Safe Arithmetic

Use checked operations for safety:

```vibelang
// Returns Option<T>
let result = checked_add(x, 1)
match result {
    Option.Some(value) => use(value)
    Option.None => handle_overflow()
}

// Wrapping (defined behavior)
let wrapped = wrapping_add(x, 1)    // 0

// Saturating (clamps to max)
let saturated = saturating_add(x, 1) // 255
```

### Division by Zero

```vibelang
let x = 10 / 0                  // UNDEFINED BEHAVIOR

// Safe alternative
let result = checked_div(10, 0) // returns Option.None
```

## 17.4 Type Safety

### No Implicit Conversions

```vibelang
let x: i32 = 42
let y: i64 = x                  // COMPILE ERROR: type mismatch
let y: i64 = x as i64           // OK: explicit cast
```

### Cast Safety

Some casts may lose information:

```vibelang
let big: i64 = 1_000_000_000_000
let small: i32 = big as i32     // TRUNCATED (undefined or wrapped)

// Safe cast
let result = try_cast<i64, i32>(big)
match result {
    Option.Some(value) => use(value)
    Option.None => handle_overflow()
}
```

### Enum Exhaustiveness

```vibelang
match status {
    Status.Active => ...
    Status.Pending => ...
    // COMPILE ERROR: non-exhaustive, missing Status.Inactive
}
```

## 17.5 Concurrency Safety

### No Data Races

Single ownership prevents data races:

```vibelang
let data = SharedData.new()

// Can't have multiple mutable references
let ref1 = &data
let ref2 = &data                // OK: both immutable borrows

let mut_ref = &data             // borrow
data.modify()                   // COMPILE ERROR: can't mutate while borrowed
```

### Thread Safety (Future)

When concurrency is added:

```vibelang
// Send: safe to transfer between threads
// Sync: safe to share references between threads

fn spawn<T: Send>(f: fn() -> T) -> Thread<T>

// Arc for shared ownership across threads
let shared = Arc.new(data)
```

## 17.6 Input Validation

### String Handling

```vibelang
// Always validate external input
fn process_input(input: &String) -> Result<Data, Error> {
    if input.len() > MAX_INPUT_SIZE {
        return Result.Err(Error.InputTooLarge)
    }
    
    // Validate UTF-8 (if needed)
    if not input.is_valid_utf8() {
        return Result.Err(Error.InvalidUtf8)
    }
    
    // Parse safely
    parse(input)
}
```

### Path Traversal

```vibelang
fn safe_path(user_input: &String) -> Result<Path, Error> {
    let path = Path.from(user_input)
    
    // Check for traversal attempts
    if path.contains("..") {
        return Result.Err(Error.PathTraversal)
    }
    
    // Ensure within allowed directory
    let canonical = path.canonicalize()?
    if not canonical.starts_with(ALLOWED_ROOT) {
        return Result.Err(Error.PathTraversal)
    }
    
    return Result.Ok(canonical)
}
```

## 17.7 Error Handling Security

### Don't Leak Information

```vibelang
// Bad: leaks internal details
fn authenticate(user: &String, pass: &String) -> Result<Session, Error> {
    let user_record = db.find_user(user)?  // reveals user exists
    if user_record.password != hash(pass) {
        return Result.Err(Error.WrongPassword)  // reveals password wrong
    }
    ...
}

// Good: generic error
fn authenticate(user: &String, pass: &String) -> Result<Session, Error> {
    let user_record = db.find_user(user)
    match user_record {
        Option.None => Result.Err(Error.AuthenticationFailed)
        Option.Some(record) => {
            if record.password != hash(pass) {
                return Result.Err(Error.AuthenticationFailed)
            }
            ...
        }
    }
}
```

### Timing Attacks

```vibelang
// Bad: early return reveals information
fn compare_secrets(a: &Slice<u8>, b: &Slice<u8>) -> bool {
    if a.len() != b.len() { return false }
    for i in 0..a.len() {
        if a[i] != b[i] { return false }  // timing leak
    }
    return true
}

// Good: constant-time comparison
fn compare_secrets(a: &Slice<u8>, b: &Slice<u8>) -> bool {
    if a.len() != b.len() { return false }
    let result: u8 = 0
    for i in 0..a.len() {
        result |= a[i] ^ b[i]  // no early exit
    }
    return result == 0
}
```

## 17.8 Secure Defaults

### Memory Zeroing

Sensitive data should be zeroed when freed:

```vibelang
struct SecureString {
    data: Array<u8>
}

impl SecureString {
    fn drop(self: SecureString) {
        // Zero memory before freeing
        for i in 0..self.data.len() {
            self.data[i] = 0
        }
        // Then normal drop
    }
}
```

### Secure Random

```vibelang
// For cryptographic purposes
let key = secure_random_bytes(32)

// NOT for crypto (predictable)
let value = random_int()
```

## 17.9 Known Limitations

### What Vibelang Does NOT Prevent

| Issue | Reason |
|-------|--------|
| Logic errors | Semantic correctness is programmer's job |
| Resource exhaustion | No built-in limits |
| Side-channel attacks | Requires careful coding |
| Unsafe block bugs | Programmer assumes responsibility |
| FFI bugs | External code not checked |

### Unsafe Code Risks

When using unsafe:
- Pointer arithmetic can create invalid pointers
- Dereferencing may cause UB
- Type punning can violate invariants
- Memory corruption possible

### Best Practices

1. **Minimize unsafe code** - Encapsulate in safe abstractions
2. **Audit unsafe blocks** - Extra review required
3. **Test boundaries** - Fuzz test unsafe code
4. **Document invariants** - Explain safety requirements

## 17.10 Security Checklist

### Code Review

- [ ] No unbounded allocations from user input
- [ ] All array accesses bounds-checked or proven safe
- [ ] Integer arithmetic checked where overflow possible
- [ ] External input validated before use
- [ ] Error messages don't leak sensitive information
- [ ] Secrets zeroed after use
- [ ] Unsafe blocks minimized and documented

### Build Configuration

- [ ] Optimization level appropriate for use case
- [ ] Debug assertions enabled during testing
- [ ] Stack protector enabled (if available)
- [ ] Address sanitizer used in testing
