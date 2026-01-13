# Section 12: Error Handling

Vibelang uses `Result` and `Option` types for explicit error handling. There are no exceptions.

## 12.1 The Option Type

Represents a value that may or may not exist:

```vibelang
enum Option<T> {
    Some(T)
    None
}
```

### Creating Options

```vibelang
let some: Option<i32> = Option.Some(42)
let none: Option<i32> = Option.None
```

### Checking Options

```vibelang
if opt.is_some() {
    // has a value
}

if opt.is_none() {
    // no value
}
```

### Extracting Values

```vibelang
// Pattern matching (safest)
match opt {
    Option.Some(x) => use(x)
    Option.None => handle_none()
}

// Unwrap (panics if None)
let value = opt.unwrap()

// Unwrap with default
let value = opt.unwrap_or(default)

// Unwrap with computed default
let value = opt.unwrap_or_else(fn() { compute_default() })
```

## 12.2 The Result Type

Represents success or failure:

```vibelang
enum Result<T, E> {
    Ok(T)
    Err(E)
}
```

### Creating Results

```vibelang
let ok: Result<i32, String> = Result.Ok(42)
let err: Result<i32, String> = Result.Err(String.from("error"))
```

### Checking Results

```vibelang
if result.is_ok() {
    // success
}

if result.is_err() {
    // failure
}
```

### Extracting Values

```vibelang
// Pattern matching (safest)
match result {
    Result.Ok(value) => use(value)
    Result.Err(error) => handle_error(error)
}

// Unwrap (panics if Err)
let value = result.unwrap()

// Unwrap error (panics if Ok)
let error = result.unwrap_err()

// Unwrap with default
let value = result.unwrap_or(default)
```

## 12.3 Error Propagation

### The ? Operator

Propagate errors automatically:

```vibelang
fn read_config() -> Result<Config, Error> {
    let text = read_file("config.json")?    // returns early if Err
    let config = parse_json(&text)?          // returns early if Err
    return Result.Ok(config)
}
```

The `?` operator:
1. If `Ok(value)`, extracts `value` and continues
2. If `Err(error)`, returns `Err(error)` immediately

### Equivalent Expansion

```vibelang
// This:
let value = expression?

// Expands to:
let value = match expression {
    Result.Ok(v) => v
    Result.Err(e) => return Result.Err(e)
}
```

### ? on Option

Works on Option too, converting None to early return:

```vibelang
fn find_user_email(id: u64) -> Option<String> {
    let user = find_user(id)?           // returns None if not found
    let email = user.email?             // returns None if no email
    return Option.Some(email)
}
```

## 12.4 Error Conversion

### From Trait (Conceptual)

Error types can be converted:

```vibelang
fn process() -> Result<Data, AppError> {
    let file = File.open("data.txt")?   // IoError -> AppError
    let data = parse(&file)?             // ParseError -> AppError
    return Result.Ok(data)
}
```

### Manual Conversion

```vibelang
fn process() -> Result<Data, AppError> {
    let file = File.open("data.txt")
        .map_err(fn(e: IoError) { AppError.Io(e) })?
    
    let data = parse(&file)
        .map_err(fn(e: ParseError) { AppError.Parse(e) })?
    
    return Result.Ok(data)
}
```

## 12.5 Combinators

### Option Combinators

```vibelang
// map: transform inner value
let doubled = opt.map(fn(x) { x * 2 })

// and_then: chain operations
let result = opt
    .and_then(fn(x) { validate(x) })
    .and_then(fn(x) { process(x) })

// filter: keep if predicate true
let positive = opt.filter(fn(x) { x > 0 })

// or: provide alternative
let value = opt1.or(opt2)

// or_else: compute alternative
let value = opt.or_else(fn() { compute_alternative() })
```

### Result Combinators

```vibelang
// map: transform success value
let doubled = result.map(fn(x) { x * 2 })

// map_err: transform error value
let converted = result.map_err(fn(e) { convert_error(e) })

// and_then: chain operations
let final = result
    .and_then(fn(x) { step1(x) })
    .and_then(fn(x) { step2(x) })

// or_else: handle error and try again
let recovered = result.or_else(fn(e) { try_recover(e) })
```

## 12.6 Custom Error Types

### Error Enum

```vibelang
enum AppError {
    NotFound { resource: String }
    PermissionDenied { action: String }
    InvalidInput { message: String }
    Io { inner: IoError }
    Parse { inner: ParseError }
}

impl AppError {
    fn not_found(resource: String) -> AppError {
        return AppError.NotFound { resource }
    }
    
    fn permission_denied(action: String) -> AppError {
        return AppError.PermissionDenied { action }
    }
    
    fn message(self: &AppError) -> String {
        return match self {
            AppError.NotFound { resource } => 
                format("Resource not found: {}", resource)
            AppError.PermissionDenied { action } =>
                format("Permission denied for: {}", action)
            AppError.InvalidInput { message } =>
                format("Invalid input: {}", message)
            AppError.Io { inner } =>
                format("IO error: {}", inner.message())
            AppError.Parse { inner } =>
                format("Parse error: {}", inner.message())
        }
    }
}
```

### Error Struct

```vibelang
struct Error {
    kind: ErrorKind
    message: String
    source: Option<*Error>
}

enum ErrorKind {
    NotFound
    PermissionDenied
    InvalidInput
    Io
    Parse
    Internal
}

impl Error {
    fn new(kind: ErrorKind, message: String) -> Error {
        return Error {
            kind: kind,
            message: message,
            source: Option.None
        }
    }
    
    fn with_source(kind: ErrorKind, message: String, source: Error) -> Error {
        return Error {
            kind: kind,
            message: message,
            source: Option.Some(Box.new(source))
        }
    }
}
```

## 12.7 Panic

For unrecoverable errors:

```vibelang
fn divide(a: i32, b: i32) -> i32 {
    if b == 0 {
        panic("division by zero")
    }
    return a / b
}
```

### When to Panic

Panic is appropriate for:
- Programming errors (bugs)
- Unrecoverable states
- Violated invariants

Use Result for:
- Expected errors (file not found, network error)
- User input validation
- Recoverable conditions

### Assert

```vibelang
assert(condition)
assert_msg(condition, "error message")

// Debug-only asserts
debug_assert(expensive_check())
```

### Unreachable

Mark code that should never execute:

```vibelang
fn process(kind: Kind) -> i32 {
    match kind {
        Kind.A => 1
        Kind.B => 2
        Kind.C => 3
    }
    unreachable()   // if match doesn't cover all cases
}
```

## 12.8 Early Return Patterns

### Guard Clauses

```vibelang
fn process(data: Option<Data>) -> Result<Output, Error> {
    // Guard: return early if no data
    let data = match data {
        Option.Some(d) => d
        Option.None => return Result.Err(Error.no_data())
    }
    
    // Guard: return early if invalid
    if not data.is_valid() {
        return Result.Err(Error.invalid_data())
    }
    
    // Main logic
    let result = transform(data)
    return Result.Ok(result)
}
```

### Let-Else

```vibelang
fn process(data: Option<Data>) -> Result<Output, Error> {
    let Option.Some(data) = data else {
        return Result.Err(Error.no_data())
    }
    
    // data now available
    return Result.Ok(transform(data))
}
```

## 12.9 Error Context

Add context to errors:

```vibelang
fn load_user(id: u64) -> Result<User, Error> {
    let data = read_file(format("users/{}.json", id))
        .map_err(fn(e) { Error.with_context(e, format("loading user {}", id)) })?
    
    let user = parse_user(&data)
        .map_err(fn(e) { Error.with_context(e, format("parsing user {}", id)) })?
    
    return Result.Ok(user)
}
```

## 12.10 Result and Option Interop

### Option to Result

```vibelang
// Convert Option to Result
let result: Result<i32, String> = opt.ok_or(String.from("not found"))
let result: Result<i32, String> = opt.ok_or_else(fn() { compute_error() })
```

### Result to Option

```vibelang
// Convert Result to Option (discards error)
let opt: Option<i32> = result.ok()

// Get error as Option
let err_opt: Option<Error> = result.err()
```

## 12.11 Handling Multiple Errors

### Collect Results

```vibelang
fn process_all(items: &Array<Item>) -> Result<Array<Output>, Error> {
    let results = Array<Output>()
    
    for item in items {
        let output = process(item)?     // fail on first error
        results.push(output)
    }
    
    return Result.Ok(results)
}
```

### Collect All Errors

```vibelang
fn process_all(items: &Array<Item>) -> (Array<Output>, Array<Error>) {
    let outputs = Array<Output>()
    let errors = Array<Error>()
    
    for item in items {
        match process(item) {
            Result.Ok(output) => outputs.push(output)
            Result.Err(error) => errors.push(error)
        }
    }
    
    return (outputs, errors)
}
```

### First Success

```vibelang
fn try_sources() -> Result<Data, Error> {
    // Try multiple sources, return first success
    let sources = [source1, source2, source3]
    
    for source in &sources {
        match source.fetch() {
            Result.Ok(data) => return Result.Ok(data)
            Result.Err(_) => continue
        }
    }
    
    return Result.Err(Error.all_sources_failed())
}
```

## 12.12 Complete Example

```vibelang
struct Config {
    host: String
    port: u32
    database: String
}

enum ConfigError {
    FileNotFound { path: String }
    ParseError { line: u32, message: String }
    MissingField { field: String }
    InvalidValue { field: String, value: String }
}

impl ConfigError {
    fn message(self: &ConfigError) -> String {
        return match self {
            ConfigError.FileNotFound { path } =>
                format("Config file not found: {}", path)
            ConfigError.ParseError { line, message } =>
                format("Parse error at line {}: {}", line, message)
            ConfigError.MissingField { field } =>
                format("Missing required field: {}", field)
            ConfigError.InvalidValue { field, value } =>
                format("Invalid value for {}: {}", field, value)
        }
    }
}

fn load_config(path: &String) -> Result<Config, ConfigError> {
    // Read file
    let text = read_file(path)
        .map_err(fn(_) { ConfigError.FileNotFound { path: path.copy() } })?
    
    // Parse
    let parsed = parse_config_file(&text)?
    
    // Extract fields
    let host = parsed.get("host")
        .ok_or(ConfigError.MissingField { field: String.from("host") })?
    
    let port_str = parsed.get("port")
        .ok_or(ConfigError.MissingField { field: String.from("port") })?
    
    let port = parse_uint(port_str)
        .map_err(fn(_) { ConfigError.InvalidValue { 
            field: String.from("port"), 
            value: port_str.copy() 
        }})?
    
    let database = parsed.get("database")
        .ok_or(ConfigError.MissingField { field: String.from("database") })?
    
    return Result.Ok(Config {
        host: host.copy(),
        port: port as u32,
        database: database.copy()
    })
}

fn main() -> Result<void, ConfigError> {
    let config = load_config(&String.from("config.ini"))?
    
    print("Loaded config:")
    print(format("  Host: {}", &config.host))
    print(format("  Port: {}", config.port))
    print(format("  Database: {}", &config.database))
    
    return Result.Ok(void)
}
```
