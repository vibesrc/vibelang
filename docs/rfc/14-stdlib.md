# Section 14: Standard Library

The standard library provides essential types and functions built on top of Vibelang's primitives. All stdlib types are written in Vibelang itself (once self-hosting is achieved).

## 14.1 Core Types

### Vec<T>

Dynamic, growable vector:

```vibelang
struct Vec<T> {
    ptr: *T
    len: u64
    cap: u64
}

impl<T> Vec<T> {
    /// Create empty array
    fn new() -> Vec<T>
    
    /// Create with initial capacity
    fn with_capacity(cap: u64) -> Vec<T>
    
    /// Number of elements
    fn len(self: &Vec<T>) -> u64
    
    /// Current capacity
    fn capacity(self: &Vec<T>) -> u64
    
    /// Is empty?
    fn is_empty(self: &Vec<T>) -> bool
    
    /// Add element to end
    fn push(self: ~Vec<T>, item: T)
    
    /// Remove and return last element
    fn pop(self: ~Vec<T>) -> Option<T>
    
    /// Get element by index
    fn get(self: &Vec<T>, index: u64) -> Option<&T>
    
    /// Get mutable element by index
    fn get_mut(self: ~Vec<T>, index: u64) -> Option<~T>
    
    /// First element
    fn first(self: &Vec<T>) -> Option<&T>
    
    /// Last element
    fn last(self: &Vec<T>) -> Option<&T>
    
    /// Clear all elements
    fn clear(self: ~Vec<T>)
    
    /// Insert at index
    fn insert(self: ~Vec<T>, index: u64, item: T)
    
    /// Remove at index
    fn remove(self: ~Vec<T>, index: u64) -> T
    
    /// Borrow as slice
    fn as_slice(self: &Vec<T>) -> Slice<T>
    
    /// Iterate
    fn iter(self: &Vec<T>) -> VecIter<T>
}
```

#### Vec Usage

```vibelang
let numbers = Vec<i32>()
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
    fn to_array(self: &Slice<T>) -> Vec<T>
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
    bytes: Vec<u8>
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
    fn split(self: &String, delim: Slice<u8>) -> Vec<Slice<u8>>
    
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
    buckets: Vec<Bucket<K, V>>
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
    fn read(self: &File, buf: &Vec<u8>) -> Result<u64, IoError>
    
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

## 14.8 Networking (std.net)

Safe wrappers around socket syscalls with RAII semantics.

### Address Types

```vibelang
/// IPv4 address (stored in network byte order)
struct Ipv4Addr {
    octets: u32
}

impl Ipv4Addr {
    /// Create from four octets
    fn new(a: u8, b: u8, c: u8, d: u8) -> Ipv4Addr

    /// Localhost (127.0.0.1)
    fn localhost() -> Ipv4Addr

    /// Unspecified (0.0.0.0) - binds to all interfaces
    fn unspecified() -> Ipv4Addr

    /// Get raw bits in network order
    fn to_bits(self: &Ipv4Addr) -> u32
}

/// IPv4 socket address (IP + port)
struct SocketAddrV4 {
    addr: Ipv4Addr
    port: u16
}

impl SocketAddrV4 {
    fn new(addr: Ipv4Addr, port: u16) -> SocketAddrV4
    fn ip(self: &SocketAddrV4) -> Ipv4Addr
    fn port(self: &SocketAddrV4) -> u16
}
```

### TCP

```vibelang
/// TCP listener for accepting connections
struct TcpListener {
    fd: i32
}

impl TcpListener {
    /// Bind to address and start listening
    fn bind(addr: SocketAddrV4) -> TcpListener

    /// Check if listener is valid
    fn is_valid(self: &TcpListener) -> bool

    /// Accept incoming connection
    fn accept(self: &TcpListener) -> TcpStream

    /// Close the listener
    fn close(self: ~TcpListener)
}

/// Connected TCP stream
struct TcpStream {
    fd: i32
}

impl TcpStream {
    /// Connect to remote address
    fn connect(addr: SocketAddrV4) -> TcpStream

    /// Check if stream is valid
    fn is_valid(self: &TcpStream) -> bool

    /// Read into buffer, returns bytes read
    fn read(self: &TcpStream, buf: *u8, len: i64) -> i64

    /// Write from buffer, returns bytes written
    fn write(self: &TcpStream, buf: *u8, len: i64) -> i64

    /// Write string slice
    fn write_str(self: &TcpStream, s: Slice<u8>) -> i64

    /// Shutdown (SHUT_RD, SHUT_WR, or SHUT_RDWR)
    fn shutdown(self: &TcpStream, how: i32) -> bool

    /// Close the stream
    fn close(self: ~TcpStream)
}
```

### UDP

```vibelang
/// UDP socket for datagrams
struct UdpSocket {
    fd: i32
}

impl UdpSocket {
    /// Bind to local address
    fn bind(addr: SocketAddrV4) -> UdpSocket

    /// Create unbound socket
    fn unbound() -> UdpSocket

    /// Check if socket is valid
    fn is_valid(self: &UdpSocket) -> bool

    /// Receive datagram
    fn recv(self: &UdpSocket, buf: *u8, len: i64) -> i64

    /// Send datagram to address
    fn send_to(self: &UdpSocket, buf: *u8, len: i64, addr: SocketAddrV4) -> i64

    /// Close the socket
    fn close(self: ~UdpSocket)
}
```

#### Networking Usage

```vibelang
use std.net.{Ipv4Addr, SocketAddrV4, TcpListener, TcpStream}

fn main() -> i32 {
    let addr = SocketAddrV4.new(Ipv4Addr.localhost(), 8080)
    let listener = TcpListener.bind(addr)

    if listener.is_valid() {
        let client = listener.accept()
        if client.is_valid() {
            client.write_str("Hello!\n")
            client.close()
        }
        listener.close()
    }
    return 0
}
```

## 14.9 Process Control (std.process)

Safe wrappers around process syscalls.

```vibelang
/// Get current process ID
fn getpid() -> i32

/// Get parent process ID
fn getppid() -> i32

/// Exit process with status code
fn exit(code: i32) -> !

/// Get current working directory
fn getcwd() -> Option<Slice<u8>>

/// Change working directory
fn chdir(path: Slice<u8>) -> bool

/// Get environment variable
fn getenv(name: Slice<u8>) -> Option<Slice<u8>>

/// Set environment variable
fn setenv(name: Slice<u8>, value: Slice<u8>) -> bool

/// Fork process (returns 0 in child, child PID in parent, -1 on error)
fn fork() -> i32

/// Execute program (replaces current process)
fn execve(path: Slice<u8>, args: *Slice<u8>, envp: *Slice<u8>) -> i32

/// Wait for child process
fn waitpid(pid: i32, options: i32) -> (i32, i32)  // (pid, status)

/// Send signal to process
fn kill(pid: i32, sig: i32) -> i32
```

### Signal Constants

```vibelang
pub static SIGTERM: i32 = 15
pub static SIGKILL: i32 = 9
pub static SIGINT: i32 = 2
pub static SIGHUP: i32 = 1
```

## 14.10 Memory Management (std.mem)

Low-level memory allocation and virtual memory control.

```vibelang
/// Allocate heap memory
fn alloc(size: i64) -> *u8

/// Resize allocation
fn resize(ptr: *u8, new_size: i64) -> *u8

/// Free heap memory
fn dealloc(ptr: *u8)

/// Map virtual memory (wrapper around mmap)
fn mmap(addr: *u8, length: i64, prot: i32, flags: i32, fd: i32, offset: i64) -> *u8

/// Unmap virtual memory
fn munmap(addr: *u8, length: i64) -> i32

/// Change memory protection
fn mprotect(addr: *u8, length: i64, prot: i32) -> i32

/// Advise kernel about memory usage
fn madvise(addr: *u8, length: i64, advice: i32) -> i32
```

### Memory Protection Constants

```vibelang
pub static PROT_NONE: i32 = 0
pub static PROT_READ: i32 = 1
pub static PROT_WRITE: i32 = 2
pub static PROT_EXEC: i32 = 4

pub static MAP_SHARED: i32 = 1
pub static MAP_PRIVATE: i32 = 2
pub static MAP_ANONYMOUS: i32 = 32
pub static MAP_FIXED: i32 = 16
```

## 14.11 Time (std.time)

Time measurement and sleeping.

```vibelang
/// A point in time (monotonic clock)
struct Instant {
    secs: i64
    nanos: i64
}

impl Instant {
    /// Get current time
    fn now() -> Instant

    /// Duration since another instant
    fn duration_since(self: &Instant, earlier: &Instant) -> Duration

    /// Time elapsed since this instant
    fn elapsed(self: &Instant) -> Duration
}

/// A duration of time
struct Duration {
    secs: i64
    nanos: i64
}

impl Duration {
    /// Create from seconds
    fn from_secs(secs: i64) -> Duration

    /// Create from milliseconds
    fn from_millis(millis: i64) -> Duration

    /// Create from nanoseconds
    fn from_nanos(nanos: i64) -> Duration

    /// Total seconds (truncated)
    fn as_secs(self: &Duration) -> i64

    /// Total milliseconds
    fn as_millis(self: &Duration) -> i64

    /// Total nanoseconds
    fn as_nanos(self: &Duration) -> i64
}

/// Sleep for duration
fn sleep(duration: Duration)

/// Sleep for milliseconds
fn sleep_ms(ms: i64)
```

#### Time Usage

```vibelang
use std.time.{Instant, Duration, sleep_ms}

fn main() {
    let start = Instant.now()

    // Do some work
    sleep_ms(100)

    let elapsed = start.elapsed()
    print("Took ${elapsed.as_millis()} ms\n")
}
```
