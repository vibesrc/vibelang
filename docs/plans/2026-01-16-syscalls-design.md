# Syscalls Design Document

This document outlines the plan for adding system call support to Vibelang, including raw syscall intrinsics and safe, idiomatic standard library abstractions.

## Design Principles

1. **Raw syscalls are intrinsics** - Prefixed with `sys_`, return error codes directly
2. **Safe wrappers return Result** - Handle errors idiomatically with `Result<T, Error>`
3. **Zero-cost abstractions** - Safe wrappers compile to efficient code
4. **Platform-specific constants** - Define in stdlib, not hardcoded in compiler

---

## 1. Time Syscalls

### Raw Intrinsics

```vibe
// Get current time from specified clock
// clock_id: CLOCK_REALTIME (0), CLOCK_MONOTONIC (1)
// Returns 0 on success, -1 on error
sys_clock_gettime(clock_id: i32, tv_sec: *i64, tv_nsec: *i64) -> i32

// High-precision sleep
// Returns 0 on success, -1 if interrupted (remaining time in rem)
sys_nanosleep(req_sec: i64, req_nsec: i64, rem_sec: *i64, rem_nsec: *i64) -> i32
```

### Safe Abstractions: `std.time`

```vibe
// Clock identifiers
pub static CLOCK_REALTIME: i32 = 0   // Wall clock, affected by NTP
pub static CLOCK_MONOTONIC: i32 = 1  // Steady clock, for measuring durations

/// A duration of time (seconds + nanoseconds)
pub struct Duration {
    secs: i64
    nanos: i64  // Always 0..999_999_999
}

impl Duration {
    pub fn zero() -> Duration
    pub fn from_secs(s: i64) -> Duration
    pub fn from_millis(ms: i64) -> Duration
    pub fn from_micros(us: i64) -> Duration
    pub fn from_nanos(ns: i64) -> Duration

    pub fn as_secs(&self) -> i64
    pub fn as_millis(&self) -> i64
    pub fn as_micros(&self) -> i64
    pub fn as_nanos(&self) -> i64

    pub fn add(&self, other: &Duration) -> Duration
    pub fn sub(&self, other: &Duration) -> Duration
}

/// A point in time from a monotonic clock (for measuring elapsed time)
pub struct Instant {
    secs: i64
    nanos: i64
}

impl Instant {
    /// Capture current instant from monotonic clock
    pub fn now() -> Instant

    /// Time elapsed since this instant
    pub fn elapsed(&self) -> Duration

    /// Duration between two instants
    pub fn duration_since(&self, earlier: &Instant) -> Duration
}

/// Wall-clock time (for timestamps, calendar operations)
pub struct SystemTime {
    secs: i64    // Seconds since Unix epoch
    nanos: i64
}

impl SystemTime {
    pub fn now() -> SystemTime
    pub fn unix_timestamp(&self) -> i64
}

// Module-level functions
pub fn sleep(duration: Duration)
pub fn sleep_secs(s: i64)
pub fn sleep_millis(ms: i64)
```

### Use Cases
- Benchmarking with `Instant::now()` and `elapsed()`
- Rate limiting with `sleep()`
- Timestamps for logging with `SystemTime::now()`

---

## 2. Networking Syscalls

### Raw Intrinsics

```vibe
// Create a socket
// domain: AF_INET (2), AF_INET6 (10), AF_UNIX (1)
// type: SOCK_STREAM (1), SOCK_DGRAM (2)
// protocol: usually 0
// Returns fd on success, -1 on error
sys_socket(domain: i32, type: i32, protocol: i32) -> i32

// Bind socket to address
sys_bind(sockfd: i32, addr: *u8, addrlen: i32) -> i32

// Listen for connections
sys_listen(sockfd: i32, backlog: i32) -> i32

// Accept connection
// Returns new fd on success, -1 on error
sys_accept(sockfd: i32, addr: *u8, addrlen: *i32) -> i32

// Connect to remote address
sys_connect(sockfd: i32, addr: *u8, addrlen: i32) -> i32

// Send data (for connected sockets)
sys_send(sockfd: i32, buf: *u8, len: i64, flags: i32) -> i64

// Receive data
sys_recv(sockfd: i32, buf: *u8, len: i64, flags: i32) -> i64

// Send to specific address (UDP)
sys_sendto(sockfd: i32, buf: *u8, len: i64, flags: i32, addr: *u8, addrlen: i32) -> i64

// Receive with sender address
sys_recvfrom(sockfd: i32, buf: *u8, len: i64, flags: i32, addr: *u8, addrlen: *i32) -> i64

// Set socket options
sys_setsockopt(sockfd: i32, level: i32, optname: i32, optval: *u8, optlen: i32) -> i32

// Shutdown socket
sys_shutdown(sockfd: i32, how: i32) -> i32
```

### Safe Abstractions: `std.net`

```vibe
// Address families
pub static AF_INET: i32 = 2
pub static AF_INET6: i32 = 10

// Socket types
pub static SOCK_STREAM: i32 = 1  // TCP
pub static SOCK_DGRAM: i32 = 2   // UDP

/// IPv4 address
pub struct Ipv4Addr {
    octets: u8[4]
}

impl Ipv4Addr {
    pub fn new(a: u8, b: u8, c: u8, d: u8) -> Ipv4Addr
    pub fn localhost() -> Ipv4Addr  // 127.0.0.1
    pub fn any() -> Ipv4Addr        // 0.0.0.0
    pub fn parse(s: &Slice<u8>) -> Result<Ipv4Addr, Error>
}

/// Socket address (IP + port)
pub struct SocketAddr {
    ip: Ipv4Addr
    port: u16
}

impl SocketAddr {
    pub fn new(ip: Ipv4Addr, port: u16) -> SocketAddr
}

/// TCP listener for accepting connections
pub struct TcpListener {
    fd: i32
}

impl TcpListener {
    /// Bind to address and start listening
    pub fn bind(addr: SocketAddr) -> Result<TcpListener, Error>

    /// Accept next connection (blocks)
    pub fn accept(~self) -> Result<(TcpStream, SocketAddr), Error>

    /// Close the listener
    pub fn close(~self) -> Result<(), Error>
}

/// TCP stream for reading/writing
pub struct TcpStream {
    fd: i32
}

impl TcpStream {
    /// Connect to remote address
    pub fn connect(addr: SocketAddr) -> Result<TcpStream, Error>

    /// Read bytes into buffer
    pub fn read(~self, buf: *u8, len: i64) -> Result<i64, Error>

    /// Write bytes from buffer
    pub fn write(~self, buf: &Slice<u8>) -> Result<i64, Error>

    /// Shutdown reading, writing, or both
    pub fn shutdown(~self, how: Shutdown) -> Result<(), Error>

    /// Close the stream
    pub fn close(~self) -> Result<(), Error>
}

pub enum Shutdown {
    Read
    Write
    Both
}

/// UDP socket for datagram communication
pub struct UdpSocket {
    fd: i32
}

impl UdpSocket {
    /// Bind to local address
    pub fn bind(addr: SocketAddr) -> Result<UdpSocket, Error>

    /// Send datagram to address
    pub fn send_to(~self, buf: &Slice<u8>, addr: SocketAddr) -> Result<i64, Error>

    /// Receive datagram, returns (bytes_read, sender_addr)
    pub fn recv_from(~self, buf: *u8, len: i64) -> Result<(i64, SocketAddr), Error>

    /// Close the socket
    pub fn close(~self) -> Result<(), Error>
}
```

### Use Cases
- HTTP servers/clients
- Custom protocols
- UDP-based services (DNS, game networking)

---

## 3. Process Syscalls

### Raw Intrinsics

```vibe
// Get current process ID
sys_getpid() -> i32

// Get parent process ID
sys_getppid() -> i32

// Fork process (returns 0 in child, child pid in parent, -1 on error)
sys_fork() -> i32

// Execute program (replaces current process)
// argv and envp are null-terminated arrays of null-terminated strings
sys_execve(pathname: *u8, argv: **u8, envp: **u8) -> i32

// Wait for child process
// options: 0 (block), WNOHANG (1) for non-blocking
// Returns child pid, 0 (WNOHANG + no child ready), or -1 on error
sys_waitpid(pid: i32, status: *i32, options: i32) -> i32

// Exit process
sys_exit(status: i32) -> !

// Kill process with signal
sys_kill(pid: i32, sig: i32) -> i32

// Get current working directory
sys_getcwd(buf: *u8, size: i64) -> i64

// Change current working directory
sys_chdir(path: *u8) -> i32

// Get environment variable (libc wrapper)
sys_getenv(name: *u8) -> *u8

// Set environment variable
sys_setenv(name: *u8, value: *u8, overwrite: i32) -> i32
```

### Safe Abstractions: `std.process`

```vibe
// Common signals
pub static SIGTERM: i32 = 15
pub static SIGKILL: i32 = 9
pub static SIGINT: i32 = 2

/// Get current process ID
pub fn id() -> i32

/// Get parent process ID
pub fn parent_id() -> i32

/// Exit the current process
pub fn exit(code: i32) -> !

/// Represents a child process
pub struct Child {
    pid: i32
}

impl Child {
    /// Wait for the child to exit (blocks)
    pub fn wait(~self) -> Result<ExitStatus, Error>

    /// Check if child has exited (non-blocking)
    pub fn try_wait(~self) -> Result<Option<ExitStatus>, Error>

    /// Send signal to child
    pub fn kill(~self) -> Result<(), Error>

    /// Get the child's PID
    pub fn id(&self) -> i32
}

/// Exit status of a process
pub struct ExitStatus {
    code: i32
}

impl ExitStatus {
    pub fn success(&self) -> bool
    pub fn code(&self) -> i32
}

/// Builder for spawning processes
pub struct Command {
    program: String
    args: Vec<String>
    env: Vec<(String, String)>
    cwd: Option<String>
}

impl Command {
    /// Create new command for program
    pub fn new(program: &Slice<u8>) -> Command

    /// Add argument
    pub fn arg(~self, arg: &Slice<u8>) -> Command

    /// Add multiple arguments
    pub fn args(~self, args: &Slice<Slice<u8>>) -> Command

    /// Set environment variable
    pub fn env(~self, key: &Slice<u8>, val: &Slice<u8>) -> Command

    /// Set working directory
    pub fn current_dir(~self, dir: &Slice<u8>) -> Command

    /// Spawn the process
    pub fn spawn(~self) -> Result<Child, Error>

    /// Run and wait for completion
    pub fn run(~self) -> Result<ExitStatus, Error>

    /// Run and capture output
    pub fn output(~self) -> Result<Output, Error>
}

/// Captured output from a process
pub struct Output {
    status: ExitStatus
    stdout: Vec<u8>
    stderr: Vec<u8>
}

// Environment access
pub fn env_var(name: &Slice<u8>) -> Option<String>
pub fn set_env_var(name: &Slice<u8>, value: &Slice<u8>) -> Result<(), Error>
pub fn current_dir() -> Result<String, Error>
pub fn set_current_dir(path: &Slice<u8>) -> Result<(), Error>
```

### Use Cases
- Running external commands
- Build systems
- Shell-like utilities
- Daemon processes

---

## 4. Memory Syscalls

### Raw Intrinsics

```vibe
// Map memory region
// prot: PROT_READ (1) | PROT_WRITE (2) | PROT_EXEC (4)
// flags: MAP_PRIVATE (2) | MAP_ANONYMOUS (32)
// Returns pointer on success, MAP_FAILED (-1) on error
sys_mmap(addr: *u8, length: i64, prot: i32, flags: i32, fd: i32, offset: i64) -> *u8

// Unmap memory region
sys_munmap(addr: *u8, length: i64) -> i32

// Change memory protection
sys_mprotect(addr: *u8, length: i64, prot: i32) -> i32

// Advise kernel about memory usage patterns
// advice: MADV_NORMAL (0), MADV_SEQUENTIAL (2), MADV_WILLNEED (3), MADV_DONTNEED (4)
sys_madvise(addr: *u8, length: i64, advice: i32) -> i32
```

### Safe Abstractions: `std.mem.mmap`

```vibe
// Protection flags
pub static PROT_NONE: i32 = 0
pub static PROT_READ: i32 = 1
pub static PROT_WRITE: i32 = 2
pub static PROT_EXEC: i32 = 4

// Mapping flags
pub static MAP_PRIVATE: i32 = 2
pub static MAP_SHARED: i32 = 1
pub static MAP_ANONYMOUS: i32 = 32
pub static MAP_FIXED: i32 = 16

/// Memory protection level
pub enum Protection {
    ReadOnly
    ReadWrite
    ReadExecute
    ReadWriteExecute
}

/// A memory-mapped region
pub struct MappedMemory {
    ptr: *u8
    len: i64
}

impl MappedMemory {
    /// Allocate anonymous memory (like malloc but via mmap)
    pub fn anonymous(size: i64, prot: Protection) -> Result<MappedMemory, Error>

    /// Map a file into memory
    pub fn from_file(file: &File, offset: i64, len: i64, prot: Protection) -> Result<MappedMemory, Error>

    /// Get pointer to mapped region
    pub fn ptr(&self) -> *u8

    /// Get length of mapped region
    pub fn len(&self) -> i64

    /// Change protection of the region
    pub fn protect(~self, prot: Protection) -> Result<(), Error>

    /// Unmap the region (also happens on drop)
    pub fn unmap(~self) -> Result<(), Error>

    /// Access as slice
    pub fn as_slice(&self) -> Slice<u8>

    /// Access as mutable slice
    pub fn as_mut_slice(~self) -> Slice<u8>
}

// Convenience function for large allocations
pub fn alloc_pages(num_pages: i64) -> Result<MappedMemory, Error>
pub fn page_size() -> i64
```

### Use Cases
- Memory-mapped file I/O
- Large allocations without heap fragmentation
- JIT compilation (allocate executable memory)
- Shared memory between processes

---

## 5. Additional Utility Syscalls

### Raw Intrinsics

```vibe
// File system
sys_unlink(pathname: *u8) -> i32           // Delete file
sys_rename(oldpath: *u8, newpath: *u8) -> i32  // Rename file
sys_mkdir(pathname: *u8, mode: i32) -> i32  // Create directory
sys_rmdir(pathname: *u8) -> i32             // Remove directory
sys_stat(pathname: *u8, statbuf: *u8) -> i32  // Get file info
sys_fstat(fd: i32, statbuf: *u8) -> i32      // Get file info by fd

// Misc
sys_pipe(pipefd: *i32) -> i32   // Create pipe (returns two fds)
sys_dup(oldfd: i32) -> i32      // Duplicate fd
sys_dup2(oldfd: i32, newfd: i32) -> i32  // Duplicate to specific fd
```

### Safe Abstractions: `std.fs` (additions)

```vibe
/// File metadata
pub struct Metadata {
    size: i64
    is_file: bool
    is_dir: bool
    modified: SystemTime
    accessed: SystemTime
}

impl File {
    pub fn metadata(&self) -> Result<Metadata, Error>
}

// Module functions
pub fn remove_file(path: &Slice<u8>) -> Result<(), Error>
pub fn rename(from: &Slice<u8>, to: &Slice<u8>) -> Result<(), Error>
pub fn create_dir(path: &Slice<u8>) -> Result<(), Error>
pub fn remove_dir(path: &Slice<u8>) -> Result<(), Error>
pub fn metadata(path: &Slice<u8>) -> Result<Metadata, Error>
pub fn is_file(path: &Slice<u8>) -> bool
pub fn is_dir(path: &Slice<u8>) -> bool
```

---

## Implementation Order

1. **Time** (simplest, immediately useful)
   - `sys_clock_gettime`, `sys_nanosleep`
   - `std.time` module

2. **Process** (medium complexity)
   - `sys_getpid`, `sys_fork`, `sys_execve`, `sys_waitpid`, `sys_exit`
   - `std.process` module

3. **Memory** (medium complexity)
   - `sys_mmap`, `sys_munmap`, `sys_mprotect`
   - `std.mem.mmap` module

4. **Networking** (most complex, many syscalls)
   - Socket syscalls
   - `std.net` module

5. **Filesystem additions** (extend existing)
   - Additional fs syscalls
   - Extend `std.fs`

---

## Compiler Changes Required

For each raw syscall, add to `bootstrap/src/codegen/intrinsics.rs`:

1. Declare the libc function in LLVM
2. Parse arguments and validate types
3. Build the LLVM call instruction
4. Handle return value appropriately

Example pattern:
```rust
pub(crate) fn compile_sys_clock_gettime(&mut self, args: &[Expr]) -> Result<BasicValueEnum<'ctx>, CodegenError> {
    // 1. Validate argument count
    // 2. Compile each argument
    // 3. Get or declare the libc function
    // 4. Build call instruction
    // 5. Return result
}
```

---

## Testing Strategy

For each syscall category:

1. **Unit tests** in `tests/run/` - test raw syscalls work
2. **Integration tests** - test safe abstractions
3. **Examples** in `examples/` - demonstrate real use cases

Example test structure:
```
tests/run/time_syscalls.vibe
tests/run/process_syscalls.vibe
tests/run/net_syscalls.vibe
tests/run/mmap_syscalls.vibe
examples/http_server.vibe
examples/benchmark.vibe
examples/spawn_process.vibe
```
