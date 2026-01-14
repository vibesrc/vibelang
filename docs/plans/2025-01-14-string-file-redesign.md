# String and File Redesign

## Overview

Redesign `std.string.String` and add `std.fs.File` with consistent, chainable APIs that work together. File operations return String where appropriate.

## String (Redesigned)

```vibe
pub struct String {
    data: Array<u8>
}

impl String {
    // === Constructors ===
    pub fn new() -> String
    pub fn with_capacity(cap: i64) -> String
    pub fn from(s: &Slice<u8>) -> String

    // === Properties ===
    pub fn len(&self) -> i64
    pub fn capacity(&self) -> i64
    pub fn is_empty(&self) -> bool
    pub fn as_bytes(&self) -> Slice<u8>

    // === Mutation (return String for chaining) ===
    pub fn push(~self, byte: u8) -> String
    pub fn push_str(~self, s: &Slice<u8>) -> String
    pub fn clear(~self) -> String

    // === Transformations (return new String) ===
    pub fn trim(&self) -> String
    pub fn trim_start(&self) -> String
    pub fn trim_end(&self) -> String
    pub fn to_lowercase(&self) -> String
    pub fn to_uppercase(&self) -> String
    pub fn replace(&self, from: &Slice<u8>, to: &Slice<u8>) -> String
    pub fn repeat(&self, n: i64) -> String

    // === Queries ===
    pub fn contains(&self, needle: &Slice<u8>) -> bool
    pub fn starts_with(&self, prefix: &Slice<u8>) -> bool
    pub fn ends_with(&self, suffix: &Slice<u8>) -> bool
    pub fn find(&self, needle: &Slice<u8>) -> Option<i64>

    // === Splitting ===
    pub fn split(&self, delim: &Slice<u8>) -> Array<String>
    pub fn lines(&self) -> Array<String>

    // === Joining (static) ===
    pub fn join(parts: &Array<String>, delim: &Slice<u8>) -> String
}
```

## File (New)

Uses raw file descriptors with safe wrapper.

```vibe
pub struct File {
    fd: i32
    is_closed: bool
}

pub enum OpenMode {
    Read       // O_RDONLY
    Write      // O_WRONLY
    ReadWrite  // O_RDWR
}

pub struct OpenFlags {
    pub create: bool    // O_CREAT
    pub truncate: bool  // O_TRUNC
    pub append: bool    // O_APPEND
}

pub enum SeekFrom {
    Start      // SEEK_SET
    Current    // SEEK_CUR
    End        // SEEK_END
}

impl File {
    // === Opening ===
    pub fn open(path: &Slice<u8>, mode: OpenMode, flags: OpenFlags) -> Result<File, Error>

    // Convenience constructors
    pub fn read(path: &Slice<u8>) -> Result<File, Error>      // open read-only
    pub fn create(path: &Slice<u8>) -> Result<File, Error>    // write + create + truncate
    pub fn append(path: &Slice<u8>) -> Result<File, Error>    // write + create + append

    // === Core I/O (buffer-based for performance) ===
    pub fn read_bytes(~self, buf: ~Array<u8>, count: i64) -> Result<i64, Error>
    pub fn write_bytes(~self, buf: &Slice<u8>) -> Result<i64, Error>
    pub fn seek(~self, offset: i64, whence: SeekFrom) -> Result<i64, Error>
    pub fn close(~self) -> Result<void, Error>

    // === Convenience (allocates, returns String) ===
    pub fn read_all(~self) -> Result<String, Error>
    pub fn read_line(~self) -> Result<Option<String>, Error>
    pub fn read_n(~self, count: i64) -> Result<Array<u8>, Error>  // allocates buffer

    // === Queries ===
    pub fn is_closed(&self) -> bool
}

// Module-level convenience (built on File)
pub fn read_file(path: &Slice<u8>) -> Result<String, Error>
pub fn write_file(path: &Slice<u8>, content: &Slice<u8>) -> Result<void, Error>
pub fn exists(path: &Slice<u8>) -> bool
```

## Intrinsics Required

New syscall wrappers in `codegen/intrinsics.rs`:

```rust
// File operations (Unix syscalls)
fn compile_open_call()   // open(path, flags, mode) -> fd
fn compile_close_call()  // close(fd) -> result
fn compile_read_call()   // read(fd, buf, count) -> bytes_read
fn compile_write_call()  // write(fd, buf, count) -> bytes_written
fn compile_lseek_call()  // lseek(fd, offset, whence) -> position
```

Declare in `declare_intrinsics()`:
```rust
// int open(const char *pathname, int flags, mode_t mode)
// int close(int fd)
// ssize_t read(int fd, void *buf, size_t count)
// ssize_t write(int fd, const void *buf, size_t count)
// off_t lseek(int fd, off_t offset, int whence)
```

## Implementation Plan

1. **Compiler intrinsics** - Add syscall wrappers for open/close/read/write/lseek
2. **std.fs.File** - Implement File struct with safe wrappers around intrinsics
3. **std.string.String** - Refactor to method-based API with chaining
4. **Integration** - File.read_all() returns String, etc.
5. **Tests** - File I/O tests, String method tests

## Usage Examples

```vibe
// Read file, process lines
let file = File.read("data.txt")?
let content = file.read_all()?
file.close()?

for line in content.lines() {
    let trimmed = line.trim()
    if not trimmed.is_empty() {
        println(trimmed.as_bytes())
    }
}

// Write to file
let file = File.create("output.txt")?
file.write_bytes("Hello, world!\n")?
file.close()?

// Device files work too
let random = File.open("/dev/urandom", OpenMode.Read, OpenFlags {})?
let bytes = random.read_n(32)?
random.close()?
```
