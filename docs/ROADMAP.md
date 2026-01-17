## Completed
- `char` type for character literals (`'c'` syntax) - just a `u8` under the hood
- Type casting syntax (`value as Type`)
- `Map<K, V>` hash map in `std.collections.map`
- `Set<T>` hash set in `std.collections.set`
- `std.hash` module with FNV-1a hash functions for primitives
- Bitwise XOR (`^`) and shift operators (`<<`, `>>`)
- Networking syscalls with `std.net` (TcpListener, TcpStream, UdpSocket, Ipv4Addr, SocketAddrV4)
- Process control syscalls with `std.process` (fork, execve, waitpid, kill, exit, getpid, getenv, setenv)
- Memory syscalls with `std.mem` (mmap, munmap, mprotect, madvise, alloc, resize, dealloc)
- Tuple types `(T1, T2, ...)` with `.0`, `.1` access and destructuring in `let`
- Struct destructuring: `let {x, y} = point`, `let {x: px, y: py} = point`

## In Progress
- Move semantics for struct/tuple destructuring (source marked as moved)

## Planned
- Attributes and macros (`@` decorators, `!` function-like macros) - see `docs/proposals/macros.md`
  - Phase 1: Attribute parsing (`@name`, `@name(...)`)
  - Phase 2: Built-in derives (`@derive(Clone, Debug)`)
  - Phase 3: Compiler attributes (`@test`, `@cfg`, `@repr`)
  - Phase 4: User-defined macros (`@macro_derive`, `@macro_attribute`, `@macro`)
  - Phase 5: Declarative macros (`@macro_rules`)
- Copy/Clone traits (after macros: `@derive(Copy)`, `@derive(Clone)`)
- Fieldsets and struct tagging
