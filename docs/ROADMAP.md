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

## In Progress

## Planned
- Tuple types `(T1, T2, ...)` with `.0`, `.1` access and destructuring in `let`
- Fieldsets and struct tagging
