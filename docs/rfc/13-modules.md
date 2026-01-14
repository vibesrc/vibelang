# Section 13: Module System

Vibelang uses a clean, prefix-based module system that provides clear semantics for different import sources.

## 13.1 Core Concepts

### Every File is a Module

Each `.vibe` file is automatically a module. The filename (without extension) is the module name.

```
src/
├── main.vibe        // main module
├── utils.vibe       // utils module
└── parser/
    ├── mod.vibe     // parser module (directory module)
    └── lexer.vibe   // parser.lexer submodule
```

### Visibility

Items are private by default. Use `pub` to export:

```vibelang
// utils.vibe
fn internal_helper() { }      // private - not accessible from other files

pub fn to_string(x: i32) -> Slice<u8> {   // public - can be imported
    // ...
}

pub struct Config {           // public struct
    pub name: Slice<u8>       // public field
    secret: Slice<u8>         // private field
}
```

### Project Root

The project root is the directory containing `vibe.toml`. This file defines the project:

```toml
[project]
name = "myapp"
version = "0.1.0"

[dependencies]
postgres = { path = "../postgres-vibe" }
json = { version = "1.0" }
```

## 13.2 Import Prefixes

All imports use explicit prefixes that specify where to find the module:

| Prefix | Resolves to |
|--------|-------------|
| `use src.foo` | `<project_root>/src/foo.vibe` |
| `use lib.pkg.foo` | `<project_root>/lib/pkg/src/foo.vibe` |
| `use dep.pkg.foo` | `<project_root>/dep/pkg/src/foo.vibe` |
| `use std.foo` | Standard library |
| `use .foo` | Relative to current file's directory |

### src. - Absolute Project Imports

For project code, use `src.` to import from the project's `src/` directory:

```vibelang
// src/handlers/user.vibe
use src.utils.{format, validate}     // src/utils.vibe
use src.models.User                  // src/models.vibe
use src.config.Config                // src/config.vibe
```

This works from anywhere in the project - the path is always relative to `src/`.

### lib. - Local Workspace Packages

For monorepo/workspace style projects, use `lib.` to import from local packages in the `lib/` directory:

```
myproject/
├── vibe.toml
├── src/
│   └── main.vibe
└── lib/
    ├── db/
    │   ├── vibe.toml
    │   └── src/
    │       └── lib.vibe
    └── parser/
        ├── vibe.toml
        └── src/
            ├── lib.vibe
            └── sql.vibe
```

```vibelang
use lib.db.{connect, query}          // lib/db/src/lib.vibe
use lib.parser.sql.{build}           // lib/parser/src/sql.vibe
```

lib. packages are your code - managed via git clone, submodule, or manual copy.

### dep. - Vendored Dependencies

For vendored external dependencies, use `dep.` to import from the `dep/` directory:

```
myproject/
├── vibe.toml
├── src/
│   └── main.vibe
└── dep/
    └── postgres/
        └── src/
            └── lib.vibe
```

```vibelang
use dep.postgres.{connect, query, Connection}  // dep/postgres/src/lib.vibe
```

dep. packages are external code - managed by `vibe get` or similar tooling.

### std. - Standard Library

Import standard library modules with `std.`:

```vibelang
use std.collections.{Array, Map}
use std.io.{read_file, write_file}
use std.fmt.format
```

### . - Relative Imports

For files in the same directory or subdirectories, use `.`:

```vibelang
// src/utils/format.vibe
use .json.{to_json, from_json}       // src/utils/json.vibe
use .helpers.trim                    // src/utils/helpers.vibe
```

## 13.3 Import Syntax

### Single Item Import

```vibelang
use src.utils.to_string              // import single item
```

### Grouped Imports

Import multiple items with braces:

```vibelang
use src.utils.{to_string, to_int, trim}
use std.collections.{Array, Map, Set}
```

### Aliased Imports

Rename imports with `as`:

```vibelang
use src.utils.to_string as stringify
use dep.postgres.Connection as PgConn
use src.models.{User as UserModel, Post as PostModel}
```

### Glob Imports

Import all public items with `*`:

```vibelang
use src.utils.*                      // import all public items from utils
use std.prelude.*                    // import all prelude items
```

## 13.4 Re-exports with mod.vibe

For directory modules, `mod.vibe` can re-export items from submodules:

```vibelang
// src/utils/mod.vibe
pub use .format.{to_json, to_xml}    // re-export from submodules
pub use .parse.{parse_int, parse_float}
```

Consumers can then import from the parent:

```vibelang
use src.utils.{to_json, parse_int}   // instead of src.utils.format.to_json
```

## 13.5 Single File Mode

When compiling a single file without `vibe.toml`:

- `use std.*` works if VIBELANG_STDLIB is set
- `use .*` works for relative imports
- `use src.*` errors: "src. imports require a vibe.toml project"
- `use dep.*` errors: "dep. imports require vibe.toml [dep] section"

This ensures clear error messages when project configuration is missing.

## 13.6 Module Resolution

### File vs Directory Modules

For `use src.utils.format`:

1. First try: `src/utils/format.vibe`
2. Fallback: `src/utils/format/mod.vibe`

### Resolution Examples

```
Project structure:
myproject/
├── vibe.toml
├── src/
│   ├── main.vibe
│   ├── config.vibe
│   └── utils/
│       ├── mod.vibe
│       ├── format.vibe
│       └── parse.vibe
└── deps/
    └── json/
        └── lib.vibe

From src/main.vibe:
  use src.config.Config           -> src/config.vibe
  use src.utils.format.to_json    -> src/utils/format.vibe
  use .config.Config              -> src/config.vibe (relative)

From src/utils/format.vibe:
  use .parse.parse_int            -> src/utils/parse.vibe
  use src.config.Config           -> src/config.vibe
```

## 13.7 Visibility Rules

### Private by Default

```vibelang
// math.vibe
fn helper(x: i32) -> i32 { x * 2 }   // private
pub fn double(x: i32) -> i32 {       // public
    helper(x)
}
```

Attempting to import a private item produces an error:

```
error: 'helper' is not public in module 'math'
```

### Public Fields

Struct fields default to private:

```vibelang
pub struct User {
    pub name: Slice<u8>    // accessible
    pub email: Slice<u8>   // accessible
    password_hash: Slice<u8>  // private - only accessible in this file
}
```

## 13.8 Circular Dependencies

Circular module dependencies are not allowed and produce a compile error:

```vibelang
// a.vibe
use .b.Foo    // a depends on b

// b.vibe
use .a.Bar    // ERROR: circular dependency: a -> b -> a
```

### Breaking Cycles

Extract shared types to a third module:

```vibelang
// types.vibe
pub struct SharedType { }

// a.vibe
use .types.SharedType

// b.vibe
use .types.SharedType
```

## 13.9 Best Practices

### Prefer Absolute Imports

Use `src.` for clarity about where code comes from:

```vibelang
// Good - clear origin
use src.models.User
use src.handlers.auth

// Less clear - requires knowing current file location
use .User
```

### Minimal Public API

Export only what's needed:

```vibelang
// Good - minimal public surface
pub fn create_user(name: Slice<u8>) -> User { }
fn validate_name(name: Slice<u8>) -> bool { }  // internal

// Bad - exposes implementation details
pub fn validate_name(name: Slice<u8>) -> bool { }
```

### Group Related Exports

Use `mod.vibe` to provide clean public APIs:

```vibelang
// src/http/mod.vibe
pub use .request.Request
pub use .response.Response
pub use .server.{serve, Server}

// Clean import for consumers:
use src.http.{Request, Response, serve}
```

## 13.10 Prelude

Some items are automatically available without import:

```vibelang
// Always available:
Option
Option.Some
Option.None
Result
Result.Ok
Result.Err
Array
Slice
String
// ... basic types (i32, f64, bool, etc.)
```

## 13.11 Error Messages

The module system provides clear error messages:

```
error: src. imports require a vibe.toml project
  --> file.vibe:1:1
  |
1 | use src.utils.format
  | ^^^^^^^^^^^^^^^^^^^^
  |
  = help: create a vibe.toml in your project root

error: dependency 'postgres' not found in vibe.toml [dep] section
  --> main.vibe:3:1
  |
3 | use dep.postgres.{connect, query}
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |
  = help: add to vibe.toml:
    [dependencies]
    postgres = { path = "..." }

error: 'internal_fn' is not public in module 'utils'
  --> main.vibe:5:1
  |
5 | use src.utils.internal_fn
  | ^^^^^^^^^^^^^^^^^^^^^^^^^
  |
  = help: add 'pub' to export: pub fn internal_fn() { }

error: module 'nonexistent' not found
  --> main.vibe:7:1
  |
7 | use src.nonexistent.foo
  | ^^^^^^^^^^^^^^^^^^^^^^^
  |
  = help: tried: src/nonexistent.vibe, src/nonexistent/mod.vibe
```
