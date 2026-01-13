# Section 3: Lexical Structure

## 3.1 Source Encoding

Vibelang source files MUST be encoded in UTF-8. The compiler MUST reject files with invalid UTF-8 sequences.

## 3.2 Whitespace and Comments

### Whitespace

Whitespace characters are space (U+0020), tab (U+0009), carriage return (U+000D), and newline (U+000A). Whitespace separates tokens but is otherwise ignored.

### Comments

```vibelang
// Line comment: extends to end of line

/* Block comment: can span
   multiple lines */

/// Documentation comment
/// Attaches to the following declaration
fn documented() { }
```

Block comments MAY be nested:

```vibelang
/* outer /* inner */ still outer */
```

## 3.3 Tokens

### 3.3.1 Keywords

The following identifiers are reserved as keywords:

```
const       let         static      fn
struct      enum        impl        trait
if          else        match       while
for         in          return      break
continue    true        false       and
or          not         self        Self
pub         use         mod         as
defer       move        mut
```

### 3.3.2 Identifiers

An identifier starts with a letter or underscore, followed by letters, digits, or underscores:

```
identifier = (letter | '_') {letter | digit | '_'}
letter     = 'a'..'z' | 'A'..'Z'
digit      = '0'..'9'
```

Examples:

```vibelang
foo
_bar
Token123
MAX_SIZE
__internal
```

Identifiers starting with `__` (double underscore) are reserved for the compiler.

### 3.3.3 Literals

#### Integer Literals

```
integer     = decimal | hex | octal | binary
decimal     = digit {digit | '_'}
hex         = '0x' hex_digit {hex_digit | '_'}
octal       = '0o' octal_digit {octal_digit | '_'}
binary      = '0b' bin_digit {bin_digit | '_'}
hex_digit   = digit | 'a'..'f' | 'A'..'F'
octal_digit = '0'..'7'
bin_digit   = '0' | '1'
```

Examples:

```vibelang
42
1_000_000
0xFF
0xff_ff
0o755
0b1010_1010
```

Integer literals may have a type suffix:

```vibelang
42i8        // i8
42i16       // i16
42i32       // i32 (default)
42i64       // i64
42u8        // u8
42u16       // u16
42u32       // u32
42u64       // u64
```

#### Float Literals

```
float       = decimal '.' decimal [exponent] | decimal exponent
exponent    = ('e' | 'E') ['+' | '-'] decimal
```

Examples:

```vibelang
3.14
3.14159265
1.0e10
2.5E-3
1e6
```

Float literals may have a type suffix:

```vibelang
3.14f32     // f32
3.14f64     // f64 (default)
```

#### String Literals

```
string      = '"' {string_char} '"'
string_char = <any UTF-8 except '"' and '\' and newline> | escape
escape      = '\' ('n' | 'r' | 't' | '\\' | '"' | '0' | 'x' hex hex)
```

Examples:

```vibelang
"hello"
"hello\nworld"
"tab\there"
"quote: \""
"null: \0"
"byte: \x1B"
```

#### Raw String Literals

Raw strings do not process escape sequences:

```
raw_string  = 'r"' {<any UTF-8 except '"'>} '"'
```

Examples:

```vibelang
r"no escape: \n is literal"
r"C:\path\to\file"
```

#### Character Literals

```
char        = '\'' char_content '\''
char_content = <any UTF-8 except '\'' and '\'> | escape
```

Examples:

```vibelang
'a'
'\n'
'\x41'      // 'A'
```

#### Boolean Literals

```vibelang
true
false
```

### 3.3.4 Operators and Punctuation

#### Arithmetic Operators

| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction / Negation |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |

#### Comparison Operators

| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

#### Logical Operators

| Operator | Description |
|----------|-------------|
| `and` | Logical AND |
| `or` | Logical OR |
| `not` | Logical NOT |

#### Bitwise Operators

| Operator | Description |
|----------|-------------|
| `&` | Bitwise AND / Read-only borrow |
| `\|` | Bitwise OR |
| `^` | Bitwise XOR |
| `!` | Bitwise NOT |
| `<<` | Left shift |
| `>>` | Right shift |

#### Reference Operators

| Operator | Description |
|----------|-------------|
| `&` | Read-only borrow |
| `~` | Mutable borrow |
| `*` | Dereference |

#### Assignment Operators

| Operator | Description |
|----------|-------------|
| `=` | Assignment |
| `+=` | Add and assign |
| `-=` | Subtract and assign |
| `*=` | Multiply and assign |
| `/=` | Divide and assign |
| `%=` | Modulo and assign |
| `&=` | Bitwise AND and assign |
| `\|=` | Bitwise OR and assign |
| `^=` | Bitwise XOR and assign |
| `<<=` | Left shift and assign |
| `>>=` | Right shift and assign |

#### Other Operators

| Operator | Description |
|----------|-------------|
| `->` | Return type arrow |
| `=>` | Match arm arrow |
| `.` | Member access |
| `..` | Range |
| `?` | Error propagation |

#### Punctuation

| Symbol | Description |
|--------|-------------|
| `(` `)` | Parentheses |
| `{` `}` | Braces |
| `[` `]` | Brackets |
| `<` `>` | Angle brackets (generics) |
| `,` | Comma |
| `:` | Colon (type annotation) |
| `;` | Semicolon (statement terminator) |

## 3.4 Grammar Overview

### 3.4.1 Program Structure

```
program     = {item}
item        = function | struct | enum | impl | static | use
```

### 3.4.2 Declarations

```
static_decl = 'static' identifier '=' expression
const_decl  = 'const' identifier ['=' type] '=' expression
let_decl    = 'let' identifier [':' type] '=' expression
```

### 3.4.3 Functions

```
function    = 'fn' identifier ['<' generics '>'] '(' [params] ')' ['->' type] block
params      = param {',' param}
param       = identifier ':' type
generics    = generic {',' generic}
generic     = identifier
```

### 3.4.4 Types

```
type        = primitive | array_type | generic_type | pointer_type | ref_type | path
primitive   = 'i8' | 'i16' | 'i32' | 'i64' | 'u8' | 'u16' | 'u32' | 'u64'
            | 'f32' | 'f64' | 'bool' | 'void'
array_type  = type '[' integer ']'
generic_type = identifier '<' type {',' type} '>' ['[' integer ']']
pointer_type = '*' type
ref_type    = '&' type
path        = identifier {'.' identifier}
```

### 3.4.5 Expressions

```
expression  = literal | identifier | binary_expr | unary_expr | call_expr
            | index_expr | field_expr | if_expr | match_expr | block
binary_expr = expression binop expression
unary_expr  = unop expression
call_expr   = expression '(' [args] ')'
index_expr  = expression '[' expression ']'
field_expr  = expression '.' identifier
args        = expression {',' expression}
```

### 3.4.6 Statements

```
statement   = let_decl | const_decl | expression | return_stmt | while_stmt
            | for_stmt | if_stmt | match_stmt | block
return_stmt = 'return' [expression]
while_stmt  = 'while' expression block
for_stmt    = 'for' identifier 'in' expression block
if_stmt     = 'if' expression block {'else' 'if' expression block} ['else' block]
match_stmt  = 'match' expression '{' {match_arm} '}'
match_arm   = pattern '=>' expression | block
block       = '{' {statement} '}'
```

### 3.4.7 Patterns

```
pattern     = literal | identifier | wildcard | enum_pattern | struct_pattern
wildcard    = '_'
enum_pattern = path ['(' pattern {',' pattern} ')']
struct_pattern = path '{' [field_pattern {',' field_pattern}] '}'
field_pattern = identifier [':' pattern]
```

## 3.5 Operator Precedence

From highest to lowest precedence:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 | `.` `()` `[]` | Left |
| 2 | Unary `-` `not` `~` `&` `*` | Right |
| 3 | `*` `/` `%` | Left |
| 4 | `+` `-` | Left |
| 5 | `<<` `>>` | Left |
| 6 | `&` | Left |
| 7 | `^` | Left |
| 8 | `\|` | Left |
| 9 | `==` `!=` `<` `<=` `>` `>=` | Left |
| 10 | `and` | Left |
| 11 | `or` | Left |
| 12 | `=` `+=` `-=` etc. | Right |
