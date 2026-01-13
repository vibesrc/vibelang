# Section 9: Functions

Functions are the primary unit of code organization in Vibelang.

## 9.1 Function Declaration

### Basic Syntax

```vibelang
fn function_name(param1: Type1, param2: Type2) -> ReturnType {
    // body
}
```

### Simple Functions

```vibelang
fn add(a: i32, b: i32) -> i32 {
    return a + b
}

fn greet(name: &String) {
    print("Hello, ")
    print(name)
}

fn get_pi() -> f64 {
    return 3.14159265358979
}
```

### No Return Type

Functions without `->` return `void`:

```vibelang
fn say_hello() {
    print("Hello!")
}

// Equivalent to:
fn say_hello() -> void {
    print("Hello!")
}
```

## 9.2 Parameters

### By Value (Owned)

```vibelang
fn consume(s: String) {
    // Takes ownership of s
    print(&s)
}   // s is freed here

fn main() {
    let msg = String.from("hello")
    consume(msg)        // msg moved
    // msg no longer valid
}
```

### By Reference (Borrowed)

```vibelang
fn inspect(s: &String) {
    // Borrows s, doesn't own it
    print(s)
}   // s NOT freed (we don't own it)

fn main() {
    let msg = String.from("hello")
    inspect(&msg)       // msg borrowed
    inspect(&msg)       // can borrow again
    // msg still valid
}
```

### Copy Types

Copy types are implicitly copied:

```vibelang
fn double(x: i32) -> i32 {
    return x * 2
}

fn main() {
    let n = 21
    let result = double(n)  // n is copied
    print_int(n)            // n still valid (42)
}
```

### Multiple Parameters

```vibelang
fn clamp(value: i32, min: i32, max: i32) -> i32 {
    if value < min {
        return min
    }
    if value > max {
        return max
    }
    return value
}
```

## 9.3 Return Values

### Single Return

```vibelang
fn square(x: i32) -> i32 {
    return x * x
}
```

### Multiple Returns via Tuple

```vibelang
fn div_rem(a: i32, b: i32) -> (i32, i32) {
    return (a / b, a % b)
}

fn main() {
    let (quotient, remainder) = div_rem(17, 5)
    // quotient = 3, remainder = 2
}
```

### Returning Owned Values

```vibelang
fn create_greeting(name: &String) -> String {
    let result = String.from("Hello, ")
    result.push(name)
    result.push("!")
    return result       // ownership transfers to caller
}
```

### Returning References

```vibelang
fn first(arr: &Array<i32>) -> Option<&i32> {
    if arr.is_empty() {
        return Option.None
    }
    return Option.Some(&arr[0])
}
```

The returned reference cannot outlive the input reference.

## 9.4 Generic Functions

### Type Parameters

```vibelang
fn identity<T>(x: T) -> T {
    return x
}

fn swap<T>(a: &T, b: &T) {
    let temp = *a
    *a = *b
    *b = temp
}
```

### Multiple Type Parameters

```vibelang
fn pair<A, B>(a: A, b: B) -> (A, B) {
    return (a, b)
}

fn map<T, U>(opt: Option<T>, f: fn(T) -> U) -> Option<U> {
    return match opt {
        Option.Some(x) => Option.Some(f(x))
        Option.None => Option.None
    }
}
```

### Calling Generic Functions

```vibelang
// Type inference
let x = identity(42)        // T = i32
let y = identity("hello")   // T = Slice<u8>

// Explicit type
let z = identity<f64>(3.14)
```

## 9.5 Methods

### Associated Functions

Functions in `impl` blocks:

```vibelang
struct Point {
    x: f64
    y: f64
}

impl Point {
    // Associated function (no self)
    fn new(x: f64, y: f64) -> Point {
        return Point { x: x, y: y }
    }
    
    // Associated function
    fn origin() -> Point {
        return Point { x: 0.0, y: 0.0 }
    }
}

// Called as:
let p = Point.new(1.0, 2.0)
let o = Point.origin()
```

### Instance Methods

Methods with `self`:

```vibelang
impl Point {
    // Borrows self (read-only)
    fn length(self: &Point) -> f64 {
        return sqrt(self.x * self.x + self.y * self.y)
    }
    
    // Borrows self (mutable)
    fn scale(self: ~Point, factor: f64) {
        self.x *= factor
        self.y *= factor
    }
    
    // Consumes self
    fn into_tuple(self: Point) -> (f64, f64) {
        return (self.x, self.y)
    }
}

// Called as:
let p = Point.new(3.0, 4.0)
let len = p.length()        // 5.0
p.scale(2.0)                // p is now (6.0, 8.0)
let tuple = p.into_tuple()  // p is consumed
```

### Self Type

Inside `impl`, `Self` refers to the implementing type:

```vibelang
impl Point {
    fn clone(self: &Self) -> Self {
        return Point { x: self.x, y: self.y }
    }
}
```

## 9.6 Function Types

### Function Pointers

```vibelang
fn add(a: i32, b: i32) -> i32 { return a + b }
fn mul(a: i32, b: i32) -> i32 { return a * b }

let op: fn(i32, i32) -> i32 = add
let result = op(2, 3)       // 5

op = mul
let result = op(2, 3)       // 6
```

### Functions as Parameters

```vibelang
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    return f(x)
}

fn double(x: i32) -> i32 { return x * 2 }

let result = apply(double, 21)  // 42
```

### Higher-Order Functions

```vibelang
fn map<T, U>(arr: &Array<T>, f: fn(&T) -> U) -> Array<U> {
    let result = Array<U>()
    for item in arr {
        result.push(f(item))
    }
    return result
}

fn filter<T>(arr: &Array<T>, pred: fn(&T) -> bool) -> Array<T> {
    let result = Array<T>()
    for item in arr {
        if pred(item) {
            result.push(item.copy())
        }
    }
    return result
}

fn fold<T, A>(arr: &Array<T>, init: A, f: fn(A, &T) -> A) -> A {
    let acc = init
    for item in arr {
        acc = f(acc, item)
    }
    return acc
}
```

## 9.7 Closures

### Anonymous Functions

```vibelang
let add = fn(a: i32, b: i32) -> i32 { return a + b }
let result = add(2, 3)
```

### Closures with Captures

```vibelang
let multiplier = 3
let scale = fn(x: i32) -> i32 { return x * multiplier }

scale(10)   // 30
```

### Capture Modes

By default, closures capture by reference:

```vibelang
let data = String.from("hello")
let printer = fn() { print(&data) }

printer()           // prints "hello"
data.push("!")
printer()           // prints "hello!"
```

Use `move` to capture by value:

```vibelang
let data = String.from("hello")
let printer = move fn() { print(&data) }

// data moved into closure
// data no longer valid here
```

### Closure Type Inference

```vibelang
// Types inferred from usage
let nums = [1, 2, 3, 4, 5]
let doubled = nums.map(fn(x) { *x * 2 })        // x: &i32 inferred
let evens = nums.filter(fn(x) { *x % 2 == 0 })  // x: &i32, returns bool
```

## 9.8 Recursion

### Direct Recursion

```vibelang
fn factorial(n: u64) -> u64 {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

fn fib(n: u64) -> u64 {
    if n <= 1 {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}
```

### Tail Recursion

The compiler MAY optimize tail calls:

```vibelang
fn factorial_tail(n: u64, acc: u64) -> u64 {
    if n <= 1 {
        return acc
    }
    return factorial_tail(n - 1, n * acc)   // tail call
}

fn factorial(n: u64) -> u64 {
    return factorial_tail(n, 1)
}
```

## 9.9 Visibility

### Public Functions

```vibelang
pub fn public_api() {
    // accessible from other modules
}

fn private_helper() {
    // only accessible within this module
}
```

### Public Methods

```vibelang
impl MyType {
    pub fn public_method(self: &Self) { }
    
    fn private_method(self: &Self) { }
}
```

## 9.10 Special Functions

### Main Function

Entry point of the program:

```vibelang
fn main() {
    // program starts here
}

// Or with return code:
fn main() -> i32 {
    return 0
}

// Or with Result:
fn main() -> Result<void, Error> {
    // ...
    return Result.Ok(void)
}
```

### Drop Function

Called when a value is freed:

```vibelang
impl MyResource {
    fn drop(self: Self) {
        // cleanup code
        release_handle(self.handle)
    }
}
```

## 9.11 Function Examples

### Complete Example

```vibelang
struct Parser {
    source: String
    pos: u64
    tokens: Array<Token>
}

impl Parser {
    pub fn new(source: String) -> Parser {
        return Parser {
            source: source,
            pos: 0,
            tokens: Array<Token>()
        }
    }
    
    pub fn parse(self: &Parser) -> Result<Ast, ParseError> {
        self.tokenize()?
        return self.build_ast()
    }
    
    fn tokenize(self: &Parser) -> Result<void, ParseError> {
        while self.pos < self.source.len() {
            let token = self.next_token()?
            self.tokens.push(token)
        }
        return Result.Ok(void)
    }
    
    fn next_token(self: &Parser) -> Result<Token, ParseError> {
        self.skip_whitespace()
        
        if self.pos >= self.source.len() {
            return Result.Ok(Token.eof())
        }
        
        const c = self.current_char()
        
        return match c {
            '(' => self.single_char_token(TokenKind.LParen)
            ')' => self.single_char_token(TokenKind.RParen)
            _ if is_alpha(c) => self.identifier()
            _ if is_digit(c) => self.number()
            _ => Result.Err(ParseError.unexpected_char(c, self.pos))
        }
    }
    
    fn current_char(self: &Parser) -> u8 {
        return self.source.byte_at(self.pos)
    }
    
    fn skip_whitespace(self: &Parser) {
        while self.pos < self.source.len() and is_whitespace(self.current_char()) {
            self.pos += 1
        }
    }
    
    fn single_char_token(self: &Parser, kind: TokenKind) -> Result<Token, ParseError> {
        const token = Token {
            kind: kind,
            lexeme: self.source.slice(self.pos, self.pos + 1),
            pos: self.pos
        }
        self.pos += 1
        return Result.Ok(token)
    }
    
    fn identifier(self: &Parser) -> Result<Token, ParseError> {
        const start = self.pos
        while self.pos < self.source.len() and is_alnum(self.current_char()) {
            self.pos += 1
        }
        return Result.Ok(Token {
            kind: TokenKind.Ident,
            lexeme: self.source.slice(start, self.pos),
            pos: start
        })
    }
    
    fn number(self: &Parser) -> Result<Token, ParseError> {
        const start = self.pos
        while self.pos < self.source.len() and is_digit(self.current_char()) {
            self.pos += 1
        }
        return Result.Ok(Token {
            kind: TokenKind.Number,
            lexeme: self.source.slice(start, self.pos),
            pos: start
        })
    }
    
    fn build_ast(self: &Parser) -> Result<Ast, ParseError> {
        // ... AST construction ...
    }
}
```
