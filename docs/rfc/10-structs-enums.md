# Section 10: Structs and Enums

User-defined types are the building blocks for complex data structures.

## 10.1 Struct Declaration

### Basic Struct

```vibelang
struct Point {
    x: f64
    y: f64
}

struct Person {
    name: String
    age: u32
    email: String
}
```

### Generic Structs

```vibelang
struct Pair<T, U> {
    first: T
    second: U
}

struct Node<T> {
    value: T
    next: Option<*Node<T>>
}
```

### Tuple Structs

```vibelang
struct Color(u8, u8, u8)
struct Meters(f64)
struct UserId(u64)
```

### Unit Struct

```vibelang
struct Marker
```

## 10.2 Struct Instantiation

### Named Fields

```vibelang
let p = Point { x: 1.0, y: 2.0 }

let person = Person {
    name: String.from("Alice"),
    age: 30,
    email: String.from("alice@example.com")
}
```

### Field Shorthand

When variable name matches field name:

```vibelang
let x = 1.0
let y = 2.0
let p = Point { x, y }      // equivalent to { x: x, y: y }
```

### Struct Update Syntax

Create a new struct based on an existing one:

```vibelang
let p1 = Point { x: 1.0, y: 2.0 }
let p2 = Point { x: 3.0, ..p1 }     // y comes from p1

let person2 = Person {
    name: String.from("Bob"),
    ..person                         // age and email from person
}
```

### Tuple Struct Instantiation

```vibelang
let red = Color(255, 0, 0)
let distance = Meters(100.5)
let id = UserId(12345)
```

## 10.3 Field Access

### Reading Fields

```vibelang
let p = Point { x: 1.0, y: 2.0 }
let x_val = p.x                     // 1.0
let y_val = p.y                     // 2.0
```

### Modifying Fields

```vibelang
let p = Point { x: 1.0, y: 2.0 }
p.x = 3.0
p.y = 4.0
```

### Nested Field Access

```vibelang
struct Rectangle {
    top_left: Point
    bottom_right: Point
}

let rect = Rectangle {
    top_left: Point { x: 0.0, y: 10.0 },
    bottom_right: Point { x: 10.0, y: 0.0 }
}

let x = rect.top_left.x             // 0.0
```

### Tuple Struct Field Access

```vibelang
let color = Color(255, 128, 64)
let r = color.0                     // 255
let g = color.1                     // 128
let b = color.2                     // 64
```

## 10.4 Struct Methods

### Impl Blocks

```vibelang
impl Point {
    fn new(x: f64, y: f64) -> Point {
        return Point { x, y }
    }
    
    fn origin() -> Point {
        return Point { x: 0.0, y: 0.0 }
    }
    
    fn length(self: &Point) -> f64 {
        return sqrt(self.x * self.x + self.y * self.y)
    }
    
    fn distance(self: &Point, other: &Point) -> f64 {
        let dx = self.x - other.x
        let dy = self.y - other.y
        return sqrt(dx * dx + dy * dy)
    }
    
    fn translate(self: &Point, dx: f64, dy: f64) {
        self.x += dx
        self.y += dy
    }
    
    fn scale(self: &Point, factor: f64) {
        self.x *= factor
        self.y *= factor
    }
}
```

### Multiple Impl Blocks

```vibelang
impl Point {
    fn new(x: f64, y: f64) -> Point { ... }
}

impl Point {
    fn length(self: &Point) -> f64 { ... }
}
```

### Generic Impl Blocks

```vibelang
impl<T, U> Pair<T, U> {
    fn new(first: T, second: U) -> Pair<T, U> {
        return Pair { first, second }
    }
    
    fn swap(self: Pair<T, U>) -> Pair<U, T> {
        return Pair { first: self.second, second: self.first }
    }
}
```

## 10.5 Enum Declaration

### Simple Enum

```vibelang
enum Direction {
    North
    South
    East
    West
}

enum Color {
    Red
    Green
    Blue
}
```

### Enum with Data

```vibelang
enum Option<T> {
    Some(T)
    None
}

enum Result<T, E> {
    Ok(T)
    Err(E)
}

enum Token {
    Ident(String)
    Number(i64)
    Float(f64)
    String(String)
    Plus
    Minus
    Star
    Slash
    LParen
    RParen
    Eof
}
```

### Enum with Named Fields

```vibelang
enum Shape {
    Circle { radius: f64 }
    Rectangle { width: f64, height: f64 }
    Triangle { a: f64, b: f64, c: f64 }
}
```

### Generic Enums

```vibelang
enum Either<L, R> {
    Left(L)
    Right(R)
}

enum Tree<T> {
    Leaf(T)
    Node { left: *Tree<T>, right: *Tree<T> }
}
```

## 10.6 Enum Instantiation

### Simple Variants

```vibelang
let dir = Direction.North
let color = Color.Red
```

### Variants with Data

```vibelang
let some = Option.Some(42)
let none: Option<i32> = Option.None

let ok: Result<i32, String> = Result.Ok(42)
let err: Result<i32, String> = Result.Err(String.from("error"))

let token = Token.Number(42)
let ident = Token.Ident(String.from("foo"))
```

### Variants with Named Fields

```vibelang
let circle = Shape.Circle { radius: 5.0 }
let rect = Shape.Rectangle { width: 10.0, height: 20.0 }
```

## 10.7 Enum Methods

```vibelang
impl<T> Option<T> {
    fn is_some(self: &Option<T>) -> bool {
        return match self {
            Option.Some(_) => true
            Option.None => false
        }
    }
    
    fn is_none(self: &Option<T>) -> bool {
        return not self.is_some()
    }
    
    fn unwrap(self: Option<T>) -> T {
        return match self {
            Option.Some(x) => x
            Option.None => panic("unwrap on None")
        }
    }
    
    fn unwrap_or(self: Option<T>, default: T) -> T {
        return match self {
            Option.Some(x) => x
            Option.None => default
        }
    }
    
    fn map<U>(self: Option<T>, f: fn(T) -> U) -> Option<U> {
        return match self {
            Option.Some(x) => Option.Some(f(x))
            Option.None => Option.None
        }
    }
}
```

## 10.8 Pattern Matching on Structs

### Destructuring

```vibelang
let p = Point { x: 1.0, y: 2.0 }
let Point { x, y } = p
// x = 1.0, y = 2.0

let Point { x: a, y: b } = p
// a = 1.0, b = 2.0
```

### Partial Destructuring

```vibelang
let Point { x, .. } = p     // only extract x
```

### In Match

```vibelang
fn describe_point(p: &Point) -> String {
    return match p {
        Point { x: 0.0, y: 0.0 } => String.from("origin")
        Point { x: 0.0, y } => format("on y-axis at {}", y)
        Point { x, y: 0.0 } => format("on x-axis at {}", x)
        Point { x, y } => format("at ({}, {})", x, y)
    }
}
```

## 10.9 Pattern Matching on Enums

### Simple Matching

```vibelang
fn describe_direction(dir: Direction) -> String {
    return match dir {
        Direction.North => String.from("up")
        Direction.South => String.from("down")
        Direction.East => String.from("right")
        Direction.West => String.from("left")
    }
}
```

### Matching with Data

```vibelang
fn get_value(opt: Option<i32>) -> i32 {
    return match opt {
        Option.Some(x) => x
        Option.None => 0
    }
}

fn describe_token(token: &Token) -> String {
    return match token {
        Token.Ident(name) => format("identifier: {}", name)
        Token.Number(n) => format("number: {}", n)
        Token.Float(f) => format("float: {}", f)
        Token.Plus => String.from("plus")
        Token.Minus => String.from("minus")
        _ => String.from("other")
    }
}
```

### Matching Named Fields

```vibelang
fn area(shape: &Shape) -> f64 {
    return match shape {
        Shape.Circle { radius } => 3.14159 * radius * radius
        Shape.Rectangle { width, height } => width * height
        Shape.Triangle { a, b, c } => {
            // Heron's formula
            let s = (a + b + c) / 2.0
            sqrt(s * (s - a) * (s - b) * (s - c))
        }
    }
}
```

## 10.10 Ownership in Structs

### Copy Structs

If all fields are Copy, the struct is Copy:

```vibelang
struct Point {
    x: f64      // Copy
    y: f64      // Copy
}

let a = Point { x: 1.0, y: 2.0 }
let b = a       // copies
// both a and b valid
```

### Owned Structs

If any field is owned, the struct is owned:

```vibelang
struct Person {
    name: String    // owned
    age: u32        // Copy
}

let a = Person { name: String.from("Alice"), age: 30 }
let b = a           // moves
// a is no longer valid
```

### Borrowing Struct Fields

```vibelang
let person = Person { name: String.from("Alice"), age: 30 }
let name_ref = &person.name     // borrow just the name
print(name_ref)
// person still fully valid
```

### Partial Moves

```vibelang
let person = Person { name: String.from("Alice"), age: 30 }
let name = person.name          // move name out
// person.name invalid
// person.age still valid
// person as a whole invalid
```

## 10.11 Visibility

### Public Fields

```vibelang
pub struct PublicPoint {
    pub x: f64
    pub y: f64
}
```

### Private Fields

```vibelang
pub struct Encapsulated {
    data: Vec<u8>     // private by default
}

impl Encapsulated {
    pub fn new() -> Encapsulated {
        return Encapsulated { data: Vec<u8>() }
    }
    
    pub fn len(self: &Encapsulated) -> u64 {
        return self.data.len()
    }
}
```

### Public Enum Variants

Enum variants follow the enum's visibility:

```vibelang
pub enum Status {
    Active      // public because enum is public
    Inactive
    Pending
}
```

## 10.12 Recursive Types

### Using Pointers

```vibelang
struct Node<T> {
    value: T
    next: *Node<T>      // pointer, known size
}

enum List<T> {
    Cons(T, *List<T>)   // pointer to next
    Nil
}
```

### Using Box (if available)

```vibelang
enum Tree<T> {
    Leaf(T)
    Branch {
        left: Box<Tree<T>>
        right: Box<Tree<T>>
    }
}
```

## 10.13 Type Aliases

### Simple Aliases

```vibelang
type Byte = u8
type Word = u32
type NodePtr<T> = *Node<T>
```

### Complex Aliases

```vibelang
type ParseResult = Result<Ast, ParseError>
type TokenList = Vec<Token>
type StringMap<V> = Map<String, V>
```

## 10.14 Complete Example

```vibelang
// AST for a simple expression language

pub enum Expr {
    Literal(i64)
    Ident(String)
    Binary {
        op: BinOp
        left: Box<Expr>
        right: Box<Expr>
    }
    Unary {
        op: UnOp
        operand: Box<Expr>
    }
    Call {
        callee: String
        args: Vec<Expr>
    }
}

pub enum BinOp {
    Add
    Sub
    Mul
    Div
}

pub enum UnOp {
    Neg
    Not
}

impl Expr {
    pub fn literal(value: i64) -> Expr {
        return Expr.Literal(value)
    }
    
    pub fn ident(name: String) -> Expr {
        return Expr.Ident(name)
    }
    
    pub fn binary(op: BinOp, left: Expr, right: Expr) -> Expr {
        return Expr.Binary {
            op: op,
            left: Box.new(left),
            right: Box.new(right)
        }
    }
    
    pub fn eval(self: &Expr, env: &Env) -> Result<i64, EvalError> {
        return match self {
            Expr.Literal(n) => Result.Ok(*n)
            Expr.Ident(name) => env.lookup(name)
            Expr.Binary { op, left, right } => {
                let l = left.eval(env)?
                let r = right.eval(env)?
                match op {
                    BinOp.Add => Result.Ok(l + r)
                    BinOp.Sub => Result.Ok(l - r)
                    BinOp.Mul => Result.Ok(l * r)
                    BinOp.Div => {
                        if r == 0 {
                            Result.Err(EvalError.DivByZero)
                        } else {
                            Result.Ok(l / r)
                        }
                    }
                }
            }
            Expr.Unary { op, operand } => {
                let v = operand.eval(env)?
                match op {
                    UnOp.Neg => Result.Ok(-v)
                    UnOp.Not => Result.Ok(if v == 0 { 1 } else { 0 })
                }
            }
            Expr.Call { callee, args } => {
                env.call(callee, args)
            }
        }
    }
}
```
