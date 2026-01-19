//! Vibelang AST types

use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Enum(Enum),
    Impl(Impl),
    Trait(Trait),
    Static(Static),
    Use(Use),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<String>,  // Type parameters like <T, U>
    pub bounds: Vec<(String, String)>,  // Type bounds: [(T, Hash), (T, Eq)]
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub generics: Vec<String>,
    pub bounds: Vec<(String, String)>,  // Type bounds: [(T, Hash), (T, Eq)]
    pub fields: Vec<Field>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub generics: Vec<String>,
    pub variants: Vec<Variant>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    pub fields: VariantFields,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum VariantFields {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<Field>),
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub trait_name: Option<String>,     // None = inherent impl, Some = trait impl
    pub generics: Vec<String>,          // impl<T> generics
    pub bounds: Vec<(String, String)>,  // Type bounds: [(T, Hash), (T, Eq)]
    pub target: Type,
    pub methods: Vec<Function>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: String,
    pub generics: Vec<String>,
    pub supertraits: Vec<String>,       // trait Ord: Eq
    pub methods: Vec<TraitMethod>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub generics: Vec<String>,
    pub bounds: Vec<(String, String)>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,            // None = abstract, Some = default impl
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Static {
    pub name: String,
    pub ty: Option<Type>,
    pub value: Expr,
    pub is_pub: bool,
    pub span: Span,
}

/// Import prefix for use statements
#[derive(Debug, Clone, PartialEq)]
pub enum ImportPrefix {
    /// use src.foo - absolute path from project root's src/ directory
    Src,
    /// use lib.foo - local workspace packages from project root's lib/ directory
    Lib,
    /// use std.foo - standard library
    Std,
    /// use dep.foo - vendored external dependencies from project root's dep/ directory
    Dep,
    /// use .foo - relative to current file's directory
    Relative,
}

/// What items to import from a module
#[derive(Debug, Clone)]
pub enum ImportItems {
    /// Import specific items: use src.foo.{bar, baz}
    Named(Vec<ImportItem>),
    /// Import all public items: use src.foo.*
    Glob,
    /// Import the module itself as a namespace: use std.fs
    /// Allows usage like fs.File, fs.read_file, etc.
    Module,
}

/// A single imported item, optionally with an alias
#[derive(Debug, Clone)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Use {
    /// The import prefix (src, std, dep, or relative)
    pub prefix: ImportPrefix,
    /// The module path after the prefix (e.g., ["utils", "format"] for src.utils.format)
    pub path: Vec<String>,
    /// The items to import from the module
    pub items: ImportItems,
    /// Whether this is a re-export (pub use)
    pub is_pub: bool,
    pub span: Span,
}

// Types

#[derive(Debug, Clone)]
pub enum Type {
    // Primitives
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Char,  // u8 under the hood
    Void,

    // Compound
    Pointer(Box<Type>),      // *T
    Ref(Box<Type>),          // &T (read-only borrow)
    RefMut(Box<Type>),       // ~T (vibing - mutable borrow)
    Array(Box<Type>, usize), // T[N]
    Slice(Box<Type>),        // Slice<T>
    Tuple(Vec<Type>),        // (T1, T2, ...)
    Fn(Vec<Type>, Box<Type>), // fn(T1, T2) -> R

    // Named
    Named {
        name: String,
        generics: Vec<Type>,
    },

    // Self type (used in impl blocks)
    SelfType,
}

// Expressions

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Type>,
        value: Expr,
        span: Span,
    },
    LetPattern {
        pattern: Pattern,
        value: Expr,
        span: Span,
    },
    Expr(Expr),
    Return {
        value: Option<Expr>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        then_block: Block,
        else_block: Option<Block>,
        span: Span,
    },
    While {
        condition: Box<Expr>,
        body: Block,
        span: Span,
    },
    For {
        name: String,
        iter: Box<Expr>,
        body: Block,
        span: Span,
    },
    Match {
        value: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    Defer {
        expr: Box<Expr>,
        span: Span,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Ident(String),
    Literal(Literal),
    Tuple(Vec<Pattern>),  // (a, b, c)
    Enum {
        path: Vec<String>,
        fields: Vec<Pattern>,
    },
    Struct {
        path: Vec<String>,
        fields: Vec<(String, Pattern)>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal, Span),
    Ident(String, Span),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
        span: Span,
    },
    Call {
        func: Box<Expr>,
        type_args: Vec<Type>,  // Generic type arguments like <i32, String>
        args: Vec<Expr>,
        span: Span,
    },
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        span: Span,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Field {
        object: Box<Expr>,
        field: String,
        span: Span,
    },
    Ref {
        operand: Box<Expr>,
        span: Span,
    },
    RefMut {
        operand: Box<Expr>,
        span: Span,
    },
    Deref {
        operand: Box<Expr>,
        span: Span,
    },
    StructInit {
        name: String,
        generics: Vec<Type>,
        fields: Vec<(String, Expr)>,
        span: Span,
    },
    ArrayInit {
        elements: Vec<Expr>,
        span: Span,
    },
    ArrayRepeat {
        value: Box<Expr>,
        count: usize,
        span: Span,
    },
    Tuple {
        elements: Vec<Expr>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
        span: Span,
    },
    Block(Block),
    Try {
        operand: Box<Expr>,
        span: Span,
    },
    Cast {
        expr: Box<Expr>,
        ty: Type,
        span: Span,
    },
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        span: Span,
    },
    InterpolatedString {
        parts: Vec<StringPart>,
        span: Span,
    },
    Unsafe {
        block: Block,
        span: Span,
    },
    Closure {
        params: Vec<(String, Option<Type>)>,
        return_type: Option<Type>,
        body: ClosureBody,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub enum ClosureBody {
    Expr(Box<Expr>),   // (x) => x + 1
    Block(Block),       // (x) => { return x + 1 }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Logical
    And,
    Or,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,

    // Assignment
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // not (logical)
    BitNot, // ! (bitwise)
}

pub use crate::lexer::IntSuffix;

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64, IntSuffix),
    Float(f64),
    String(String),
    Char(u8),
    Bool(bool),
}

/// A part of an interpolated string
#[derive(Debug, Clone)]
pub enum StringPart {
    /// A literal string segment
    Literal(String),
    /// An expression to be converted to string: ${expr}
    Expr(Box<Expr>),
}

/// An interpolated string like "hello ${name}, you are ${age} years old"
#[derive(Debug, Clone)]
pub struct InterpolatedString {
    pub parts: Vec<StringPart>,
}
