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
    Static(Static),
    Use(Use),
    Mod(Mod),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub generics: Vec<String>,  // Type parameters like <T, U>
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
    pub target: Type,
    pub methods: Vec<Function>,
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

#[derive(Debug, Clone)]
pub struct Use {
    pub path: Vec<String>,
    pub alias: Option<String>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub name: String,
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
    Void,

    // Compound
    Pointer(Box<Type>),      // *T
    Ref(Box<Type>),          // &T (read-only borrow)
    RefMut(Box<Type>),       // ~T (vibing - mutable borrow)
    Array(Box<Type>, usize), // T[N]
    Slice(Box<Type>),        // Slice<T>

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

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}
