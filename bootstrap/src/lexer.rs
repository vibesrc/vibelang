//! Vibelang lexer - tokenizes source into tokens

/// Integer literal suffix for explicit typing
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntSuffix {
    None,  // No suffix, use default (i32)
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Int(i64, IntSuffix),
    Float(f64),
    String(String),
    Char(u8),
    Bool(bool),

    /// Interpolated string start: "text before ${
    /// Contains the literal text before the first interpolation
    InterpolatedStringStart(String),
    /// Interpolated string middle: }text${
    /// Contains the literal text between interpolations
    InterpolatedStringMiddle(String),
    /// Interpolated string end: }text"
    /// Contains the literal text after the last interpolation
    InterpolatedStringEnd(String),

    // Identifiers and keywords
    Ident(String),
    Keyword(Keyword),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=
    Amp,        // & (borrow or bitwise AND)
    Tilde,      // ~ (vibing - mutable borrow)
    Pipe,
    Caret,
    Bang,       // ! (bitwise NOT)
    Eq,
    EqEq,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Arrow,      // ->
    FatArrow,   // =>
    Question,   // ?
    Dot,
    DotDot,     // ..

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,

    // Special
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Let,
    Const,
    Static,
    Fn,
    Struct,
    Enum,
    Impl,
    If,
    Else,
    Match,
    While,
    For,
    In,
    Return,
    Break,
    Continue,
    True,
    False,
    And,
    Or,
    Not,
    SelfValue, // self
    SelfType,  // Self
    Pub,
    Use,
    As,
    Defer,
    Unsafe,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    line: u32,
    column: u32,
    /// Stack of brace depths for nested interpolations
    /// When inside `"...${` we push the current brace depth onto this stack
    /// When we encounter `}` and depth matches, we're ending an interpolation
    interpolation_brace_depth: Vec<u32>,
    /// Current brace nesting level (for tracking `{` and `}` inside interpolations)
    brace_depth: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            chars: source.char_indices().peekable(),
            line: 1,
            column: 1,
            interpolation_brace_depth: Vec::new(),
            brace_depth: 0,
        }
    }

    /// Returns true if we're currently inside an interpolated string expression
    fn in_interpolation(&self) -> bool {
        !self.interpolation_brace_depth.is_empty()
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        self.skip_whitespace_and_comments();

        let start = self.current_pos();
        let start_line = self.line;
        let start_column = self.column;

        let kind = match self.advance() {
            None => TokenKind::Eof,
            Some((_, c)) => match c {
                // Operators with optional = suffix
                '+' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::PlusEq
                    } else {
                        TokenKind::Plus
                    }
                }
                '-' => {
                    if self.peek_char() == Some('>') {
                        self.advance();
                        TokenKind::Arrow
                    } else if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::MinusEq
                    } else {
                        TokenKind::Minus
                    }
                }
                '*' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::StarEq
                    } else {
                        TokenKind::Star
                    }
                }
                '/' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::SlashEq
                    } else {
                        TokenKind::Slash
                    }
                }
                '%' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::PercentEq
                    } else {
                        TokenKind::Percent
                    }
                }
                '&' => TokenKind::Amp,
                '~' => TokenKind::Tilde,
                '|' => TokenKind::Pipe,
                '^' => TokenKind::Caret,
                '!' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::BangEq
                    } else {
                        TokenKind::Bang
                    }
                }
                '=' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::EqEq
                    } else if self.peek_char() == Some('>') {
                        self.advance();
                        TokenKind::FatArrow
                    } else {
                        TokenKind::Eq
                    }
                }
                '<' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::LtEq
                    } else {
                        TokenKind::Lt
                    }
                }
                '>' => {
                    if self.peek_char() == Some('=') {
                        self.advance();
                        TokenKind::GtEq
                    } else {
                        TokenKind::Gt
                    }
                }
                '?' => TokenKind::Question,
                '.' => {
                    if self.peek_char() == Some('.') {
                        self.advance();
                        TokenKind::DotDot
                    } else {
                        TokenKind::Dot
                    }
                }
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => {
                    self.brace_depth += 1;
                    TokenKind::LBrace
                }
                '}' => {
                    // Check if this closes an interpolation
                    if let Some(&interp_depth) = self.interpolation_brace_depth.last() {
                        if self.brace_depth == interp_depth {
                            // This closes an interpolation - continue reading the string
                            self.interpolation_brace_depth.pop();
                            return self.lex_interpolation_continuation();
                        }
                    }
                    if self.brace_depth > 0 {
                        self.brace_depth -= 1;
                    }
                    TokenKind::RBrace
                }
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                ',' => TokenKind::Comma,
                ':' => TokenKind::Colon,
                ';' => TokenKind::Semicolon,

                // String literal
                '"' => self.lex_string()?,

                // Character literal
                '\'' => self.lex_char()?,

                // Number
                c if c.is_ascii_digit() => self.lex_number(c)?,

                // Identifier or keyword
                c if c.is_alphabetic() || c == '_' => self.lex_ident(c),

                _ => return Err(LexError::UnexpectedChar(c, start_line, start_column)),
            },
        };

        Ok(Token {
            kind,
            span: Span {
                start,
                end: self.current_pos(),
                line: start_line,
                column: start_column,
            },
        })
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let result = self.chars.next();
        if let Some((_, c)) = result {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        result
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn current_pos(&mut self) -> usize {
        self.chars.peek().map(|(i, _)| *i).unwrap_or(self.source.len())
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(' ' | '\t' | '\r' | '\n') => {
                    self.advance();
                }
                Some('/') => {
                    // Look ahead for comment
                    let mut lookahead = self.chars.clone();
                    lookahead.next();
                    match lookahead.peek() {
                        Some((_, '/')) => {
                            // Line comment
                            while let Some(c) = self.peek_char() {
                                if c == '\n' {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        Some((_, '*')) => {
                            // Block comment
                            self.advance(); // /
                            self.advance(); // *
                            let mut depth = 1;
                            while depth > 0 {
                                match self.advance() {
                                    Some((_, '*')) if self.peek_char() == Some('/') => {
                                        self.advance();
                                        depth -= 1;
                                    }
                                    Some((_, '/')) if self.peek_char() == Some('*') => {
                                        self.advance();
                                        depth += 1;
                                    }
                                    None => break,
                                    _ => {}
                                }
                            }
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }
    }

    fn lex_string(&mut self) -> Result<TokenKind, LexError> {
        let mut value = String::new();

        loop {
            match self.advance() {
                None => return Err(LexError::UnterminatedString(self.line)),
                Some((_, '"')) => break,
                Some((_, '\\')) => {
                    let escaped = match self.advance() {
                        Some((_, 'n')) => '\n',
                        Some((_, 'r')) => '\r',
                        Some((_, 't')) => '\t',
                        Some((_, '\\')) => '\\',
                        Some((_, '"')) => '"',
                        Some((_, '0')) => '\0',
                        Some((_, c)) => return Err(LexError::InvalidEscape(c, self.line)),
                        None => return Err(LexError::UnterminatedString(self.line)),
                    };
                    value.push(escaped);
                }
                Some((_, '$')) => {
                    // Check for interpolation or escaped $
                    match self.peek_char() {
                        Some('{') => {
                            // Start interpolation: push current brace depth and consume '{'
                            self.advance(); // consume '{'
                            self.interpolation_brace_depth.push(self.brace_depth);
                            return Ok(TokenKind::InterpolatedStringStart(value));
                        }
                        Some('$') => {
                            // Escaped $$ -> literal $
                            self.advance();
                            value.push('$');
                        }
                        _ => {
                            // Just a regular $ character
                            value.push('$');
                        }
                    }
                }
                Some((_, c)) => value.push(c),
            }
        }

        Ok(TokenKind::String(value))
    }

    /// Lex a character literal like 'a' or '\n'
    fn lex_char(&mut self) -> Result<TokenKind, LexError> {
        let c = match self.advance() {
            None => return Err(LexError::UnterminatedChar(self.line)),
            Some((_, '\\')) => {
                // Escape sequence
                match self.advance() {
                    Some((_, 'n')) => b'\n',
                    Some((_, 'r')) => b'\r',
                    Some((_, 't')) => b'\t',
                    Some((_, '\\')) => b'\\',
                    Some((_, '\'')) => b'\'',
                    Some((_, '0')) => b'\0',
                    Some((_, 'x')) => {
                        // Hex escape \xNN
                        let mut hex = String::new();
                        for _ in 0..2 {
                            match self.advance() {
                                Some((_, c)) if c.is_ascii_hexdigit() => hex.push(c),
                                _ => return Err(LexError::InvalidEscape('x', self.line)),
                            }
                        }
                        u8::from_str_radix(&hex, 16).map_err(|_| LexError::InvalidEscape('x', self.line))?
                    }
                    Some((_, c)) => return Err(LexError::InvalidEscape(c, self.line)),
                    None => return Err(LexError::UnterminatedChar(self.line)),
                }
            }
            Some((_, '\'')) => return Err(LexError::EmptyChar(self.line)),
            Some((_, c)) => {
                // Single ASCII character
                if c.is_ascii() {
                    c as u8
                } else {
                    return Err(LexError::NonAsciiChar(self.line));
                }
            }
        };

        // Expect closing quote
        match self.advance() {
            Some((_, '\'')) => Ok(TokenKind::Char(c)),
            _ => Err(LexError::UnterminatedChar(self.line)),
        }
    }

    /// Continue lexing after an interpolation expression ends (after the closing `}`)
    fn lex_interpolation_continuation(&mut self) -> Result<Token, LexError> {
        let start = self.current_pos();
        let start_line = self.line;
        let start_column = self.column;

        let mut value = String::new();

        loop {
            match self.advance() {
                None => return Err(LexError::UnterminatedString(self.line)),
                Some((_, '"')) => {
                    // End of interpolated string
                    return Ok(Token {
                        kind: TokenKind::InterpolatedStringEnd(value),
                        span: Span {
                            start,
                            end: self.current_pos(),
                            line: start_line,
                            column: start_column,
                        },
                    });
                }
                Some((_, '\\')) => {
                    let escaped = match self.advance() {
                        Some((_, 'n')) => '\n',
                        Some((_, 'r')) => '\r',
                        Some((_, 't')) => '\t',
                        Some((_, '\\')) => '\\',
                        Some((_, '"')) => '"',
                        Some((_, '0')) => '\0',
                        Some((_, c)) => return Err(LexError::InvalidEscape(c, self.line)),
                        None => return Err(LexError::UnterminatedString(self.line)),
                    };
                    value.push(escaped);
                }
                Some((_, '$')) => {
                    match self.peek_char() {
                        Some('{') => {
                            // Another interpolation
                            self.advance(); // consume '{'
                            self.interpolation_brace_depth.push(self.brace_depth);
                            return Ok(Token {
                                kind: TokenKind::InterpolatedStringMiddle(value),
                                span: Span {
                                    start,
                                    end: self.current_pos(),
                                    line: start_line,
                                    column: start_column,
                                },
                            });
                        }
                        Some('$') => {
                            // Escaped $$ -> literal $
                            self.advance();
                            value.push('$');
                        }
                        _ => {
                            // Just a regular $ character
                            value.push('$');
                        }
                    }
                }
                Some((_, c)) => value.push(c),
            }
        }
    }

    fn lex_number(&mut self, first: char) -> Result<TokenKind, LexError> {
        let mut num_str = String::from(first);
        let mut is_float = false;

        // Check for hex/octal/binary
        if first == '0' {
            match self.peek_char() {
                Some('x' | 'X') => {
                    self.advance();
                    num_str.clear();
                    while let Some(c) = self.peek_char() {
                        if c.is_ascii_hexdigit() || c == '_' {
                            if c != '_' {
                                num_str.push(c);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&num_str, 16)
                        .map_err(|_| LexError::InvalidNumber(self.line))?;
                    let suffix = self.try_parse_int_suffix();
                    return Ok(TokenKind::Int(val, suffix));
                }
                Some('o' | 'O') => {
                    self.advance();
                    num_str.clear();
                    while let Some(c) = self.peek_char() {
                        if c.is_digit(8) || c == '_' {
                            if c != '_' {
                                num_str.push(c);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&num_str, 8)
                        .map_err(|_| LexError::InvalidNumber(self.line))?;
                    let suffix = self.try_parse_int_suffix();
                    return Ok(TokenKind::Int(val, suffix));
                }
                Some('b' | 'B') => {
                    self.advance();
                    num_str.clear();
                    while let Some(c) = self.peek_char() {
                        if c == '0' || c == '1' || c == '_' {
                            if c != '_' {
                                num_str.push(c);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let val = i64::from_str_radix(&num_str, 2)
                        .map_err(|_| LexError::InvalidNumber(self.line))?;
                    let suffix = self.try_parse_int_suffix();
                    return Ok(TokenKind::Int(val, suffix));
                }
                _ => {}
            }
        }

        // Decimal number
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() || c == '_' {
                if c != '_' {
                    num_str.push(c);
                }
                self.advance();
            } else if c == '.' && !is_float {
                // Check it's not a range operator
                let mut lookahead = self.chars.clone();
                lookahead.next();
                if lookahead.peek().map(|(_, c)| c.is_ascii_digit()) == Some(true) {
                    is_float = true;
                    num_str.push('.');
                    self.advance();
                } else {
                    break;
                }
            } else if (c == 'e' || c == 'E') && !num_str.contains('e') {
                is_float = true;
                num_str.push('e');
                self.advance();
                if self.peek_char() == Some('+') || self.peek_char() == Some('-') {
                    num_str.push(self.advance().unwrap().1);
                }
            } else {
                break;
            }
        }

        if is_float {
            let val: f64 = num_str.parse().map_err(|_| LexError::InvalidNumber(self.line))?;
            Ok(TokenKind::Float(val))
        } else {
            // Check for integer suffix (i8, i16, i32, i64, u8, u16, u32, u64)
            let suffix = self.try_parse_int_suffix();
            let val: i64 = num_str.parse().map_err(|_| LexError::InvalidNumber(self.line))?;
            Ok(TokenKind::Int(val, suffix))
        }
    }

    fn try_parse_int_suffix(&mut self) -> IntSuffix {
        // Look for type suffix: i8, i16, i32, i64, u8, u16, u32, u64
        let start_pos = self.chars.clone();

        if let Some(c) = self.peek_char() {
            if c == 'i' || c == 'u' {
                let is_unsigned = c == 'u';
                self.advance();

                // Collect digits for the size
                let mut size_str = String::new();
                while let Some(d) = self.peek_char() {
                    if d.is_ascii_digit() {
                        size_str.push(d);
                        self.advance();
                    } else {
                        break;
                    }
                }

                // Parse the suffix
                match (is_unsigned, size_str.as_str()) {
                    (false, "8") => return IntSuffix::I8,
                    (false, "16") => return IntSuffix::I16,
                    (false, "32") => return IntSuffix::I32,
                    (false, "64") => return IntSuffix::I64,
                    (true, "8") => return IntSuffix::U8,
                    (true, "16") => return IntSuffix::U16,
                    (true, "32") => return IntSuffix::U32,
                    (true, "64") => return IntSuffix::U64,
                    _ => {
                        // Invalid suffix, restore position
                        self.chars = start_pos;
                    }
                }
            }
        }

        IntSuffix::None
    }

    fn lex_ident(&mut self, first: char) -> TokenKind {
        let mut ident = String::from(first);

        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Check for keywords
        match ident.as_str() {
            "let" => TokenKind::Keyword(Keyword::Let),
            "const" => TokenKind::Keyword(Keyword::Const),
            "static" => TokenKind::Keyword(Keyword::Static),
            "fn" => TokenKind::Keyword(Keyword::Fn),
            "struct" => TokenKind::Keyword(Keyword::Struct),
            "enum" => TokenKind::Keyword(Keyword::Enum),
            "impl" => TokenKind::Keyword(Keyword::Impl),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "match" => TokenKind::Keyword(Keyword::Match),
            "while" => TokenKind::Keyword(Keyword::While),
            "for" => TokenKind::Keyword(Keyword::For),
            "in" => TokenKind::Keyword(Keyword::In),
            "return" => TokenKind::Keyword(Keyword::Return),
            "break" => TokenKind::Keyword(Keyword::Break),
            "continue" => TokenKind::Keyword(Keyword::Continue),
            "true" => TokenKind::Bool(true),
            "false" => TokenKind::Bool(false),
            "and" => TokenKind::Keyword(Keyword::And),
            "or" => TokenKind::Keyword(Keyword::Or),
            "not" => TokenKind::Keyword(Keyword::Not),
            "self" => TokenKind::Keyword(Keyword::SelfValue),
            "Self" => TokenKind::Keyword(Keyword::SelfType),
            "pub" => TokenKind::Keyword(Keyword::Pub),
            "use" => TokenKind::Keyword(Keyword::Use),
            "as" => TokenKind::Keyword(Keyword::As),
            "defer" => TokenKind::Keyword(Keyword::Defer),
            "unsafe" => TokenKind::Keyword(Keyword::Unsafe),
            _ => TokenKind::Ident(ident),
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(char, u32, u32),
    UnterminatedString(u32),
    UnterminatedChar(u32),
    EmptyChar(u32),
    NonAsciiChar(u32),
    InvalidEscape(char, u32),
    InvalidNumber(u32),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedChar(c, line, col) => {
                write!(f, "unexpected character '{}' at {}:{}", c, line, col)
            }
            LexError::UnterminatedString(line) => {
                write!(f, "unterminated string at line {}", line)
            }
            LexError::UnterminatedChar(line) => {
                write!(f, "unterminated character literal at line {}", line)
            }
            LexError::EmptyChar(line) => {
                write!(f, "empty character literal at line {}", line)
            }
            LexError::NonAsciiChar(line) => {
                write!(f, "non-ASCII character in character literal at line {}", line)
            }
            LexError::InvalidEscape(c, line) => {
                write!(f, "invalid escape sequence '\\{}' at line {}", c, line)
            }
            LexError::InvalidNumber(line) => {
                write!(f, "invalid number at line {}", line)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("+ - * /");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
    }

    #[test]
    fn test_vibing_operator() {
        let mut lexer = Lexer::new("~x &y");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Tilde);
        assert_eq!(tokens[2].kind, TokenKind::Amp);
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("let fn and or not");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Keyword(Keyword::Let));
        assert_eq!(tokens[1].kind, TokenKind::Keyword(Keyword::Fn));
        assert_eq!(tokens[2].kind, TokenKind::Keyword(Keyword::And));
        assert_eq!(tokens[3].kind, TokenKind::Keyword(Keyword::Or));
        assert_eq!(tokens[4].kind, TokenKind::Keyword(Keyword::Not));
    }

    #[test]
    fn test_plain_string() {
        let mut lexer = Lexer::new(r#""hello world""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::String("hello world".to_string()));
    }

    #[test]
    fn test_interpolated_string_simple() {
        // "hello ${name}"
        let mut lexer = Lexer::new(r#""hello ${name}""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart("hello ".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Ident("name".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::InterpolatedStringEnd("".to_string()));
    }

    #[test]
    fn test_interpolated_string_multiple() {
        // "x=${x}, y=${y}"
        let mut lexer = Lexer::new(r#""x=${x}, y=${y}""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart("x=".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Ident("x".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::InterpolatedStringMiddle(", y=".to_string()));
        assert_eq!(tokens[3].kind, TokenKind::Ident("y".to_string()));
        assert_eq!(tokens[4].kind, TokenKind::InterpolatedStringEnd("".to_string()));
    }

    #[test]
    fn test_interpolated_string_with_expr() {
        // "result: ${x + 1}"
        let mut lexer = Lexer::new(r#""result: ${x + 1}""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart("result: ".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Ident("x".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Plus);
        assert_eq!(tokens[3].kind, TokenKind::Int(1, IntSuffix::None));
        assert_eq!(tokens[4].kind, TokenKind::InterpolatedStringEnd("".to_string()));
    }

    #[test]
    fn test_escaped_dollar() {
        // "cost: $$50"
        let mut lexer = Lexer::new(r#""cost: $$50""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::String("cost: $50".to_string()));
    }

    #[test]
    fn test_interpolation_with_braces() {
        // "obj: ${SomeStruct { x: 1 }}"
        let mut lexer = Lexer::new(r#""obj: ${SomeStruct { x: 1 }}""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::InterpolatedStringStart("obj: ".to_string()));
        assert_eq!(tokens[1].kind, TokenKind::Ident("SomeStruct".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::LBrace);
        assert_eq!(tokens[3].kind, TokenKind::Ident("x".to_string()));
        assert_eq!(tokens[4].kind, TokenKind::Colon);
        assert_eq!(tokens[5].kind, TokenKind::Int(1, IntSuffix::None));
        assert_eq!(tokens[6].kind, TokenKind::RBrace);
        assert_eq!(tokens[7].kind, TokenKind::InterpolatedStringEnd("".to_string()));
    }

    #[test]
    fn test_integer_suffixes() {
        let mut lexer = Lexer::new("42i64 100u8 255i32");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Int(42, IntSuffix::I64));
        assert_eq!(tokens[1].kind, TokenKind::Int(100, IntSuffix::U8));
        assert_eq!(tokens[2].kind, TokenKind::Int(255, IntSuffix::I32));
    }

    #[test]
    fn test_underscore_in_numbers() {
        let mut lexer = Lexer::new("1_000_000 0xFF_FF 0b1111_0000");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Int(1000000, IntSuffix::None));
        assert_eq!(tokens[1].kind, TokenKind::Int(0xFFFF, IntSuffix::None));
        assert_eq!(tokens[2].kind, TokenKind::Int(0b11110000, IntSuffix::None));
    }
}
