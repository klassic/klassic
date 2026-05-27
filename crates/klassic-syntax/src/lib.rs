use klassic_span::{Diagnostic, SourceFile, Span};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntLiteralKind {
    Byte,
    Short,
    Int,
    Long,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatLiteralKind {
    Float,
    Double,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeAnnotation {
    pub text: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecordField {
    pub name: String,
    pub annotation: Option<TypeAnnotation>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeClassMethod {
    pub name: String,
    pub annotation: TypeAnnotation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    /// Variant parameters in `field: Type` form. The field name lets
    /// user code access the payload via record-style field access
    /// (`opt.v`) without writing an explicit pattern match.
    pub params: Vec<(String, TypeAnnotation)>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub constructor: String,
    pub bindings: Vec<String>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeClassConstraint {
    pub class_name: String,
    pub arguments: Vec<TypeAnnotation>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Int {
        value: i64,
        kind: IntLiteralKind,
        span: Span,
    },
    Double {
        value: f64,
        kind: FloatLiteralKind,
        span: Span,
    },
    Bool {
        value: bool,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Null {
        span: Span,
    },
    Unit {
        span: Span,
    },
    Identifier {
        name: String,
        span: Span,
    },
    ModuleHeader {
        name: String,
        span: Span,
    },
    Import {
        path: String,
        alias: Option<String>,
        members: Option<Vec<String>>,
        excludes: Vec<String>,
        span: Span,
    },
    RecordDeclaration {
        name: String,
        type_params: Vec<String>,
        fields: Vec<RecordField>,
        span: Span,
    },
    RecordLiteral {
        fields: Vec<(String, Expr)>,
        span: Span,
    },
    TypeClassDeclaration {
        name: String,
        type_params: Vec<String>,
        methods: Vec<TypeClassMethod>,
        span: Span,
    },
    InstanceDeclaration {
        class_name: String,
        for_type: String,
        for_type_annotation: TypeAnnotation,
        constraints: Vec<TypeClassConstraint>,
        methods: Vec<Expr>,
        span: Span,
    },
    TheoremDeclaration {
        name: String,
        params: Vec<String>,
        param_annotations: Vec<Option<TypeAnnotation>>,
        proposition: Box<Expr>,
        body: Box<Expr>,
        trusted: bool,
        span: Span,
    },
    AxiomDeclaration {
        name: String,
        params: Vec<String>,
        param_annotations: Vec<Option<TypeAnnotation>>,
        proposition: Box<Expr>,
        span: Span,
    },
    /// `extension <type-params>? (this: <ReceiverType>) { <DefDecl>* }`
    ///
    /// Adds new methods to an existing type. Each `def` inside the
    /// block is dispatched through `value.method(args)` syntax with
    /// the value bound to `this_name` (always `"this"` today) in the
    /// def body. Type parameters declared on the extension header are
    /// in scope across every method, so `extension <a>(this: List<a>)`
    /// can refer to `a` in any contained `def`.
    ExtensionDeclaration {
        type_params: Vec<String>,
        this_name: String,
        receiver_type: TypeAnnotation,
        methods: Vec<Expr>,
        span: Span,
    },
    /// `data Name<type-params>? = Variant1(types...) | Variant2 | ...`
    ///
    /// Algebraic data type declaration. Each variant becomes a
    /// constructor — calling `Variant(args)` yields a tagged value
    /// that pattern matches via `match`. Variants without arguments
    /// are nullary constants.
    EnumDeclaration {
        name: String,
        type_params: Vec<String>,
        variants: Vec<EnumVariant>,
        span: Span,
    },
    /// `match scrutinee { Variant1(x, y) => body1 | Variant2 => body2 | ... }`
    ///
    /// Dispatches on the variant tag of an ADT value. Pattern
    /// variables become bindings inside their arm body.
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    PegRuleBlock {
        span: Span,
    },
    VarDecl {
        mutable: bool,
        name: String,
        annotation: Option<TypeAnnotation>,
        value: Box<Expr>,
        span: Span,
    },
    DefDecl {
        name: String,
        type_params: Vec<String>,
        constraints: Vec<TypeClassConstraint>,
        params: Vec<String>,
        param_annotations: Vec<Option<TypeAnnotation>>,
        return_annotation: Option<TypeAnnotation>,
        body: Box<Expr>,
        span: Span,
    },
    Lambda {
        params: Vec<String>,
        param_annotations: Vec<Option<TypeAnnotation>>,
        body: Box<Expr>,
        span: Span,
    },
    Assign {
        name: String,
        value: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        span: Span,
    },
    FieldAccess {
        target: Box<Expr>,
        field: String,
        span: Span,
    },
    Cleanup {
        body: Box<Expr>,
        cleanup: Box<Expr>,
        span: Span,
    },
    RecordConstructor {
        name: String,
        arguments: Vec<Expr>,
        span: Span,
    },
    ListLiteral {
        elements: Vec<Expr>,
        span: Span,
    },
    MapLiteral {
        entries: Vec<(Expr, Expr)>,
        span: Span,
    },
    SetLiteral {
        elements: Vec<Expr>,
        span: Span,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
        span: Span,
    },
    While {
        condition: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },
    Foreach {
        binding: String,
        iterable: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },
    Block {
        expressions: Vec<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::Int { span, .. }
            | Self::Double { span, .. }
            | Self::Bool { span, .. }
            | Self::String { span, .. }
            | Self::Null { span }
            | Self::Unit { span }
            | Self::Identifier { span, .. }
            | Self::ModuleHeader { span, .. }
            | Self::Import { span, .. }
            | Self::RecordDeclaration { span, .. }
            | Self::RecordLiteral { span, .. }
            | Self::TypeClassDeclaration { span, .. }
            | Self::InstanceDeclaration { span, .. }
            | Self::TheoremDeclaration { span, .. }
            | Self::AxiomDeclaration { span, .. }
            | Self::ExtensionDeclaration { span, .. }
            | Self::EnumDeclaration { span, .. }
            | Self::Match { span, .. }
            | Self::PegRuleBlock { span }
            | Self::VarDecl { span, .. }
            | Self::DefDecl { span, .. }
            | Self::Lambda { span, .. }
            | Self::Assign { span, .. }
            | Self::Unary { span, .. }
            | Self::Binary { span, .. }
            | Self::Call { span, .. }
            | Self::FieldAccess { span, .. }
            | Self::Cleanup { span, .. }
            | Self::RecordConstructor { span, .. }
            | Self::ListLiteral { span, .. }
            | Self::MapLiteral { span, .. }
            | Self::SetLiteral { span, .. }
            | Self::If { span, .. }
            | Self::While { span, .. }
            | Self::Foreach { span, .. }
            | Self::Block { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Token {
    kind: TokenKind,
    span: Span,
}

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    Int(i64, IntLiteralKind),
    Double(f64, FloatLiteralKind),
    String(String),
    Identifier(String),
    True,
    False,
    Null,
    Val,
    Mutable,
    Def,
    Module,
    Import,
    As,
    Record,
    TypeClass,
    Instance,
    Theorem,
    Trust,
    Axiom,
    Extension,
    Enum,
    Match,
    Rule,
    Where,
    Cleanup,
    If,
    Else,
    Foreach,
    In,
    Then,
    While,
    Newline,
    Semicolon,
    Comma,
    Colon,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    Caret,
    Ampersand,
    Pipe,
    Hash,
    Percent,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    Assign,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    AndAnd,
    OrOr,
    FatArrow,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Eof,
}

pub fn parse_source(source: &SourceFile) -> Result<Expr, Diagnostic> {
    let tokens = Lexer::new(source).lex()?;
    Parser::new(tokens).parse_program()
}

pub fn parse_inline_expression(name: &str, text: &str) -> Result<Expr, Diagnostic> {
    let source = SourceFile::new(name, text);
    parse_source(&source)
}

struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a SourceFile) -> Self {
        Self {
            input: source.text(),
            position: 0,
        }
    }

    fn lex(mut self) -> Result<Vec<Token>, Diagnostic> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let done = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if done {
                break;
            }
        }
        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, Diagnostic> {
        self.skip_trivia()?;
        let start = self.position;
        let token = match self.peek_char() {
            None => TokenKind::Eof,
            Some('\n') => {
                self.bump_char();
                TokenKind::Newline
            }
            Some(';') => {
                self.bump_char();
                TokenKind::Semicolon
            }
            Some(',') => {
                self.bump_char();
                TokenKind::Comma
            }
            Some(':') => {
                self.bump_char();
                TokenKind::Colon
            }
            Some('.') => {
                self.bump_char();
                TokenKind::Dot
            }
            Some('(') => {
                self.bump_char();
                TokenKind::LParen
            }
            Some(')') => {
                self.bump_char();
                TokenKind::RParen
            }
            Some('[') => {
                self.bump_char();
                TokenKind::LBracket
            }
            Some(']') => {
                self.bump_char();
                TokenKind::RBracket
            }
            Some('{') => {
                self.bump_char();
                TokenKind::LBrace
            }
            Some('}') => {
                self.bump_char();
                TokenKind::RBrace
            }
            Some('+') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            Some('-') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::MinusEqual
                } else {
                    TokenKind::Minus
                }
            }
            Some('*') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            Some('/') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            Some('!') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            }
            Some('=') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::EqualEqual
                } else if self.consume_if('>') {
                    TokenKind::FatArrow
                } else {
                    TokenKind::Assign
                }
            }
            Some('<') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                }
            }
            Some('>') => {
                self.bump_char();
                if self.consume_if('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                }
            }
            Some('&') => {
                self.bump_char();
                if self.consume_if('&') {
                    TokenKind::AndAnd
                } else {
                    TokenKind::Ampersand
                }
            }
            Some('|') => {
                self.bump_char();
                if self.consume_if('|') {
                    TokenKind::OrOr
                } else {
                    TokenKind::Pipe
                }
            }
            Some('#') => {
                self.bump_char();
                TokenKind::Hash
            }
            Some('%') => {
                self.bump_char();
                TokenKind::Percent
            }
            Some('^') => {
                self.bump_char();
                TokenKind::Caret
            }
            Some('"') => return self.lex_string(),
            Some(ch) if ch.is_ascii_digit() => return self.lex_number(),
            Some(ch) if Self::is_identifier_start(ch) => return self.lex_identifier(),
            Some(ch) => {
                return Err(Diagnostic::parse(
                    Span::new(start, start + ch.len_utf8()),
                    format!("unexpected character `{ch}`"),
                ));
            }
        };
        Ok(Token {
            kind: token,
            span: Span::new(start, self.position),
        })
    }

    fn lex_number(&mut self) -> Result<Token, Diagnostic> {
        let start = self.position;
        while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
            self.bump_char();
        }

        let is_double = if self.peek_char() == Some('.')
            && matches!(self.peek_nth_char(1), Some(ch) if ch.is_ascii_digit())
        {
            self.bump_char();
            while matches!(self.peek_char(), Some(ch) if ch.is_ascii_digit()) {
                self.bump_char();
            }
            true
        } else {
            false
        };

        if is_double {
            let (text_end, kind) = if matches!(self.peek_char(), Some('F')) {
                let end = self.position;
                self.bump_char();
                (end, FloatLiteralKind::Float)
            } else {
                (self.position, FloatLiteralKind::Double)
            };
            let text = &self.input[start..text_end];
            let value = text.parse::<f64>().map_err(|_| {
                Diagnostic::parse(Span::new(start, text_end), "invalid floating point literal")
            })?;
            return Ok(Token {
                kind: TokenKind::Double(value, kind),
                span: Span::new(start, self.position),
            });
        }

        let kind = if self.input[self.position..].starts_with("BY") {
            self.position += 2;
            IntLiteralKind::Byte
        } else if matches!(self.peek_char(), Some('S')) {
            self.bump_char();
            IntLiteralKind::Short
        } else if matches!(self.peek_char(), Some('L')) {
            self.bump_char();
            IntLiteralKind::Long
        } else {
            IntLiteralKind::Int
        };

        let text_end = match kind {
            IntLiteralKind::Byte => self.position - 2,
            IntLiteralKind::Short | IntLiteralKind::Long => self.position - 1,
            IntLiteralKind::Int => self.position,
        };

        let text = &self.input[start..text_end];
        let value = text.parse::<i64>().map_err(|_| {
            Diagnostic::parse(
                Span::new(start, text_end),
                "integer literal is out of range",
            )
        })?;

        Ok(Token {
            kind: TokenKind::Int(value, kind),
            span: Span::new(start, self.position),
        })
    }

    fn lex_identifier(&mut self) -> Result<Token, Diagnostic> {
        let start = self.position;
        self.bump_char();
        while matches!(self.peek_char(), Some(ch) if Self::is_identifier_continue(ch)) {
            self.bump_char();
        }
        let text = &self.input[start..self.position];
        let kind = match text {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "val" => TokenKind::Val,
            "mutable" => TokenKind::Mutable,
            "def" => TokenKind::Def,
            "module" => TokenKind::Module,
            "import" => TokenKind::Import,
            "as" => TokenKind::As,
            "record" => TokenKind::Record,
            "typeclass" => TokenKind::TypeClass,
            "instance" => TokenKind::Instance,
            "theorem" => TokenKind::Theorem,
            "trust" => TokenKind::Trust,
            "axiom" => TokenKind::Axiom,
            "extension" => TokenKind::Extension,
            "enum" => TokenKind::Enum,
            "match" => TokenKind::Match,
            "rule" => TokenKind::Rule,
            "where" => TokenKind::Where,
            "cleanup" => TokenKind::Cleanup,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "foreach" => TokenKind::Foreach,
            "in" => TokenKind::In,
            "then" => TokenKind::Then,
            "while" => TokenKind::While,
            _ => TokenKind::Identifier(text.to_string()),
        };
        Ok(Token {
            kind,
            span: Span::new(start, self.position),
        })
    }

    fn lex_string(&mut self) -> Result<Token, Diagnostic> {
        let start = self.position;
        self.bump_char();
        let mut value = String::new();

        loop {
            match self.peek_char() {
                None => {
                    return Err(Diagnostic::parse(
                        Span::new(start, self.input.len()),
                        "unterminated string",
                    )
                    .with_incomplete_input());
                }
                Some('"') => {
                    self.bump_char();
                    break;
                }
                Some('\\') => {
                    self.bump_char();
                    let escaped = match self.peek_char() {
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('b') => '\u{0008}',
                        Some('f') => '\u{000C}',
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some(other) => {
                            return Err(Diagnostic::parse(
                                Span::new(self.position, self.position + other.len_utf8()),
                                format!("unsupported escape sequence `\\{other}`"),
                            ));
                        }
                        None => {
                            return Err(Diagnostic::parse(
                                Span::new(start, self.input.len()),
                                "unterminated string escape",
                            )
                            .with_incomplete_input());
                        }
                    };
                    self.bump_char();
                    value.push(escaped);
                }
                Some(ch) => {
                    self.bump_char();
                    value.push(ch);
                }
            }
        }

        Ok(Token {
            kind: TokenKind::String(value),
            span: Span::new(start, self.position),
        })
    }

    fn skip_trivia(&mut self) -> Result<(), Diagnostic> {
        loop {
            while matches!(self.peek_char(), Some(ch) if matches!(ch, ' ' | '\t' | '\r')) {
                self.bump_char();
            }

            if self.starts_with("//") {
                while let Some(ch) = self.peek_char() {
                    if ch == '\n' {
                        break;
                    }
                    self.bump_char();
                }
                continue;
            }

            if self.starts_with("/*") {
                self.skip_block_comment()?;
                continue;
            }

            break;
        }
        Ok(())
    }

    fn skip_block_comment(&mut self) -> Result<(), Diagnostic> {
        let start = self.position;
        self.position += 2;
        let mut depth = 1usize;

        while self.position < self.input.len() {
            if self.starts_with("/*") {
                depth += 1;
                self.position += 2;
                continue;
            }
            if self.starts_with("*/") {
                depth -= 1;
                self.position += 2;
                if depth == 0 {
                    return Ok(());
                }
                continue;
            }
            self.bump_char();
        }

        Err(Diagnostic::parse(
            Span::new(start, self.input.len()),
            "unterminated block comment",
        )
        .with_incomplete_input())
    }

    fn starts_with(&self, needle: &str) -> bool {
        self.input[self.position..].starts_with(needle)
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.position..].chars().next()
    }

    fn peek_nth_char(&self, n: usize) -> Option<char> {
        self.input[self.position..].chars().nth(n)
    }

    fn bump_char(&mut self) -> Option<char> {
        let ch = self.peek_char()?;
        self.position += ch.len_utf8();
        Some(ch)
    }

    fn consume_if(&mut self, ch: char) -> bool {
        if self.peek_char() == Some(ch) {
            self.bump_char();
            true
        } else {
            false
        }
    }

    fn is_identifier_start(ch: char) -> bool {
        ch == '_' || ch == '\'' || ch.is_ascii_alphabetic()
    }

    fn is_identifier_continue(ch: char) -> bool {
        ch == '_' || ch == '\'' || ch.is_ascii_alphanumeric()
    }
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_program(mut self) -> Result<Expr, Diagnostic> {
        let expressions = self.parse_sequence_until(|kind| matches!(kind, TokenKind::Eof))?;
        let eof_span = self.peek().span;
        Ok(match expressions.len() {
            0 => Expr::Unit { span: eof_span },
            1 => expressions
                .into_iter()
                .next()
                .expect("single expression exists"),
            _ => Expr::Block {
                span: expressions
                    .first()
                    .expect("block start exists")
                    .span()
                    .merge(expressions.last().expect("block end exists").span()),
                expressions,
            },
        })
    }

    fn parse_sequence_until<F>(&mut self, is_end: F) -> Result<Vec<Expr>, Diagnostic>
    where
        F: Fn(&TokenKind) -> bool,
    {
        let mut expressions = Vec::new();
        self.consume_separators();

        while !is_end(&self.peek().kind) {
            expressions.push(self.parse_expression()?);
            if is_end(&self.peek().kind) {
                break;
            }
            if self.consume_separators() {
                continue;
            }
            return Err(Diagnostic::parse(
                self.peek().span,
                "expected a newline or `;` between expressions",
            ));
        }

        Ok(expressions)
    }

    fn parse_expression(&mut self) -> Result<Expr, Diagnostic> {
        self.consume_separators();
        let expr = match self.peek().kind {
            TokenKind::Module => self.parse_module_header(),
            TokenKind::Import => self.parse_import_expression(),
            TokenKind::Record => {
                if matches!(
                    self.tokens.get(self.index + 1).map(|token| &token.kind),
                    Some(TokenKind::LBrace)
                ) {
                    self.parse_record_literal()
                } else {
                    self.parse_record_declaration()
                }
            }
            TokenKind::TypeClass => self.parse_typeclass_declaration(),
            TokenKind::Instance => self.parse_instance_declaration(),
            TokenKind::Trust | TokenKind::Theorem | TokenKind::Axiom => {
                self.parse_proof_declaration()
            }
            TokenKind::Extension => self.parse_extension_declaration(),
            TokenKind::Enum => self.parse_enum_declaration(),
            TokenKind::Rule => self.parse_peg_rule_block(),
            TokenKind::Val => self.parse_variable_declaration(false),
            TokenKind::Mutable => self.parse_variable_declaration(true),
            TokenKind::Def => self.parse_function_declaration(),
            TokenKind::If => self.parse_if_expression(),
            TokenKind::Foreach => self.parse_foreach_expression(),
            TokenKind::While => self.parse_while_expression(),
            _ => self.parse_assignment(),
        }?;
        self.parse_cleanup_suffix(expr)
    }

    fn parse_record_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (name, name_span) = self.expect_identifier()?;
        let (type_params, inline_constraints) = self.parse_optional_generic_param_names()?;
        if let Some(c) = inline_constraints.first() {
            return Err(Diagnostic::parse(
                c.span,
                "record declarations do not accept inline type-class constraints",
            ));
        }
        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }

        let mut fields = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            let (field, _) = self.expect_identifier()?;
            let annotation = self.parse_optional_type_annotation(&[
                TokenMarker::Newline,
                TokenMarker::Semicolon,
                TokenMarker::RBrace,
            ])?;
            if annotation.is_none() {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `:` in record",
                ));
            }
            fields.push(RecordField {
                name: field,
                annotation,
            });
            self.consume_separators();
        }

        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };

        Ok(Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span: start.merge(name_span).merge(end),
        })
    }

    fn parse_record_literal(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }

        let mut fields = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            let (field, _) = self.expect_identifier()?;
            match self.peek().kind {
                TokenKind::Colon => {
                    self.bump();
                }
                _ => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `:` in record literal",
                    ));
                }
            }
            let value = self.parse_expression()?;
            fields.push((field, value));
            self.consume_separators();
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.bump();
                self.consume_separators();
            } else if matches!(self.peek().kind, TokenKind::RBrace) {
                break;
            }
        }

        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };

        Ok(Expr::RecordLiteral {
            fields,
            span: start.merge(end),
        })
    }

    fn parse_typeclass_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (name, name_span) = self.expect_identifier()?;
        let (type_params, inline_constraints) = self.parse_optional_generic_param_names()?;
        if let Some(c) = inline_constraints.first() {
            return Err(Diagnostic::parse(
                c.span,
                "typeclass declarations do not accept inline type-class constraints",
            ));
        }
        match self.peek().kind {
            TokenKind::Where => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `where`")),
        }
        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }
        let mut methods = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            let (method, _) = self.expect_identifier()?;
            let annotation = self
                .parse_optional_type_annotation(&[
                    TokenMarker::Newline,
                    TokenMarker::Semicolon,
                    TokenMarker::RBrace,
                ])?
                .ok_or_else(|| {
                    Diagnostic::parse(self.peek().span, "expected `:` in typeclass declaration")
                })?;
            methods.push(TypeClassMethod {
                name: method,
                annotation,
            });
            self.consume_separators();
        }
        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };
        Ok(Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span: start.merge(name_span).merge(end),
        })
    }

    fn parse_instance_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (class_name, class_span) = self.expect_identifier()?;
        let (for_type, for_type_annotation) = self.parse_instance_target_type()?;
        match self.peek().kind {
            TokenKind::Where => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `where`")),
        }
        let constraints = if matches!(self.peek().kind, TokenKind::LBrace) {
            Vec::new()
        } else {
            self.parse_instance_constraints()?
        };
        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }

        let mut methods = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            if !matches!(self.peek().kind, TokenKind::Def) {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `def` inside instance declaration",
                ));
            }
            methods.push(self.parse_function_declaration()?);
            self.consume_separators();
        }

        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };

        Ok(Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods,
            span: start.merge(class_span).merge(end),
        })
    }

    fn parse_extension_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span; // consume `extension`
        let (type_params, _inline_constraints) = self.parse_optional_generic_param_names()?;

        match self.peek().kind {
            TokenKind::LParen => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `(` after `extension`")
                        .with_incomplete_input(),
                );
            }
            _ => {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `(` after `extension`",
                ));
            }
        }

        let (this_name, this_span) = self.expect_identifier()?;

        match self.peek().kind {
            TokenKind::Colon => {
                self.bump();
            }
            _ => {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `:` after extension receiver name",
                ));
            }
        }

        let annotation_start = self.index;
        let mut paren_depth = 1usize;
        let mut angle_depth = 0usize;
        while paren_depth > 0 {
            match self.peek().kind {
                TokenKind::LParen => {
                    paren_depth += 1;
                    self.bump();
                }
                TokenKind::RParen => {
                    if paren_depth == 1 && angle_depth == 0 {
                        break;
                    }
                    paren_depth = paren_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Less => {
                    angle_depth += 1;
                    self.bump();
                }
                TokenKind::Greater => {
                    angle_depth = angle_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `)` to close extension receiver type",
                    )
                    .with_incomplete_input());
                }
                _ => {
                    self.bump();
                }
            }
        }
        let annotation_end = self.index;
        if annotation_start == annotation_end {
            return Err(Diagnostic::parse(
                self.peek().span,
                "expected receiver type annotation",
            ));
        }
        let annotation_span = self.tokens[annotation_start]
            .span
            .merge(self.tokens[annotation_end - 1].span);
        let receiver_type = TypeAnnotation {
            text: self.render_tokens(annotation_start, annotation_end),
            span: annotation_span,
        };

        match self.peek().kind {
            TokenKind::RParen => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
        }

        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }

        let mut methods = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            if !matches!(self.peek().kind, TokenKind::Def) {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `def` inside extension declaration",
                ));
            }
            methods.push(self.parse_function_declaration()?);
            self.consume_separators();
        }

        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };

        Ok(Expr::ExtensionDeclaration {
            type_params,
            this_name,
            receiver_type,
            methods,
            span: start.merge(this_span).merge(end),
        })
    }

    fn parse_enum_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span; // consume `enum`
        let (name, name_span) = self.expect_identifier()?;
        let (type_params, _inline) = self.parse_optional_generic_param_names()?;

        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }

        let mut variants = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            // Each variant is introduced by `case`. We parse `case` as
            // an identifier rather than a dedicated keyword so user
            // code can still use `case` as a regular name elsewhere.
            match &self.peek().kind {
                TokenKind::Identifier(text) if text == "case" => {
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(
                        Diagnostic::parse(self.peek().span, "expected `case` or `}`")
                            .with_incomplete_input(),
                    );
                }
                _ => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `case` or `}`",
                    ));
                }
            }
            let (variant_name, variant_span) = self.expect_identifier()?;
            let params = if matches!(self.peek().kind, TokenKind::LParen) {
                self.bump();
                let mut entries = Vec::new();
                self.consume_separators();
                if !matches!(self.peek().kind, TokenKind::RParen) {
                    loop {
                        let (field_name, _field_span) = self.expect_identifier()?;
                        match self.peek().kind {
                            TokenKind::Colon => {
                                self.bump();
                            }
                            _ => {
                                return Err(Diagnostic::parse(
                                    self.peek().span,
                                    "expected `:` after variant field name",
                                ));
                            }
                        }
                        let annotation = self.parse_variant_field_type()?;
                        entries.push((field_name, annotation));
                        self.consume_separators();
                        if matches!(self.peek().kind, TokenKind::Comma) {
                            self.bump();
                            self.consume_separators();
                            continue;
                        }
                        break;
                    }
                }
                match self.peek().kind {
                    TokenKind::RParen => {
                        self.bump();
                    }
                    _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
                }
                entries
            } else {
                Vec::new()
            };
            variants.push(EnumVariant {
                name: variant_name,
                params,
                span: variant_span,
            });
            self.consume_separators();
        }

        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };

        Ok(Expr::EnumDeclaration {
            name,
            type_params,
            variants,
            span: start.merge(name_span).merge(end),
        })
    }

    /// Reads a single field's type annotation, stopping at `,` or `)`.
    /// Mirrors the simple-token capture used by
    /// `parse_extension_declaration` so the same nested generics rules
    /// apply.
    fn parse_variant_field_type(&mut self) -> Result<TypeAnnotation, Diagnostic> {
        let start_index = self.index;
        let mut paren_depth = 0usize;
        let mut angle_depth = 0usize;
        loop {
            match self.peek().kind {
                TokenKind::LParen => {
                    paren_depth += 1;
                    self.bump();
                }
                TokenKind::RParen => {
                    if paren_depth == 0 && angle_depth == 0 {
                        break;
                    }
                    paren_depth = paren_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Comma if paren_depth == 0 && angle_depth == 0 => break,
                TokenKind::Less => {
                    angle_depth += 1;
                    self.bump();
                }
                TokenKind::Greater => {
                    angle_depth = angle_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `)` to close variant field list",
                    )
                    .with_incomplete_input());
                }
                _ => {
                    self.bump();
                }
            }
        }
        if start_index == self.index {
            return Err(Diagnostic::parse(
                self.peek().span,
                "expected type annotation for variant field",
            ));
        }
        let span = self.tokens[start_index]
            .span
            .merge(self.tokens[self.index - 1].span);
        Ok(TypeAnnotation {
            text: self.render_tokens(start_index, self.index),
            span,
        })
    }

    fn parse_postfix_match(&mut self, scrutinee: Expr) -> Result<Expr, Diagnostic> {
        let start = self.bump().span; // consume `match`
        let scrutinee_span = scrutinee.span();
        match self.peek().kind {
            TokenKind::LBrace => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        }
        let mut arms = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBrace) {
            // Each arm is introduced by `case`. We parse `case` as
            // an identifier so it doesn't collide with user names.
            match &self.peek().kind {
                TokenKind::Identifier(text) if text == "case" => {
                    self.bump();
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `case`")),
            }
            let (ctor, ctor_span) = self.expect_identifier()?;
            let bindings = if matches!(self.peek().kind, TokenKind::LParen) {
                self.bump();
                let mut names = Vec::new();
                self.consume_separators();
                if !matches!(self.peek().kind, TokenKind::RParen) {
                    loop {
                        let (binding, _) = self.expect_identifier()?;
                        names.push(binding);
                        self.consume_separators();
                        if matches!(self.peek().kind, TokenKind::Comma) {
                            self.bump();
                            self.consume_separators();
                            continue;
                        }
                        break;
                    }
                }
                match self.peek().kind {
                    TokenKind::RParen => {
                        self.bump();
                    }
                    _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
                }
                names
            } else {
                Vec::new()
            };
            match self.peek().kind {
                TokenKind::FatArrow => {
                    self.bump();
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `=>`")),
            }
            let body = self.parse_expression()?;
            let arm_span = ctor_span.merge(body.span());
            arms.push(MatchArm {
                constructor: ctor,
                bindings,
                body,
                span: arm_span,
            });
            self.consume_separators();
        }
        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };
        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span: start.merge(scrutinee_span).merge(end),
        })
    }

    fn parse_proof_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.peek().span;
        let trusted = if matches!(self.peek().kind, TokenKind::Trust) {
            self.bump();
            match self.peek().kind {
                TokenKind::Theorem => {
                    self.bump();
                    true
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(self.peek().span, "expected `theorem`")
                        .with_incomplete_input());
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `theorem`")),
            }
        } else {
            false
        };

        if matches!(self.peek().kind, TokenKind::Axiom) {
            let axiom_span = self.bump().span;
            let (name, name_span) = self.expect_identifier()?;
            let (params, param_annotations) = if matches!(self.peek().kind, TokenKind::LParen) {
                self.parse_parameter_list()?
            } else {
                (Vec::new(), Vec::new())
            };
            match self.peek().kind {
                TokenKind::Colon => {
                    self.bump();
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `:`")),
            }
            let (proposition, prop_span) = self.parse_braced_proposition()?;
            return Ok(Expr::AxiomDeclaration {
                name,
                params,
                param_annotations,
                proposition: Box::new(proposition),
                span: start.merge(axiom_span).merge(name_span).merge(prop_span),
            });
        }

        if !trusted {
            match self.peek().kind {
                TokenKind::Theorem => {
                    self.bump();
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `theorem`")),
            }
        }

        let (name, name_span) = self.expect_identifier()?;
        let (params, param_annotations) = if matches!(self.peek().kind, TokenKind::LParen) {
            self.parse_parameter_list()?
        } else {
            (Vec::new(), Vec::new())
        };
        match self.peek().kind {
            TokenKind::Colon => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `:`")),
        }
        let (proposition, prop_span) = self.parse_braced_proposition()?;
        self.expect_assign()?;
        let body = self.parse_expression()?;
        Ok(Expr::TheoremDeclaration {
            name,
            params,
            param_annotations,
            proposition: Box::new(proposition),
            body: Box::new(body.clone()),
            trusted,
            span: start.merge(name_span).merge(prop_span).merge(body.span()),
        })
    }

    fn parse_peg_rule_block(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let brace_start = match self.peek().kind {
            TokenKind::LBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        };

        let mut depth = 1usize;
        let mut end = brace_start;
        while depth > 0 {
            match self.peek().kind {
                TokenKind::LBrace => {
                    end = self.bump().span;
                    depth += 1;
                }
                TokenKind::RBrace => {
                    end = self.bump().span;
                    depth -= 1;
                }
                TokenKind::Eof => {
                    return Err(
                        Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                    );
                }
                _ => {
                    end = self.bump().span;
                }
            }
        }

        Ok(Expr::PegRuleBlock {
            span: start.merge(end),
        })
    }

    fn parse_braced_proposition(&mut self) -> Result<(Expr, Span), Diagnostic> {
        let start = match self.peek().kind {
            TokenKind::LBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `{`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `{`")),
        };
        let expressions = self.parse_sequence_until(|kind| matches!(kind, TokenKind::RBrace))?;
        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };
        let span = start.merge(end);
        let expr = if expressions.len() == 1 {
            expressions
                .into_iter()
                .next()
                .expect("single expression exists")
        } else {
            Expr::Block { expressions, span }
        };
        Ok((expr, span))
    }

    fn parse_cleanup_suffix(&mut self, expr: Expr) -> Result<Expr, Diagnostic> {
        if !matches!(self.peek().kind, TokenKind::Cleanup) {
            return Ok(expr);
        }
        let cleanup_token = self.bump().span;
        let cleanup = self.parse_expression()?;
        Ok(Expr::Cleanup {
            span: expr.span().merge(cleanup_token).merge(cleanup.span()),
            body: Box::new(expr),
            cleanup: Box::new(cleanup),
        })
    }

    fn parse_module_header(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (name, name_span) = self.parse_identifier_path()?;
        Ok(Expr::ModuleHeader {
            name,
            span: start.merge(name_span),
        })
    }

    fn parse_import_expression(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (path, mut span) = self.parse_identifier_path()?;
        let alias = if matches!(self.peek().kind, TokenKind::As) {
            let as_span = self.bump().span;
            let (alias, alias_span) = self.expect_identifier()?;
            span = span.merge(as_span).merge(alias_span);
            Some(alias)
        } else {
            None
        };
        let (members, excludes) = if matches!(self.peek().kind, TokenKind::Dot)
            && matches!(
                self.tokens.get(self.index + 1).map(|token| &token.kind),
                Some(TokenKind::LBrace)
            ) {
            let dot_span = self.bump().span;
            let brace_start = self.bump().span;
            let mut members = Vec::new();
            let mut excludes = Vec::new();
            self.consume_separators();
            if !matches!(self.peek().kind, TokenKind::RBrace) {
                loop {
                    let (member, member_span) = self.expect_identifier()?;
                    span = span.merge(dot_span).merge(brace_start).merge(member_span);
                    if matches!(self.peek().kind, TokenKind::FatArrow) {
                        let arrow_span = self.bump().span;
                        let (hidden, hidden_span) = self.expect_identifier()?;
                        if hidden != "_" {
                            return Err(Diagnostic::parse(hidden_span, "expected `_`"));
                        }
                        span = span.merge(arrow_span).merge(hidden_span);
                        excludes.push(member);
                    } else {
                        members.push(member);
                    }
                    self.consume_separators();
                    if matches!(self.peek().kind, TokenKind::Comma) {
                        self.bump();
                        self.consume_separators();
                        continue;
                    }
                    break;
                }
            }
            let brace_end = match self.peek().kind {
                TokenKind::RBrace => self.bump().span,
                TokenKind::Eof => {
                    return Err(
                        Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                    );
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
            };
            span = span.merge(dot_span).merge(brace_start).merge(brace_end);
            ((!members.is_empty()).then_some(members), excludes)
        } else {
            (None, Vec::new())
        };

        Ok(Expr::Import {
            path,
            alias,
            members,
            excludes,
            span: start.merge(span),
        })
    }

    fn parse_variable_declaration(&mut self, mutable: bool) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (name, name_span) = self.expect_identifier()?;
        let annotation = self.parse_optional_type_annotation(&[TokenMarker::Assign])?;
        self.expect_assign()?;
        let value = self.parse_expression()?;
        Ok(Expr::VarDecl {
            mutable,
            name,
            annotation,
            span: start.merge(value.span()).merge(name_span),
            value: Box::new(value),
        })
    }

    fn parse_function_declaration(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let (name, name_span) = self.expect_identifier()?;
        let (type_params, inline_constraints) = self.parse_optional_generic_param_names()?;
        let (params, param_annotations) = self.parse_parameter_list()?;
        let return_annotation =
            self.parse_optional_type_annotation(&[TokenMarker::Assign, TokenMarker::Where])?;
        let mut constraints = self.parse_optional_constraints()?;
        // Merge inline `<Show 'a>` constraints with any explicit
        // `where Show<'a>` constraints — they live in the same list.
        for c in inline_constraints {
            constraints.push(c);
        }
        self.expect_assign()?;
        let body = self.parse_expression()?;
        Ok(Expr::DefDecl {
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            span: start.merge(name_span).merge(body.span()),
            body: Box::new(body),
        })
    }

    fn parse_if_expression(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        self.expect_lparen()?;
        let condition = self.parse_expression()?;
        self.expect_rparen()?;
        let then_branch = self.parse_branch_expression()?;
        let mut span = start.merge(condition.span()).merge(then_branch.span());
        let else_branch = if matches!(self.peek().kind, TokenKind::Else) {
            let else_token = self.bump().span;
            let branch = self.parse_branch_expression()?;
            span = span.merge(else_token).merge(branch.span());
            Some(Box::new(branch))
        } else {
            None
        };
        Ok(Expr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
            span,
        })
    }

    fn parse_while_expression(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        self.expect_lparen()?;
        let condition = self.parse_expression()?;
        self.expect_rparen()?;
        let body = self.parse_branch_expression()?;
        Ok(Expr::While {
            span: start.merge(condition.span()).merge(body.span()),
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_foreach_expression(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        self.expect_lparen()?;
        let (binding, binding_span) = self.expect_identifier()?;
        match self.peek().kind {
            TokenKind::In => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `in`")),
        }
        let iterable = self.parse_expression()?;
        self.expect_rparen()?;
        let body = self.parse_branch_expression()?;
        Ok(Expr::Foreach {
            binding,
            span: start
                .merge(binding_span)
                .merge(iterable.span())
                .merge(body.span()),
            iterable: Box::new(iterable),
            body: Box::new(body),
        })
    }

    fn parse_branch_expression(&mut self) -> Result<Expr, Diagnostic> {
        if matches!(self.peek().kind, TokenKind::LBrace) {
            self.parse_primary()
        } else {
            self.parse_expression()
        }
    }

    fn parse_assignment(&mut self) -> Result<Expr, Diagnostic> {
        let lhs = self.parse_ternary()?;
        let operator = match self.peek().kind {
            TokenKind::Assign => Some(None),
            TokenKind::PlusEqual => Some(Some(BinaryOp::Add)),
            TokenKind::MinusEqual => Some(Some(BinaryOp::Subtract)),
            TokenKind::StarEqual => Some(Some(BinaryOp::Multiply)),
            TokenKind::SlashEqual => Some(Some(BinaryOp::Divide)),
            _ => None,
        };

        let Some(compound) = operator else {
            return Ok(lhs);
        };

        let token_span = self.bump().span;
        let Expr::Identifier {
            name,
            span: name_span,
        } = lhs
        else {
            return Err(Diagnostic::parse(
                token_span,
                "left-hand side of assignment must be an identifier",
            ));
        };
        self.consume_separators();
        let rhs = self.parse_assignment()?;
        let value = match compound {
            None => rhs,
            Some(op) => Expr::Binary {
                span: name_span.merge(rhs.span()).merge(token_span),
                lhs: Box::new(Expr::Identifier {
                    name: name.clone(),
                    span: name_span,
                }),
                op,
                rhs: Box::new(rhs),
            },
        };

        Ok(Expr::Assign {
            span: name_span.merge(value.span()).merge(token_span),
            name,
            value: Box::new(value),
        })
    }

    fn parse_ternary(&mut self) -> Result<Expr, Diagnostic> {
        let condition = self.parse_logical_or()?;
        if !matches!(self.peek().kind, TokenKind::Then) {
            return Ok(condition);
        }
        let then_token = self.bump().span;
        let then_branch = self.parse_expression()?;
        if !matches!(self.peek().kind, TokenKind::Else) {
            return Err(Diagnostic::parse(self.peek().span, "expected `else`"));
        }
        let else_token = self.bump().span;
        let else_branch = self.parse_expression()?;
        Ok(Expr::If {
            span: condition
                .span()
                .merge(then_token)
                .merge(then_branch.span())
                .merge(else_token)
                .merge(else_branch.span()),
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Some(Box::new(else_branch)),
        })
    }

    fn parse_logical_or(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_logical_and, |kind| match kind {
            TokenKind::OrOr => Some(BinaryOp::LogicalOr),
            _ => None,
        })
    }

    fn parse_logical_and(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_bit_or, |kind| match kind {
            TokenKind::AndAnd => Some(BinaryOp::LogicalAnd),
            _ => None,
        })
    }

    fn parse_bit_or(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_bit_xor, |kind| match kind {
            TokenKind::Pipe => Some(BinaryOp::BitOr),
            _ => None,
        })
    }

    fn parse_bit_xor(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_bit_and, |kind| match kind {
            TokenKind::Caret => Some(BinaryOp::BitXor),
            _ => None,
        })
    }

    fn parse_bit_and(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_equality, |kind| match kind {
            TokenKind::Ampersand => Some(BinaryOp::BitAnd),
            _ => None,
        })
    }

    fn parse_equality(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_comparison, |kind| match kind {
            TokenKind::EqualEqual => Some(BinaryOp::Equal),
            TokenKind::BangEqual => Some(BinaryOp::NotEqual),
            _ => None,
        })
    }

    fn parse_comparison(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_additive, |kind| match kind {
            TokenKind::Less => Some(BinaryOp::Less),
            TokenKind::LessEqual => Some(BinaryOp::LessEqual),
            TokenKind::Greater => Some(BinaryOp::Greater),
            TokenKind::GreaterEqual => Some(BinaryOp::GreaterEqual),
            _ => None,
        })
    }

    fn parse_additive(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_multiplicative, |kind| match kind {
            TokenKind::Plus => Some(BinaryOp::Add),
            TokenKind::Minus => Some(BinaryOp::Subtract),
            _ => None,
        })
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, Diagnostic> {
        self.parse_left_associative(Self::parse_unary, |kind| match kind {
            TokenKind::Star => Some(BinaryOp::Multiply),
            TokenKind::Slash => Some(BinaryOp::Divide),
            _ => None,
        })
    }

    fn parse_left_associative(
        &mut self,
        inner: fn(&mut Self) -> Result<Expr, Diagnostic>,
        operator: impl Fn(&TokenKind) -> Option<BinaryOp>,
    ) -> Result<Expr, Diagnostic> {
        let mut expr = inner(self)?;
        while let Some(op) = operator(&self.peek().kind) {
            let op_token = self.bump().span;
            self.consume_separators();
            let rhs = inner(self)?;
            expr = Expr::Binary {
                span: expr.span().merge(rhs.span()).merge(op_token),
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, Diagnostic> {
        match self.peek().kind {
            TokenKind::Plus => {
                let token = self.bump().span;
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Plus,
                    span: token.merge(expr.span()),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Minus => {
                let token = self.bump().span;
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Minus,
                    span: token.merge(expr.span()),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Bang => {
                let token = self.bump().span;
                let expr = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    span: token.merge(expr.span()),
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, Diagnostic> {
        let mut expr = self.parse_primary()?;
        loop {
            if matches!(self.peek().kind, TokenKind::LParen) {
                let start = self.bump().span;
                let mut arguments = Vec::new();
                self.consume_separators();
                if !matches!(self.peek().kind, TokenKind::RParen) {
                    loop {
                        arguments.push(self.parse_expression()?);
                        self.consume_separators();
                        if matches!(self.peek().kind, TokenKind::Comma) {
                            self.bump();
                            self.consume_separators();
                            continue;
                        }
                        break;
                    }
                }
                let end = match self.peek().kind {
                    TokenKind::RParen => self.bump().span,
                    TokenKind::Eof => {
                        return Err(Diagnostic::parse(self.peek().span, "expected `)`")
                            .with_incomplete_input());
                    }
                    _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
                };
                expr = Expr::Call {
                    span: expr.span().merge(start).merge(end),
                    callee: Box::new(expr),
                    arguments,
                };
                continue;
            }

            if matches!(self.peek().kind, TokenKind::LBrace) && self.looks_like_brace_lambda() {
                let lambda = self.parse_brace_lambda()?;
                let span = expr.span().merge(lambda.span());
                expr = Expr::Call {
                    span,
                    callee: Box::new(expr),
                    arguments: vec![lambda],
                };
                continue;
            }

            if matches!(self.peek().kind, TokenKind::Match) {
                expr = self.parse_postfix_match(expr)?;
                continue;
            }

            if matches!(self.peek().kind, TokenKind::Dot)
                && matches!(
                    self.tokens.get(self.index + 1).map(|token| &token.kind),
                    Some(TokenKind::Identifier(_))
                )
            {
                let dot_span = self.bump().span;
                let (field, field_span) = self.expect_identifier()?;
                let span = expr.span().merge(dot_span).merge(field_span);
                expr = Expr::FieldAccess {
                    target: Box::new(expr),
                    field,
                    span,
                };
                continue;
            }

            if matches!(self.peek().kind, TokenKind::Hash)
                && matches!(
                    self.tokens.get(self.index + 1).map(|token| &token.kind),
                    Some(TokenKind::Identifier(_))
                )
            {
                let hash_span = self.bump().span;
                let (name, name_span) = self.expect_identifier()?;
                let rhs = self.parse_postfix()?;
                let callee = Expr::Identifier {
                    name,
                    span: hash_span.merge(name_span),
                };
                if matches!(&callee, Expr::Identifier { name, .. } if name == "cons") {
                    let partial = Expr::Call {
                        span: expr.span().merge(callee.span()),
                        callee: Box::new(callee),
                        arguments: vec![expr],
                    };
                    expr = Expr::Call {
                        span: partial.span().merge(rhs.span()),
                        callee: Box::new(partial),
                        arguments: vec![rhs],
                    };
                } else {
                    let span = expr.span().merge(callee.span()).merge(rhs.span());
                    expr = Expr::Call {
                        span,
                        callee: Box::new(callee),
                        arguments: vec![expr, rhs],
                    };
                }
                continue;
            }

            if matches!(self.peek().kind, TokenKind::Colon)
                && matches!(
                    self.tokens.get(self.index + 1).map(|token| &token.kind),
                    Some(TokenKind::Greater)
                )
            {
                let expr_span = expr.span();
                let colon_span = self.bump().span;
                let greater_span = self.bump().span;
                self.skip_cast_type()?;
                expr = rewrap_span(expr, expr_span.merge(colon_span).merge(greater_span));
                continue;
            }

            if matches!(&self.peek().kind, TokenKind::Identifier(name) if name == "map") {
                expr = self.parse_map_syntax(expr)?;
                continue;
            }

            if matches!(&self.peek().kind, TokenKind::Identifier(name) if name == "reduce") {
                expr = self.parse_reduce_syntax(expr)?;
                continue;
            }

            if let Some((name, callee_span)) = self.try_consume_module_identifier() {
                let callee = Expr::Identifier {
                    name,
                    span: callee_span,
                };
                let rhs = if matches!(self.peek().kind, TokenKind::LBrace)
                    && self.looks_like_brace_lambda()
                {
                    self.parse_brace_lambda()?
                } else {
                    self.parse_postfix()?
                };
                let span = expr.span().merge(callee_span).merge(rhs.span());
                expr = Expr::Call {
                    span,
                    callee: Box::new(callee),
                    arguments: vec![expr, rhs],
                };
                continue;
            }

            break;
        }
        Ok(expr)
    }

    fn parse_map_syntax(&mut self, iterable: Expr) -> Result<Expr, Diagnostic> {
        let map_span = self.bump().span;
        self.consume_separators();
        let lambda = self.parse_postfix_lambda(&["e"])?;
        let callee = Expr::Identifier {
            name: "map".to_string(),
            span: map_span,
        };
        let partial = Expr::Call {
            span: iterable.span().merge(map_span),
            callee: Box::new(callee),
            arguments: vec![iterable],
        };
        Ok(Expr::Call {
            span: partial.span().merge(lambda.span()),
            callee: Box::new(partial),
            arguments: vec![lambda],
        })
    }

    fn parse_reduce_syntax(&mut self, iterable: Expr) -> Result<Expr, Diagnostic> {
        let reduce_span = self.bump().span;
        self.consume_separators();
        let initial = self.parse_expression()?;
        self.consume_separators();
        if matches!(self.peek().kind, TokenKind::Comma) {
            self.bump();
            self.consume_separators();
        }
        let lambda = self.parse_postfix_lambda(&["r", "e"])?;
        let callee = Expr::Identifier {
            name: "foldLeft".to_string(),
            span: reduce_span,
        };
        let with_iterable = Expr::Call {
            span: iterable.span().merge(reduce_span),
            callee: Box::new(callee),
            arguments: vec![iterable],
        };
        let with_initial = Expr::Call {
            span: with_iterable.span().merge(initial.span()),
            callee: Box::new(with_iterable),
            arguments: vec![initial],
        };
        Ok(Expr::Call {
            span: with_initial.span().merge(lambda.span()),
            callee: Box::new(with_initial),
            arguments: vec![lambda],
        })
    }

    fn parse_postfix_lambda(&mut self, implicit_params: &[&str]) -> Result<Expr, Diagnostic> {
        if matches!(self.peek().kind, TokenKind::LParen) && self.looks_like_lambda() {
            return self.parse_lambda();
        }

        let start = self.peek().span;
        let params = if matches!(self.peek().kind, TokenKind::FatArrow) {
            self.bump();
            implicit_params
                .iter()
                .map(|name| (*name).to_string())
                .collect()
        } else {
            let mut params = Vec::new();
            loop {
                let (param, _) = self.expect_identifier()?;
                params.push(param);
                self.consume_separators();
                if matches!(self.peek().kind, TokenKind::Comma) {
                    self.bump();
                    self.consume_separators();
                    continue;
                }
                break;
            }
            match self.peek().kind {
                TokenKind::FatArrow => {
                    self.bump();
                    params
                }
                _ => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `=>` in postfix lambda syntax",
                    ));
                }
            }
        };
        let body = self.parse_expression()?;
        let param_count = params.len();
        Ok(Expr::Lambda {
            params,
            param_annotations: vec![None; param_count],
            span: start.merge(body.span()),
            body: Box::new(body),
        })
    }

    fn parse_brace_lambda(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        self.consume_separators();
        let lambda = self.parse_postfix_lambda(&["e"])?;
        self.consume_separators();
        let end = match self.peek().kind {
            TokenKind::RBrace => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `}`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
        };
        Ok(rewrap_span(lambda, start.merge(end)))
    }

    fn parse_primary(&mut self) -> Result<Expr, Diagnostic> {
        match self.peek().kind.clone() {
            TokenKind::Int(value, kind) => {
                let token = self.bump().span;
                Ok(Expr::Int {
                    value,
                    kind,
                    span: token,
                })
            }
            TokenKind::Double(value, kind) => {
                let token = self.bump().span;
                Ok(Expr::Double {
                    value,
                    kind,
                    span: token,
                })
            }
            TokenKind::String(value) => {
                let token = self.bump().span;
                Ok(Expr::String { value, span: token })
            }
            TokenKind::True => {
                let token = self.bump().span;
                Ok(Expr::Bool {
                    value: true,
                    span: token,
                })
            }
            TokenKind::False => {
                let token = self.bump().span;
                Ok(Expr::Bool {
                    value: false,
                    span: token,
                })
            }
            TokenKind::Null => {
                let token = self.bump().span;
                Ok(Expr::Null { span: token })
            }
            TokenKind::Record
                if matches!(
                    self.tokens.get(self.index + 1).map(|token| &token.kind),
                    Some(TokenKind::LBrace)
                ) =>
            {
                self.parse_record_literal()
            }
            TokenKind::FatArrow => self.parse_zero_arg_lambda(),
            TokenKind::Identifier(name) => {
                let token = self.bump().span;
                let (name, span) = self.consume_identifier_selector_suffix(name, token)?;
                Ok(Expr::Identifier { name, span })
            }
            TokenKind::Hash => self.parse_record_constructor(),
            TokenKind::LParen if self.looks_like_lambda() => self.parse_lambda(),
            TokenKind::LParen => {
                let start = self.bump().span;
                self.consume_separators();
                if matches!(self.peek().kind, TokenKind::RParen) {
                    let end = self.bump().span;
                    return Ok(Expr::Unit {
                        span: start.merge(end),
                    });
                }
                let expr = self.parse_expression()?;
                self.consume_separators();
                let end = match self.peek().kind {
                    TokenKind::RParen => self.bump().span,
                    TokenKind::Eof => {
                        return Err(Diagnostic::parse(self.peek().span, "expected `)`")
                            .with_incomplete_input());
                    }
                    _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
                };
                Ok(rewrap_span(expr, start.merge(end)))
            }
            TokenKind::LBracket => self.parse_list_literal(),
            TokenKind::Percent => self.parse_percent_literal(),
            TokenKind::LBrace => {
                let start = self.bump().span;
                let expressions =
                    self.parse_sequence_until(|kind| matches!(kind, TokenKind::RBrace))?;
                let end = match self.peek().kind {
                    TokenKind::RBrace => self.bump().span,
                    TokenKind::Eof => {
                        return Err(Diagnostic::parse(self.peek().span, "expected `}`")
                            .with_incomplete_input());
                    }
                    _ => return Err(Diagnostic::parse(self.peek().span, "expected `}`")),
                };
                Ok(Expr::Block {
                    expressions,
                    span: start.merge(end),
                })
            }
            TokenKind::Eof => Err(
                Diagnostic::parse(self.peek().span, "unexpected end of input")
                    .with_incomplete_input(),
            ),
            _ => Err(Diagnostic::parse(
                self.peek().span,
                "expected an expression",
            )),
        }
    }

    fn parse_zero_arg_lambda(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let body = self.parse_expression()?;
        Ok(Expr::Lambda {
            params: Vec::new(),
            param_annotations: Vec::new(),
            span: start.merge(body.span()),
            body: Box::new(body),
        })
    }

    fn parse_record_constructor(&mut self) -> Result<Expr, Diagnostic> {
        let hash_span = self.bump().span;
        let (name, name_span) = self.expect_identifier()?;
        match self.peek().kind {
            TokenKind::LParen => {
                self.bump();
            }
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `(`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `(`")),
        }

        let mut arguments = Vec::new();
        self.consume_separators();
        if !matches!(self.peek().kind, TokenKind::RParen) {
            loop {
                arguments.push(self.parse_expression()?);
                self.consume_separators();
                if matches!(self.peek().kind, TokenKind::Comma) {
                    self.bump();
                    self.consume_separators();
                    continue;
                }
                break;
            }
        }

        let end = match self.peek().kind {
            TokenKind::RParen => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `)`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
        };

        Ok(Expr::RecordConstructor {
            name,
            arguments,
            span: hash_span.merge(name_span).merge(end),
        })
    }

    fn parse_list_literal(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let mut elements = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBracket) {
            elements.push(self.parse_expression()?);
            self.consume_separators();
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.bump();
                self.consume_separators();
            } else if matches!(self.peek().kind, TokenKind::RBracket) {
                break;
            }
        }
        let end = match self.peek().kind {
            TokenKind::RBracket => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `]`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `]`")),
        };
        Ok(Expr::ListLiteral {
            elements,
            span: start.merge(end),
        })
    }

    fn parse_percent_literal(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        match self.peek().kind {
            TokenKind::LBracket => self.parse_map_literal(start),
            TokenKind::LParen => self.parse_set_literal(start),
            _ => Err(Diagnostic::parse(
                self.peek().span,
                "expected `[` or `(` after `%`",
            )),
        }
    }

    fn parse_map_literal(&mut self, start: Span) -> Result<Expr, Diagnostic> {
        self.bump(); // [
        let mut entries = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RBracket) {
            let key = self.parse_expression()?;
            match self.peek().kind {
                TokenKind::Colon => {
                    self.bump();
                }
                _ => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `:` in map literal",
                    ));
                }
            }
            let value = self.parse_expression()?;
            entries.push((key, value));
            self.consume_separators();
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.bump();
                self.consume_separators();
            } else if matches!(self.peek().kind, TokenKind::RBracket) {
                break;
            }
        }
        let end = match self.peek().kind {
            TokenKind::RBracket => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `]`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `]`")),
        };
        Ok(Expr::MapLiteral {
            entries,
            span: start.merge(end),
        })
    }

    fn parse_set_literal(&mut self, start: Span) -> Result<Expr, Diagnostic> {
        self.bump(); // (
        let mut elements = Vec::new();
        self.consume_separators();
        while !matches!(self.peek().kind, TokenKind::RParen) {
            elements.push(self.parse_expression()?);
            self.consume_separators();
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.bump();
                self.consume_separators();
            } else if matches!(self.peek().kind, TokenKind::RParen) {
                break;
            }
        }
        let end = match self.peek().kind {
            TokenKind::RParen => self.bump().span,
            TokenKind::Eof => {
                return Err(
                    Diagnostic::parse(self.peek().span, "expected `)`").with_incomplete_input()
                );
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
        };
        Ok(Expr::SetLiteral {
            elements,
            span: start.merge(end),
        })
    }

    fn parse_lambda(&mut self) -> Result<Expr, Diagnostic> {
        let start = self.bump().span;
        let mut params = Vec::new();
        let mut param_annotations = Vec::new();
        self.consume_separators();
        if !matches!(self.peek().kind, TokenKind::RParen) {
            loop {
                let (name, _) = self.expect_identifier()?;
                let annotation = self
                    .parse_optional_type_annotation(&[TokenMarker::Comma, TokenMarker::RParen])?;
                params.push(name);
                param_annotations.push(annotation);
                self.consume_separators();
                if matches!(self.peek().kind, TokenKind::Comma) {
                    self.bump();
                    self.consume_separators();
                    continue;
                }
                break;
            }
        }
        self.expect_rparen()?;
        self.expect_fat_arrow()?;
        let body = self.parse_expression()?;
        Ok(Expr::Lambda {
            params,
            param_annotations,
            span: start.merge(body.span()),
            body: Box::new(body),
        })
    }

    fn parse_parameter_list(
        &mut self,
    ) -> Result<(Vec<String>, Vec<Option<TypeAnnotation>>), Diagnostic> {
        self.expect_lparen()?;
        let mut params = Vec::new();
        let mut annotations = Vec::new();
        self.consume_separators();
        if !matches!(self.peek().kind, TokenKind::RParen) {
            loop {
                let (name, _) = self.expect_identifier()?;
                let annotation = self
                    .parse_optional_type_annotation(&[TokenMarker::Comma, TokenMarker::RParen])?;
                params.push(name);
                annotations.push(annotation);
                self.consume_separators();
                if matches!(self.peek().kind, TokenKind::Comma) {
                    self.bump();
                    self.consume_separators();
                    continue;
                }
                break;
            }
        }
        self.expect_rparen()?;
        Ok((params, annotations))
    }

    fn looks_like_lambda(&self) -> bool {
        let mut index = self.index + 1;
        while matches!(
            self.tokens.get(index).map(|token| &token.kind),
            Some(TokenKind::Newline)
        ) {
            index += 1;
        }

        if matches!(
            self.tokens.get(index).map(|token| &token.kind),
            Some(TokenKind::RParen)
        ) {
            index += 1;
            return matches!(
                self.tokens.get(index).map(|token| &token.kind),
                Some(TokenKind::FatArrow)
            );
        }

        loop {
            if !matches!(
                self.tokens.get(index).map(|token| &token.kind),
                Some(TokenKind::Identifier(_))
            ) {
                return false;
            }
            index += 1;

            if matches!(
                self.tokens.get(index).map(|token| &token.kind),
                Some(TokenKind::Colon)
            ) {
                index += 1;
                let mut depth = 0usize;
                loop {
                    match self.tokens.get(index).map(|token| &token.kind) {
                        Some(TokenKind::LParen) => {
                            depth += 1;
                            index += 1;
                        }
                        Some(TokenKind::RParen) if depth == 0 => break,
                        Some(TokenKind::RParen) => {
                            depth -= 1;
                            index += 1;
                        }
                        Some(TokenKind::Comma) if depth == 0 => break,
                        Some(TokenKind::Eof) | None => return false,
                        _ => index += 1,
                    }
                }
            }

            match self.tokens.get(index).map(|token| &token.kind) {
                Some(TokenKind::Comma) => {
                    index += 1;
                }
                Some(TokenKind::RParen) => {
                    index += 1;
                    return matches!(
                        self.tokens.get(index).map(|token| &token.kind),
                        Some(TokenKind::FatArrow)
                    );
                }
                _ => return false,
            }
        }
    }

    fn looks_like_brace_lambda(&self) -> bool {
        let mut index = self.index + 1;
        while matches!(
            self.tokens.get(index).map(|token| &token.kind),
            Some(TokenKind::Newline)
        ) {
            index += 1;
        }

        if matches!(
            self.tokens.get(index).map(|token| &token.kind),
            Some(TokenKind::FatArrow)
        ) {
            return true;
        }

        loop {
            if !matches!(
                self.tokens.get(index).map(|token| &token.kind),
                Some(TokenKind::Identifier(_))
            ) {
                return false;
            }
            index += 1;
            while matches!(
                self.tokens.get(index).map(|token| &token.kind),
                Some(TokenKind::Newline)
            ) {
                index += 1;
            }

            match self.tokens.get(index).map(|token| &token.kind) {
                Some(TokenKind::Comma) => {
                    index += 1;
                    while matches!(
                        self.tokens.get(index).map(|token| &token.kind),
                        Some(TokenKind::Newline)
                    ) {
                        index += 1;
                    }
                }
                Some(TokenKind::FatArrow) => return true,
                _ => return false,
            }
        }
    }

    fn parse_optional_type_annotation(
        &mut self,
        stop_tokens: &[TokenMarker],
    ) -> Result<Option<TypeAnnotation>, Diagnostic> {
        if !matches!(self.peek().kind, TokenKind::Colon) {
            return Ok(None);
        }
        let colon_span = self.bump().span;
        let start_index = self.index;
        let mut depth = 0usize;
        loop {
            let marker = TokenMarker::from_kind(&self.peek().kind);
            if depth == 0 && stop_tokens.contains(&marker) {
                let end_index = self.index;
                let span = if start_index < end_index {
                    self.tokens[start_index]
                        .span
                        .merge(self.tokens[end_index - 1].span)
                } else {
                    colon_span
                };
                return Ok(Some(TypeAnnotation {
                    text: self.render_tokens(start_index, end_index),
                    span,
                }));
            }
            match self.peek().kind {
                TokenKind::LParen => {
                    depth += 1;
                    self.bump();
                }
                TokenKind::RParen => {
                    if depth == 0 {
                        let end_index = self.index;
                        let span = if start_index < end_index {
                            self.tokens[start_index]
                                .span
                                .merge(self.tokens[end_index - 1].span)
                        } else {
                            colon_span
                        };
                        return Ok(Some(TypeAnnotation {
                            text: self.render_tokens(start_index, end_index),
                            span,
                        }));
                    }
                    depth -= 1;
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "unterminated type annotation",
                    )
                    .with_incomplete_input());
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn render_tokens(&self, start: usize, end: usize) -> String {
        let mut rendered = String::new();
        let mut previous = None;
        for token in &self.tokens[start..end] {
            if !rendered.is_empty() && needs_spacing_between(previous, Some(&token.kind)) {
                rendered.push(' ');
            }
            rendered.push_str(&token_text(&token.kind));
            previous = Some(&token.kind);
        }
        rendered
    }

    fn expect_identifier(&mut self) -> Result<(String, Span), Diagnostic> {
        match self.peek().kind.clone() {
            TokenKind::Identifier(name) => {
                let span = self.bump().span;
                Ok((name, span))
            }
            _ => Err(Diagnostic::parse(
                self.peek().span,
                "expected an identifier",
            )),
        }
    }

    fn parse_identifier_path(&mut self) -> Result<(String, Span), Diagnostic> {
        let (name, span) = self.expect_identifier()?;
        self.extend_identifier_path(name, span)
    }

    fn extend_identifier_path(
        &mut self,
        mut name: String,
        mut span: Span,
    ) -> Result<(String, Span), Diagnostic> {
        while matches!(self.peek().kind, TokenKind::Dot)
            && matches!(
                self.tokens.get(self.index + 1).map(|token| &token.kind),
                Some(TokenKind::Identifier(_))
            )
        {
            let dot_span = self.bump().span;
            let (segment, segment_span) = self.expect_identifier()?;
            name.push('.');
            name.push_str(&segment);
            span = span.merge(dot_span).merge(segment_span);
        }
        Ok((name, span))
    }

    fn consume_identifier_selector_suffix(
        &mut self,
        mut name: String,
        mut span: Span,
    ) -> Result<(String, Span), Diagnostic> {
        let mut cursor = self.index;
        while matches!(
            self.tokens.get(cursor).map(|token| &token.kind),
            Some(TokenKind::Dot)
        ) && matches!(
            self.tokens.get(cursor + 1).map(|token| &token.kind),
            Some(TokenKind::Identifier(_))
        ) {
            cursor += 2;
        }

        let selector_prefix_end = if cursor == self.index {
            span.end
        } else {
            self.tokens
                .get(cursor.saturating_sub(1))
                .map(|token| token.span.end)
                .unwrap_or(span.end)
        };

        let hash_is_selector = self
            .tokens
            .get(cursor)
            .zip(self.tokens.get(cursor + 1))
            .is_some_and(|(hash, member)| {
                matches!(hash.kind, TokenKind::Hash)
                    && matches!(member.kind, TokenKind::Identifier(_))
                    && hash.span.start == selector_prefix_end
                    && member.span.start == hash.span.end
            });

        if hash_is_selector {
            while self.index < cursor {
                let separator = self.bump();
                span = span.merge(separator.span);
                if let TokenKind::Identifier(segment) = &separator.kind {
                    if !name.is_empty() {
                        name.push('.');
                    }
                    name.push_str(segment);
                }
            }
            let hash_span = self.bump().span;
            let (member, member_span) = self.expect_identifier()?;
            name.push('#');
            name.push_str(&member);
            span = span.merge(hash_span).merge(member_span);
        }

        Ok((name, span))
    }

    /// Parse `<...>` after a `def` / `typeclass` / `instance` / `record`
    /// header. Accepts plain type-variable names (`'a`) and the inline
    /// constraint shorthand `ClassName 'a`, which lowers to declaring
    /// `'a` as a parameter and adding a `ClassName<'a>` constraint.
    fn parse_optional_generic_param_names(
        &mut self,
    ) -> Result<(Vec<String>, Vec<TypeClassConstraint>), Diagnostic> {
        if !matches!(self.peek().kind, TokenKind::Less) {
            return Ok((Vec::new(), Vec::new()));
        }
        self.bump();
        let mut params: Vec<String> = Vec::new();
        let mut constraints: Vec<TypeClassConstraint> = Vec::new();
        loop {
            match self.peek().kind {
                TokenKind::Identifier(_) => {
                    let (first_name, first_span) = self.expect_identifier()?;
                    // Detect `ClassName 'var` shorthand: the first
                    // identifier does not start with `'` and the next
                    // token is another identifier that does. The first
                    // is the constraint class, the second is the type
                    // variable that picks it up.
                    let inline_constraint = if !first_name.starts_with('\'') {
                        if let TokenKind::Identifier(ref next) = self.peek().kind {
                            next.starts_with('\'')
                        } else {
                            false
                        }
                    } else {
                        false
                    };
                    if inline_constraint {
                        let (var_name, var_span) = self.expect_identifier()?;
                        if !params.contains(&var_name) {
                            params.push(var_name.clone());
                        }
                        let argument = TypeAnnotation {
                            text: var_name,
                            span: var_span,
                        };
                        constraints.push(TypeClassConstraint {
                            class_name: first_name,
                            arguments: vec![argument],
                            span: first_span.merge(var_span),
                        });
                    } else {
                        params.push(first_name);
                    }
                }
                TokenKind::Greater => {
                    self.bump();
                    return Ok((params, constraints));
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "unterminated generic parameters",
                    )
                    .with_incomplete_input());
                }
                _ => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected generic parameter name",
                    ));
                }
            }

            match self.peek().kind {
                TokenKind::Colon => {
                    self.bump();
                    let mut angle_depth = 0usize;
                    loop {
                        match self.peek().kind {
                            TokenKind::Less => {
                                angle_depth += 1;
                                self.bump();
                            }
                            TokenKind::Greater if angle_depth > 0 => {
                                angle_depth = angle_depth.saturating_sub(1);
                                self.bump();
                            }
                            TokenKind::Comma | TokenKind::Greater if angle_depth == 0 => {
                                break;
                            }
                            TokenKind::Eof => {
                                return Err(Diagnostic::parse(
                                    self.peek().span,
                                    "unterminated generic parameters",
                                )
                                .with_incomplete_input());
                            }
                            _ => {
                                self.bump();
                            }
                        }
                    }
                }
                TokenKind::Comma => {
                    self.bump();
                }
                TokenKind::Greater => {
                    self.bump();
                    return Ok((params, constraints));
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "unterminated generic parameters",
                    )
                    .with_incomplete_input());
                }
                _ => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "expected `,` or `>` after generic parameter",
                    ));
                }
            }
        }
    }

    fn skip_cast_type(&mut self) -> Result<(), Diagnostic> {
        self.skip_type_until(
            |kind, paren_depth, bracket_depth, brace_depth, angle_depth| {
                matches!(
                    kind,
                    TokenKind::Newline
                        | TokenKind::Semicolon
                        | TokenKind::Comma
                        | TokenKind::RParen
                        | TokenKind::RBracket
                        | TokenKind::RBrace
                ) && paren_depth == 0
                    && bracket_depth == 0
                    && brace_depth == 0
                    && angle_depth == 0
            },
            "unterminated cast type",
        )
    }

    fn skip_type_until(
        &mut self,
        stop: impl Fn(&TokenKind, usize, usize, usize, usize) -> bool,
        eof_message: &str,
    ) -> Result<(), Diagnostic> {
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut brace_depth = 0usize;
        let mut angle_depth = 0usize;
        loop {
            if stop(
                &self.peek().kind,
                paren_depth,
                bracket_depth,
                brace_depth,
                angle_depth,
            ) {
                return Ok(());
            }

            match self.peek().kind {
                TokenKind::Newline | TokenKind::Semicolon | TokenKind::RBrace
                    if paren_depth == 0
                        && bracket_depth == 0
                        && brace_depth == 0
                        && angle_depth == 0 =>
                {
                    return Ok(());
                }
                TokenKind::LParen => {
                    paren_depth += 1;
                    self.bump();
                }
                TokenKind::RParen => {
                    paren_depth = paren_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::LBracket => {
                    bracket_depth += 1;
                    self.bump();
                }
                TokenKind::RBracket => {
                    bracket_depth = bracket_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::LBrace => {
                    brace_depth += 1;
                    self.bump();
                }
                TokenKind::RBrace if brace_depth > 0 => {
                    brace_depth = brace_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Less => {
                    angle_depth += 1;
                    self.bump();
                }
                TokenKind::Greater if angle_depth > 0 => {
                    angle_depth = angle_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(
                        Diagnostic::parse(self.peek().span, eof_message).with_incomplete_input()
                    );
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn parse_instance_target_type(&mut self) -> Result<(String, TypeAnnotation), Diagnostic> {
        match self.peek().kind {
            TokenKind::Less => {
                self.bump();
            }
            _ => return Err(Diagnostic::parse(self.peek().span, "expected `<`")),
        }

        let mut captured = None;
        let start_index = self.index;
        let mut angle_depth = 1usize;
        while angle_depth > 0 {
            match self.peek().kind.clone() {
                TokenKind::Identifier(name) if captured.is_none() => {
                    captured = Some(name);
                    self.bump();
                }
                TokenKind::Less => {
                    angle_depth += 1;
                    self.bump();
                }
                TokenKind::Greater => {
                    angle_depth = angle_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(
                        Diagnostic::parse(self.peek().span, "unterminated instance type")
                            .with_incomplete_input(),
                    );
                }
                _ => {
                    self.bump();
                }
            }
        }

        let end_index = self.index.saturating_sub(1);
        let annotation_span = if start_index < end_index {
            self.tokens[start_index]
                .span
                .merge(self.tokens[end_index - 1].span)
        } else {
            self.tokens
                .get(start_index)
                .map(|token| token.span)
                .unwrap_or(self.peek().span)
        };

        captured
            .map(|for_type| {
                (
                    for_type,
                    TypeAnnotation {
                        text: self.render_tokens(start_index, end_index),
                        span: annotation_span,
                    },
                )
            })
            .ok_or_else(|| Diagnostic::parse(self.peek().span, "expected instance type"))
    }

    fn parse_optional_constraints(&mut self) -> Result<Vec<TypeClassConstraint>, Diagnostic> {
        if !matches!(self.peek().kind, TokenKind::Where) {
            return Ok(Vec::new());
        }
        self.bump();
        self.consume_separators();

        let grouped = matches!(self.peek().kind, TokenKind::LParen);
        if grouped {
            self.bump();
            self.consume_separators();
        }

        let mut constraints = Vec::new();
        loop {
            let (class_name, class_span) = self.expect_identifier()?;
            if !matches!(self.peek().kind, TokenKind::Less) {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `<` in typeclass constraint",
                ));
            }
            let arguments = self.parse_constraint_type_arguments()?;
            let span = arguments
                .last()
                .map(|annotation| class_span.merge(annotation.span))
                .unwrap_or(class_span);
            constraints.push(TypeClassConstraint {
                class_name,
                arguments,
                span,
            });
            self.consume_separators();
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.bump();
                self.consume_separators();
                continue;
            }
            break;
        }

        if grouped {
            match self.peek().kind {
                TokenKind::RParen => {
                    self.bump();
                }
                TokenKind::Eof => {
                    return Err(
                        Diagnostic::parse(self.peek().span, "expected `)`").with_incomplete_input()
                    );
                }
                _ => return Err(Diagnostic::parse(self.peek().span, "expected `)`")),
            }
        }

        Ok(constraints)
    }

    fn parse_instance_constraints(&mut self) -> Result<Vec<TypeClassConstraint>, Diagnostic> {
        let mut constraints = Vec::new();
        loop {
            let (class_name, class_span) = self.expect_identifier()?;
            if !matches!(self.peek().kind, TokenKind::Less) {
                return Err(Diagnostic::parse(
                    self.peek().span,
                    "expected `<` in instance constraint",
                ));
            }
            let arguments = self.parse_constraint_type_arguments()?;
            let span = arguments
                .last()
                .map(|annotation| class_span.merge(annotation.span))
                .unwrap_or(class_span);
            constraints.push(TypeClassConstraint {
                class_name,
                arguments,
                span,
            });
            self.consume_separators();
            if matches!(self.peek().kind, TokenKind::Comma) {
                self.bump();
                self.consume_separators();
                continue;
            }
            break;
        }
        Ok(constraints)
    }

    fn parse_constraint_type_arguments(&mut self) -> Result<Vec<TypeAnnotation>, Diagnostic> {
        let less_span = self.bump().span;
        let mut arguments = Vec::new();
        let mut segment_start = self.index;
        let mut angle_depth = 1usize;
        let mut paren_depth = 0usize;
        let mut bracket_depth = 0usize;
        let mut brace_depth = 0usize;

        while angle_depth > 0 {
            match self.peek().kind {
                TokenKind::LParen => {
                    paren_depth += 1;
                    self.bump();
                }
                TokenKind::RParen => {
                    paren_depth = paren_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::LBracket => {
                    bracket_depth += 1;
                    self.bump();
                }
                TokenKind::RBracket => {
                    bracket_depth = bracket_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::LBrace => {
                    brace_depth += 1;
                    self.bump();
                }
                TokenKind::RBrace => {
                    brace_depth = brace_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Less => {
                    angle_depth += 1;
                    self.bump();
                }
                TokenKind::Greater
                    if angle_depth == 1
                        && paren_depth == 0
                        && bracket_depth == 0
                        && brace_depth == 0 =>
                {
                    let end_index = self.index;
                    if segment_start < end_index {
                        let span = self.tokens[segment_start]
                            .span
                            .merge(self.tokens[end_index.saturating_sub(1)].span);
                        arguments.push(TypeAnnotation {
                            text: self.render_tokens(segment_start, end_index),
                            span,
                        });
                    }
                    self.bump();
                    angle_depth -= 1;
                }
                TokenKind::Greater => {
                    angle_depth = angle_depth.saturating_sub(1);
                    self.bump();
                }
                TokenKind::Comma
                    if angle_depth == 1
                        && paren_depth == 0
                        && bracket_depth == 0
                        && brace_depth == 0 =>
                {
                    let end_index = self.index;
                    if segment_start < end_index {
                        let span = self.tokens[segment_start]
                            .span
                            .merge(self.tokens[end_index.saturating_sub(1)].span);
                        arguments.push(TypeAnnotation {
                            text: self.render_tokens(segment_start, end_index),
                            span,
                        });
                    }
                    self.bump();
                    segment_start = self.index;
                }
                TokenKind::Eof => {
                    return Err(Diagnostic::parse(
                        self.peek().span,
                        "unterminated typeclass constraint",
                    )
                    .with_incomplete_input());
                }
                _ => {
                    self.bump();
                }
            }
        }

        if arguments.is_empty() {
            return Err(Diagnostic::parse(
                less_span,
                "typeclass constraint requires at least one type argument",
            ));
        }

        Ok(arguments)
    }

    fn expect_assign(&mut self) -> Result<(), Diagnostic> {
        match self.peek().kind {
            TokenKind::Assign => {
                self.bump();
                Ok(())
            }
            _ => Err(Diagnostic::parse(self.peek().span, "expected `=`")),
        }
    }

    fn expect_fat_arrow(&mut self) -> Result<(), Diagnostic> {
        match self.peek().kind {
            TokenKind::FatArrow => {
                self.bump();
                Ok(())
            }
            _ => Err(Diagnostic::parse(self.peek().span, "expected `=>`")),
        }
    }

    fn expect_lparen(&mut self) -> Result<(), Diagnostic> {
        match self.peek().kind {
            TokenKind::LParen => {
                self.bump();
                Ok(())
            }
            _ => Err(Diagnostic::parse(self.peek().span, "expected `(`")),
        }
    }

    fn expect_rparen(&mut self) -> Result<(), Diagnostic> {
        match self.peek().kind {
            TokenKind::RParen => {
                self.bump();
                Ok(())
            }
            TokenKind::Eof => {
                Err(Diagnostic::parse(self.peek().span, "expected `)`").with_incomplete_input())
            }
            _ => Err(Diagnostic::parse(self.peek().span, "expected `)`")),
        }
    }

    fn consume_separators(&mut self) -> bool {
        let mut consumed = false;
        while matches!(self.peek().kind, TokenKind::Newline | TokenKind::Semicolon) {
            self.bump();
            consumed = true;
        }
        consumed
    }

    fn try_consume_module_identifier(&mut self) -> Option<(String, Span)> {
        let TokenKind::Identifier(first) = self.peek().kind.clone() else {
            return None;
        };

        let mut cursor = self.index + 1;
        let mut path = first;
        while matches!(
            self.tokens.get(cursor).map(|token| &token.kind),
            Some(TokenKind::Dot)
        ) && matches!(
            self.tokens.get(cursor + 1).map(|token| &token.kind),
            Some(TokenKind::Identifier(_))
        ) {
            let TokenKind::Identifier(segment) = self.tokens.get(cursor + 1)?.kind.clone() else {
                return None;
            };
            path.push('.');
            path.push_str(&segment);
            cursor += 2;
        }

        if !matches!(
            self.tokens.get(cursor).map(|token| &token.kind),
            Some(TokenKind::Hash)
        ) {
            return None;
        }
        let TokenKind::Identifier(member) = self.tokens.get(cursor + 1)?.kind.clone() else {
            return None;
        };

        let start = self.index;
        let mut merged = self.bump().span;
        while self.index < cursor {
            merged = merged.merge(self.bump().span);
        }
        let hash_span = self.bump().span;
        let member_span = self.bump().span;
        debug_assert_eq!(start + (cursor - start) + 2, self.index);
        Some((
            format!("{path}#{member}"),
            merged.merge(hash_span).merge(member_span),
        ))
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn bump(&mut self) -> &Token {
        let index = self.index;
        self.index += 1;
        &self.tokens[index]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokenMarker {
    Assign,
    Comma,
    Newline,
    Where,
    RParen,
    RBrace,
    Semicolon,
    Other,
}

impl TokenMarker {
    fn from_kind(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::Assign => Self::Assign,
            TokenKind::Comma => Self::Comma,
            TokenKind::Newline => Self::Newline,
            TokenKind::Where => Self::Where,
            TokenKind::RParen => Self::RParen,
            TokenKind::RBrace => Self::RBrace,
            TokenKind::Semicolon => Self::Semicolon,
            _ => Self::Other,
        }
    }
}

fn token_text(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Int(value, kind) => match kind {
            IntLiteralKind::Byte => format!("{value}BY"),
            IntLiteralKind::Short => format!("{value}S"),
            IntLiteralKind::Int => value.to_string(),
            IntLiteralKind::Long => format!("{value}L"),
        },
        TokenKind::Double(value, kind) => match kind {
            FloatLiteralKind::Float => format!("{value}F"),
            FloatLiteralKind::Double => value.to_string(),
        },
        TokenKind::String(value) => format!("\"{value}\""),
        TokenKind::Identifier(value) => value.clone(),
        TokenKind::True => "true".to_string(),
        TokenKind::False => "false".to_string(),
        TokenKind::Null => "null".to_string(),
        TokenKind::Val => "val".to_string(),
        TokenKind::Mutable => "mutable".to_string(),
        TokenKind::Def => "def".to_string(),
        TokenKind::Module => "module".to_string(),
        TokenKind::Import => "import".to_string(),
        TokenKind::As => "as".to_string(),
        TokenKind::Record => "record".to_string(),
        TokenKind::TypeClass => "typeclass".to_string(),
        TokenKind::Instance => "instance".to_string(),
        TokenKind::Theorem => "theorem".to_string(),
        TokenKind::Trust => "trust".to_string(),
        TokenKind::Axiom => "axiom".to_string(),
        TokenKind::Extension => "extension".to_string(),
        TokenKind::Enum => "enum".to_string(),
        TokenKind::Match => "match".to_string(),
        TokenKind::Rule => "rule".to_string(),
        TokenKind::Where => "where".to_string(),
        TokenKind::Cleanup => "cleanup".to_string(),
        TokenKind::If => "if".to_string(),
        TokenKind::Else => "else".to_string(),
        TokenKind::Foreach => "foreach".to_string(),
        TokenKind::In => "in".to_string(),
        TokenKind::Then => "then".to_string(),
        TokenKind::While => "while".to_string(),
        TokenKind::Newline => "\n".to_string(),
        TokenKind::Semicolon => ";".to_string(),
        TokenKind::Comma => ",".to_string(),
        TokenKind::Colon => ":".to_string(),
        TokenKind::Dot => ".".to_string(),
        TokenKind::Plus => "+".to_string(),
        TokenKind::Minus => "-".to_string(),
        TokenKind::Star => "*".to_string(),
        TokenKind::Slash => "/".to_string(),
        TokenKind::Bang => "!".to_string(),
        TokenKind::Caret => "^".to_string(),
        TokenKind::Ampersand => "&".to_string(),
        TokenKind::Pipe => "|".to_string(),
        TokenKind::Hash => "#".to_string(),
        TokenKind::Percent => "%".to_string(),
        TokenKind::PlusEqual => "+=".to_string(),
        TokenKind::MinusEqual => "-=".to_string(),
        TokenKind::StarEqual => "*=".to_string(),
        TokenKind::SlashEqual => "/=".to_string(),
        TokenKind::Assign => "=".to_string(),
        TokenKind::EqualEqual => "==".to_string(),
        TokenKind::BangEqual => "!=".to_string(),
        TokenKind::Less => "<".to_string(),
        TokenKind::LessEqual => "<=".to_string(),
        TokenKind::Greater => ">".to_string(),
        TokenKind::GreaterEqual => ">=".to_string(),
        TokenKind::AndAnd => "&&".to_string(),
        TokenKind::OrOr => "||".to_string(),
        TokenKind::FatArrow => "=>".to_string(),
        TokenKind::LParen => "(".to_string(),
        TokenKind::RParen => ")".to_string(),
        TokenKind::LBracket => "[".to_string(),
        TokenKind::RBracket => "]".to_string(),
        TokenKind::LBrace => "{".to_string(),
        TokenKind::RBrace => "}".to_string(),
        TokenKind::Eof => String::new(),
    }
}

fn needs_spacing_between(previous: Option<&TokenKind>, next: Option<&TokenKind>) -> bool {
    let (Some(previous), Some(next)) = (previous, next) else {
        return false;
    };
    fn is_word(kind: &TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Identifier(_)
                | TokenKind::Int(..)
                | TokenKind::Double(..)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Null
                | TokenKind::Val
                | TokenKind::Mutable
                | TokenKind::Def
                | TokenKind::Module
                | TokenKind::Import
                | TokenKind::As
                | TokenKind::Record
                | TokenKind::TypeClass
                | TokenKind::Instance
                | TokenKind::Theorem
                | TokenKind::Trust
                | TokenKind::Axiom
                | TokenKind::Where
                | TokenKind::Cleanup
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Foreach
                | TokenKind::In
                | TokenKind::Then
                | TokenKind::While
        )
    }
    is_word(previous) && is_word(next)
}

fn rewrap_span(expr: Expr, span: Span) -> Expr {
    match expr {
        Expr::Int { value, kind, .. } => Expr::Int { value, kind, span },
        Expr::Double { value, kind, .. } => Expr::Double { value, kind, span },
        Expr::Bool { value, .. } => Expr::Bool { value, span },
        Expr::String { value, .. } => Expr::String { value, span },
        Expr::Null { .. } => Expr::Null { span },
        Expr::Unit { .. } => Expr::Unit { span },
        Expr::Identifier { name, .. } => Expr::Identifier { name, span },
        Expr::ModuleHeader { name, .. } => Expr::ModuleHeader { name, span },
        Expr::Import {
            path,
            alias,
            members,
            excludes,
            ..
        } => Expr::Import {
            path,
            alias,
            members,
            excludes,
            span,
        },
        Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            ..
        } => Expr::RecordDeclaration {
            name,
            type_params,
            fields,
            span,
        },
        Expr::RecordLiteral { fields, .. } => Expr::RecordLiteral { fields, span },
        Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            ..
        } => Expr::TypeClassDeclaration {
            name,
            type_params,
            methods,
            span,
        },
        Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods,
            ..
        } => Expr::InstanceDeclaration {
            class_name,
            for_type,
            for_type_annotation,
            constraints,
            methods,
            span,
        },
        Expr::TheoremDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            body,
            trusted,
            ..
        } => Expr::TheoremDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            body,
            trusted,
            span,
        },
        Expr::AxiomDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            ..
        } => Expr::AxiomDeclaration {
            name,
            params,
            param_annotations,
            proposition,
            span,
        },
        Expr::ExtensionDeclaration {
            type_params,
            this_name,
            receiver_type,
            methods,
            ..
        } => Expr::ExtensionDeclaration {
            type_params,
            this_name,
            receiver_type,
            methods,
            span,
        },
        Expr::EnumDeclaration {
            name,
            type_params,
            variants,
            ..
        } => Expr::EnumDeclaration {
            name,
            type_params,
            variants,
            span,
        },
        Expr::Match {
            scrutinee, arms, ..
        } => Expr::Match {
            scrutinee,
            arms,
            span,
        },
        Expr::PegRuleBlock { .. } => Expr::PegRuleBlock { span },
        Expr::VarDecl {
            mutable,
            name,
            annotation,
            value,
            ..
        } => Expr::VarDecl {
            mutable,
            name,
            annotation,
            value,
            span,
        },
        Expr::DefDecl {
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body,
            ..
        } => Expr::DefDecl {
            name,
            type_params,
            constraints,
            params,
            param_annotations,
            return_annotation,
            body,
            span,
        },
        Expr::Lambda {
            params,
            param_annotations,
            body,
            ..
        } => Expr::Lambda {
            params,
            param_annotations,
            body,
            span,
        },
        Expr::Assign { name, value, .. } => Expr::Assign { name, value, span },
        Expr::Unary { op, expr, .. } => Expr::Unary { op, expr, span },
        Expr::Binary { lhs, op, rhs, .. } => Expr::Binary { lhs, op, rhs, span },
        Expr::Call {
            callee, arguments, ..
        } => Expr::Call {
            callee,
            arguments,
            span,
        },
        Expr::FieldAccess { target, field, .. } => Expr::FieldAccess {
            target,
            field,
            span,
        },
        Expr::Cleanup { body, cleanup, .. } => Expr::Cleanup {
            body,
            cleanup,
            span,
        },
        Expr::RecordConstructor {
            name, arguments, ..
        } => Expr::RecordConstructor {
            name,
            arguments,
            span,
        },
        Expr::ListLiteral { elements, .. } => Expr::ListLiteral { elements, span },
        Expr::MapLiteral { entries, .. } => Expr::MapLiteral { entries, span },
        Expr::SetLiteral { elements, .. } => Expr::SetLiteral { elements, span },
        Expr::If {
            condition,
            then_branch,
            else_branch,
            ..
        } => Expr::If {
            condition,
            then_branch,
            else_branch,
            span,
        },
        Expr::While {
            condition, body, ..
        } => Expr::While {
            condition,
            body,
            span,
        },
        Expr::Foreach {
            binding,
            iterable,
            body,
            ..
        } => Expr::Foreach {
            binding,
            iterable,
            body,
            span,
        },
        Expr::Block { expressions, .. } => Expr::Block { expressions, span },
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOp, Expr, parse_source};
    use klassic_span::SourceFile;

    #[test]
    fn nested_block_comments_are_skipped() {
        let file = SourceFile::new("test.kl", "1 + /* outer /* inner */ outer */ 2");
        let expr = parse_source(&file).expect("expression should parse");
        match expr {
            Expr::Binary {
                op: BinaryOp::Add, ..
            } => {}
            other => panic!("unexpected expression: {other:?}"),
        }
    }

    #[test]
    fn line_comments_are_skipped() {
        let file = SourceFile::new("test.kl", "1 + // comment\n2");
        let expr = parse_source(&file).expect("expression should parse");
        match expr {
            Expr::Binary {
                op: BinaryOp::Add, ..
            } => {}
            other => panic!("unexpected expression: {other:?}"),
        }
    }

    #[test]
    fn parses_lambda_and_function_definition() {
        let file = SourceFile::new(
            "test.kl",
            "def add(x, y) = x + y\nval f = (x, y) => add(x, y)\nf(1, 2)",
        );
        let expr = parse_source(&file).expect("program should parse");
        match expr {
            Expr::Block { expressions, .. } => assert_eq!(expressions.len(), 3),
            other => panic!("unexpected expression: {other:?}"),
        }

        let zero_arg = parse_source(&SourceFile::new("test.kl", "stopwatch( => { 1 })"))
            .expect("zero-arg lambda should parse");
        match zero_arg {
            Expr::Call { arguments, .. } => assert_eq!(arguments.len(), 1),
            other => panic!("unexpected zero-arg lambda expression: {other:?}"),
        }
    }

    #[test]
    fn parses_if_and_assignments() {
        let file = SourceFile::new("test.kl", "mutable a = 1\nif(a < 2) {\n  a += 1\n}\na");
        let expr = parse_source(&file).expect("program should parse");
        match expr {
            Expr::Block { expressions, .. } => assert_eq!(expressions.len(), 3),
            other => panic!("unexpected expression: {other:?}"),
        }
    }

    #[test]
    fn parses_list_literals_and_foreach() {
        let file = SourceFile::new("test.kl", "foreach(x in [1 2\n3]) {\n  x\n}");
        let expr = parse_source(&file).expect("program should parse");
        match expr {
            Expr::Foreach { .. } => {}
            other => panic!("unexpected expression: {other:?}"),
        }
    }

    #[test]
    fn parses_list_map_and_reduce_syntax() {
        let map = parse_source(&SourceFile::new("test.kl", "[1 2 3] map x => x + 1"))
            .expect("map syntax should parse");
        match map {
            Expr::Call {
                callee, arguments, ..
            } => {
                assert_eq!(arguments.len(), 1);
                match *callee {
                    Expr::Call {
                        callee, arguments, ..
                    } => {
                        assert_eq!(arguments.len(), 1);
                        match *callee {
                            Expr::Identifier { name, .. } => assert_eq!(name, "map"),
                            other => panic!("unexpected inner map callee: {other:?}"),
                        }
                    }
                    other => panic!("unexpected map callee: {other:?}"),
                }
            }
            other => panic!("unexpected map syntax expression: {other:?}"),
        }

        let reduce = parse_source(&SourceFile::new("test.kl", "[1 2 3] reduce 0 => r + e"))
            .expect("reduce syntax should parse");
        match reduce {
            Expr::Call {
                callee, arguments, ..
            } => {
                assert_eq!(arguments.len(), 1);
                match *callee {
                    Expr::Call {
                        callee, arguments, ..
                    } => {
                        assert_eq!(arguments.len(), 1);
                        match *callee {
                            Expr::Call {
                                callee, arguments, ..
                            } => {
                                assert_eq!(arguments.len(), 1);
                                match *callee {
                                    Expr::Identifier { name, .. } => assert_eq!(name, "foldLeft"),
                                    other => panic!("unexpected inner reduce callee: {other:?}"),
                                }
                            }
                            other => panic!("unexpected middle reduce callee: {other:?}"),
                        }
                    }
                    other => panic!("unexpected reduce callee: {other:?}"),
                }
            }
            other => panic!("unexpected reduce syntax expression: {other:?}"),
        }

        let trailing = parse_source(&SourceFile::new("test.kl", "map([1 2 3]){x => x + 1}"))
            .expect("trailing lambda call should parse");
        match trailing {
            Expr::Call {
                callee, arguments, ..
            } => {
                assert_eq!(arguments.len(), 1);
                assert!(matches!(*callee, Expr::Call { .. }));
            }
            other => panic!("unexpected trailing lambda expression: {other:?}"),
        }
    }

    #[test]
    fn parses_module_function_calls() {
        let file = SourceFile::new("test.kl", "FileOutput#write(\"x\", \"y\")");
        let expr = parse_source(&file).expect("program should parse");
        match expr {
            Expr::Call { callee, .. } => match *callee {
                Expr::Identifier { name, .. } => assert_eq!(name, "FileOutput#write"),
                other => panic!("unexpected callee: {other:?}"),
            },
            other => panic!("unexpected expression: {other:?}"),
        }

        let infix = parse_source(&SourceFile::new("test.kl", "%[\"x\": 1] Map#get \"x\""))
            .expect("infix module call should parse");
        match infix {
            Expr::Call { arguments, .. } => assert_eq!(arguments.len(), 2),
            other => panic!("unexpected infix expression: {other:?}"),
        }

        let fqcn = parse_source(&SourceFile::new("test.kl", "user.util#inc(41)"))
            .expect("fqcn selector should parse");
        match fqcn {
            Expr::Call { callee, .. } => match *callee {
                Expr::Identifier { name, .. } => assert_eq!(name, "user.util#inc"),
                other => panic!("unexpected fqcn callee: {other:?}"),
            },
            other => panic!("unexpected fqcn expression: {other:?}"),
        }

        let infix_hash = parse_source(&SourceFile::new("test.kl", "3 #cons []"))
            .expect("hash infix call should parse");
        match infix_hash {
            Expr::Call {
                callee, arguments, ..
            } => {
                assert_eq!(arguments.len(), 1);
                match *callee {
                    Expr::Call {
                        callee, arguments, ..
                    } => {
                        assert_eq!(arguments.len(), 1);
                        match *callee {
                            Expr::Identifier { name, .. } => assert_eq!(name, "cons"),
                            other => panic!("unexpected inner hash infix callee: {other:?}"),
                        }
                    }
                    other => panic!("unexpected hash infix callee: {other:?}"),
                }
            }
            other => panic!("unexpected hash infix expression: {other:?}"),
        }
    }

    #[test]
    fn parses_cleanup_suffix() {
        let expr = parse_source(&SourceFile::new(
            "test.kl",
            "while(true) { 1 } cleanup { 2 }",
        ))
        .expect("cleanup expression should parse");
        match expr {
            Expr::Cleanup { body, cleanup, .. } => {
                assert!(matches!(*body, Expr::While { .. }));
                assert!(matches!(*cleanup, Expr::Block { .. }));
            }
            other => panic!("unexpected cleanup expression: {other:?}"),
        }
    }

    #[test]
    fn parses_dynamic_casts_as_expressions() {
        let expr = parse_source(&SourceFile::new("test.kl", "val s: * = (100 :> *)\ns"))
            .expect("dynamic cast should parse");
        match expr {
            Expr::Block { expressions, .. } => assert_eq!(expressions.len(), 2),
            other => panic!("unexpected cast program: {other:?}"),
        }

        let null_expr = parse_source(&SourceFile::new("test.kl", "val f: ('a) => 'b = null\nf"))
            .expect("null should parse");
        match null_expr {
            Expr::Block { expressions, .. } => assert_eq!(expressions.len(), 2),
            other => panic!("unexpected null program: {other:?}"),
        }
    }

    #[test]
    fn parses_map_and_set_literals() {
        let map = parse_source(&SourceFile::new("test.kl", "%[\"a\": 1 \"b\": 2]"))
            .expect("map should parse");
        match map {
            Expr::MapLiteral { entries, .. } => assert_eq!(entries.len(), 2),
            other => panic!("unexpected map expression: {other:?}"),
        }

        let set = parse_source(&SourceFile::new("test.kl", "%(1 2 3)")).expect("set should parse");
        match set {
            Expr::SetLiteral { elements, .. } => assert_eq!(elements.len(), 3),
            other => panic!("unexpected set expression: {other:?}"),
        }
    }

    #[test]
    fn incomplete_input_is_marked() {
        let file = SourceFile::new("test.kl", "while(true) {");
        let error = parse_source(&file).expect_err("parse should fail");
        assert!(error.is_incomplete());
    }

    #[test]
    fn parses_module_headers_and_imports() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "module foo.bar\nimport Map as M\nimport Map.{size, get => _}\nsize(%[\"a\": 1])",
        ))
        .expect("module/import program should parse");
        match program {
            Expr::Block { expressions, .. } => {
                assert!(matches!(expressions[0], Expr::ModuleHeader { .. }));
                assert!(matches!(
                    expressions[1],
                    Expr::Import {
                        alias: Some(_),
                        members: None,
                        ..
                    }
                ));
                assert!(matches!(
                    &expressions[2],
                    Expr::Import {
                        alias: None,
                        members: Some(_),
                        excludes,
                        ..
                    } if *excludes == vec!["get".to_string()]
                ));
            }
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_record_declarations_and_access() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "record Tuple<'a, 'b> {\n  _1: 'a\n  _2: 'b\n}\nval t = #Tuple(1, 2)\nt._1",
        ))
        .expect("record program should parse");
        match program {
            Expr::Block { expressions, .. } => {
                match &expressions[0] {
                    Expr::RecordDeclaration {
                        type_params,
                        fields,
                        ..
                    } => {
                        assert_eq!(type_params, &vec!["'a".to_string(), "'b".to_string()]);
                        assert_eq!(fields.len(), 2);
                        assert_eq!(fields[0].name, "_1");
                        assert_eq!(
                            fields[0].annotation.as_ref().map(|ty| ty.text.as_str()),
                            Some("'a")
                        );
                        assert_eq!(fields[1].name, "_2");
                        assert_eq!(
                            fields[1].annotation.as_ref().map(|ty| ty.text.as_str()),
                            Some("'b")
                        );
                    }
                    other => panic!("unexpected record declaration: {other:?}"),
                }
                assert!(matches!(expressions[1], Expr::VarDecl { .. }));
                assert!(matches!(expressions[2], Expr::FieldAccess { .. }));
            }
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_structural_record_literals() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "val show_int = record { show: (x: Int) => \"Int(\" + x + \")\" }\nshow_int.show(42)",
        ))
        .expect("record literal program should parse");
        match program {
            Expr::Block { expressions, .. } => {
                assert!(matches!(expressions[0], Expr::VarDecl { .. }));
                assert!(matches!(expressions[1], Expr::Call { .. }));
            }
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_embedded_macro_peg_rule_blocks() {
        let program = parse_source(&SourceFile::new("test.kl", "rule {\n  S = \"a\";\n}\n1"))
            .expect("embedded macro peg should parse");
        match program {
            Expr::Block { expressions, .. } => {
                assert!(matches!(expressions[0], Expr::PegRuleBlock { .. }));
                assert!(matches!(expressions[1], Expr::Int { value: 1, .. }));
            }
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_typeclass_and_instance_declarations() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<Int> where {\n  def show(x: Int): String = \"Int: \" + x\n}\nshow(42)",
        ))
        .expect("typeclass program should parse");
        match program {
            Expr::Block { expressions, .. } => {
                assert!(matches!(expressions[0], Expr::TypeClassDeclaration { .. }));
                assert!(matches!(expressions[1], Expr::InstanceDeclaration { .. }));
                assert!(matches!(expressions[2], Expr::Call { .. }));
            }
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_constrained_polymorphic_function_declarations() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ndef display<'a>(x: 'a): String where Show<'a> = show(x)",
        ))
        .expect("constrained function should parse");
        match program {
            Expr::Block { expressions, .. } => match &expressions[1] {
                Expr::DefDecl {
                    type_params,
                    constraints,
                    ..
                } => {
                    assert_eq!(type_params, &vec!["'a".to_string()]);
                    assert_eq!(constraints.len(), 1);
                    assert_eq!(constraints[0].class_name, "Show");
                    assert_eq!(constraints[0].arguments.len(), 1);
                    assert_eq!(constraints[0].arguments[0].text, "'a");
                }
                other => panic!("unexpected def declaration: {other:?}"),
            },
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_inline_constraint_shorthand() {
        // `<Show 'a>` is sugar for `<'a> where Show<'a>`.
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ndef display<Show 'a>(x: 'a): String = show(x)",
        ))
        .expect("inline-constraint function should parse");
        match program {
            Expr::Block { expressions, .. } => match &expressions[1] {
                Expr::DefDecl {
                    type_params,
                    constraints,
                    ..
                } => {
                    assert_eq!(type_params, &vec!["'a".to_string()]);
                    assert_eq!(constraints.len(), 1);
                    assert_eq!(constraints[0].class_name, "Show");
                    assert_eq!(constraints[0].arguments.len(), 1);
                    assert_eq!(constraints[0].arguments[0].text, "'a");
                }
                other => panic!("unexpected def declaration: {other:?}"),
            },
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_inline_constraint_shorthand_multiple() {
        // Two constraints on the same type variable, plus another
        // type variable carrying its own constraint.
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ntypeclass Eq<'a> where {\n  equals: ('a, 'a) => Boolean\n}\ndef both<Show 'a, Eq 'a, Show 'b>(x: 'a, y: 'a, z: 'b): String = show(x)",
        ))
        .expect("multi-constraint function should parse");
        match program {
            Expr::Block { expressions, .. } => match &expressions[2] {
                Expr::DefDecl {
                    type_params,
                    constraints,
                    ..
                } => {
                    // 'a appears twice in the source but is only added
                    // to type_params once; 'b is a separate parameter.
                    assert_eq!(type_params, &vec!["'a".to_string(), "'b".to_string()]);
                    assert_eq!(constraints.len(), 3);
                    assert_eq!(constraints[0].class_name, "Show");
                    assert_eq!(constraints[0].arguments[0].text, "'a");
                    assert_eq!(constraints[1].class_name, "Eq");
                    assert_eq!(constraints[1].arguments[0].text, "'a");
                    assert_eq!(constraints[2].class_name, "Show");
                    assert_eq!(constraints[2].arguments[0].text, "'b");
                }
                other => panic!("unexpected def declaration: {other:?}"),
            },
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn inline_and_where_constraints_compose() {
        // `<Show 'a>` plus an explicit `where Eq<'a>` clause.
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ntypeclass Eq<'a> where {\n  equals: ('a, 'a) => Boolean\n}\ndef both<Show 'a>(x: 'a, y: 'a): String where Eq<'a> = show(x)",
        ))
        .expect("mixed inline+where function should parse");
        match program {
            Expr::Block { expressions, .. } => match &expressions[2] {
                Expr::DefDecl { constraints, .. } => {
                    assert_eq!(constraints.len(), 2);
                    // `where` clauses are parsed first, inline ones
                    // appended after.
                    assert_eq!(constraints[0].class_name, "Eq");
                    assert_eq!(constraints[1].class_name, "Show");
                }
                other => panic!("unexpected def declaration: {other:?}"),
            },
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_instance_constraints() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "typeclass Show<'a> where {\n  show: ('a) => String\n}\ninstance Show<List<'a>> where Show<'a> {\n  def show(xs: List<'a>): String = \"ok\"\n}",
        ))
        .expect("instance constraints should parse");
        match program {
            Expr::Block { expressions, .. } => match &expressions[1] {
                Expr::InstanceDeclaration { constraints, .. } => {
                    assert_eq!(constraints.len(), 1);
                    assert_eq!(constraints[0].class_name, "Show");
                    assert_eq!(constraints[0].arguments[0].text, "'a");
                }
                other => panic!("unexpected instance declaration: {other:?}"),
            },
            other => panic!("unexpected program: {other:?}"),
        }
    }

    #[test]
    fn parses_theorem_and_axiom_declarations() {
        let program = parse_source(&SourceFile::new(
            "test.kl",
            "trust theorem foo(x: Int): { true } = assert(true)\naxiom bar(): { foo(1) }",
        ))
        .expect("proof program should parse");
        match program {
            Expr::Block { expressions, .. } => {
                assert!(matches!(
                    expressions[0],
                    Expr::TheoremDeclaration { trusted: true, .. }
                ));
                assert!(matches!(expressions[1], Expr::AxiomDeclaration { .. }));
            }
            other => panic!("unexpected program: {other:?}"),
        }
    }
}
