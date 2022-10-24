use rssl_text::*;

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct Identifier(pub String);

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum FollowedBy {
    Token,
    Whitespace,
}

#[derive(PartialEq, Debug, Clone)]
pub enum RegisterSlot {
    T(u32),
    U(u32),
    B(u32),
    S(u32),
}

#[derive(PartialEq, Debug, Clone)]
pub enum OffsetSlot {
    T(u32),
    U(u32),
    B(u32),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Eof, // Marks the end of a stream

    Id(Identifier),
    LiteralInt(u64), // Int (literals do not have sign, the - is an operator on the literal)
    LiteralUInt(u64), // Int with explicit unsigned type
    LiteralLong(u64), // Int with explicit long type
    LiteralHalf(f32),
    LiteralFloat(f32),
    LiteralDouble(f64),
    True,
    False,

    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftSquareBracket,
    RightSquareBracket,
    LeftAngleBracket(FollowedBy),
    RightAngleBracket(FollowedBy),
    Semicolon,
    Comma,
    QuestionMark,

    Plus,
    PlusPlus,
    PlusEquals,
    Minus,
    MinusMinus,
    MinusEquals,
    ForwardSlash,
    ForwardSlashEquals,
    Percent,
    PercentEquals,
    Asterix,
    AsterixEquals,
    VerticalBar,
    VerticalBarVerticalBar,
    VerticalBarEquals,
    Ampersand,
    AmpersandAmpersand,
    AmpersandEquals,
    Hat,
    HatEquals,
    Equals,
    EqualsEquals,
    Hash,
    At,
    ExclamationPoint,
    ExclamationPointEquals,
    Tilde,
    Period,

    If,
    Else,
    For,
    While,
    Switch,
    Return,
    Break,
    Continue,

    Struct,
    Enum,
    ConstantBuffer,
    Register(RegisterSlot),
    PackOffset(OffsetSlot),
    Namespace,
    Colon,
    ScopeResolution,

    In,
    Out,
    InOut,

    Const,

    Extern,
    Static,
    GroupShared,

    SizeOf,

    Template,
    Typename,

    ReservedWord(String),
}

#[derive(PartialEq, Clone)]
pub struct LexToken(pub Token, pub SourceLocation);

impl LexToken {
    /// Extract the file location from a lex token
    pub fn to_loc(self) -> SourceLocation {
        self.1
    }

    /// Create a token with no file location
    pub fn with_no_loc(token: Token) -> LexToken {
        LexToken(token, SourceLocation::UNKNOWN)
    }
}

impl std::fmt::Debug for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} @ {}", self.0, self.1.get_raw())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Tokens {
    pub stream: Vec<LexToken>,
}
