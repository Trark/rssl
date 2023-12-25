//! Tokens
//!
//! The tokens module contains all the definitions for tokens used by the RSSL lexer.

use crate::*;

/// An arbitrary identifier token string
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone)]
pub struct Identifier(pub String);

impl std::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "\"{}\"", self.0)
    }
}

/// Marker for if a token is followed directly by the next token or if there is whitespace between them
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum FollowedBy {
    Token,
    Whitespace,
}

/// Any token that may appear in an RSSL file
#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Eof, // Marks the end of a stream

    Id(Identifier),

    /// Literal int without an explicit type
    /// These do not have sign. The - is an operator on the literal
    LiteralInt(u64),

    /// Literal int with an explicit unsigned type
    LiteralIntUnsigned32(u64),

    /// Literal int with an explicit uint64_t type
    LiteralIntUnsigned64(u64),

    /// Literal int with an explicit int64_t type
    LiteralIntSigned64(i64),

    /// Literal float without an explicit type
    LiteralFloat(f64),

    /// Literal float with an explicit half type
    LiteralFloat16(f32),

    /// Literal float with an explicit float type
    LiteralFloat32(f32),

    /// Literal float with an explicit double type
    LiteralFloat64(f64),

    LiteralString(String),
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
    HashHash,
    At,
    ExclamationPoint,
    ExclamationPointEquals,
    Tilde,
    Period,

    If,
    Else,
    For,
    While,
    Do,
    Switch,
    Return,
    Break,
    Continue,
    Discard,
    Case,
    Default,

    Struct,
    Enum,
    Typedef,
    ConstantBuffer,
    Register,
    PackOffset,
    Namespace,
    Colon,
    ScopeResolution,

    In,
    Out,
    InOut,

    Const,
    Volatile,
    RowMajor,
    ColumnMajor,
    Unorm,
    Snorm,

    Extern,
    Static,
    Inline,
    GroupShared,
    Constexpr,

    SizeOf,

    Template,
    Typename,
    Decltype,

    ReservedWord(String),

    /// Header name in <>
    HeaderName(String),

    /// Line ending
    Endline,

    /// Physical line ending which does not end the logical line
    PhysicalEndline,

    /// Non-line ending whitespace
    Whitespace,

    /// Line and block comments
    Comment,

    /// Temporary reference to macro argument used during define processing
    MacroArg(u32),

    /// Temporary version of [Token::HashHash] for a concat operation which is pending
    Concat,
}

impl Token {
    /// Test if the token is for whitespace, endlines, or comments
    pub fn is_whitespace(&self) -> bool {
        matches!(
            self,
            Token::Endline | Token::PhysicalEndline | Token::Whitespace | Token::Comment
        )
    }
}

/// A [Token] for use during preprocessing
#[derive(PartialEq, Clone)]
pub struct PreprocessToken(
    /// The base token
    pub Token,
    /// Additional metadata for the token
    pub PreprocessTokenData,
);

/// Additional data associated with a token for the preprocessing phase
#[derive(PartialEq, Eq, Clone)]
pub struct PreprocessTokenData {
    /// The location the token is sourced from
    start_location: SourceLocation,

    /// The location after the token
    /// Must be from the same file and after start_location
    end_location: SourceLocation,
}

/// A [Token] with source location information attached
#[derive(PartialEq, Clone)]
pub struct LexToken(pub Token, pub SourceLocation);

impl PreprocessToken {
    /// Construct a new token with the given source range
    pub fn new(
        tok: Token,
        base_location: SourceLocation,
        start_offset: u32,
        end_offset: u32,
    ) -> Self {
        if base_location == SourceLocation::UNKNOWN {
            Self::without_location(tok)
        } else {
            assert!(start_offset <= end_offset);
            PreprocessToken(
                tok,
                PreprocessTokenData {
                    start_location: base_location.offset(start_offset),
                    end_location: base_location.offset(end_offset),
                },
            )
        }
    }

    /// Construct a new token with the unknown source range
    pub fn without_location(tok: Token) -> Self {
        PreprocessToken(
            tok,
            PreprocessTokenData {
                start_location: SourceLocation::UNKNOWN,
                end_location: SourceLocation::UNKNOWN,
            },
        )
    }
}

impl Locate for PreprocessToken {
    fn get_location(&self) -> SourceLocation {
        self.1.start_location
    }
}

impl LocateEnd for PreprocessToken {
    fn get_end_location(&self) -> SourceLocation {
        self.1.end_location
    }
}

impl Locate for PreprocessTokenData {
    fn get_location(&self) -> SourceLocation {
        self.start_location
    }
}

impl LocateEnd for PreprocessTokenData {
    fn get_end_location(&self) -> SourceLocation {
        self.end_location
    }
}

impl Locate for Option<&PreprocessToken> {
    fn get_location(&self) -> SourceLocation {
        match self {
            Some(tok) => tok.1.start_location,
            None => SourceLocation::UNKNOWN,
        }
    }
}

impl LocateEnd for Option<&PreprocessToken> {
    fn get_end_location(&self) -> SourceLocation {
        match self {
            Some(tok) => tok.1.end_location,
            None => SourceLocation::UNKNOWN,
        }
    }
}

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

impl std::fmt::Debug for PreprocessToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{:?} @ {}-{}",
            self.0,
            self.1.start_location.get_raw(),
            self.1.end_location.get_raw()
        )
    }
}

impl std::fmt::Debug for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} @ {}", self.0, self.1.get_raw())
    }
}
