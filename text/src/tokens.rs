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
    LiteralInt(u64), // Int (literals do not have sign, the - is an operator on the literal)
    LiteralUInt(u64), // Int with explicit unsigned type
    LiteralLong(u64), // Int with explicit long type
    LiteralHalf(f32),
    LiteralFloat(f32),
    LiteralDouble(f64),
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
    Concat,
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
    Discard,

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

    Extern,
    Static,
    GroupShared,

    SizeOf,

    Template,
    Typename,

    ReservedWord(String),

    /// Line ending
    Endline,

    /// Physical line ending which does not end the logical line
    PhysicalEndline,

    /// Non-line ending whitespace
    Whitespace,

    /// Line and block comments
    Comment,
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
    location: SourceLocation,
}

/// A [Token] with source location information attached
#[derive(PartialEq, Clone)]
pub struct LexToken(pub Token, pub SourceLocation);

impl PreprocessToken {
    pub fn new(tok: Token, base_location: SourceLocation, offset: u32) -> Self {
        PreprocessToken(
            tok,
            PreprocessTokenData {
                location: base_location.offset(offset),
            },
        )
    }
}

impl Locate for PreprocessToken {
    fn get_location(&self) -> SourceLocation {
        self.1.location
    }
}

impl Locate for PreprocessTokenData {
    fn get_location(&self) -> SourceLocation {
        self.location
    }
}

impl Locate for Option<&PreprocessToken> {
    fn get_location(&self) -> SourceLocation {
        match self {
            Some(tok) => tok.1.location,
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
        write!(f, "{:?} @ {}", self.0, self.1.location.get_raw())
    }
}

impl std::fmt::Debug for LexToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} @ {}", self.0, self.1.get_raw())
    }
}
