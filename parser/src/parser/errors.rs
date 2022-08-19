use super::*;

/// Provides details on why a parse operation failed
#[derive(PartialEq, Clone)]
pub struct ParseError(
    pub ParseErrorReason,
    pub Option<Vec<LexToken>>,
    pub Option<Box<ParseError>>,
);

impl std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "ParseError({:?}, {:?}, {:?})",
            self.0,
            self.1
                .as_ref()
                .map(|vec| if vec.len() > 12 { &vec[..12] } else { vec }),
            self.2
        )
    }
}

/// The basic reason for a parse failure
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErrorReason {
    Unknown,
    UnexpectedEndOfStream,
    FailedToParse,
    WrongToken,
    WrongSlotType,
    UnknownType,
    DuplicateStructSymbol,
    DuplicateEnumSymbol,
    SymbolIsNotAStructuredType,
    UnexpectedAttribute(String),
    ErrorKind(nom::error::ErrorKind),
}

/// Result type for internal parse functions
pub type ParseResult<'t, T> = nom::IResult<&'t [LexToken], T, ParseErrorContext<'t>>;

/// Internal error type for propagating error information
#[derive(PartialEq, Debug, Clone)]
pub struct ParseErrorContext<'a>(pub &'a [LexToken], pub ParseErrorReason);

impl<'a> nom::error::ParseError<&'a [LexToken]> for ParseErrorContext<'a> {
    fn from_error_kind(input: &'a [LexToken], kind: nom::error::ErrorKind) -> Self {
        ParseErrorContext(input, ParseErrorReason::ErrorKind(kind))
    }

    fn append(_: &[LexToken], _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

/// Find the error with the longest tokens used
pub fn get_most_relevant_error<'a: 'c, 'b: 'c, 'c>(
    lhs: nom::Err<ParseErrorContext<'a>>,
    rhs: nom::Err<ParseErrorContext<'b>>,
) -> nom::Err<ParseErrorContext<'c>> {
    let lhs_remaining = match lhs {
        nom::Err::Incomplete(_) => 0,
        nom::Err::Error(ParseErrorContext(rest, _)) => rest.len(),
        nom::Err::Failure(ParseErrorContext(rest, _)) => rest.len(),
    };
    let rhs_remaining = match rhs {
        nom::Err::Incomplete(_) => 0,
        nom::Err::Error(ParseErrorContext(rest, _)) => rest.len(),
        nom::Err::Failure(ParseErrorContext(rest, _)) => rest.len(),
    };
    if rhs_remaining < lhs_remaining {
        rhs
    } else {
        lhs
    }
}
