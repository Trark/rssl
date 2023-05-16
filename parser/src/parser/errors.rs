use super::*;

/// Provides details on why a parse operation failed
#[derive(PartialEq, Clone)]
pub struct ParseError(pub ParseErrorReason, pub Option<Vec<LexToken>>);

impl ParseError {
    /// Make an error for when there were unused tokens after parsing
    pub fn from_tokens_remaining(remaining: &[LexToken]) -> Self {
        ParseError(ParseErrorReason::TokensUnconsumed, Some(remaining.to_vec()))
    }
}

impl<'a> From<ParseErrorContext<'a>> for ParseError {
    fn from(internal_error: ParseErrorContext<'a>) -> ParseError {
        ParseError(internal_error.1, Some(internal_error.0.to_vec()))
    }
}

impl std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "ParseError({:?}, {:?})",
            self.0,
            self.1
                .as_ref()
                .map(|vec| if vec.len() > 12 { &vec[..12] } else { vec })
        )
    }
}

impl CompileError for ParseError {
    fn print(&self, w: &mut MessagePrinter) -> std::fmt::Result {
        // Find the failing location
        let loc = match &self.1 {
            Some(tokens) => match tokens.as_slice() {
                [LexToken(_, loc), ..] => *loc,
                _ => SourceLocation::UNKNOWN,
            },
            _ => SourceLocation::UNKNOWN,
        };

        match &self.0 {
            ParseErrorReason::InvalidSlotType(slot_string) => w.write_message(
                &|f| write!(f, "register type is not supported in '{slot_string}'"),
                loc,
                Severity::Error,
            ),
            ParseErrorReason::InvalidSlotIndex(slot_string) => w.write_message(
                &|f| write!(f, "register index should be an integer in '{slot_string}'"),
                loc,
                Severity::Error,
            ),
            ParseErrorReason::InvalidSpaceIdentifier(space_string) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "expect space identifier of form 'spaceX' but received '{space_string}'"
                    )
                },
                loc,
                Severity::Error,
            ),
            _ => w.write_message(
                &|f| write!(f, "failed to parse source"),
                loc,
                Severity::Error,
            ),
        }
    }
}

/// The basic reason for a parse failure
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub enum ParseErrorReason {
    UnexpectedEndOfStream,
    TokensUnconsumed,
    WrongToken,
    SymbolIsNotAType,
    InvalidSlotType(String),
    InvalidSlotIndex(String),
    InvalidSpaceIdentifier(String),
    UnexpectedAttribute(String),
}

impl ParseErrorReason {
    pub fn into_result<T>(self, remaining: &[LexToken]) -> ParseResult<T> {
        Err(ParseErrorContext(remaining, self))
    }

    pub fn wrong_token<T>(remaining: &[LexToken]) -> ParseResult<T> {
        Err(ParseErrorContext(remaining, ParseErrorReason::WrongToken))
    }

    pub fn end_of_stream<'t, T>() -> ParseResult<'t, T> {
        Err(ParseErrorContext(
            &[],
            ParseErrorReason::UnexpectedEndOfStream,
        ))
    }
}

/// Result type for internal parse functions
pub type ParseResult<'t, T> = Result<(&'t [LexToken], T), ParseErrorContext<'t>>;

pub trait ParseResultExt {
    /// Choose between two results based on the longest amount they parsed
    /// Favors self over next
    fn select(self, next: Self) -> Self;
}

impl<'t, T> ParseResultExt for ParseResult<'t, T> {
    fn select(self, next: Self) -> Self {
        get_most_relevant_result(self, next)
    }
}

/// Internal error type for propagating error information
#[derive(PartialEq, Debug, Clone)]
pub struct ParseErrorContext<'a>(pub &'a [LexToken], pub ParseErrorReason);

/// Get the significance value for a result
pub fn get_result_significance<T>(result: &ParseResult<T>) -> usize {
    match result {
        Ok((rest, _)) => rest.len() * 2,
        Err(ParseErrorContext(rest, _)) => rest.len() * 2 + 1,
    }
}

/// Find the result with the longest tokens used
fn get_most_relevant_result<'a: 'c, 'b: 'c, 'c, T>(
    lhs: ParseResult<'a, T>,
    rhs: ParseResult<'b, T>,
) -> ParseResult<'c, T> {
    let lhs_remaining = get_result_significance(&lhs);
    let rhs_remaining = get_result_significance(&rhs);
    if rhs_remaining < lhs_remaining {
        rhs
    } else {
        lhs
    }
}
