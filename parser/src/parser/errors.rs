use super::*;

/// Provides details on why a parse operation failed
#[derive(PartialEq, Clone)]
pub struct ParseError(pub ParseErrorReason, pub Option<Vec<LexToken>>);

impl ParseError {
    /// Get formatter to print the error
    pub fn display<'a>(&'a self, source_manager: &'a SourceManager) -> ParserErrorPrinter<'a> {
        ParserErrorPrinter(self, source_manager)
    }

    /// Make an error for when there were unused tokens after parsing
    pub fn from_tokens_remaining(remaining: &[LexToken]) -> Self {
        ParseError(ParseErrorReason::FailedToParse, Some(remaining.to_vec()))
    }
}

impl<'a> From<nom::Err<ParseErrorContext<'a>>> for ParseError {
    fn from(internal_error: nom::Err<ParseErrorContext<'a>>) -> ParseError {
        match internal_error {
            nom::Err::Error(ParseErrorContext(rest, err)) => ParseError(err, Some(rest.to_vec())),
            nom::Err::Failure(ParseErrorContext(rest, err)) => ParseError(err, Some(rest.to_vec())),
            nom::Err::Incomplete(_) => ParseError(ParseErrorReason::UnexpectedEndOfStream, None),
        }
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

/// Prints parser errors
pub struct ParserErrorPrinter<'a>(&'a ParseError, &'a SourceManager);

impl<'a> std::fmt::Display for ParserErrorPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let ParserErrorPrinter(err, source_manager) = self;

        // Find the failing location
        let loc_opt = match &err.1 {
            Some(tokens) => match tokens.as_slice() {
                [LexToken(_, loc), ..] => Some(*loc),
                _ => None,
            },
            _ => None,
        };

        // Get file location info
        let file_location = match loc_opt {
            Some(loc) => source_manager.get_file_location(loc),
            None => FileLocation::Unknown,
        };

        // Print basic failure reason
        writeln!(f, "{}: Failed to parse source", file_location)?;

        // Print source that caused the error
        source_manager.write_source_for_error(f, loc_opt)
    }
}

/// The basic reason for a parse failure
#[derive(PartialEq, Debug, Clone)]
pub enum ParseErrorReason {
    Unknown,
    UnexpectedEndOfStream,
    TokensUnconsumed,
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

impl ParseErrorReason {
    pub fn into_result<T>(self, remaining: &[LexToken]) -> ParseResult<T> {
        Err(nom::Err::Error(ParseErrorContext(remaining, self)))
    }

    pub fn wrong_token<T>(remaining: &[LexToken]) -> ParseResult<T> {
        Err(nom::Err::Error(ParseErrorContext(
            remaining,
            ParseErrorReason::WrongToken,
        )))
    }

    pub fn end_of_stream<'t, T>() -> ParseResult<'t, T> {
        Err(nom::Err::Error(ParseErrorContext(
            &[],
            ParseErrorReason::UnexpectedEndOfStream,
        )))
    }
}

/// Result type for internal parse functions
pub type ParseResult<'t, T> = nom::IResult<&'t [LexToken], T, ParseErrorContext<'t>>;

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

impl<'a> nom::error::ParseError<&'a [LexToken]> for ParseErrorContext<'a> {
    fn from_error_kind(input: &'a [LexToken], kind: nom::error::ErrorKind) -> Self {
        ParseErrorContext(input, ParseErrorReason::ErrorKind(kind))
    }

    fn append(_: &[LexToken], _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }

    fn or(self, other: Self) -> Self {
        if other.0.len() < self.0.len() {
            other
        } else {
            self
        }
    }
}

/// Get the significance value for a result
fn get_result_significance<T>(result: &ParseResult<T>) -> usize {
    match result {
        Ok((rest, _)) => rest.len(),
        Err(nom::Err::Incomplete(_)) => 0,
        Err(nom::Err::Error(ParseErrorContext(rest, _))) => rest.len(),
        Err(nom::Err::Failure(ParseErrorContext(rest, _))) => rest.len(),
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
