use rssl_ast::*;
use rssl_text::tokens::*;
use rssl_text::*;

/// Parser state for a compilation unit
pub struct Parser {
    /// All input tokens
    tokens: Vec<LexToken>,

    /// Current position
    current: usize,

    /// Number of namespaces deep the current position is at
    namespace_depth: usize,
}

/// Callback to provide contextual state for the parser
pub trait SymbolResolver {
    fn is_type(&self, ty: &TypeLayout) -> bool;
    fn is_function(&self, ty: &TypeLayout) -> bool;
}

impl Parser {
    /// Create a new parser object
    pub fn new(tokens: Vec<LexToken>) -> Self {
        Parser {
            tokens,
            current: 0,
            namespace_depth: 0,
        }
    }

    /// Parse then next top level item
    pub fn parse_item(&mut self, resolver: &dyn SymbolResolver) -> Result<ParserItem, ParseError> {
        fn try_parse_item<'t>(
            input: &'t [LexToken],
            resolver: &dyn SymbolResolver,
        ) -> ParseResult<'t, ParserItem> {
            if let Ok((rest, _)) = parse_token(Token::Semicolon)(input) {
                return Ok((rest, ParserItem::Empty));
            }

            if let Ok((rest, _)) = parse_token(Token::Namespace)(input) {
                return parse_namespace_enter(rest);
            }

            if let Ok((rest, _)) = parse_token(Token::RightBrace)(input) {
                return Ok((rest, ParserItem::NamespaceExit));
            }

            let (input, def) = parse_root_definition(input, resolver)?;
            Ok((input, ParserItem::Definition(def)))
        }

        let rest = &self.tokens[self.current..];
        match try_parse_item(rest, resolver) {
            Ok((remaining, root)) => {
                assert!(self.current < self.tokens.len() - remaining.len());
                self.current = self.tokens.len() - remaining.len();
                Ok(root)
            }
            Err(_) if rest.len() == 1 && rest[0].0 == Token::Eof => {
                if self.namespace_depth != 0 {
                    Err(ParseError(ParseErrorReason::UnexpectedEndOfStream, None))
                } else {
                    Ok(ParserItem::EndOfFile)
                }
            }
            Err(err) => Err(ParseError::from(err)),
        }
    }
}

/// Individual top level items that can be encountered during parsing
pub enum ParserItem {
    Definition(RootDefinition),
    NamespaceEnter(Located<String>),
    NamespaceExit,
    Empty,
    EndOfFile,
}

/// Failure cases
mod errors;
pub use errors::{ParseError, ParseResultExt};
use errors::{ParseErrorContext, ParseErrorReason, ParseResult};

/// Stores current context of active symbols while parsing
#[derive(Debug)]
pub struct SymbolTable {
    terminator: Terminator,
}

/// When the expression parsing has to end
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Terminator {
    /// Expression has no explicit end point
    Standard,

    /// Expression is in a function call or array subscript so must move to next expression on comma
    /// Terminating ) or ] does not need ignoring so these cases are the same
    Sequence,

    /// Expression is in a type list so must move to next expression on comma or right angle bracket
    TypeList,
}

/// Provide symbol table to another parser
fn contextual<'t, 's, T>(
    parse_fn: impl Fn(&'t [LexToken], &'s SymbolTable, &'s dyn SymbolResolver) -> ParseResult<'t, T>
        + 's,
    st: &'s SymbolTable,
    resolver: &'s dyn SymbolResolver,
) -> impl Fn(&'t [LexToken]) -> ParseResult<'t, T> + 's {
    move |input: &'t [LexToken]| parse_fn(input, st, resolver)
}

/// Provide symbol resolver to another parser
fn contextual2<'t, 's, T>(
    parse_fn: impl Fn(&'t [LexToken], &'s dyn SymbolResolver) -> ParseResult<'t, T> + 's,
    resolver: &'s dyn SymbolResolver,
) -> impl Fn(&'t [LexToken]) -> ParseResult<'t, T> + 's {
    move |input: &'t [LexToken]| parse_fn(input, resolver)
}

/// Augment a parser with location information
fn locate<'t, T>(
    parser: impl Fn(&'t [LexToken]) -> ParseResult<'t, T>,
) -> impl Fn(&'t [LexToken]) -> ParseResult<'t, Located<T>> {
    move |input: &'t [LexToken]| match parser(input) {
        Ok((after, value)) => {
            assert_ne!(
                input.len(),
                after.len(),
                "Parser used in locate used no tokens"
            );
            assert!(!input.is_empty());
            Ok((after, Located::new(value, input[0].1)))
        }
        Err(err) => Err(err),
    }
}

/// Parse a list of elements separated with the given separator
fn parse_list_base<'t, T, G>(
    mut parse_separator: impl FnMut(&'t [LexToken]) -> ParseResult<'t, G>,
    mut parse_element: impl FnMut(&'t [LexToken]) -> ParseResult<'t, T>,
    allow_empty: bool,
) -> impl FnMut(&'t [LexToken]) -> ParseResult<'t, Vec<T>> {
    move |input: &'t [LexToken]| {
        match parse_element(input) {
            Ok((rest, element)) => {
                let mut input = rest;
                let mut values = Vec::from([element]);

                while let Ok((after_sep, _)) = parse_separator(input) {
                    match parse_element(after_sep) {
                        Ok((rest, element)) => {
                            values.push(element);
                            input = rest;
                        }
                        // If we failed to parse the element then finish
                        // We finish before the last separator so trailing separators are not captured
                        Err(ParseErrorContext(rest, _, _)) if rest.len() == after_sep.len() => {
                            break
                        }
                        // If we failed to parse but made progress into parsing then fail
                        Err(err) => return Err(err),
                    }
                }

                Ok((input, values))
            }
            // If we failed to parse the first element at all then return an empty list
            Err(ParseErrorContext(rest, _, _)) if allow_empty && rest.len() == input.len() => {
                Ok((input, Vec::new()))
            }
            // If we failed to parse the first element but made progress in parsing it then fail
            Err(err) => Err(err),
        }
    }
}

/// Parse a list of zero or more elements separated with the given separator
fn parse_list<'t, T, G>(
    parse_separator: impl FnMut(&'t [LexToken]) -> ParseResult<'t, G>,
    parse_element: impl FnMut(&'t [LexToken]) -> ParseResult<'t, T>,
) -> impl FnMut(&'t [LexToken]) -> ParseResult<'t, Vec<T>> {
    parse_list_base(parse_separator, parse_element, true)
}

/// Parse a list of one or more elements separated with the given separator
fn parse_list_nonempty<'t, T, G>(
    parse_separator: impl FnMut(&'t [LexToken]) -> ParseResult<'t, G>,
    parse_element: impl FnMut(&'t [LexToken]) -> ParseResult<'t, T>,
) -> impl FnMut(&'t [LexToken]) -> ParseResult<'t, Vec<T>> {
    parse_list_base(parse_separator, parse_element, false)
}

/// Parse a list of zero or more elements with no separator
fn parse_multiple<'t, T>(
    parse_element: impl FnMut(&'t [LexToken]) -> ParseResult<'t, T>,
) -> impl FnMut(&'t [LexToken]) -> ParseResult<'t, Vec<T>> {
    parse_list_base(|i| Ok((i, ())), parse_element, true)
}

/// Parse an element or nothing
fn parse_optional<'t, T>(
    parse_element: impl Fn(&'t [LexToken]) -> ParseResult<'t, T>,
) -> impl Fn(&'t [LexToken]) -> ParseResult<'t, Option<T>> {
    move |input: &'t [LexToken]| {
        match parse_element(input) {
            // If we succeeded then return the element
            Ok((rest, element)) => Ok((rest, Some(element))),
            // If we failed to parse the element at all then return nothing
            Err(ParseErrorContext(rest, _, _)) if rest.len() == input.len() => Ok((input, None)),
            // If we failed to parse the element but made progress in parsing it then fail
            Err(err) => Err(err),
        }
    }
}

/// Parse an exact token from the start of the stream
fn parse_token<'t>(token: Token) -> impl Fn(&'t [LexToken]) -> ParseResult<'t, LexToken> {
    move |input: &'t [LexToken]| match input {
        [tok @ LexToken(ref t, _), rest @ ..] if *t == token => Ok((rest, tok.clone())),
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Match a single identifier token
fn match_identifier(input: &[LexToken]) -> ParseResult<&Identifier> {
    match input {
        [LexToken(Token::Id(ref id), _), rest @ ..] => Ok((rest, id)),
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Match a non-keyword named identifier token
fn match_named_identifier<'t>(
    name: &'static str,
    input: &'t [LexToken],
) -> ParseResult<'t, &'t Identifier> {
    match input {
        [LexToken(Token::Id(ref id), _), rest @ ..] if id.0 == name => Ok((rest, id)),
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Match a single < that may or may not be followed by whitespace
fn match_left_angle_bracket(input: &[LexToken]) -> ParseResult<LexToken> {
    match input {
        [first @ LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => Ok((rest, first.clone())),
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Match a single > that may or may not be followed by whitespace
fn match_right_angle_bracket(input: &[LexToken]) -> ParseResult<LexToken> {
    match input {
        [first @ LexToken(Token::RightAngleBracket(_), _), rest @ ..] => Ok((rest, first.clone())),
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Parsing identifier that may be a variable name
fn parse_variable_name(input: &[LexToken]) -> ParseResult<Located<String>> {
    match input {
        [LexToken(Token::Id(Identifier(name)), loc), rest @ ..] => {
            Ok((rest, Located::new(name.clone(), *loc)))
        }
        _ => ParseErrorReason::wrong_token(input),
    }
}

// Implement parsing for type names
mod types;
use types::{parse_template_params, parse_type};

fn parse_arraydim<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Option<Box<Located<Expression>>>> {
    let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;
    let (input, constant_expression) = match parse_expression_no_seq(input, resolver) {
        Ok((rest, constant_expression)) => (rest, Some(Box::new(constant_expression))),
        _ => (input, None),
    };
    let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
    Ok((input, constant_expression))
}

// Implement parsing for expressions
mod expressions;
use expressions::parse_expression;
use expressions::parse_expression_no_seq;

/// Implement parsing for declarations
mod declarations;
use declarations::parse_init_declarators;

// Implement parsing for statements
mod statements;
use statements::{parse_initializer, statement_block};

// Implement parsing for struct types
mod structs;

// Implement parsing for enum types
mod enums;

// Implement parsing for shader global parameters
mod globals;

// Implement parsing for functions
mod functions;

// Implement parsing for pipelines
mod pipelines;

// Implement parsing for root definitions
mod root_definitions;
use root_definitions::{parse_namespace_enter, parse_root_definition};

#[cfg(test)]
mod test_support;
