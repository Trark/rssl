use rssl_ast::*;
use rssl_text::*;
use rssl_tok::*;
use std::collections::HashMap;

/// Failure cases
mod errors;
pub use errors::ParseError;
use errors::{get_most_relevant_error, ParseErrorContext, ParseErrorReason, ParseResult};

/// Class of a type name
enum SymbolType {
    Struct,
    Enum,
}

/// Stores current context of active symbols while parsing
pub struct SymbolTable(HashMap<String, SymbolType>);

impl SymbolTable {
    fn empty() -> SymbolTable {
        SymbolTable(HashMap::new())
    }
}

/// Provides standard way of parsing the implementing type
pub trait Parse: Sized {
    type Output;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self::Output>;
}

/// Create parser for a type that implements Parse
fn parse_typed<'t, 's, T: Parse>(
    st: &'s SymbolTable,
) -> impl Fn(&'t [LexToken]) -> ParseResult<T::Output> + 's {
    move |input: &'t [LexToken]| T::parse(input, st)
}

/// Parse an exact token from the start of the stream
fn parse_token<'t>(token: Token) -> impl Fn(&'t [LexToken]) -> ParseResult<LexToken> {
    move |input: &'t [LexToken]| {
        if input.is_empty() {
            return Err(nom::Err::Incomplete(nom::Needed::new(1)));
        }
        match input[0] {
            LexToken(ref t, _) if *t == token => Ok((&input[1..], input[0].clone())),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

/// Match a single identifier token
fn match_identifier(input: &[LexToken]) -> ParseResult<&Identifier> {
    match input {
        [LexToken(Token::Id(ref id), _), rest @ ..] => Ok((rest, id)),
        _ => Err(nom::Err::Error(ParseErrorContext(
            input,
            ParseErrorReason::WrongToken,
        ))),
    }
}

/// Match a single < that may or may not be followed by whitespace
fn match_left_angle_bracket(input: &[LexToken]) -> ParseResult<LexToken> {
    match input {
        [first @ LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => Ok((rest, first.clone())),
        _ => Err(nom::Err::Error(ParseErrorContext(
            input,
            ParseErrorReason::WrongToken,
        ))),
    }
}

/// Match a single > that may or may not be followed by whitespace
fn match_right_angle_bracket(input: &[LexToken]) -> ParseResult<LexToken> {
    match input {
        [first @ LexToken(Token::RightAngleBracket(_), _), rest @ ..] => Ok((rest, first.clone())),
        _ => Err(nom::Err::Error(ParseErrorContext(
            input,
            ParseErrorReason::WrongToken,
        ))),
    }
}

/// Helper type for parsing identifiers that may be variable names
struct VariableName;

impl Parse for VariableName {
    type Output = Located<String>;
    fn parse<'t>(input: &'t [LexToken], _: &SymbolTable) -> ParseResult<'t, Self::Output> {
        if input.is_empty() {
            return Err(nom::Err::Incomplete(nom::Needed::new(1)));
        }

        match &input[0] {
            LexToken(Token::Id(Identifier(name)), loc) => {
                Ok((&input[1..], Located::new(name.clone(), *loc)))
            }
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

// Implement parsing for type names
mod types;

fn parse_arraydim<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Option<Located<Expression>>> {
    let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;
    let (input, constant_expression) = match ExpressionNoSeq::parse(input, st) {
        Ok((rest, constant_expression)) => (rest, Some(constant_expression)),
        _ => (input, None),
    };
    let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
    Ok((input, constant_expression))
}

// Implement parsing for expressions
mod expressions;
use expressions::ExpressionNoSeq;

// Implement parsing for statements
mod statements;
use statements::statement_block;

// Implement parsing for struct types
mod structs;

// Implement parsing for enum types
mod enums;

// Implement parsing for shader global parameters
mod globals;

// Implement parsing for functions
mod functions;

// Implement parsing for root definitions
mod root_definitions;
use root_definitions::rootdefinition_with_semicolon;

fn parse_internal(input: &[LexToken]) -> ParseResult<Vec<RootDefinition>> {
    let mut roots = Vec::new();
    let mut rest = input;
    let mut symbol_table = SymbolTable::empty();
    loop {
        let last_def = rootdefinition_with_semicolon(rest, &symbol_table);
        if let Ok((remaining, root)) = last_def {
            match root {
                RootDefinition::Struct(ref sd) => {
                    match symbol_table.0.insert(sd.name.clone(), SymbolType::Struct) {
                        Some(_) => {
                            let reason = ParseErrorReason::DuplicateStructSymbol;
                            return Err(nom::Err::Error(ParseErrorContext(input, reason)));
                        }
                        None => {}
                    }
                }
                RootDefinition::Enum(ref ed) => {
                    match symbol_table.0.insert(ed.name.clone(), SymbolType::Enum) {
                        Some(_) => {
                            let reason = ParseErrorReason::DuplicateEnumSymbol;
                            return Err(nom::Err::Error(ParseErrorContext(input, reason)));
                        }
                        None => {}
                    }
                }
                _ => {}
            }
            roots.push(root);
            rest = remaining;
        } else {
            return match rest {
                a if a.len() == 1 && a[0].0 == Token::Eof => Ok((&[], roots)),
                _ => match last_def {
                    Ok(_) => unreachable!(),
                    Err(err) => Err(err),
                },
            };
        }
    }
}

/// Parse a stream of lex tokens into an abstract syntax tree
pub fn parse(source: &[LexToken]) -> Result<Module, ParseError> {
    match parse_internal(source) {
        Ok((rest, _)) if !rest.is_empty() => Err(ParseError::from_tokens_remaining(rest)),
        Ok((_, hlsl)) => Ok(Module {
            root_definitions: hlsl,
        }),
        Err(err) => Err(ParseError::from(err)),
    }
}

#[cfg(test)]
mod test_support;
