use rssl_ast::*;
use rssl_text::*;
use rssl_tok::*;
use std::collections::HashMap;

/// Failure cases
mod errors;
pub use errors::{ParseError, ParseResultExt};
use errors::{ParseErrorContext, ParseErrorReason, ParseResult};

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

/// Provide symbol table to another parser
fn contextual<'t, 's, T>(
    parse_fn: impl Fn(&'t [LexToken], &'s SymbolTable) -> ParseResult<'t, T> + 's,
    st: &'s SymbolTable,
) -> impl Fn(&'t [LexToken]) -> ParseResult<'t, T> + 's {
    move |input: &'t [LexToken]| parse_fn(input, st)
}

/// Augment a parser with location information
fn locate<'t, 's, T>(
    parser: impl Fn(&'t [LexToken]) -> ParseResult<T> + 's,
) -> impl Fn(&'t [LexToken]) -> ParseResult<Located<T>> + 's {
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

/// Parsing identifier that may be a variable name
fn parse_variable_name(input: &[LexToken]) -> ParseResult<Located<String>> {
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

// Implement parsing for type names
mod types;
use types::{parse_data_layout, parse_type};

fn parse_arraydim<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Option<Located<Expression>>> {
    let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;
    let (input, constant_expression) = match parse_expression_no_seq(input, st) {
        Ok((rest, constant_expression)) => (rest, Some(constant_expression)),
        _ => (input, None),
    };
    let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
    Ok((input, constant_expression))
}

// Implement parsing for expressions
mod expressions;
use expressions::parse_expression;
use expressions::parse_expression_no_seq;

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

// Implement parsing for root definitions
mod root_definitions;
use root_definitions::parse_root_definition_with_semicolon;

fn parse_internal(input: &[LexToken]) -> ParseResult<Vec<RootDefinition>> {
    let mut roots = Vec::new();
    let mut rest = input;
    let mut symbol_table = SymbolTable::empty();
    loop {
        let last_def = parse_root_definition_with_semicolon(rest, &symbol_table);
        if let Ok((remaining, root)) = last_def {
            // Remember symbol names that may be used in type contexts later
            // If there are duplicate symbols then overwrite for now
            // We expect duplicates to be dealt with later during type checking
            match root {
                RootDefinition::Struct(ref sd) => {
                    symbol_table.0.insert(sd.name.clone(), SymbolType::Struct);
                }
                RootDefinition::Enum(ref ed) => {
                    symbol_table.0.insert(ed.name.clone(), SymbolType::Enum);
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
