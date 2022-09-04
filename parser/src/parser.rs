use rssl_ast::*;
use rssl_text::*;
use rssl_tok::*;
use std::collections::HashMap;

/// Failure cases
mod errors;
pub use errors::{ParseError, ParseResultExt};
use errors::{ParseErrorContext, ParseErrorReason, ParseResult};

/// Class of a type name
#[derive(Copy, Clone, Debug)]
enum SymbolType {
    Struct,
    Enum,
    Object,
    TemplateType,
}

/// Stores current context of active symbols while parsing
#[derive(Debug)]
pub struct SymbolTable(HashMap<String, SymbolType>);

impl SymbolTable {
    #[cfg(test)]
    fn from(initial_symbols: &[(&str, SymbolType)]) -> SymbolTable {
        let mut table = SymbolTable::default();
        for (name, symbol_type) in initial_symbols {
            table.0.insert(name.to_string(), *symbol_type);
        }
        table
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        let mut table = SymbolTable(HashMap::new());
        // Register built in struct or primitive types
        let struct_types = ["vector", "matrix"];
        for struct_type in struct_types {
            table.0.insert(struct_type.to_string(), SymbolType::Struct);
        }
        // Register built in object types
        let object_types = [
            "Buffer",
            "RWBuffer",
            "ByteAddressBuffer",
            "RWByteAddressBuffer",
            "StructuredBuffer",
            "RWStructuredBuffer",
            "AppendStructuredBuffer",
            "ConsumeStructuredBuffer",
            "Texture1D",
            "Texture1DArray",
            "Texture2D",
            "Texture2DArray",
            "Texture2DMS",
            "Texture2DMSArray",
            "Texture3D",
            "TextureCube",
            "TextureCubeArray",
            "RWTexture1D",
            "RWTexture1DArray",
            "RWTexture2D",
            "RWTexture2DArray",
            "RWTexture3D",
            "ConstantBuffer",
            "InputPatch",
            "OutputPatch",
            "SamplerState",
        ];
        for object_type in object_types {
            table.0.insert(object_type.to_string(), SymbolType::Object);
        }
        table
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

/// Parse a list of elements separated with the given separator
fn parse_list_base<'t, T, G>(
    parse_separator: impl Fn(&'t [LexToken]) -> ParseResult<G>,
    parse_element: impl Fn(&'t [LexToken]) -> ParseResult<T>,
    allow_empty: bool,
) -> impl Fn(&'t [LexToken]) -> ParseResult<Vec<T>> {
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
                        Err(nom::Err::Error(ParseErrorContext(rest, _))) if rest == after_sep => {
                            break
                        }
                        // If we failed to parse but made progress into parsing then fail
                        Err(err) => return Err(err),
                    }
                }

                Ok((input, values))
            }
            // If we failed to parse the first element at all then return an empty list
            Err(nom::Err::Error(ParseErrorContext(rest, _))) if allow_empty && rest == input => {
                Ok((input, Vec::new()))
            }
            // If we failed to parse the first element but made progress in parsing it then fail
            Err(err) => Err(err),
        }
    }
}

/// Parse a list of zero or more elements separated with the given separator
fn parse_list<'t, T, G>(
    parse_separator: impl Fn(&'t [LexToken]) -> ParseResult<G>,
    parse_element: impl Fn(&'t [LexToken]) -> ParseResult<T>,
) -> impl Fn(&'t [LexToken]) -> ParseResult<Vec<T>> {
    parse_list_base(parse_separator, parse_element, true)
}

/// Parse a list of one or more elements separated with the given separator
fn parse_list_nonempty<'t, T, G>(
    parse_separator: impl Fn(&'t [LexToken]) -> ParseResult<G>,
    parse_element: impl Fn(&'t [LexToken]) -> ParseResult<T>,
) -> impl Fn(&'t [LexToken]) -> ParseResult<Vec<T>> {
    parse_list_base(parse_separator, parse_element, false)
}

/// Parse a list of zero or more elements with no separator
fn parse_multiple<'t, T>(
    parse_element: impl Fn(&'t [LexToken]) -> ParseResult<T>,
) -> impl Fn(&'t [LexToken]) -> ParseResult<Vec<T>> {
    parse_list_base(|i| Ok((i, ())), parse_element, true)
}

/// Parse an element or nothing
fn parse_optional<'t, T>(
    parse_element: impl Fn(&'t [LexToken]) -> ParseResult<T>,
) -> impl Fn(&'t [LexToken]) -> ParseResult<Option<T>> {
    move |input: &'t [LexToken]| {
        match parse_element(input) {
            // If we succeeded then return the element
            Ok((rest, element)) => Ok((rest, Some(element))),
            // If we failed to parse the element at all then return nothing
            Err(nom::Err::Error(ParseErrorContext(rest, _))) if rest == input => Ok((input, None)),
            // If we failed to parse the element but made progress in parsing it then fail
            Err(err) => Err(err),
        }
    }
}

/// Parse an exact token from the start of the stream
fn parse_token<'t>(token: Token) -> impl Fn(&'t [LexToken]) -> ParseResult<LexToken> {
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
use types::{parse_data_layout, parse_template_params, parse_type};

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
    let mut symbol_table = SymbolTable::default();
    loop {
        let last_def = parse_root_definition_with_semicolon(rest, &symbol_table);
        if let Ok((remaining, root)) = last_def {
            // Remember symbol names that may be used in type contexts later
            // If there are duplicate symbols then overwrite for now
            // We expect duplicates to be dealt with later during type checking
            match root {
                RootDefinition::Struct(ref sd) => {
                    symbol_table
                        .0
                        .insert(sd.name.node.clone(), SymbolType::Struct);
                }
                RootDefinition::Enum(ref ed) => {
                    symbol_table
                        .0
                        .insert(ed.name.node.clone(), SymbolType::Enum);
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
