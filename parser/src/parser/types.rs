use super::*;

/// Parse a type layout
fn parse_type_layout_internal<'t>(
    input: &'t [LexToken],
    st: Option<&SymbolTable>,
) -> ParseResult<'t, TypeLayout> {
    // Attempt to read a scoped identifier
    // All identifier paths are potential types - but we may reject paths when reparsing ambiguous parse trees
    match expressions::parse_scoped_identifier(input) {
        Ok((input, name)) => {
            let reject = if let Some(st) = st {
                let name_unlocated = name.clone().unlocate();
                st.reject_symbols.contains(&name_unlocated)
            } else {
                false
            };

            if reject {
                ParseErrorReason::SymbolIsNotAType.into_result(input)
            } else {
                let (input, args) = expressions::parse_template_args(input)?;
                Ok((input, TypeLayout(name, args.into_boxed_slice())))
            }
        }
        Err(err) => Err(err),
    }
}

/// Parse a type
fn parse_type_internal<'t>(
    mut input: &'t [LexToken],
    st: Option<&SymbolTable>,
) -> ParseResult<'t, Type> {
    let original_input = input;

    let mut modifier = TypeModifier::default();

    // Modifiers on left of type
    loop {
        match input {
            [LexToken(Token::Const, _), rest @ ..] => {
                modifier.is_const = true;
                input = rest;
            }
            [LexToken(Token::Volatile, _), rest @ ..] => {
                modifier.volatile = true;
                input = rest;
            }
            [LexToken(Token::RowMajor, _), rest @ ..] => {
                modifier.row_major = true;
                input = rest;
            }
            [LexToken(Token::ColumnMajor, _), rest @ ..] => {
                modifier.column_major = true;
                input = rest;
            }
            _ => break,
        }
    }

    let (mut input, tl) = parse_type_layout_internal(input, st)?;

    // Modifiers on right of type
    loop {
        match input {
            [LexToken(Token::Const, _), rest @ ..] => {
                modifier.is_const = true;
                input = rest;
            }
            [LexToken(Token::Volatile, _), rest @ ..] => {
                modifier.volatile = true;
                input = rest;
            }
            _ => break,
        }
    }

    assert_ne!(original_input.len(), input.len());

    Ok((
        input,
        Type {
            layout: tl,
            modifier,
            location: original_input[0].1,
        },
    ))
}

/// Parse a type
pub fn parse_type_with_symbols<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Type> {
    parse_type_internal(input, Some(st))
}

/// Parse a type
pub fn parse_type(input: &[LexToken]) -> ParseResult<Type> {
    parse_type_internal(input, None)
}

#[test]
fn test_type() {
    use test_support::*;
    let ty = ParserTester::new(parse_type);

    // Normal type name
    ty.check(
        "uint",
        Type {
            layout: "uint".loc(0).into(),
            modifier: TypeModifier::default(),
            location: SourceLocation::first(),
        },
    );

    // Type marked as const on left
    ty.check(
        "const uint",
        Type {
            layout: "uint".loc(6).into(),
            modifier: TypeModifier::const_only(),
            location: SourceLocation::first(),
        },
    );

    // Type marked with const twice is also valid
    ty.check(
        "const const uint",
        Type {
            layout: "uint".loc(12).into(),
            modifier: TypeModifier::const_only(),
            location: SourceLocation::first(),
        },
    );

    // Type marked as const on right
    ty.check(
        "uint const",
        Type {
            layout: "uint".loc(0).into(),
            modifier: TypeModifier::const_only(),
            location: SourceLocation::first(),
        },
    );

    // Type marked as volatile on left
    ty.check(
        "volatile uint",
        Type {
            layout: "uint".loc(9).into(),
            modifier: TypeModifier {
                volatile: true,
                ..Default::default()
            },
            location: SourceLocation::first(),
        },
    );

    // Type marked as volatile on right
    ty.check(
        "uint volatile",
        Type {
            layout: "uint".loc(0).into(),
            modifier: TypeModifier {
                volatile: true,
                ..Default::default()
            },
            location: SourceLocation::first(),
        },
    );

    // Type marked as row_major
    ty.check(
        "row_major uint",
        Type {
            layout: "uint".loc(10).into(),
            modifier: TypeModifier {
                row_major: true,
                ..Default::default()
            },
            location: SourceLocation::first(),
        },
    );

    // Type marked as column_major
    ty.check(
        "column_major uint",
        Type {
            layout: "uint".loc(13).into(),
            modifier: TypeModifier {
                column_major: true,
                ..Default::default()
            },
            location: SourceLocation::first(),
        },
    );

    // Type marked as row_major must be before the type
    ty.expect_fail("uint row_major", ParseErrorReason::TokensUnconsumed, 5);

    // Type marked as column_major must be before the type
    ty.expect_fail("uint column_major", ParseErrorReason::TokensUnconsumed, 5);

    // Many modifiers
    ty.check(
        "row_major column_major const volatile row_major column_major const volatile uint const volatile",
        Type {
            layout: "uint".loc(76).into(),
            modifier: TypeModifier {
                is_const: true,
                volatile: true,
                row_major: true,
                column_major: true,
            },
            location: SourceLocation::first(),
        },
    );
}

/// Parse a list of template parameters or no template parameters
pub fn parse_template_params(input: &[LexToken]) -> ParseResult<TemplateParamList> {
    let input = match parse_token(Token::Template)(input) {
        Ok((input, _)) => input,
        Err(_) => return Ok((input, TemplateParamList(Vec::new()))),
    };

    let (mut input, _) = match_left_angle_bracket(input)?;

    // Require at least one template argument
    // No specialisation supported
    let mut args = Vec::new();
    loop {
        let (after_typename, _) = parse_token(Token::Typename)(input)?;
        let (after_type, name) = match_identifier(after_typename)?;
        args.push(Located::new(name.0.clone(), after_typename[0].1));
        input = after_type;

        match parse_token(Token::Comma)(input) {
            Ok((rest, _)) => input = rest,
            Err(_) => break,
        }
    }

    let (input, _) = match_right_angle_bracket(input)?;

    Ok((input, TemplateParamList(args)))
}

/// Parse a typedef
pub fn parse_typedef(input: &[LexToken]) -> ParseResult<Typedef> {
    let (input, _) = parse_token(Token::Typedef)(input)?;
    let (input, ty) = parse_type(input)?;
    let (input, id) = locate(match_identifier)(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let td = Typedef {
        name: Located::new(id.node.0.clone(), id.location),
        source: ty,
        bind: VariableBind(bind),
    };
    Ok((input, td))
}

#[test]
fn test_typedef() {
    use test_support::*;
    let typedef = ParserTester::new(parse_typedef);

    typedef.check(
        "typedef uint u32;",
        Typedef {
            name: "u32".to_string().loc(13),
            source: Type::from("uint".loc(8)),
            bind: Default::default(),
        },
    );

    typedef.check(
        "typedef uint u32x4[4];",
        Typedef {
            name: "u32x4".to_string().loc(13),
            source: Type::from("uint".loc(8)),
            bind: VariableBind(Vec::from([Some(
                Expression::Literal(Literal::UntypedInt(4)).loc(19),
            )])),
        },
    );
}
