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
    input: &'t [LexToken],
    st: Option<&SymbolTable>,
) -> ParseResult<'t, Type> {
    let original_input = input;

    let mut modifiers = TypeModifierSet::default();
    let input = parse_type_modifiers_before(input, &mut modifiers);
    let (input, tl) = parse_type_layout_internal(input, st)?;
    let input = parse_type_modifiers_after(input, &mut modifiers);

    assert_ne!(original_input.len(), input.len());

    Ok((
        input,
        Type {
            layout: tl,
            modifiers,
            location: original_input[0].1,
        },
    ))
}

/// Parse a set of type modifiers before the type name
fn parse_type_modifiers_before<'t>(
    mut input: &'t [LexToken],
    modifiers: &mut TypeModifierSet,
) -> &'t [LexToken] {
    loop {
        let (modifier, loc, rest) = match input {
            [LexToken(Token::Const, loc), rest @ ..] => (TypeModifier::Const, *loc, rest),
            [LexToken(Token::Volatile, loc), rest @ ..] => (TypeModifier::Volatile, *loc, rest),
            [LexToken(Token::RowMajor, loc), rest @ ..] => (TypeModifier::RowMajor, *loc, rest),
            [LexToken(Token::ColumnMajor, loc), rest @ ..] => {
                (TypeModifier::ColumnMajor, *loc, rest)
            }
            [LexToken(Token::Unorm, loc), rest @ ..] => (TypeModifier::Unorm, *loc, rest),
            [LexToken(Token::Snorm, loc), rest @ ..] => (TypeModifier::Snorm, *loc, rest),
            [LexToken(Token::In, loc), rest @ ..] => (TypeModifier::In, *loc, rest),
            [LexToken(Token::Out, loc), rest @ ..] => (TypeModifier::Out, *loc, rest),
            [LexToken(Token::InOut, loc), rest @ ..] => (TypeModifier::InOut, *loc, rest),
            [LexToken(Token::Extern, loc), rest @ ..] => (TypeModifier::Extern, *loc, rest),
            [LexToken(Token::Static, loc), rest @ ..] => (TypeModifier::Static, *loc, rest),
            [LexToken(Token::GroupShared, loc), rest @ ..] => {
                (TypeModifier::GroupShared, *loc, rest)
            }
            [LexToken(Token::Id(id), loc), rest @ ..] => {
                // Handle non-keyword modifiers
                let modifier = match id.0.as_str() {
                    "precise" => TypeModifier::Precise,
                    "nointerpolation" => TypeModifier::NoInterpolation,
                    "linear" => TypeModifier::Linear,
                    "centroid" => TypeModifier::Centroid,
                    "noperspective" => TypeModifier::NoPerspective,
                    "sample" => TypeModifier::Sample,
                    "vertices" => TypeModifier::Vertices,
                    "primitives" => TypeModifier::Primitives,
                    "indices" => TypeModifier::Indices,
                    "payload" => TypeModifier::Payload,
                    _ => break,
                };
                (modifier, *loc, rest)
            }
            _ => break,
        };
        modifiers.modifiers.push(Located::new(modifier, loc));
        input = rest;
    }
    input
}

/// Parse a set of type modifiers after the type name
fn parse_type_modifiers_after<'t>(
    mut input: &'t [LexToken],
    modifiers: &mut TypeModifierSet,
) -> &'t [LexToken] {
    loop {
        let (modifier, loc, rest) = match input {
            [LexToken(Token::Const, loc), rest @ ..] => (TypeModifier::Const, *loc, rest),
            [LexToken(Token::Volatile, loc), rest @ ..] => (TypeModifier::Volatile, *loc, rest),
            _ => break,
        };
        modifiers.modifiers.push(Located::new(modifier, loc));
        input = rest;
    }
    input
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
            modifiers: TypeModifierSet::default(),
            location: SourceLocation::first(),
        },
    );

    // Type marked as const on left
    ty.check(
        "const uint",
        Type {
            layout: "uint".loc(6).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::Const.loc(0)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked with const twice is also valid
    ty.check(
        "const const uint",
        Type {
            layout: "uint".loc(12).into(),
            modifiers: TypeModifierSet::from(&[
                TypeModifier::Const.loc(0),
                TypeModifier::Const.loc(6),
            ]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as const on right
    ty.check(
        "uint const",
        Type {
            layout: "uint".loc(0).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::Const.loc(5)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as volatile on left
    ty.check(
        "volatile uint",
        Type {
            layout: "uint".loc(9).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::Volatile.loc(0)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as volatile on right
    ty.check(
        "uint volatile",
        Type {
            layout: "uint".loc(0).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::Volatile.loc(5)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as row_major
    ty.check(
        "row_major uint",
        Type {
            layout: "uint".loc(10).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::RowMajor.loc(0)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as row_major must be before the type
    ty.expect_fail("uint row_major", ParseErrorReason::TokensUnconsumed, 5);

    // Type marked as column_major
    ty.check(
        "column_major uint",
        Type {
            layout: "uint".loc(13).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::ColumnMajor.loc(0)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as column_major must be before the type
    ty.expect_fail("uint column_major", ParseErrorReason::TokensUnconsumed, 5);

    // Type marked as unorm
    ty.check(
        "unorm uint",
        Type {
            layout: "uint".loc(6).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::Unorm.loc(0)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as unorm must be before the type
    ty.expect_fail("uint unorm", ParseErrorReason::TokensUnconsumed, 5);

    // Type marked as snorm
    ty.check(
        "snorm uint",
        Type {
            layout: "uint".loc(6).into(),
            modifiers: TypeModifierSet::from(&[TypeModifier::Snorm.loc(0)]),
            location: SourceLocation::first(),
        },
    );

    // Type marked as snorm must be before the type
    ty.expect_fail("uint snorm", ParseErrorReason::TokensUnconsumed, 5);

    // Many modifiers
    ty.check(
        "row_major column_major unorm snorm const volatile row_major column_major unorm snorm const volatile uint const volatile",
        Type {
            layout: "uint".loc(100).into(),
            modifiers: TypeModifierSet::from(&[
                TypeModifier::RowMajor.loc(0),
                TypeModifier::ColumnMajor.loc(10),
                TypeModifier::Unorm.loc(23),
                TypeModifier::Snorm.loc(29),
                TypeModifier::Const.loc(35),
                TypeModifier::Volatile.loc(41),
                TypeModifier::RowMajor.loc(50),
                TypeModifier::ColumnMajor.loc(60),
                TypeModifier::Unorm.loc(73),
                TypeModifier::Snorm.loc(79),
                TypeModifier::Const.loc(85),
                TypeModifier::Volatile.loc(91),
                TypeModifier::Const.loc(105),
                TypeModifier::Volatile.loc(111),
            ]),
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
