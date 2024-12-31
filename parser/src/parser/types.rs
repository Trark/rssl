use super::declarations::{parse_abstract_declarator, parse_declarator};
use super::*;

/// Parse a type layout
fn parse_type_layout_internal<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, TypeLayout> {
    let (input, name) = expressions::parse_scoped_identifier(input, false, resolver)?;
    let (input, args) = expressions::parse_template_args(input, resolver)?;
    let tyl = TypeLayout(name, args.into_boxed_slice());
    if resolver.is_type(&tyl) {
        Ok((input, tyl))
    } else {
        ParseErrorReason::SymbolIsNotAType.into_result(input)
    }
}

/// Parse a type
fn parse_type_internal<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Type> {
    let original_input = input;

    let mut modifiers = TypeModifierSet::default();
    let input = parse_type_modifiers_before(input, &mut modifiers);
    let (input, tl) = parse_type_layout_internal(input, resolver)?;
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
            [LexToken(Token::Inline, _), rest @ ..] => {
                // Ignore all inline modifiers
                input = rest;
                continue;
            }
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
                    "point" => TypeModifier::Point,
                    "line" => TypeModifier::Line,
                    "triangle" => TypeModifier::Triangle,
                    "lineadj" => TypeModifier::LineAdj,
                    "triangleadj" => TypeModifier::TriangleAdj,
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
pub fn parse_type_modifiers_after<'t>(
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
pub fn parse_type<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Type> {
    parse_type_internal(input, resolver)
}

/// Parse a type id
pub fn parse_type_id<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, TypeId> {
    let (input, base) = parse_type_internal(input, resolver)?;

    let (input, abstract_declarator) = parse_abstract_declarator(input, resolver)?;

    let ty = TypeId {
        base,
        abstract_declarator,
    };

    Ok((input, ty))
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

/// Parse a typedef
pub fn parse_typedef<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Typedef> {
    let (input, _) = parse_token(Token::Typedef)(input)?;
    let (input, ty) = parse_type(input, resolver)?;
    let (input, declarator) = parse_declarator(input, resolver)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let td = Typedef {
        source: ty,
        declarator,
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
            source: Type::from("uint".loc(8)),
            declarator: Declarator::from("u32".loc(13)),
        },
    );

    typedef.check(
        "typedef uint u32x4[4];",
        Typedef {
            source: Type::from("uint".loc(8)),
            declarator: Declarator::Array(ArrayDeclarator {
                inner: Box::new(Declarator::from("u32x4".loc(13))),
                array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(19)),
                attributes: Vec::new(),
            }),
        },
    );
}
