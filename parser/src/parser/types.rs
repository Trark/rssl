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

    // Todo: modifiers that aren't const
    let (input, is_const) = match parse_token(Token::Const)(input) {
        Ok((input, _)) => (input, true),
        Err(_) => (input, false),
    };

    let (input, tl) = parse_type_layout_internal(input, st)?;

    let tm = TypeModifier {
        is_const,
        ..TypeModifier::default()
    };

    assert_ne!(original_input.len(), input.len());

    Ok((
        input,
        Type {
            layout: tl,
            modifier: tm,
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
