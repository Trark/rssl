use super::*;

enum PrimitiveTypeError {
    UnexpectedCharacter,
    EndOfStream,
}

// Parse scalar type as part of a string
fn parse_scalartype_str(input: &[u8]) -> Result<(&[u8], ScalarType), PrimitiveTypeError> {
    match input {
        [b'b', b'o', b'o', b'l', rest @ ..] => Ok((rest, ScalarType::Bool)),
        [b'i', b'n', b't', rest @ ..] => Ok((rest, ScalarType::Int)),
        [b'u', b'i', b'n', b't', rest @ ..] => Ok((rest, ScalarType::UInt)),
        [b'd', b'w', b'o', b'r', b'd', rest @ ..] => Ok((rest, ScalarType::UInt)),
        [b'h', b'a', b'l', b'f', rest @ ..] => Ok((rest, ScalarType::Half)),
        [b'f', b'l', b'o', b'a', b't', rest @ ..] => Ok((rest, ScalarType::Float)),
        [b'd', b'o', b'u', b'b', b'l', b'e', rest @ ..] => Ok((rest, ScalarType::Double)),
        _ => Err(PrimitiveTypeError::UnexpectedCharacter),
    }
}

// Parse data type as part of a string
fn parse_datalayout_str(typename: &str) -> Option<TypeLayout> {
    fn digit(input: &[u8]) -> Result<(&[u8], u32), PrimitiveTypeError> {
        // Handle end of stream
        if input.is_empty() {
            return Err(PrimitiveTypeError::EndOfStream);
        };

        // Match on the next character
        let n = match input[0] {
            b'1' => 1,
            b'2' => 2,
            b'3' => 3,
            b'4' => 4,
            _ => {
                // Not a digit
                return Err(PrimitiveTypeError::UnexpectedCharacter);
            }
        };

        // Success
        Ok((&input[1..], n))
    }

    fn parse_str(input: &[u8]) -> Result<(&[u8], TypeLayout), PrimitiveTypeError> {
        let (rest, ty) = parse_scalartype_str(input)?;
        if rest.is_empty() {
            return Ok((&[], TypeLayout::Scalar(ty)));
        }

        let (rest, x) = digit(rest)?;
        if rest.is_empty() {
            return Ok((&[], TypeLayout::Vector(ty, x)));
        }

        let rest = match rest.first() {
            Some(b'x') => &rest[1..],
            _ => return Err(PrimitiveTypeError::UnexpectedCharacter),
        };

        let (rest, y) = digit(rest)?;
        if rest.is_empty() {
            return Ok((&[], TypeLayout::Matrix(ty, x, y)));
        }

        Err(PrimitiveTypeError::UnexpectedCharacter)
    }

    match parse_str(typename[..].as_bytes()) {
        Ok((rest, ty)) => {
            assert_eq!(rest.len(), 0);
            Some(ty)
        }
        Err(_) => None,
    }
}

#[test]
fn test_parse_datalayout_str() {
    assert_eq!(
        parse_datalayout_str("float"),
        Some(TypeLayout::Scalar(ScalarType::Float))
    );
    assert_eq!(
        parse_datalayout_str("uint3"),
        Some(TypeLayout::Vector(ScalarType::UInt, 3))
    );
    assert_eq!(
        parse_datalayout_str("bool2x3"),
        Some(TypeLayout::Matrix(ScalarType::Bool, 2, 3))
    );

    assert_eq!(parse_datalayout_str(""), None);
    assert_eq!(parse_datalayout_str("float5"), None);
    assert_eq!(parse_datalayout_str("float2x"), None);
}

/// Parse a type layout for a basic data type
pub fn parse_data_layout(input: &[LexToken]) -> ParseResult<TypeLayout> {
    // Parse a vector dimension as a token
    fn parse_digit(input: &[LexToken]) -> ParseResult<u32> {
        if input.is_empty() {
            return ParseErrorReason::end_of_stream();
        }

        match input[0] {
            LexToken(Token::LiteralInt(i), _) => Ok((&input[1..], i as u32)),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    // Parse scalar type as a full token
    fn parse_scalartype(input: &[LexToken]) -> ParseResult<ScalarType> {
        if input.is_empty() {
            return ParseErrorReason::end_of_stream();
        }

        match input[0] {
            LexToken(Token::Id(Identifier(ref name)), _) => {
                match parse_scalartype_str(name[..].as_bytes()) {
                    Ok((rest, ty)) => {
                        if rest.is_empty() {
                            Ok((&input[1..], ty))
                        } else {
                            ParseErrorReason::UnknownType.into_result(input)
                        }
                    }
                    Err(_) => ParseErrorReason::UnknownType.into_result(input),
                }
            }
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    if input.is_empty() {
        ParseErrorReason::end_of_stream()
    } else {
        match &input[0] {
            &LexToken(Token::Id(Identifier(ref name)), _) => match &name[..] {
                "vector" => {
                    let (input, _) = match_left_angle_bracket(&input[1..])?;
                    let (input, scalar) = parse_scalartype(input)?;
                    let (input, _) = parse_token(Token::Comma)(input)?;
                    let (input, x) = parse_digit(input)?;
                    let (input, _) = match_right_angle_bracket(input)?;
                    Ok((input, TypeLayout::Vector(scalar, x)))
                }
                "matrix" => {
                    let (input, _) = match_left_angle_bracket(&input[1..])?;
                    let (input, scalar) = parse_scalartype(input)?;
                    let (input, _) = parse_token(Token::Comma)(input)?;
                    let (input, x) = parse_digit(input)?;
                    let (input, _) = parse_token(Token::Comma)(input)?;
                    let (input, y) = parse_digit(input)?;
                    let (input, _) = match_right_angle_bracket(input)?;
                    Ok((input, TypeLayout::Matrix(scalar, x, y)))
                }
                _ => match parse_datalayout_str(&name[..]) {
                    Some(ty) => Ok((&input[1..], ty)),
                    None => ParseErrorReason::UnknownType.into_result(input),
                },
            },
            _ => ParseErrorReason::wrong_token(input),
        }
    }
}

/// Parse the void type
fn parse_voidtype(input: &[LexToken]) -> ParseResult<TypeLayout> {
    if input.is_empty() {
        ParseErrorReason::end_of_stream()
    } else {
        match &input[0] {
            &LexToken(Token::Id(Identifier(ref name)), _) => {
                if name == "void" {
                    Ok((&input[1..], TypeLayout::Void))
                } else {
                    ParseErrorReason::UnknownType.into_result(input)
                }
            }
            _ => ParseErrorReason::wrong_token(input),
        }
    }
}

/// Parse a type layout
fn parse_type_layout_internal<'t>(
    input: &'t [LexToken],
    st: Option<&SymbolTable>,
) -> ParseResult<'t, TypeLayout> {
    if let Ok((input, ty)) = parse_voidtype(input) {
        return Ok((input, ty));
    }

    // Attempt to parse a primitive structured type
    if let Ok((input, tl)) = parse_data_layout(input) {
        return Ok((input, tl));
    }

    // Attempt to parse a custom type
    match match_identifier(input) {
        Ok((input, Identifier(name))) => {
            let reject = {
                if let Some(st) = st {
                    st.reject_symbols.contains(name)
                } else {
                    false
                }
            };
            if reject {
                ParseErrorReason::SymbolIsNotAType.into_result(input)
            } else {
                let (input, args) = expressions::parse_template_args(input)?;
                Ok((input, TypeLayout::Custom(name.clone(), args)))
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
    Ok((input, Type(tl, tm)))
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
