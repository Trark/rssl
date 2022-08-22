use super::*;

// Parse scalar type as part of a string
fn parse_scalartype_str(input: &[u8]) -> nom::IResult<&[u8], ScalarType> {
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    nom::branch::alt((
        map(tag("bool"), |_| ScalarType::Bool),
        map(tag("int"), |_| ScalarType::Int),
        map(tag("uint"), |_| ScalarType::UInt),
        map(tag("dword"), |_| ScalarType::UInt),
        map(tag("half"), |_| ScalarType::Half),
        map(tag("float"), |_| ScalarType::Float),
        map(tag("double"), |_| ScalarType::Double),
    ))(input)
}

// Parse data type as part of a string
fn parse_datalayout_str(typename: &str) -> Option<DataLayout> {
    fn digit(input: &[u8]) -> nom::IResult<&[u8], u32> {
        // Handle end of stream
        if input.is_empty() {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )));
        };

        // Match on the next character
        let n = match input[0] {
            b'1' => 1,
            b'2' => 2,
            b'3' => 3,
            b'4' => 4,
            _ => {
                // Not a digit
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )));
            }
        };

        // Success
        Ok((&input[1..], n))
    }

    fn parse_str(input: &[u8]) -> nom::IResult<&[u8], DataLayout> {
        let (rest, ty) = parse_scalartype_str(input)?;
        if rest.is_empty() {
            return Ok((&[], DataLayout::Scalar(ty)));
        }

        let (rest, x) = digit(rest)?;
        if rest.is_empty() {
            return Ok((&[], DataLayout::Vector(ty, x)));
        }

        let rest = match rest.first() {
            Some(b'x') => &rest[1..],
            _ => {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                )))
            }
        };

        let (rest, y) = digit(rest)?;
        if rest.is_empty() {
            return Ok((&[], DataLayout::Matrix(ty, x, y)));
        }

        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
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
        Some(DataLayout::Scalar(ScalarType::Float))
    );
    assert_eq!(
        parse_datalayout_str("uint3"),
        Some(DataLayout::Vector(ScalarType::UInt, 3))
    );
    assert_eq!(
        parse_datalayout_str("bool2x3"),
        Some(DataLayout::Matrix(ScalarType::Bool, 2, 3))
    );

    assert_eq!(parse_datalayout_str(""), None);
    assert_eq!(parse_datalayout_str("float5"), None);
    assert_eq!(parse_datalayout_str("float2x"), None);
}

/// Parse a type layout for a basic data type
pub fn parse_data_layout(input: &[LexToken]) -> ParseResult<DataLayout> {
    // Parse a vector dimension as a token
    fn parse_digit(input: &[LexToken]) -> ParseResult<u32> {
        if input.is_empty() {
            return Err(nom::Err::Incomplete(nom::Needed::new(1)));
        }

        match input[0] {
            LexToken(Token::LiteralInt(i), _) => Ok((&input[1..], i as u32)),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    // Parse scalar type as a full token
    fn parse_scalartype(input: &[LexToken]) -> ParseResult<ScalarType> {
        if input.is_empty() {
            return Err(nom::Err::Incomplete(nom::Needed::new(1)));
        }

        match input[0] {
            LexToken(Token::Id(Identifier(ref name)), _) => {
                match parse_scalartype_str(name[..].as_bytes()) {
                    Ok((rest, ty)) => {
                        if rest.is_empty() {
                            Ok((&input[1..], ty))
                        } else {
                            Err(nom::Err::Error(ParseErrorContext(
                                input,
                                ParseErrorReason::UnknownType,
                            )))
                        }
                    }
                    Err(nom::Err::Incomplete(rem)) => Err(nom::Err::Incomplete(rem)),
                    Err(_) => Err(nom::Err::Error(ParseErrorContext(
                        input,
                        ParseErrorReason::UnknownType,
                    ))),
                }
            }
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    if input.is_empty() {
        Err(nom::Err::Incomplete(nom::Needed::new(1)))
    } else {
        match &input[0] {
            &LexToken(Token::Id(Identifier(ref name)), _) => match &name[..] {
                "vector" => {
                    let (input, _) = match_left_angle_bracket(&input[1..])?;
                    let (input, scalar) = parse_scalartype(input)?;
                    let (input, _) = parse_token(Token::Comma)(input)?;
                    let (input, x) = parse_digit(input)?;
                    let (input, _) = match_right_angle_bracket(input)?;
                    Ok((input, DataLayout::Vector(scalar, x)))
                }
                "matrix" => {
                    let (input, _) = match_left_angle_bracket(&input[1..])?;
                    let (input, scalar) = parse_scalartype(input)?;
                    let (input, _) = parse_token(Token::Comma)(input)?;
                    let (input, x) = parse_digit(input)?;
                    let (input, _) = parse_token(Token::Comma)(input)?;
                    let (input, y) = parse_digit(input)?;
                    let (input, _) = match_right_angle_bracket(input)?;
                    Ok((input, DataLayout::Matrix(scalar, x, y)))
                }
                _ => match parse_datalayout_str(&name[..]) {
                    Some(ty) => Ok((&input[1..], ty)),
                    None => Err(nom::Err::Error(ParseErrorContext(
                        input,
                        ParseErrorReason::UnknownType,
                    ))),
                },
            },
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

/// Parse a type for a basic data type
fn parse_data_type(input: &[LexToken]) -> ParseResult<DataType> {
    // Todo: Modifiers
    match parse_data_layout(input) {
        Ok((rest, layout)) => Ok((rest, DataType(layout, Default::default()))),
        Err(err) => Err(err),
    }
}

/// Parse a type layout for a structured type
fn parse_structured_layout<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, StructuredLayout> {
    // Attempt to parse a primitive structured type
    if let Ok((input, ty)) = parse_data_layout(input) {
        let sl = match ty {
            DataLayout::Scalar(scalar) => StructuredLayout::Scalar(scalar),
            DataLayout::Vector(scalar, x) => StructuredLayout::Vector(scalar, x),
            DataLayout::Matrix(scalar, x, y) => StructuredLayout::Matrix(scalar, x, y),
        };
        return Ok((input, sl));
    }

    // Attempt to parse a custom type
    match match_identifier(input) {
        Ok((input, Identifier(name))) => match st.0.get(name) {
            Some(&SymbolType::Struct) => Ok((input, StructuredLayout::Custom(name.clone()))),
            Some(&SymbolType::Enum) => Ok((input, StructuredLayout::Custom(name.clone()))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::SymbolIsNotAStructuredType,
            ))),
        },
        Err(err) => Err(err),
    }
}

/// Parse a type for a structured type
fn parse_structured_type<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, StructuredType> {
    // Todo: Modifiers
    match parse_structured_layout(input, st) {
        Ok((rest, layout)) => Ok((rest, StructuredType(layout, Default::default()))),
        Err(err) => Err(err),
    }
}

/// Parse an object type
fn parse_object_type<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, ObjectType> {
    if input.is_empty() {
        return Err(nom::Err::Incomplete(nom::Needed::new(1)));
    }

    enum ParseType {
        Buffer,
        RWBuffer,

        ByteAddressBuffer,
        RWByteAddressBuffer,

        StructuredBuffer,
        RWStructuredBuffer,
        AppendStructuredBuffer,
        ConsumeStructuredBuffer,

        Texture1D,
        Texture1DArray,
        Texture2D,
        Texture2DArray,
        Texture2DMS,
        Texture2DMSArray,
        Texture3D,
        TextureCube,
        TextureCubeArray,
        RWTexture1D,
        RWTexture1DArray,
        RWTexture2D,
        RWTexture2DArray,
        RWTexture3D,

        ConstantBuffer,

        InputPatch,
        OutputPatch,
    }

    let object_type = match &input[0] {
        &LexToken(Token::Id(Identifier(ref name)), _) => match &name[..] {
            "Buffer" => ParseType::Buffer,
            "RWBuffer" => ParseType::RWBuffer,

            "ByteAddressBuffer" => ParseType::ByteAddressBuffer,
            "RWByteAddressBuffer" => ParseType::RWByteAddressBuffer,

            "StructuredBuffer" => ParseType::StructuredBuffer,
            "RWStructuredBuffer" => ParseType::RWStructuredBuffer,
            "AppendStructuredBuffer" => ParseType::AppendStructuredBuffer,
            "ConsumeStructuredBuffer" => ParseType::ConsumeStructuredBuffer,

            "Texture1D" => ParseType::Texture1D,
            "Texture1DArray" => ParseType::Texture1DArray,
            "Texture2D" => ParseType::Texture2D,
            "Texture2DArray" => ParseType::Texture2DArray,
            "Texture2DMS" => ParseType::Texture2DMS,
            "Texture2DMSArray" => ParseType::Texture2DMSArray,
            "Texture3D" => ParseType::Texture3D,
            "TextureCube" => ParseType::TextureCube,
            "TextureCubeArray" => ParseType::TextureCubeArray,
            "RWTexture1D" => ParseType::RWTexture1D,
            "RWTexture1DArray" => ParseType::RWTexture1DArray,
            "RWTexture2D" => ParseType::RWTexture2D,
            "RWTexture2DArray" => ParseType::RWTexture2DArray,
            "RWTexture3D" => ParseType::RWTexture3D,

            "ConstantBuffer" => ParseType::ConstantBuffer,

            "InputPatch" => ParseType::InputPatch,
            "OutputPatch" => ParseType::OutputPatch,

            _ => {
                return Err(nom::Err::Error(ParseErrorContext(
                    input,
                    ParseErrorReason::UnknownType,
                )))
            }
        },
        _ => {
            return Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::UnknownType,
            )))
        }
    };

    let rest = &input[1..];

    match object_type {
        ParseType::ByteAddressBuffer => Ok((rest, ObjectType::ByteAddressBuffer)),
        ParseType::RWByteAddressBuffer => Ok((rest, ObjectType::RWByteAddressBuffer)),

        ParseType::Buffer
        | ParseType::RWBuffer
        | ParseType::Texture1D
        | ParseType::Texture1DArray
        | ParseType::Texture2D
        | ParseType::Texture2DArray
        | ParseType::Texture2DMS
        | ParseType::Texture2DMSArray
        | ParseType::Texture3D
        | ParseType::TextureCube
        | ParseType::TextureCubeArray
        | ParseType::RWTexture1D
        | ParseType::RWTexture1DArray
        | ParseType::RWTexture2D
        | ParseType::RWTexture2DArray
        | ParseType::RWTexture3D => {
            let (buffer_arg, rest) = match nom::sequence::delimited(
                match_left_angle_bracket,
                parse_data_type,
                match_right_angle_bracket,
            )(rest)
            {
                Ok((rest, ty)) => (ty, rest),
                Err(nom::Err::Incomplete(needed)) => return Err(nom::Err::Incomplete(needed)),
                Err(_) => (
                    DataType(
                        DataLayout::Vector(ScalarType::Float, 4),
                        TypeModifier::default(),
                    ),
                    rest,
                ),
            };

            let ty = match object_type {
                ParseType::Buffer => ObjectType::Buffer(buffer_arg),
                ParseType::RWBuffer => ObjectType::RWBuffer(buffer_arg),
                ParseType::Texture1D => ObjectType::Texture1D(buffer_arg),
                ParseType::Texture1DArray => ObjectType::Texture1DArray(buffer_arg),
                ParseType::Texture2D => ObjectType::Texture2D(buffer_arg),
                ParseType::Texture2DArray => ObjectType::Texture2DArray(buffer_arg),
                ParseType::Texture2DMS => ObjectType::Texture2DMS(buffer_arg),
                ParseType::Texture2DMSArray => ObjectType::Texture2DMSArray(buffer_arg),
                ParseType::Texture3D => ObjectType::Texture3D(buffer_arg),
                ParseType::TextureCube => ObjectType::TextureCube(buffer_arg),
                ParseType::TextureCubeArray => ObjectType::TextureCubeArray(buffer_arg),
                ParseType::RWTexture1D => ObjectType::RWTexture1D(buffer_arg),
                ParseType::RWTexture1DArray => ObjectType::RWTexture1DArray(buffer_arg),
                ParseType::RWTexture2D => ObjectType::RWTexture2D(buffer_arg),
                ParseType::RWTexture2DArray => ObjectType::RWTexture2DArray(buffer_arg),
                ParseType::RWTexture3D => ObjectType::RWTexture3D(buffer_arg),
                _ => unreachable!(),
            };
            Ok((rest, ty))
        }

        ParseType::StructuredBuffer
        | ParseType::RWStructuredBuffer
        | ParseType::AppendStructuredBuffer
        | ParseType::ConsumeStructuredBuffer
        | ParseType::ConstantBuffer => {
            let (buffer_arg, rest) = match nom::sequence::delimited(
                match_left_angle_bracket,
                contextual(parse_structured_type, st),
                match_right_angle_bracket,
            )(rest)
            {
                Ok((rest, ty)) => (ty, rest),
                Err(nom::Err::Incomplete(needed)) => return Err(nom::Err::Incomplete(needed)),
                Err(_) => (
                    StructuredType(
                        StructuredLayout::Vector(ScalarType::Float, 4),
                        TypeModifier::default(),
                    ),
                    rest,
                ),
            };

            let ty = match object_type {
                ParseType::StructuredBuffer => ObjectType::StructuredBuffer(buffer_arg),
                ParseType::RWStructuredBuffer => ObjectType::RWStructuredBuffer(buffer_arg),
                ParseType::AppendStructuredBuffer => ObjectType::AppendStructuredBuffer(buffer_arg),
                ParseType::ConsumeStructuredBuffer => {
                    ObjectType::ConsumeStructuredBuffer(buffer_arg)
                }
                ParseType::ConstantBuffer => ObjectType::ConstantBuffer(buffer_arg),
                _ => unreachable!(),
            };
            Ok((rest, ty))
        }

        ParseType::InputPatch => Ok((rest, ObjectType::InputPatch)),
        ParseType::OutputPatch => Ok((rest, ObjectType::OutputPatch)),
    }
}

/// Parse the void type
fn parse_voidtype(input: &[LexToken]) -> ParseResult<TypeLayout> {
    if input.is_empty() {
        Err(nom::Err::Incomplete(nom::Needed::new(1)))
    } else {
        match &input[0] {
            &LexToken(Token::Id(Identifier(ref name)), _) => {
                if name == "void" {
                    Ok((&input[1..], TypeLayout::Void))
                } else {
                    Err(nom::Err::Error(ParseErrorContext(
                        input,
                        ParseErrorReason::UnknownType,
                    )))
                }
            }
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

/// Parse a type layout
fn parse_type_layout<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, TypeLayout> {
    if let Ok((input, ty)) = parse_object_type(input, st) {
        return Ok((input, TypeLayout::Object(ty)));
    }

    if let Ok((input, ty)) = parse_voidtype(input) {
        return Ok((input, ty));
    }

    if let Ok((input, _)) = parse_token(Token::SamplerState)(input) {
        return Ok((input, TypeLayout::SamplerState));
    }

    match parse_structured_layout(input, st) {
        Ok((input, sl)) => Ok((input, TypeLayout::from(sl))),
        Err(err) => Err(err),
    }
}

/// Parse a type
pub fn parse_type<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Type> {
    // Todo: modifiers that aren't const
    let (input, is_const) = match parse_token(Token::Const)(input) {
        Ok((input, _)) => (input, true),
        Err(_) => (input, false),
    };

    let (input, tl) = parse_type_layout(input, st)?;

    let tm = TypeModifier {
        is_const,
        ..TypeModifier::default()
    };
    Ok((input, Type(tl, tm)))
}
