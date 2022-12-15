use super::*;

/// Parse the type for a global variable
fn parse_global_type(input: &[LexToken]) -> ParseResult<GlobalType> {
    if input.is_empty() {
        return ParseErrorReason::end_of_stream();
    }
    // Non-standard combinations of storage classes unimplemented
    let (input, gs) = match input[0] {
        LexToken(Token::Static, _) => (&input[1..], Some(GlobalStorage::Static)),
        LexToken(Token::GroupShared, _) => (&input[1..], Some(GlobalStorage::GroupShared)),
        LexToken(Token::Extern, _) => (&input[1..], Some(GlobalStorage::Extern)),
        _ => (input, None),
    };
    let (input, ty) = parse_type(input)?;
    let gt = GlobalType(ty, gs.unwrap_or_default());
    Ok((input, gt))
}

/// Parse a single named constant in a constant buffer definition
fn parse_constant_variable_name(input: &[LexToken]) -> ParseResult<ConstantVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;
    let v = ConstantVariableName {
        name: name.to_node(),
        bind: VariableBind(bind),
        offset: None,
    };
    Ok((input, v))
}

/// Parse a single line in a constant buffer definition
fn parse_constant_variable(input: &[LexToken]) -> ParseResult<ConstantVariable> {
    let (input, typename) = parse_type(input)?;
    let (input, defs) =
        parse_list_nonempty(parse_token(Token::Comma), parse_constant_variable_name)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let var = ConstantVariable { ty: typename, defs };
    Ok((input, var))
}

#[test]
fn test_constant_variable() {
    use test_support::*;
    let constantvariable = ParserTester::new(parse_constant_variable);

    let test_cbuffervar_str = "float4x4 wvp;";
    let test_cbuffervar_ast = ConstantVariable {
        ty: Type::from("float4x4".loc(0)),
        defs: Vec::from([ConstantVariableName {
            name: "wvp".to_string(),
            bind: Default::default(),
            offset: None,
        }]),
    };
    constantvariable.check(test_cbuffervar_str, test_cbuffervar_ast);
}

/// Parse a constant buffer definition
pub fn parse_constant_buffer(input: &[LexToken]) -> ParseResult<ConstantBuffer> {
    let (input, _) = parse_token(Token::ConstantBuffer)(input)?;
    let (input, name) = parse_variable_name(input)?;
    let (input, slot) = parse_register(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = parse_multiple(parse_constant_variable)(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let cb = ConstantBuffer {
        name,
        slot,
        members,
    };
    Ok((input, cb))
}

/// Parse a single name in a global variable definition
fn parse_global_variable_name(input: &[LexToken]) -> ParseResult<GlobalVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;
    let (input, slot) = parse_register(input)?;
    let (input, init) = parse_initializer(input)?;
    let v = GlobalVariableName {
        name,
        bind: VariableBind(bind),
        slot,
        init,
    };
    Ok((input, v))
}

/// Parse a global variable definition
pub fn parse_global_variable(input: &[LexToken]) -> ParseResult<GlobalVariable> {
    let (input, typename) = parse_global_type(input)?;
    let (input, defs) =
        parse_list_nonempty(parse_token(Token::Comma), parse_global_variable_name)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let var = GlobalVariable {
        global_type: typename,
        defs,
    };
    Ok((input, var))
}

#[test]
fn test_global_variable() {
    use test_support::*;
    let globalvariable = ParserTester::new(parse_global_variable);

    globalvariable.check(
        "Buffer g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from("Buffer".loc(0)).into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(7),
                bind: Default::default(),
                slot: Some(Register {
                    slot_type: RegisterType::T,
                    index: 1,
                }),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "Buffer<uint4> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::with_template_types(
                "Buffer".loc(0),
                &[ExpressionOrType::from("uint4".loc(7))],
            ))
            .into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(14),
                bind: Default::default(),
                slot: Some(Register {
                    slot_type: RegisterType::T,
                    index: 1,
                }),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "Buffer<vector<int, 4>> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::with_template_types(
                "Buffer".loc(0),
                &[ExpressionOrType::Type(Type::from_layout(
                    TypeLayout::with_template_types(
                        "vector".loc(7),
                        &[
                            ExpressionOrType::from("int".loc(14)),
                            ExpressionOrType::Expression(
                                Expression::Literal(Literal::UntypedInt(4)).loc(19),
                            ),
                        ],
                    ),
                ))],
            ))
            .into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(23),
                bind: Default::default(),
                slot: Some(Register {
                    slot_type: RegisterType::T,
                    index: 1,
                }),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "StructuredBuffer<CustomType> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::with_template_types(
                "StructuredBuffer".loc(0),
                &[ExpressionOrType::from("CustomType".loc(17))],
            ))
            .into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(29),
                bind: Default::default(),
                slot: Some(Register {
                    slot_type: RegisterType::T,
                    index: 1,
                }),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "static const int c_numElements = 4;",
        GlobalVariable {
            global_type: GlobalType(
                Type {
                    layout: TypeLayout::from("int".loc(13)),
                    modifier: TypeModifier {
                        is_const: true,
                        ..TypeModifier::default()
                    },
                    location: SourceLocation::first().offset(7),
                },
                GlobalStorage::Static,
            ),
            defs: Vec::from([GlobalVariableName {
                name: "c_numElements".to_string().loc(17),
                bind: Default::default(),
                slot: None,
                init: Some(Initializer::Expression(
                    Expression::Literal(Literal::UntypedInt(4)).loc(33),
                )),
            }]),
        },
    );

    globalvariable.check(
        "static const int data[4] = { 0, 1, 2, 3 };",
        GlobalVariable {
            global_type: GlobalType(
                Type {
                    layout: TypeLayout::from("int".loc(13)),
                    modifier: TypeModifier {
                        is_const: true,
                        ..TypeModifier::default()
                    },
                    location: SourceLocation::first().offset(7),
                },
                GlobalStorage::Static,
            ),
            defs: Vec::from([GlobalVariableName {
                name: "data".to_string().loc(17),
                bind: VariableBind(Vec::from([Some(
                    Expression::Literal(Literal::UntypedInt(4)).loc(22),
                )])),
                slot: None,
                init: Some(Initializer::Aggregate(Vec::from([
                    Initializer::Expression(Expression::Literal(Literal::UntypedInt(0)).loc(29)),
                    Initializer::Expression(Expression::Literal(Literal::UntypedInt(1)).loc(32)),
                    Initializer::Expression(Expression::Literal(Literal::UntypedInt(2)).loc(35)),
                    Initializer::Expression(Expression::Literal(Literal::UntypedInt(3)).loc(38)),
                ]))),
            }]),
        },
    );

    globalvariable.check(
        "groupshared float4 local_data[32];",
        GlobalVariable {
            global_type: GlobalType(Type::from("float4".loc(12)), GlobalStorage::GroupShared),
            defs: Vec::from([GlobalVariableName {
                name: "local_data".to_string().loc(19),
                bind: VariableBind(Vec::from([Some(
                    Expression::Literal(Literal::UntypedInt(32)).loc(30),
                )])),
                slot: None,
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "static const int data[2][3] = { { 0, 1 }, { 2, 3 }, { 4, 5 } };",
        GlobalVariable {
            global_type: GlobalType(
                Type {
                    layout: TypeLayout::from("int".loc(13)),
                    modifier: TypeModifier {
                        is_const: true,
                        ..TypeModifier::default()
                    },
                    location: SourceLocation::first().offset(7),
                },
                GlobalStorage::Static,
            ),
            defs: Vec::from([GlobalVariableName {
                name: "data".to_string().loc(17),
                bind: VariableBind(Vec::from([
                    Some(Expression::Literal(Literal::UntypedInt(2)).loc(22)),
                    Some(Expression::Literal(Literal::UntypedInt(3)).loc(25)),
                ])),
                slot: None,
                init: Some(Initializer::Aggregate(Vec::from([
                    Initializer::Aggregate(Vec::from([
                        Initializer::Expression(
                            Expression::Literal(Literal::UntypedInt(0)).loc(34),
                        ),
                        Initializer::Expression(
                            Expression::Literal(Literal::UntypedInt(1)).loc(37),
                        ),
                    ])),
                    Initializer::Aggregate(Vec::from([
                        Initializer::Expression(
                            Expression::Literal(Literal::UntypedInt(2)).loc(44),
                        ),
                        Initializer::Expression(
                            Expression::Literal(Literal::UntypedInt(3)).loc(47),
                        ),
                    ])),
                    Initializer::Aggregate(Vec::from([
                        Initializer::Expression(
                            Expression::Literal(Literal::UntypedInt(4)).loc(54),
                        ),
                        Initializer::Expression(
                            Expression::Literal(Literal::UntypedInt(5)).loc(57),
                        ),
                    ])),
                ]))),
            }]),
        },
    );

    globalvariable.check(
        "S s = N::p;",
        GlobalVariable {
            global_type: GlobalType(Type::from("S".loc(0)), GlobalStorage::Extern),
            defs: Vec::from([GlobalVariableName {
                name: "s".to_string().loc(2),
                bind: Default::default(),
                init: Some(Initializer::Expression(
                    Expression::Identifier(ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["N".to_string().loc(6), "p".to_string().loc(9)]),
                    })
                    .loc(6),
                )),
                slot: None,
            }]),
        },
    );
}

#[test]
fn test_constant_buffer() {
    use test_support::*;
    let cbuffer = ParserTester::new(parse_constant_buffer);

    cbuffer.check(
        "cbuffer globals { float4x4 wvp; }",
        ConstantBuffer {
            name: "globals".to_string().loc(8),
            slot: None,
            members: Vec::from([ConstantVariable {
                ty: Type::from("float4x4".loc(18)),
                defs: Vec::from([ConstantVariableName {
                    name: "wvp".to_string(),
                    bind: Default::default(),
                    offset: None,
                }]),
            }]),
        },
    );

    cbuffer.check(
        "cbuffer globals : register(b12) { float4x4 wvp; float x, y[2], z[3][4]; }",
        ConstantBuffer {
            name: "globals".to_string().loc(8),
            slot: Some(Register {
                slot_type: RegisterType::B,
                index: 12,
            }),
            members: Vec::from([
                ConstantVariable {
                    ty: Type::from("float4x4".loc(34)),
                    defs: vec![ConstantVariableName {
                        name: "wvp".to_string(),
                        bind: Default::default(),
                        offset: None,
                    }],
                },
                ConstantVariable {
                    ty: Type::from("float".loc(48)),
                    defs: Vec::from([
                        ConstantVariableName {
                            name: "x".to_string(),
                            bind: Default::default(),
                            offset: None,
                        },
                        ConstantVariableName {
                            name: "y".to_string(),
                            bind: VariableBind(Vec::from([Some(
                                Expression::Literal(Literal::UntypedInt(2)).loc(59),
                            )])),
                            offset: None,
                        },
                        ConstantVariableName {
                            name: "z".to_string(),
                            bind: VariableBind(Vec::from([
                                Some(Expression::Literal(Literal::UntypedInt(3)).loc(65)),
                                Some(Expression::Literal(Literal::UntypedInt(4)).loc(68)),
                            ])),
                            offset: None,
                        },
                    ]),
                },
            ]),
        },
    );
}

/// Parse a register slot for a resource
fn parse_register(input: &[LexToken]) -> ParseResult<Option<Register>> {
    let input = match parse_token(Token::Colon)(input) {
        Ok((input, _)) => input,
        Err(_) => return Ok((input, None)),
    };

    let (input, _) = parse_token(Token::Register)(input)?;
    let (input, _) = parse_token(Token::LeftParen)(input)?;
    let identifier_input_start = input;
    let (input, id) = match_identifier(input)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;

    // Identifiers must have at least one character
    // Only single byte chars should be valid identifiers so the split is safe
    let (register_type_char, register_index_str) = id.0.split_at(1);

    let slot_type = match register_type_char {
        "t" => RegisterType::T,
        "u" => RegisterType::U,
        "b" => RegisterType::B,
        "s" => RegisterType::S,
        _ => {
            return Err(ParseErrorContext(
                identifier_input_start,
                ParseErrorReason::InvalidSlotType(id.0.clone()),
            ))
        }
    };

    let index = {
        if register_index_str.chars().all(|c| char::is_ascii_digit(&c)) {
            match register_index_str.parse::<u32>() {
                Ok(v) => v,
                Err(_) => {
                    return Err(ParseErrorContext(
                        identifier_input_start,
                        ParseErrorReason::InvalidSlotIndex(id.0.clone()),
                    ));
                }
            }
        } else {
            return Err(ParseErrorContext(
                identifier_input_start,
                ParseErrorReason::InvalidSlotIndex(id.0.clone()),
            ));
        }
    };

    let register = Register { slot_type, index };
    Ok((input, Some(register)))
}

#[test]
fn test_register() {
    use test_support::*;
    let globalvariable = ParserTester::new(parse_register);

    globalvariable.check(
        ": register(t0)",
        Some(Register {
            slot_type: RegisterType::T,
            index: 0,
        }),
    );

    globalvariable.check(
        ": register(t1)",
        Some(Register {
            slot_type: RegisterType::T,
            index: 1,
        }),
    );

    globalvariable.check(
        ": register(u1)",
        Some(Register {
            slot_type: RegisterType::U,
            index: 1,
        }),
    );

    globalvariable.expect_fail(
        ": register(p0)",
        ParseErrorReason::InvalidSlotType("p0".to_string()),
        11,
    );

    globalvariable.expect_fail(
        ": register(tY)",
        ParseErrorReason::InvalidSlotIndex("tY".to_string()),
        11,
    );

    globalvariable.expect_fail(
        ": register(pY)",
        ParseErrorReason::InvalidSlotType("pY".to_string()),
        11,
    );

    globalvariable.check(
        ": register(t4294967295)",
        Some(Register {
            slot_type: RegisterType::T,
            index: u32::MAX,
        }),
    );

    globalvariable.expect_fail(
        ": register(t4294967296)",
        ParseErrorReason::InvalidSlotIndex("t4294967296".to_string()),
        11,
    );

    // This parser does not expect semicolon on the end
    globalvariable.expect_fail(": register(t0);", ParseErrorReason::TokensUnconsumed, 14);
}
