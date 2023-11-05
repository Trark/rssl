use super::statements::parse_attribute;
use super::*;

/// Parse a single named constant in a constant buffer definition
fn parse_constant_variable_name(input: &[LexToken]) -> ParseResult<ConstantVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;
    let v = ConstantVariableName {
        name,
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
            name: "wvp".to_string().loc(9),
            bind: Default::default(),
            offset: None,
        }]),
    };
    constantvariable.check(test_cbuffervar_str, test_cbuffervar_ast);
}

/// Parse a constant buffer definition
pub fn parse_constant_buffer(input: &[LexToken]) -> ParseResult<ConstantBuffer> {
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
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
        attributes,
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
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
    let (input, typename) = parse_type(input)?;
    let (input, defs) =
        parse_list_nonempty(parse_token(Token::Comma), parse_global_variable_name)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let var = GlobalVariable {
        global_type: typename,
        defs,
        attributes,
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
            global_type: Type::from("Buffer".loc(0)),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(7),
                bind: Default::default(),
                slot: Some(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                }),
                init: None,
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "Buffer<uint4> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::with_template_types(
                "Buffer".loc(0),
                &[ExpressionOrType::from("uint4".loc(7))],
            )),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(14),
                bind: Default::default(),
                slot: Some(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                }),
                init: None,
            }]),
            attributes: Vec::new(),
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
                                Expression::Literal(Literal::IntUntyped(4)).loc(19),
                            ),
                        ],
                    ),
                ))],
            )),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(23),
                bind: Default::default(),
                slot: Some(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                }),
                init: None,
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "StructuredBuffer<CustomType> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::with_template_types(
                "StructuredBuffer".loc(0),
                &[ExpressionOrType::from("CustomType".loc(17))],
            )),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(29),
                bind: Default::default(),
                slot: Some(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                }),
                init: None,
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "static const int c_numElements = 4;",
        GlobalVariable {
            global_type: Type {
                layout: TypeLayout::from("int".loc(13)),
                modifiers: TypeModifierSet::from(&[
                    TypeModifier::Static.loc(0),
                    TypeModifier::Const.loc(7),
                ]),
                location: SourceLocation::first(),
            },
            defs: Vec::from([GlobalVariableName {
                name: "c_numElements".to_string().loc(17),
                bind: Default::default(),
                slot: None,
                init: Some(Initializer::Expression(
                    Expression::Literal(Literal::IntUntyped(4)).loc(33),
                )),
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "static const int data[4] = { 0, 1, 2, 3 };",
        GlobalVariable {
            global_type: Type {
                layout: TypeLayout::from("int".loc(13)),
                modifiers: TypeModifierSet::from(&[
                    TypeModifier::Static.loc(0),
                    TypeModifier::Const.loc(7),
                ]),
                location: SourceLocation::first(),
            },
            defs: Vec::from([GlobalVariableName {
                name: "data".to_string().loc(17),
                bind: VariableBind(Vec::from([Some(
                    Expression::Literal(Literal::IntUntyped(4)).loc(22),
                )])),
                slot: None,
                init: Some(Initializer::Aggregate(Vec::from([
                    Initializer::Expression(Expression::Literal(Literal::IntUntyped(0)).loc(29)),
                    Initializer::Expression(Expression::Literal(Literal::IntUntyped(1)).loc(32)),
                    Initializer::Expression(Expression::Literal(Literal::IntUntyped(2)).loc(35)),
                    Initializer::Expression(Expression::Literal(Literal::IntUntyped(3)).loc(38)),
                ]))),
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "groupshared float4 local_data[32];",
        GlobalVariable {
            global_type: Type {
                layout: TypeLayout::from("float4".loc(12)),
                modifiers: TypeModifierSet::from(&[TypeModifier::GroupShared.loc(0)]),
                location: SourceLocation::first(),
            },
            defs: Vec::from([GlobalVariableName {
                name: "local_data".to_string().loc(19),
                bind: VariableBind(Vec::from([Some(
                    Expression::Literal(Literal::IntUntyped(32)).loc(30),
                )])),
                slot: None,
                init: None,
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "static const int data[2][3] = { { 0, 1 }, { 2, 3 }, { 4, 5 } };",
        GlobalVariable {
            global_type: Type {
                layout: TypeLayout::from("int".loc(13)),
                modifiers: TypeModifierSet::from(&[
                    TypeModifier::Static.loc(0),
                    TypeModifier::Const.loc(7),
                ]),
                location: SourceLocation::first(),
            },
            defs: Vec::from([GlobalVariableName {
                name: "data".to_string().loc(17),
                bind: VariableBind(Vec::from([
                    Some(Expression::Literal(Literal::IntUntyped(2)).loc(22)),
                    Some(Expression::Literal(Literal::IntUntyped(3)).loc(25)),
                ])),
                slot: None,
                init: Some(Initializer::Aggregate(Vec::from([
                    Initializer::Aggregate(Vec::from([
                        Initializer::Expression(
                            Expression::Literal(Literal::IntUntyped(0)).loc(34),
                        ),
                        Initializer::Expression(
                            Expression::Literal(Literal::IntUntyped(1)).loc(37),
                        ),
                    ])),
                    Initializer::Aggregate(Vec::from([
                        Initializer::Expression(
                            Expression::Literal(Literal::IntUntyped(2)).loc(44),
                        ),
                        Initializer::Expression(
                            Expression::Literal(Literal::IntUntyped(3)).loc(47),
                        ),
                    ])),
                    Initializer::Aggregate(Vec::from([
                        Initializer::Expression(
                            Expression::Literal(Literal::IntUntyped(4)).loc(54),
                        ),
                        Initializer::Expression(
                            Expression::Literal(Literal::IntUntyped(5)).loc(57),
                        ),
                    ])),
                ]))),
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "S s = N::p;",
        GlobalVariable {
            global_type: Type::from("S".loc(0)),
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
            attributes: Vec::new(),
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
                    name: "wvp".to_string().loc(27),
                    bind: Default::default(),
                    offset: None,
                }]),
            }]),
            attributes: Vec::new(),
        },
    );

    {
        let input = "[[vk::binding(1, 2)]] cbuffer globals : register(b12) { float4x4 wvp; float x, y[2], z[3][4]; }";

        let members = Vec::from([
            ConstantVariable {
                ty: Type::from("float4x4".loc(56)),
                defs: vec![ConstantVariableName {
                    name: "wvp".to_string().loc(65),
                    bind: Default::default(),
                    offset: None,
                }],
            },
            ConstantVariable {
                ty: Type::from("float".loc(70)),
                defs: Vec::from([
                    ConstantVariableName {
                        name: "x".to_string().loc(76),
                        bind: Default::default(),
                        offset: None,
                    },
                    ConstantVariableName {
                        name: "y".to_string().loc(79),
                        bind: VariableBind(Vec::from([Some(
                            Expression::Literal(Literal::IntUntyped(2)).loc(81),
                        )])),
                        offset: None,
                    },
                    ConstantVariableName {
                        name: "z".to_string().loc(85),
                        bind: VariableBind(Vec::from([
                            Some(Expression::Literal(Literal::IntUntyped(3)).loc(87)),
                            Some(Expression::Literal(Literal::IntUntyped(4)).loc(90)),
                        ])),
                        offset: None,
                    },
                ]),
            },
        ]);

        let attributes = Vec::from([Attribute {
            name: Vec::from(["vk".to_string().loc(2), "binding".to_string().loc(6)]),
            arguments: Vec::from([
                Expression::Literal(Literal::IntUntyped(1)).loc(14),
                Expression::Literal(Literal::IntUntyped(2)).loc(17),
            ]),
            two_square_brackets: true,
        }]);

        cbuffer.check(
            input,
            ConstantBuffer {
                name: "globals".to_string().loc(30),
                slot: Some(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::B,
                        index: 12,
                    }),
                    space: None,
                }),
                members,
                attributes,
            },
        );
    }
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
    let (mut input, id) = match_identifier(input)?;

    let mut slot = None;
    let mut space = None;

    if let Some(index) = parse_space_identifier(&id.0) {
        space = Some(index);
    } else {
        // Identifiers must have at least one character
        // Only single byte chars should be valid identifiers so the split is safe
        let (register_type_char, register_index_str) = id.0.split_at(1);

        let slot_type = match register_type_char {
            "t" => RegisterType::T,
            "u" => RegisterType::U,
            "b" => RegisterType::B,
            "s" => RegisterType::S,
            _ => {
                return ParseErrorReason::InvalidSlotType(id.0.clone())
                    .into_result(identifier_input_start)
            }
        };

        let index = {
            if register_index_str.chars().all(|c| char::is_ascii_digit(&c)) {
                match register_index_str.parse::<u32>() {
                    Ok(v) => v,
                    Err(_) => {
                        return ParseErrorReason::InvalidSlotIndex(id.0.clone())
                            .into_result(identifier_input_start)
                    }
                }
            } else {
                return ParseErrorReason::InvalidSlotIndex(id.0.clone())
                    .into_result(identifier_input_start);
            }
        };

        slot = Some(RegisterSlot { slot_type, index });
    }

    if space.is_none() {
        // Parse an optional space identifier
        let (rest, space_identifier) = match parse_token(Token::Comma)(input) {
            Ok((space_start, _)) => match match_identifier(space_start) {
                Ok((input, id)) => (input, Some((id, space_start))),
                Err(err) => return Err(err),
            },
            Err(_) => (input, None),
        };

        // Convert the space identifier into a space index
        space = match space_identifier {
            Some((id, space_start)) => match parse_space_identifier(&id.0) {
                Some(index) => Some(index),
                None => {
                    return ParseErrorReason::InvalidSpaceIdentifier(id.0.clone())
                        .into_result(space_start)
                }
            },
            None => None,
        };

        input = rest;
    }

    let (input, _) = parse_token(Token::RightParen)(input)?;

    let register = Register { slot, space };
    Ok((input, Some(register)))
}

/// Attempt to turn a space identifier into a space index
fn parse_space_identifier(space_identifier: &str) -> Option<u32> {
    if let Some(index_string) = space_identifier.strip_prefix("space") {
        if let Ok(index) = index_string.parse::<u32>() {
            Some(index)
        } else {
            None
        }
    } else {
        None
    }
}

#[test]
fn test_register() {
    use test_support::*;
    let register = ParserTester::new(parse_register);

    register.check(
        ": register(t0)",
        Some(Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 0,
            }),
            space: None,
        }),
    );

    register.check(
        ": register(t1)",
        Some(Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 1,
            }),
            space: None,
        }),
    );

    register.check(
        ": register(u1)",
        Some(Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::U,
                index: 1,
            }),
            space: None,
        }),
    );

    register.expect_fail(
        ": register(p0)",
        ParseErrorReason::InvalidSlotType("p0".to_string()),
        11,
    );

    register.expect_fail(
        ": register(tY)",
        ParseErrorReason::InvalidSlotIndex("tY".to_string()),
        11,
    );

    register.expect_fail(
        ": register(pY)",
        ParseErrorReason::InvalidSlotType("pY".to_string()),
        11,
    );

    register.check(
        ": register(t4294967295)",
        Some(Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: u32::MAX,
            }),
            space: None,
        }),
    );

    register.expect_fail(
        ": register(t4294967296)",
        ParseErrorReason::InvalidSlotIndex("t4294967296".to_string()),
        11,
    );

    register.check(
        ": register(t1, space2)",
        Some(Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 1,
            }),
            space: Some(2),
        }),
    );

    register.expect_fail(
        ": register(t1, space)",
        ParseErrorReason::InvalidSpaceIdentifier("space".to_string()),
        15,
    );

    register.check(
        ": register(t1, space4294967295)",
        Some(Register {
            slot: Some(RegisterSlot {
                slot_type: RegisterType::T,
                index: 1,
            }),
            space: Some(u32::MAX),
        }),
    );

    register.expect_fail(
        ": register(t1, space4294967296)",
        ParseErrorReason::InvalidSpaceIdentifier("space4294967296".to_string()),
        15,
    );

    register.check(
        ": register(space2)",
        Some(Register {
            slot: None,
            space: Some(2),
        }),
    );

    register.expect_fail(
        ": register(space2, space3)",
        ParseErrorReason::WrongToken,
        17,
    );

    // This parser does not expect semicolon on the end
    register.expect_fail(": register(t0);", ParseErrorReason::TokensUnconsumed, 14);
}
