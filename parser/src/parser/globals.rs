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

/// Parse a register slot for a constant buffer
fn parse_constant_slot(input: &[LexToken]) -> ParseResult<ConstantSlot> {
    match input {
        [LexToken(Token::Colon, _), LexToken(Token::Register(reg), _), rest @ ..] => match *reg {
            RegisterSlot::B(slot) => Ok((rest, ConstantSlot(slot))),
            _ => ParseErrorReason::WrongSlotType.into_result(input),
        },
        _ => ParseErrorReason::wrong_token(input),
    }
}

#[test]
fn test_constant_slot() {
    use test_support::*;

    let cbuffer_register = ParserTester::new(parse_constant_slot);
    cbuffer_register.check(" : register(b12) ", ConstantSlot(12));
}

/// Parse a constant buffer definition
pub fn parse_constant_buffer(input: &[LexToken]) -> ParseResult<ConstantBuffer> {
    let (input, _) = parse_token(Token::ConstantBuffer)(input)?;
    let (input, name) = parse_variable_name(input)?;
    let (input, slot) = parse_optional(parse_constant_slot)(input)?;
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

/// Parse a register slot for a resource
fn parse_global_slot(input: &[LexToken]) -> ParseResult<Register> {
    match input {
        [LexToken(Token::Colon, _), LexToken(Token::Register(reg), _), rest @ ..] => match *reg {
            RegisterSlot::T(slot) => Ok((
                rest,
                Register {
                    slot_type: RegisterType::T,
                    index: slot,
                },
            )),
            RegisterSlot::U(slot) => Ok((
                rest,
                Register {
                    slot_type: RegisterType::U,
                    index: slot,
                },
            )),
            RegisterSlot::S(slot) => Ok((
                rest,
                Register {
                    slot_type: RegisterType::S,
                    index: slot,
                },
            )),
            RegisterSlot::B(slot) => Ok((
                rest,
                Register {
                    slot_type: RegisterType::B,
                    index: slot,
                },
            )),
        },
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Parse a single name in a global variable definition
fn parse_global_variable_name(input: &[LexToken]) -> ParseResult<GlobalVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;
    let (input, slot) = parse_optional(parse_global_slot)(input)?;
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
            slot: Some(ConstantSlot(12)),
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
