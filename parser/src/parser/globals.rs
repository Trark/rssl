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
    let (input, array_dim) = parse_optional(parse_arraydim)(input)?;
    let v = ConstantVariableName {
        name: name.to_node(),
        bind: match array_dim {
            Some(ref expr) => VariableBind::Array(expr.clone()),
            None => VariableBind::Normal,
        },
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
        ty: Type::float4x4(),
        defs: Vec::from([ConstantVariableName {
            name: "wvp".to_string(),
            bind: VariableBind::Normal,
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
fn parse_global_slot(input: &[LexToken]) -> ParseResult<GlobalSlot> {
    match input {
        [LexToken(Token::Colon, _), LexToken(Token::Register(reg), _), rest @ ..] => match *reg {
            RegisterSlot::T(slot) => Ok((rest, GlobalSlot::ReadSlot(slot))),
            RegisterSlot::U(slot) => Ok((rest, GlobalSlot::ReadWriteSlot(slot))),
            RegisterSlot::S(slot) => Ok((rest, GlobalSlot::SamplerSlot(slot))),
            RegisterSlot::B(slot) => Ok((rest, GlobalSlot::ConstantSlot(slot))),
        },
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Parse a single name in a global variable definition
fn parse_global_variable_name(input: &[LexToken]) -> ParseResult<GlobalVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, array_dim) = parse_optional(parse_arraydim)(input)?;
    let (input, slot) = parse_optional(parse_global_slot)(input)?;
    let (input, init) = parse_initializer(input)?;
    let v = GlobalVariableName {
        name,
        bind: match array_dim {
            Some(ref expr) => VariableBind::Array(expr.clone()),
            None => VariableBind::Normal,
        },
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
            global_type: Type::from_layout(TypeLayout::custom("Buffer".loc(0))).into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(7),
                bind: VariableBind::Normal,
                slot: Some(GlobalSlot::ReadSlot(1)),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "Buffer<uint4> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::custom_templated(
                "Buffer".loc(0),
                Vec::from([Type::from_layout(TypeLayout::from_vector(ScalarType::UInt, 4)).loc(7)]),
            ))
            .into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(14),
                bind: VariableBind::Normal,
                slot: Some(GlobalSlot::ReadSlot(1)),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "Buffer<vector<int, 4>> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::custom_templated(
                "Buffer".loc(0),
                Vec::from([Type::from_layout(TypeLayout::from_vector(ScalarType::Int, 4)).loc(7)]),
            ))
            .into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(23),
                bind: VariableBind::Normal,
                slot: Some(GlobalSlot::ReadSlot(1)),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "StructuredBuffer<CustomType> g_myBuffer : register(t1);",
        GlobalVariable {
            global_type: Type::from_layout(TypeLayout::custom_templated(
                "StructuredBuffer".loc(0),
                Vec::from([Type::from_layout(TypeLayout::custom("CustomType".loc(17))).loc(17)]),
            ))
            .into(),
            defs: Vec::from([GlobalVariableName {
                name: "g_myBuffer".to_string().loc(29),
                bind: VariableBind::Normal,
                slot: Some(GlobalSlot::ReadSlot(1)),
                init: None,
            }]),
        },
    );

    globalvariable.check(
        "static const int c_numElements = 4;",
        GlobalVariable {
            global_type: GlobalType(
                Type(
                    TypeLayout::int(),
                    TypeModifier {
                        is_const: true,
                        ..TypeModifier::default()
                    },
                ),
                GlobalStorage::Static,
            ),
            defs: Vec::from([GlobalVariableName {
                name: "c_numElements".to_string().loc(17),
                bind: VariableBind::Normal,
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
                Type(
                    TypeLayout::int(),
                    TypeModifier {
                        is_const: true,
                        ..TypeModifier::default()
                    },
                ),
                GlobalStorage::Static,
            ),
            defs: Vec::from([GlobalVariableName {
                name: "data".to_string().loc(17),
                bind: VariableBind::Array(Some(
                    Expression::Literal(Literal::UntypedInt(4)).loc(22),
                )),
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
            global_type: GlobalType(Type::floatn(4), GlobalStorage::GroupShared),
            defs: Vec::from([GlobalVariableName {
                name: "local_data".to_string().loc(19),
                bind: VariableBind::Array(Some(
                    Expression::Literal(Literal::UntypedInt(32)).loc(30),
                )),
                slot: None,
                init: None,
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
                ty: Type::float4x4(),
                defs: Vec::from([ConstantVariableName {
                    name: "wvp".to_string(),
                    bind: VariableBind::Normal,
                    offset: None,
                }]),
            }]),
        },
    );

    cbuffer.check(
        "cbuffer globals : register(b12) { float4x4 wvp; float x, y[2]; }",
        ConstantBuffer {
            name: "globals".to_string().loc(8),
            slot: Some(ConstantSlot(12)),
            members: Vec::from([
                ConstantVariable {
                    ty: Type::float4x4(),
                    defs: vec![ConstantVariableName {
                        name: "wvp".to_string(),
                        bind: VariableBind::Normal,
                        offset: None,
                    }],
                },
                ConstantVariable {
                    ty: Type::float(),
                    defs: Vec::from([
                        ConstantVariableName {
                            name: "x".to_string(),
                            bind: VariableBind::Normal,
                            offset: None,
                        },
                        ConstantVariableName {
                            name: "y".to_string(),
                            bind: VariableBind::Array(Some(
                                Expression::Literal(Literal::UntypedInt(2)).loc(59),
                            )),
                            offset: None,
                        },
                    ]),
                },
            ]),
        },
    );
}
