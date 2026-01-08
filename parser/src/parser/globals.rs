use super::declarations::parse_location_annotation;
use super::statements::parse_attribute;
use super::*;

/// Parse a single line in a constant buffer definition
fn parse_constant_variable(input: &[LexToken]) -> ParseResult<'_, ConstantVariable> {
    let (input, typename) = parse_type(input)?;
    let (input, defs) = parse_init_declarators(input)?;
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
        defs: Vec::from([InitDeclarator {
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["wvp".to_string().loc(9)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            init: None,
        }]),
    };
    constantvariable.check(test_cbuffervar_str, test_cbuffervar_ast);
}

/// Parse a constant buffer definition
pub fn parse_constant_buffer(input: &[LexToken]) -> ParseResult<'_, ConstantBuffer> {
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
    let (input, _) = parse_token(Token::ConstantBuffer)(input)?;
    let (input, name) = parse_variable_name(input)?;
    let (input, location_annotations) = parse_multiple(parse_location_annotation)(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = parse_multiple(parse_constant_variable)(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let cb = ConstantBuffer {
        name,
        location_annotations,
        members,
        attributes,
    };
    Ok((input, cb))
}

/// Parse a global variable definition
pub fn parse_global_variable(input: &[LexToken]) -> ParseResult<'_, GlobalVariable> {
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
    let (input, typename) = parse_type(input)?;
    let (input, defs) = parse_init_declarators(input)?;
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["g_myBuffer".to_string().loc(7)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::from([LocationAnnotation::Register(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                })]),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["g_myBuffer".to_string().loc(14)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::from([LocationAnnotation::Register(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                })]),
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
                &[ExpressionOrType::Type(TypeId::from(
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["g_myBuffer".to_string().loc(23)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::from([LocationAnnotation::Register(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                })]),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["g_myBuffer".to_string().loc(29)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::from([LocationAnnotation::Register(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::T,
                        index: 1,
                    }),
                    space: None,
                })]),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["c_numElements".to_string().loc(17)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::new(),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Array(ArrayDeclarator {
                    inner: Box::new(Declarator::Identifier(
                        ScopedIdentifier {
                            base: ScopedIdentifierBase::Relative,
                            identifiers: Vec::from(["data".to_string().loc(17)]),
                        },
                        Vec::new(),
                    )),
                    array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(22)),
                    attributes: Vec::new(),
                }),
                location_annotations: Vec::new(),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Array(ArrayDeclarator {
                    inner: Box::new(Declarator::Identifier(
                        ScopedIdentifier {
                            base: ScopedIdentifierBase::Relative,
                            identifiers: Vec::from(["local_data".to_string().loc(19)]),
                        },
                        Vec::new(),
                    )),
                    array_size: Some(Expression::Literal(Literal::IntUntyped(32)).bloc(30)),
                    attributes: Vec::new(),
                }),
                location_annotations: Vec::new(),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Array(ArrayDeclarator {
                    inner: Box::new(Declarator::Array(ArrayDeclarator {
                        inner: Box::new(Declarator::Identifier(
                            ScopedIdentifier {
                                base: ScopedIdentifierBase::Relative,
                                identifiers: Vec::from(["data".to_string().loc(17)]),
                            },
                            Vec::new(),
                        )),
                        array_size: Some(Expression::Literal(Literal::IntUntyped(2)).bloc(22)),
                        attributes: Vec::new(),
                    })),
                    array_size: Some(Expression::Literal(Literal::IntUntyped(3)).bloc(25)),
                    attributes: Vec::new(),
                }),
                location_annotations: Vec::new(),
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
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["s".to_string().loc(2)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::new(),
                init: Some(Initializer::Expression(
                    Expression::Identifier(ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["N".to_string().loc(6), "p".to_string().loc(9)]),
                    })
                    .loc(6),
                )),
            }]),
            attributes: Vec::new(),
        },
    );

    globalvariable.check(
        "float const*& x;",
        GlobalVariable {
            global_type: Type::from("float".loc(0)).with_modifiers(&[TypeModifier::Const.loc(6)]),
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Pointer(PointerDeclarator {
                    attributes: Vec::new(),
                    qualifiers: Default::default(),
                    inner: Box::new(Declarator::Reference(ReferenceDeclarator {
                        attributes: Vec::new(),
                        inner: Box::new(Declarator::Identifier(
                            ScopedIdentifier {
                                base: ScopedIdentifierBase::Relative,
                                identifiers: Vec::from(["x".to_string().loc(14)]),
                            },
                            Vec::new(),
                        )),
                    })),
                }),
                location_annotations: Vec::new(),
                init: None,
            }]),
            attributes: Vec::new(),
        },
    );
}
#[test]
fn test_sampler_state() {
    use test_support::*;
    let globalvariable = ParserTester::new(parse_global_variable);

    let s = "SamplerState s = StaticSampler { Filter = MIN_MAG_MIP_LINEAR; };";
    globalvariable.check(
        s,
        GlobalVariable {
            global_type: Type::from("SamplerState".loc(0)),
            defs: Vec::from([InitDeclarator {
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["s".to_string().loc(13)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::new(),
                init: Some(Initializer::StaticSampler(Vec::from([PipelineProperty {
                    property: String::from("Filter").loc(33),
                    value: Located::new(
                        PipelinePropertyValue::Single(Expression::Identifier(
                            ScopedIdentifier::from("MIN_MAG_MIP_LINEAR".loc(42)),
                        )),
                        SourceLocation::first().offset(42),
                    ),
                }]))),
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
            location_annotations: Vec::new(),
            members: Vec::from([ConstantVariable {
                ty: Type::from("float4x4".loc(18)),
                defs: Vec::from([InitDeclarator {
                    declarator: Declarator::Identifier(
                        ScopedIdentifier {
                            base: ScopedIdentifierBase::Relative,
                            identifiers: Vec::from(["wvp".to_string().loc(27)]),
                        },
                        Vec::new(),
                    ),
                    location_annotations: Vec::new(),
                    init: None,
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
                defs: Vec::from([InitDeclarator {
                    declarator: Declarator::Identifier(
                        ScopedIdentifier {
                            base: ScopedIdentifierBase::Relative,
                            identifiers: Vec::from(["wvp".to_string().loc(65)]),
                        },
                        Vec::new(),
                    ),
                    location_annotations: Vec::new(),
                    init: None,
                }]),
            },
            ConstantVariable {
                ty: Type::from("float".loc(70)),
                defs: Vec::from([
                    InitDeclarator {
                        declarator: Declarator::Identifier(
                            ScopedIdentifier {
                                base: ScopedIdentifierBase::Relative,
                                identifiers: Vec::from(["x".to_string().loc(76)]),
                            },
                            Vec::new(),
                        ),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new(Declarator::Identifier(
                                ScopedIdentifier {
                                    base: ScopedIdentifierBase::Relative,
                                    identifiers: Vec::from(["y".to_string().loc(79)]),
                                },
                                Vec::new(),
                            )),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(2)).bloc(81)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new(Declarator::Array(ArrayDeclarator {
                                inner: Box::new(Declarator::Identifier(
                                    ScopedIdentifier {
                                        base: ScopedIdentifierBase::Relative,
                                        identifiers: Vec::from(["z".to_string().loc(85)]),
                                    },
                                    Vec::new(),
                                )),
                                array_size: Some(
                                    Expression::Literal(Literal::IntUntyped(3)).bloc(87),
                                ),
                                attributes: Vec::new(),
                            })),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(90)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
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
                location_annotations: Vec::from([LocationAnnotation::Register(Register {
                    slot: Some(RegisterSlot {
                        slot_type: RegisterType::B,
                        index: 12,
                    }),
                    space: None,
                })]),
                members,
                attributes,
            },
        );
    }
}
