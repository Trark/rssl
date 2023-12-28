use super::functions::parse_function_definition;
use super::statements::parse_attribute;
use super::*;

/// Parse a struct member variable - with potentially multiple members per line
fn parse_struct_member(input: &[LexToken]) -> ParseResult<StructMember> {
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
    let (input, typename) = parse_type(input)?;
    let (input, defs) = parse_init_declarators(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sm = StructMember {
        ty: typename,
        defs,
        attributes,
    };
    Ok((input, sm))
}

/// Parse a struct member variable or method
fn parse_struct_entry(input: &[LexToken]) -> ParseResult<StructEntry> {
    let variable_res =
        parse_struct_member(input).map(|(input, def)| (input, StructEntry::Variable(def)));
    let method_res =
        parse_function_definition(input).map(|(input, def)| (input, StructEntry::Method(def)));
    let (input, value) = variable_res.select(method_res)?;
    let (input, _) = parse_multiple(parse_token(Token::Semicolon))(input)?;
    Ok((input, value))
}

/// Parse a full struct definition
pub fn parse_struct_definition(input: &[LexToken]) -> ParseResult<StructDefinition> {
    let (input, template_params) = parse_template_params(input)?;
    let (input, _) = parse_token(Token::Struct)(input)?;
    let (input, name) = parse_variable_name(input)?;

    // Parse list of base types if present
    let (input, base_types) = match parse_token(Token::Colon)(input) {
        Ok((input, _)) => parse_list_nonempty(parse_token(Token::Comma), parse_type)(input)?,
        Err(_) => (input, Vec::new()),
    };

    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = parse_multiple(parse_struct_entry)(input)?;
    let (input, _) = parse_multiple(parse_token(Token::Semicolon))(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sd = StructDefinition {
        name,
        base_types,
        template_params,
        members,
    };
    Ok((input, sd))
}

#[test]
fn test_struct() {
    use test_support::*;
    let structdefinition = ParserTester::new(parse_struct_definition);

    structdefinition.check(
        "struct MyStruct {};",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::new()),
            members: Vec::new(),
        },
    );

    structdefinition.check(
        "struct MyStruct {;;;};",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::new()),
            members: Vec::new(),
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: Vec::from([InitDeclarator {
                    declarator: "a".loc(23).into(),
                    location_annotations: Vec::new(),
                    init: None,
                }]),
                attributes: Vec::new(),
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a, b; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: Vec::from([
                    InitDeclarator {
                        declarator: "a".loc(23).into(),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: "b".loc(26).into(),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                ]),
                attributes: Vec::new(),
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a[2], b[3][4]; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: Vec::from([
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new("a".loc(23).into()),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(2)).bloc(25)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new(Declarator::Array(ArrayDeclarator {
                                inner: Box::new("b".loc(29).into()),
                                array_size: Some(
                                    Expression::Literal(Literal::IntUntyped(3)).bloc(31),
                                ),
                                attributes: Vec::new(),
                            })),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(34)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                ]),
                attributes: Vec::new(),
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct : Parent { [[vk::offset(8)]] uint a; void f() {} };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::from([Type::from("Parent".loc(18))]),
            template_params: TemplateParamList(Vec::new()),
            members: vec![
                StructEntry::Variable(StructMember {
                    ty: Type::from("uint".loc(45)),
                    defs: Vec::from([InitDeclarator {
                        declarator: "a".loc(50).into(),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                    attributes: Vec::from([Attribute {
                        name: Vec::from(["vk".to_string().loc(29), "offset".to_string().loc(33)]),
                        arguments: Vec::from([Expression::Literal(Literal::IntUntyped(8)).loc(40)]),
                        two_square_brackets: true,
                    }]),
                }),
                StructEntry::Method(FunctionDefinition {
                    name: "f".to_string().loc(58),
                    returntype: Type::from("void".loc(53)).into(),
                    template_params: TemplateParamList(Vec::new()),
                    params: Vec::new(),
                    is_const: false,
                    is_volatile: false,
                    body: Some(Vec::new()),
                    attributes: Vec::new(),
                }),
            ],
        },
    );

    structdefinition.check(
        "template<typename T> struct MyStruct { T a, b; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(28),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::from([TemplateParam::Type(
                TemplateTypeParam {
                    name: Some("T".to_string().loc(18)),
                    default: None,
                },
            )])),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("T".loc(39)),
                defs: Vec::from([
                    InitDeclarator {
                        declarator: "a".loc(41).into(),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: "b".loc(44).into(),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                ]),
                attributes: Vec::new(),
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a[2] : USER0, b[3][4] : USER1; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            base_types: Vec::new(),
            template_params: TemplateParamList(Vec::new()),
            members: Vec::from([StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: Vec::from([
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new("a".loc(23).into()),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(2)).bloc(25)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::from([LocationAnnotation::Semantic(
                            Semantic::User("USER0".to_string()),
                        )]),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new(Declarator::Array(ArrayDeclarator {
                                inner: Box::new("b".loc(37).into()),
                                array_size: Some(
                                    Expression::Literal(Literal::IntUntyped(3)).bloc(39),
                                ),
                                attributes: Vec::new(),
                            })),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(42)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::from([LocationAnnotation::Semantic(
                            Semantic::User("USER1".to_string()),
                        )]),
                        init: None,
                    },
                ]),
                attributes: Vec::new(),
            })]),
        },
    );
}
