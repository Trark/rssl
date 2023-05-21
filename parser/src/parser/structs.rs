use super::functions::{parse_function_definition, parse_semantic};
use super::*;

/// Parse a struct member name in an entry
fn parse_struct_member_name(input: &[LexToken]) -> ParseResult<StructMemberName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;

    // Parse semantic if present
    let (input, semantic) = match parse_token(Token::Colon)(input) {
        Ok((input, _)) => match parse_semantic(input) {
            Ok((input, semantic)) => (input, Some(semantic)),
            Err(err) => return Err(err),
        },
        Err(_) => (input, None),
    };

    let member_name = StructMemberName {
        name,
        bind: VariableBind(bind),
        semantic,
    };
    Ok((input, member_name))
}

/// Parse a struct member variable - with potentially multiple members per line
fn parse_struct_member(input: &[LexToken]) -> ParseResult<StructMember> {
    let (input, typename) = parse_type(input)?;
    let (input, defs) =
        parse_list_nonempty(parse_token(Token::Comma), parse_struct_member_name)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sm = StructMember { ty: typename, defs };
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
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = parse_multiple(parse_struct_entry)(input)?;
    let (input, _) = parse_multiple(parse_token(Token::Semicolon))(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sd = StructDefinition {
        name,
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
            template_params: TemplateParamList(Vec::new()),
            members: Vec::new(),
        },
    );

    structdefinition.check(
        "struct MyStruct {;;;};",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            template_params: TemplateParamList(Vec::new()),
            members: Vec::new(),
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: vec![StructMemberName {
                    name: "a".to_string().loc(23),
                    bind: Default::default(),
                    semantic: Default::default(),
                }],
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a, b; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: vec![
                    StructMemberName {
                        name: "a".to_string().loc(23),
                        bind: Default::default(),
                        semantic: Default::default(),
                    },
                    StructMemberName {
                        name: "b".to_string().loc(26),
                        bind: Default::default(),
                        semantic: Default::default(),
                    },
                ],
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a[2], b[3][4]; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: vec![
                    StructMemberName {
                        name: "a".to_string().loc(23),
                        bind: VariableBind(Vec::from([Some(
                            Expression::Literal(Literal::IntUntyped(2)).loc(25),
                        )])),
                        semantic: Default::default(),
                    },
                    StructMemberName {
                        name: "b".to_string().loc(29),
                        bind: VariableBind(Vec::from([
                            Some(Expression::Literal(Literal::IntUntyped(3)).loc(31)),
                            Some(Expression::Literal(Literal::IntUntyped(4)).loc(34)),
                        ])),
                        semantic: Default::default(),
                    },
                ],
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a; void f() {} };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            template_params: TemplateParamList(Vec::new()),
            members: vec![
                StructEntry::Variable(StructMember {
                    ty: Type::from("uint".loc(18)),
                    defs: vec![StructMemberName {
                        name: "a".to_string().loc(23),
                        bind: Default::default(),
                        semantic: Default::default(),
                    }],
                }),
                StructEntry::Method(FunctionDefinition {
                    name: "f".to_string().loc(31),
                    returntype: Type::from("void".loc(26)).into(),
                    template_params: TemplateParamList(Vec::new()),
                    params: Vec::new(),
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
            template_params: TemplateParamList(Vec::from([TemplateParam::Type(
                TemplateTypeParam {
                    name: "T".to_string().loc(18),
                    default: None,
                },
            )])),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("T".loc(39)),
                defs: vec![
                    StructMemberName {
                        name: "a".to_string().loc(41),
                        bind: Default::default(),
                        semantic: Default::default(),
                    },
                    StructMemberName {
                        name: "b".to_string().loc(44),
                        bind: Default::default(),
                        semantic: Default::default(),
                    },
                ],
            })],
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a[2] : USER0, b[3][4] : USER1; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            template_params: TemplateParamList(Vec::new()),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: vec![
                    StructMemberName {
                        name: "a".to_string().loc(23),
                        bind: VariableBind(Vec::from([Some(
                            Expression::Literal(Literal::IntUntyped(2)).loc(25),
                        )])),
                        semantic: Some(Semantic::User("USER0".to_string())),
                    },
                    StructMemberName {
                        name: "b".to_string().loc(37),
                        bind: VariableBind(Vec::from([
                            Some(Expression::Literal(Literal::IntUntyped(3)).loc(39)),
                            Some(Expression::Literal(Literal::IntUntyped(4)).loc(42)),
                        ])),
                        semantic: Some(Semantic::User("USER1".to_string())),
                    },
                ],
            })],
        },
    );
}
