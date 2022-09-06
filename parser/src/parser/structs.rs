use super::functions::parse_function_definition;
use super::*;

/// Parse a struct member name in an entry
fn parse_struct_member_name(input: &[LexToken]) -> ParseResult<StructMemberName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, array_dim) = match parse_arraydim(input) {
        Ok((input, array_dim)) => (input, Some(array_dim)),
        Err(_) => (input, None),
    };
    let member_name = StructMemberName {
        name: name.to_node(),
        bind: match array_dim {
            Some(ref expr) => VariableBind::Array(expr.clone()),
            None => VariableBind::Normal,
        },
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
                ty: Type::uint(),
                defs: vec![StructMemberName {
                    name: "a".to_string(),
                    bind: VariableBind::Normal,
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
                ty: Type::uint(),
                defs: vec![
                    StructMemberName {
                        name: "a".to_string(),
                        bind: VariableBind::Normal,
                    },
                    StructMemberName {
                        name: "b".to_string(),
                        bind: VariableBind::Normal,
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
                    ty: Type::uint(),
                    defs: vec![StructMemberName {
                        name: "a".to_string(),
                        bind: VariableBind::Normal,
                    }],
                }),
                StructEntry::Method(FunctionDefinition {
                    name: "f".to_string().loc(31),
                    returntype: Type::void().into(),
                    template_params: TemplateParamList(Vec::new()),
                    params: Vec::new(),
                    body: Vec::new(),
                    attributes: Vec::new(),
                }),
            ],
        },
    );

    structdefinition.check(
        "template<typename T> struct MyStruct { T a, b; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(28),
            template_params: TemplateParamList(Vec::from(["T".to_string().loc(18)])),
            members: vec![StructEntry::Variable(StructMember {
                ty: Type::custom("T"),
                defs: vec![
                    StructMemberName {
                        name: "a".to_string(),
                        bind: VariableBind::Normal,
                    },
                    StructMemberName {
                        name: "b".to_string(),
                        bind: VariableBind::Normal,
                    },
                ],
            })],
        },
    );
}
