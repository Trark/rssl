use super::functions::parse_function_definition;
use super::*;

/// Parse a struct member name in an entry
fn parse_struct_member_name<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, StructMemberName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, array_dim) = match parse_arraydim(input, st) {
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
fn parse_struct_member<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, StructMember> {
    let (input, typename) = parse_type(input, st)?;
    let (input, defs) = nom::multi::separated_list1(
        parse_token(Token::Comma),
        contextual(parse_struct_member_name, st),
    )(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sm = StructMember { ty: typename, defs };
    Ok((input, sm))
}

/// Parse a struct member variable or method
fn parse_struct_entry<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, StructEntry> {
    let variable_res =
        parse_struct_member(input, st).map(|(input, def)| (input, StructEntry::Variable(def)));
    let method_res =
        parse_function_definition(input, st).map(|(input, def)| (input, StructEntry::Method(def)));
    let (input, value) = variable_res.select(method_res)?;
    let (input, _) = nom::multi::many0(parse_token(Token::Semicolon))(input)?;
    Ok((input, value))
}

/// Parse a full struct definition
pub fn parse_struct_definition<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, StructDefinition> {
    let (input, _) = parse_token(Token::Struct)(input)?;
    let (input, name) = parse_variable_name(input)?;

    // Add the struct name to the symbol table temporarily
    // Will be added to containing scope later
    // This needs better structure
    let st = &SymbolTable({
        let mut map = st.0.clone();
        map.insert(name.node.clone(), SymbolType::Struct);
        map
    });

    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = nom::multi::many0(contextual(parse_struct_entry, st))(input)?;
    let (input, _) = nom::multi::many0(parse_token(Token::Semicolon))(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sd = StructDefinition { name, members };
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
            members: Vec::new(),
        },
    );

    structdefinition.check(
        "struct MyStruct {;;;};",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
            members: Vec::new(),
        },
    );

    structdefinition.check(
        "struct MyStruct { uint a; };",
        StructDefinition {
            name: "MyStruct".to_string().loc(7),
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
                    template_params: None,
                    params: Vec::new(),
                    body: Vec::new(),
                    attributes: Vec::new(),
                }),
            ],
        },
    );
}
