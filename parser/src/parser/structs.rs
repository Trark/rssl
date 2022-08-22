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

/// Parse a struct member entry - with potentially multiple members per line
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

/// Parse a full struct definition
pub fn parse_struct_definition<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, StructDefinition> {
    let (input, _) = parse_token(Token::Struct)(input)?;
    let (input, structname) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = nom::multi::many0(contextual(parse_struct_member, st))(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sd = StructDefinition {
        name: structname.to_node(),
        members,
    };
    Ok((input, sd))
}
