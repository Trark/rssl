use super::*;

impl Parse for StructMemberName {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, name) = contextual(VariableName::parse, st)(input)?;
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
}

impl Parse for StructMember {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, typename) = contextual(Type::parse, st)(input)?;
        let (input, defs) = nom::multi::separated_list1(
            parse_token(Token::Comma),
            contextual(StructMemberName::parse, st),
        )(input)?;
        let (input, _) = parse_token(Token::Semicolon)(input)?;
        let sm = StructMember { ty: typename, defs };
        Ok((input, sm))
    }
}

impl Parse for StructDefinition {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, _) = parse_token(Token::Struct)(input)?;
        let (input, structname) = contextual(VariableName::parse, st)(input)?;
        let (input, _) = parse_token(Token::LeftBrace)(input)?;
        let (input, members) = nom::multi::many0(contextual(StructMember::parse, st))(input)?;
        let (input, _) = parse_token(Token::RightBrace)(input)?;
        let (input, _) = parse_token(Token::Semicolon)(input)?;
        let sd = StructDefinition {
            name: structname.to_node(),
            members,
        };
        Ok((input, sd))
    }
}
