use super::*;

impl Parse for GlobalType {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        if input.is_empty() {
            return Err(nom::Err::Incomplete(nom::Needed::new(1)));
        }
        // Interpolation modifiers unimplemented
        // Non-standard combinations of storage classes unimplemented
        let (input, gs) = match input[0] {
            LexToken(Token::Static, _) => (&input[1..], Some(GlobalStorage::Static)),
            LexToken(Token::GroupShared, _) => (&input[1..], Some(GlobalStorage::GroupShared)),
            LexToken(Token::Extern, _) => (&input[1..], Some(GlobalStorage::Extern)),
            _ => (input, None),
        };
        let (input, ty) = Type::parse(input, st)?;
        let gt = GlobalType(ty, gs.unwrap_or_default(), None);
        Ok((input, gt))
    }
}

impl Parse for ConstantVariableName {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, name) = parse_typed::<VariableName>(st)(input)?;
        let (input, array_dim) = nom::combinator::opt(|input| parse_arraydim(input, st))(input)?;
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
}

impl Parse for ConstantVariable {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, typename) = parse_typed::<Type>(st)(input)?;
        let (input, defs) = nom::multi::separated_list1(
            parse_token(Token::Comma),
            parse_typed::<ConstantVariableName>(st),
        )(input)?;
        let (input, _) = parse_token(Token::Semicolon)(input)?;
        let var = ConstantVariable { ty: typename, defs };
        Ok((input, var))
    }
}

impl Parse for ConstantSlot {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], _: &SymbolTable) -> ParseResult<'t, Self> {
        match input {
            [LexToken(Token::Colon, _), LexToken(Token::Register(reg), _), rest @ ..] => match *reg
            {
                RegisterSlot::B(slot) => Ok((rest, ConstantSlot(slot))),
                _ => Err(nom::Err::Error(ParseErrorContext(
                    input,
                    ParseErrorReason::WrongSlotType,
                ))),
            },
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

impl Parse for ConstantBuffer {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, _) = parse_token(Token::ConstantBuffer)(input)?;
        let (input, name) = parse_typed::<VariableName>(st)(input)?;
        let (input, slot) = nom::combinator::opt(parse_typed::<ConstantSlot>(st))(input)?;
        let (input, members) = nom::sequence::delimited(
            parse_token(Token::LeftBrace),
            nom::multi::many0(parse_typed::<ConstantVariable>(st)),
            parse_token(Token::RightBrace),
        )(input)?;
        let cb = ConstantBuffer {
            name: name.to_node(),
            slot,
            members,
        };
        Ok((input, cb))
    }
}

impl Parse for GlobalSlot {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], _: &SymbolTable) -> ParseResult<'t, Self> {
        match input {
            [LexToken(Token::Colon, _), LexToken(Token::Register(reg), _), rest @ ..] => match *reg
            {
                RegisterSlot::T(slot) => Ok((rest, GlobalSlot::ReadSlot(slot))),
                RegisterSlot::U(slot) => Ok((rest, GlobalSlot::ReadWriteSlot(slot))),
                RegisterSlot::S(slot) => Ok((rest, GlobalSlot::SamplerSlot(slot))),
                RegisterSlot::B(slot) => Ok((rest, GlobalSlot::ConstantSlot(slot))),
            },
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

impl Parse for GlobalVariableName {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, name) = parse_typed::<VariableName>(st)(input)?;
        let (input, array_dim) = nom::combinator::opt(|input| parse_arraydim(input, st))(input)?;
        let (input, slot) = nom::combinator::opt(parse_typed::<GlobalSlot>(st))(input)?;
        let (input, init) = parse_typed::<Initializer>(st)(input)?;
        let v = GlobalVariableName {
            name: name.to_node(),
            bind: match array_dim {
                Some(ref expr) => VariableBind::Array(expr.clone()),
                None => VariableBind::Normal,
            },
            slot,
            init,
        };
        Ok((input, v))
    }
}

impl Parse for GlobalVariable {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, typename) = parse_typed::<GlobalType>(st)(input)?;
        let (input, defs) = nom::multi::separated_list1(
            parse_token(Token::Comma),
            parse_typed::<GlobalVariableName>(st),
        )(input)?;
        let (input, _) = parse_token(Token::Semicolon)(input)?;
        let var = GlobalVariable {
            global_type: typename,
            defs,
        };
        Ok((input, var))
    }
}