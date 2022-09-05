use super::*;

/// Parse the type for a global variable
fn parse_global_type(input: &[LexToken]) -> ParseResult<GlobalType> {
    if input.is_empty() {
        return ParseErrorReason::end_of_stream();
    }
    // Interpolation modifiers unimplemented
    // Non-standard combinations of storage classes unimplemented
    let (input, gs) = match input[0] {
        LexToken(Token::Static, _) => (&input[1..], Some(GlobalStorage::Static)),
        LexToken(Token::GroupShared, _) => (&input[1..], Some(GlobalStorage::GroupShared)),
        LexToken(Token::Extern, _) => (&input[1..], Some(GlobalStorage::Extern)),
        _ => (input, None),
    };
    let (input, ty) = parse_type(input)?;
    let gt = GlobalType(ty, gs.unwrap_or_default(), None);
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
        defs: vec![ConstantVariableName {
            name: "wvp".to_string(),
            bind: VariableBind::Normal,
            offset: None,
        }],
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
