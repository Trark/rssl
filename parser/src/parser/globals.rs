use super::*;

/// Parse the type for a global variable
fn parse_global_type<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, GlobalType> {
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
    let (input, ty) = parse_type(input, st)?;
    let gt = GlobalType(ty, gs.unwrap_or_default(), None);
    Ok((input, gt))
}

/// Parse a single named constant in a constant buffer definition
fn parse_constant_variable_name<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, ConstantVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, array_dim) = parse_optional(|input| parse_arraydim(input, st))(input)?;
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
fn parse_constant_variable<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, ConstantVariable> {
    let (input, typename) = parse_type(input, st)?;
    let (input, defs) = parse_list_nonempty(
        parse_token(Token::Comma),
        contextual(parse_constant_variable_name, st),
    )(input)?;
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

    let cbuffer_register = ParserTester::new(|input, _| parse_constant_slot(input));
    cbuffer_register.check(" : register(b12) ", ConstantSlot(12));
}

/// Parse a constant buffer definition
pub fn parse_constant_buffer<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, ConstantBuffer> {
    let (input, _) = parse_token(Token::ConstantBuffer)(input)?;
    let (input, name) = parse_variable_name(input)?;
    let (input, slot) = parse_optional(parse_constant_slot)(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, members) = parse_multiple(contextual(parse_constant_variable, st))(input)?;
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
fn parse_global_variable_name<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, GlobalVariableName> {
    let (input, name) = parse_variable_name(input)?;
    let (input, array_dim) = parse_optional(|input| parse_arraydim(input, st))(input)?;
    let (input, slot) = parse_optional(parse_global_slot)(input)?;
    let (input, init) = parse_initializer(input, st)?;
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
pub fn parse_global_variable<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, GlobalVariable> {
    let (input, typename) = parse_global_type(input, st)?;
    let (input, defs) = parse_list_nonempty(
        parse_token(Token::Comma),
        contextual(parse_global_variable_name, st),
    )(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let var = GlobalVariable {
        global_type: typename,
        defs,
    };
    Ok((input, var))
}
