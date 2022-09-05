use super::*;

/// Parse a declaration of a value inside an enum
fn parse_enum_value(input: &[LexToken]) -> ParseResult<EnumValue> {
    let (input, name) = parse_variable_name(input)?;
    let (input, value) = match parse_token(Token::Equals)(input) {
        Ok((input, _)) => {
            let (input, expr) = parse_expression_no_seq(input)?;
            (input, Some(expr))
        }
        Err(_) => (input, None),
    };
    let sd = EnumValue {
        name: name.to_node(),
        value,
    };
    Ok((input, sd))
}

/// Parse an enum definition
pub fn parse_enum_definition(input: &[LexToken]) -> ParseResult<EnumDefinition> {
    let (input, _) = parse_token(Token::Enum)(input)?;
    let (input, name) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, values) = parse_list(parse_token(Token::Comma), parse_enum_value)(input)?;
    // Read optional trailing comma on last element
    let input = if !values.is_empty() {
        match parse_token(Token::Comma)(input) {
            Ok((input, _)) => input,
            Err(_) => input,
        }
    } else {
        input
    };
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let (input, _) = parse_token(Token::Semicolon)(input)?;
    let sd = EnumDefinition { name, values };
    Ok((input, sd))
}

#[test]
fn test_enum_definition() {
    use test_support::*;
    let enum_definition = ParserTester::new(parse_enum_definition);

    enum_definition.check(
        "enum TestEnum {};",
        EnumDefinition {
            name: "TestEnum".to_string().loc(5),
            values: Vec::new(),
        },
    );

    enum_definition.check(
        "enum TestEnum { X, Y, Z, };",
        EnumDefinition {
            name: "TestEnum".to_string().loc(5),
            values: vec![
                EnumValue {
                    name: "X".to_string(),
                    value: None,
                },
                EnumValue {
                    name: "Y".to_string(),
                    value: None,
                },
                EnumValue {
                    name: "Z".to_string(),
                    value: None,
                },
            ],
        },
    );

    enum_definition.check(
        "enum TestEnum { X, Y, Z };",
        EnumDefinition {
            name: "TestEnum".to_string().loc(5),
            values: vec![
                EnumValue {
                    name: "X".to_string(),
                    value: None,
                },
                EnumValue {
                    name: "Y".to_string(),
                    value: None,
                },
                EnumValue {
                    name: "Z".to_string(),
                    value: None,
                },
            ],
        },
    );

    enum_definition.check(
        "enum TestEnum { X = 0, Y, Z = 3, };",
        EnumDefinition {
            name: "TestEnum".to_string().loc(5),
            values: vec![
                EnumValue {
                    name: "X".to_string(),
                    value: Some(Expression::Literal(Literal::UntypedInt(0)).loc(20)),
                },
                EnumValue {
                    name: "Y".to_string(),
                    value: None,
                },
                EnumValue {
                    name: "Z".to_string(),
                    value: Some(Expression::Literal(Literal::UntypedInt(3)).loc(30)),
                },
            ],
        },
    );
}
