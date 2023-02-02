use super::*;

/// Parse a pipeline definition
pub fn parse_pipeline_definition(input: &[LexToken]) -> ParseResult<PipelineDefinition> {
    let (input, _) = match_named_identifier("Pipeline", input)?;
    let (input, name) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, properties) = parse_multiple(parse_pipeline_property)(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;

    let sd = PipelineDefinition { name, properties };
    Ok((input, sd))
}

/// Parse a struct member variable or method
fn parse_pipeline_property(input: &[LexToken]) -> ParseResult<PipelineProperty> {
    let (input, property) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::Equals)(input)?;
    let (input, value) = parse_expression(input)?;

    // Property ends with a single semicolon - multiple or zero are forbidden
    let (input, _) = parse_token(Token::Semicolon)(input)?;

    let pp = PipelineProperty { property, value };
    Ok((input, pp))
}

#[test]
fn test_pipeline() {
    use test_support::*;
    let def = ParserTester::new(parse_pipeline_definition);

    def.check(
        "Pipeline Test {}",
        PipelineDefinition {
            name: "Test".to_string().loc(9),
            properties: Vec::new(),
        },
    );

    def.check(
        "Pipeline Test { PixelShader = PSMain; }",
        PipelineDefinition {
            name: "Test".to_string().loc(9),
            properties: Vec::from([PipelineProperty {
                property: "PixelShader".to_string().loc(16),
                value: "PSMain".as_var(30),
            }]),
        },
    );
}
