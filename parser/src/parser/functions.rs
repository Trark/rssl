use super::*;

/// Parse an input modifier for a function parameter
fn parse_input_modifier(input: &[LexToken]) -> ParseResult<InputModifier> {
    match input {
        [LexToken(Token::In, _), rest @ ..] => Ok((rest, InputModifier::In)),
        [LexToken(Token::Out, _), rest @ ..] => Ok((rest, InputModifier::Out)),
        [LexToken(Token::InOut, _), rest @ ..] => Ok((rest, InputModifier::InOut)),
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Parse the type of a function parameter
fn parse_param_type<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, ParamType> {
    let (input, it) = match parse_input_modifier(input) {
        Ok((input, it)) => (input, it),
        Err(_) => (input, InputModifier::default()),
    };
    // Todo: interpolation modifiers
    match parse_type(input, st) {
        Ok((rest, ty)) => Ok((rest, ParamType(ty, it, None))),
        Err(err) => Err(err),
    }
}

/// Parse the [numthreads] attribute
fn parse_numthreads(input: &[LexToken]) -> ParseResult<()> {
    match input.first() {
        Some(LexToken(Token::Id(Identifier(name)), _)) => match &name[..] {
            "numthreads" => Ok((&input[1..], ())),
            _ => ParseErrorReason::UnexpectedAttribute(name.clone()).into_result(input),
        },
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Parse an attribute for a function
fn parse_function_attribute<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, FunctionAttribute> {
    let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;

    // Only currently support [numthreads]
    let (input, _) = parse_numthreads(input)?;
    let (input, _) = parse_token(Token::LeftParen)(input)?;
    let (input, x) = parse_expression_no_seq(input, st)?;
    let (input, _) = parse_token(Token::Comma)(input)?;
    let (input, y) = parse_expression_no_seq(input, st)?;
    let (input, _) = parse_token(Token::Comma)(input)?;
    let (input, z) = parse_expression_no_seq(input, st)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;
    let attr = FunctionAttribute::NumThreads(x, y, z);

    let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
    Ok((input, attr))
}

/// Parse a semantic
fn parse_semantic(input: &[LexToken]) -> ParseResult<Semantic> {
    match input.first() {
        Some(LexToken(Token::Id(Identifier(name)), _)) => {
            let semantic = match name[..].to_lowercase().as_str() {
                "sv_dispatchthreadid" => Semantic::DispatchThreadId,
                "sv_groupid" => Semantic::GroupId,
                "sv_groupindex" => Semantic::GroupIndex,
                "sv_groupthreadid" => Semantic::GroupThreadId,
                "sv_vertexid" => Semantic::VertexId,
                "sv_instanceid" => Semantic::InstanceId,
                "sv_primitiveid" => Semantic::PrimitiveId,
                "sv_position" => Semantic::Position,
                "sv_target" => Semantic::Target(0),
                "sv_target0" => Semantic::Target(0),
                "sv_target1" => Semantic::Target(1),
                "sv_target2" => Semantic::Target(2),
                "sv_target3" => Semantic::Target(3),
                "sv_target4" => Semantic::Target(4),
                "sv_target5" => Semantic::Target(5),
                "sv_target6" => Semantic::Target(6),
                "sv_target7" => Semantic::Target(7),
                "sv_depth" => Semantic::Depth,
                "sv_depthgreaterequal" => Semantic::DepthGreaterEqual,
                "sv_depthlessequal" => Semantic::DepthLessEqual,
                _ => Semantic::User(name.clone()),
            };
            Ok((&input[1..], semantic))
        }
        _ => ParseErrorReason::wrong_token(input),
    }
}

/// Parse a parameter for a function
fn parse_function_param<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, FunctionParam> {
    let (input, ty) = parse_param_type(input, st)?;
    let (input, param) = parse_variable_name(input)?;

    // Parse semantic if present
    let (input, semantic) = match parse_token(Token::Colon)(input) {
        Ok((input, _)) => match parse_semantic(input) {
            Ok((input, semantic)) => (input, Some(semantic)),
            Err(err) => return Err(err),
        },
        Err(_) => (input, None),
    };

    let param = FunctionParam {
        name: param,
        param_type: ty,
        semantic,
    };
    Ok((input, param))
}

#[test]
fn test_function_param() {
    use test_support::*;
    let function_param = ParserTester::new(parse_function_param);

    function_param.check(
        "float x",
        FunctionParam {
            name: "x".to_string().loc(6),
            param_type: Type::float().into(),
            semantic: None,
        },
    );
    function_param.check(
        "in float x",
        FunctionParam {
            name: "x".to_string().loc(9),
            param_type: ParamType(Type::float(), InputModifier::In, None),
            semantic: None,
        },
    );
    function_param.check(
        "out float x",
        FunctionParam {
            name: "x".to_string().loc(10),
            param_type: ParamType(Type::float(), InputModifier::Out, None),
            semantic: None,
        },
    );
    function_param.check(
        "inout float x",
        FunctionParam {
            name: "x".to_string().loc(12),
            param_type: ParamType(Type::float(), InputModifier::InOut, None),
            semantic: None,
        },
    );
    function_param.check(
        "in uint vertex_id : SV_VertexID",
        FunctionParam {
            name: "vertex_id".to_string().loc(8),
            param_type: ParamType(Type::uint(), InputModifier::In, None),
            semantic: Some(Semantic::VertexId),
        },
    );
    function_param.check(
        "in float2 uv : TEXCOORD",
        FunctionParam {
            name: "uv".to_string().loc(10),
            param_type: ParamType(Type::floatn(2), InputModifier::In, None),
            semantic: Some(Semantic::User("TEXCOORD".to_string())),
        },
    );
}

/// Parse a function definition
pub fn parse_function_definition<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, FunctionDefinition> {
    // Not clear on ordering of template args and attributes
    // Attributes are used on entry points which can not be template functions
    let (input, template_params) = parse_template_params(input)?;

    // If we have template arguments then add those as types into the symbol table
    // The scope management will need reworking later to handle more complex cases
    let local_symbols = template_params.as_ref().map(|args| {
        SymbolTable({
            let mut map = st.0.clone();
            for arg in &args.0 {
                map.insert(arg.node.clone(), SymbolType::TemplateType);
            }
            map
        })
    });
    let st = match local_symbols {
        Some(ref st) => st,
        None => st,
    };

    let (input, attributes) = parse_multiple(contextual(parse_function_attribute, st))(input)?;
    let (input, ret) = parse_type(input, st)?;
    let (input, func_name) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::LeftParen)(input)?;
    let (input, params) = parse_list(
        parse_token(Token::Comma),
        contextual(parse_function_param, st),
    )(input)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;
    let (input, return_semantic) = parse_optional(|input| {
        let (input, _) = parse_token(Token::Colon)(input)?;
        let (input, semantic) = parse_semantic(input)?;
        Ok((input, semantic))
    })(input)?;
    let (input, body) = statement_block(input, st)?;
    let def = FunctionDefinition {
        name: func_name,
        returntype: FunctionReturn {
            return_type: ret,
            semantic: return_semantic,
        },
        template_params,
        params,
        body,
        attributes,
    };
    Ok((input, def))
}

#[test]
fn test_template_function() {
    use test_support::*;
    let functiondefinition = ParserTester::new(parse_function_definition);

    functiondefinition.check(
        "template<typename T> void f(T arg) {}",
        FunctionDefinition {
            name: "f".to_string().loc(26),
            returntype: FunctionReturn {
                return_type: Type::void(),
                semantic: None,
            },
            template_params: Some(TemplateParamList(Vec::from(["T".to_string().loc(18)]))),
            params: vec![FunctionParam {
                name: "arg".to_string().loc(30),
                param_type: Type::custom("T").into(),
                semantic: None,
            }],
            body: vec![],
            attributes: vec![],
        },
    );
}
