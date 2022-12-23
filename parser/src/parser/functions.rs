use super::statements::parse_attribute;
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

/// Parse an interpolation modifier
pub fn parse_interp_modifier(input: &[LexToken]) -> (&[LexToken], Option<InterpolationModifier>) {
    if let [LexToken(Token::Id(id), _), rest @ ..] = input {
        match id.0.as_str() {
            "linear" => (rest, Some(InterpolationModifier::Linear)),
            "centroid" => (rest, Some(InterpolationModifier::Centroid)),
            "nointerpolation" => (rest, Some(InterpolationModifier::NoInterpolation)),
            "noperspective" => (rest, Some(InterpolationModifier::NoPerspective)),
            "sample" => (rest, Some(InterpolationModifier::Sample)),
            "vertices" => (rest, Some(InterpolationModifier::Vertices)),
            "primitives" => (rest, Some(InterpolationModifier::Primitives)),
            "indices" => (rest, Some(InterpolationModifier::Indices)),
            "payload" => (rest, Some(InterpolationModifier::Payload)),
            _ => (input, None),
        }
    } else {
        (input, None)
    }
}

/// Parse the type of a function parameter
fn parse_param_type(input: &[LexToken]) -> ParseResult<ParamType> {
    let (input, it) = match parse_input_modifier(input) {
        Ok((input, it)) => (input, it),
        Err(_) => (input, InputModifier::default()),
    };

    // Try to sample an interpolation modifier
    // HLSL permits these on any types like a modifier and allows duplicates - but we only allow one
    let (input, im) = parse_interp_modifier(input);

    match parse_type(input) {
        Ok((rest, ty)) => Ok((rest, ParamType(ty, it, im))),
        Err(err) => Err(err),
    }
}

/// Parse a semantic
pub fn parse_semantic(input: &[LexToken]) -> ParseResult<Semantic> {
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
fn parse_function_param(input: &[LexToken]) -> ParseResult<FunctionParam> {
    let (input, ty) = parse_param_type(input)?;
    let (input, param) = parse_variable_name(input)?;
    let (input, bind) = parse_multiple(parse_arraydim)(input)?;

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
        bind: VariableBind(bind),
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
            param_type: Type::from("float".loc(0)).into(),
            bind: Default::default(),
            semantic: None,
        },
    );
    function_param.check(
        "in float x",
        FunctionParam {
            name: "x".to_string().loc(9),
            param_type: ParamType(Type::from("float".loc(3)), InputModifier::In, None),
            bind: Default::default(),
            semantic: None,
        },
    );
    function_param.check(
        "out float x",
        FunctionParam {
            name: "x".to_string().loc(10),
            param_type: ParamType(Type::from("float".loc(4)), InputModifier::Out, None),
            bind: Default::default(),
            semantic: None,
        },
    );
    function_param.check(
        "inout float x",
        FunctionParam {
            name: "x".to_string().loc(12),
            param_type: ParamType(Type::from("float".loc(6)), InputModifier::InOut, None),
            bind: Default::default(),
            semantic: None,
        },
    );
    function_param.check(
        "in uint vertex_id : SV_VertexID",
        FunctionParam {
            name: "vertex_id".to_string().loc(8),
            param_type: ParamType(Type::from("uint".loc(3)), InputModifier::In, None),
            bind: Default::default(),
            semantic: Some(Semantic::VertexId),
        },
    );
    function_param.check(
        "in float2 uv : TEXCOORD",
        FunctionParam {
            name: "uv".to_string().loc(10),
            param_type: ParamType(Type::from("float2".loc(3)), InputModifier::In, None),
            bind: Default::default(),
            semantic: Some(Semantic::User("TEXCOORD".to_string())),
        },
    );
    function_param.check(
        "float v[4]",
        FunctionParam {
            name: "v".to_string().loc(6),
            param_type: Type::from("float".loc(0)).into(),
            bind: VariableBind(Vec::from([Some(
                Expression::Literal(Literal::UntypedInt(4)).loc(8),
            )])),
            semantic: None,
        },
    );
}

/// Parse a function definition
pub fn parse_function_definition(input: &[LexToken]) -> ParseResult<FunctionDefinition> {
    // Not clear on ordering of template args and attributes
    // Attributes are used on entry points which can not be template functions
    let (input, template_params) = parse_template_params(input)?;
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
    let (input, ret) = parse_type(input)?;
    let (input, func_name) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::LeftParen)(input)?;
    let (input, params) = parse_list(parse_token(Token::Comma), parse_function_param)(input)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;
    let (input, return_semantic) = parse_optional(|input| {
        let (input, _) = parse_token(Token::Colon)(input)?;
        let (input, semantic) = parse_semantic(input)?;
        Ok((input, semantic))
    })(input)?;
    let (input, body) = statement_block(input)?;
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
                return_type: Type::from("void".loc(21)),
                semantic: None,
            },
            template_params: TemplateParamList(Vec::from(["T".to_string().loc(18)])),
            params: vec![FunctionParam {
                name: "arg".to_string().loc(30),
                param_type: Type::from("T".loc(28)).into(),
                bind: Default::default(),
                semantic: None,
            }],
            body: vec![],
            attributes: vec![],
        },
    );
}
