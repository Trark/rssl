use super::*;

impl Parse for InputModifier {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], _: &SymbolTable) -> ParseResult<'t, Self> {
        match input {
            [LexToken(Token::In, _), rest @ ..] => Ok((rest, InputModifier::In)),
            [LexToken(Token::Out, _), rest @ ..] => Ok((rest, InputModifier::Out)),
            [LexToken(Token::InOut, _), rest @ ..] => Ok((rest, InputModifier::InOut)),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }
}

impl Parse for ParamType {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, it) = match InputModifier::parse(input, st) {
            Ok((input, it)) => (input, it),
            Err(nom::Err::Incomplete(needed)) => return Err(nom::Err::Incomplete(needed)),
            Err(_) => (input, InputModifier::default()),
        };
        // Todo: interpolation modifiers
        match Type::parse(input, st) {
            Ok((rest, ty)) => Ok((rest, ParamType(ty, it, None))),
            Err(err) => Err(err),
        }
    }
}

fn parse_numthreads(input: &[LexToken]) -> ParseResult<()> {
    match input.first() {
        Some(LexToken(Token::Id(Identifier(name)), _)) => match &name[..] {
            "numthreads" => Ok((&input[1..], ())),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::UnexpectedAttribute(name.clone()),
            ))),
        },
        _ => Err(nom::Err::Error(ParseErrorContext(
            input,
            ParseErrorReason::WrongToken,
        ))),
    }
}

impl Parse for FunctionAttribute {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;

        // Only currently support [numthreads]
        let (input, _) = parse_numthreads(input)?;
        let (input, _) = parse_token(Token::LeftParen)(input)?;
        let (input, x) = contextual(ExpressionNoSeq::parse, st)(input)?;
        let (input, _) = parse_token(Token::Comma)(input)?;
        let (input, y) = contextual(ExpressionNoSeq::parse, st)(input)?;
        let (input, _) = parse_token(Token::Comma)(input)?;
        let (input, z) = contextual(ExpressionNoSeq::parse, st)(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        let attr = FunctionAttribute::NumThreads(x, y, z);

        let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
        Ok((input, attr))
    }
}

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
        _ => Err(nom::Err::Error(ParseErrorContext(
            input,
            ParseErrorReason::WrongToken,
        ))),
    }
}

impl Parse for FunctionParam {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, ty) = contextual(ParamType::parse, st)(input)?;
        let (input, param) = contextual(VariableName::parse, st)(input)?;

        // Parse semantic if present
        let (input, semantic) = match parse_token(Token::Colon)(input) {
            Ok((input, _)) => match parse_semantic(input) {
                Ok((input, semantic)) => (input, Some(semantic)),
                Err(err) => return Err(err),
            },
            Err(_) => (input, None),
        };

        let param = FunctionParam {
            name: param.to_node(),
            param_type: ty,
            semantic,
        };
        Ok((input, param))
    }
}

#[test]
fn test_function_param() {
    use test_support::*;
    let function_param = ParserTester::new(FunctionParam::parse);

    function_param.check(
        "float x",
        FunctionParam {
            name: "x".to_string(),
            param_type: Type::float().into(),
            semantic: None,
        },
    );
    function_param.check(
        "in float x",
        FunctionParam {
            name: "x".to_string(),
            param_type: ParamType(Type::float(), InputModifier::In, None),
            semantic: None,
        },
    );
    function_param.check(
        "out float x",
        FunctionParam {
            name: "x".to_string(),
            param_type: ParamType(Type::float(), InputModifier::Out, None),
            semantic: None,
        },
    );
    function_param.check(
        "inout float x",
        FunctionParam {
            name: "x".to_string(),
            param_type: ParamType(Type::float(), InputModifier::InOut, None),
            semantic: None,
        },
    );
    function_param.check(
        "in uint vertex_id : SV_VertexID",
        FunctionParam {
            name: "vertex_id".to_string(),
            param_type: ParamType(Type::uint(), InputModifier::In, None),
            semantic: Some(Semantic::VertexId),
        },
    );
    function_param.check(
        "in float2 uv : TEXCOORD",
        FunctionParam {
            name: "uv".to_string(),
            param_type: ParamType(Type::floatn(2), InputModifier::In, None),
            semantic: Some(Semantic::User("TEXCOORD".to_string())),
        },
    );
}

impl Parse for FunctionDefinition {
    type Output = Self;
    fn parse<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Self> {
        let (input, attributes) =
            nom::multi::many0(contextual(FunctionAttribute::parse, st))(input)?;
        let (input, ret) = contextual(Type::parse, st)(input)?;
        let (input, func_name) = contextual(VariableName::parse, st)(input)?;
        let (input, _) = parse_token(Token::LeftParen)(input)?;
        let (input, params) = nom::multi::separated_list0(
            parse_token(Token::Comma),
            contextual(FunctionParam::parse, st),
        )(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        let (input, return_semantic) = nom::combinator::opt(|input| {
            let (input, _) = parse_token(Token::Colon)(input)?;
            let (input, semantic) = parse_semantic(input)?;
            Ok((input, semantic))
        })(input)?;
        let (input, body) = statement_block(input, st)?;
        let def = FunctionDefinition {
            name: func_name.to_node(),
            returntype: FunctionReturn {
                return_type: ret,
                semantic: return_semantic,
            },
            params,
            body,
            attributes,
        };
        Ok((input, def))
    }
}
