use super::declarations::{parse_declarator, parse_location_annotation};
use super::statements::parse_attribute;
use super::*;

/// Parse a parameter for a function
fn parse_function_param(input: &[LexToken]) -> ParseResult<'_, FunctionParam> {
    let (input, param_type) = parse_type(input)?;
    let (input, declarator) = parse_declarator(input)?;
    let (input, location_annotations) = parse_multiple(parse_location_annotation)(input)?;

    // Parse default value if present
    let (input, default_expr) = match parse_token(Token::Equals)(input) {
        Ok((input, _)) => match parse_expression_no_seq(input) {
            Ok((input, expr)) => (input, Some(expr.node)),
            Err(err) => return Err(err),
        },
        Err(_) => (input, None),
    };

    let param = FunctionParam {
        param_type,
        declarator,
        location_annotations,
        default_expr,
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
            param_type: Type::from("float".loc(0)),
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["x".to_string().loc(6)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    );
    function_param.check(
        "in float x",
        FunctionParam {
            param_type: Type {
                layout: TypeLayout::from("float".loc(3)),
                modifiers: TypeModifierSet::from(&[TypeModifier::In.loc(0)]),
                location: SourceLocation::first(),
            },
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["x".to_string().loc(9)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    );
    function_param.check(
        "out float x",
        FunctionParam {
            param_type: Type {
                layout: TypeLayout::from("float".loc(4)),
                modifiers: TypeModifierSet::from(&[TypeModifier::Out.loc(0)]),
                location: SourceLocation::first(),
            },
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["x".to_string().loc(10)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    );
    function_param.check(
        "inout float x",
        FunctionParam {
            param_type: Type {
                layout: TypeLayout::from("float".loc(6)),
                modifiers: TypeModifierSet::from(&[TypeModifier::InOut.loc(0)]),
                location: SourceLocation::first(),
            },
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["x".to_string().loc(12)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    );
    function_param.check(
        "in uint vertex_id : SV_VertexID",
        FunctionParam {
            param_type: Type {
                layout: TypeLayout::from("uint".loc(3)),
                modifiers: TypeModifierSet::from(&[TypeModifier::In.loc(0)]),
                location: SourceLocation::first(),
            },
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["vertex_id".to_string().loc(8)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::from([LocationAnnotation::Semantic(Semantic::VertexId)]),
            default_expr: None,
        },
    );
    function_param.check(
        "in float2 uv : TEXCOORD",
        FunctionParam {
            param_type: Type {
                layout: TypeLayout::from("float2".loc(3)),
                modifiers: TypeModifierSet::from(&[TypeModifier::In.loc(0)]),
                location: SourceLocation::first(),
            },
            declarator: Declarator::Identifier(
                ScopedIdentifier {
                    base: ScopedIdentifierBase::Relative,
                    identifiers: Vec::from(["uv".to_string().loc(10)]),
                },
                Vec::new(),
            ),
            location_annotations: Vec::from([LocationAnnotation::Semantic(Semantic::User(
                "TEXCOORD".to_string(),
            ))]),
            default_expr: None,
        },
    );
    function_param.check(
        "float v[4]",
        FunctionParam {
            param_type: Type::from("float".loc(0)),
            declarator: Declarator::Array(ArrayDeclarator {
                inner: Box::new(Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["v".to_string().loc(6)]),
                    },
                    Vec::new(),
                )),
                array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(8)),
                attributes: Vec::new(),
            }),
            location_annotations: Vec::new(),
            default_expr: None,
        },
    );
}

/// Parse a function definition
pub fn parse_function_definition(input: &[LexToken]) -> ParseResult<'_, FunctionDefinition> {
    // Not clear on ordering of template args and attributes
    // Attributes are used on entry points which can not be template functions
    let (input, template_params) = parse_template_params(input)?;
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;
    let (input, ret) = parse_type(input)?;
    let (input, func_name) = parse_variable_name(input)?;
    let (input, _) = parse_token(Token::LeftParen)(input)?;
    let (input, params) = parse_list(parse_token(Token::Comma), parse_function_param)(input)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;
    let (input, location_annotations) = parse_multiple(parse_location_annotation)(input)?;

    let (input, body) = match parse_token(Token::Semicolon)(input) {
        Ok((input, _)) => (input, None),
        _ => {
            let (input, body) = statement_block(input)?;
            (input, Some(body))
        }
    };

    let def = FunctionDefinition {
        name: func_name,
        returntype: FunctionReturn {
            return_type: ret,
            location_annotations,
        },
        template_params,
        params,
        is_const: false,
        is_volatile: false,
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
                location_annotations: Vec::new(),
            },
            template_params: TemplateParamList(Vec::from([TemplateParam::Type(
                TemplateTypeParam {
                    name: Some("T".to_string().loc(18)),
                    default: None,
                },
            )])),
            params: Vec::from([FunctionParam {
                param_type: Type::from("T".loc(28)),
                declarator: Declarator::Identifier(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from(["arg".to_string().loc(30)]),
                    },
                    Vec::new(),
                ),
                location_annotations: Vec::new(),
                default_expr: None,
            }]),
            is_const: false,
            is_volatile: false,
            body: Some(Vec::new()),
            attributes: Vec::new(),
        },
    );

    functiondefinition.check(
        "template<uint L = 3> void f() {}",
        FunctionDefinition {
            name: "f".to_string().loc(26),
            returntype: FunctionReturn {
                return_type: Type::from("void".loc(21)),
                location_annotations: Vec::new(),
            },

            template_params: TemplateParamList(Vec::from([TemplateParam::Value(
                TemplateValueParam {
                    value_type: Type::from("uint".loc(9)),
                    name: Some("L".to_string().loc(14)),
                    default: Some(Expression::Literal(Literal::IntUntyped(3)).loc(18)),
                },
            )])),
            params: Vec::new(),
            is_const: false,
            is_volatile: false,
            body: Some(Vec::new()),
            attributes: Vec::new(),
        },
    );

    functiondefinition.check(
        "template<> void f() {}",
        FunctionDefinition {
            name: "f".to_string().loc(16),
            returntype: FunctionReturn {
                return_type: Type::from("void".loc(11)),
                location_annotations: Vec::new(),
            },
            template_params: TemplateParamList(Vec::new()),
            params: Vec::new(),
            is_const: false,
            is_volatile: false,
            body: Some(Vec::new()),
            attributes: Vec::new(),
        },
    );

    functiondefinition.check(
        "template<typename T, typename G> void f() {}",
        FunctionDefinition {
            name: "f".to_string().loc(38),
            returntype: FunctionReturn {
                return_type: Type::from("void".loc(33)),
                location_annotations: Vec::new(),
            },
            template_params: TemplateParamList(Vec::from([
                TemplateParam::Type(TemplateTypeParam {
                    name: Some("T".to_string().loc(18)),
                    default: None,
                }),
                TemplateParam::Type(TemplateTypeParam {
                    name: Some("G".to_string().loc(30)),
                    default: None,
                }),
            ])),
            params: Vec::new(),
            is_const: false,
            is_volatile: false,
            body: Some(Vec::new()),
            attributes: Vec::new(),
        },
    );
}
