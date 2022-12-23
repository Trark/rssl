use super::*;
use enums::parse_enum_definition;
use functions::parse_function_definition;
use globals::{parse_constant_buffer, parse_global_variable};
use structs::parse_struct_definition;
use types::parse_typedef;

/// Parse a root element in a shader document
fn parse_root_definition(input: &[LexToken]) -> ParseResult<RootDefinition> {
    let res = match parse_struct_definition(input) {
        Ok((rest, structdef)) => Ok((rest, RootDefinition::Struct(structdef))),
        Err(err) => Err(err),
    };

    let res = res.select(match parse_enum_definition(input) {
        Ok((rest, enumdef)) => Ok((rest, RootDefinition::Enum(enumdef))),
        Err(err) => Err(err),
    });

    let res = res.select(match parse_typedef(input) {
        Ok((rest, typedef)) => Ok((rest, RootDefinition::Typedef(typedef))),
        Err(err) => Err(err),
    });

    let res = res.select(match parse_constant_buffer(input) {
        Ok((rest, cbuffer)) => Ok((rest, RootDefinition::ConstantBuffer(cbuffer))),
        Err(err) => Err(err),
    });

    let res = res.select(match parse_global_variable(input) {
        Ok((rest, globalvariable)) => {
            return Ok((rest, RootDefinition::GlobalVariable(globalvariable)))
        }
        Err(err) => Err(err),
    });

    let res = res.select(match parse_function_definition(input) {
        Ok((rest, funcdef)) => return Ok((rest, RootDefinition::Function(funcdef))),
        Err(err) => Err(err),
    });

    let res = res.select(match parse_namespace(input) {
        Ok((rest, def)) => return Ok((rest, def)),
        Err(err) => Err(err),
    });

    res
}

/// Parse a root definition which may have many semicolons after it
pub fn parse_root_definition_with_semicolon(input: &[LexToken]) -> ParseResult<RootDefinition> {
    let (input, def) = parse_root_definition(input)?;
    let (input, _) = parse_multiple(parse_token(Token::Semicolon))(input)?;
    Ok((input, def))
}

/// Parse a namespace
fn parse_namespace(input: &[LexToken]) -> ParseResult<RootDefinition> {
    let (input, _) = parse_token(Token::Namespace)(input)?;
    let (input, name) = parse_optional(match_identifier)(input)?;
    let (input, _) = parse_token(Token::LeftBrace)(input)?;
    let (input, defs) = parse_namespace_contents(input)?;
    let (input, _) = parse_token(Token::RightBrace)(input)?;
    let name = match name {
        Some(tok) => tok.0.clone(),
        None => String::new(),
    };
    let ns = RootDefinition::Namespace(name, defs);
    Ok((input, ns))
}

/// Parse the insides of a namespace
fn parse_namespace_contents(input: &[LexToken]) -> ParseResult<Vec<RootDefinition>> {
    parse_multiple(parse_root_definition_with_semicolon)(input)
}

#[test]
fn test_struct() {
    use test_support::*;
    let rootdefinition = ParserTester::new(parse_root_definition);
    let structdefinition = ParserTester::new(parse_struct_definition);

    let test_struct_str = "struct MyStruct { uint a; float b; };";
    let test_struct_ast = StructDefinition {
        name: "MyStruct".to_string().loc(7),
        template_params: TemplateParamList(Vec::new()),
        members: vec![
            StructEntry::Variable(StructMember {
                ty: Type::from("uint".loc(18)),
                defs: vec![StructMemberName {
                    name: "a".to_string(),
                    bind: Default::default(),
                    semantic: Default::default(),
                }],
            }),
            StructEntry::Variable(StructMember {
                ty: Type::from("float".loc(26)),
                defs: vec![StructMemberName {
                    name: "b".to_string(),
                    bind: Default::default(),
                    semantic: Default::default(),
                }],
            }),
        ],
    };
    structdefinition.check(test_struct_str, test_struct_ast.clone());
    rootdefinition.check(test_struct_str, RootDefinition::Struct(test_struct_ast));
}

#[test]
fn test_function() {
    use test_support::*;
    let rootdefinition = ParserTester::new(parse_root_definition);
    let functiondefinition = ParserTester::new(parse_function_definition);

    let test_func_str = "float func(float x) : SV_Depth { }";
    let test_func_ast = FunctionDefinition {
        name: "func".to_string().loc(6),
        returntype: FunctionReturn {
            return_type: Type::from("float".loc(0)),
            semantic: Some(Semantic::Depth),
        },
        template_params: TemplateParamList(Vec::new()),
        params: vec![FunctionParam {
            name: "x".to_string().loc(17),
            param_type: Type::from("float".loc(11)).into(),
            bind: Default::default(),
            semantic: None,
        }],
        body: Vec::new(),
        attributes: Vec::new(),
    };
    functiondefinition.check(test_func_str, test_func_ast.clone());
    rootdefinition.check(test_func_str, RootDefinition::Function(test_func_ast));

    rootdefinition.check(
        "[numthreads(16, 16, 1)] void func(float x) { if (x < 0) { return; } }",
        RootDefinition::Function(FunctionDefinition {
            name: "func".to_string().loc(29),
            returntype: Type::from("void".loc(24)).into(),
            template_params: TemplateParamList(Vec::new()),
            params: vec![FunctionParam {
                name: "x".to_string().loc(40),
                param_type: Type::from("float".loc(34)).into(),
                bind: Default::default(),
                semantic: None,
            }],
            body: vec![Statement::If(
                Expression::BinaryOperation(
                    BinOp::LessThan,
                    "x".as_bvar(49),
                    Expression::Literal(Literal::UntypedInt(0)).bloc(53),
                )
                .loc(49),
                Box::new(Statement::Block(vec![Statement::Return(None)])),
            )],
            attributes: vec![Attribute {
                name: "numthreads".to_string().loc(1),
                arguments: Vec::from([
                    Expression::Literal(Literal::UntypedInt(16)).loc(12),
                    Expression::Literal(Literal::UntypedInt(16)).loc(16),
                    Expression::Literal(Literal::UntypedInt(1)).loc(20),
                ]),
            }],
        }),
    );
}

#[test]
fn test_namespace() {
    use test_support::*;
    let namespace = ParserTester::new(parse_namespace);

    namespace.check(
        "namespace A {}",
        RootDefinition::Namespace("A".to_string(), Vec::new()),
    );

    namespace.check(
        "namespace {}",
        RootDefinition::Namespace(String::new(), Vec::new()),
    );

    namespace.check(
        "namespace A { struct S {}; }",
        RootDefinition::Namespace(
            "A".to_string(),
            Vec::from([RootDefinition::Struct(StructDefinition {
                name: "S".to_string().loc(21),
                template_params: TemplateParamList(Vec::new()),
                members: Vec::new(),
            })]),
        ),
    );

    namespace.check(
        "namespace A { namespace B { struct S {}; } namespace C { void f() {} } }",
        RootDefinition::Namespace(
            "A".to_string(),
            Vec::from([
                RootDefinition::Namespace(
                    "B".to_string(),
                    Vec::from([RootDefinition::Struct(StructDefinition {
                        name: "S".to_string().loc(35),
                        template_params: TemplateParamList(Vec::new()),
                        members: Vec::new(),
                    })]),
                ),
                RootDefinition::Namespace(
                    "C".to_string(),
                    Vec::from([RootDefinition::Function(FunctionDefinition {
                        name: "f".to_string().loc(62),
                        returntype: FunctionReturn {
                            return_type: Type::from("void".loc(57)),
                            semantic: None,
                        },
                        template_params: TemplateParamList(Vec::new()),
                        params: Vec::new(),
                        body: Vec::new(),
                        attributes: Vec::new(),
                    })]),
                ),
            ]),
        ),
    );
}
