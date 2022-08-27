use rssl_ast as ast;
use rssl_ir as ir;
use std::collections::HashMap;

#[test]
fn test_typeparse() {
    use rssl_text::Located;

    let module = ast::Module {
        root_definitions: vec![
            ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
                global_type: ast::Type::from_object(ast::ObjectType::RWBuffer(ast::DataType(
                    ast::DataLayout::Scalar(ast::ScalarType::Int),
                    ast::TypeModifier::default(),
                )))
                .into(),
                defs: vec![ast::GlobalVariableName {
                    name: "g_myOutBuffer".to_string(),
                    bind: ast::VariableBind::Normal,
                    slot: Some(ast::GlobalSlot::ReadSlot(0)),
                    init: None,
                }],
            }),
            ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
                global_type: ast::GlobalType(
                    ast::Type(
                        ast::TypeLayout::from_scalar(ast::ScalarType::Int),
                        ast::TypeModifier {
                            is_const: true,
                            ..ast::TypeModifier::default()
                        },
                    ),
                    ast::GlobalStorage::Static,
                    None,
                ),
                defs: vec![ast::GlobalVariableName {
                    name: "g_myFour".to_string(),
                    bind: ast::VariableBind::Normal,
                    slot: None,
                    init: Some(ast::Initializer::Expression(Located::none(
                        ast::Expression::Literal(ast::Literal::UntypedInt(4)),
                    ))),
                }],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: "myFunc".to_string(),
                returntype: ast::Type::void().into(),
                params: vec![ast::FunctionParam {
                    name: "x".to_string(),
                    param_type: ast::Type::uint().into(),
                    semantic: None,
                }],
                body: vec![],
                attributes: vec![],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: "myFunc".to_string(),
                returntype: ast::Type::void().into(),
                params: vec![ast::FunctionParam {
                    name: "x".to_string(),
                    param_type: ast::Type::float().into(),
                    semantic: None,
                }],
                body: vec![],
                attributes: vec![],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: "outFunc".to_string(),
                returntype: ast::Type::void().into(),
                params: vec![ast::FunctionParam {
                    name: "x".to_string(),
                    param_type: ast::ParamType(ast::Type::float(), ast::InputModifier::Out, None),
                    semantic: None,
                }],
                body: vec![
                    ast::Statement::Var(ast::VarDef::one(
                        "local_static",
                        ast::LocalType(ast::Type::uint(), ast::LocalStorage::Static, None),
                    )),
                    ast::Statement::Expression(Located::none(ast::Expression::BinaryOperation(
                        ast::BinOp::Assignment,
                        Box::new(Located::none(ast::Expression::Variable("x".to_string()))),
                        Box::new(Located::none(ast::Expression::Literal(
                            ast::Literal::Float(1.5f32),
                        ))),
                    ))),
                ],
                attributes: vec![],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: "CSMAIN".to_string(),
                returntype: ast::Type::void().into(),
                params: vec![],
                body: vec![
                    ast::Statement::Empty,
                    ast::Statement::Var(ast::VarDef::one("a", ast::Type::uint().into())),
                    ast::Statement::Var(ast::VarDef::one("b", ast::Type::uint().into())),
                    ast::Statement::Expression(Located::none(ast::Expression::BinaryOperation(
                        ast::BinOp::Assignment,
                        Box::new(Located::none(ast::Expression::Variable("a".to_string()))),
                        Box::new(Located::none(ast::Expression::Variable("b".to_string()))),
                    ))),
                    ast::Statement::If(
                        Located::none(ast::Expression::Variable("b".to_string())),
                        Box::new(ast::Statement::Empty),
                    ),
                    ast::Statement::Expression(Located::none(ast::Expression::BinaryOperation(
                        ast::BinOp::Assignment,
                        Box::new(Located::none(ast::Expression::ArraySubscript(
                            Box::new(Located::none(ast::Expression::Variable(
                                "g_myOutBuffer".to_string(),
                            ))),
                            Box::new(Located::none(ast::Expression::Literal(ast::Literal::Int(
                                0,
                            )))),
                        ))),
                        Box::new(Located::none(ast::Expression::Literal(ast::Literal::Int(
                            4,
                        )))),
                    ))),
                    ast::Statement::Expression(Located::none(ast::Expression::Call(
                        Box::new(Located::none(ast::Expression::Variable(
                            "myFunc".to_string(),
                        ))),
                        vec![],
                        vec![Located::none(ast::Expression::Variable("b".to_string()))],
                    ))),
                    ast::Statement::Var(ast::VarDef::one("testOut", ast::Type::float().into())),
                    ast::Statement::Var(ast::VarDef {
                        local_type: ast::Type::from_layout(ast::TypeLayout::float()).into(),
                        defs: vec![ast::LocalVariableName {
                            name: "x".to_string(),
                            bind: ast::VariableBind::Array(Some(Located::none(
                                ast::Expression::Literal(ast::Literal::UntypedInt(3)),
                            ))),
                            init: None,
                        }],
                    }),
                    ast::Statement::Expression(Located::none(ast::Expression::Call(
                        Box::new(Located::none(ast::Expression::Variable(
                            "outFunc".to_string(),
                        ))),
                        vec![],
                        vec![Located::none(ast::Expression::Variable(
                            "testOut".to_string(),
                        ))],
                    ))),
                ],
                attributes: vec![ast::FunctionAttribute::numthreads(8, 8, 1)],
            }),
        ],
    };
    let res = rssl_typer::type_check(&module);
    assert!(res.is_ok(), "{:?}", res);

    let static_global_test = ast::Module {
        root_definitions: vec![
            ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
                global_type: ast::GlobalType(
                    ast::Type(
                        ast::TypeLayout::from_scalar(ast::ScalarType::Int),
                        ast::TypeModifier {
                            is_const: true,
                            ..ast::TypeModifier::default()
                        },
                    ),
                    ast::GlobalStorage::Static,
                    None,
                ),
                defs: vec![ast::GlobalVariableName {
                    name: "g_myFour".to_string(),
                    bind: ast::VariableBind::Normal,
                    slot: None,
                    init: Some(ast::Initializer::Expression(Located::none(
                        ast::Expression::Literal(ast::Literal::UntypedInt(4)),
                    ))),
                }],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: "CSMAIN".to_string(),
                returntype: ast::Type::void().into(),
                params: vec![],
                body: vec![ast::Statement::Expression(Located::none(
                    ast::Expression::Variable("g_myFour".to_string()),
                ))],
                attributes: vec![ast::FunctionAttribute::numthreads(8, 8, 1)],
            }),
        ],
    };

    let static_global_result = rssl_typer::type_check(&static_global_test);
    let static_global_expected = Ok(ir::Module {
        global_declarations: ir::GlobalDeclarations {
            functions: HashMap::from([(ir::FunctionId(0), "CSMAIN".to_string())]),
            globals: {
                let mut map = HashMap::new();
                map.insert(ir::GlobalId(0), "g_myFour".to_string());
                map
            },
            structs: HashMap::new(),
            constants: HashMap::new(),
        },
        root_definitions: vec![
            ir::RootDefinition::GlobalVariable(ir::GlobalVariable {
                id: ir::GlobalId(0),
                global_type: ir::GlobalType(
                    ir::Type(
                        ir::TypeLayout::from_scalar(ir::ScalarType::Int),
                        ir::TypeModifier {
                            is_const: true,
                            ..ir::TypeModifier::default()
                        },
                    ),
                    ir::GlobalStorage::Static,
                    None,
                ),
                init: Some(ir::Initializer::Expression(ir::Expression::Cast(
                    ir::Type::int(),
                    Box::new(ir::Expression::Literal(ir::Literal::UntypedInt(4))),
                ))),
            }),
            ir::RootDefinition::Function(ir::FunctionDefinition {
                id: ir::FunctionId(0),
                returntype: ir::FunctionReturn {
                    return_type: ir::Type::void().into(),
                },
                params: Vec::new(),
                scope_block: ir::ScopeBlock(
                    vec![ir::Statement::Expression(ir::Expression::Global(
                        ir::GlobalId(0),
                    ))],
                    ir::ScopedDeclarations {
                        variables: HashMap::new(),
                    },
                ),
                attributes: vec![ir::FunctionAttribute::numthreads(8, 8, 1)],
            }),
        ],
    });
    assert_eq!(static_global_result, static_global_expected);
}
