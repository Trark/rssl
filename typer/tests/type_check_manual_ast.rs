use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;
use std::collections::HashMap;

fn make_id(name: &str) -> Located<ast::Expression> {
    Located::none(ast::Expression::Identifier(ast::ScopedIdentifier::trivial(
        name,
    )))
}

fn as_statement(stmt: ast::StatementKind) -> ast::Statement {
    ast::Statement {
        kind: stmt,
        location: SourceLocation::UNKNOWN,
        attributes: Vec::new(),
    }
}

#[test]
fn test_ast_pass() {
    let module = ast::Module {
        root_definitions: vec![
            ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
                global_type: ast::Type::from_layout(ast::TypeLayout::with_template_types(
                    Located::none("RWBuffer"),
                    &[ast::ExpressionOrType::Type(ast::Type::from("float4"))],
                )),
                defs: vec![ast::GlobalVariableName {
                    name: Located::none("g_myOutBuffer".to_string()),
                    bind: Default::default(),
                    slot: Some(ast::Register {
                        slot_type: ast::RegisterType::U,
                        index: 0,
                    }),
                    init: None,
                }],
            }),
            ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
                global_type: ast::Type::from("int").with_modifiers(&[
                    Located::none(ast::TypeModifier::Static),
                    Located::none(ast::TypeModifier::Const),
                ]),
                defs: vec![ast::GlobalVariableName {
                    name: Located::none("g_myFour".to_string()),
                    bind: Default::default(),
                    slot: None,
                    init: Some(ast::Initializer::Expression(Located::none(
                        ast::Expression::Literal(ast::Literal::UntypedInt(4)),
                    ))),
                }],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: Located::none("myFunc".to_string()),
                returntype: ast::Type::from("void").into(),
                template_params: ast::TemplateParamList(Vec::new()),
                params: vec![ast::FunctionParam {
                    name: Located::none("x".to_string()),
                    param_type: ast::Type::from("uint"),
                    bind: Default::default(),
                    semantic: None,
                }],
                body: vec![],
                attributes: vec![],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: Located::none("myFunc".to_string()),
                returntype: ast::Type::from("void").into(),
                template_params: ast::TemplateParamList(Vec::new()),
                params: vec![ast::FunctionParam {
                    name: Located::none("x".to_string()),
                    param_type: ast::Type::from("float"),
                    bind: Default::default(),
                    semantic: None,
                }],
                body: vec![],
                attributes: vec![],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: Located::none("outFunc".to_string()),
                returntype: ast::Type::from("void").into(),
                template_params: ast::TemplateParamList(Vec::new()),
                params: vec![ast::FunctionParam {
                    name: Located::none("x".to_string()),
                    param_type: ast::Type::from("float")
                        .with_modifiers(&[Located::none(ast::TypeModifier::Out)]),
                    bind: Default::default(),
                    semantic: None,
                }],
                body: vec![
                    as_statement(ast::StatementKind::Var(ast::VarDef::one(
                        Located::none("local_static".to_string()),
                        ast::Type::from("uint")
                            .with_modifiers(&[Located::none(ast::TypeModifier::Static)]),
                    ))),
                    as_statement(ast::StatementKind::Expression(
                        ast::Expression::BinaryOperation(
                            ast::BinOp::Assignment,
                            Box::new(make_id("x")),
                            Box::new(Located::none(ast::Expression::Literal(
                                ast::Literal::Float(1.5f32),
                            ))),
                        ),
                    )),
                ],
                attributes: vec![],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: Located::none("CSMAIN".to_string()),
                returntype: ast::Type::from("void").into(),
                template_params: ast::TemplateParamList(Vec::new()),
                params: vec![],
                body: vec![
                    as_statement(ast::StatementKind::Empty),
                    as_statement(ast::StatementKind::Var(ast::VarDef::one(
                        Located::none("a".to_string()),
                        ast::Type::from("uint"),
                    ))),
                    as_statement(ast::StatementKind::Var(ast::VarDef::one(
                        Located::none("b".to_string()),
                        ast::Type::from("uint"),
                    ))),
                    as_statement(ast::StatementKind::Expression(
                        ast::Expression::BinaryOperation(
                            ast::BinOp::Assignment,
                            Box::new(make_id("a")),
                            Box::new(make_id("b")),
                        ),
                    )),
                    as_statement(ast::StatementKind::If(
                        make_id("b"),
                        Box::new(as_statement(ast::StatementKind::Empty)),
                    )),
                    as_statement(ast::StatementKind::Expression(
                        ast::Expression::BinaryOperation(
                            ast::BinOp::Assignment,
                            Box::new(Located::none(ast::Expression::ArraySubscript(
                                Box::new(make_id("g_myOutBuffer")),
                                Box::new(Located::none(ast::Expression::Literal(
                                    ast::Literal::Int(0),
                                ))),
                            ))),
                            Box::new(Located::none(ast::Expression::Literal(ast::Literal::Int(
                                4,
                            )))),
                        ),
                    )),
                    as_statement(ast::StatementKind::Expression(ast::Expression::Call(
                        Box::new(make_id("myFunc")),
                        vec![],
                        vec![make_id("b")],
                    ))),
                    as_statement(ast::StatementKind::Var(ast::VarDef::one(
                        Located::none("testOut".to_string()),
                        ast::Type::from("float"),
                    ))),
                    as_statement(ast::StatementKind::Var(ast::VarDef {
                        local_type: ast::Type::from_layout(ast::TypeLayout::from("float")),
                        defs: vec![ast::LocalVariableName {
                            name: Located::none("x".to_string()),
                            bind: ast::VariableBind(Vec::from([Some(Located::none(
                                ast::Expression::Literal(ast::Literal::UntypedInt(3)),
                            ))])),
                            init: None,
                        }],
                    })),
                    as_statement(ast::StatementKind::Expression(ast::Expression::Call(
                        Box::new(make_id("outFunc")),
                        vec![],
                        vec![make_id("testOut")],
                    ))),
                ],
                attributes: vec![ast::Attribute::numthreads(8, 8, 1)],
            }),
        ],
    };
    let res = rssl_typer::type_check(&module);
    assert!(res.is_ok(), "{res:?}");
}

#[test]
fn test_ast_to_ir() {
    let static_global_test = ast::Module {
        root_definitions: vec![
            ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
                global_type: ast::Type::from("int").with_modifiers(&[
                    Located::none(ast::TypeModifier::Static),
                    Located::none(ast::TypeModifier::Const),
                ]),
                defs: vec![ast::GlobalVariableName {
                    name: Located::none("g_myFour".to_string()),
                    bind: Default::default(),
                    slot: None,
                    init: Some(ast::Initializer::Expression(Located::none(
                        ast::Expression::Literal(ast::Literal::UntypedInt(4)),
                    ))),
                }],
            }),
            ast::RootDefinition::Function(ast::FunctionDefinition {
                name: Located::none("CSMAIN".to_string()),
                returntype: ast::Type::from("void").into(),
                template_params: ast::TemplateParamList(Vec::new()),
                params: vec![],
                body: vec![
                    as_statement(ast::StatementKind::Expression(make_id("g_myFour").node)),
                    as_statement(ast::StatementKind::Expression(ast::Expression::Call(
                        Box::new(make_id("GroupMemoryBarrierWithGroupSync")),
                        Vec::new(),
                        Vec::new(),
                    ))),
                ],
                attributes: vec![ast::Attribute::numthreads(8, 8, 1)],
            }),
        ],
    };

    match rssl_typer::type_check(&static_global_test) {
        Ok(actual) => {
            let mut base_func_id = ir::FunctionId(u32::MAX);
            for def in &actual.root_definitions {
                if let ir::RootDefinition::Function(id) = def {
                    base_func_id = *id;
                    break;
                }
            }

            assert_eq!(
                actual.root_definitions,
                Vec::from([
                    ir::RootDefinition::GlobalVariable(ir::GlobalId(0)),
                    ir::RootDefinition::Function(base_func_id)
                ])
            );

            let mut reference_module = ir::Module::create();
            let void_id = reference_module
                .type_registry
                .register_type(ir::TypeLayer::Void);
            let int_id = reference_module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Int));
            let const_int_id = reference_module.type_registry.make_const(int_id);
            reference_module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::UntypedInt));

            assert_eq!(actual.type_registry, reference_module.type_registry);
            assert_eq!(actual.struct_registry, Vec::new());
            assert_eq!(actual.struct_template_registry, Vec::new());

            assert_eq!(
                *actual
                    .function_registry
                    .get_function_signature(base_func_id),
                ir::FunctionSignature {
                    return_type: ir::FunctionReturn {
                        return_type: void_id,
                        semantic: None,
                    },
                    template_params: ir::TemplateParamCount(0),
                    param_types: Vec::new(),
                }
            );

            let mut intrinsic_func_id = None;
            for i in 0..actual.function_registry.get_function_count() {
                let id = ir::FunctionId(i);
                if let Some(ir::Intrinsic::GroupMemoryBarrierWithGroupSync) =
                    actual.function_registry.get_intrinsic_data(id)
                {
                    intrinsic_func_id = Some(id);
                }
            }
            let intrinsic_func_id = intrinsic_func_id.unwrap();

            assert_eq!(
                *actual
                    .function_registry
                    .get_function_implementation(base_func_id),
                Some(ir::FunctionImplementation {
                    params: Vec::new(),
                    scope_block: ir::ScopeBlock(
                        Vec::from([
                            ir::Statement {
                                kind: ir::StatementKind::Expression(ir::Expression::Global(
                                    ir::GlobalId(0)
                                )),
                                location: SourceLocation::UNKNOWN,
                                attributes: Vec::new(),
                            },
                            ir::Statement {
                                kind: ir::StatementKind::Expression(ir::Expression::Call(
                                    intrinsic_func_id,
                                    ir::CallType::FreeFunction,
                                    Vec::new(),
                                )),
                                location: SourceLocation::UNKNOWN,
                                attributes: Vec::new(),
                            },
                        ]),
                        ir::ScopedDeclarations {
                            variables: HashMap::new(),
                        },
                    ),
                    attributes: vec![ir::FunctionAttribute::numthreads(8, 8, 1)],
                })
            );

            assert_eq!(
                *actual
                    .function_registry
                    .get_function_name_definition(base_func_id),
                ir::FunctionNameDefinition {
                    name: Located::none("CSMAIN".to_string()),
                    full_name: ir::ScopedName(Vec::from(["CSMAIN".to_string()])),
                }
            );

            assert_eq!(
                actual.global_registry,
                Vec::from([ir::GlobalVariable {
                    id: ir::GlobalId(0),
                    name: Located::none("g_myFour".to_string()),
                    full_name: ir::ScopedName(Vec::from(["g_myFour".to_string()])),
                    type_id: const_int_id,
                    storage_class: ir::GlobalStorage::Static,
                    lang_slot: None,
                    api_slot: None,
                    init: Some(ir::Initializer::Expression(ir::Expression::Literal(
                        ir::Constant::Int(4),
                    ))),
                    constexpr_value: Some(ir::Constant::Int(4)),
                }])
            );

            assert_eq!(actual.cbuffer_registry, Vec::new());
        }
        Err(err) => panic!("Failed to type check: {err:?}"),
    }
}
