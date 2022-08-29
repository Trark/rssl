use super::*;
use enums::parse_enum_definition;
use functions::parse_function_definition;
use globals::{parse_constant_buffer, parse_global_variable};
use structs::parse_struct_definition;

/// Parse a root element in a shader document
fn parse_root_definition<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, RootDefinition> {
    let res = match parse_struct_definition(input, st) {
        Ok((rest, structdef)) => Ok((rest, RootDefinition::Struct(structdef))),
        Err(err) => Err(err),
    };

    let res = res.select(match parse_enum_definition(input, st) {
        Ok((rest, enumdef)) => Ok((rest, RootDefinition::Enum(enumdef))),
        Err(err) => Err(err),
    });

    let res = res.select(match parse_constant_buffer(input, st) {
        Ok((rest, cbuffer)) => Ok((rest, RootDefinition::ConstantBuffer(cbuffer))),
        Err(err) => Err(err),
    });

    let res = res.select(match parse_global_variable(input, st) {
        Ok((rest, globalvariable)) => {
            return Ok((rest, RootDefinition::GlobalVariable(globalvariable)))
        }
        Err(err) => Err(err),
    });

    let res = res.select(match parse_function_definition(input, st) {
        Ok((rest, funcdef)) => return Ok((rest, RootDefinition::Function(funcdef))),
        Err(err) => Err(err),
    });

    res
}

/// Parse a root definition which may have many semicolons after it
pub fn parse_root_definition_with_semicolon<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, RootDefinition> {
    let (input, def) = parse_root_definition(input, st)?;
    let (input, _) = nom::multi::many0(parse_token(Token::Semicolon))(input)?;
    Ok((input, def))
}

#[test]
fn test_struct() {
    use test_support::*;
    let rootdefinition = ParserTester::new(parse_root_definition);
    let structdefinition = ParserTester::new(parse_struct_definition);

    let test_struct_str = "struct MyStruct { uint a; float b; };";
    let test_struct_ast = StructDefinition {
        name: "MyStruct".to_string().loc(7),
        members: vec![
            StructMember {
                ty: Type::uint(),
                defs: vec![StructMemberName {
                    name: "a".to_string(),
                    bind: VariableBind::Normal,
                }],
            },
            StructMember {
                ty: Type::float(),
                defs: vec![StructMemberName {
                    name: "b".to_string(),
                    bind: VariableBind::Normal,
                }],
            },
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
            return_type: Type::float(),
            semantic: Some(Semantic::Depth),
        },
        params: vec![FunctionParam {
            name: "x".to_string().loc(17),
            param_type: Type::float().into(),
            semantic: None,
        }],
        body: vec![],
        attributes: vec![],
    };
    functiondefinition.check(test_func_str, test_func_ast.clone());
    rootdefinition.check(test_func_str, RootDefinition::Function(test_func_ast));
    let numthreads = FunctionAttribute::NumThreads(
        Expression::Literal(Literal::UntypedInt(16)).loc(12),
        Expression::Literal(Literal::UntypedInt(16)).loc(16),
        Expression::Literal(Literal::UntypedInt(1)).loc(20),
    );
    rootdefinition.check(
        "[numthreads(16, 16, 1)] void func(float x) { if (x < 0) { return; } }",
        RootDefinition::Function(FunctionDefinition {
            name: "func".to_string().loc(29),
            returntype: Type::void().into(),
            params: vec![FunctionParam {
                name: "x".to_string().loc(40),
                param_type: Type::float().into(),
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
            attributes: vec![numthreads],
        }),
    );
}

#[test]
fn test_constant_buffer() {
    use test_support::*;
    let rootdefinition = ParserTester::new(parse_root_definition);
    let cbuffer = ParserTester::new(parse_constant_buffer);

    let test_cbuffer1_str = "cbuffer globals { float4x4 wvp; }";
    let test_cbuffer1_ast = ConstantBuffer {
        name: "globals".to_string().loc(8),
        slot: None,
        members: vec![ConstantVariable {
            ty: Type::float4x4(),
            defs: vec![ConstantVariableName {
                name: "wvp".to_string(),
                bind: VariableBind::Normal,
                offset: None,
            }],
        }],
    };
    cbuffer.check(test_cbuffer1_str, test_cbuffer1_ast.clone());
    rootdefinition.check(
        test_cbuffer1_str,
        RootDefinition::ConstantBuffer(test_cbuffer1_ast),
    );

    let test_cbuffer2_str = "cbuffer globals : register(b12) { float4x4 wvp; float x, y[2]; }";
    let test_cbuffer2_ast_wvp = ConstantVariable {
        ty: Type::float4x4(),
        defs: vec![ConstantVariableName {
            name: "wvp".to_string(),
            bind: VariableBind::Normal,
            offset: None,
        }],
    };
    let test_cbuffer2_ast_xy_m1 = ConstantVariableName {
        name: "x".to_string(),
        bind: VariableBind::Normal,
        offset: None,
    };
    let test_cbuffer2_ast_xy_m2 = ConstantVariableName {
        name: "y".to_string(),
        bind: VariableBind::Array(Some(Expression::Literal(Literal::UntypedInt(2)).loc(59))),
        offset: None,
    };
    let test_cbuffer2_ast_xy = ConstantVariable {
        ty: Type::float(),
        defs: vec![test_cbuffer2_ast_xy_m1, test_cbuffer2_ast_xy_m2],
    };
    let test_cbuffer2_ast = ConstantBuffer {
        name: "globals".to_string().loc(8),
        slot: Some(ConstantSlot(12)),
        members: vec![test_cbuffer2_ast_wvp, test_cbuffer2_ast_xy],
    };
    cbuffer.check(test_cbuffer2_str, test_cbuffer2_ast.clone());
    rootdefinition.check(
        test_cbuffer2_str,
        RootDefinition::ConstantBuffer(test_cbuffer2_ast),
    );
}

#[test]
fn test_global_variable() {
    use test_support::*;
    let rootdefinition = ParserTester::new(parse_root_definition);
    let globalvariable = ParserTester::new(parse_global_variable);

    let test_buffersrv_str = "Buffer g_myBuffer : register(t1);";
    let test_buffersrv_ast = GlobalVariable {
        global_type: Type::from_object(ObjectType::Buffer(DataType(
            DataLayout::Vector(ScalarType::Float, 4),
            TypeModifier::default(),
        )))
        .into(),
        defs: vec![GlobalVariableName {
            name: "g_myBuffer".to_string().loc(7),
            bind: VariableBind::Normal,
            slot: Some(GlobalSlot::ReadSlot(1)),
            init: None,
        }],
    };
    globalvariable.check(test_buffersrv_str, test_buffersrv_ast.clone());
    rootdefinition.check(
        test_buffersrv_str,
        RootDefinition::GlobalVariable(test_buffersrv_ast),
    );

    let test_buffersrv2_str = "Buffer<uint4> g_myBuffer : register(t1);";
    let test_buffersrv2_ast = GlobalVariable {
        global_type: Type::from_object(ObjectType::Buffer(DataType(
            DataLayout::Vector(ScalarType::UInt, 4),
            TypeModifier::default(),
        )))
        .into(),
        defs: vec![GlobalVariableName {
            name: "g_myBuffer".to_string().loc(14),
            bind: VariableBind::Normal,
            slot: Some(GlobalSlot::ReadSlot(1)),
            init: None,
        }],
    };
    globalvariable.check(test_buffersrv2_str, test_buffersrv2_ast.clone());
    rootdefinition.check(
        test_buffersrv2_str,
        RootDefinition::GlobalVariable(test_buffersrv2_ast),
    );

    let test_buffersrv3_str = "Buffer<vector<int, 4>> g_myBuffer : register(t1);";
    let test_buffersrv3_ast = GlobalVariable {
        global_type: Type::from_object(ObjectType::Buffer(DataType(
            DataLayout::Vector(ScalarType::Int, 4),
            TypeModifier::default(),
        )))
        .into(),
        defs: vec![GlobalVariableName {
            name: "g_myBuffer".to_string().loc(23),
            bind: VariableBind::Normal,
            slot: Some(GlobalSlot::ReadSlot(1)),
            init: None,
        }],
    };
    globalvariable.check(test_buffersrv3_str, test_buffersrv3_ast.clone());
    rootdefinition.check(
        test_buffersrv3_str,
        RootDefinition::GlobalVariable(test_buffersrv3_ast),
    );

    let test_buffersrv4_str = "StructuredBuffer<CustomType> g_myBuffer : register(t1);";
    let test_buffersrv4_ast = GlobalVariable {
        global_type: Type::from_object(ObjectType::StructuredBuffer(StructuredType(
            StructuredLayout::Custom("CustomType".to_string()),
            TypeModifier::default(),
        )))
        .into(),
        defs: vec![GlobalVariableName {
            name: "g_myBuffer".to_string().loc(29),
            bind: VariableBind::Normal,
            slot: Some(GlobalSlot::ReadSlot(1)),
            init: None,
        }],
    };
    let test_buffersrv4_symbols = SymbolTable({
        let mut map = HashMap::new();
        map.insert("CustomType".to_string(), SymbolType::Struct);
        map
    });
    globalvariable.check_symbolic(
        test_buffersrv4_str,
        &test_buffersrv4_symbols,
        test_buffersrv4_ast.clone(),
    );
    rootdefinition.check_symbolic(
        test_buffersrv4_str,
        &test_buffersrv4_symbols,
        RootDefinition::GlobalVariable(test_buffersrv4_ast),
    );

    let test_static_const_str = "static const int c_numElements = 4;";
    let test_static_const_ast = GlobalVariable {
        global_type: GlobalType(
            Type(
                TypeLayout::int(),
                TypeModifier {
                    is_const: true,
                    ..TypeModifier::default()
                },
            ),
            GlobalStorage::Static,
            None,
        ),
        defs: vec![GlobalVariableName {
            name: "c_numElements".to_string().loc(17),
            bind: VariableBind::Normal,
            slot: None,
            init: Some(Initializer::Expression(
                Expression::Literal(Literal::UntypedInt(4)).loc(33),
            )),
        }],
    };
    globalvariable.check(test_static_const_str, test_static_const_ast.clone());
    rootdefinition.check(
        test_static_const_str,
        RootDefinition::GlobalVariable(test_static_const_ast),
    );

    let test_const_arr_str = "static const int data[4] = { 0, 1, 2, 3 };";
    let test_const_arr_ast_lits = vec![
        Initializer::Expression(Expression::Literal(Literal::UntypedInt(0)).loc(29)),
        Initializer::Expression(Expression::Literal(Literal::UntypedInt(1)).loc(32)),
        Initializer::Expression(Expression::Literal(Literal::UntypedInt(2)).loc(35)),
        Initializer::Expression(Expression::Literal(Literal::UntypedInt(3)).loc(38)),
    ];
    let test_const_arr_ast_gvn = GlobalVariableName {
        name: "data".to_string().loc(17),
        bind: VariableBind::Array(Some(Expression::Literal(Literal::UntypedInt(4)).loc(22))),
        slot: None,
        init: Some(Initializer::Aggregate(test_const_arr_ast_lits)),
    };
    let test_const_arr_ast = GlobalVariable {
        global_type: GlobalType(
            Type(
                TypeLayout::int(),
                TypeModifier {
                    is_const: true,
                    ..TypeModifier::default()
                },
            ),
            GlobalStorage::Static,
            None,
        ),
        defs: vec![test_const_arr_ast_gvn],
    };
    globalvariable.check(test_const_arr_str, test_const_arr_ast.clone());
    rootdefinition.check(
        test_const_arr_str,
        RootDefinition::GlobalVariable(test_const_arr_ast),
    );

    let test_groupshared_str = "groupshared float4 local_data[32];";
    let test_groupshared_ast_gvn = GlobalVariableName {
        name: "local_data".to_string().loc(19),
        bind: VariableBind::Array(Some(Expression::Literal(Literal::UntypedInt(32)).loc(30))),
        slot: None,
        init: None,
    };
    let test_groupshared_ast = GlobalVariable {
        global_type: GlobalType(Type::floatn(4), GlobalStorage::GroupShared, None),
        defs: vec![test_groupshared_ast_gvn],
    };
    globalvariable.check(test_groupshared_str, test_groupshared_ast.clone());
    rootdefinition.check(
        test_groupshared_str,
        RootDefinition::GlobalVariable(test_groupshared_ast),
    );
}
