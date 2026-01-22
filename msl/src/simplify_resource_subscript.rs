use rssl_ir::*;

/// Replace resource[coord] with intrinsics where possible
pub fn simplify_resource_subscript(module: &mut Module) {
    // Ensure object methods are all present in the module
    for id in module.type_registry.iter() {
        let tyl = module.type_registry.get_type_layer(id);

        if let TypeLayer::Object(object_type) = tyl {
            module.register_object(object_type);
        }
    }

    // Make a clone for type queries
    let reference_module = module.clone();

    // Structs do not currently have default values for member variables so have nothing to update
    // Struct methods are handled in the function loop

    // Ignore struct templates as they are already resolved by this point anyway

    for id in module.function_registry.iter() {
        if let Some(function_impl) = module.function_registry.get_function_implementation_mut(id) {
            for param in &mut function_impl.params {
                if let Some(expr) = &mut param.default_expr {
                    process_expression(expr, true, &reference_module);
                }
            }
            process_scope_block(&mut function_impl.scope_block, &reference_module);
        }
    }

    for global_var in &mut module.global_registry {
        process_init_opt(&mut global_var.init, &reference_module);
    }
}

fn process_scope_block(scope_block: &mut ScopeBlock, reference_module: &Module) {
    for statement in &mut scope_block.0 {
        process_statement(statement, reference_module);
    }
}

fn process_statement(statement: &mut Statement, reference_module: &Module) {
    match &mut statement.kind {
        StatementKind::Expression(expr) => process_expression(expr, true, reference_module),
        StatementKind::Var(vd) => process_init_opt(&mut vd.init, reference_module),
        StatementKind::Block(scope_block) => {
            process_scope_block(scope_block, reference_module);
        }
        StatementKind::If(expr, scope_block) => {
            process_expression(expr, true, reference_module);
            process_scope_block(scope_block, reference_module);
        }
        StatementKind::IfElse(expr, block_true, block_false) => {
            process_expression(expr, true, reference_module);
            process_scope_block(block_true, reference_module);
            process_scope_block(block_false, reference_module);
        }
        StatementKind::For(init, cond, acc, scope_block) => {
            match init {
                ForInit::Empty => {}
                ForInit::Expression(expr) => process_expression(expr, true, reference_module),
                ForInit::Definitions(defs) => {
                    for def in defs {
                        process_init_opt(&mut def.init, reference_module)
                    }
                }
            }
            if let Some(cond) = cond {
                process_expression(cond, true, reference_module);
            }
            if let Some(acc) = acc {
                process_expression(acc, true, reference_module);
            }
            process_scope_block(scope_block, reference_module);
        }
        StatementKind::While(expr, scope_block) => {
            process_expression(expr, true, reference_module);
            process_scope_block(scope_block, reference_module);
        }
        StatementKind::DoWhile(scope_block, expr) => {
            process_scope_block(scope_block, reference_module);
            process_expression(expr, true, reference_module);
        }
        StatementKind::Switch(expr, scope_block) => {
            process_expression(expr, true, reference_module);
            process_scope_block(scope_block, reference_module);
        }
        StatementKind::Break => {}
        StatementKind::Continue => {}
        StatementKind::Discard => {}
        StatementKind::Return(Some(expr)) => process_expression(expr, true, reference_module),
        StatementKind::Return(None) => {}
        StatementKind::CaseLabel(_) => {}
        StatementKind::DefaultLabel => {}
    }
}

fn process_expression(expr: &mut Expression, is_read_only: bool, reference_module: &Module) {
    match *expr {
        Expression::Literal(_) => {}
        Expression::Variable(_) => {}
        Expression::MemberVariable(_, _) => {}
        Expression::Global(_) => {}
        Expression::ConstantVariable(_) => {}
        Expression::EnumValue(_) => {}
        Expression::TernaryConditional(ref mut cond, ref mut expr_true, ref mut expr_false) => {
            process_expression(cond, true, reference_module);
            process_expression(expr_true, false, reference_module);
            process_expression(expr_false, false, reference_module);
        }
        Expression::Sequence(ref mut exprs) => {
            for expr in exprs {
                process_expression(expr, false, reference_module);
            }
        }
        Expression::Swizzle(ref mut object, _) => {
            process_expression(object, false, reference_module)
        }
        Expression::MatrixSwizzle(ref mut object, _) => {
            process_expression(object, false, reference_module);
        }
        Expression::ArraySubscript(ref mut object, ref mut index) => {
            process_expression(object, false, reference_module);
            process_expression(index, true, reference_module);

            if is_read_only {
                let ety = match object.get_type(reference_module) {
                    Ok(ety) => ety,
                    Err(_) => panic!("Invalid module"),
                };

                if let Some(lhs_object_type) = get_object_type(ety, reference_module)
                    && let Some(load) = get_remapped_load(lhs_object_type)
                {
                    let object = *object.clone();
                    let index = *index.clone();

                    let index = match lhs_object_type {
                        ObjectType::Texture2D(_)
                        | ObjectType::Texture2DArray(_)
                        | ObjectType::Texture3D(_) => Expression::IntrinsicOp(
                            IntrinsicOp::MakeSignedPushZero,
                            Vec::from([index]),
                        ),

                        ObjectType::RWBuffer(_)
                        | ObjectType::RWTexture2D(_)
                        | ObjectType::RWTexture2DArray(_)
                        | ObjectType::RWTexture3D(_) => {
                            Expression::IntrinsicOp(IntrinsicOp::MakeSigned, Vec::from([index]))
                        }
                        ObjectType::Buffer(_)
                        | ObjectType::StructuredBuffer(_)
                        | ObjectType::RWStructuredBuffer(_) => {
                            // Int should already exist as intrinsics will create it
                            let int_ty = reference_module
                                .type_registry
                                .try_find_type(TypeLayer::Scalar(ScalarType::Int32))
                                .unwrap();

                            Expression::Cast(int_ty, Box::new(index))
                        }
                        _ => panic!("Unexpected object type {:?}", lhs_object_type),
                    };

                    let arguments = Vec::from([object, index]);
                    let function_id =
                        find_function_for_intrinsic(load, &arguments, reference_module);

                    *expr = Expression::Call(function_id, CallType::MethodExternal, arguments);
                }
            }
        }
        Expression::StructMember(ref mut object, _, _) => {
            process_expression(object, false, reference_module);
        }
        Expression::ObjectMember(ref mut object, _) => {
            process_expression(object, false, reference_module);
        }
        Expression::Call(id, ref call_type, ref mut args) => {
            let sig = reference_module
                .function_registry
                .get_function_signature(id);
            let has_object_arg = *call_type == CallType::MethodExternal;
            if has_object_arg {
                assert!(args.len() <= sig.param_types.len() + 1);
                assert!(args.len() > sig.non_default_params);
            } else {
                assert!(args.len() <= sig.param_types.len());
                assert!(args.len() >= sig.non_default_params);
            }
            for (i, arg) in args.iter_mut().enumerate() {
                let is_read_only = if has_object_arg {
                    if i == 0 {
                        true
                    } else {
                        sig.param_types[i - 1].input_modifier == InputModifier::In
                    }
                } else {
                    sig.param_types[i].input_modifier == InputModifier::In
                };
                process_expression(arg, is_read_only, reference_module);
            }
        }
        Expression::Constructor(_, ref mut slots) => {
            for slot in slots {
                process_expression(&mut slot.expr, false, reference_module);
            }
        }
        Expression::Cast(_, ref mut inner) => process_expression(inner, true, reference_module),
        Expression::SizeOf(_) => {}
        Expression::IntrinsicOp(ref op, ref mut args) => {
            let is_binop = args.len() == 2;
            let is_assign = if is_binop {
                match op {
                    IntrinsicOp::PrefixIncrement
                    | IntrinsicOp::PrefixDecrement
                    | IntrinsicOp::PostfixIncrement
                    | IntrinsicOp::PostfixDecrement
                    | IntrinsicOp::Plus
                    | IntrinsicOp::Minus
                    | IntrinsicOp::LogicalNot
                    | IntrinsicOp::BitwiseNot => panic!("Invalid module"),
                    IntrinsicOp::Add
                    | IntrinsicOp::Subtract
                    | IntrinsicOp::Multiply
                    | IntrinsicOp::Divide
                    | IntrinsicOp::Modulus
                    | IntrinsicOp::LeftShift
                    | IntrinsicOp::RightShift
                    | IntrinsicOp::BitwiseAnd
                    | IntrinsicOp::BitwiseOr
                    | IntrinsicOp::BitwiseXor
                    | IntrinsicOp::BooleanAnd
                    | IntrinsicOp::BooleanOr
                    | IntrinsicOp::LessThan
                    | IntrinsicOp::LessEqual
                    | IntrinsicOp::GreaterThan
                    | IntrinsicOp::GreaterEqual
                    | IntrinsicOp::Equality
                    | IntrinsicOp::Inequality => false,
                    IntrinsicOp::Assignment
                    | IntrinsicOp::SumAssignment
                    | IntrinsicOp::DifferenceAssignment
                    | IntrinsicOp::ProductAssignment
                    | IntrinsicOp::QuotientAssignment
                    | IntrinsicOp::RemainderAssignment
                    | IntrinsicOp::LeftShiftAssignment
                    | IntrinsicOp::RightShiftAssignment
                    | IntrinsicOp::BitwiseAndAssignment
                    | IntrinsicOp::BitwiseOrAssignment
                    | IntrinsicOp::BitwiseXorAssignment => true,
                    IntrinsicOp::MakeSigned
                    | IntrinsicOp::MakeSignedPushZero
                    | IntrinsicOp::MeshOutputSetVertex
                    | IntrinsicOp::MeshOutputSetPrimitive
                    | IntrinsicOp::MeshOutputSetIndices => {
                        panic!("We do not expect to have generated any {:?} yet", op)
                    }
                }
            } else {
                false
            };

            for (i, arg) in args.iter_mut().enumerate() {
                let is_read_only = is_binop && (!is_assign || i != 0);
                process_expression(arg, is_read_only, reference_module);
            }

            if is_assign && matches!(op, IntrinsicOp::Assignment) {
                let modification =
                    if let Expression::ArraySubscript(lhs_object, lhs_index) = &args[0] {
                        let lhs_ety = match lhs_object.get_type(reference_module) {
                            Ok(ety) => ety,
                            Err(_) => panic!("Invalid module"),
                        };

                        get_object_type(lhs_ety, reference_module)
                            .and_then(get_remapped_store)
                            .map(|store| (store, lhs_object.clone(), lhs_index.clone()))
                    } else {
                        None
                    };

                if let Some((intrinsic, object_expr, index_expr)) = modification {
                    let arguments = Vec::from([*object_expr, *index_expr, args[1].clone()]);
                    let function_id =
                        find_function_for_intrinsic(intrinsic, &arguments, reference_module);
                    *expr = Expression::Call(function_id, CallType::MethodExternal, arguments);
                }
            }
        }
    }
}

fn process_init_opt(init: &mut Option<Initializer>, reference_module: &Module) {
    if let Some(init) = init {
        process_init(init, reference_module);
    }
}

fn process_init(init: &mut Initializer, reference_module: &Module) {
    match init {
        Initializer::Expression(expr) => process_expression(expr, true, reference_module),
        Initializer::Aggregate(entries) => {
            for entry in entries {
                process_init(entry, reference_module);
            }
        }
    }
}

fn get_object_type(ety: ExpressionType, reference_module: &Module) -> Option<ObjectType> {
    let ty = reference_module.type_registry.remove_modifier(ety.0);
    let tyl = reference_module.type_registry.get_type_layer(ty);
    if let TypeLayer::Object(object_type) = tyl {
        Some(object_type)
    } else {
        None
    }
}

fn get_remapped_load(object_type: ObjectType) -> Option<Intrinsic> {
    Some(match object_type {
        ObjectType::Buffer(_) => Intrinsic::BufferLoad,
        ObjectType::RWBuffer(_) => Intrinsic::RWBufferLoad,
        ObjectType::StructuredBuffer(_) => Intrinsic::StructuredBufferLoad,
        ObjectType::RWStructuredBuffer(_) => Intrinsic::RWStructuredBufferLoad,
        ObjectType::Texture2D(_) => Intrinsic::Texture2DLoad,
        ObjectType::Texture2DMips(_) => return None,
        ObjectType::Texture2DMipsSlice(_) => return None,
        ObjectType::Texture2DArray(_) => Intrinsic::Texture2DArrayLoad,
        ObjectType::Texture2DArrayMips(_) => return None,
        ObjectType::Texture2DArrayMipsSlice(_) => return None,
        ObjectType::RWTexture2D(_) => Intrinsic::RWTexture2DLoad,
        ObjectType::RWTexture2DArray(_) => Intrinsic::RWTexture2DArrayLoad,
        ObjectType::Texture3D(_) => Intrinsic::Texture3DLoad,
        ObjectType::Texture3DMips(_) => return None,
        ObjectType::Texture3DMipsSlice(_) => return None,
        ObjectType::RWTexture3D(_) => Intrinsic::RWTexture3DLoad,
        _ => panic!("Invalid module"),
    })
}

fn get_remapped_store(object_type: ObjectType) -> Option<Intrinsic> {
    Some(match object_type {
        ObjectType::RWBuffer(_) => Intrinsic::RWBufferStore,
        ObjectType::RWStructuredBuffer(_) => Intrinsic::RWStructuredBufferStore,
        ObjectType::RWTexture2D(_) => Intrinsic::RWTexture2DStore,
        ObjectType::RWTexture2DArray(_) => Intrinsic::RWTexture2DArrayStore,
        ObjectType::RWTexture3D(_) => Intrinsic::RWTexture3DStore,
        _ => panic!("Invalid module"),
    })
}

fn find_function_for_intrinsic(
    intrinsic: Intrinsic,
    arguments: &[Expression],
    reference_module: &Module,
) -> FunctionId {
    let (object_argument, normal_argments) = arguments.split_first().unwrap();

    let object_ty = match object_argument.get_type(reference_module) {
        Ok(ety) => reference_module.type_registry.remove_modifier(ety.0),
        Err(_) => panic!("Invalid module"),
    };

    let object_tyl = reference_module.type_registry.get_type_layer(object_ty);
    let object_type = match object_tyl {
        TypeLayer::Object(object_type) => object_type,
        _ => panic!("Invalid module"),
    };

    let object_id = reference_module.try_find_object(object_type).unwrap();

    let object_functions = reference_module
        .type_registry
        .get_object_functions(object_id);

    let argument_types = normal_argments
        .iter()
        .map(|expr| match expr.get_type(reference_module) {
            Ok(ety) => ety.0,
            Err(_) => panic!("Invalid module"),
        })
        .collect::<Vec<_>>();

    let mut found = None;
    for id in object_functions.iter().cloned() {
        if let Some(data) = reference_module.function_registry.get_intrinsic_data(id)
            && *data == intrinsic
        {
            let sig = &reference_module
                .function_registry
                .get_function_signature(id);
            if argument_types.len() >= sig.non_default_params
                && argument_types.len() <= sig.param_types.len()
            {
                let mut same_params = true;
                for (i, argument_type) in argument_types.iter().enumerate() {
                    let param_type = &sig.param_types[i];
                    if *argument_type != param_type.type_id {
                        same_params = false;
                        break;
                    }
                }

                if same_params {
                    assert!(found.is_none());
                    found = Some(id);
                }
            }
        }
    }

    match found {
        Some(id) => id,
        None => panic!("Failed to find function for {:?}", intrinsic),
    }
}
