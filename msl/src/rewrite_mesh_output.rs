use crate::GenerateError;
use rssl_ir as ir;

/// Modify the mesh shader entry point
pub fn process_mesh_entry(
    module: &mut ir::Module,
    pipeline_index: usize,
    stage_index: usize,
) -> Result<MeshOutputLayout, GenerateError> {
    // Find parameter that determine the mesh output format before stripping the function
    let layout = analyse_mesh_layout(
        &module.pipelines[pipeline_index].stages[stage_index],
        module,
    )?;

    // Modify the function in place
    // This will break if another function calls the entry point - but this is silly for a mesh shader entry point

    let entry_point = module.pipelines[pipeline_index].stages[stage_index].entry_point;

    let implementation = module
        .function_registry
        .get_function_implementation(entry_point)
        .as_ref()
        .unwrap()
        .clone();

    let mut removed_slots = Vec::new();
    let mut removed_ids = Vec::new();
    let mut vertices_id = None;
    let mut primitives_id = None;
    let mut indices_id = None;
    for (i, param) in implementation.params.iter().enumerate() {
        match param.interpolation_modifier {
            Some(ir::InterpolationModifier::Vertices) => {
                removed_slots.push(i);
                removed_ids.push(param.id);
                vertices_id = Some(param.id);
            }
            Some(ir::InterpolationModifier::Primitives) => {
                removed_slots.push(i);
                removed_ids.push(param.id);
                primitives_id = Some(param.id);
            }
            Some(ir::InterpolationModifier::Indices) => {
                removed_slots.push(i);
                removed_ids.push(param.id);
                indices_id = Some(param.id);
            }
            _ => {}
        }
    }

    {
        let sig_mut = module
            .function_registry
            .get_function_signature_mut(entry_point);
        sig_mut.param_types = sig_mut
            .param_types
            .iter()
            .cloned()
            .enumerate()
            .filter_map(|(i, p)| {
                if !removed_slots.contains(&i) {
                    Some(p)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        sig_mut.non_default_params -= removed_slots.len();
    }

    {
        let impl_mut = module
            .function_registry
            .get_function_implementation_mut(entry_point)
            .as_mut()
            .unwrap();

        // Remove mesh outputs from the signature
        impl_mut.params = impl_mut
            .params
            .iter()
            .cloned()
            .enumerate()
            .filter_map(|(i, p)| {
                if !removed_slots.contains(&i) {
                    Some(p)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        // Remove mesh outputs from the local variables
        impl_mut
            .scope_block
            .1
            .variables
            .retain(|id| !removed_ids.contains(id));
    }

    // Replace access to mesh outputs within the body
    {
        let impl_ref = module
            .function_registry
            .get_function_implementation(entry_point)
            .as_ref()
            .unwrap();

        let mut new_block = impl_ref.scope_block.clone();
        let context = ProcessContext {
            vertices_id,
            primitives_id,
            indices_id,
        };
        process_scope_block(&mut new_block, &context)?;

        module
            .function_registry
            .get_function_implementation_mut(entry_point)
            .as_mut()
            .unwrap()
            .scope_block = new_block;
    }

    Ok(layout)
}

/// Parameters that describe the layout of a mesh output by a mesh shader
#[derive(Clone)]
pub struct MeshOutputLayout {
    pub vertex_type: ir::TypeId,
    pub primitive_type: ir::TypeId,
    pub max_vertex_count: u64,
    pub max_primitive_count: u64,
    pub topology: ir::OutputTopology,
}

fn analyse_mesh_layout(
    stage: &ir::PipelineStage,
    module: &ir::Module,
) -> Result<MeshOutputLayout, GenerateError> {
    let function = module
        .function_registry
        .get_function_implementation(stage.entry_point)
        .as_ref()
        .unwrap();

    let mut topology = None;

    for attribute in &function.attributes {
        if let ir::FunctionAttribute::OutputTopology(t) = attribute {
            if topology.is_some() {
                return Err(GenerateError::MultipleMeshTopology);
            }
            topology = Some(*t);
        }
    }

    let topology = match topology {
        Some(topology) => topology,
        None => return Err(GenerateError::MissingMeshTopology),
    };

    let mut vertex_type = None;
    let mut primitive_type = None;
    let mut max_vertex_count = None;
    let mut max_primitive_count = None;

    fn extract_type_size(
        param: &ir::FunctionParam,
        module: &ir::Module,
    ) -> Result<(ir::TypeId, u64), GenerateError> {
        let nomod = module
            .type_registry
            .remove_modifier(param.param_type.type_id);
        let array_layer = module.type_registry.get_type_layer(nomod);
        match array_layer {
            ir::TypeLayer::Array(ty, Some(size)) => Ok((ty, size)),
            _ => Err(GenerateError::InvalidMeshOutputs),
        }
    }

    for param in &function.params {
        match param.interpolation_modifier {
            Some(ir::InterpolationModifier::Vertices) => {
                let (ty, size) = extract_type_size(param, module)?;
                vertex_type = Some(ty);
                max_vertex_count = Some(size);
            }
            Some(ir::InterpolationModifier::Primitives) => {
                let (ty, size) = extract_type_size(param, module)?;
                primitive_type = Some(ty);
                max_primitive_count = Some(size);
            }
            Some(ir::InterpolationModifier::Indices) => {
                let (_, size) = extract_type_size(param, module)?;
                max_primitive_count = Some(size);
            }
            _ => {}
        }
    }

    let vertex_type = match vertex_type {
        Some(vertex_type) => vertex_type,
        None => return Err(GenerateError::InvalidMeshOutputs),
    };

    let primitive_type = match primitive_type {
        Some(primitive_type) => primitive_type,
        None => module.type_registry.register_type(ir::TypeLayer::Void),
    };

    let max_vertex_count = match max_vertex_count {
        Some(max_vertex_count) => max_vertex_count,
        None => return Err(GenerateError::InvalidMeshOutputs),
    };

    let max_primitive_count = match max_primitive_count {
        Some(max_primitive_count) => max_primitive_count,
        None => return Err(GenerateError::InvalidMeshOutputs),
    };

    let layout = MeshOutputLayout {
        vertex_type,
        primitive_type,
        max_vertex_count,
        max_primitive_count,
        topology,
    };

    Ok(layout)
}

struct ProcessContext {
    vertices_id: Option<ir::VariableId>,
    primitives_id: Option<ir::VariableId>,
    indices_id: Option<ir::VariableId>,
}

fn process_scope_block(
    scope_block: &mut ir::ScopeBlock,
    context: &ProcessContext,
) -> Result<(), GenerateError> {
    for statement in &mut scope_block.0 {
        process_statement(statement, context)?;
    }
    Ok(())
}

fn process_statement(
    statement: &mut ir::Statement,
    context: &ProcessContext,
) -> Result<(), GenerateError> {
    match &mut statement.kind {
        ir::StatementKind::Expression(expr) => {
            process_expression(expr, context)?;
        }
        ir::StatementKind::Var(vd) => {
            process_init_opt(&mut vd.init, context)?;
        }
        ir::StatementKind::Block(scope_block) => {
            process_scope_block(scope_block, context)?;
        }
        ir::StatementKind::If(expr, scope_block) => {
            process_expression(expr, context)?;
            process_scope_block(scope_block, context)?;
        }
        ir::StatementKind::IfElse(expr, block_true, block_false) => {
            process_expression(expr, context)?;
            process_scope_block(block_true, context)?;
            process_scope_block(block_false, context)?;
        }
        ir::StatementKind::For(init, cond, acc, scope_block) => {
            match init {
                ir::ForInit::Empty => {}
                ir::ForInit::Expression(expr) => {
                    process_expression(expr, context)?;
                }
                ir::ForInit::Definitions(defs) => {
                    for def in defs {
                        process_init_opt(&mut def.init, context)?;
                    }
                }
            }
            if let Some(cond) = cond {
                process_expression(cond, context)?;
            }
            if let Some(acc) = acc {
                process_expression(acc, context)?;
            }
            process_scope_block(scope_block, context)?;
        }
        ir::StatementKind::While(expr, scope_block) => {
            process_expression(expr, context)?;
            process_scope_block(scope_block, context)?;
        }
        ir::StatementKind::DoWhile(scope_block, expr) => {
            process_scope_block(scope_block, context)?;
            process_expression(expr, context)?;
        }
        ir::StatementKind::Switch(expr, scope_block) => {
            process_expression(expr, context)?;
            process_scope_block(scope_block, context)?;
        }
        ir::StatementKind::Break => {}
        ir::StatementKind::Continue => {}
        ir::StatementKind::Discard => {}
        ir::StatementKind::Return(Some(expr)) => {
            process_expression(expr, context)?;
        }
        ir::StatementKind::Return(None) => {}
        ir::StatementKind::CaseLabel(_) => {}
        ir::StatementKind::DefaultLabel => {}
    }

    Ok(())
}

fn process_init_opt(
    init: &mut Option<ir::Initializer>,
    context: &ProcessContext,
) -> Result<(), GenerateError> {
    if let Some(init) = init {
        process_init(init, context)?;
    }
    Ok(())
}

fn process_init(init: &mut ir::Initializer, context: &ProcessContext) -> Result<(), GenerateError> {
    match init {
        ir::Initializer::Expression(expr) => {
            process_expression(expr, context)?;
        }
        ir::Initializer::Aggregate(entries) => {
            for entry in entries {
                process_init(entry, context)?;
            }
        }
    }
    Ok(())
}

fn process_expression(
    expr: &mut ir::Expression,
    context: &ProcessContext,
) -> Result<(), GenerateError> {
    // Attempt to find assignments of mesh output arrays
    if let ir::Expression::IntrinsicOp(ir::IntrinsicOp::Assignment, exprs) = expr
        && let [ir::Expression::ArraySubscript(object, index), value] = &exprs[..]
        && let ir::Expression::Variable(id) = **object
    {
        if Some(id) == context.vertices_id {
            *expr = ir::Expression::IntrinsicOp(
                ir::IntrinsicOp::MeshOutputSetVertex,
                Vec::from([(**index).clone(), value.clone()]),
            )
        } else if Some(id) == context.primitives_id {
            *expr = ir::Expression::IntrinsicOp(
                ir::IntrinsicOp::MeshOutputSetPrimitive,
                Vec::from([(**index).clone(), value.clone()]),
            )
        } else if Some(id) == context.indices_id {
            *expr = ir::Expression::IntrinsicOp(
                ir::IntrinsicOp::MeshOutputSetIndices,
                Vec::from([(**index).clone(), value.clone()]),
            )
        }
    }

    // Search remaining expressions
    match *expr {
        ir::Expression::Literal(_) => {}
        ir::Expression::Variable(id) => {
            if Some(id) == context.vertices_id
                || Some(id) == context.primitives_id
                || Some(id) == context.indices_id
            {
                // If we did not replace the usage above then the variable will be invalid
                return Err(GenerateError::ComplexMeshOutput);
            }
        }
        ir::Expression::MemberVariable(_, _) => {}
        ir::Expression::Global(_) => {}
        ir::Expression::ConstantVariable(_) => {}
        ir::Expression::EnumValue(_) => {}
        ir::Expression::TernaryConditional(ref mut cond, ref mut expr_true, ref mut expr_false) => {
            process_expression(cond, context)?;
            process_expression(expr_true, context)?;
            process_expression(expr_false, context)?;
        }
        ir::Expression::Sequence(ref mut exprs) => {
            for expr in exprs {
                process_expression(expr, context)?;
            }
        }
        ir::Expression::Swizzle(ref mut object, _) => {
            process_expression(object, context)?;
        }
        ir::Expression::MatrixSwizzle(ref mut object, _) => {
            process_expression(object, context)?;
        }
        ir::Expression::ArraySubscript(ref mut object, ref mut index) => {
            process_expression(object, context)?;
            process_expression(index, context)?;
        }
        ir::Expression::StructMember(ref mut object, _, _) => {
            process_expression(object, context)?;
        }
        ir::Expression::ObjectMember(ref mut object, _) => {
            process_expression(object, context)?;
        }
        ir::Expression::Call(_, _, ref mut args) => {
            for arg in args {
                process_expression(arg, context)?;
            }
        }
        ir::Expression::Constructor(_, ref mut slots) => {
            for slot in slots {
                process_expression(&mut slot.expr, context)?;
            }
        }
        ir::Expression::Cast(_, ref mut inner) => {
            process_expression(inner, context)?;
        }
        ir::Expression::SizeOf(_) => {}
        ir::Expression::IntrinsicOp(_, ref mut args) => {
            for arg in args {
                process_expression(arg, context)?;
            }
        }
    }

    Ok(())
}
