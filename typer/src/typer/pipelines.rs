use super::errors::*;
use super::scopes::*;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;
use rssl_text::SourceLocation;

/// Process an AST pipeline definition
pub fn parse_pipeline(def: &ast::PipelineDefinition, context: &mut Context) -> TyperResult<()> {
    let mut pipeline = ir::PipelineDefinition {
        name: def.name.clone(),
        default_bind_group_index: 0,
        stages: Vec::new(),
        graphics_pipeline_state: None,
    };

    // Check for duplicate properties
    for i in 1..def.properties.len() {
        let new_property = &def.properties[i];
        let before_properties = &def.properties[..i];
        for before_prop in before_properties {
            if new_property.property.as_str() == before_prop.property.as_str() {
                return Err(TyperError::PipelinePropertyDuplicate(
                    new_property.property.location,
                ));
            }
        }
    }

    // Process entry points
    let mut remaining_properties = Vec::new();
    for property in &def.properties {
        match property.property.as_str() {
            "VertexShader" => add_stage(
                &property.value,
                ir::ShaderStage::Vertex,
                context,
                &mut pipeline,
            )?,
            "PixelShader" => add_stage(
                &property.value,
                ir::ShaderStage::Pixel,
                context,
                &mut pipeline,
            )?,
            "ComputeShader" => add_stage(
                &property.value,
                ir::ShaderStage::Compute,
                context,
                &mut pipeline,
            )?,
            "TaskShader" => add_stage(
                &property.value,
                ir::ShaderStage::Task,
                context,
                &mut pipeline,
            )?,
            "MeshShader" => add_stage(
                &property.value,
                ir::ShaderStage::Mesh,
                context,
                &mut pipeline,
            )?,
            _ => remaining_properties.push(property),
        }
    }

    // Ensure there is at least one stage
    if pipeline.stages.is_empty() {
        return Err(TyperError::PipelineNoEntryPoint(pipeline.name.location));
    }

    // Pick pipeline type
    let is_compute = pipeline.stages[0].stage == ir::ShaderStage::Compute;

    // Validate stage combinations
    // TODO: Mesh vs Vertex pipeline
    if is_compute {
        if pipeline.stages.len() != 1 {
            return Err(TyperError::PipelineInvalidStageCombination(
                pipeline.name.location,
            ));
        }
    } else {
        for stage in &pipeline.stages {
            if stage.stage == ir::ShaderStage::Compute {
                return Err(TyperError::PipelineInvalidStageCombination(
                    pipeline.name.location,
                ));
            }
        }
    }

    // Init all state values to unset or default state
    let mut gpo = ir::GraphicsPipelineState {
        render_target_formats: Vec::new(),
        depth_target_format: None,
        cull_mode: Default::default(),
        winding_order: Default::default(),
        blend_state: Default::default(),
    };

    let mut cull_mode_set = false;
    let mut winding_order_set = false;
    let mut shared_blend_state = ir::BlendAttachmentState::default();
    let mut blend_state_set = [false; 8];

    // Process remaining properties for states
    for property in &remaining_properties {
        match property.property.as_str() {
            "RenderTargetFormat0"
            | "RenderTargetFormat1"
            | "RenderTargetFormat2"
            | "RenderTargetFormat3"
            | "RenderTargetFormat4"
            | "RenderTargetFormat5"
            | "RenderTargetFormat6"
            | "RenderTargetFormat7" => {
                if is_compute {
                    return Err(TyperError::PipelinePropertyRequiresGraphicsPipeline(
                        property.property.location,
                    ));
                }
                let index = (property.property.as_str().as_bytes()[18] - b'0') as usize;
                if gpo.render_target_formats.len() < index + 1 {
                    gpo.render_target_formats.resize(index + 1, None);
                }
                assert!(gpo.render_target_formats[index].is_none());
                gpo.render_target_formats[index] =
                    Some(extract_string(&property.value)?.to_string());
            }
            "DepthTargetFormat" => {
                if is_compute {
                    return Err(TyperError::PipelinePropertyRequiresGraphicsPipeline(
                        property.property.location,
                    ));
                }
                assert!(gpo.depth_target_format.is_none());
                gpo.depth_target_format = Some(extract_string(&property.value)?.to_string());
            }
            "DefaultBindGroup" => {
                let value = extract_uint32(&property.value, context)?;
                pipeline.default_bind_group_index = value;
            }
            "CullMode" => {
                if is_compute {
                    return Err(TyperError::PipelinePropertyRequiresGraphicsPipeline(
                        property.property.location,
                    ));
                }
                assert!(!cull_mode_set);
                cull_mode_set = true;
                gpo.cull_mode = extract_cull_mode(property)?;
            }
            "WindingOrder" => {
                if is_compute {
                    return Err(TyperError::PipelinePropertyRequiresGraphicsPipeline(
                        property.property.location,
                    ));
                }
                assert!(!winding_order_set);
                winding_order_set = true;
                gpo.winding_order = extract_winding_order(property)?;
            }
            "BlendState" => {
                shared_blend_state = parse_blend_state(&property.value, context)?;
            }
            "BlendState0" | "BlendState1" | "BlendState2" | "BlendState3" | "BlendState4"
            | "BlendState5" | "BlendState6" | "BlendState7" => {
                let index = (property.property.as_str().as_bytes()[10] - b'0') as usize;
                gpo.blend_state.attachments[index] = parse_blend_state(&property.value, context)?;
                blend_state_set[index] = true;
            }
            _ => {
                return Err(TyperError::PipelinePropertyUnknown(
                    property.property.location,
                ))
            }
        }
    }

    for (i, set) in blend_state_set.iter().enumerate() {
        if !set {
            gpo.blend_state.attachments[i] = shared_blend_state;
        }
    }

    if !is_compute {
        // Move the states onto the output pipeline
        pipeline.graphics_pipeline_state = Some(gpo);
    } else {
        // All states are invalid on a compute pipeline - we expect to have failed earlier
        assert!(gpo.render_target_formats.is_empty());
        assert!(gpo.depth_target_format.is_none());
    }

    context.module.pipelines.push(pipeline);

    Ok(())
}

/// Find an entry point for a shader stage
fn add_stage(
    entry_name: &Located<ast::PipelinePropertyValue>,
    stage: ir::ShaderStage,
    context: &mut Context,
    def: &mut ir::PipelineDefinition,
) -> TyperResult<()> {
    let location = entry_name.location;
    let entry_name = match &entry_name.node {
        ast::PipelinePropertyValue::Single(ast::Expression::Identifier(id)) => {
            match id.try_trivial() {
                Some(name) => name.as_str(),
                None => return Err(TyperError::PipelineEntryPointFunctionUnknown(location)),
            }
        }
        _ => return Err(TyperError::PipelineEntryPointFunctionUnknown(location)),
    };

    let mut func_id = None;
    for id in context.module.function_registry.iter() {
        let name = context.module.function_registry.get_function_name(id);
        if name == entry_name {
            if func_id.is_some() {
                return Err(TyperError::PipelineEntryPointFunctionUnknown(location));
            }
            func_id = Some(id);
        }
    }

    let mut thread_group_size = None;

    let func_id = match func_id {
        Some(id) => id,
        None => return Err(TyperError::PipelineEntryPointFunctionUnknown(location)),
    };

    let function_impl = context
        .module
        .function_registry
        .get_function_implementation(func_id)
        .as_ref()
        .unwrap();

    for attribute in &function_impl.attributes.clone() {
        if let ir::FunctionAttribute::NumThreads(x, y, z) = attribute {
            let mut evaluate = |expr: &ir::Expression| {
                let value = match crate::evaluator::evaluate_constexpr(expr, &mut context.module) {
                    Ok(value) => value,
                    _ => {
                        return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                            SourceLocation::UNKNOWN,
                        ))
                    }
                };
                let integer = match value.to_uint64() {
                    Some(v) if v <= u32::MAX as u64 => v as u32,
                    _ => {
                        return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                            SourceLocation::UNKNOWN,
                        ))
                    }
                };
                Ok(integer)
            };
            let x = evaluate(x)?;
            let y = evaluate(y)?;
            let z = evaluate(z)?;
            thread_group_size = Some((x, y, z));
        }
    }

    def.stages.push(ir::PipelineStage {
        stage,
        entry_point: func_id,
        thread_group_size,
    });

    Ok(())
}

/// Process all blend state parameters
fn parse_blend_state(
    properties: &Located<ast::PipelinePropertyValue>,
    context: &mut Context,
) -> TyperResult<ir::BlendAttachmentState> {
    let properties = match &properties.node {
        rssl_ast::PipelinePropertyValue::Single(_) => {
            return Err(TyperError::PipelinePropertyArgumentUnknown(
                properties.location,
            ))
        }
        rssl_ast::PipelinePropertyValue::Aggregate(properties) => properties,
    };

    let mut state = ir::BlendAttachmentState::default();
    for property in properties {
        match property.property.as_str() {
            "BlendEnabled" => state.blend_enabled = extract_bool(&property.value)?,
            "SrcBlend" => state.src_blend = extract_blend_factor(property)?,
            "DstBlend" => state.dst_blend = extract_blend_factor(property)?,
            "BlendOp" => state.blend_op = extract_blend_op(property)?,
            "SrcBlendAlpha" => state.src_blend_alpha = extract_blend_factor(property)?,
            "DstBlendAlpha" => state.dst_blend_alpha = extract_blend_factor(property)?,
            "BlendOpAlpha" => state.blend_op_alpha = extract_blend_op(property)?,
            "WriteMask" => {
                let value = extract_uint32(&property.value, context)?;
                let value = match u8::try_from(value) {
                    Ok(value) => value,
                    _ => {
                        return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                            property.value.location,
                        ))
                    }
                };
                state.write_mask = ir::ComponentMask(value);
            }
            _ => {
                return Err(TyperError::PipelinePropertyUnknown(
                    property.property.location,
                ))
            }
        }
    }
    Ok(state)
}

/// Attempt to view an expression as a bool
fn extract_bool(value: &Located<ast::PipelinePropertyValue>) -> TyperResult<bool> {
    match &value.node {
        ast::PipelinePropertyValue::Single(ast::Expression::Literal(ast::Literal::Bool(v))) => {
            Ok(*v)
        }
        _ => Err(TyperError::PipelinePropertyArgumentUnknown(value.location)),
    }
}

/// Attempt to view an expression as a uint
fn extract_uint32(
    property: &Located<ast::PipelinePropertyValue>,
    context: &mut Context,
) -> TyperResult<u32> {
    let property_value = match &property.node {
        rssl_ast::PipelinePropertyValue::Single(expr) => expr,
        rssl_ast::PipelinePropertyValue::Aggregate(_) => {
            return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                property.location,
            ))
        }
    };
    let value_expr = super::expressions::parse_expr(property_value, context)?;
    let value_res = crate::evaluator::evaluate_constexpr(&value_expr.0, &mut context.module);
    let value = match value_res {
        Ok(value) => value,
        _ => {
            return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                property.location,
            ))
        }
    };
    match value.to_uint64() {
        Some(v) if v <= u32::MAX as u64 => Ok(v as u32),
        _ => Err(TyperError::PipelinePropertyRequiresIntegerArgument(
            property.location,
        )),
    }
}

/// Attempt to view an expression as a string
fn extract_string(value: &Located<ast::PipelinePropertyValue>) -> TyperResult<&str> {
    match &value.node {
        ast::PipelinePropertyValue::Single(ast::Expression::Literal(ast::Literal::String(s))) => {
            Ok(s)
        }
        _ => Err(TyperError::PipelinePropertyRequiresStringArgument(
            value.location,
        )),
    }
}

/// Attempt to view an expression as a cull mode
fn extract_cull_mode(property: &ast::PipelineProperty) -> TyperResult<ir::CullMode> {
    Ok(match extract_string(&property.value)? {
        "None" => ir::CullMode::None,
        "Front" => ir::CullMode::Front,
        "Back" => ir::CullMode::Back,
        _ => {
            return Err(TyperError::PipelinePropertyArgumentUnknown(
                property.property.location,
            ))
        }
    })
}

/// Attempt to view an expression as a winding order
fn extract_winding_order(property: &ast::PipelineProperty) -> TyperResult<ir::WindingOrder> {
    Ok(match extract_string(&property.value)? {
        "CounterClockwise" => ir::WindingOrder::CounterClockwise,
        "Clockwise" => ir::WindingOrder::Clockwise,
        _ => {
            return Err(TyperError::PipelinePropertyArgumentUnknown(
                property.property.location,
            ))
        }
    })
}

/// Attempt to view an expression as a blend factor
fn extract_blend_factor(property: &ast::PipelineProperty) -> TyperResult<ir::BlendFactor> {
    Ok(match extract_string(&property.value)? {
        "Zero" => ir::BlendFactor::Zero,
        "One" => ir::BlendFactor::One,
        "SrcColor" => ir::BlendFactor::SrcColor,
        "OneMinusSrcColor" => ir::BlendFactor::OneMinusSrcColor,
        "DstColor" => ir::BlendFactor::DstColor,
        "OneMinusDstColor" => ir::BlendFactor::OneMinusDstColor,
        "SrcAlpha" => ir::BlendFactor::SrcAlpha,
        "OneMinusSrcAlpha" => ir::BlendFactor::OneMinusSrcAlpha,
        "DstAlpha" => ir::BlendFactor::DstAlpha,
        "OneMinusDstAlpha" => ir::BlendFactor::OneMinusDstAlpha,
        "SrcAlphaSaturate" => ir::BlendFactor::SrcAlphaSaturate,
        "ConstantColor" => ir::BlendFactor::ConstantColor,
        "OneMinusConstantColor" => ir::BlendFactor::OneMinusConstantColor,
        "ConstantAlpha" => ir::BlendFactor::ConstantAlpha,
        "OneMinusConstantAlpha" => ir::BlendFactor::OneMinusConstantAlpha,
        "Src1Color" => ir::BlendFactor::Src1Color,
        "OneMinusSrc1Color" => ir::BlendFactor::OneMinusSrc1Color,
        "Src1Alpha" => ir::BlendFactor::Src1Alpha,
        "OneMinusSrc1Alpha" => ir::BlendFactor::OneMinusSrc1Alpha,
        _ => {
            return Err(TyperError::PipelinePropertyArgumentUnknown(
                property.property.location,
            ))
        }
    })
}

/// Attempt to view an expression as a blend op
fn extract_blend_op(property: &ast::PipelineProperty) -> TyperResult<ir::BlendOp> {
    Ok(match extract_string(&property.value)? {
        "Add" => ir::BlendOp::Add,
        "Subtrack" => ir::BlendOp::Subtrack,
        "RevSubtract" => ir::BlendOp::RevSubtract,
        "Min" => ir::BlendOp::Min,
        "Max" => ir::BlendOp::Max,
        _ => {
            return Err(TyperError::PipelinePropertyArgumentUnknown(
                property.property.location,
            ))
        }
    })
}
