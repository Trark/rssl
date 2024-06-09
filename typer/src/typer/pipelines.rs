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
    };

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
                let value_expr = super::expressions::parse_expr(&property.value.node, context)?;
                let value_res =
                    crate::evaluator::evaluate_constexpr(&value_expr.0, &mut context.module);
                let value = match value_res {
                    Ok(value) => value,
                    _ => {
                        return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                            property.property.location,
                        ))
                    }
                };
                let value = match value.to_uint64() {
                    Some(v) if v <= u32::MAX as u64 => v as u32,
                    _ => {
                        return Err(TyperError::PipelinePropertyRequiresIntegerArgument(
                            property.property.location,
                        ))
                    }
                };
                pipeline.default_bind_group_index = value;
            }
            _ => {
                return Err(TyperError::PipelinePropertyUnknown(
                    property.property.location,
                ))
            }
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
    entry_name: &Located<ast::Expression>,
    stage: ir::ShaderStage,
    context: &mut Context,
    def: &mut ir::PipelineDefinition,
) -> TyperResult<()> {
    let location = entry_name.location;
    let entry_name = match &entry_name.node {
        ast::Expression::Identifier(id) => match id.try_trivial() {
            Some(name) => name.as_str(),
            None => return Err(TyperError::PipelineEntryPointFunctionUnknown(location)),
        },
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

/// Attempt to view an expression as a string
fn extract_string(expr: &Located<ast::Expression>) -> TyperResult<&str> {
    match &expr.node {
        ast::Expression::Literal(ast::Literal::String(s)) => Ok(s),
        _ => Err(TyperError::PipelinePropertyRequiresStringArgument(
            expr.location,
        )),
    }
}
