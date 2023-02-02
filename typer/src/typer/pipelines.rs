use super::errors::*;
use super::scopes::*;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;

/// Process an AST pipeline definition
pub fn parse_pipeline(def: &ast::PipelineDefinition, context: &mut Context) -> TyperResult<()> {
    let mut pipeline = ir::PipelineDefinition {
        name: def.name.clone(),
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
    let intrinsic_count = context.module.function_registry.get_function_count();
    for index in 1..intrinsic_count {
        let id = ir::FunctionId(index as u32);
        let name = context.module.function_registry.get_function_name(id);
        if name == entry_name {
            if func_id.is_some() {
                return Err(TyperError::PipelineEntryPointFunctionUnknown(location));
            }
            func_id = Some(id);
        }
    }

    let func_id = match func_id {
        Some(id) => id,
        None => return Err(TyperError::PipelineEntryPointFunctionUnknown(location)),
    };

    def.stages.push(ir::PipelineStage {
        stage,
        entry_point: func_id,
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
