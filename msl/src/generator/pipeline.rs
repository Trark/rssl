use std::collections::HashMap;

use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::export::*;
use rssl_text::{Located, SourceLocation};

use super::{
    generate_function_param, generate_semantic_annotation, generate_type,
    scoped_name_to_identifier, GenerateContext, GenerateError, GlobalMode,
    ImplicitFunctionParameter,
};
use crate::names::*;

/// Generate the entry point and helper structs for a pipeline definition
pub(crate) fn generate_pipeline(
    def: Option<&ir::PipelineDefinition>,
    context: &mut GenerateContext,
) -> Result<(Vec<ast::RootDefinition>, PipelineDescription), GenerateError> {
    // Generate binding info
    let mut binding_layout = PipelineBindingLayout::default();
    for decl in &context.module.root_definitions {
        analyse_bindings(decl, context.module, &mut binding_layout)?;
    }

    // Find all used globals
    let mut all_used_globals = Vec::new();
    if let Some(def) = def {
        for stage in &def.stages {
            let stage_used_globals = context
                .function_required_globals
                .get(&stage.entry_point)
                .unwrap();
            all_used_globals.extend_from_slice(stage_used_globals);
        }
    }

    // Mark all bindings as used / unused
    for argument_buffer in &mut binding_layout.0 {
        for argument in &mut argument_buffer.0 {
            argument.metadata.is_used =
                all_used_globals.contains(&ImplicitFunctionParameter::Global(argument.id));
        }
    }

    // Make map to find the argument buffer index for each global
    let mut global_to_set_index = HashMap::new();
    for (i, argument_buffer) in &mut binding_layout.0.iter().enumerate() {
        for argument in &argument_buffer.0 {
            global_to_set_index.insert(argument.id, i);
        }
    }

    pub const ARGUMENT_BUFFER_NAMES: &[&str] = &[
        ARGUMENT_BUFFER_0_NAME,
        ARGUMENT_BUFFER_1_NAME,
        ARGUMENT_BUFFER_2_NAME,
        ARGUMENT_BUFFER_3_NAME,
    ];

    let mut defs = Vec::new();

    let mut binding_params = Vec::new();
    for (i, argument_buffer) in binding_layout.0.iter_mut().enumerate() {
        // Ensure parameters are in order
        argument_buffer.0.sort_by(|lhs, rhs| {
            let index_lhs = match lhs.metadata.api_binding {
                ApiLocation::Index(i) => i,
                ApiLocation::InlineConstant(_) => panic!(),
            };
            let index_rhs = match rhs.metadata.api_binding {
                ApiLocation::Index(i) => i,
                ApiLocation::InlineConstant(_) => panic!(),
            };
            std::cmp::Ord::cmp(&index_lhs, &index_rhs)
        });

        let mut members = Vec::new();

        for argument in &argument_buffer.0 {
            let mode = context.global_variable_modes.get(&argument.id).unwrap();
            let (ty, declarator) = match mode {
                GlobalMode::Parameter {
                    base_type,
                    base_declarator,
                    ..
                } => (base_type, base_declarator),
                GlobalMode::Constant => panic!(),
            };

            let index = match argument.metadata.api_binding {
                ApiLocation::Index(i) => i,
                ApiLocation::InlineConstant(_) => panic!(),
            };

            members.push(ast::StructMember {
                ty: ty.clone(),
                defs: Vec::from([ast::InitDeclarator {
                    declarator: declarator.clone(),
                    location_annotations: Vec::new(),
                    init: None,
                }]),
                attributes: Vec::from([ast::Attribute {
                    name: Vec::from([Located::none(String::from("id"))]),
                    arguments: Vec::from([Located::none(ast::Expression::Literal(
                        ast::Literal::IntUntyped(index as u64),
                    ))]),
                    two_square_brackets: true,
                }]),
            });
        }

        let struct_name = ARGUMENT_BUFFER_NAMES[i];
        let sd = ast::StructDefinition {
            name: Located::none(String::from(struct_name)),
            base_types: Vec::new(),
            template_params: ast::TemplateParamList(Vec::new()),
            members: members
                .into_iter()
                .map(ast::StructEntry::Variable)
                .collect::<Vec<_>>(),
        };

        defs.push(ast::RootDefinition::Struct(sd));

        let mut param_type = ast::Type::trivial(struct_name);
        param_type
            .modifiers
            .prepend(Located::none(ast::TypeModifier::AddressSpace(
                ast::AddressSpace::Constant,
            )));

        binding_params.push(ast::FunctionParam {
            param_type,
            declarator: ast::Declarator::Reference(ast::ReferenceDeclarator {
                attributes: Vec::new(),
                inner: Box::new(ast::Declarator::Identifier(
                    ast::ScopedIdentifier::trivial(&format!("set{}", i)),
                    Vec::from([ast::Attribute {
                        name: Vec::from([Located::none(String::from("buffer"))]),
                        arguments: Vec::from([Located::none(ast::Expression::Literal(
                            ast::Literal::IntUntyped(i as u64),
                        ))]),
                        two_square_brackets: true,
                    }]),
                )),
            }),
            location_annotations: Vec::new(),
            default_expr: None,
        });
    }

    // Sort stages so vertex is processed before pixel
    let mut ordered_stages = def.map_or(Vec::new(), |def| def.stages.clone());
    ordered_stages.sort_by(|lhs, rhs| lhs.stage.cmp(&rhs.stage));

    // Maintain a map of the interpolator output names in the vertex / mesh stage
    let mut vertex_outputs = HashMap::<ir::Semantic, Vec<String>>::new();
    let mut pixel_in: Option<(&str, Option<Vec<(_, String)>>)> = None;

    for stage in ordered_stages {
        let mut body = Vec::new();
        let function_name =
            scoped_name_to_identifier(context.get_function_name_full(stage.entry_point).unwrap());

        let mut entry_params = Vec::new();
        let mut out_params = Vec::new();
        let mut args = Vec::new();

        let entry_return = &context
            .module
            .function_registry
            .get_function_signature(stage.entry_point)
            .return_type;

        let has_return = context
            .module
            .type_registry
            .get_type_layer(entry_return.return_type)
            != ir::TypeLayer::Void;

        if has_return {
            let ty = generate_type(entry_return.return_type, context)?;
            let attrs = match generate_semantic_annotation(&entry_return.semantic)? {
                Some(attr) => Vec::from([attr]),
                None => Vec::new(),
            };
            out_params.push(ast::StructMember {
                ty,
                defs: Vec::from([ast::InitDeclarator {
                    declarator: ast::Declarator::Identifier(
                        ast::ScopedIdentifier::trivial(STAGE_OUTPUT_NAME_LOCAL),
                        attrs,
                    ),
                    location_annotations: Vec::new(),
                    init: None,
                }]),
                attributes: Vec::new(),
            });
        }

        let semantic_params = &context
            .module
            .function_registry
            .get_function_implementation(stage.entry_point)
            .as_ref()
            .unwrap()
            .params;

        let mut requires_vertex_input = false;
        let mut pixel_input_members: Vec<(ir::TypeId, String)> = Vec::new();
        for param in semantic_params {
            let param_name = String::from(context.get_variable_name(param.id)?);
            if param.param_type.input_modifier == ir::InputModifier::In {
                match (stage.stage, &param.semantic) {
                    (ir::ShaderStage::Pixel, Some(semantic)) => {
                        match vertex_outputs.get(semantic) {
                            Some(member_name) => {
                                // User semantic which the previous stage provided
                                // Or a system semantic where the previous stage wrote the same named semantic
                                // This does not handle system semantics which have different types in different stages
                                args.push(Located::none(build_member_chain(
                                    STAGE_INPUT_NAME_LOCAL,
                                    member_name,
                                )));

                                requires_vertex_input = true;
                            }
                            None => {
                                if let ir::Semantic::User(name) = semantic {
                                    // User semantic which the previous stage did not provide
                                    return Err(GenerateError::MissingInterpolator(name.clone()));
                                } else {
                                    // System semantic where previous stage did not write the same named semantic
                                    entry_params.push(generate_function_param(
                                        param, true, false, context,
                                    )?);
                                    args.push(Located::none(ast::Expression::Identifier(
                                        ast::ScopedIdentifier::trivial(&param_name),
                                    )));
                                }
                            }
                        }
                    }
                    _ if param.interpolation_modifier
                        == Some(ir::InterpolationModifier::Payload) =>
                    {
                        let mut param_type = generate_type(param.param_type.type_id, context)?;
                        param_type
                            .modifiers
                            .prepend(Located::none(ast::TypeModifier::Const));
                        param_type.modifiers.prepend(Located::none(
                            ast::TypeModifier::AddressSpace(ast::AddressSpace::ObjectData),
                        ));
                        entry_params.push(ast::FunctionParam {
                            param_type,
                            declarator: ast::Declarator::Reference(ast::ReferenceDeclarator {
                                attributes: Vec::new(),
                                inner: Box::new(ast::Declarator::Identifier(
                                    Located::none(param_name.as_str()).into(),
                                    Vec::from([ast::Attribute {
                                        name: Vec::from([Located::none(String::from("payload"))]),
                                        arguments: Vec::new(),
                                        two_square_brackets: true,
                                    }]),
                                )),
                            }),
                            location_annotations: Vec::new(),
                            default_expr: None,
                        });
                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial(&param_name),
                        )));
                    }
                    _ => {
                        entry_params.push(generate_function_param(param, true, false, context)?);
                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial(&param_name),
                        )));
                    }
                }
            } else {
                // There should not be any valid inout parameters
                if param.param_type.input_modifier != ir::InputModifier::Out {
                    return Err(GenerateError::InvalidModule);
                }

                if stage.stage == ir::ShaderStage::Vertex || stage.stage == ir::ShaderStage::Mesh {
                    record_interpolator_location(
                        &param_name,
                        param.param_type.type_id,
                        &param.semantic,
                        param.interpolation_modifier,
                        &mut vertex_outputs,
                        &mut pixel_input_members,
                        context.module,
                    );
                }

                // Reuse function parameter generation to get basic member details
                let ast_param = {
                    let mut param = param.clone();
                    // With in parameter so it is not a reference
                    param.param_type.input_modifier = ir::InputModifier::In;
                    generate_function_param(&param, true, false, context)?
                };

                out_params.push(ast::StructMember {
                    ty: ast_param.param_type,
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: ast_param.declarator,
                        location_annotations: ast_param.location_annotations,
                        init: ast_param
                            .default_expr
                            .map(Located::none)
                            .map(ast::Initializer::Expression),
                    }]),
                    attributes: Vec::new(),
                });

                args.push(Located::none(ast::Expression::Member(
                    Box::new(Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial(STAGE_OUTPUT_NAME_LOCAL),
                    ))),
                    ast::ScopedIdentifier::trivial(&param_name),
                )));
            }
        }

        if stage.stage == ir::ShaderStage::Mesh {
            let mesh_layout = &context.mesh_layout.as_ref().unwrap();

            record_interpolator_location(
                MESH_VERTEX_ATTRIBUTES_MEMBER_NAME,
                mesh_layout.vertex_type,
                &None,
                Some(ir::InterpolationModifier::Vertices),
                &mut vertex_outputs,
                &mut pixel_input_members,
                context.module,
            );

            record_interpolator_location(
                MESH_PRIMITIVE_ATTRIBUTES_MEMBER_NAME,
                mesh_layout.primitive_type,
                &None,
                Some(ir::InterpolationModifier::Primitives),
                &mut vertex_outputs,
                &mut pixel_input_members,
                context.module,
            );
        }

        if requires_vertex_input {
            let input_name = match &pixel_in {
                Some((name, _)) => *name,
                // Should have triggered an error before setting requires_vertex_input
                _ => panic!("No interpolator struct name when generating interpolator inputs"),
            };

            entry_params.push(ast::FunctionParam {
                param_type: ast::Type::from(input_name),
                declarator: ast::Declarator::Identifier(
                    ast::ScopedIdentifier::trivial(STAGE_INPUT_NAME_LOCAL),
                    Vec::from([ast::Attribute {
                        name: Vec::from([Located::none(String::from("stage_in"))]),
                        arguments: Vec::new(),
                        two_square_brackets: true,
                    }]),
                ),

                location_annotations: Vec::new(),
                default_expr: None,
            });
        }

        entry_params.extend_from_slice(&binding_params);

        {
            let parameters_for_globals = context
                .function_required_globals
                .get(&stage.entry_point)
                .unwrap()
                .clone();
            for param in parameters_for_globals {
                match param {
                    ImplicitFunctionParameter::ThreadIndexInSimdgroup => {
                        entry_params.push(ast::FunctionParam {
                            param_type: ast::Type::from("uint"),
                            declarator: ast::Declarator::Identifier(
                                ast::ScopedIdentifier::trivial("thread_index_in_simdgroup"),
                                Vec::from([ast::Attribute {
                                    name: Vec::from([Located::none(String::from(
                                        "thread_index_in_simdgroup",
                                    ))]),
                                    arguments: Vec::new(),
                                    two_square_brackets: true,
                                }]),
                            ),
                            location_annotations: Vec::new(),
                            default_expr: None,
                        });

                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial("thread_index_in_simdgroup"),
                        )));
                    }
                    ImplicitFunctionParameter::ThreadsPerSimdgroup => {
                        entry_params.push(ast::FunctionParam {
                            param_type: ast::Type::from("uint"),
                            declarator: ast::Declarator::Identifier(
                                ast::ScopedIdentifier::trivial("threads_per_simdgroup"),
                                Vec::from([ast::Attribute {
                                    name: Vec::from([Located::none(String::from(
                                        "threads_per_simdgroup",
                                    ))]),
                                    arguments: Vec::new(),
                                    two_square_brackets: true,
                                }]),
                            ),
                            location_annotations: Vec::new(),
                            default_expr: None,
                        });

                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial("threads_per_simdgroup"),
                        )));
                    }
                    ImplicitFunctionParameter::MeshOutput => {
                        entry_params.push(ast::FunctionParam {
                            param_type: match context.mesh_output_type {
                                Some(ref ty) => ty.clone(),
                                None => return Err(GenerateError::InvalidPipelineForMeshIntrinsic),
                            },
                            declarator: ast::Declarator::from(Located::none(MESH_OUTPUT_NAME)),
                            location_annotations: Vec::new(),
                            default_expr: None,
                        });

                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial(MESH_OUTPUT_NAME),
                        )));
                    }
                    ImplicitFunctionParameter::PayloadOutput(ty) => {
                        let mut param_type = generate_type(ty, context)?;
                        param_type.modifiers.prepend(Located::none(
                            ast::TypeModifier::AddressSpace(ast::AddressSpace::ObjectData),
                        ));
                        entry_params.push(ast::FunctionParam {
                            param_type,
                            declarator: ast::Declarator::Reference(ast::ReferenceDeclarator {
                                attributes: Vec::new(),
                                inner: Box::new(ast::Declarator::Identifier(
                                    Located::none(PAYLOAD_OUTPUT_NAME).into(),
                                    Vec::from([ast::Attribute {
                                        name: Vec::from([Located::none(String::from("payload"))]),
                                        arguments: Vec::new(),
                                        two_square_brackets: true,
                                    }]),
                                )),
                            }),
                            location_annotations: Vec::new(),
                            default_expr: None,
                        });

                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial(PAYLOAD_OUTPUT_NAME),
                        )));
                    }
                    ImplicitFunctionParameter::MeshGridProperties => {
                        entry_params.push(ast::FunctionParam {
                            param_type: ast::Type::from(super::metal_lib_identifier(
                                "mesh_grid_properties",
                            )),
                            declarator: ast::Declarator::from(Located::none(
                                MESH_GRID_PROPERTIES_OUTPUT_NAME,
                            )),
                            location_annotations: Vec::new(),
                            default_expr: None,
                        });

                        args.push(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial(MESH_GRID_PROPERTIES_OUTPUT_NAME),
                        )));
                    }
                    ImplicitFunctionParameter::Global(ref gid) => {
                        let var = &context.module.global_registry[gid.0 as usize];
                        let remapped_class = match var.storage_class {
                            ir::GlobalStorage::Extern if var.static_sampler.is_some() => {
                                ir::GlobalStorage::Static
                            }
                            v => v,
                        };
                        match remapped_class {
                            ir::GlobalStorage::Extern => {
                                match context.global_variable_modes.get(gid).unwrap() {
                                    GlobalMode::Parameter { .. } => {
                                        let set_index = global_to_set_index.get(gid).unwrap();
                                        let member_expr = ast::Expression::Member(
                                            Box::new(Located::none(ast::Expression::Identifier(
                                                ast::ScopedIdentifier::trivial(&format!(
                                                    "set{}",
                                                    set_index
                                                )),
                                            ))),
                                            ast::ScopedIdentifier::trivial(
                                                context.get_global_name(*gid).unwrap(),
                                            ),
                                        );
                                        args.push(Located::none(member_expr));
                                    }
                                    GlobalMode::Constant => {
                                        panic!("global does not require a parameter")
                                    }
                                }
                            }
                            ir::GlobalStorage::Static => {
                                match context.global_variable_modes.get(gid).unwrap() {
                                    GlobalMode::Parameter {
                                        base_type,
                                        base_declarator,
                                        init,
                                        ..
                                    } => {
                                        let name = context.get_global_name(*gid).unwrap();

                                        body.push(ast::Statement {
                                            kind: ast::StatementKind::Var(ast::VarDef {
                                                local_type: base_type.clone(),
                                                defs: Vec::from([ast::InitDeclarator {
                                                    declarator: base_declarator.clone(),
                                                    location_annotations: Vec::new(),
                                                    init: init.clone(),
                                                }]),
                                            }),
                                            location: SourceLocation::UNKNOWN,
                                            attributes: Vec::new(),
                                        });

                                        let member_expr = ast::Expression::Identifier(
                                            ast::ScopedIdentifier::trivial(name),
                                        );
                                        args.push(Located::none(member_expr));
                                    }
                                    GlobalMode::Constant => {
                                        panic!("static does not require a parameter")
                                    }
                                }
                            }
                            ir::GlobalStorage::GroupShared => {
                                match context.global_variable_modes.get(gid).unwrap() {
                                    GlobalMode::Parameter {
                                        base_type,
                                        base_declarator,
                                        init,
                                        ..
                                    } => {
                                        let name = context.get_global_name(*gid).unwrap();

                                        body.push(ast::Statement {
                                            kind: ast::StatementKind::Var(ast::VarDef {
                                                local_type: base_type.clone(),
                                                defs: Vec::from([ast::InitDeclarator {
                                                    declarator: base_declarator.clone(),
                                                    location_annotations: Vec::new(),
                                                    init: init.clone(),
                                                }]),
                                            }),
                                            location: SourceLocation::UNKNOWN,
                                            attributes: Vec::new(),
                                        });

                                        let member_expr = ast::Expression::Identifier(
                                            ast::ScopedIdentifier::trivial(name),
                                        );
                                        args.push(Located::none(member_expr));
                                    }
                                    GlobalMode::Constant => {
                                        panic!("static does not require a parameter")
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // Generate input struct for pixel shader if required
        if stage.stage == ir::ShaderStage::Pixel {
            if let Some((name, Some(pixel_in_members))) = &pixel_in {
                let mut members = Vec::new();
                for (ty, name) in pixel_in_members {
                    members.push(ast::StructEntry::Variable(ast::StructMember {
                        ty: generate_type(*ty, context)?,
                        defs: Vec::from([ast::InitDeclarator {
                            declarator: ast::Declarator::Identifier(
                                ast::ScopedIdentifier::trivial(name),
                                Default::default(),
                            ),
                            location_annotations: Default::default(),
                            init: None,
                        }]),
                        attributes: Default::default(),
                    }));
                }

                let entry_point = ast::StructDefinition {
                    name: Located::none(String::from(*name)),
                    base_types: Default::default(),
                    template_params: ast::TemplateParamList(Vec::new()),
                    members,
                };
                defs.push(ast::RootDefinition::Struct(entry_point));
            }
        }

        // Generate output struct for stage
        let return_type = if !out_params.is_empty() {
            let stage_out_param_name = match stage.stage {
                ir::ShaderStage::Vertex => STAGE_OUTPUT_NAME_VERTEX,
                ir::ShaderStage::Pixel => STAGE_OUTPUT_NAME_PIXEL,
                ir::ShaderStage::Compute => return Err(GenerateError::InvalidModule),
                ir::ShaderStage::Task => return Err(GenerateError::UnimplementedTaskShader),
                ir::ShaderStage::Mesh => return Err(GenerateError::UnexpectedOutputFromMeshStage),
            };

            let sd = ast::StructDefinition {
                name: Located::none(String::from(stage_out_param_name)),
                base_types: Vec::new(),
                template_params: ast::TemplateParamList(Vec::new()),
                members: out_params
                    .into_iter()
                    .map(ast::StructEntry::Variable)
                    .collect(),
            };

            defs.push(ast::RootDefinition::Struct(sd));

            body.push(ast::Statement {
                kind: ast::StatementKind::Var(ast::VarDef::one(
                    Located::none(String::from(STAGE_OUTPUT_NAME_LOCAL)),
                    ast::Type::from(stage_out_param_name),
                )),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });

            match stage.stage {
                ir::ShaderStage::Vertex | ir::ShaderStage::Pixel => Some(stage_out_param_name),
                ir::ShaderStage::Compute => return Err(GenerateError::InvalidModule),
                ir::ShaderStage::Task | ir::ShaderStage::Mesh => None,
            }
        } else {
            None
        };

        let call_expr = ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(function_name))),
            Vec::new(),
            args,
        );

        let call_expr = if has_return {
            ast::Expression::BinaryOperation(
                ast::BinOp::Assignment,
                Box::new(Located::none(ast::Expression::Member(
                    Box::new(Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial(STAGE_OUTPUT_NAME_LOCAL),
                    ))),
                    ast::ScopedIdentifier::trivial(STAGE_OUTPUT_NAME_LOCAL),
                ))),
                Box::new(Located::none(call_expr)),
            )
        } else {
            call_expr
        };

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(call_expr),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });

        if return_type.is_some() {
            body.push(ast::Statement {
                kind: ast::StatementKind::Return(Some(Located::none(ast::Expression::Identifier(
                    ast::ScopedIdentifier::trivial(STAGE_OUTPUT_NAME_LOCAL),
                )))),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
        }

        let entry_point_name = match stage.stage {
            ir::ShaderStage::Vertex => ENTRY_POINT_NAME_VERTEX,
            ir::ShaderStage::Pixel => ENTRY_POINT_NAME_PIXEL,
            ir::ShaderStage::Compute => ENTRY_POINT_NAME_COMPUTE,
            ir::ShaderStage::Task => ENTRY_POINT_NAME_TASK,
            ir::ShaderStage::Mesh => ENTRY_POINT_NAME_MESH,
        };

        let stage_attribute_name = match stage.stage {
            ir::ShaderStage::Vertex => "vertex",
            ir::ShaderStage::Pixel => "fragment",
            ir::ShaderStage::Compute => "kernel",
            ir::ShaderStage::Task => "object",
            ir::ShaderStage::Mesh => "mesh",
        };

        let mut attributes = Vec::from([ast::Attribute {
            name: Vec::from([Located::none(String::from(stage_attribute_name))]),
            arguments: Vec::new(),
            two_square_brackets: true,
        }]);

        for attribute in &context
            .module
            .function_registry
            .get_function_implementation(stage.entry_point)
            .as_ref()
            .unwrap()
            .attributes
        {
            if let Some(attr) = super::generate_function_attribute(attribute, true, context)? {
                attributes.push(attr);
            }
        }

        let entry_point = ast::FunctionDefinition {
            name: Located::none(String::from(entry_point_name)),
            returntype: ast::FunctionReturn {
                return_type: ast::Type::trivial(return_type.unwrap_or("void")),
                location_annotations: Vec::new(),
            },
            template_params: ast::TemplateParamList(Vec::new()),
            params: entry_params.clone(),
            is_const: false,
            is_volatile: false,
            body: Some(body),
            attributes,
        };
        defs.push(ast::RootDefinition::Function(entry_point));

        match stage.stage {
            ir::ShaderStage::Vertex => {
                // Vertex shaders use the same types as the pixel shader
                // We reuse the vertex output struct for pixel input
                assert!(pixel_input_members.is_empty());
                pixel_in = return_type.map(|ty| (ty, None));
            }
            ir::ShaderStage::Mesh => {
                // Mesh shaders use non-array form of output types
                // We declare a new struct for this form
                pixel_in = Some((STAGE_INPUT_NAME_PIXEL, Some(pixel_input_members)));
            }
            _ => {
                assert!(pixel_input_members.is_empty());
            }
        }
    }

    let desc = binding_layout.finish();
    Ok((defs, desc))
}

/// Track where output parameters are bound
fn record_interpolator_location(
    param_name: &str,
    param_type: ir::TypeId,
    semantic: &Option<ir::Semantic>,
    interpolation_modifier: Option<ir::InterpolationModifier>,
    vertex_outputs: &mut HashMap<ir::Semantic, Vec<String>>,
    pixel_input_members: &mut Vec<(ir::TypeId, String)>,
    module: &ir::Module,
) {
    if let Some(semantic) = &semantic {
        vertex_outputs.insert(semantic.clone(), Vec::from([String::from(param_name)]));
    } else {
        let param_ty = module.type_registry.remove_modifier(param_type);
        let param_tyl = module.type_registry.get_type_layer(param_ty);

        // Record mesh output types
        if matches!(
            interpolation_modifier,
            Some(ir::InterpolationModifier::Vertices | ir::InterpolationModifier::Primitives)
        ) && !matches!(param_tyl, ir::TypeLayer::Void)
        {
            pixel_input_members.push((param_ty, String::from(param_name)));
        }

        if let ir::TypeLayer::Struct(sid) = param_tyl {
            let sd = &module.struct_registry[sid.0 as usize];

            for member in &sd.members {
                if let Some(semantic) = &member.semantic {
                    vertex_outputs.insert(
                        semantic.clone(),
                        Vec::from([String::from(param_name), member.name.clone()]),
                    );
                }
            }
        }
    }
}

/// Build a multi level member access from member names
fn build_member_chain(root: &str, names: &[String]) -> ast::Expression {
    match names {
        [start] => ast::Expression::Member(
            Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial(root),
            ))),
            ast::ScopedIdentifier::trivial(start),
        ),
        [start @ .., end] => ast::Expression::Member(
            Box::new(Located::none(build_member_chain(root, start))),
            ast::ScopedIdentifier::trivial(end),
        ),
        &[] => panic!("Invalid empty member name chain"),
    }
}

#[derive(Default)]
pub struct PipelineBindingLayout(pub Vec<ArgumentBufferLayout>);

#[derive(Default)]
pub struct ArgumentBufferLayout(pub Vec<ArgumentBufferEntry>);

pub struct ArgumentBufferEntry {
    metadata: DescriptorBinding,
    id: ir::GlobalId,
}

impl PipelineBindingLayout {
    /// Add a binding to the pipeline layout description
    fn register_binding(&mut self, group_index: u32, binding: DescriptorBinding, id: ir::GlobalId) {
        let group_index = group_index as usize;
        if group_index >= self.0.len() {
            self.0
                .resize_with(group_index + 1, ArgumentBufferLayout::default)
        }

        self.0[group_index].0.push(ArgumentBufferEntry {
            metadata: binding,
            id,
        });
    }

    fn finish(self) -> PipelineDescription {
        PipelineDescription {
            bind_groups: self
                .0
                .into_iter()
                .map(|set| BindGroup {
                    bindings: set
                        .0
                        .into_iter()
                        .map(|binding| binding.metadata)
                        .collect::<Vec<_>>(),
                    inline_constants: None,
                })
                .collect::<Vec<_>>(),
        }
    }
}

/// Find a binding within a root definition
fn analyse_bindings(
    decl: &ir::RootDefinition,
    module: &ir::Module,
    layout: &mut PipelineBindingLayout,
) -> Result<(), GenerateError> {
    match decl {
        ir::RootDefinition::Struct(_)
        | ir::RootDefinition::StructTemplate(_)
        | ir::RootDefinition::Enum(_)
        | ir::RootDefinition::FunctionDeclaration(_)
        | ir::RootDefinition::Function(_) => {}
        ir::RootDefinition::ConstantBuffer(_) => {
            return Err(GenerateError::ConstantBuffersNotSimplified);
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let decl = &module.global_registry[id.0 as usize];

            // Remove outer layer of modifier
            let unmodified_id = module.type_registry.remove_modifier(decl.type_id);

            // Attempt to remove array type - and extract the length
            let (unmodified_id, descriptor_count) = if let ir::TypeLayer::Array(inner, len) =
                module.type_registry.get_type_layer(unmodified_id)
            {
                // Remove inner layer of modifier
                let unmodified_id = module.type_registry.remove_modifier(inner);
                let len = len.map(|v| v as u32);
                (unmodified_id, len)
            } else {
                (unmodified_id, Some(1))
            };

            // Get info for type layer after extracting outer shells
            let type_layer = module.type_registry.get_type_layer(unmodified_id);

            let descriptor_type = match type_layer {
                ir::TypeLayer::Object(ir::ObjectType::ConstantBuffer(_)) => {
                    DescriptorType::ConstantBuffer
                }
                ir::TypeLayer::Object(ir::ObjectType::ByteAddressBuffer) => {
                    DescriptorType::ByteBuffer
                }
                ir::TypeLayer::Object(ir::ObjectType::RWByteAddressBuffer) => {
                    DescriptorType::RwByteBuffer
                }
                ir::TypeLayer::Object(ir::ObjectType::BufferAddress) => {
                    DescriptorType::BufferAddress
                }
                ir::TypeLayer::Object(ir::ObjectType::RWBufferAddress) => {
                    DescriptorType::RwBufferAddress
                }
                ir::TypeLayer::Object(ir::ObjectType::StructuredBuffer(_)) => {
                    DescriptorType::StructuredBuffer
                }
                ir::TypeLayer::Object(ir::ObjectType::RWStructuredBuffer(_)) => {
                    DescriptorType::RwStructuredBuffer
                }
                ir::TypeLayer::Object(ir::ObjectType::Buffer(_)) => DescriptorType::TexelBuffer,
                ir::TypeLayer::Object(ir::ObjectType::RWBuffer(_)) => DescriptorType::RwTexelBuffer,
                ir::TypeLayer::Object(ir::ObjectType::Texture2D(_)) => DescriptorType::Texture2d,
                ir::TypeLayer::Object(ir::ObjectType::Texture2DArray(_)) => {
                    DescriptorType::Texture2dArray
                }
                ir::TypeLayer::Object(ir::ObjectType::RWTexture2D(_)) => {
                    DescriptorType::RwTexture2d
                }
                ir::TypeLayer::Object(ir::ObjectType::RWTexture2DArray(_)) => {
                    DescriptorType::RwTexture2dArray
                }
                ir::TypeLayer::Object(ir::ObjectType::TextureCube(_)) => {
                    DescriptorType::TextureCube
                }
                ir::TypeLayer::Object(ir::ObjectType::TextureCubeArray(_)) => {
                    DescriptorType::TextureCubeArray
                }
                ir::TypeLayer::Object(ir::ObjectType::Texture3D(_)) => DescriptorType::Texture3d,
                ir::TypeLayer::Object(ir::ObjectType::RWTexture3D(_)) => {
                    DescriptorType::RwTexture3d
                }
                ir::TypeLayer::Object(ir::ObjectType::RaytracingAccelerationStructure) => {
                    DescriptorType::RaytracingAccelerationStructure
                }
                ir::TypeLayer::Object(ir::ObjectType::SamplerState) => DescriptorType::SamplerState,
                ir::TypeLayer::Object(ir::ObjectType::SamplerComparisonState) => {
                    DescriptorType::SamplerComparisonState
                }
                ir::TypeLayer::Object(_) => return Err(GenerateError::UnsupportedObjectType),
                _ => DescriptorType::PushConstants,
            };

            if let Some(api_slot) = decl.api_slot {
                let binding = DescriptorBinding {
                    name: module.get_global_name(*id).to_string(),
                    api_binding: api_slot.location,
                    descriptor_type,
                    descriptor_count,
                    is_bindless: decl.is_bindless,
                    is_used: true, // We will update this later
                    // Static samplers are in source code so are not reflected
                    static_sampler: None,
                };

                layout.register_binding(api_slot.set, binding, *id);
            }
        }
    }
    Ok(())
}
