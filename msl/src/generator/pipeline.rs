use std::collections::HashMap;

use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::export::*;
use rssl_text::{Located, SourceLocation};

use super::{
    generate_function_param, scoped_name_to_identifier, GenerateContext, GenerateError, GlobalMode,
};
use crate::names::*;

/// Generate the entry point and helper structs for a pipeline definition
pub(crate) fn generate_pipeline(
    def: &ir::PipelineDefinition,
    context: &mut GenerateContext,
) -> Result<(Vec<ast::RootDefinition>, PipelineDescription), GenerateError> {
    // Generate binding info
    let mut binding_layout = PipelineBindingLayout::default();
    for decl in &context.module.root_definitions {
        analyse_bindings(decl, context.module, &mut binding_layout)?;
    }

    // Find all used globals
    let mut all_used_globals = Vec::new();
    for stage in &def.stages {
        let stage_used_globals = context
            .function_required_globals
            .get(&stage.entry_point)
            .unwrap();
        all_used_globals.extend_from_slice(stage_used_globals);
    }

    // Mark all bindings as used / unused
    for argument_buffer in &mut binding_layout.0 {
        for argument in &mut argument_buffer.0 {
            argument.metadata.is_used = all_used_globals.contains(&argument.id);
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
            let param = match mode {
                GlobalMode::Parameter(param, _, _) => param,
                GlobalMode::Constant => panic!(),
            };

            let index = match argument.metadata.api_binding {
                ApiLocation::Index(i) => i,
                ApiLocation::InlineConstant(_) => panic!(),
            };

            members.push(ast::StructMember {
                ty: param.param_type.clone(),
                defs: Vec::from([ast::InitDeclarator {
                    declarator: param.declarator.clone(),
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

    for stage in &def.stages {
        let mut body = Vec::new();
        let function_name =
            scoped_name_to_identifier(context.get_function_name_full(stage.entry_point).unwrap());

        let mut entry_params = Vec::new();
        let mut args = Vec::new();

        let semantic_params = &context
            .module
            .function_registry
            .get_function_implementation(stage.entry_point)
            .as_ref()
            .unwrap()
            .params;

        for param in semantic_params {
            entry_params.push(generate_function_param(param, true, context)?);
            args.push(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial(context.get_variable_name(param.id)?),
            )));
        }

        entry_params.extend_from_slice(&binding_params);

        {
            let parameters_for_globals = context
                .function_required_globals
                .get(&stage.entry_point)
                .unwrap();
            for gid in parameters_for_globals {
                match context.module.global_registry[gid.0 as usize].storage_class {
                    ir::GlobalStorage::Extern => match context
                        .global_variable_modes
                        .get(gid)
                        .unwrap()
                    {
                        GlobalMode::Parameter(_, _, _) => {
                            let set_index = global_to_set_index.get(gid).unwrap();
                            let member_expr = ast::Expression::Member(
                                Box::new(Located::none(ast::Expression::Identifier(
                                    ast::ScopedIdentifier::trivial(&format!("set{}", set_index)),
                                ))),
                                ast::ScopedIdentifier::trivial(
                                    context.get_global_name(*gid).unwrap(),
                                ),
                            );
                            args.push(Located::none(member_expr));
                        }
                        GlobalMode::Constant => panic!("global does not require a parameter"),
                    },
                    ir::GlobalStorage::Static => {
                        return Err(GenerateError::UnimplementedFunctionEntryStatic)
                    }
                    ir::GlobalStorage::GroupShared => {
                        return Err(GenerateError::UnimplementedFunctionEntryGroupShared)
                    }
                }
            }
        }

        body.push(ast::Statement {
            kind: ast::StatementKind::Expression(ast::Expression::Call(
                Box::new(Located::none(ast::Expression::Identifier(function_name))),
                Vec::new(),
                args,
            )),
            location: SourceLocation::UNKNOWN,
            attributes: Vec::new(),
        });

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

        let entry_point = ast::FunctionDefinition {
            name: Located::none(String::from(entry_point_name)),
            returntype: ast::FunctionReturn {
                return_type: ast::Type::trivial("void"),
                location_annotations: Vec::new(),
            },
            template_params: ast::TemplateParamList(Vec::new()),
            params: entry_params.clone(),
            is_const: false,
            is_volatile: false,
            body: Some(body),
            attributes: Vec::from([ast::Attribute {
                name: Vec::from([Located::none(String::from(stage_attribute_name))]),
                arguments: Vec::new(),
                two_square_brackets: true,
            }]),
        };
        defs.push(ast::RootDefinition::Function(entry_point));
    }

    let desc = binding_layout.finish();
    Ok((defs, desc))
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
                };

                layout.register_binding(api_slot.set, binding, *id);
            }
        }
    }
    Ok(())
}
