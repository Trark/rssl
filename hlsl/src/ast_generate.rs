use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::export::*;
use rssl_text::{Located, SourceLocation};

use crate::names::*;

/// Result from generating HLSL from RSSL
pub struct GeneratedAST {
    /// Output ast - reusing the RSSL ast as it is a superset of HLSL
    pub ast_module: ast::Module,

    /// Metadata to describe the pipeline that has been generated
    pub pipeline_description: PipelineDescription,
}

/// Error result when generating HLSL fails
#[derive(Debug)]
pub enum GenerateError {
    /// Input module is invalid as it references an unknown id
    NamelessId,

    /// Unable to generate a valid ast for a type with an array modifier in this position
    ComplexTypeBind,
}

/// Generate HLSL ast from ir module
///
/// We assume the generated code will be built with:
/// * HLSL version 2021
/// * If using half: -enable-16bit-types
pub fn generate_module(module: &ir::Module) -> Result<GeneratedAST, GenerateError> {
    let mut context = GenerateContext::new(module);
    let mut root_definitions = Vec::new();

    // Generate binding info
    for decl in &module.root_definitions {
        analyse_bindings(decl, &mut context)?;
    }

    // Create inline constant buffers from bindings
    generate_inline_constant_buffers(
        &module.inline_constant_buffers,
        &mut root_definitions,
        &mut context,
    )?;

    generate_root_definitions(
        module,
        &module.root_definitions,
        &mut root_definitions,
        &mut context,
    )?;

    let root_definitions = simplify_namespaces(root_definitions);

    Ok(GeneratedAST {
        ast_module: ast::Module { root_definitions },
        pipeline_description: context.pipeline_description,
    })
}

/// Check bindings
fn analyse_bindings(
    decl: &ir::RootDefinition,
    context: &mut GenerateContext,
) -> Result<(), GenerateError> {
    match decl {
        ir::RootDefinition::Struct(_)
        | ir::RootDefinition::StructTemplate(_)
        | ir::RootDefinition::Enum(_)
        | ir::RootDefinition::FunctionDeclaration(_)
        | ir::RootDefinition::Function(_) => {}
        ir::RootDefinition::ConstantBuffer(id) => {
            let cb = &context.module.cbuffer_registry[id.0 as usize];
            if let Some(api_slot) = cb.api_binding {
                let binding = DescriptorBinding {
                    name: context.get_constant_buffer_name(*id)?.to_string(),
                    api_binding: api_slot.location,
                    descriptor_type: DescriptorType::ConstantBuffer,
                    descriptor_count: Some(1),
                    is_bindless: false,
                    is_used: true, // We do not currently check for usage
                };

                context.register_binding(api_slot.set, binding);
            }
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let decl = &context.module.global_registry[id.0 as usize];

            // Remove outer layer of modifier
            let unmodified_id = context.module.type_registry.remove_modifier(decl.type_id);

            // Attempt to remove array type - and extract the length
            let (unmodified_id, descriptor_count) = if let ir::TypeLayer::Array(inner, len) =
                context.module.type_registry.get_type_layer(unmodified_id)
            {
                // Remove inner layer of modifier
                let unmodified_id = context.module.type_registry.remove_modifier(inner);
                let len = len.map(|v| v as u32);
                (unmodified_id, len)
            } else {
                (unmodified_id, Some(1))
            };

            // Get info for type layer after extracting outer shells
            let type_layer = context.module.type_registry.get_type_layer(unmodified_id);

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
                    name: context.get_global_name(*id)?.to_string(),
                    api_binding: api_slot.location,
                    descriptor_type,
                    descriptor_count,
                    is_bindless: decl.is_bindless,
                    is_used: true, // We do not currently check for usage
                };

                context.register_binding(api_slot.set, binding);
            }
        }
    }
    Ok(())
}

/// Take the binding definitions and build the inline constant buffers required
fn generate_inline_constant_buffers(
    inline_constant_buffers: &[ir::InlineConstantBuffer],
    output: &mut Vec<ast::RootDefinition>,
    context: &mut GenerateContext,
) -> Result<(), GenerateError> {
    for buffer in inline_constant_buffers {
        let bind_group = &mut context.pipeline_description.bind_groups[buffer.set as usize];
        let mut found_size = 0;

        let mut members = Vec::new();
        for binding in &bind_group.bindings {
            if let ApiLocation::InlineConstant(offset) = binding.api_binding {
                assert!(offset + 8 <= buffer.size_in_bytes);
                members.push(ast::StructEntry::Variable(ast::StructMember {
                    ty: ast::Type::trivial("uint64_t"),
                    defs: Vec::from([ast::InitDeclarator {
                        declarator: Located::none(binding.name.as_str()).into(),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                    attributes: Vec::from([ast::Attribute {
                        name: Vec::from([
                            Located::none("vk".to_string()),
                            Located::none("offset".to_string()),
                        ]),
                        arguments: Vec::from([Located::none(ast::Expression::Literal(
                            ast::Literal::IntUntyped(offset as u64),
                        ))]),
                        two_square_brackets: true,
                    }]),
                }));
                found_size += 8;
            }
        }
        assert_eq!(buffer.size_in_bytes, found_size);

        let struct_name = format!("InlineDescriptor{}", buffer.set);
        let struct_name_identifier = ast::ExpressionOrType::Type(ast::Type::trivial(&struct_name));

        let inline_descriptor = ast::StructDefinition {
            name: Located::none(struct_name),
            base_types: Vec::new(),
            template_params: ast::TemplateParamList(Vec::new()),
            members,
        };

        output.push(ast::RootDefinition::Struct(inline_descriptor));

        let binding_attribute = generate_vk_binding_annotation(&Some(ir::ApiBinding {
            set: buffer.set,
            location: ApiLocation::Index(buffer.api_location),
            slot_type: None,
        }))?;

        output.push(ast::RootDefinition::GlobalVariable(ast::GlobalVariable {
            global_type: ast::Type::with_template_types(
                Located::none("ConstantBuffer"),
                &[struct_name_identifier],
            ),
            defs: Vec::from([ast::InitDeclarator {
                declarator: ast::Declarator::Identifier(
                    ast::ScopedIdentifier::trivial(&format!("g_inlineDescriptor{}", buffer.set)),
                    Vec::new(),
                ),
                location_annotations: Vec::new(),
                init: None,
            }]),
            attributes: match binding_attribute {
                Some(attr) => Vec::from([attr]),
                None => Vec::new(),
            },
        }));

        assert_eq!(bind_group.inline_constants, None);
        bind_group.inline_constants = Some(InlineConstantBuffer {
            api_location: buffer.api_location,
            size_in_bytes: buffer.size_in_bytes,
        });
    }

    Ok(())
}

/// Remove adjacent namespace nodes
fn simplify_namespaces(original: Vec<ast::RootDefinition>) -> Vec<ast::RootDefinition> {
    let mut simplified = Vec::new();
    for root_definition in original {
        use ast::RootDefinition::Namespace;
        match (root_definition, simplified.last_mut()) {
            // Matching namespaces are merged with the existing namespace
            (
                Namespace(next_name, mut next_defs),
                Some(Namespace(last_name, ref mut last_defs)),
            ) if last_name.node == next_name.node => {
                last_defs.append(&mut next_defs);
            }
            // Non-matching namespaces and other root definitions are appended to the output unchanged
            (root_definition, _) => simplified.push(root_definition),
        }
    }

    // Apply recursively
    for root_definition in &mut simplified {
        if let ast::RootDefinition::Namespace(_, ref mut defs) = root_definition {
            let v = std::mem::take(defs);
            let v = simplify_namespaces(v);
            *defs = v;
        }
    }

    simplified
}

/// Generate HLSL for a set of root definitions
fn generate_root_definitions(
    module: &ir::Module,
    decls: &[ir::RootDefinition],
    output: &mut Vec<ast::RootDefinition>,
    context: &mut GenerateContext,
) -> Result<(), GenerateError> {
    for decl in decls {
        let (namespace, mut defs) = generate_root_definition(module, decl, context)?;

        // Push into namespaces from the root
        // We will clean up the number of namespace nodes at the end
        let mut current_namespace = namespace;
        while let Some(namespace_id) = current_namespace {
            let name = context
                .module
                .namespace_registry
                .get_namespace_name(namespace_id);

            defs = Vec::from([ast::RootDefinition::Namespace(
                Located::none(name.to_string()),
                defs,
            )]);

            current_namespace = context
                .module
                .namespace_registry
                .get_namespace_parent(namespace_id);
        }

        for def in defs {
            output.push(def);
        }
    }

    Ok(())
}

/// Generate HLSL for a single root definition
fn generate_root_definition(
    module: &ir::Module,
    decl: &ir::RootDefinition,
    context: &mut GenerateContext,
) -> Result<(Option<ir::NamespaceId>, Vec<ast::RootDefinition>), GenerateError> {
    // Get the namespace we will need to be in
    let namespace = match decl {
        ir::RootDefinition::Struct(id) => module.struct_registry[id.0 as usize].namespace,
        ir::RootDefinition::StructTemplate(_) => {
            todo!("RootDefinition::StructTemplate")
        }
        ir::RootDefinition::Enum(id) => module.enum_registry.get_enum_definition(*id).namespace,
        ir::RootDefinition::ConstantBuffer(id) => module.cbuffer_registry[id.0 as usize].namespace,
        ir::RootDefinition::GlobalVariable(id) => module.global_registry[id.0 as usize].namespace,
        ir::RootDefinition::FunctionDeclaration(id) | ir::RootDefinition::Function(id) => {
            module
                .function_registry
                .get_function_name_definition(*id)
                .namespace
        }
    };

    let defs = match decl {
        ir::RootDefinition::Struct(id) => {
            let sd = &module.struct_registry[id.0 as usize];
            let def = generate_struct(sd, context)?;
            Vec::from([ast::RootDefinition::Struct(def)])
        }
        ir::RootDefinition::StructTemplate(_) => {
            todo!("RootDefinition::StructTemplate")
        }
        ir::RootDefinition::Enum(id) => {
            let def = generate_enum(*id, context)?;
            Vec::from([ast::RootDefinition::Enum(def)])
        }
        ir::RootDefinition::ConstantBuffer(id) => {
            let def = generate_constant_buffer(*id, context)?;
            Vec::from([ast::RootDefinition::ConstantBuffer(def)])
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let def = generate_global_variable(*id, context)?;
            Vec::from([ast::RootDefinition::GlobalVariable(def)])
        }
        ir::RootDefinition::FunctionDeclaration(id) => generate_function(*id, true, context)?
            .into_iter()
            .map(ast::RootDefinition::Function)
            .collect::<Vec<_>>(),
        ir::RootDefinition::Function(id) => generate_function(*id, false, context)?
            .into_iter()
            .map(ast::RootDefinition::Function)
            .collect::<Vec<_>>(),
    };

    Ok((namespace, defs))
}

/// Generate HLSL for a global variable declaration
fn generate_global_variable(
    id: ir::GlobalId,
    context: &mut GenerateContext,
) -> Result<ast::GlobalVariable, GenerateError> {
    let decl = &context.module.global_registry[id.0 as usize];

    let mut attributes = Vec::new();
    let is_extern = decl.storage_class == ir::GlobalStorage::Extern;
    if is_extern && context.module.flags.requires_vk_binding {
        append_vk_binding_annotation(&decl.api_slot, &mut attributes)?;
    }

    // const is implicit on extern variables so there is no need to write it out
    // volatile is not valid on globals so happens to get suppressed at the same time
    let suppress_const_volatile = matches!(decl.storage_class, ir::GlobalStorage::Extern);

    let storage_modifier = match decl.storage_class {
        ir::GlobalStorage::Extern => None,
        ir::GlobalStorage::Static => Some(ast::TypeModifier::Static),
        ir::GlobalStorage::GroupShared => Some(ast::TypeModifier::GroupShared),
    };

    let name = Located::none(context.get_global_name(id)?.to_string());

    let (type_base, declarator) =
        generate_type_and_declarator(decl.type_id, &name, suppress_const_volatile, context)?;

    let global_type = prepend_modifiers(type_base, &[storage_modifier]);

    let slot = if is_extern && !context.module.flags.requires_vk_binding {
        generate_register_annotation(&decl.api_slot)?
    } else {
        None
    };

    let init = if let Some(ir::ApiBinding {
        set,
        location: ApiLocation::InlineConstant(_),
        ..
    }) = decl.api_slot
    {
        assert_eq!(decl.init, None);

        let global_name = ast::ScopedIdentifier::trivial(&format!("g_inlineDescriptor{set}"));
        let global_name_expr = Box::new(Located::none(ast::Expression::Identifier(global_name)));
        let member_name = ast::ScopedIdentifier::trivial(&name);
        let expr = ast::Expression::Member(global_name_expr, member_name);

        Some(ast::Initializer::Expression(Located::none(expr)))
    } else {
        generate_initializer(&decl.init, context)?
    };

    let location_annotations = if let Some(register) = slot {
        Vec::from([ast::LocationAnnotation::Register(register)])
    } else {
        Vec::new()
    };

    let init_declarator = ast::InitDeclarator {
        declarator,
        location_annotations,
        init,
    };

    let gv = ast::GlobalVariable {
        global_type,
        defs: Vec::from([init_declarator]),
        attributes,
    };

    Ok(gv)
}

/// Generate a HLSL register annotation from a binding location
fn generate_register_annotation(
    slot: &Option<ir::ApiBinding>,
) -> Result<Option<ast::Register>, GenerateError> {
    if let Some(slot) = &slot {
        let slot_type = match slot.slot_type {
            Some(register_type) => register_type,
            None => panic!("HLSL generator requires register types in api binding metadata"),
        };

        let index = match slot.location {
            ApiLocation::Index(index) => index,
            ApiLocation::InlineConstant(_) => {
                panic!("generate_register_annotation did not expect an inline constant")
            }
        };

        Ok(Some(ast::Register {
            slot: Some(ast::RegisterSlot { slot_type, index }),
            space: if slot.set != 0 { Some(slot.set) } else { None },
        }))
    } else {
        Ok(None)
    }
}

/// Generate a HLSL vk::binding annotation from a binding location
fn generate_vk_binding_annotation(
    slot: &Option<ir::ApiBinding>,
) -> Result<Option<ast::Attribute>, GenerateError> {
    if let Some(slot) = &slot {
        assert_eq!(slot.slot_type, None);
        match slot.location {
            ApiLocation::Index(index) => {
                let name = Vec::from([
                    Located::none("vk".to_string()),
                    Located::none("binding".to_string()),
                ]);
                let index = Located::none(ast::Expression::Literal(ast::Literal::IntUntyped(
                    index as u64,
                )));
                let set_index = Located::none(ast::Expression::Literal(ast::Literal::IntUntyped(
                    slot.set as u64,
                )));
                let arguments = if slot.set != 0 {
                    Vec::from([index, set_index])
                } else {
                    Vec::from([index])
                };
                Ok(Some(ast::Attribute {
                    name,
                    arguments,
                    two_square_brackets: true,
                }))
            }
            ApiLocation::InlineConstant(_) => {
                panic!("generate_register_annotation did not expect an inline constant")
            }
        }
    } else {
        Ok(None)
    }
}

/// Append a HLSL vk::binding annotation from a binding location to a list of attributes
fn append_vk_binding_annotation(
    slot: &Option<ir::ApiBinding>,
    attributes: &mut Vec<ast::Attribute>,
) -> Result<(), GenerateError> {
    let attr_opt = generate_vk_binding_annotation(slot)?;
    if let Some(attr) = attr_opt {
        attributes.push(attr);
    }
    Ok(())
}

/// Generate HLSL for a function and all template instantiations
fn generate_function(
    id: ir::FunctionId,
    only_declare: bool,
    context: &mut GenerateContext,
) -> Result<Vec<ast::FunctionDefinition>, GenerateError> {
    let sig = context.module.function_registry.get_function_signature(id);
    let is_template = !sig.template_params.is_empty();

    if is_template {
        let mut fs = Vec::new();

        for child_id in context.module.function_registry.iter() {
            if let Some(data) = context
                .module
                .function_registry
                .get_template_instantiation_data(child_id)
            {
                if data.parent_id == id {
                    let def = generate_function_inner(child_id, only_declare, context)?;
                    fs.push(def);
                }
            }
        }

        Ok(fs)
    } else {
        let def = generate_function_inner(id, only_declare, context)?;
        Ok(Vec::from([def]))
    }
}

/// Generate HLSL for a function
fn generate_function_inner(
    id: ir::FunctionId,
    only_declare: bool,
    context: &mut GenerateContext,
) -> Result<ast::FunctionDefinition, GenerateError> {
    let sig = context.module.function_registry.get_function_signature(id);
    let decl = context
        .module
        .function_registry
        .get_function_implementation(id)
        .as_ref()
        .unwrap();

    let mut attributes = Vec::new();
    for attribute in &decl.attributes {
        attributes.push(generate_function_attribute(attribute, context)?);
    }

    // Emit template parameters around template instantiations
    // This ensures we also export the full type list at call sites
    // For the moment we never refer to these parameters in the body
    // We may need to if we want to use types that must be defined later
    let mut template_params = Vec::new();
    if !sig.template_params.is_empty() {
        let instantiation_data = context
            .module
            .function_registry
            .get_template_instantiation_data(id)
            .as_ref()
            .unwrap();
        assert_eq!(
            instantiation_data.template_args.len(),
            sig.template_params.len()
        );

        for i in 0..sig.template_params.len() {
            let arg = &instantiation_data.template_args[i];
            match arg {
                ir::TypeOrConstant::Type(id) => {
                    // Undefined or incomplete types may fail as we are now no longer a template
                    // As an unreliable hack keep the template type name with the name of the replaced type
                    // This ensures when we export the real type name within the instantiation it will get tricked into being the unknown template type
                    // As we provide the real type at the call site this will get evaluated later
                    let layer = context.module.type_registry.get_type_layer(*id);
                    let name = if let ir::TypeLayer::Struct(id) = layer {
                        let left_name = context.get_struct_name(id).unwrap();
                        Some(Located::none(left_name.to_string()))
                    } else {
                        None
                    };

                    template_params.push(ast::TemplateParam::Type(ast::TemplateTypeParam {
                        name,
                        default: None,
                    }));
                }
                ir::TypeOrConstant::Constant(c) => {
                    let value_type_name = match c {
                        ir::RestrictedConstant::Bool(_) => "bool",
                        // TODO: Literal types aren't meant to be used for template instantiations but the typer doesn't cast types correctly when resolving templates
                        ir::RestrictedConstant::IntLiteral(_) => "int",
                        ir::RestrictedConstant::Int32(_) => "int",
                        ir::RestrictedConstant::UInt32(_) => "uint",
                        _ => todo!(
                            "Type for non-type template parameter is not handled: {:?}",
                            c
                        ),
                    };

                    template_params.push(ast::TemplateParam::Value(ast::TemplateValueParam {
                        value_type: ast::Type::trivial(value_type_name),
                        name: None,
                        default: None,
                    }));
                }
            }
        }
    }

    let return_type = generate_type(sig.return_type.return_type, context)?;

    let name = Located::none(context.get_function_name(id)?.to_string());

    let mut params = Vec::new();
    for param in &decl.params {
        params.push(generate_function_param(param, context)?);
    }

    let location_annotations = if let Some(semantic) = &sig.return_type.semantic {
        Vec::from([ast::LocationAnnotation::Semantic(semantic.clone())])
    } else {
        Vec::new()
    };

    let body = if only_declare {
        None
    } else {
        let mut statements = Vec::new();
        for statement in &decl.scope_block.0 {
            statements.push(generate_statement(statement, context)?);
        }
        Some(statements)
    };

    Ok(ast::FunctionDefinition {
        name,
        returntype: ast::FunctionReturn {
            return_type,
            location_annotations,
        },
        template_params: ast::TemplateParamList(template_params),
        params,
        body,
        attributes,
    })
}

/// Generate HLSL for a function attribute
fn generate_function_attribute(
    attr: &ir::FunctionAttribute,
    context: &mut GenerateContext,
) -> Result<ast::Attribute, GenerateError> {
    let ast = match attr {
        ir::FunctionAttribute::NumThreads(x, y, z) => {
            let x = generate_expression(x, context)?;
            let y = generate_expression(y, context)?;
            let z = generate_expression(z, context)?;
            ast::Attribute {
                name: Vec::from([Located::none("numthreads".to_string())]),
                arguments: Vec::from([Located::none(x), Located::none(y), Located::none(z)]),
                two_square_brackets: false,
            }
        }
        ir::FunctionAttribute::MaxVertexCount(count) => {
            let count = generate_expression(count, context)?;
            ast::Attribute {
                name: Vec::from([Located::none("maxvertexcount".to_string())]),
                arguments: Vec::from([Located::none(count)]),
                two_square_brackets: false,
            }
        }
        ir::FunctionAttribute::WaveSize(size) => {
            let size = generate_expression(size, context)?;
            ast::Attribute {
                name: Vec::from([Located::none("WaveSize".to_string())]),
                arguments: Vec::from([Located::none(size)]),
                two_square_brackets: false,
            }
        }
        ir::FunctionAttribute::OutputTopology(s) => ast::Attribute {
            name: Vec::from([Located::none("outputtopology".to_string())]),
            arguments: Vec::from([Located::none(ast::Expression::Literal(
                ast::Literal::String(s.clone()),
            ))]),
            two_square_brackets: false,
        },
    };
    Ok(ast)
}

/// Generate HLSL for a function parameter
fn generate_function_param(
    param: &ir::FunctionParam,
    context: &mut GenerateContext,
) -> Result<ast::FunctionParam, GenerateError> {
    let input_modifier = match param.param_type.input_modifier {
        // payload modified parameters require an explicit in instead of an implicit in
        ir::InputModifier::In
            if param.interpolation_modifier == Some(ir::InterpolationModifier::Payload) =>
        {
            Some(ast::TypeModifier::In)
        }
        ir::InputModifier::In => None,
        ir::InputModifier::Out => Some(ast::TypeModifier::Out),
        ir::InputModifier::InOut => Some(ast::TypeModifier::InOut),
    };

    let precise_modifier = if param.precise {
        Some(ast::TypeModifier::Precise)
    } else {
        None
    };

    let interpolation_modifier = generate_interpolation_modifier(&param.interpolation_modifier)?;

    let name = context.get_variable_name(param.id)?.to_string();
    let (base_type, declarator) =
        generate_type_and_declarator(param.param_type.type_id, &name, false, context)?;

    let type_with_interp = prepend_modifiers(base_type, &interpolation_modifier);
    let param_type = prepend_modifiers(type_with_interp, &[input_modifier, precise_modifier]);

    let location_annotations = if let Some(semantic) = &param.semantic {
        Vec::from([ast::LocationAnnotation::Semantic(semantic.clone())])
    } else {
        Vec::new()
    };

    let default_expr = if let Some(default_expr) = &param.default_expr {
        Some(generate_expression(default_expr, context)?)
    } else {
        None
    };

    Ok(ast::FunctionParam {
        param_type,
        declarator,
        location_annotations,
        default_expr,
    })
}

/// Generate HLSL type modifier for an interpolation modifier
fn generate_interpolation_modifier(
    interpolation_modifier: &Option<ir::InterpolationModifier>,
) -> Result<Vec<Option<ast::TypeModifier>>, GenerateError> {
    if let Some(interpolation_modifier) = &interpolation_modifier {
        let modifiers = match interpolation_modifier {
            ir::InterpolationModifier::CenterPerspective => &[ast::TypeModifier::Linear],
            ir::InterpolationModifier::CentroidPerspective => &[ast::TypeModifier::Centroid],
            ir::InterpolationModifier::SamplePerspective => &[ast::TypeModifier::Sample],
            ir::InterpolationModifier::CenterNoPerspective => &[ast::TypeModifier::NoPerspective],
            ir::InterpolationModifier::CentroidNoPerspective => [
                ast::TypeModifier::Centroid,
                ast::TypeModifier::NoPerspective,
            ]
            .as_slice(),
            ir::InterpolationModifier::SampleNoPerspective => {
                &[ast::TypeModifier::Sample, ast::TypeModifier::NoPerspective]
            }
            ir::InterpolationModifier::Flat => &[ast::TypeModifier::NoInterpolation],
            ir::InterpolationModifier::Point => &[ast::TypeModifier::Point],
            ir::InterpolationModifier::Line => &[ast::TypeModifier::Line],
            ir::InterpolationModifier::Triangle => &[ast::TypeModifier::Triangle],
            ir::InterpolationModifier::LineAdj => &[ast::TypeModifier::LineAdj],
            ir::InterpolationModifier::TriangleAdj => &[ast::TypeModifier::TriangleAdj],
            ir::InterpolationModifier::Vertices => &[ast::TypeModifier::Vertices],
            ir::InterpolationModifier::Primitives => &[ast::TypeModifier::Primitives],
            ir::InterpolationModifier::Indices => &[ast::TypeModifier::Indices],
            ir::InterpolationModifier::Payload => &[ast::TypeModifier::Payload],
        };
        Ok(modifiers.iter().cloned().map(Some).collect::<Vec<_>>())
    } else {
        Ok(Vec::new())
    }
}

/// Generate HLSL type name
fn generate_type(
    ty: ir::TypeId,
    context: &mut GenerateContext,
) -> Result<ast::Type, GenerateError> {
    let base_declarator = ast::Declarator::Empty;
    let (base, declarator) = generate_type_impl(ty, base_declarator, false, context)?;
    if declarator != ast::Declarator::Empty {
        return Err(GenerateError::ComplexTypeBind);
    }
    Ok(base)
}

/// Generate base type and declarator from a type for a declaration
fn generate_type_and_declarator(
    ty: ir::TypeId,
    name: &str,
    suppress_const_volatile: bool,
    context: &mut GenerateContext,
) -> Result<(ast::Type, ast::Declarator), GenerateError> {
    let base_declarator =
        ast::Declarator::Identifier(ast::ScopedIdentifier::trivial(name), Vec::new());

    let (base, declarator) =
        generate_type_impl(ty, base_declarator, suppress_const_volatile, context)?;

    Ok((base, declarator))
}

/// Internal type generation
fn generate_type_impl(
    ty: ir::TypeId,
    mut declarator: ast::Declarator,
    suppress_const_volatile: bool,
    context: &mut GenerateContext,
) -> Result<(ast::Type, ast::Declarator), GenerateError> {
    let tyl = context.module.type_registry.get_type_layer(ty);
    let base = match tyl {
        ir::TypeLayer::Void => ast::Type::trivial("void"),
        ir::TypeLayer::Scalar(st) => generate_scalar_type(st)?,
        ir::TypeLayer::Vector(st, x) => {
            // Allowed types in a vector should construct valid vector type names
            // This will break down for vector of enums
            let (mut base, inner_declarator) = generate_type_impl(st, declarator, false, context)?;
            declarator = inner_declarator;
            assert!(base.layout.0.identifiers.len() == 1);
            base.layout.0.identifiers[0].node += &format!("{x}");
            base
        }
        ir::TypeLayer::Matrix(st, x, y) => {
            // Allowed types in a matrix should construct valid matrix type names
            // This will break down for vector of enums
            let (mut base, inner_declarator) = generate_type_impl(st, declarator, false, context)?;
            declarator = inner_declarator;
            assert!(base.layout.0.identifiers.len() == 1);
            base.layout.0.identifiers[0].node += &format!("{x}x{y}");
            base
        }
        ir::TypeLayer::Struct(id) => {
            let scoped_name = context.get_struct_name_full(id)?;
            ast::Type::from_layout(ast::TypeLayout(
                scoped_name_to_identifier(scoped_name),
                Default::default(),
            ))
        }
        ir::TypeLayer::Enum(id) => {
            let scoped_name = context.get_enum_name_full(id)?;
            ast::Type::from_layout(ast::TypeLayout(
                scoped_name_to_identifier(scoped_name),
                Default::default(),
            ))
        }
        ir::TypeLayer::Object(ot) => {
            fn build_single_param(
                name: &str,
                ty: ir::TypeId,
                context: &mut GenerateContext,
            ) -> Result<ast::Type, GenerateError> {
                Ok(ast::Type::with_template_types(
                    Located::none(name),
                    &[ast::ExpressionOrType::Type(generate_type(ty, context)?)],
                ))
            }

            use ir::ObjectType::*;
            match ot {
                Buffer(ty) => build_single_param("Buffer", ty, context)?,
                RWBuffer(ty) => build_single_param("RWBuffer", ty, context)?,
                ByteAddressBuffer => ast::Type::trivial("ByteAddressBuffer"),
                RWByteAddressBuffer => ast::Type::trivial("RWByteAddressBuffer"),
                BufferAddress | RWBufferAddress if context.module.flags.requires_buffer_address => {
                    ast::Type::trivial("uint64_t")
                }
                BufferAddress => ast::Type::trivial("ByteAddressBuffer"),
                RWBufferAddress => ast::Type::trivial("RWByteAddressBuffer"),
                StructuredBuffer(ty) => build_single_param("StructuredBuffer", ty, context)?,
                RWStructuredBuffer(ty) => build_single_param("RWStructuredBuffer", ty, context)?,

                Texture2D(ty) => build_single_param("Texture2D", ty, context)?,
                Texture2DMips(_) | Texture2DMipsSlice(_) => {
                    // We do not expect intermediate values for mips to get exported
                    // It's possible to make shaders in HLSL that reference these but these are all silly use cases
                    // Trying to use mips-slice will likely break HLSL anyway as you can't make intermediates of these
                    panic!("trying to export Texture2D.mips intermediates");
                }

                Texture2DArray(ty) => build_single_param("Texture2DArray", ty, context)?,
                Texture2DArrayMips(_) | Texture2DArrayMipsSlice(_) => {
                    // We do not expect intermediate values for mips to get exported
                    // It's possible to make shaders in HLSL that reference these but these are all silly use cases
                    // Trying to use mips-slice will likely break HLSL anyway as you can't make intermediates of these
                    panic!("trying to export Texture2DArray.mips intermediates");
                }

                RWTexture2D(ty) => build_single_param("RWTexture2D", ty, context)?,

                RWTexture2DArray(ty) => build_single_param("RWTexture2DArray", ty, context)?,

                TextureCube(ty) => build_single_param("TextureCube", ty, context)?,

                TextureCubeArray(ty) => build_single_param("TextureCubeArray", ty, context)?,

                Texture3D(ty) => build_single_param("Texture3D", ty, context)?,
                Texture3DMips(_) | Texture3DMipsSlice(_) => {
                    panic!("trying to export Texture3D.mips intermediates");
                }
                RWTexture3D(ty) => build_single_param("RWTexture3D", ty, context)?,

                ConstantBuffer(ty) => build_single_param("ConstantBuffer", ty, context)?,
                SamplerState => ast::Type::trivial("SamplerState"),
                SamplerComparisonState => ast::Type::trivial("SamplerComparisonState"),

                TriangleStream(ty) => build_single_param("TriangleStream", ty, context)?,

                RaytracingAccelerationStructure => {
                    ast::Type::trivial("RaytracingAccelerationStructure")
                }
                RayQuery(v) => ast::Type::with_template_types(
                    Located::none("RayQuery"),
                    &[ast::ExpressionOrType::Expression(Located::none(
                        ast::Expression::Literal(ast::Literal::IntUntyped(v as u64)),
                    ))],
                ),
                RayDesc => ast::Type::trivial("RayDesc"),
            }
        }
        ir::TypeLayer::Array(ty, len) => {
            let array_size = len
                .map(ast::Literal::IntUntyped)
                .map(ast::Expression::Literal)
                .map(Located::none);

            declarator = ast::Declarator::Array(ast::ArrayDeclarator {
                inner: Box::new(declarator),
                array_size,
                attributes: Vec::new(),
            });

            let (base, inner_declarator) =
                generate_type_impl(ty, declarator, suppress_const_volatile, context)?;
            declarator = inner_declarator;

            base
        }
        ir::TypeLayer::Modifier(mut modifier, ty) => {
            if suppress_const_volatile {
                modifier.is_const = false;
                modifier.volatile = false;
            }

            let (mut base, inner_declarator) = generate_type_impl(ty, declarator, false, context)?;

            // Array layers can be inside modifiers - when we export the modifier ends up with the type
            // The language does not distinguish between a "const array T" vs an "array const T"
            declarator = inner_declarator;

            let mut modifiers = Vec::new();
            if modifier.row_major {
                modifiers.push(ast::TypeModifier::RowMajor);
            }
            if modifier.column_major {
                modifiers.push(ast::TypeModifier::ColumnMajor);
            }
            if modifier.unorm {
                modifiers.push(ast::TypeModifier::Unorm);
            }
            if modifier.snorm {
                modifiers.push(ast::TypeModifier::Snorm);
            }
            if modifier.is_const {
                modifiers.push(ast::TypeModifier::Const);
            }
            if modifier.volatile {
                modifiers.push(ast::TypeModifier::Volatile);
            }

            if !modifiers.is_empty() {
                for modifier in modifiers.iter().rev() {
                    base.modifiers.prepend(Located::none(*modifier));
                }
            }

            base
        }
        _ => todo!("Type layout not implemented: {:?}", tyl),
    };

    Ok((base, declarator))
}

/// Generate HLSL type for a scalar type
fn generate_scalar_type(ty: ir::ScalarType) -> Result<ast::Type, GenerateError> {
    let name = match ty {
        ir::ScalarType::Bool => "bool",
        ir::ScalarType::IntLiteral => panic!("int literal should not be required on output"),
        ir::ScalarType::Int32 => "int",
        ir::ScalarType::UInt32 => "uint",
        ir::ScalarType::FloatLiteral => panic!("float literal should not be required on output"),
        ir::ScalarType::Float16 => "half",
        ir::ScalarType::Float32 => "float",
        ir::ScalarType::Float64 => "double",
    };

    Ok(ast::Type::trivial(name))
}

/// Generate HLSL expression or type from a constant value or type
fn generate_type_or_constant(
    tc: &ir::TypeOrConstant,
    context: &mut GenerateContext,
) -> Result<ast::ExpressionOrType, GenerateError> {
    match tc {
        ir::TypeOrConstant::Type(ty) => {
            let ty = generate_type(*ty, context)?;
            Ok(ast::ExpressionOrType::Type(ty))
        }
        ir::TypeOrConstant::Constant(c) => {
            let expr = generate_literal(&c.clone().unrestrict(), context)?;
            Ok(ast::ExpressionOrType::Expression(Located::none(expr)))
        }
    }
}

/// Generate HLSL expression from a constant value
fn generate_literal(
    literal: &ir::Constant,
    context: &mut GenerateContext,
) -> Result<ast::Expression, GenerateError> {
    let lit = match *literal {
        ir::Constant::Bool(v) => ast::Literal::Bool(v),
        ir::Constant::IntLiteral(v) if v < 0 && -v <= u64::MAX as i128 => {
            return Ok(ast::Expression::UnaryOperation(
                ast::UnaryOp::Minus,
                Box::new(Located::none(ast::Expression::Literal(
                    ast::Literal::IntUntyped(-v as u64),
                ))),
            ))
        }
        ir::Constant::IntLiteral(v) if v >= 0 && v <= u64::MAX as i128 => {
            ast::Literal::IntUntyped(v as u64)
        }
        ir::Constant::IntLiteral(_) => panic!("cannot represent {literal:?}"),
        ir::Constant::Int32(v) if v < 0 => {
            return Ok(ast::Expression::UnaryOperation(
                ast::UnaryOp::Minus,
                Box::new(Located::none(ast::Expression::Literal(
                    ast::Literal::IntUntyped(-v as u64),
                ))),
            ));
        }
        ir::Constant::Int32(v) => ast::Literal::IntUntyped(v as u64),
        ir::Constant::UInt32(v) => ast::Literal::IntUnsigned32(u64::from(v)),
        ir::Constant::Int64(v) => ast::Literal::IntSigned64(v),
        ir::Constant::UInt64(v) => ast::Literal::IntUnsigned64(v),
        ir::Constant::FloatLiteral(v) => ast::Literal::FloatUntyped(v),
        ir::Constant::Float16(v) => ast::Literal::Float16(v),
        ir::Constant::Float32(v) => ast::Literal::Float32(v),
        ir::Constant::Float64(v) => ast::Literal::Float64(v),
        ir::Constant::String(_) => panic!("literal string not expected in output"),
        ir::Constant::Enum(id, ref c) => {
            // Try to find an enum value which matches the constant
            let mut found_value_id = None;
            for value_id in context.module.enum_registry.get_values(id) {
                if context.module.enum_registry.get_enum_value(*value_id).value == **c {
                    found_value_id = Some(value_id);
                    break;
                }
            }

            match found_value_id {
                Some(found_value_id) => {
                    // Output with the declared name of the enum value
                    let scoped_name = context.get_enum_value_name_full(*found_value_id)?;
                    let identifier = scoped_name_to_identifier(scoped_name);
                    return Ok(ast::Expression::Identifier(identifier));
                }
                None => {
                    // Output as a raw value casted to the enum
                    let enum_type = {
                        let scoped_name = context.get_enum_name_full(id)?;
                        let identifier = scoped_name_to_identifier(scoped_name);

                        ast::Type::from_layout(ast::TypeLayout(identifier, Default::default()))
                    };

                    let literal_expr = {
                        let literal_expr = generate_literal(c, context)?;
                        Box::new(Located::none(literal_expr))
                    };

                    return Ok(ast::Expression::Cast(enum_type, literal_expr));
                }
            }
        }
    };
    Ok(ast::Expression::Literal(lit))
}

/// Generate HLSL statement
fn generate_statement(
    statement: &ir::Statement,
    context: &mut GenerateContext,
) -> Result<ast::Statement, GenerateError> {
    let mut attributes = Vec::new();
    for attribute in &statement.attributes {
        attributes.push(generate_statement_attribute(attribute, context)?);
    }

    let kind = match &statement.kind {
        ir::StatementKind::Expression(expr) => {
            let expr = generate_expression(expr, context)?;
            ast::StatementKind::Expression(expr)
        }
        ir::StatementKind::Var(def) => {
            let def = generate_variable_definition(def, context)?;
            ast::StatementKind::Var(def)
        }
        ir::StatementKind::Block(block) => {
            let statements = generate_scope_block(block, context)?;
            ast::StatementKind::Block(statements)
        }
        ir::StatementKind::If(cond, block) => {
            let cond = generate_expression(cond, context)?;
            let block = generate_scope_block(block, context)?;
            let cond = Located::none(cond);
            let block = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::If(cond, block)
        }
        ir::StatementKind::IfElse(cond, block_true, block_false) => {
            let cond = generate_expression(cond, context)?;
            let block_true = generate_scope_block(block_true, context)?;
            let block_false = generate_scope_block(block_false, context)?;
            let cond = Located::none(cond);
            let block_true = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block_true),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            let block_false = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block_false),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::IfElse(cond, block_true, block_false)
        }
        ir::StatementKind::For(init, cond, inc, block) => {
            let init = generate_for_init(init, context)?;
            let cond = match cond {
                Some(cond) => Some(Located::none(generate_expression(cond, context)?)),
                None => None,
            };
            let inc = match inc {
                Some(inc) => Some(Located::none(generate_expression(inc, context)?)),
                None => None,
            };
            let block = generate_scope_block(block, context)?;
            let block = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::For(init, cond, inc, block)
        }
        ir::StatementKind::While(cond, block) => {
            let cond = generate_expression(cond, context)?;
            let block = generate_scope_block(block, context)?;
            let cond = Located::none(cond);
            let block = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::While(cond, block)
        }
        ir::StatementKind::DoWhile(block, cond) => {
            let block = generate_scope_block(block, context)?;
            let cond = generate_expression(cond, context)?;
            let cond = Located::none(cond);
            let block = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::DoWhile(block, cond)
        }
        ir::StatementKind::Switch(cond, block) => {
            let cond = generate_expression(cond, context)?;
            let block = generate_scope_block(block, context)?;
            let cond = Located::none(cond);
            let block = Box::new(ast::Statement {
                kind: ast::StatementKind::Block(block),
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::Switch(cond, block)
        }
        ir::StatementKind::Break => ast::StatementKind::Break,
        ir::StatementKind::Continue => ast::StatementKind::Continue,
        ir::StatementKind::Discard => ast::StatementKind::Discard,
        ir::StatementKind::Return(expr_opt) => {
            if let Some(expr) = expr_opt {
                let expr = generate_expression(expr, context)?;
                ast::StatementKind::Return(Some(Located::none(expr)))
            } else {
                ast::StatementKind::Return(None)
            }
        }
        ir::StatementKind::CaseLabel(value) => {
            let expr = generate_literal(value, context)?;
            // We use an empty statement as the syntax requires a statement after a label
            // This is part of the label with the current AST design - while IR splits them out
            // We remove these in generate_scope_block after processing the next statement
            let empty_statement = Box::new(ast::Statement {
                kind: ast::StatementKind::Empty,
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::CaseLabel(Located::none(expr), empty_statement)
        }
        ir::StatementKind::DefaultLabel => {
            let empty_statement = Box::new(ast::Statement {
                kind: ast::StatementKind::Empty,
                location: SourceLocation::UNKNOWN,
                attributes: Vec::new(),
            });
            ast::StatementKind::DefaultLabel(empty_statement)
        }
    };

    Ok(ast::Statement {
        kind,
        location: SourceLocation::UNKNOWN,
        attributes,
    })
}

/// Generate HLSL attribute for a statement
fn generate_statement_attribute(
    attribute: &ir::StatementAttribute,
    _: &mut GenerateContext,
) -> Result<ast::Attribute, GenerateError> {
    let ast = match attribute {
        ir::StatementAttribute::Branch => ast::Attribute {
            name: Vec::from([Located::none("branch".to_string())]),
            arguments: Vec::new(),
            two_square_brackets: false,
        },
        ir::StatementAttribute::Flatten => ast::Attribute {
            name: Vec::from([Located::none("flatten".to_string())]),
            arguments: Vec::new(),
            two_square_brackets: false,
        },
        ir::StatementAttribute::Unroll(None) => ast::Attribute {
            name: Vec::from([Located::none("unroll".to_string())]),
            arguments: Vec::new(),
            two_square_brackets: false,
        },
        ir::StatementAttribute::Unroll(Some(v)) => ast::Attribute {
            name: Vec::from([Located::none("unroll".to_string())]),
            arguments: Vec::from([Located::none(ast::Expression::Literal(
                ast::Literal::IntUntyped(*v),
            ))]),
            two_square_brackets: false,
        },
        ir::StatementAttribute::Loop => ast::Attribute {
            name: Vec::from([Located::none("loop".to_string())]),
            arguments: Vec::new(),
            two_square_brackets: false,
        },
        ir::StatementAttribute::Fastopt => ast::Attribute {
            name: Vec::from([Located::none("fastopt".to_string())]),
            arguments: Vec::new(),
            two_square_brackets: false,
        },
        ir::StatementAttribute::AllowUavCondition => ast::Attribute {
            name: Vec::from([Located::none("allow_uav_condition".to_string())]),
            arguments: Vec::new(),
            two_square_brackets: false,
        },
    };
    Ok(ast)
}

/// Generate HLSL variable definition
fn generate_variable_definition(
    def: &ir::VarDef,
    context: &mut GenerateContext,
) -> Result<ast::VarDef, GenerateError> {
    let var_def = context.module.variable_registry.get_local_variable(def.id);

    let storage_modifier = match var_def.storage_class {
        ir::LocalStorage::Local => None,
        ir::LocalStorage::Static => Some(ast::TypeModifier::Static),
    };

    let precise_modifier = if var_def.precise {
        Some(ast::TypeModifier::Precise)
    } else {
        None
    };

    let name = context.get_variable_name(def.id)?.to_string();
    let (base, declarator) = generate_type_and_declarator(var_def.type_id, &name, false, context)?;

    let local_type = prepend_modifiers(base, &[storage_modifier, precise_modifier]);

    let init = generate_initializer(&def.init, context)?;

    let init_declarator = ast::InitDeclarator {
        declarator,
        location_annotations: Vec::new(),
        init,
    };

    let def = ast::VarDef {
        local_type,
        defs: Vec::from([init_declarator]),
    };

    Ok(def)
}

/// Generate HLSL for a block of statements
fn generate_scope_block(
    block: &ir::ScopeBlock,
    context: &mut GenerateContext,
) -> Result<Vec<ast::Statement>, GenerateError> {
    let mut statements = Vec::new();
    for statement in &block.0 {
        let statement = generate_statement(statement, context)?;

        // If the previous statement was a case label that has no contents
        // then insert the new statement into its contents
        if let Some(ast::Statement {
            kind:
                ast::StatementKind::CaseLabel(_, current) | ast::StatementKind::DefaultLabel(current),
            ..
        }) = statements.last_mut()
        {
            if let ast::Statement {
                kind: ast::StatementKind::Empty,
                ..
            } = **current
            {
                *current = Box::new(statement);
                continue;
            }
        }

        statements.push(statement);
    }
    Ok(statements)
}

/// Generate HLSL for a for init expression
fn generate_for_init(
    init: &ir::ForInit,
    context: &mut GenerateContext,
) -> Result<ast::InitStatement, GenerateError> {
    let ast = match init {
        ir::ForInit::Empty => ast::InitStatement::Empty,
        ir::ForInit::Expression(expr) => {
            ast::InitStatement::Expression(Located::none(generate_expression(expr, context)?))
        }
        ir::ForInit::Definitions(defs) => {
            let (head, tail) = defs.split_first().unwrap();

            // Construct ast for the first entry first
            let mut ast = generate_variable_definition(head, context)?;

            // There should only be one entry to start with
            assert_eq!(ast.defs.len(), 1);

            for def in tail {
                // Construct ast for the non-first entry
                let mut tail_ast = generate_variable_definition(def, context)?;

                // The base type definitions should all match
                assert_eq!(ast.local_type, tail_ast.local_type);

                // There should only be one entry
                assert_eq!(tail_ast.defs.len(), 1);

                ast.defs.append(&mut tail_ast.defs);
            }

            ast::InitStatement::Declaration(ast)
        }
    };
    Ok(ast)
}

/// Generate HLSL for an expression
fn generate_expression(
    expr: &ir::Expression,
    context: &mut GenerateContext,
) -> Result<ast::Expression, GenerateError> {
    let expr = match expr {
        ir::Expression::Literal(lit) => generate_literal(lit, context)?,
        ir::Expression::Variable(v) => ast::Expression::Identifier(ast::ScopedIdentifier::trivial(
            context.get_variable_name(*v)?,
        )),
        ir::Expression::MemberVariable(id, member_index) => {
            let member_def =
                &context.module.struct_registry[id.0 as usize].members[*member_index as usize];
            ast::Expression::Identifier(ast::ScopedIdentifier::trivial(&member_def.name))
        }
        ir::Expression::Global(v) => ast::Expression::Identifier(ast::ScopedIdentifier::trivial(
            context.get_global_name(*v)?,
        )),
        ir::Expression::ConstantVariable(id) => {
            let def = &context.module.cbuffer_registry[id.0 .0 as usize].members[id.1 as usize];
            ast::Expression::Identifier(ast::ScopedIdentifier::trivial(&def.name))
        }
        ir::Expression::EnumValue(id) => ast::Expression::Identifier(scoped_name_to_identifier(
            context.get_enum_value_name_full(*id)?,
        )),
        ir::Expression::TernaryConditional(expr_cond, expr_true, expr_false) => {
            let expr_cond = generate_expression(expr_cond, context)?;
            let expr_true = generate_expression(expr_true, context)?;
            let expr_false = generate_expression(expr_false, context)?;
            let expr_cond = Box::new(Located::none(expr_cond));
            let expr_true = Box::new(Located::none(expr_true));
            let expr_false = Box::new(Located::none(expr_false));
            ast::Expression::TernaryConditional(expr_cond, expr_true, expr_false)
        }
        ir::Expression::Sequence(exprs) => {
            // We must have at least 1 entry for this logic - but expect at least 2 for a valid module
            assert!(exprs.len() >= 2);
            let (last, front) = exprs.split_last().unwrap();
            let mut end = generate_expression(last, context)?;
            for expr in front.iter().rev() {
                let expr = generate_expression(expr, context)?;
                end = ast::Expression::BinaryOperation(
                    ast::BinOp::Sequence,
                    Box::new(Located::none(expr)),
                    Box::new(Located::none(end)),
                )
            }
            end
        }
        ir::Expression::Swizzle(expr_object, swizzle) => {
            let object = generate_expression(expr_object, context)?;
            let member = {
                let mut member = String::new();
                for channel in swizzle {
                    match channel {
                        ir::SwizzleSlot::X => member.push('x'),
                        ir::SwizzleSlot::Y => member.push('y'),
                        ir::SwizzleSlot::Z => member.push('z'),
                        ir::SwizzleSlot::W => member.push('w'),
                    }
                }
                ast::ScopedIdentifier::trivial(&member)
            };
            ast::Expression::Member(Box::new(Located::none(object)), member)
        }
        ir::Expression::MatrixSwizzle(expr_object, swizzle) => {
            let object = generate_expression(expr_object, context)?;
            let member = {
                let mut member = String::new();
                for channel in swizzle {
                    member.push_str("_m");
                    for dim in [channel.0, channel.1] {
                        match dim {
                            ir::ComponentIndex::First => member.push('0'),
                            ir::ComponentIndex::Second => member.push('1'),
                            ir::ComponentIndex::Third => member.push('2'),
                            ir::ComponentIndex::Forth => member.push('3'),
                        }
                    }
                }
                ast::ScopedIdentifier::trivial(&member)
            };
            ast::Expression::Member(Box::new(Located::none(object)), member)
        }
        ir::Expression::ArraySubscript(expr_object, expr_index) => {
            let object = generate_expression(expr_object, context)?;
            let index = generate_expression(expr_index, context)?;
            let object = Box::new(Located::none(object));
            let index = Box::new(Located::none(index));
            ast::Expression::ArraySubscript(object, index)
        }
        ir::Expression::Constructor(type_id, args) => {
            let ty = generate_type(*type_id, context)?;
            assert!(ty.modifiers.modifiers.is_empty());
            let name = ast::Expression::Identifier(ty.layout.0);
            let name = Box::new(Located::none(name));
            let mut ast_args = Vec::new();
            for slot in args {
                ast_args.push(Located::none(generate_expression(&slot.expr, context)?));
            }
            ast::Expression::Call(name, ty.layout.1.to_vec(), ast_args)
        }
        ir::Expression::Cast(type_id, expr) => {
            // Check if we are casting to a literal type
            // We can not emits such a cast as the type can not be named
            // These occur only where they would get implicitly converted so we can drop them
            let unmod_id = context.module.type_registry.remove_modifier(*type_id);
            let to_literal = matches!(
                context.module.type_registry.get_type_layer(unmod_id),
                ir::TypeLayer::Scalar(ir::ScalarType::IntLiteral)
                    | ir::TypeLayer::Scalar(ir::ScalarType::FloatLiteral)
            );

            let inner = generate_expression(expr, context)?;

            if !to_literal {
                let ty = generate_type(*type_id, context)?;
                ast::Expression::Cast(ty, Box::new(Located::none(inner)))
            } else {
                inner
            }
        }
        ir::Expression::SizeOf(type_id) => {
            let ty = generate_type(*type_id, context)?;
            ast::Expression::SizeOf(Box::new(ast::ExpressionOrType::Type(ty)))
        }
        ir::Expression::StructMember(expr, id, member_index) => {
            let member_def =
                &context.module.struct_registry[id.0 as usize].members[*member_index as usize];
            let object = generate_expression(expr, context)?;
            let object = Box::new(Located::none(object));
            ast::Expression::Member(object, ast::ScopedIdentifier::trivial(&member_def.name))
        }
        ir::Expression::ObjectMember(expr, name) => {
            let object = generate_expression(expr, context)?;
            let object = Box::new(Located::none(object));
            ast::Expression::Member(object, ast::ScopedIdentifier::trivial(name))
        }
        ir::Expression::Call(id, ct, exprs) => {
            let tys = if let Some(template_instantiation_data) = context
                .module
                .function_registry
                .get_template_instantiation_data(*id)
            {
                template_instantiation_data.template_args.as_slice()
            } else {
                &[]
            };

            if let Some(intrinsic) = context.module.function_registry.get_intrinsic_data(*id) {
                generate_intrinsic_function(intrinsic, tys, exprs, context)?
            } else {
                generate_user_call(*id, ct, tys, exprs, context)?
            }
        }
        ir::Expression::IntrinsicOp(intrinsic, exprs) => {
            generate_intrinsic_op(intrinsic, exprs, context)?
        }
    };
    Ok(expr)
}

/// Write out a call expression for a user function
fn generate_user_call(
    id: ir::FunctionId,
    ct: &ir::CallType,
    tys: &[ir::TypeOrConstant],
    exprs: &Vec<ir::Expression>,
    context: &mut GenerateContext,
) -> Result<ast::Expression, GenerateError> {
    let (object, arguments) = match ct {
        ir::CallType::FreeFunction => {
            let scoped_name = context.get_function_name_full(id)?;
            let object = ast::Expression::Identifier(scoped_name_to_identifier(scoped_name));
            (object, exprs.as_slice())
        }
        ir::CallType::MethodExternal => {
            let leaf_name = ast::ScopedIdentifier::trivial(context.get_function_name(id)?);
            let object = generate_expression(&exprs[0], context)?;
            let method = ast::Expression::Member(Box::new(Located::none(object)), leaf_name);
            (method, &exprs[1..])
        }
        ir::CallType::MethodInternal => {
            // Assume all method calls (both on an object and internally) have
            // sufficient qualification that they do not need the full name
            let leaf_name = context.get_function_name(id)?;
            let object = ast::Expression::Identifier(ast::ScopedIdentifier::trivial(leaf_name));
            (object, exprs.as_slice())
        }
    };

    let type_args = generate_template_type_args(tys, context)?;
    let args = generate_invocation_args(arguments, context)?;
    let expr = ast::Expression::Call(Box::new(Located::none(object)), type_args, args);
    Ok(expr)
}

/// Write out an intrinsic function expression
fn generate_intrinsic_function(
    intrinsic: &ir::Intrinsic,
    tys: &[ir::TypeOrConstant],
    exprs: &Vec<ir::Expression>,
    context: &mut GenerateContext,
) -> Result<ast::Expression, GenerateError> {
    enum Form {
        Invoke(&'static str),
        Method(&'static str),
        AddressMethod(&'static str, &'static str),
    }

    use ir::Intrinsic::*;
    let form = match &intrinsic {
        AllMemoryBarrier => Form::Invoke("AllMemoryBarrier"),
        AllMemoryBarrierWithGroupSync => Form::Invoke("AllMemoryBarrierWithGroupSync"),
        DeviceMemoryBarrier => Form::Invoke("DeviceMemoryBarrier"),
        DeviceMemoryBarrierWithGroupSync => Form::Invoke("DeviceMemoryBarrierWithGroupSync"),
        GroupMemoryBarrier => Form::Invoke("GroupMemoryBarrier"),
        GroupMemoryBarrierWithGroupSync => Form::Invoke("GroupMemoryBarrierWithGroupSync"),

        AsInt => Form::Invoke("asint"),
        AsUInt => Form::Invoke("asuint"),
        AsFloat => Form::Invoke("asfloat"),
        AsDouble => Form::Invoke("asdouble"),

        All => Form::Invoke("all"),
        Any => Form::Invoke("any"),
        And => Form::Invoke("and"),
        Or => Form::Invoke("or"),
        Select => Form::Invoke("select"),

        Abs => Form::Invoke("abs"),

        // Transcendental functions
        Acos => Form::Invoke("acos"),
        Asin => Form::Invoke("asin"),
        Atan => Form::Invoke("atan"),
        Atan2 => Form::Invoke("atan2"),
        Cos => Form::Invoke("cos"),
        Cosh => Form::Invoke("cosh"),
        Sin => Form::Invoke("sin"),
        Sinh => Form::Invoke("sinh"),
        Sincos => Form::Invoke("sincos"),
        Tan => Form::Invoke("tan"),
        Tanh => Form::Invoke("tanh"),
        Sqrt => Form::Invoke("sqrt"),
        RcpSqrt => Form::Invoke("rsqrt"),
        Pow => Form::Invoke("pow"),
        Exp => Form::Invoke("exp"),
        Exp2 => Form::Invoke("exp2"),
        Log => Form::Invoke("log"),
        Log2 => Form::Invoke("log2"),
        Log10 => Form::Invoke("log10"),

        F16ToF32 => Form::Invoke("f16tof32"),
        F32ToF16 => Form::Invoke("f32tof16"),

        Floor => Form::Invoke("floor"),
        Ceil => Form::Invoke("ceil"),
        Trunc => Form::Invoke("trunc"),
        Round => Form::Invoke("round"),
        Frac => Form::Invoke("frac"),
        Modf => Form::Invoke("modf"),
        Fmod => Form::Invoke("fmod"),

        IsNaN => Form::Invoke("isnan"),
        IsInfinite => Form::Invoke("isinf"),
        IsFinite => Form::Invoke("isfinite"),

        Length => Form::Invoke("length"),
        Normalize => Form::Invoke("normalize"),
        Rcp => Form::Invoke("rcp"),

        Reflect => Form::Invoke("reflect"),
        Refract => Form::Invoke("refract"),

        CountBits => Form::Invoke("countbits"),
        ReverseBits => Form::Invoke("reversebits"),
        FirstBitHigh => Form::Invoke("firstbithigh"),
        FirstBitLow => Form::Invoke("firstbitlow"),

        Saturate => Form::Invoke("saturate"),

        Sign => Form::Invoke("sign"),

        Cross => Form::Invoke("cross"),
        Distance => Form::Invoke("distance"),
        Dot => Form::Invoke("dot"),

        Mul => Form::Invoke("mul"),

        Min => Form::Invoke("min"),
        Max => Form::Invoke("max"),

        Step => Form::Invoke("step"),

        Clamp => Form::Invoke("clamp"),
        Lerp => Form::Invoke("lerp"),
        SmoothStep => Form::Invoke("smoothstep"),

        Transpose => Form::Invoke("transpose"),
        Determinant => Form::Invoke("determinant"),

        DDX => Form::Invoke("ddx"),
        DDXCoarse => Form::Invoke("ddx_coarse"),
        DDXFine => Form::Invoke("ddx_fine"),
        DDY => Form::Invoke("ddy"),
        DDYCoarse => Form::Invoke("ddy_coarse"),
        DDYFine => Form::Invoke("ddy_fine"),

        InterlockedAdd => Form::Invoke("InterlockedAdd"),
        InterlockedAnd => Form::Invoke("InterlockedAnd"),
        InterlockedCompareExchange => Form::Invoke("InterlockedCompareExchange"),
        InterlockedCompareStore => Form::Invoke("InterlockedCompareStore"),
        InterlockedExchange => Form::Invoke("InterlockedExchange"),
        InterlockedMax => Form::Invoke("InterlockedMax"),
        InterlockedMin => Form::Invoke("InterlockedMin"),
        InterlockedOr => Form::Invoke("InterlockedOr"),
        InterlockedXor => Form::Invoke("InterlockedXor"),

        NonUniformResourceIndex => Form::Invoke("NonUniformResourceIndex"),

        WaveGetLaneCount => Form::Invoke("WaveGetLaneCount"),
        WaveGetLaneIndex => Form::Invoke("WaveGetLaneIndex"),
        WaveIsFirstLane => Form::Invoke("WaveIsFirstLane"),
        WaveActiveAnyTrue => Form::Invoke("WaveActiveAnyTrue"),
        WaveActiveAllTrue => Form::Invoke("WaveActiveAllTrue"),
        WaveActiveBallot => Form::Invoke("WaveActiveBallot"),
        WaveReadLaneAt => Form::Invoke("WaveReadLaneAt"),
        WaveReadLaneFirst => Form::Invoke("WaveReadLaneFirst"),
        WaveActiveAllEqual => Form::Invoke("WaveActiveAllEqual"),
        WaveActiveCountBits => Form::Invoke("WaveActiveCountBits"),
        WaveActiveSum => Form::Invoke("WaveActiveSum"),
        WaveActiveProduct => Form::Invoke("WaveActiveProduct"),
        WaveActiveBitAnd => Form::Invoke("WaveActiveBitAnd"),
        WaveActiveBitOr => Form::Invoke("WaveActiveBitOr"),
        WaveActiveBitXor => Form::Invoke("WaveActiveBitXor"),
        WaveActiveMin => Form::Invoke("WaveActiveMin"),
        WaveActiveMax => Form::Invoke("WaveActiveMax"),
        WavePrefixCountBits => Form::Invoke("WavePrefixCountBits"),
        WavePrefixProduct => Form::Invoke("WavePrefixProduct"),
        WavePrefixSum => Form::Invoke("WavePrefixSum"),
        QuadReadAcrossX => Form::Invoke("QuadReadAcrossX"),
        QuadReadAcrossY => Form::Invoke("QuadReadAcrossY"),
        QuadReadAcrossDiagonal => Form::Invoke("QuadReadAcrossDiagonal"),
        QuadReadLaneAt => Form::Invoke("QuadReadLaneAt"),

        SetMeshOutputCounts => Form::Invoke("SetMeshOutputCounts"),
        DispatchMesh => Form::Invoke("DispatchMesh"),

        BufferGetDimensions => Form::Method("GetDimensions"),
        BufferLoad => Form::Method("Load"),

        RWBufferGetDimensions => Form::Method("GetDimensions"),
        RWBufferLoad => Form::Method("Load"),

        StructuredBufferGetDimensions => Form::Method("GetDimensions"),
        StructuredBufferLoad => Form::Method("Load"),

        RWStructuredBufferGetDimensions => Form::Method("GetDimensions"),
        RWStructuredBufferLoad => Form::Method("Load"),

        ByteAddressBufferGetDimensions => Form::Method("GetDimensions"),
        ByteAddressBufferLoad => Form::Method("Load"),
        ByteAddressBufferLoad2 => Form::Method("Load2"),
        ByteAddressBufferLoad3 => Form::Method("Load3"),
        ByteAddressBufferLoad4 => Form::Method("Load4"),
        ByteAddressBufferLoadT => Form::Method("Load"),

        RWByteAddressBufferGetDimensions => Form::Method("GetDimensions"),
        RWByteAddressBufferLoad => Form::Method("Load"),
        RWByteAddressBufferLoad2 => Form::Method("Load2"),
        RWByteAddressBufferLoad3 => Form::Method("Load3"),
        RWByteAddressBufferLoad4 => Form::Method("Load4"),
        RWByteAddressBufferLoadT => Form::Method("Load"),
        RWByteAddressBufferStore => Form::Method("Store"),
        RWByteAddressBufferStore2 => Form::Method("Store2"),
        RWByteAddressBufferStore3 => Form::Method("Store3"),
        RWByteAddressBufferStore4 => Form::Method("Store4"),
        RWByteAddressBufferInterlockedAdd => Form::Method("InterlockedAdd"),
        RWByteAddressBufferInterlockedAnd => Form::Method("InterlockedAnd"),
        RWByteAddressBufferInterlockedCompareExchange => Form::Method("InterlockedCompareExchange"),
        RWByteAddressBufferInterlockedCompareStore => Form::Method("InterlockedCompareStore"),
        RWByteAddressBufferInterlockedExchange => Form::Method("InterlockedExchange"),
        RWByteAddressBufferInterlockedMax => Form::Method("InterlockedMax"),
        RWByteAddressBufferInterlockedMin => Form::Method("InterlockedMin"),
        RWByteAddressBufferInterlockedOr => Form::Method("InterlockedOr"),
        RWByteAddressBufferInterlockedXor => Form::Method("InterlockedXor"),

        BufferAddressLoad => Form::AddressMethod("Load", "vk::RawBufferLoad"),

        RWBufferAddressLoad => Form::AddressMethod("Load", "vk::RawBufferLoad"),
        RWBufferAddressStore => Form::AddressMethod("Store", "vk::RawBufferStore"),

        Texture2DGetDimensions => Form::Method("GetDimensions"),
        Texture2DLoad => Form::Method("Load"),
        Texture2DSample => Form::Method("Sample"),
        Texture2DSampleBias => Form::Method("SampleBias"),
        Texture2DSampleCmp => Form::Method("SampleCmp"),
        Texture2DSampleCmpLevelZero => Form::Method("SampleCmpLevelZero"),
        Texture2DSampleGrad => Form::Method("SampleGrad"),
        Texture2DSampleLevel => Form::Method("SampleLevel"),
        Texture2DGatherRed => Form::Method("GatherRed"),
        Texture2DGatherGreen => Form::Method("GatherGreen"),
        Texture2DGatherBlue => Form::Method("GatherBlue"),
        Texture2DGatherAlpha => Form::Method("GatherAlpha"),
        Texture2DGatherCmpRed => Form::Method("GatherCmpRed"),
        Texture2DGatherCmpGreen => Form::Method("GatherCmpGreen"),
        Texture2DGatherCmpBlue => Form::Method("GatherCmpBlue"),
        Texture2DGatherCmpAlpha => Form::Method("GatherCmpAlpha"),

        Texture2DArrayGetDimensions => Form::Method("GetDimensions"),
        Texture2DArrayLoad => Form::Method("Load"),
        Texture2DArraySample => Form::Method("Sample"),
        Texture2DArraySampleBias => Form::Method("SampleBias"),
        Texture2DArraySampleCmp => Form::Method("SampleCmp"),
        Texture2DArraySampleCmpLevelZero => Form::Method("SampleCmpLevelZero"),
        Texture2DArraySampleGrad => Form::Method("SampleGrad"),
        Texture2DArraySampleLevel => Form::Method("SampleLevel"),
        Texture2DArrayGatherRed => Form::Method("GatherRed"),
        Texture2DArrayGatherGreen => Form::Method("GatherGreen"),
        Texture2DArrayGatherBlue => Form::Method("GatherBlue"),
        Texture2DArrayGatherAlpha => Form::Method("GatherAlpha"),
        Texture2DArrayGatherCmpRed => Form::Method("GatherCmpRed"),
        Texture2DArrayGatherCmpGreen => Form::Method("GatherCmpGreen"),
        Texture2DArrayGatherCmpBlue => Form::Method("GatherCmpBlue"),
        Texture2DArrayGatherCmpAlpha => Form::Method("GatherCmpAlpha"),

        RWTexture2DGetDimensions => Form::Method("GetDimensions"),
        RWTexture2DLoad => Form::Method("Load"),

        RWTexture2DArrayGetDimensions => Form::Method("GetDimensions"),
        RWTexture2DArrayLoad => Form::Method("Load"),

        TextureCubeSample => Form::Method("Sample"),
        TextureCubeSampleLevel => Form::Method("SampleLevel"),

        TextureCubeArraySample => Form::Method("Sample"),
        TextureCubeArraySampleLevel => Form::Method("SampleLevel"),

        Texture3DGetDimensions => Form::Method("GetDimensions"),
        Texture3DLoad => Form::Method("Load"),
        Texture3DSample => Form::Method("Sample"),
        Texture3DSampleBias => Form::Method("SampleBias"),
        Texture3DSampleGrad => Form::Method("SampleGrad"),
        Texture3DSampleLevel => Form::Method("SampleLevel"),

        RWTexture3DGetDimensions => Form::Method("GetDimensions"),
        RWTexture3DLoad => Form::Method("Load"),

        TriangleStreamAppend => Form::Method("Append"),
        TriangleStreamRestartStrip => Form::Method("RestartStrip"),

        RayQueryTraceRayInline => Form::Method("TraceRayInline"),
        RayQueryProceed => Form::Method("Proceed"),
        RayQueryAbort => Form::Method("Abort"),
        RayQueryCommittedStatus => Form::Method("CommittedStatus"),
        RayQueryCandidateType => Form::Method("CandidateType"),
        RayQueryCandidateProceduralPrimitiveNonOpaque => {
            Form::Method("CandidateProceduralPrimitiveNonOpaque")
        }
        RayQueryCommitNonOpaqueTriangleHit => Form::Method("CommitNonOpaqueTriangleHit"),
        RayQueryCommitProceduralPrimitiveHit => Form::Method("CommitProceduralPrimitiveHit"),
        RayQueryRayFlags => Form::Method("RayFlags"),
        RayQueryWorldRayOrigin => Form::Method("WorldRayOrigin"),
        RayQueryWorldRayDirection => Form::Method("WorldRayDirection"),
        RayQueryRayTMin => Form::Method("RayTMin"),
        RayQueryCandidateTriangleRayT => Form::Method("CandidateTriangleRayT"),
        RayQueryCommittedRayT => Form::Method("CommittedRayT"),
        RayQueryCandidateInstanceIndex => Form::Method("CandidateInstanceIndex"),
        RayQueryCandidateInstanceID => Form::Method("CandidateInstanceID"),
        RayQueryCandidateInstanceContributionToHitGroupIndex => {
            Form::Method("CandidateInstanceContributionToHitGroupIndex")
        }
        RayQueryCandidateGeometryIndex => Form::Method("CandidateGeometryIndex"),
        RayQueryCandidatePrimitiveIndex => Form::Method("CandidatePrimitiveIndex"),
        RayQueryCandidateObjectRayOrigin => Form::Method("CandidateObjectRayOrigin"),
        RayQueryCandidateObjectRayDirection => Form::Method("CandidateObjectRayDirection"),
        RayQueryCandidateObjectToWorld3x4 => Form::Method("CandidateObjectToWorld3x4"),
        RayQueryCandidateObjectToWorld4x3 => Form::Method("CandidateObjectToWorld4x3"),
        RayQueryCandidateWorldToObject3x4 => Form::Method("CandidateWorldToObject3x4"),
        RayQueryCandidateWorldToObject4x3 => Form::Method("CandidateWorldToObject4x3"),
        RayQueryCommittedInstanceIndex => Form::Method("CommittedInstanceIndex"),
        RayQueryCommittedInstanceID => Form::Method("CommittedInstanceID"),
        RayQueryCommittedInstanceContributionToHitGroupIndex => {
            Form::Method("CommittedInstanceContributionToHitGroupIndex")
        }
        RayQueryCommittedGeometryIndex => Form::Method("CommittedGeometryIndex"),
        RayQueryCommittedPrimitiveIndex => Form::Method("CommittedPrimitiveIndex"),
        RayQueryCommittedObjectRayOrigin => Form::Method("CommittedObjectRayOrigin"),
        RayQueryCommittedObjectRayDirection => Form::Method("CommittedObjectRayDirection"),
        RayQueryCommittedObjectToWorld3x4 => Form::Method("CommittedObjectToWorld3x4"),
        RayQueryCommittedObjectToWorld4x3 => Form::Method("CommittedObjectToWorld4x3"),
        RayQueryCommittedWorldToObject3x4 => Form::Method("CommittedWorldToObject3x4"),
        RayQueryCommittedWorldToObject4x3 => Form::Method("CommittedWorldToObject4x3"),
        RayQueryCandidateTriangleBarycentrics => Form::Method("CandidateTriangleBarycentrics"),
        RayQueryCandidateTriangleFrontFace => Form::Method("CandidateTriangleFrontFace"),
        RayQueryCommittedTriangleBarycentrics => Form::Method("CommittedTriangleBarycentrics"),
        RayQueryCommittedTriangleFrontFace => Form::Method("CommittedTriangleFrontFace"),
    };

    let expr = match form {
        Form::Invoke(s) => {
            let object = Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial(s),
            )));

            // Do not export type arguments - the only templated non-method is not templated in HLSL so is not allowed type argments
            let type_args = Vec::new();

            let args = generate_invocation_args(exprs, context)?;

            ast::Expression::Call(object, type_args, args)
        }
        Form::AddressMethod(_, s) if context.module.flags.requires_buffer_address => {
            assert!(
                exprs.len() >= 2,
                "Buffer address intrinsic expects at least an address and offset"
            );

            let object = Box::new(Located::none(ast::Expression::Identifier(
                ast::ScopedIdentifier::trivial(s),
            )));

            let type_args = generate_template_type_args(tys, context)?;

            let mut args = Vec::new();
            if let [addr, offset, rest @ ..] = exprs.as_slice() {
                let addr_base_expr = generate_expression(addr, context)?;
                let addr_base_expr = Box::new(Located::none(addr_base_expr));
                let addr_offset_expr = generate_expression(offset, context)?;
                let addr_expr = Located::none(ast::Expression::BinaryOperation(
                    ast::BinOp::Add,
                    addr_base_expr,
                    Box::new(Located::none(ast::Expression::Call(
                        Box::new(Located::none(ast::Expression::Identifier(
                            ast::ScopedIdentifier::trivial("uint64_t"),
                        ))),
                        Vec::new(),
                        Vec::from([Located::none(addr_offset_expr)]),
                    ))),
                ));
                args.push(addr_expr);

                for expr in rest {
                    args.push(Located::none(generate_expression(expr, context)?));
                }
            } else {
                panic!("Incorrect number of arguments in buffer address load");
            }

            ast::Expression::Call(object, type_args, args)
        }
        Form::Method(s) | Form::AddressMethod(s, _) => {
            assert!(!exprs.is_empty());
            let object = generate_expression(&exprs[0], context)?;
            let name = ast::ScopedIdentifier::trivial(s);
            let member = ast::Expression::Member(Box::new(Located::none(object)), name);
            let type_args = generate_template_type_args(tys, context)?;
            let args = generate_invocation_args(&exprs[1..], context)?;
            ast::Expression::Call(Box::new(Located::none(member)), type_args, args)
        }
    };
    Ok(expr)
}

/// Write out an intrinsic operator expression
fn generate_intrinsic_op(
    intrinsic: &ir::IntrinsicOp,
    exprs: &Vec<ir::Expression>,
    context: &mut GenerateContext,
) -> Result<ast::Expression, GenerateError> {
    enum Form {
        Unary(ast::UnaryOp),
        Binary(ast::BinOp),
    }

    use ir::IntrinsicOp::*;
    let form = match &intrinsic {
        PrefixIncrement => Form::Unary(ast::UnaryOp::PrefixIncrement),
        PrefixDecrement => Form::Unary(ast::UnaryOp::PrefixDecrement),
        PostfixIncrement => Form::Unary(ast::UnaryOp::PostfixIncrement),
        PostfixDecrement => Form::Unary(ast::UnaryOp::PostfixDecrement),
        Plus => Form::Unary(ast::UnaryOp::Plus),
        Minus => Form::Unary(ast::UnaryOp::Minus),
        LogicalNot => Form::Unary(ast::UnaryOp::LogicalNot),
        BitwiseNot => Form::Unary(ast::UnaryOp::BitwiseNot),

        Add => Form::Binary(ast::BinOp::Add),
        Subtract => Form::Binary(ast::BinOp::Subtract),
        Multiply => Form::Binary(ast::BinOp::Multiply),
        Divide => Form::Binary(ast::BinOp::Divide),
        Modulus => Form::Binary(ast::BinOp::Modulus),
        LeftShift => Form::Binary(ast::BinOp::LeftShift),
        RightShift => Form::Binary(ast::BinOp::RightShift),
        BitwiseAnd => Form::Binary(ast::BinOp::BitwiseAnd),
        BitwiseOr => Form::Binary(ast::BinOp::BitwiseOr),
        BitwiseXor => Form::Binary(ast::BinOp::BitwiseXor),
        BooleanAnd => Form::Binary(ast::BinOp::BooleanAnd),
        BooleanOr => Form::Binary(ast::BinOp::BooleanOr),
        LessThan => Form::Binary(ast::BinOp::LessThan),
        LessEqual => Form::Binary(ast::BinOp::LessEqual),
        GreaterThan => Form::Binary(ast::BinOp::GreaterThan),
        GreaterEqual => Form::Binary(ast::BinOp::GreaterEqual),
        Equality => Form::Binary(ast::BinOp::Equality),
        Inequality => Form::Binary(ast::BinOp::Inequality),
        Assignment => Form::Binary(ast::BinOp::Assignment),
        SumAssignment => Form::Binary(ast::BinOp::SumAssignment),
        DifferenceAssignment => Form::Binary(ast::BinOp::DifferenceAssignment),
        ProductAssignment => Form::Binary(ast::BinOp::ProductAssignment),
        QuotientAssignment => Form::Binary(ast::BinOp::QuotientAssignment),
        RemainderAssignment => Form::Binary(ast::BinOp::RemainderAssignment),
        LeftShiftAssignment => Form::Binary(ast::BinOp::LeftShiftAssignment),
        RightShiftAssignment => Form::Binary(ast::BinOp::RightShiftAssignment),
        BitwiseAndAssignment => Form::Binary(ast::BinOp::BitwiseAndAssignment),
        BitwiseOrAssignment => Form::Binary(ast::BinOp::BitwiseOrAssignment),
        BitwiseXorAssignment => Form::Binary(ast::BinOp::BitwiseXorAssignment),
    };

    let expr = match form {
        Form::Unary(op) => {
            assert_eq!(exprs.len(), 1);
            let inner = generate_expression(&exprs[0], context)?;
            ast::Expression::UnaryOperation(op, Box::new(Located::none(inner)))
        }
        Form::Binary(op) => {
            assert_eq!(exprs.len(), 2);
            let left = generate_expression(&exprs[0], context)?;
            let right = generate_expression(&exprs[1], context)?;
            ast::Expression::BinaryOperation(
                op,
                Box::new(Located::none(left)),
                Box::new(Located::none(right)),
            )
        }
    };
    Ok(expr)
}

/// Generate HLSL for an initializer
fn generate_initializer(
    init_opt: &Option<ir::Initializer>,
    context: &mut GenerateContext,
) -> Result<Option<ast::Initializer>, GenerateError> {
    if let Some(init) = init_opt {
        Ok(Some(generate_initializer_inner(init, context)?))
    } else {
        Ok(None)
    }
}

/// Internal initializer generation
fn generate_initializer_inner(
    init: &ir::Initializer,
    context: &mut GenerateContext,
) -> Result<ast::Initializer, GenerateError> {
    let init = match init {
        ir::Initializer::Expression(expr) => {
            ast::Initializer::Expression(Located::none(generate_expression(expr, context)?))
        }
        ir::Initializer::Aggregate(exprs) => {
            let mut values = Vec::new();
            for expr in exprs {
                values.push(generate_initializer_inner(expr, context)?);
            }
            ast::Initializer::Aggregate(values)
        }
    };
    Ok(init)
}

/// Generate HLSL for a set of function call arguments
fn generate_invocation_args(
    exprs: &[ir::Expression],
    context: &mut GenerateContext,
) -> Result<Vec<Located<ast::Expression>>, GenerateError> {
    let mut ast = Vec::new();
    for expr in exprs {
        ast.push(Located::none(generate_expression(expr, context)?));
    }
    Ok(ast)
}

/// Generate HLSL for a set of template arguments
fn generate_template_type_args(
    template_args: &[ir::TypeOrConstant],
    context: &mut GenerateContext,
) -> Result<Vec<ast::ExpressionOrType>, GenerateError> {
    let mut args = Vec::new();
    for ta in template_args {
        args.push(generate_type_or_constant(ta, context)?);
    }
    Ok(args)
}

/// Generate HLSL for a struct definition
fn generate_struct(
    decl: &ir::StructDefinition,
    context: &mut GenerateContext,
) -> Result<ast::StructDefinition, GenerateError> {
    let mut members = Vec::new();

    for member in &decl.members {
        let precise_modifier = if member.precise {
            Some(ast::TypeModifier::Precise)
        } else {
            None
        };

        let interpolation_modifier =
            generate_interpolation_modifier(&member.interpolation_modifier)?;

        let (base, declarator) =
            generate_type_and_declarator(member.type_id, &member.name, true, context)?;

        // Combine type with modifiers
        let base_with_interp = prepend_modifiers(base, &interpolation_modifier);
        let ty = prepend_modifiers(base_with_interp, &[precise_modifier]);

        let location_annotations = if let Some(semantic) = &member.semantic {
            Vec::from([ast::LocationAnnotation::Semantic(semantic.clone())])
        } else {
            Vec::new()
        };

        let init_declarator = ast::InitDeclarator {
            declarator,
            location_annotations,
            init: None,
        };

        members.push(ast::StructEntry::Variable(ast::StructMember {
            ty,
            defs: Vec::from([init_declarator]),
            attributes: Vec::new(),
        }));
    }

    for method in &decl.methods {
        let defs = generate_function(*method, false, context)?;
        for def in defs {
            members.push(ast::StructEntry::Method(def));
        }
    }

    let sd = ast::StructDefinition {
        name: Located::none(context.get_struct_name(decl.id)?.to_string()),
        base_types: Vec::new(),
        template_params: ast::TemplateParamList(Vec::new()),
        members,
    };

    Ok(sd)
}

/// Generate HLSL for an enum definition
fn generate_enum(
    id: ir::EnumId,
    context: &mut GenerateContext,
) -> Result<ast::EnumDefinition, GenerateError> {
    let name = Located::none(context.get_enum_name(id)?.to_string());

    let mut values = Vec::new();
    for value_id in context.module.enum_registry.get_values(id) {
        let value_data = context.module.enum_registry.get_enum_value(*value_id);

        let value_name = Located::none(context.get_enum_value_name(*value_id)?.to_string());
        let value_expr = generate_literal(&value_data.value, context)?;

        values.push(ast::EnumValue {
            name: value_name,
            value: Some(Located::none(value_expr)),
        });
    }

    let def = ast::EnumDefinition { name, values };

    Ok(def)
}

/// Generate HLSL for a constant buffer
fn generate_constant_buffer(
    id: ir::ConstantBufferId,
    context: &mut GenerateContext,
) -> Result<ast::ConstantBuffer, GenerateError> {
    let decl = &context.module.cbuffer_registry[id.0 as usize];

    let binding_attribute = if context.module.flags.requires_vk_binding {
        generate_vk_binding_annotation(&decl.api_binding)?
    } else {
        None
    };

    let name = context.get_constant_buffer_name(id)?;
    let name = Located::none(name.to_string());

    let register = if !context.module.flags.requires_vk_binding {
        generate_register_annotation(&decl.api_binding)?
    } else {
        None
    };

    let location_annotations = if let Some(register) = register {
        Vec::from([ast::LocationAnnotation::Register(register)])
    } else {
        Vec::new()
    };

    let mut members = Vec::new();
    for member in &decl.members {
        let (base, declarator) =
            generate_type_and_declarator(member.type_id, &member.name, false, context)?;

        let location_annotations = if let Some(packoffset) = &member.offset {
            Vec::from([ast::LocationAnnotation::PackOffset(packoffset.clone())])
        } else {
            Vec::new()
        };

        let init_declarator = ast::InitDeclarator {
            declarator,
            location_annotations,
            init: None,
        };

        members.push(ast::ConstantVariable {
            ty: base,
            defs: Vec::from([init_declarator]),
        });
    }

    let attributes = match binding_attribute {
        Some(attr) => Vec::from([attr]),
        None => Vec::new(),
    };

    let def = ast::ConstantBuffer {
        name,
        location_annotations,
        members,
        attributes,
    };

    Ok(def)
}

/// Construct an ast scoped identifier from a HLSL generator scoped name
fn scoped_name_to_identifier(scoped_name: ScopedName) -> ast::ScopedIdentifier {
    ast::ScopedIdentifier {
        // Technically should be absolute but that generates uglier paths in the common case
        base: ast::ScopedIdentifierBase::Relative,
        identifiers: scoped_name
            .0
            .into_iter()
            .map(Located::none)
            .collect::<Vec<_>>(),
    }
}

/// Add additional modifiers to a type
fn prepend_modifiers(mut ty: ast::Type, modifiers: &[Option<ast::TypeModifier>]) -> ast::Type {
    for modifier in modifiers.iter().rev().flatten() {
        if !ty.modifiers.modifiers.iter().any(|e| e.node == *modifier) {
            ty.modifiers.prepend(Located::none(*modifier))
        }
    }
    ty
}

/// Contextual state for HLSL generator
struct GenerateContext<'m> {
    module: &'m ir::Module,
    name_map: NameMap,

    pipeline_description: PipelineDescription,
}

impl<'m> GenerateContext<'m> {
    /// Start a new generate state
    fn new(module: &'m ir::Module) -> Self {
        let name_map = NameMap::build(module, RESERVED_NAMES, true);

        GenerateContext {
            module,
            name_map,
            pipeline_description: PipelineDescription {
                bind_groups: Vec::new(),
            },
        }
    }

    /// Get the name of a global variable
    fn get_global_name(&self, id: ir::GlobalId) -> Result<&str, GenerateError> {
        let def = &self.module.global_registry[id.0 as usize];
        if def.is_intrinsic {
            Ok(&def.name.node)
        } else {
            Ok(self.name_map.get_name_leaf(NameSymbol::GlobalVariable(id)))
        }
    }

    /// Get the name of a function
    fn get_function_name(&self, id: ir::FunctionId) -> Result<&str, GenerateError> {
        Ok(self.name_map.get_name_leaf(NameSymbol::Function(id)))
    }

    /// Get the full name of a function
    fn get_function_name_full(&self, id: ir::FunctionId) -> Result<ScopedName, GenerateError> {
        Ok(self.name_map.get_name_qualified(NameSymbol::Function(id)))
    }

    /// Get the name of a struct
    fn get_struct_name(&self, id: ir::StructId) -> Result<&str, GenerateError> {
        Ok(self.name_map.get_name_leaf(NameSymbol::Struct(id)))
    }

    /// Get the full name of a struct
    fn get_struct_name_full(&self, id: ir::StructId) -> Result<ScopedName, GenerateError> {
        Ok(self.name_map.get_name_qualified(NameSymbol::Struct(id)))
    }

    /// Get the name of an enum
    fn get_enum_name(&self, id: ir::EnumId) -> Result<&str, GenerateError> {
        Ok(self.name_map.get_name_leaf(NameSymbol::Enum(id)))
    }

    /// Get the full name of an enum
    fn get_enum_name_full(&self, id: ir::EnumId) -> Result<ScopedName, GenerateError> {
        Ok(self.name_map.get_name_qualified(NameSymbol::Enum(id)))
    }

    /// Get the name of an enum value
    fn get_enum_value_name(&self, id: ir::EnumValueId) -> Result<&str, GenerateError> {
        Ok(&self.module.enum_registry.get_enum_value(id).name)
    }

    /// Get the full name of an enum value
    fn get_enum_value_name_full(&self, id: ir::EnumValueId) -> Result<ScopedName, GenerateError> {
        let value = self.module.enum_registry.get_enum_value(id);
        let mut name = self.get_enum_name_full(value.enum_id).unwrap();
        name.0.push(value.name.node.clone());
        Ok(name)
    }

    /// Get the name of a constant buffer
    fn get_constant_buffer_name(&self, id: ir::ConstantBufferId) -> Result<&str, GenerateError> {
        match self.module.cbuffer_registry.get(id.0 as usize) {
            Some(cd) => Ok(cd.name.as_str()),
            None => Err(GenerateError::NamelessId),
        }
    }

    /// Get the name of a local variable
    fn get_variable_name(&self, id: ir::VariableId) -> Result<&str, GenerateError> {
        Ok(&self
            .module
            .variable_registry
            .get_local_variable(id)
            .name
            .node)
    }

    /// Add a binding to the pipeline layout description
    fn register_binding(&mut self, group_index: u32, binding: DescriptorBinding) {
        let group_index = group_index as usize;
        if group_index >= self.pipeline_description.bind_groups.len() {
            self.pipeline_description.bind_groups.resize(
                group_index + 1,
                BindGroup {
                    bindings: Vec::new(),
                    inline_constants: None,
                },
            )
        }

        self.pipeline_description.bind_groups[group_index]
            .bindings
            .push(binding);
    }
}
