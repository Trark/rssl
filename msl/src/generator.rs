use std::collections::{HashMap, HashSet};

use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::export::*;
use rssl_text::{Located, SourceLocation};

use crate::names::*;

mod intrinsic_helpers;
use intrinsic_helpers::*;

/// Result from generating MSL from RSSL
pub struct GeneratedAST {
    /// Output ast - reusing the RSSL ast as it is can represent the subset of MSL we want
    pub ast_module: ast::Module,

    /// Metadata to describe the pipeline that has been generated
    pub pipeline_description: PipelineDescription,
}

/// Error result when generating MSL fails
#[derive(PartialEq, Debug)]
pub enum GenerateError {
    /// Input module is invalid
    InvalidModule,

    /// Unable to generate a valid ast for a type with an array modifier in this position
    ComplexTypeBind,

    /// All constants must be initialized in metal
    UninitializedConstant,

    /// Metal does not support matrix with width or height of 1
    UnsupportedUnitMatrix,

    /// Metal does not support matrix with underlying types that are not float or half
    UnsupportedNonFloatMatrix,

    /// Metal does not support a 64-bit floating point type
    UnsupportedDouble,

    /// Metal does not allow precise as a type modifier - we need to propagate to all operations manually
    UnsupportedPrecise,

    /// Interpolation attributes can not be applied to arrays
    UnsupportedArrayInterpolator,

    /// Geometry shaders are not supported by Metal
    UnsupportedGeometryShader,

    /// Texture only supports specific numeric types
    UnsupportedTextureComponentType,

    /// \[WaveSize\] not supported
    UnsupportedWaveSize,

    /// Metal does not have a native matrix swizzle so we would need to decompose into components to reconstruct the same behaviour
    UnimplementedMatrixSwizzle,

    /// Metal matrix indexing first selects the column then row, RSSL - like HLSL - selects the row then column
    UnimplementedMatrixIndex,

    /// Function argument passing rules are different and need careful handling
    UnimplementedOutParameters,

    /// Semantics need special handling
    UnimplementedFunctionParameterWithSemantic,

    /// Semantics need special handling
    UnimplementedFunctionReturnWithSemantic,

    /// Semantics need special handling
    UnimplementedStructWithSemantic,

    /// Texture type is not implemented yet
    UnimplementedTexture(&'static str),

    /// RSSL object type is not implemented yet
    UnimplementedObject(ir::ObjectType),

    /// Constant buffers are not implemented
    UnimplementedConstantBuffer,

    /// RSSL intrinsic is not implemented yet
    UnimplementedIntrinsic(ir::Intrinsic),

    /// Mesh shaders require more rewriting of inputs than is currently supported
    UnimplementedMeshShader,

    /// Raytracing has not been implemented
    UnimplementedRaytracing,
}

/// Generate MSL ast from ir module
pub fn generate_module(module: &ir::Module) -> Result<GeneratedAST, GenerateError> {
    let mut context = GenerateContext::new(module);
    let mut root_definitions = Vec::new();

    // Generate binding info
    for decl in &module.root_definitions {
        analyse_bindings(decl, &mut context)?;
    }

    analyse_globals(&mut context)?;

    assert!(module.inline_constant_buffers.is_empty());

    generate_root_definitions(
        module,
        &module.root_definitions,
        &mut root_definitions,
        &mut context,
    )?;

    let mut root_definitions = simplify_namespaces(root_definitions);

    if let Some(helpers) = generate_helpers(context.required_helpers)? {
        root_definitions.insert(0, helpers);
    }

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
                };

                context.register_binding(api_slot.set, binding);
            }
        }
    }
    Ok(())
}

/// How we will handle generating a global variable
enum GlobalMode {
    /// Variables that need passing between all functions - with additional metadata on the entry function
    Parameter(ast::FunctionParam, ast::Expression, GlobalParameterMode),

    /// A constant value can be emitted at global scope
    Constant,
}

enum GlobalParameterMode {
    /// Shader inputs require additional metadata on the entry function
    ShaderInput,

    /// Local state requires declaring in the entry function
    LaneState,
}

/// Process global state that must be passed between functions
fn analyse_globals(context: &mut GenerateContext) -> Result<(), GenerateError> {
    let global_usage = ir::usage_analysis::GlobalUsageAnalysis::calculate(context.module);

    for i in 0..context.module.global_registry.len() {
        let id = ir::GlobalId(i as u32);
        let def = &context.module.global_registry[id.0 as usize];

        if def.is_intrinsic {
            continue;
        }

        let is_const = context.module.type_registry.is_const(def.type_id);
        let is_global_constant = is_const && def.storage_class == ir::GlobalStorage::Static;

        let mode = if is_global_constant {
            GlobalMode::Constant
        } else {
            let name = context.get_global_name(id)?.to_string();

            let (mut param_type, declarator) =
                generate_type_and_declarator(def.type_id, &name, is_global_constant, context)?;

            // Pick the address space for the global
            let (address_space, is_object) = {
                let natural_address_space = match def.storage_class {
                    ir::GlobalStorage::Extern => ast::AddressSpace::Device,
                    ir::GlobalStorage::Static => ast::AddressSpace::Thread,
                    ir::GlobalStorage::GroupShared => ast::AddressSpace::ThreadGroup,
                };

                let unarray_id = context.module.type_registry.get_non_array_id(def.type_id);
                let unmod_id = context.module.type_registry.remove_modifier(unarray_id);
                let tyl = context.module.type_registry.get_type_layer(unmod_id);
                match tyl {
                    ir::TypeLayer::Object(ot) => {
                        // Object types depend on if they are pointer-like or not
                        assert_eq!(natural_address_space, ast::AddressSpace::Device);
                        match ot {
                            // Types that turn into pointers need an address space
                            ir::ObjectType::StructuredBuffer(_)
                            | ir::ObjectType::RWStructuredBuffer(_) => {
                                (Some(ast::AddressSpace::Device), true)
                            }
                            _ => {
                                // Types that turn into intrinsic object types do not
                                (None, true)
                            }
                        }
                    }
                    _ => {
                        // Non-object types will need an address space qualifier
                        (Some(natural_address_space), false)
                    }
                }
            };

            // Add the address space to the type
            if let Some(address_space) = address_space {
                param_type
                    .modifiers
                    .prepend(Located::none(ast::TypeModifier::AddressSpace(
                        address_space,
                    )));
            }

            let declarator = if !is_object {
                // Make the parameter into a reference
                ast::Declarator::Reference(ast::ReferenceDeclarator {
                    attributes: Vec::new(),
                    inner: Box::new(declarator),
                })
            } else {
                // The parameter will already be some kind of pointer type
                declarator
            };

            let param = ast::FunctionParam {
                param_type,
                declarator,
                location_annotations: Vec::new(),
                default_expr: None,
            };

            let arg_expr = ast::Expression::Identifier(ast::ScopedIdentifier::from(Located::none(
                name.as_str(),
            )));

            let entry_mode = if def.storage_class == ir::GlobalStorage::Static {
                GlobalParameterMode::LaneState
            } else {
                GlobalParameterMode::ShaderInput
            };

            GlobalMode::Parameter(param, arg_expr, entry_mode)
        };

        let valid_insert = context.global_variable_modes.insert(id, mode).is_none();
        assert!(valid_insert);
    }

    let function_count = context.module.function_registry.get_function_count();
    for i in 0..function_count {
        let id = ir::FunctionId(i);

        if context
            .module
            .function_registry
            .get_intrinsic_data(id)
            .is_some()
        {
            continue;
        }

        let mut required_globals = Vec::new();
        for symbol in global_usage.get_usage_for_function(id) {
            if let ir::usage_analysis::UsageSymbol::GlobalVariable(gid) = symbol {
                if !matches!(
                    context.global_variable_modes.get(gid).unwrap(),
                    GlobalMode::Constant
                ) {
                    required_globals.push(*gid);
                }
            }
        }

        required_globals.sort();

        let valid_insert = context
            .function_required_globals
            .insert(id, required_globals)
            .is_none();
        assert!(valid_insert);
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

/// Generate a set of root definitions
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

/// Generate a single root definition
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
        ir::RootDefinition::ConstantBuffer(_) => {
            return Err(GenerateError::UnimplementedConstantBuffer);
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let mode = context.global_variable_modes.get(id).unwrap();
            match mode {
                GlobalMode::Parameter(_, _, _) => Vec::new(),
                GlobalMode::Constant => {
                    let def = generate_global_constant(*id, context)?;
                    Vec::from([ast::RootDefinition::GlobalVariable(def)])
                }
            }
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

/// Generate a global variable for any constant global
fn generate_global_constant(
    id: ir::GlobalId,
    context: &mut GenerateContext,
) -> Result<ast::GlobalVariable, GenerateError> {
    let decl = &context.module.global_registry[id.0 as usize];

    let name = context.get_global_name(id)?.to_string();

    let (type_base, declarator) = generate_type_and_declarator(decl.type_id, &name, true, context)?;

    let storage_modifier = Some(ast::TypeModifier::AddressSpace(ast::AddressSpace::Constant));
    let global_type = prepend_modifiers(type_base, &[storage_modifier]);

    let init = generate_initializer(&decl.init, decl.type_id, context)?;

    let init_declarator = ast::InitDeclarator {
        declarator,
        location_annotations: Vec::new(),
        init,
    };

    let gv = ast::GlobalVariable {
        global_type,
        defs: Vec::from([init_declarator]),
        attributes: Vec::new(),
    };

    Ok(gv)
}

/// Generate a function and all template instantiations
fn generate_function(
    id: ir::FunctionId,
    only_declare: bool,
    context: &mut GenerateContext,
) -> Result<Vec<ast::FunctionDefinition>, GenerateError> {
    let sig = context.module.function_registry.get_function_signature(id);
    let is_template = !sig.template_params.is_empty();

    if is_template {
        let mut fs = Vec::new();

        let function_count = context.module.function_registry.get_function_count();
        for index in 0..function_count {
            let child_id = ir::FunctionId(index);
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

/// Generate a function
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
        if let Some(attr) = generate_function_attribute(attribute, context)? {
            attributes.push(attr);
        }
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

    // Parameters for implementing global variables come after the normal parameters
    let parameters_for_globals = context.function_required_globals.get(&id).unwrap();
    for gid in parameters_for_globals {
        match context.global_variable_modes.get(gid).unwrap() {
            GlobalMode::Parameter(param, _, _) => {
                // TODO: Entry function special case
                params.push(param.clone());
            }
            GlobalMode::Constant => panic!("global does not require a parameter"),
        }
    }

    let semantic = generate_semantic_annotation(&sig.return_type.semantic)?;
    if semantic.is_some() {
        return Err(GenerateError::UnimplementedFunctionReturnWithSemantic);
    }

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
            location_annotations: Vec::new(),
        },
        template_params: ast::TemplateParamList(template_params),
        params,
        body,
        attributes,
    })
}

/// Generate output for a function attribute
fn generate_function_attribute(
    attr: &ir::FunctionAttribute,
    _: &mut GenerateContext,
) -> Result<Option<ast::Attribute>, GenerateError> {
    let ast = match attr {
        ir::FunctionAttribute::NumThreads(..) => {
            // TODO: We need to export the thread group count as metadata
            None
        }
        ir::FunctionAttribute::MaxVertexCount(_) => {
            return Err(GenerateError::UnsupportedGeometryShader);
        }
        ir::FunctionAttribute::WaveSize(_) => {
            return Err(GenerateError::UnsupportedWaveSize);
        }
        ir::FunctionAttribute::OutputTopology(_) => {
            return Err(GenerateError::UnimplementedMeshShader)
        }
    };
    Ok(ast)
}

/// Generate a function parameter
fn generate_function_param(
    param: &ir::FunctionParam,
    context: &mut GenerateContext,
) -> Result<ast::FunctionParam, GenerateError> {
    let input_modifier = match param.param_type.input_modifier {
        ir::InputModifier::In => None,
        ir::InputModifier::Out => return Err(GenerateError::UnimplementedOutParameters),
        ir::InputModifier::InOut => return Err(GenerateError::UnimplementedOutParameters),
    };

    if param.precise {
        return Err(GenerateError::UnsupportedPrecise);
    };

    let name = context.get_variable_name(param.id)?.to_string();
    let (base_type, declarator) =
        generate_type_and_declarator(param.param_type.type_id, &name, false, context)?;

    let interpolation_modifier =
        generate_interpolation_modifier(&param.interpolation_modifier, &declarator)?;
    if interpolation_modifier.is_some() {
        todo!();
    }

    let param_type = prepend_modifiers(base_type, &[input_modifier]);

    let semantic = generate_semantic_annotation(&param.semantic)?;
    if semantic.is_some() {
        return Err(GenerateError::UnimplementedFunctionParameterWithSemantic);
    }

    let default_expr = if let Some(default_expr) = &param.default_expr {
        Some(generate_expression(default_expr, context)?)
    } else {
        None
    };

    Ok(ast::FunctionParam {
        param_type,
        declarator,
        location_annotations: Vec::new(),
        default_expr,
    })
}

/// Generate attribute for an interpolation modifier
fn generate_interpolation_modifier(
    interpolation_modifier: &Option<ir::InterpolationModifier>,
    declarator: &ast::Declarator,
) -> Result<Option<ast::Attribute>, GenerateError> {
    if let Some(interpolation_modifier) = &interpolation_modifier {
        // This only catches outer array
        let is_array = matches!(declarator, ast::Declarator::Array(_));

        // If we are an array then ensure we do not have a standard interpolation modifier
        // as it will generate invalid MSL
        if is_array
            && matches!(
                interpolation_modifier,
                CenterPerspective
                    | CentroidPerspective
                    | SamplePerspective
                    | CenterNoPerspective
                    | CentroidNoPerspective
                    | SampleNoPerspective
                    | Flat
            )
        {
            return Err(GenerateError::UnsupportedArrayInterpolator);
        }

        use ir::InterpolationModifier::*;
        Ok(Some(match interpolation_modifier {
            CenterPerspective => make_attribute("center_perspective"),
            CentroidPerspective => make_attribute("centroid_perspective"),
            SamplePerspective => make_attribute("sample_perspective"),
            CenterNoPerspective => make_attribute("center_no_perspective"),
            CentroidNoPerspective => make_attribute("centroid_no_perspective"),
            SampleNoPerspective => make_attribute("sample_no_perspective"),
            Flat => make_attribute("flat"),
            Point | Line | Triangle | LineAdj | TriangleAdj => {
                return Err(GenerateError::UnsupportedGeometryShader)
            }
            Vertices | Primitives | Indices | Payload => {
                return Err(GenerateError::UnimplementedMeshShader)
            }
        }))
    } else {
        Ok(None)
    }
}

/// Generate attribute for a semantic
fn generate_semantic_annotation(
    semantic: &Option<ir::Semantic>,
) -> Result<Option<ast::Attribute>, GenerateError> {
    if let Some(semantic) = semantic {
        use ir::Semantic::*;
        let name = match semantic {
            DispatchThreadId => "thread_position_in_grid",
            GroupId => "threadgroup_position_in_grid",
            GroupIndex => "thread_index_in_threadgroup",
            GroupThreadId => "thread_position_in_threadgroup",
            VertexId => "vertex_id",
            InstanceId => "instance_id",
            PrimitiveId => "primitive_id",
            Position => "position",
            Target(i) => {
                return Ok(Some(ast::Attribute {
                    name: Vec::from([Located::none(String::from("color"))]),
                    arguments: Vec::from([Located::none(ast::Expression::Literal(
                        ast::Literal::IntUntyped(*i as u64),
                    ))]),
                    two_square_brackets: true,
                }))
            }
            Depth => {
                return Ok(Some(ast::Attribute {
                    name: Vec::from([Located::none(String::from("depth"))]),
                    arguments: Vec::from([Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("any"),
                    ))]),
                    two_square_brackets: true,
                }))
            }
            DepthGreaterEqual => {
                return Ok(Some(ast::Attribute {
                    name: Vec::from([Located::none(String::from("depth"))]),
                    arguments: Vec::from([Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("greater"),
                    ))]),
                    two_square_brackets: true,
                }))
            }
            DepthLessEqual => {
                return Ok(Some(ast::Attribute {
                    name: Vec::from([Located::none(String::from("depth"))]),
                    arguments: Vec::from([Located::none(ast::Expression::Identifier(
                        ast::ScopedIdentifier::trivial("less"),
                    ))]),
                    two_square_brackets: true,
                }))
            }
            User(_) => {
                // TODO: We do not currently implement tagging user semantics
                return Ok(None);
            }
        };
        Ok(Some(ast::Attribute {
            name: Vec::from([Located::none(String::from(name))]),
            arguments: Vec::new(),
            two_square_brackets: true,
        }))
    } else {
        Ok(None)
    }
}

/// Generate type name
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
            if x != 1 {
                base.layout.0.identifiers[0].node += &format!("{x}");
            }
            base
        }
        ir::TypeLayer::Matrix(st, x, y) => {
            let base_name = match context.module.type_registry.get_type_layer(st) {
                ir::TypeLayer::Scalar(ir::ScalarType::Float16) => "half",
                ir::TypeLayer::Scalar(ir::ScalarType::Float32) => "float",
                _ => return Err(GenerateError::UnsupportedNonFloatMatrix),
            };

            if x == 1 || y == 1 {
                return Err(GenerateError::UnsupportedUnitMatrix);
            }

            let name = format!("{base_name}{y}x{x}");

            let mut type_name = ast::Type::trivial(&name);

            // TODO: Mark <metal_matrix> as required
            type_name
                .layout
                .0
                .identifiers
                .insert(0, Located::none("metal".to_string()));

            type_name
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
            fn build_texture(
                name: &'static str,
                ty: ir::TypeId,
                read_write: bool,
                context: &mut GenerateContext,
            ) -> Result<ast::Type, GenerateError> {
                let component_type = match context.module.type_registry.extract_scalar(ty) {
                    Some(scalar) => match scalar {
                        ir::ScalarType::Float16
                        | ir::ScalarType::Float32
                        | ir::ScalarType::Int32
                        | ir::ScalarType::UInt32 => generate_scalar_type(scalar)?,
                        _ => return Err(GenerateError::UnsupportedTextureComponentType),
                    },
                    _ => return Err(GenerateError::UnsupportedTextureComponentType),
                };

                let mut type_args = Vec::from([ast::ExpressionOrType::Type(component_type)]);

                if read_write {
                    let access = ast::Expression::Identifier(ast::ScopedIdentifier {
                        base: ast::ScopedIdentifierBase::Relative,
                        identifiers: Vec::from([
                            Located::none(String::from("metal")),
                            Located::none(String::from("access")),
                            Located::none(String::from("read_write")),
                        ]),
                    });
                    type_args.push(ast::ExpressionOrType::Expression(Located::none(access)));
                }

                let ty = ast::Type::from_layout(ast::TypeLayout(
                    metal_lib_identifier(name),
                    type_args.into_boxed_slice(),
                ));

                Ok(ty)
            }

            fn build_buffer(
                id: ir::TypeId,
                read_write: bool,
                declarator: &mut ast::Declarator,
                context: &mut GenerateContext,
            ) -> Result<ast::Type, GenerateError> {
                let mut ty = generate_type(id, context)?;

                assert!(!ty
                    .modifiers
                    .modifiers
                    .contains(&Located::none(ast::TypeModifier::Const)));

                if !read_write {
                    ty.modifiers
                        .modifiers
                        .insert(0, Located::none(ast::TypeModifier::Const));
                }

                let prev_declarator = std::mem::replace(declarator, ast::Declarator::Empty);
                *declarator = ast::Declarator::Pointer(ast::PointerDeclarator {
                    attributes: Vec::new(),
                    qualifiers: ast::TypeModifierSet::new(),
                    inner: Box::new(prev_declarator),
                });

                Ok(ty)
            }

            use ir::ObjectType::*;
            match ot {
                Buffer(ty) => build_texture("texture_buffer", ty, false, context)?,
                RWBuffer(ty) => build_texture("texture_buffer", ty, true, context)?,
                ByteAddressBuffer => return Err(GenerateError::UnimplementedObject(ot)),
                RWByteAddressBuffer => return Err(GenerateError::UnimplementedObject(ot)),
                BufferAddress => return Err(GenerateError::UnimplementedObject(ot)),
                RWBufferAddress => return Err(GenerateError::UnimplementedObject(ot)),
                StructuredBuffer(id) => build_buffer(id, false, &mut declarator, context)?,
                RWStructuredBuffer(id) => build_buffer(id, true, &mut declarator, context)?,

                Texture2D(ty) => build_texture("texture2d", ty, false, context)?,
                Texture2DMips(_) | Texture2DMipsSlice(_) => {
                    // We do not expect intermediate values for mips to get exported
                    panic!("trying to export Texture2D.mips intermediates");
                }

                Texture2DArray(ty) => build_texture("texture2d_array", ty, false, context)?,
                Texture2DArrayMips(_) | Texture2DArrayMipsSlice(_) => {
                    // We do not expect intermediate values for mips to get exported
                    panic!("trying to export Texture2DArray.mips intermediates");
                }

                RWTexture2D(ty) => build_texture("texture2d", ty, true, context)?,

                RWTexture2DArray(ty) => build_texture("texture2d_array", ty, true, context)?,

                TextureCube(ty) => build_texture("texturecube", ty, false, context)?,

                TextureCubeArray(ty) => build_texture("texturecube_array", ty, false, context)?,

                Texture3D(ty) => build_texture("texture3d", ty, false, context)?,
                Texture3DMips(_) | Texture3DMipsSlice(_) => {
                    panic!("trying to export Texture3D.mips intermediates");
                }
                RWTexture3D(ty) => build_texture("texture3d", ty, true, context)?,

                ConstantBuffer(_) => return Err(GenerateError::UnimplementedObject(ot)),
                SamplerState => ast::Type::from(metal_lib_identifier("sampler")),
                SamplerComparisonState => ast::Type::from(metal_lib_identifier("sampler")),

                TriangleStream(_) => return Err(GenerateError::UnsupportedGeometryShader),

                RaytracingAccelerationStructure | RayQuery(_) | RayDesc => {
                    return Err(GenerateError::UnimplementedRaytracing)
                }
            }
        }
        ir::TypeLayer::Array(ty, len) => {
            let array_size = len
                .map(ast::Literal::IntUntyped)
                .map(ast::Expression::Literal)
                .map(Located::none);

            let unarray_id = context.module.type_registry.get_non_array_id(ty);
            let unmod_id = context.module.type_registry.remove_modifier(unarray_id);
            let unmod_tyl = context.module.type_registry.get_type_layer(unmod_id);
            let is_object = matches!(unmod_tyl, ir::TypeLayer::Object(_));

            if is_object {
                let (base, inner_declarator) =
                    generate_type_impl(ty, declarator, suppress_const_volatile, context)?;

                let array_id = metal_lib_identifier(if array_size.is_none() {
                    "array_ref"
                } else {
                    "array"
                });

                let modified_args = match array_size {
                    Some(array_size) => Vec::from([
                        ast::ExpressionOrType::Type(base),
                        ast::ExpressionOrType::Expression(array_size),
                    ]),
                    None => Vec::from([ast::ExpressionOrType::Type(base)]),
                };

                let modified = ast::Type::from_layout(ast::TypeLayout(
                    array_id,
                    modified_args.into_boxed_slice(),
                ));

                declarator = inner_declarator;

                modified
            } else {
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
                match &mut declarator {
                    ast::Declarator::Pointer(ast::PointerDeclarator { qualifiers, .. }) => {
                        for modifier in modifiers.iter().rev() {
                            qualifiers.prepend(Located::none(*modifier));
                        }
                    }
                    ast::Declarator::Empty
                    | ast::Declarator::Identifier(..)
                    | ast::Declarator::Array(..) => {
                        for modifier in modifiers.iter().rev() {
                            base.modifiers.prepend(Located::none(*modifier));
                        }
                    }
                    _ => panic!(
                        "Failed to insert modifiers into declarator: {:?} ->  {:?}",
                        modifiers, declarator,
                    ),
                }
            }

            base
        }
        _ => todo!("Type layout not implemented: {:?}", tyl),
    };

    Ok((base, declarator))
}

/// Generate type for a scalar type
fn generate_scalar_type(ty: ir::ScalarType) -> Result<ast::Type, GenerateError> {
    let name = match ty {
        ir::ScalarType::Bool => "bool",
        ir::ScalarType::IntLiteral => panic!("int literal should not be required on output"),
        ir::ScalarType::Int32 => "int",
        ir::ScalarType::UInt32 => "uint",
        ir::ScalarType::FloatLiteral => panic!("float literal should not be required on output"),
        ir::ScalarType::Float16 => "half",
        ir::ScalarType::Float32 => "float",
        ir::ScalarType::Float64 => return Err(GenerateError::UnsupportedDouble),
    };

    Ok(ast::Type::trivial(name))
}

/// Generate expression or type from a constant value or type
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

/// Generate an expression from a constant value
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

/// Generate a statement
fn generate_statement(
    statement: &ir::Statement,
    context: &mut GenerateContext,
) -> Result<ast::Statement, GenerateError> {
    let mut attributes = Vec::new();
    for attribute in &statement.attributes {
        if let Some(attr) = generate_statement_attribute(attribute, context)? {
            attributes.push(attr);
        }
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
        ir::StatementKind::Discard => ast::StatementKind::Expression(ast::Expression::Call(
            Box::new(Located::none(ast::Expression::Identifier(
                metal_lib_identifier("discard_fragment"),
            ))),
            Vec::new(),
            Vec::new(),
        )),
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

/// Generate attribute for a statement
fn generate_statement_attribute(
    attribute: &ir::StatementAttribute,
    _: &mut GenerateContext,
) -> Result<Option<ast::Attribute>, GenerateError> {
    let ast = match attribute {
        ir::StatementAttribute::Branch => None,
        ir::StatementAttribute::Flatten => None,
        ir::StatementAttribute::Unroll(_) => None,
        ir::StatementAttribute::Loop => None,
        ir::StatementAttribute::Fastopt => None,
        ir::StatementAttribute::AllowUavCondition => None,
    };
    Ok(ast)
}

/// Generate a variable definition
fn generate_variable_definition(
    def: &ir::VarDef,
    context: &mut GenerateContext,
) -> Result<ast::VarDef, GenerateError> {
    let var_def = context.module.variable_registry.get_local_variable(def.id);

    let storage_modifier = match var_def.storage_class {
        ir::LocalStorage::Local => None,
        ir::LocalStorage::Static => Some(ast::TypeModifier::Static),
    };

    if var_def.precise {
        return Err(GenerateError::UnsupportedPrecise);
    };

    let name = context.get_variable_name(def.id)?.to_string();
    let (base, declarator) = generate_type_and_declarator(var_def.type_id, &name, false, context)?;

    let local_type = prepend_modifiers(base, &[storage_modifier]);

    let init = generate_initializer(&def.init, var_def.type_id, context)?;

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

/// Generate a block of statements
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

/// Generate a for init expression
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

/// Generate an expression
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
        ir::Expression::MatrixSwizzle(_, _) => {
            return Err(GenerateError::UnimplementedMatrixSwizzle);
        }
        ir::Expression::ArraySubscript(expr_object, expr_index) => {
            // Find the type of the expression - without modifiers
            let type_id = match expr_object.get_type(context.module) {
                Ok(ty) => context.module.type_registry.remove_modifier(ty.0),
                Err(_) => return Err(GenerateError::InvalidModule),
            };

            // The type without modifiers
            let type_id = context.module.type_registry.remove_modifier(type_id);

            // Check for a matrix type and fail
            // The default indexing will index into a column instead of a row and we do not currently rewrite to work around this
            if let ir::TypeLayer::Matrix(_, _, _) =
                context.module.type_registry.get_type_layer(type_id)
            {
                return Err(GenerateError::UnimplementedMatrixIndex);
            }

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
            let output_type = match expr.get_type(context.module) {
                Ok(ty) => ty,
                Err(_) => return Err(GenerateError::InvalidModule),
            };
            generate_intrinsic_op(intrinsic, exprs, output_type.0, context)?
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
    let mut args = generate_invocation_args(arguments, context)?;

    // Add arguments for passing global variable references into subfunctions
    let parameters_for_globals = context.function_required_globals.get(&id).unwrap();
    for gid in parameters_for_globals {
        match context.global_variable_modes.get(gid).unwrap() {
            GlobalMode::Parameter(_, arg_expr, _) => {
                args.push(Located::none(arg_expr.clone()));
            }
            GlobalMode::Constant => panic!("global does not require a parameter"),
        }
    }

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
        InvokeHelper(IntrinsicHelper),
        Unimplemented,
    }

    use ir::Intrinsic::*;
    use GenerateError::UnimplementedIntrinsic;
    let form = match &intrinsic {
        AllMemoryBarrier => Form::Unimplemented,
        AllMemoryBarrierWithGroupSync => Form::Unimplemented,
        DeviceMemoryBarrier => Form::Unimplemented,
        DeviceMemoryBarrierWithGroupSync => Form::Unimplemented,
        GroupMemoryBarrier => Form::Unimplemented,
        GroupMemoryBarrierWithGroupSync => Form::Unimplemented,

        AsInt => Form::Unimplemented,
        AsUInt => Form::Unimplemented,
        AsFloat => Form::Unimplemented,
        AsDouble => Form::Unimplemented,

        All => Form::Unimplemented,
        Any => Form::Unimplemented,
        And => Form::Unimplemented,
        Or => Form::Unimplemented,
        Select => Form::Unimplemented,

        Abs => Form::Invoke("abs"),

        // Transcendental functions
        Acos => Form::Unimplemented,
        Asin => Form::Unimplemented,
        Atan => Form::Unimplemented,
        Atan2 => Form::Unimplemented,
        Cos => Form::Unimplemented,
        Cosh => Form::Unimplemented,
        Sin => Form::Unimplemented,
        Sinh => Form::Unimplemented,
        Sincos => Form::Unimplemented,
        Tan => Form::Unimplemented,
        Tanh => Form::Unimplemented,
        Sqrt => Form::Unimplemented,
        RcpSqrt => Form::Unimplemented,
        Pow => Form::Unimplemented,
        Exp => Form::Unimplemented,
        Exp2 => Form::Unimplemented,
        Log => Form::Unimplemented,
        Log2 => Form::Unimplemented,
        Log10 => Form::Unimplemented,

        F16ToF32 => Form::Unimplemented,
        F32ToF16 => Form::Unimplemented,

        Floor => Form::Unimplemented,
        Ceil => Form::Unimplemented,
        Trunc => Form::Unimplemented,
        Round => Form::Unimplemented,
        Frac => Form::Unimplemented,
        Modf => Form::Unimplemented,
        Fmod => Form::Unimplemented,

        IsNaN => Form::Unimplemented,
        IsInfinite => Form::Unimplemented,
        IsFinite => Form::Unimplemented,

        Length => Form::Unimplemented,
        Normalize => Form::Unimplemented,
        Rcp => Form::Unimplemented,

        Reflect => Form::Unimplemented,
        Refract => Form::Unimplemented,

        CountBits => Form::Unimplemented,
        ReverseBits => Form::Unimplemented,
        FirstBitHigh => Form::Unimplemented,
        FirstBitLow => Form::Unimplemented,

        Saturate => Form::Unimplemented,

        Sign => Form::Unimplemented,

        Cross => Form::Unimplemented,
        Distance => Form::Unimplemented,
        Dot => Form::Unimplemented,

        Mul => {
            assert_eq!(exprs.len(), 2);
            let left = generate_expression(&exprs[0], context)?;
            let right = generate_expression(&exprs[1], context)?;
            let ast = ast::Expression::BinaryOperation(
                ast::BinOp::Multiply,
                Box::new(Located::none(left)),
                Box::new(Located::none(right)),
            );
            return Ok(ast);
        }

        Min => Form::Unimplemented,
        Max => Form::Unimplemented,

        Step => Form::Unimplemented,

        Clamp => Form::Unimplemented,
        Lerp => Form::Unimplemented,
        SmoothStep => Form::Unimplemented,

        Transpose => Form::Unimplemented,
        Determinant => Form::Unimplemented,

        DDX => Form::Unimplemented,
        DDXCoarse => Form::Unimplemented,
        DDXFine => Form::Unimplemented,
        DDY => Form::Unimplemented,
        DDYCoarse => Form::Unimplemented,
        DDYFine => Form::Unimplemented,

        InterlockedAdd => Form::Unimplemented,
        InterlockedAnd => Form::Unimplemented,
        InterlockedCompareExchange => Form::Unimplemented,
        InterlockedCompareStore => Form::Unimplemented,
        InterlockedExchange => Form::Unimplemented,
        InterlockedMax => Form::Unimplemented,
        InterlockedMin => Form::Unimplemented,
        InterlockedOr => Form::Unimplemented,
        InterlockedXor => Form::Unimplemented,

        NonUniformResourceIndex => Form::Unimplemented,

        WaveGetLaneCount => Form::Unimplemented,
        WaveGetLaneIndex => Form::Unimplemented,
        WaveIsFirstLane => Form::Unimplemented,
        WaveActiveAnyTrue => Form::Unimplemented,
        WaveActiveAllTrue => Form::Unimplemented,
        WaveActiveBallot => Form::Unimplemented,
        WaveReadLaneAt => Form::Unimplemented,
        WaveReadLaneFirst => Form::Unimplemented,
        WaveActiveAllEqual => Form::Unimplemented,
        WaveActiveCountBits => Form::Unimplemented,
        WaveActiveSum => Form::Unimplemented,
        WaveActiveProduct => Form::Unimplemented,
        WaveActiveBitAnd => Form::Unimplemented,
        WaveActiveBitOr => Form::Unimplemented,
        WaveActiveBitXor => Form::Unimplemented,
        WaveActiveMin => Form::Unimplemented,
        WaveActiveMax => Form::Unimplemented,
        WavePrefixCountBits => Form::Unimplemented,
        WavePrefixProduct => Form::Unimplemented,
        WavePrefixSum => Form::Unimplemented,
        QuadReadAcrossX => Form::Unimplemented,
        QuadReadAcrossY => Form::Unimplemented,
        QuadReadAcrossDiagonal => Form::Unimplemented,
        QuadReadLaneAt => Form::Unimplemented,

        SetMeshOutputCounts => Form::Unimplemented,
        DispatchMesh => Form::Unimplemented,

        BufferGetDimensions => Form::Unimplemented,
        BufferLoad => Form::Unimplemented,

        RWBufferGetDimensions => Form::Unimplemented,
        RWBufferLoad => Form::Unimplemented,

        StructuredBufferGetDimensions => Form::Unimplemented,
        StructuredBufferLoad => Form::Unimplemented,

        RWStructuredBufferGetDimensions => Form::Unimplemented,
        RWStructuredBufferLoad => Form::Unimplemented,

        ByteAddressBufferGetDimensions => Form::Unimplemented,
        ByteAddressBufferLoad => Form::Unimplemented,
        ByteAddressBufferLoad2 => Form::Unimplemented,
        ByteAddressBufferLoad3 => Form::Unimplemented,
        ByteAddressBufferLoad4 => Form::Unimplemented,
        ByteAddressBufferLoadT => Form::Unimplemented,

        RWByteAddressBufferGetDimensions => Form::Unimplemented,
        RWByteAddressBufferLoad => Form::Unimplemented,
        RWByteAddressBufferLoad2 => Form::Unimplemented,
        RWByteAddressBufferLoad3 => Form::Unimplemented,
        RWByteAddressBufferLoad4 => Form::Unimplemented,
        RWByteAddressBufferLoadT => Form::Unimplemented,
        RWByteAddressBufferStore => Form::Unimplemented,
        RWByteAddressBufferStore2 => Form::Unimplemented,
        RWByteAddressBufferStore3 => Form::Unimplemented,
        RWByteAddressBufferStore4 => Form::Unimplemented,
        RWByteAddressBufferInterlockedAdd => Form::Unimplemented,
        RWByteAddressBufferInterlockedAnd => Form::Unimplemented,
        RWByteAddressBufferInterlockedCompareExchange => Form::Unimplemented,
        RWByteAddressBufferInterlockedCompareStore => Form::Unimplemented,
        RWByteAddressBufferInterlockedExchange => Form::Unimplemented,
        RWByteAddressBufferInterlockedMax => Form::Unimplemented,
        RWByteAddressBufferInterlockedMin => Form::Unimplemented,
        RWByteAddressBufferInterlockedOr => Form::Unimplemented,
        RWByteAddressBufferInterlockedXor => Form::Unimplemented,

        BufferAddressLoad => Form::Unimplemented,

        RWBufferAddressLoad => Form::Unimplemented,
        RWBufferAddressStore => Form::Unimplemented,

        Texture2DGetDimensions => {
            Form::InvokeHelper(IntrinsicHelper::GetDimensions(GetDimensionsHelper {
                dim: Dim::Tex2D,
                read_write: false,
            }))
        }
        Texture2DLoad => Form::InvokeHelper(IntrinsicHelper::Load(LoadHelper {
            dim: Dim::Tex2D,
            read_write: false,
            has_offset: exprs.len() >= 3,
            has_status: exprs.len() >= 4,
        })),
        Texture2DSample => Form::InvokeHelper(IntrinsicHelper::Sample(SampleHelper {
            dim: Dim::Tex2D,
            has_offset: exprs.len() >= 4,
            has_clamp: exprs.len() >= 5,
            has_status: exprs.len() >= 6,
        })),
        Texture2DSampleBias => Form::Unimplemented,
        Texture2DSampleCmp => Form::Unimplemented,
        Texture2DSampleCmpLevelZero => Form::Unimplemented,
        Texture2DSampleGrad => Form::Unimplemented,
        Texture2DSampleLevel => Form::Unimplemented,
        Texture2DGatherRed => Form::Unimplemented,
        Texture2DGatherGreen => Form::Unimplemented,
        Texture2DGatherBlue => Form::Unimplemented,
        Texture2DGatherAlpha => Form::Unimplemented,
        Texture2DGatherCmpRed => Form::Unimplemented,
        Texture2DGatherCmpGreen => Form::Unimplemented,
        Texture2DGatherCmpBlue => Form::Unimplemented,
        Texture2DGatherCmpAlpha => Form::Unimplemented,

        Texture2DArrayGetDimensions => {
            Form::InvokeHelper(IntrinsicHelper::GetDimensions(GetDimensionsHelper {
                dim: Dim::Tex2DArray,
                read_write: false,
            }))
        }
        Texture2DArrayLoad => Form::InvokeHelper(IntrinsicHelper::Load(LoadHelper {
            dim: Dim::Tex2DArray,
            read_write: false,
            has_offset: exprs.len() >= 3,
            has_status: exprs.len() >= 4,
        })),
        Texture2DArraySample => Form::InvokeHelper(IntrinsicHelper::Sample(SampleHelper {
            dim: Dim::Tex2DArray,
            has_offset: exprs.len() >= 4,
            has_clamp: exprs.len() >= 5,
            has_status: exprs.len() >= 6,
        })),
        Texture2DArraySampleBias => Form::Unimplemented,
        Texture2DArraySampleCmp => Form::Unimplemented,
        Texture2DArraySampleCmpLevelZero => Form::Unimplemented,
        Texture2DArraySampleGrad => Form::Unimplemented,
        Texture2DArraySampleLevel => Form::Unimplemented,
        Texture2DArrayGatherRed => Form::Unimplemented,
        Texture2DArrayGatherGreen => Form::Unimplemented,
        Texture2DArrayGatherBlue => Form::Unimplemented,
        Texture2DArrayGatherAlpha => Form::Unimplemented,
        Texture2DArrayGatherCmpRed => Form::Unimplemented,
        Texture2DArrayGatherCmpGreen => Form::Unimplemented,
        Texture2DArrayGatherCmpBlue => Form::Unimplemented,
        Texture2DArrayGatherCmpAlpha => Form::Unimplemented,

        RWTexture2DGetDimensions => {
            Form::InvokeHelper(IntrinsicHelper::GetDimensions(GetDimensionsHelper {
                dim: Dim::Tex2D,
                read_write: true,
            }))
        }
        RWTexture2DLoad => Form::InvokeHelper(IntrinsicHelper::Load(LoadHelper {
            dim: Dim::Tex2D,
            read_write: true,
            has_offset: false,
            has_status: exprs.len() >= 3,
        })),

        RWTexture2DArrayGetDimensions => {
            Form::InvokeHelper(IntrinsicHelper::GetDimensions(GetDimensionsHelper {
                dim: Dim::Tex2DArray,
                read_write: true,
            }))
        }
        RWTexture2DArrayLoad => Form::InvokeHelper(IntrinsicHelper::Load(LoadHelper {
            dim: Dim::Tex2DArray,
            read_write: true,
            has_offset: false,
            has_status: exprs.len() >= 3,
        })),

        TextureCubeSample => Form::InvokeHelper(IntrinsicHelper::Sample(SampleHelper {
            dim: Dim::TexCube,
            has_offset: false,
            has_clamp: exprs.len() >= 4,
            has_status: exprs.len() >= 5,
        })),
        TextureCubeSampleLevel => Form::Unimplemented,

        TextureCubeArraySample => Form::InvokeHelper(IntrinsicHelper::Sample(SampleHelper {
            dim: Dim::TexCubeArray,
            has_offset: false,
            has_clamp: exprs.len() >= 4,
            has_status: exprs.len() >= 5,
        })),
        TextureCubeArraySampleLevel => Form::Unimplemented,

        Texture3DGetDimensions => {
            Form::InvokeHelper(IntrinsicHelper::GetDimensions(GetDimensionsHelper {
                dim: Dim::Tex3D,
                read_write: false,
            }))
        }
        Texture3DLoad => Form::InvokeHelper(IntrinsicHelper::Load(LoadHelper {
            dim: Dim::Tex3D,
            read_write: false,
            has_offset: exprs.len() >= 3,
            has_status: exprs.len() >= 4,
        })),
        Texture3DSample => Form::InvokeHelper(IntrinsicHelper::Sample(SampleHelper {
            dim: Dim::Tex3D,
            has_offset: exprs.len() >= 4,
            has_clamp: exprs.len() >= 5,
            has_status: exprs.len() >= 6,
        })),
        Texture3DSampleBias => Form::Unimplemented,
        Texture3DSampleGrad => Form::Unimplemented,
        Texture3DSampleLevel => Form::Unimplemented,

        RWTexture3DGetDimensions => {
            Form::InvokeHelper(IntrinsicHelper::GetDimensions(GetDimensionsHelper {
                dim: Dim::Tex3D,
                read_write: true,
            }))
        }
        RWTexture3DLoad => Form::InvokeHelper(IntrinsicHelper::Load(LoadHelper {
            dim: Dim::Tex3D,
            read_write: true,
            has_offset: false,
            has_status: exprs.len() >= 3,
        })),

        TriangleStreamAppend | TriangleStreamRestartStrip => {
            return Err(GenerateError::UnsupportedGeometryShader)
        }

        RayQueryTraceRayInline
        | RayQueryProceed
        | RayQueryAbort
        | RayQueryCommittedStatus
        | RayQueryCandidateType
        | RayQueryCandidateProceduralPrimitiveNonOpaque
        | RayQueryCommitNonOpaqueTriangleHit
        | RayQueryCommitProceduralPrimitiveHit
        | RayQueryRayFlags
        | RayQueryWorldRayOrigin
        | RayQueryWorldRayDirection
        | RayQueryRayTMin
        | RayQueryCandidateTriangleRayT
        | RayQueryCommittedRayT
        | RayQueryCandidateInstanceIndex
        | RayQueryCandidateInstanceID
        | RayQueryCandidateInstanceContributionToHitGroupIndex
        | RayQueryCandidateGeometryIndex
        | RayQueryCandidatePrimitiveIndex
        | RayQueryCandidateObjectRayOrigin
        | RayQueryCandidateObjectRayDirection
        | RayQueryCandidateObjectToWorld3x4
        | RayQueryCandidateObjectToWorld4x3
        | RayQueryCandidateWorldToObject3x4
        | RayQueryCandidateWorldToObject4x3
        | RayQueryCommittedInstanceIndex
        | RayQueryCommittedInstanceID
        | RayQueryCommittedInstanceContributionToHitGroupIndex
        | RayQueryCommittedGeometryIndex
        | RayQueryCommittedPrimitiveIndex
        | RayQueryCommittedObjectRayOrigin
        | RayQueryCommittedObjectRayDirection
        | RayQueryCommittedObjectToWorld3x4
        | RayQueryCommittedObjectToWorld4x3
        | RayQueryCommittedWorldToObject3x4
        | RayQueryCommittedWorldToObject4x3
        | RayQueryCandidateTriangleBarycentrics
        | RayQueryCandidateTriangleFrontFace
        | RayQueryCommittedTriangleBarycentrics
        | RayQueryCommittedTriangleFrontFace => return Err(GenerateError::UnimplementedRaytracing),
    };

    let expr = match form {
        Form::Invoke(s) => {
            let identifier = metal_lib_identifier(s);
            let object = Box::new(Located::none(ast::Expression::Identifier(identifier)));
            let type_args = generate_template_type_args(tys, context)?;
            let args = generate_invocation_args(exprs, context)?;
            ast::Expression::Call(object, type_args, args)
        }
        Form::InvokeHelper(helper) => {
            context.required_helpers.insert(helper);

            let name = get_intrinsic_helper_name(helper);
            let identifier = ast::ScopedIdentifier {
                base: ast::ScopedIdentifierBase::Relative,
                identifiers: Vec::from([
                    Located::none("helper".to_string()),
                    Located::none(name.to_string()),
                ]),
            };
            let object = Box::new(Located::none(ast::Expression::Identifier(identifier)));
            let type_args = generate_template_type_args(tys, context)?;
            let args = generate_invocation_args(exprs, context)?;
            ast::Expression::Call(object, type_args, args)
        }
        Form::Unimplemented => return Err(UnimplementedIntrinsic(intrinsic.clone())),
    };
    Ok(expr)
}

/// Write out an intrinsic operator expression
fn generate_intrinsic_op(
    intrinsic: &ir::IntrinsicOp,
    exprs: &Vec<ir::Expression>,
    output_type: ir::TypeId,
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
            let output = ast::Expression::BinaryOperation(
                op,
                Box::new(Located::none(left)),
                Box::new(Located::none(right)),
            );

            // Operations on enums return enums in RSSL but integers in MSL
            let unmod_ty = context.module.type_registry.remove_modifier(output_type);
            let tyl = context.module.type_registry.get_type_layer(unmod_ty);
            if matches!(tyl, ir::TypeLayer::Enum(_)) {
                // Emit a cast that returns back to the enum type
                // If we would then cast again this will be redundant
                let cast_target = generate_type(output_type, context)?;
                ast::Expression::Cast(cast_target, Box::new(Located::none(output)))
            } else {
                output
            }
        }
    };
    Ok(expr)
}

/// Generate an initializer
fn generate_initializer(
    init_opt: &Option<ir::Initializer>,
    type_id: ir::TypeId,
    context: &mut GenerateContext,
) -> Result<Option<ast::Initializer>, GenerateError> {
    if let Some(init) = init_opt {
        Ok(Some(generate_initializer_inner(init, context)?))
    } else if context.module.type_registry.is_const(type_id) {
        Err(GenerateError::UninitializedConstant)
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

/// Generate  a set of function call arguments
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

/// Generate a set of template arguments
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

/// Generate a struct definition
fn generate_struct(
    decl: &ir::StructDefinition,
    context: &mut GenerateContext,
) -> Result<ast::StructDefinition, GenerateError> {
    let mut members = Vec::new();

    for member in &decl.members {
        if member.precise {
            return Err(GenerateError::UnsupportedPrecise);
        };

        let (ty, declarator) =
            generate_type_and_declarator(member.type_id, &member.name, true, context)?;

        let semantic = generate_semantic_annotation(&member.semantic)?;
        if semantic.is_some() {
            return Err(GenerateError::UnimplementedStructWithSemantic);
        }

        let interpolation_modifier =
            generate_interpolation_modifier(&member.interpolation_modifier, &declarator)?;

        let init_declarator = ast::InitDeclarator {
            declarator,
            location_annotations: Vec::new(),
            init: None,
        };

        members.push(ast::StructEntry::Variable(ast::StructMember {
            ty,
            defs: Vec::from([init_declarator]),
            attributes: [semantic, interpolation_modifier]
                .into_iter()
                .flatten()
                .collect::<Vec<_>>(),
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

/// Generate an enum definition
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

/// Construct an ast scoped identifier from a generator scoped name
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

/// Construct an ast scoped identifier from a name from the metal standard library
fn metal_lib_identifier(name: &str) -> ast::ScopedIdentifier {
    ast::ScopedIdentifier {
        // metal is a reserved name so we can drop the leading ::
        base: ast::ScopedIdentifierBase::Relative,
        identifiers: Vec::from([
            Located::none(String::from("metal")),
            Located::none(String::from(name)),
        ]),
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

/// Construct a basic attribute
fn make_attribute(name: &str) -> ast::Attribute {
    ast::Attribute {
        name: Vec::from([Located::none(String::from(name))]),
        arguments: Vec::new(),
        two_square_brackets: true,
    }
}

/// Contextual state for MSL generator
struct GenerateContext<'m> {
    module: &'m ir::Module,
    name_map: NameMap,
    global_variable_modes: HashMap<ir::GlobalId, GlobalMode>,
    function_required_globals: HashMap<ir::FunctionId, Vec<ir::GlobalId>>,
    required_helpers: HashSet<IntrinsicHelper>,

    pipeline_description: PipelineDescription,
}

impl<'m> GenerateContext<'m> {
    /// Start a new generate state
    fn new(module: &'m ir::Module) -> Self {
        let name_map = NameMap::build(module, RESERVED_NAMES, false);

        GenerateContext {
            module,
            name_map,
            global_variable_modes: HashMap::new(),
            function_required_globals: HashMap::new(),
            required_helpers: HashSet::new(),
            pipeline_description: PipelineDescription {
                bind_groups: Vec::new(),
            },
        }
    }

    /// Get the name of a global variable
    fn get_global_name(&self, id: ir::GlobalId) -> Result<&str, GenerateError> {
        Ok(self.name_map.get_name_leaf(NameSymbol::GlobalVariable(id)))
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
            None => Err(GenerateError::InvalidModule),
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