use rssl_ir as ir;
use rssl_text::Located;
use std::fmt::Write;

use crate::*;

pub struct ExportedSource {
    pub source: String,
    pub pipeline_description: PipelineDescription,
}

/// Error result when exporting to HLSL fails
#[derive(Debug)]
pub enum ExportError {
    NamelessId,
}

/// Export ir module to HLSL
///
/// We assume the generated code will be built with:
/// * HLSL version 2021
/// * If using half: -enable-16bit-types
pub fn export_to_hlsl(module: &ir::Module) -> Result<ExportedSource, ExportError> {
    let mut context = ExportContext::new(module);
    let mut output_string = String::new();

    // Generate binding info
    for decl in &module.root_definitions {
        analyse_bindings(decl, &mut context)?;
    }

    // Create inline constant buffers from bindings
    generate_inline_constant_buffers(
        &module.inline_constant_buffers,
        &mut output_string,
        &mut context,
    )?;

    export_root_definitions(
        module,
        &module.root_definitions,
        &mut output_string,
        &mut context,
    )?;
    context.new_line(&mut output_string);

    Ok(ExportedSource {
        source: output_string,
        pipeline_description: context.pipeline_description,
    })
}

/// Check bindings
fn analyse_bindings(
    decl: &ir::RootDefinition,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match decl {
        ir::RootDefinition::Struct(_)
        | ir::RootDefinition::StructTemplate(_)
        | ir::RootDefinition::Enum(_)
        | ir::RootDefinition::Function(_) => {}
        ir::RootDefinition::ConstantBuffer(id) => {
            let cb = &context.module.cbuffer_registry[id.0 as usize];
            if let Some(api_slot) = cb.api_binding {
                assert_eq!(cb.lang_binding.set, api_slot.set);

                let binding = DescriptorBinding {
                    name: context.get_constant_buffer_name(cb.id)?.to_string(),
                    api_binding: api_slot.location,
                    descriptor_type: DescriptorType::ConstantBuffer,
                };

                context.register_binding(api_slot.set, binding);
            }
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let decl = &context.module.global_registry[id.0 as usize];
            let unmodified_id = context.module.type_registry.remove_modifier(decl.type_id);
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
                _ => DescriptorType::PushConstants,
            };

            if let Some(api_slot) = decl.api_slot {
                assert_eq!(decl.lang_slot.set, api_slot.set);

                let binding = DescriptorBinding {
                    name: context.get_global_name(decl.id)?.to_string(),
                    api_binding: api_slot.location,
                    descriptor_type,
                };

                context.register_binding(api_slot.set, binding);
            }
        }
        ir::RootDefinition::Namespace(_, decls) => {
            for decl in decls {
                analyse_bindings(decl, context)?;
            }
        }
    }
    Ok(())
}

/// Take the binding definitions and build the inline constant buffers required
fn generate_inline_constant_buffers(
    inline_constant_buffers: &[ir::InlineConstantBuffer],
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    for buffer in inline_constant_buffers {
        let bind_group = &mut context.pipeline_description.bind_groups[buffer.set as usize];
        let mut found_size = 0;

        write!(output, "struct InlineDescriptor{}\n{{\n", buffer.set).unwrap();
        for binding in &bind_group.bindings {
            if let ApiLocation::InlineConstant(offset) = binding.api_binding {
                assert!(offset + 8 <= buffer.size_in_bytes);
                writeln!(
                    output,
                    "    [[vk::offset({})]] uint64_t {};",
                    offset, binding.name
                )
                .unwrap();
                found_size += 8;
            }
        }
        assert_eq!(buffer.size_in_bytes, found_size);

        output.push_str("};\n");
        write!(
            output,
            "[[vk::binding({})]] ConstantBuffer<InlineDescriptor{}> g_inlineDescriptor{};",
            buffer.api_location, buffer.set, buffer.set
        )
        .unwrap();

        assert_eq!(bind_group.inline_constants, None);
        bind_group.inline_constants = Some(InlineConstantBuffer {
            api_location: buffer.api_location,
            size_in_bytes: buffer.size_in_bytes,
        });
    }

    Ok(())
}

/// Export ir root definitions to HLSL
fn export_root_definitions(
    module: &ir::Module,
    decls: &[ir::RootDefinition],
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let mut last_was_variable = false;
    for decl in decls {
        export_root_definition(module, decl, &mut last_was_variable, output, context)?;
    }
    Ok(())
}

/// Export ir root definition to HLSL
fn export_root_definition(
    module: &ir::Module,
    decl: &ir::RootDefinition,
    last_was_variable: &mut bool,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    // Start a new line
    context.new_line(output);

    // Start a second new line if we are not two sequential variables
    match decl {
        ir::RootDefinition::GlobalVariable(_) => {
            if !*last_was_variable {
                context.new_line(output);
            }
            *last_was_variable = true;
        }
        _ => {
            context.new_line(output);
            *last_was_variable = false;
        }
    }

    match decl {
        ir::RootDefinition::Struct(id) => {
            let sd = &module.struct_registry[id.0 as usize];
            export_struct(sd, output, context)?;
        }
        ir::RootDefinition::StructTemplate(_) => {
            todo!("RootDefinition::StructTemplate")
        }
        ir::RootDefinition::Enum(id) => {
            export_enum(*id, output, context)?;
        }
        ir::RootDefinition::ConstantBuffer(id) => {
            let cb = &module.cbuffer_registry[id.0 as usize];
            export_constant_buffer(cb, output, context)?;
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let decl = &module.global_registry[id.0 as usize];
            export_global_variable(decl, output, context)?;
        }
        ir::RootDefinition::Function(id) => {
            export_function(*id, output, context)?;
        }
        ir::RootDefinition::Namespace(name, decls) => {
            output.push_str("namespace ");
            output.push_str(name);
            output.push_str(" {");
            export_root_definitions(module, decls, output, context)?;
            context.new_line(output);
            context.new_line(output);
            output.push_str("} // namespace ");
            output.push_str(name);
        }
    }
    Ok(())
}

/// Export ir global variable to HLSL
fn export_global_variable(
    decl: &ir::GlobalVariable,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let is_extern = decl.storage_class == ir::GlobalStorage::Extern;
    if is_extern && context.module.flags.requires_vk_binding {
        export_vk_binding_annotation(&decl.api_slot, output)?;
    }

    // const is implicit on extern variables so there is no need to write it out
    // volatile is not valid on globals so happens to get suppressed at the same time
    let mut suppress_const_volatile = false;
    match decl.storage_class {
        ir::GlobalStorage::Extern => suppress_const_volatile = true,
        ir::GlobalStorage::Static => output.push_str("static "),
        ir::GlobalStorage::GroupShared => output.push_str("groupshared "),
    }

    let mut array_part = String::new();
    export_type_impl(
        decl.type_id,
        suppress_const_volatile,
        output,
        &mut array_part,
        context,
    )?;

    output.push(' ');

    let name = context.get_global_name(decl.id)?;
    output.push_str(name);
    output.push_str(&array_part);

    if is_extern && !context.module.flags.requires_vk_binding {
        export_register_annotation(&decl.api_slot, output)?;
    }

    if let Some(ir::ApiBinding {
        set,
        location: ApiLocation::InlineConstant(_),
        ..
    }) = decl.api_slot
    {
        assert_eq!(decl.init, None);
        write!(output, " = g_inlineDescriptor{set}.{name}").unwrap();
    } else {
        export_initializer(&decl.init, output, context)?;
    }

    output.push(';');

    Ok(())
}

/// Export register slot annotation to HLSL
fn export_register_annotation(
    slot: &Option<ir::ApiBinding>,
    output: &mut String,
) -> Result<(), ExportError> {
    if let Some(slot) = &slot {
        output.push_str(" : register(");
        match slot.slot_type {
            Some(register_type) => write!(output, "{register_type}").unwrap(),
            None => panic!("Exporter requires register types in api binding metadata"),
        }
        match slot.location {
            ApiLocation::Index(index) => write!(output, "{index}").unwrap(),
            ApiLocation::InlineConstant(_) => {
                panic!("export_register_annotation did not expect an inline constant")
            }
        }
        if slot.set != 0 {
            write!(output, ", space{}", slot.set).unwrap();
        }
        output.push(')');
    }

    Ok(())
}

/// Export vk::binding annotation to HLSL
fn export_vk_binding_annotation(
    slot: &Option<ir::ApiBinding>,
    output: &mut String,
) -> Result<(), ExportError> {
    if let Some(slot) = &slot {
        assert_eq!(slot.slot_type, None);
        match slot.location {
            ApiLocation::Index(index) => {
                if slot.set != 0 {
                    write!(output, "[[vk::binding({index}, {})]] ", slot.set).unwrap()
                } else {
                    write!(output, "[[vk::binding({index})]] ").unwrap()
                }
            }
            ApiLocation::InlineConstant(_) => {
                panic!("export_register_annotation did not expect an inline constant")
            }
        }
    }

    Ok(())
}

/// Export ir function to HLSL
fn export_function(
    id: ir::FunctionId,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let sig = context.module.function_registry.get_function_signature(id);
    let decl = context
        .module
        .function_registry
        .get_function_implementation(id)
        .as_ref()
        .unwrap();

    for attribute in &decl.attributes {
        export_function_attribute(attribute, output, context)?;
    }

    export_type(sig.return_type.return_type, output, context)?;
    output.push(' ');

    output.push_str(context.get_function_name(id)?);

    // Scope also contains the names of parameters
    context.push_scope(decl.scope_block.1.clone());
    context.push_indent();

    output.push('(');

    if let Some((last, main)) = decl.params.split_last() {
        for param in main {
            export_function_param(param, output, context)?;
            output.push_str(", ");
        }
        export_function_param(last, output, context)?;
    }

    output.push(')');

    export_semantic_annotation(&sig.return_type.semantic, output)?;

    output.push_str(" {");
    for statement in &decl.scope_block.0 {
        export_statement(statement, output, context)?;
    }

    context.pop_scope();
    context.pop_indent();

    if !decl.scope_block.0.is_empty() {
        context.new_line(output);
    }
    output.push('}');

    Ok(())
}

/// Export ir function attribute to HLSL
fn export_function_attribute(
    attr: &ir::FunctionAttribute,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match attr {
        ir::FunctionAttribute::NumThreads(x, y, z) => {
            output.push_str("[numthreads(");
            export_expression(x, output, context)?;
            output.push_str(", ");
            export_expression(y, output, context)?;
            output.push_str(", ");
            export_expression(z, output, context)?;
            output.push_str(")]");
        }
        ir::FunctionAttribute::WaveSize(size) => {
            output.push_str("[WaveSize(");
            export_expression(size, output, context)?;
            output.push_str(")]");
        }
        ir::FunctionAttribute::OutputTopology(s) => {
            output.push_str("[outputtopology(\"");
            output.push_str(s);
            output.push_str("\")]");
        }
    }
    context.new_line(output);
    Ok(())
}

/// Export ir function parameter to HLSL
fn export_function_param(
    param: &ir::FunctionParam,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match param.param_type.input_modifier {
        // payload modified parameters require an explicit in instead of an implicit in
        ir::InputModifier::In
            if param.param_type.interpolation_modifier
                == Some(ir::InterpolationModifier::Payload) =>
        {
            output.push_str("in ")
        }
        ir::InputModifier::In => {}
        ir::InputModifier::Out => output.push_str("out "),
        ir::InputModifier::InOut => output.push_str("inout "),
    }

    if param.param_type.precise {
        output.push_str("precise ");
    }

    export_interpolation_modifier(&param.param_type.interpolation_modifier, output)?;

    let mut array_part = String::new();
    export_type_for_def(param.param_type.type_id, output, &mut array_part, context)?;

    output.push(' ');

    output.push_str(context.get_variable_name_direct(param.id)?);
    output.push_str(&array_part);

    export_semantic_annotation(&param.semantic, output)?;

    Ok(())
}

/// Export variable interpolation modifier to HLSL
fn export_interpolation_modifier(
    interpolation_modifier: &Option<ir::InterpolationModifier>,
    output: &mut String,
) -> Result<(), ExportError> {
    if let Some(interpolation_modifier) = &interpolation_modifier {
        match interpolation_modifier {
            ir::InterpolationModifier::NoInterpolation => output.push_str("nointerpolation "),
            ir::InterpolationModifier::Linear => output.push_str("linear "),
            ir::InterpolationModifier::Centroid => output.push_str("centroid "),
            ir::InterpolationModifier::NoPerspective => output.push_str("noperspective "),
            ir::InterpolationModifier::Sample => output.push_str("sample "),
            ir::InterpolationModifier::Vertices => output.push_str("vertices "),
            ir::InterpolationModifier::Primitives => output.push_str("primitives "),
            ir::InterpolationModifier::Indices => output.push_str("indices "),
            ir::InterpolationModifier::Payload => output.push_str("payload "),
        }
    }
    Ok(())
}

/// Export variable semantic annotation to HLSL
fn export_semantic_annotation(
    semantic: &Option<ir::Semantic>,
    output: &mut String,
) -> Result<(), ExportError> {
    if let Some(semantic) = &semantic {
        output.push_str(" : ");
        match semantic {
            ir::Semantic::DispatchThreadId => output.push_str("SV_DispatchThreadID"),
            ir::Semantic::GroupId => output.push_str("SV_GroupID"),
            ir::Semantic::GroupIndex => output.push_str("SV_GroupIndex"),
            ir::Semantic::GroupThreadId => output.push_str("SV_GroupThreadID"),
            ir::Semantic::VertexId => output.push_str("SV_VertexID"),
            ir::Semantic::InstanceId => output.push_str("SV_InstanceID"),
            ir::Semantic::PrimitiveId => output.push_str("SV_PrimitiveID"),
            ir::Semantic::Position => output.push_str("SV_Position"),
            ir::Semantic::Target(i) => write!(output, "SV_Target{i}").unwrap(),
            ir::Semantic::Depth => output.push_str("SV_Depth"),
            ir::Semantic::DepthGreaterEqual => output.push_str("SV_DepthGreaterEqual"),
            ir::Semantic::DepthLessEqual => output.push_str("SV_DepthLessEqual"),
            ir::Semantic::User(s) => output.push_str(s),
        }
    }
    Ok(())
}

/// Export ir type to HLSL
fn export_type(
    ty: ir::TypeId,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let mut array_part = String::new();
    export_type_for_def(ty, output, &mut array_part, context)?;
    output.push_str(&array_part);
    Ok(())
}

/// Export ir type to HLSL
fn export_type_for_def(
    ty: ir::TypeId,
    output: &mut String,
    output_array: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    export_type_impl(ty, false, output, output_array, context)?;
    Ok(())
}

/// Export ir type to HLSL
fn export_type_impl(
    ty: ir::TypeId,
    suppress_const_volatile: bool,
    output: &mut String,
    output_array: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let tyl = context.module.type_registry.get_type_layer(ty);
    match tyl {
        ir::TypeLayer::Void => write!(output, "void").unwrap(),
        ir::TypeLayer::Scalar(st) => write!(output, "{}", export_scalar_type(st)?).unwrap(),
        ir::TypeLayer::Vector(st, x) => {
            // Allowed types in a vector should construct valid vector type names
            // This will break down for vector of enums
            export_type_impl(st, false, output, output_array, context)?;
            write!(output, "{x}").unwrap()
        }
        ir::TypeLayer::Matrix(st, x, y) => {
            // Allowed types in a matrix should construct valid matrix type names
            // This will break down for vector of enums
            export_type_impl(st, false, output, output_array, context)?;
            write!(output, "{x}x{y}").unwrap()
        }
        ir::TypeLayer::Struct(id) => {
            write!(output, "{}", context.get_struct_name_full(id)?).unwrap()
        }
        ir::TypeLayer::Enum(id) => write!(output, "{}", context.get_enum_name_full(id)?).unwrap(),
        ir::TypeLayer::Object(ot) => {
            match ot {
                ir::ObjectType::Buffer(ty) => {
                    output.push_str("Buffer<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::RWBuffer(ty) => {
                    output.push_str("RWBuffer<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::ByteAddressBuffer => output.push_str("ByteAddressBuffer"),
                ir::ObjectType::RWByteAddressBuffer => output.push_str("RWByteAddressBuffer"),
                ir::ObjectType::BufferAddress | ir::ObjectType::RWBufferAddress
                    if context.module.flags.requires_buffer_address =>
                {
                    output.push_str("uint64_t")
                }
                ir::ObjectType::BufferAddress => output.push_str("ByteAddressBuffer"),
                ir::ObjectType::RWBufferAddress => output.push_str("RWByteAddressBuffer"),
                ir::ObjectType::StructuredBuffer(ty) => {
                    output.push_str("StructuredBuffer<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::RWStructuredBuffer(ty) => {
                    output.push_str("RWStructuredBuffer<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }

                ir::ObjectType::Texture2D(ty) => {
                    output.push_str("Texture2D<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::Texture2DMips(_) | ir::ObjectType::Texture2DMipsSlice(_) => {
                    // We do not expect intermediate values for mips to get exported
                    // It's possible to make shaders in HLSL that reference these but these are all silly use cases
                    // Trying to use mips-slice will likely break HLSL anyway as you can't make intermediates of these
                    panic!("trying to export Texture2D.mips intermediates");
                }

                ir::ObjectType::Texture2DArray(ty) => {
                    output.push_str("Texture2DArray<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::Texture2DArrayMips(_)
                | ir::ObjectType::Texture2DArrayMipsSlice(_) => {
                    // We do not expect intermediate values for mips to get exported
                    // It's possible to make shaders in HLSL that reference these but these are all silly use cases
                    // Trying to use mips-slice will likely break HLSL anyway as you can't make intermediates of these
                    panic!("trying to export Texture2DArray.mips intermediates");
                }

                ir::ObjectType::RWTexture2D(ty) => {
                    output.push_str("RWTexture2D<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }

                ir::ObjectType::RWTexture2DArray(ty) => {
                    output.push_str("RWTexture2DArray<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }

                ir::ObjectType::TextureCube(ty) => {
                    output.push_str("TextureCube<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }

                ir::ObjectType::TextureCubeArray(ty) => {
                    output.push_str("TextureCubeArray<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }

                ir::ObjectType::Texture3D(ty) => {
                    output.push_str("Texture3D<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::Texture3DMips(_) | ir::ObjectType::Texture3DMipsSlice(_) => {
                    panic!("trying to export Texture3D.mips intermediates");
                }
                ir::ObjectType::RWTexture3D(ty) => {
                    output.push_str("RWTexture3D<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }

                ir::ObjectType::ConstantBuffer(ty) => {
                    output.push_str("ConstantBuffer<");
                    export_type(ty, output, context)?;
                    output.push('>');
                }
                ir::ObjectType::SamplerState => {
                    output.push_str("SamplerState");
                }
                ir::ObjectType::SamplerComparisonState => {
                    output.push_str("SamplerComparisonState");
                }
            };
        }
        ir::TypeLayer::Array(ty, len) => {
            export_type_impl(ty, suppress_const_volatile, output, output_array, context)?;
            match len {
                Some(len) => write!(output_array, "[{len}]"),
                None => write!(output_array, "[]"),
            }
            .unwrap()
        }
        ir::TypeLayer::Modifier(mut modifier, ty) => {
            if suppress_const_volatile {
                modifier.is_const = false;
                modifier.volatile = false;
            }
            write!(output, "{modifier:?}").unwrap();
            export_type_impl(ty, false, output, output_array, context)?;
        }
        _ => todo!("Type layout not implemented: {:?}", tyl),
    };

    Ok(())
}

/// Export ir scalar type to HLSL
fn export_scalar_type(ty: ir::ScalarType) -> Result<&'static str, ExportError> {
    Ok(match ty {
        ir::ScalarType::Bool => "bool",
        ir::ScalarType::UntypedInt => panic!("Untyped int should not be required on output"),
        ir::ScalarType::Int => "int",
        ir::ScalarType::UInt => "uint",
        ir::ScalarType::Half => "half",
        ir::ScalarType::Float => "float",
        ir::ScalarType::Double => "double",
    })
}

/// Export ir type or constant parameter to HLSL
fn export_type_or_constant(
    tc: &ir::TypeOrConstant,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match tc {
        ir::TypeOrConstant::Type(ty) => export_type(*ty, output, context),
        ir::TypeOrConstant::Constant(_) => todo!("Non-type template arguments"),
    }
}

/// Export ir literal to HLSL
fn export_literal(literal: &ir::Constant, output: &mut String) -> Result<(), ExportError> {
    match literal {
        ir::Constant::Bool(true) => output.push_str("true"),
        ir::Constant::Bool(false) => output.push_str("false"),
        ir::Constant::UntypedInt(v) => write!(output, "{v}").unwrap(),
        // There is no signed integer suffix - but the way we generate code should avoid overload issues with unsigned integers
        ir::Constant::Int(v) => write!(output, "{v}").unwrap(),
        ir::Constant::UInt(v) => write!(output, "{v}u").unwrap(),
        ir::Constant::Long(v) => write!(output, "{v}l").unwrap(),
        ir::Constant::Half(v) => write!(output, "{v}h").unwrap(),
        ir::Constant::Float(v) if *v == (*v as i64 as f32) => {
            write!(output, "{}.0", *v as i64).unwrap()
        }
        ir::Constant::Float(v) if *v > i64::MAX as f32 || *v < i64::MIN as f32 => {
            write!(output, "{v}.0").unwrap()
        }
        ir::Constant::Float(v) => write!(output, "{v}").unwrap(),
        ir::Constant::Double(v) => write!(output, "{v}L").unwrap(),
        ir::Constant::String(_) => panic!("literal string not expected in output"),
        ir::Constant::Enum(_, _) => panic!("literal enum not expected in output"),
    }
    Ok(())
}

/// Export ir statement to HLSL
fn export_statement(
    statement: &ir::Statement,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    for attribute in &statement.attributes {
        export_statement_attribute(attribute, output, context)?;
    }

    context.new_line(output);
    match &statement.kind {
        ir::StatementKind::Expression(expr) => {
            export_expression(expr, output, context)?;
            output.push(';');
        }
        ir::StatementKind::Var(def) => {
            export_variable_definition(def, output, context)?;
            output.push(';');
        }
        ir::StatementKind::Block(block) => {
            enter_scope_block(block, context);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::StatementKind::If(cond, block) => {
            enter_scope_block(block, context);
            output.push_str("if (");
            export_expression(cond, output, context)?;
            output.push(')');
            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::StatementKind::IfElse(cond, block_true, block_false) => {
            enter_scope_block(block_true, context);
            output.push_str("if (");
            export_expression(cond, output, context)?;
            output.push(')');
            context.new_line(output);
            export_scope_block(block_true, output, context)?;
            context.pop_scope();

            context.new_line(output);
            output.push_str("else");
            context.new_line(output);
            enter_scope_block(block_false, context);
            export_scope_block(block_false, output, context)?;
            context.pop_scope();
        }
        ir::StatementKind::For(init, cond, inc, block) => {
            enter_scope_block(block, context);

            output.push_str("for (");
            export_for_init(init, output, context)?;
            output.push_str("; ");
            export_expression(cond, output, context)?;
            output.push_str("; ");
            export_expression(inc, output, context)?;
            output.push(')');

            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::StatementKind::While(cond, block) => {
            enter_scope_block(block, context);

            output.push_str("while (");
            export_expression(cond, output, context)?;
            output.push(')');

            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::StatementKind::Break => output.push_str("break;"),
        ir::StatementKind::Continue => output.push_str("continue;"),
        ir::StatementKind::Discard => output.push_str("discard;"),
        ir::StatementKind::Return(expr_opt) => {
            output.push_str("return");
            if let Some(expr) = expr_opt {
                output.push(' ');
                export_expression(expr, output, context)?;
            }
            output.push(';');
        }
    }
    Ok(())
}

/// Export statement attribute to HLSL
fn export_statement_attribute(
    attribute: &ir::StatementAttribute,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    context.new_line(output);
    match attribute {
        ir::StatementAttribute::Branch => output.push_str("[branch]"),
        ir::StatementAttribute::Flatten => output.push_str("[flatten]"),
        ir::StatementAttribute::Unroll(None) => output.push_str("[unroll]"),
        ir::StatementAttribute::Unroll(Some(v)) => write!(output, "[unroll({v})]").unwrap(),
        ir::StatementAttribute::Loop => output.push_str("[loop]"),
        ir::StatementAttribute::Fastopt => output.push_str("[fastopt]"),
        ir::StatementAttribute::AllowUavCondition => output.push_str("[allow_uav_condition]"),
    };
    Ok(())
}

/// Export ir variable definition to HLSL
fn export_variable_definition(
    def: &ir::VarDef,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match def.storage_class {
        ir::LocalStorage::Local => {}
        ir::LocalStorage::Static => output.push_str("static "),
    }
    if def.precise {
        output.push_str("precise ");
    }
    let mut array_part = String::new();
    export_type_for_def(def.type_id, output, &mut array_part, context)?;
    export_variable_definition_no_type(def, &array_part, output, context)
}

/// Export ir single variable definition to HLSL
fn export_variable_definition_no_type(
    def: &ir::VarDef,
    array_part: &str,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push(' ');
    output.push_str(context.get_variable_name_direct(def.id)?);
    output.push_str(array_part);

    export_initializer(&def.init, output, context)
}

/// Add scoped variables from a scope block
fn enter_scope_block(block: &ir::ScopeBlock, context: &mut ExportContext) {
    context.push_scope(block.1.clone());
}

/// Export block of ir statements to HLSL
fn export_scope_block(
    block: &ir::ScopeBlock,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push('{');
    context.push_indent();
    for statement in &block.0 {
        export_statement(statement, output, context)?;
    }
    context.pop_indent();
    context.new_line(output);
    output.push('}');
    Ok(())
}

/// Export ir initialiser expression to HLSL
fn export_for_init(
    init: &ir::ForInit,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match init {
        ir::ForInit::Empty => Ok(()),
        ir::ForInit::Expression(expr) => export_expression(expr, output, context),
        ir::ForInit::Definitions(defs) => {
            let (head, tail) = defs.split_first().unwrap();

            // Extract type information from first definition to ensure later definitions match
            let mut head_core_part = String::new();
            let mut head_array_part = String::new();
            export_type_for_def(
                head.type_id,
                &mut head_core_part,
                &mut head_array_part,
                context,
            )?;

            export_variable_definition(head, output, context)?;
            for def in tail {
                output.push(',');
                // Extract type information from the non-first definition
                let mut cur_core_part = String::new();
                let mut cur_array_part = String::new();
                export_type_for_def(
                    def.type_id,
                    &mut cur_core_part,
                    &mut cur_array_part,
                    context,
                )?;
                // The base type definitions should all match - so the generated string should be the same
                assert_eq!(head_core_part, cur_core_part);
                // The array part of the type definition is used here - and may vary between definitions
                export_variable_definition_no_type(def, &cur_array_part, output, context)?;
            }
            Ok(())
        }
    }
}

/// Export ir expression to HLSL
fn export_expression(
    expr: &ir::Expression,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    export_subexpression(
        expr,
        u32::max_value(),
        OperatorSide::Middle,
        output,
        context,
    )
}

enum OperatorSide {
    Left,
    Right,
    Middle,
    CommaList,
}

enum Associativity {
    LeftToRight,
    RightToLeft,
    None,
}

/// Export an expression within another expression
fn export_subexpression(
    expr: &ir::Expression,
    outer_precedence: u32,
    side: OperatorSide,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let prec = get_expression_precedence(expr);
    let requires_paren = match prec.cmp(&outer_precedence) {
        std::cmp::Ordering::Greater => true,
        std::cmp::Ordering::Less => false,
        std::cmp::Ordering::Equal => !matches!(
            (side, get_precedence_associativity(prec)),
            (OperatorSide::Left, Associativity::LeftToRight)
                | (OperatorSide::Right, Associativity::RightToLeft)
                | (OperatorSide::Middle, _)
        ),
    };
    if requires_paren {
        output.push('(')
    }
    match expr {
        ir::Expression::Literal(lit) => export_literal(lit, output)?,
        ir::Expression::Variable(v) => output.push_str(context.get_variable_name(*v)?),
        ir::Expression::MemberVariable(name) => output.push_str(name),
        ir::Expression::Global(v) => output.push_str(context.get_global_name(*v)?),
        ir::Expression::ConstantVariable(_, name) => output.push_str(name),
        ir::Expression::EnumValue(id) => {
            write!(output, "{}", context.get_enum_value_name_full(*id)?).unwrap()
        }
        ir::Expression::TernaryConditional(expr_cond, expr_true, expr_false) => {
            export_subexpression(expr_cond, prec, OperatorSide::Left, output, context)?;
            output.push_str(" ? ");
            export_subexpression(expr_true, prec, OperatorSide::Middle, output, context)?;
            output.push_str(" : ");
            export_subexpression(expr_false, prec, OperatorSide::Right, output, context)?;
        }
        ir::Expression::Sequence(exprs) => {
            for i in 0..exprs.len() {
                let side = if i == 0 {
                    OperatorSide::Left
                } else if i == exprs.len() - 1 {
                    OperatorSide::Right
                } else {
                    OperatorSide::Middle
                };
                export_subexpression(&exprs[i], prec, side, output, context)?;
                if i != exprs.len() - 1 {
                    output.push_str(", ");
                }
            }
        }
        ir::Expression::Swizzle(expr_object, swizzle) => {
            export_subexpression(expr_object, prec, OperatorSide::Left, output, context)?;
            output.push('.');
            for channel in swizzle {
                match channel {
                    ir::SwizzleSlot::X => output.push('x'),
                    ir::SwizzleSlot::Y => output.push('y'),
                    ir::SwizzleSlot::Z => output.push('z'),
                    ir::SwizzleSlot::W => output.push('w'),
                }
            }
        }
        ir::Expression::ArraySubscript(expr_object, expr_index) => {
            export_subexpression(expr_object, prec, OperatorSide::Left, output, context)?;
            output.push('[');
            export_subexpression(expr_index, prec, OperatorSide::Middle, output, context)?;
            output.push(']');
        }
        ir::Expression::Constructor(type_id, args) => {
            export_type(*type_id, output, context)?;
            output.push('(');
            if let Some((last, main)) = args.split_last() {
                for slot in main {
                    export_subexpression(&slot.expr, 17, OperatorSide::Middle, output, context)?;
                    output.push_str(", ");
                }
                export_subexpression(&last.expr, 17, OperatorSide::Middle, output, context)?;
            }
            output.push(')');
        }
        ir::Expression::Cast(type_id, expr) => {
            output.push('(');
            export_type(*type_id, output, context)?;
            output.push(')');
            export_subexpression(expr, prec, OperatorSide::Right, output, context)?;
        }
        ir::Expression::SizeOf(type_id) => {
            output.push_str("sizeof(");
            export_type(*type_id, output, context)?;
            output.push(')');
        }
        ir::Expression::Member(expr, name) => {
            export_subexpression(expr, prec, OperatorSide::Left, output, context)?;
            output.push('.');
            output.push_str(name)
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
                export_intrinsic_function(intrinsic, tys, exprs, prec, output, context)?;
            } else {
                export_user_call(*id, ct, tys, exprs, output, context)?;
            }
        }
        ir::Expression::IntrinsicOp(intrinsic, tys, exprs) => {
            export_intrinsic_op(intrinsic, tys, exprs, prec, output, context)?;
        }
    }
    if requires_paren {
        output.push(')')
    }
    Ok(())
}

/// Get the precedence of an expression when it is HLSL
fn get_expression_precedence(expr: &ir::Expression) -> u32 {
    match expr {
        ir::Expression::Literal(_)
        | ir::Expression::Variable(_)
        | ir::Expression::MemberVariable(_)
        | ir::Expression::Global(_)
        | ir::Expression::EnumValue(_) => 0,

        ir::Expression::ConstantVariable(_, _) => 1,
        ir::Expression::TernaryConditional(_, _, _) => 16,
        ir::Expression::Sequence(_) => 17,
        ir::Expression::Swizzle(_, _) => 2,
        ir::Expression::ArraySubscript(_, _) => 2,
        ir::Expression::Constructor(_, _) => 2,
        ir::Expression::Cast(_, _) => 3,
        ir::Expression::SizeOf(_) => 3,
        ir::Expression::Member(_, _) => 2,
        ir::Expression::Call(_, _, _) => 2,
        ir::Expression::IntrinsicOp(intrinsic, _, _) => {
            use ir::IntrinsicOp::*;
            match &intrinsic {
                PrefixIncrement => 3,
                PrefixDecrement => 3,
                PostfixIncrement => 2,
                PostfixDecrement => 2,
                Plus => 3,
                Minus => 3,
                LogicalNot => 3,
                BitwiseNot => 3,

                Add => 6,
                Subtract => 6,
                Multiply => 5,
                Divide => 5,
                Modulus => 5,
                LeftShift => 7,
                RightShift => 7,
                BitwiseAnd => 11,
                BitwiseOr => 13,
                BitwiseXor => 12,
                BooleanAnd => 14,
                BooleanOr => 15,
                LessThan => 9,
                LessEqual => 9,
                GreaterThan => 9,
                GreaterEqual => 9,
                Equality => 10,
                Inequality => 10,
                Assignment => 16,
                SumAssignment => 16,
                DifferenceAssignment => 16,
                ProductAssignment => 16,
                QuotientAssignment => 16,
                RemainderAssignment => 16,
            }
        }
    }
}

/// Get the associativity of a precedence level
fn get_precedence_associativity(prec: u32) -> Associativity {
    match prec {
        1 | 2 => Associativity::LeftToRight,
        3 => Associativity::RightToLeft,
        4..=15 => Associativity::LeftToRight,
        16 => Associativity::RightToLeft,
        17 => Associativity::LeftToRight,

        _ => Associativity::None,
    }
}

/// Write out a call expression for a user function
fn export_user_call(
    id: ir::FunctionId,
    ct: &ir::CallType,
    tys: &[ir::TypeOrConstant],
    exprs: &Vec<ir::Expression>,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let (object, arguments) = match ct {
        ir::CallType::FreeFunction | ir::CallType::MethodInternal => (None, exprs.as_slice()),
        ir::CallType::MethodExternal => (Some(&exprs[0]), &exprs[1..]),
    };
    if let Some(object) = object {
        export_subexpression(object, 2, OperatorSide::Left, output, context)?;
        output.push('.');
    }
    if *ct == ir::CallType::FreeFunction {
        write!(output, "{}", context.get_function_name_full(id)?).unwrap();
    } else {
        // Assume all method calls (both on an object and internally) have
        // sufficient qualification that they do not need the full name
        write!(output, "{}", context.get_function_name(id)?).unwrap();
    }
    export_template_type_args(tys, output, context)?;
    export_invocation_args(arguments, output, context)?;
    Ok(())
}

/// Write out an intrinsic function expression
fn export_intrinsic_function(
    intrinsic: &ir::Intrinsic,
    tys: &[ir::TypeOrConstant],
    exprs: &Vec<ir::Expression>,
    prec: u32,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
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

        TextureCubeArraySample => Form::Method("Sample"),

        Texture3DGetDimensions => Form::Method("GetDimensions"),
        Texture3DLoad => Form::Method("Load"),
        Texture3DSample => Form::Method("Sample"),
        Texture3DSampleBias => Form::Method("SampleBias"),
        Texture3DSampleGrad => Form::Method("SampleGrad"),
        Texture3DSampleLevel => Form::Method("SampleLevel"),

        RWTexture3DGetDimensions => Form::Method("GetDimensions"),
        RWTexture3DLoad => Form::Method("Load"),
    };

    match form {
        Form::Invoke(s) => {
            output.push_str(s);
            // Do not export type arguments - the only templated non-method is not templated in HLSL so is not allowed type argments
            export_invocation_args(exprs, output, context)?;
        }
        Form::AddressMethod(_, s) if context.module.flags.requires_buffer_address => {
            assert!(
                exprs.len() >= 2,
                "Buffer address intrinsic expects at least an address and offset"
            );

            output.push_str(s);
            export_template_type_args(tys, output, context)?;
            output.push('(');
            if let [addr, offset, rest @ ..] = exprs.as_slice() {
                export_subexpression(addr, 6, OperatorSide::Left, output, context)?;
                output.push_str(" + uint64_t(");
                export_subexpression(offset, 17, OperatorSide::Middle, output, context)?;
                output.push(')');

                for expr in rest {
                    output.push_str(", ");
                    export_subexpression(expr, 17, OperatorSide::CommaList, output, context)?;
                }
            } else {
                panic!("Incorrect number of arguments in buffer address load");
            }
            output.push(')');
        }
        Form::Method(s) | Form::AddressMethod(s, _) => {
            assert!(!exprs.is_empty());
            export_subexpression(&exprs[0], prec, OperatorSide::Left, output, context)?;
            output.push('.');
            output.push_str(s);
            export_template_type_args(tys, output, context)?;
            export_invocation_args(&exprs[1..], output, context)?;
        }
    }
    Ok(())
}

/// Write out an intrinsic operator expression
fn export_intrinsic_op(
    intrinsic: &ir::IntrinsicOp,
    tys: &Vec<Located<ir::TypeOrConstant>>,
    exprs: &Vec<ir::Expression>,
    prec: u32,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    enum Form {
        Unary(&'static str),
        UnaryPostfix(&'static str),
        Binary(&'static str),
    }

    use ir::IntrinsicOp::*;
    let form = match &intrinsic {
        PrefixIncrement => Form::Unary("++"),
        PrefixDecrement => Form::Unary("--"),
        PostfixIncrement => Form::UnaryPostfix("++"),
        PostfixDecrement => Form::UnaryPostfix("--"),
        Plus => Form::Unary("+"),
        Minus => Form::Unary("-"),
        LogicalNot => Form::Unary("!"),
        BitwiseNot => Form::Unary("~"),

        Add => Form::Binary("+"),
        Subtract => Form::Binary("-"),
        Multiply => Form::Binary("*"),
        Divide => Form::Binary("/"),
        Modulus => Form::Binary("%"),
        LeftShift => Form::Binary("<<"),
        RightShift => Form::Binary(">>"),
        BitwiseAnd => Form::Binary("&"),
        BitwiseOr => Form::Binary("|"),
        BitwiseXor => Form::Binary("^"),
        BooleanAnd => Form::Binary("&&"),
        BooleanOr => Form::Binary("||"),
        LessThan => Form::Binary("<"),
        LessEqual => Form::Binary("<="),
        GreaterThan => Form::Binary(">"),
        GreaterEqual => Form::Binary(">="),
        Equality => Form::Binary("=="),
        Inequality => Form::Binary("!="),
        Assignment => Form::Binary("="),
        SumAssignment => Form::Binary("+="),
        DifferenceAssignment => Form::Binary("-="),
        ProductAssignment => Form::Binary("*="),
        QuotientAssignment => Form::Binary("/="),
        RemainderAssignment => Form::Binary("%="),
    };

    match form {
        Form::Unary(s) => {
            assert!(tys.is_empty());
            assert_eq!(exprs.len(), 1);
            output.push_str(s);
            export_subexpression(&exprs[0], prec, OperatorSide::Right, output, context)?;
        }
        Form::UnaryPostfix(s) => {
            assert!(tys.is_empty());
            assert_eq!(exprs.len(), 1);
            export_subexpression(&exprs[0], prec, OperatorSide::Left, output, context)?;
            output.push_str(s);
        }
        Form::Binary(s) => {
            assert!(tys.is_empty());
            assert_eq!(exprs.len(), 2);
            export_subexpression(&exprs[0], prec, OperatorSide::Left, output, context)?;
            output.push(' ');
            output.push_str(s);
            output.push(' ');
            export_subexpression(&exprs[1], prec, OperatorSide::Right, output, context)?;
        }
    }
    Ok(())
}

/// Export ir variable initializer to HLSL
fn export_initializer(
    init_opt: &Option<ir::Initializer>,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    if let Some(init) = init_opt {
        output.push_str(" = ");
        export_initializer_inner(init, output, context)?;
    }
    Ok(())
}

/// Export ir variable initializer part to HLSL
fn export_initializer_inner(
    init: &ir::Initializer,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match init {
        ir::Initializer::Expression(expr) => export_expression(expr, output, context)?,
        ir::Initializer::Aggregate(exprs) => {
            output.push_str("{ ");
            let (head, tail) = exprs.split_first().unwrap();
            export_initializer_inner(head, output, context)?;
            for expr in tail {
                output.push_str(", ");
                export_initializer_inner(expr, output, context)?;
            }
            output.push_str(" }");
        }
    }
    Ok(())
}

/// Export function invocation argument list to HLSL
fn export_invocation_args(
    exprs: &[ir::Expression],
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push('(');
    if let Some((last, main)) = exprs.split_last() {
        for expr in main {
            export_subexpression(expr, 17, OperatorSide::CommaList, output, context)?;
            output.push_str(", ");
        }
        export_subexpression(last, 17, OperatorSide::CommaList, output, context)?;
    }
    output.push(')');
    Ok(())
}

/// Export template argument list to HLSL
fn export_template_type_args(
    template_args: &[ir::TypeOrConstant],
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    if let Some((ta_last, ta_main)) = template_args.split_last() {
        output.push('<');
        for ta in ta_main {
            export_type_or_constant(ta, output, context)?;
            output.push_str(", ");
        }
        export_type_or_constant(ta_last, output, context)?;
        output.push('>');
    }
    Ok(())
}

/// Export ir struct to HLSL
fn export_struct(
    decl: &ir::StructDefinition,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push_str("struct ");
    output.push_str(context.get_struct_name(decl.id)?);

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for member in &decl.members {
        context.new_line(output);
        if member.precise {
            output.push_str("precise ");
        }
        export_interpolation_modifier(&member.interpolation_modifier, output)?;
        let mut array_part = String::new();
        export_type_impl(member.type_id, true, output, &mut array_part, context)?;
        output.push(' ');
        output.push_str(&member.name);
        output.push_str(&array_part);
        export_semantic_annotation(&member.semantic, output)?;
        output.push(';');
    }

    for method in &decl.methods {
        context.new_line(output);
        context.new_line(output);
        export_function(*method, output, context)?;
    }

    context.pop_indent();
    context.new_line(output);
    output.push_str("};");

    Ok(())
}

/// Export ir enum to HLSL
fn export_enum(
    id: ir::EnumId,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    output.push_str("enum ");
    output.push_str(context.get_enum_name(id)?);

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for value_id in context.module.enum_registry.get_values(id) {
        let value_data = context.module.enum_registry.get_enum_value(*value_id);

        context.new_line(output);
        output.push_str(context.get_enum_value_name(*value_id)?);
        output.push_str(" = ");
        export_literal(&value_data.value, output)?;
        output.push(',');
    }

    context.pop_indent();
    context.new_line(output);
    output.push_str("};");

    Ok(())
}

/// Export ir constant buffer to HLSL
fn export_constant_buffer(
    decl: &ir::ConstantBuffer,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    if context.module.flags.requires_vk_binding {
        export_vk_binding_annotation(&decl.api_binding, output)?;
    }
    output.push_str("cbuffer ");
    output.push_str(context.get_constant_buffer_name(decl.id)?);
    if !context.module.flags.requires_vk_binding {
        export_register_annotation(&decl.api_binding, output)?;
    }

    context.new_line(output);
    output.push('{');
    context.push_indent();

    for member in &decl.members {
        context.new_line(output);
        let mut array_part = String::new();
        export_type_for_def(member.type_id, output, &mut array_part, context)?;
        output.push(' ');
        output.push_str(&member.name);
        output.push_str(&array_part);
        output.push(';');

        if member.offset.is_some() {
            todo!("Constant buffer variable with packoffset");
        }
    }

    context.pop_indent();
    context.new_line(output);
    output.push('}');

    Ok(())
}

/// Contextual state for exporter
struct ExportContext<'m> {
    module: &'m ir::Module,
    scopes: Vec<ir::ScopedDeclarations>,

    indent: u32,

    pipeline_description: PipelineDescription,
}

impl<'m> ExportContext<'m> {
    /// Start a new exporter state
    fn new(module: &'m ir::Module) -> Self {
        // TODO: Rename declarations so they are unique
        ExportContext {
            module,
            scopes: Vec::new(),
            indent: 0,
            pipeline_description: PipelineDescription {
                bind_groups: Vec::new(),
            },
        }
    }

    /// Push a local scope
    fn push_scope(&mut self, scope_declarations: ir::ScopedDeclarations) {
        // TODO: Make names unique
        self.scopes.push(scope_declarations);
    }

    /// Remove a scope previously added with push_scope
    /// Okay to not call this if we hit an error case as we do not expect to recover from errors
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    /// Increase indentation
    fn push_indent(&mut self) {
        self.indent += 1;
    }

    /// Decrease indentation
    fn pop_indent(&mut self) {
        self.indent -= 1;
    }

    /// Get the name of a global variable
    fn get_global_name(&self, id: ir::GlobalId) -> Result<&str, ExportError> {
        match self.module.global_registry.get(id.0 as usize) {
            Some(name) => Ok(name.name.as_str()),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a function
    fn get_function_name(&self, id: ir::FunctionId) -> Result<&str, ExportError> {
        Ok(self.module.function_registry.get_function_name(id))
    }

    /// Get the full name of a function
    fn get_function_name_full(&self, id: ir::FunctionId) -> Result<&ir::ScopedName, ExportError> {
        Ok(&self
            .module
            .function_registry
            .get_function_name_definition(id)
            .full_name)
    }

    /// Get the name of a struct
    fn get_struct_name(&self, id: ir::StructId) -> Result<&str, ExportError> {
        match self.module.struct_registry.get(id.0 as usize) {
            Some(sd) => Ok(sd.name.as_str()),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the full name of a struct
    fn get_struct_name_full(&self, id: ir::StructId) -> Result<&ir::ScopedName, ExportError> {
        match self.module.struct_registry.get(id.0 as usize) {
            Some(sd) => Ok(&sd.full_name),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of an enum
    fn get_enum_name(&self, id: ir::EnumId) -> Result<&str, ExportError> {
        Ok(&self.module.enum_registry.get_enum_definition(id).name)
    }

    /// Get the full name of an enum
    fn get_enum_name_full(&self, id: ir::EnumId) -> Result<&ir::ScopedName, ExportError> {
        Ok(&self.module.enum_registry.get_enum_definition(id).full_name)
    }

    /// Get the name of an enum value
    fn get_enum_value_name(&self, id: ir::EnumValueId) -> Result<&str, ExportError> {
        Ok(&self.module.enum_registry.get_enum_value(id).name)
    }

    /// Get the full name of an enum value
    fn get_enum_value_name_full(&self, id: ir::EnumValueId) -> Result<ir::ScopedName, ExportError> {
        let value = self.module.enum_registry.get_enum_value(id);
        let enum_def = self.module.enum_registry.get_enum_definition(value.enum_id);
        let mut name = enum_def.full_name.clone();
        name.0.push(value.name.node.clone());
        Ok(name)
    }

    /// Get the name of a constant buffer
    fn get_constant_buffer_name(&self, id: ir::ConstantBufferId) -> Result<&str, ExportError> {
        match self.module.cbuffer_registry.get(id.0 as usize) {
            Some(cd) => Ok(cd.name.as_str()),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a local variable
    fn get_variable_name(&self, id_ref: ir::VariableRef) -> Result<&str, ExportError> {
        let scope = &self.scopes[self.scopes.len() - (id_ref.1 .0 as usize) - 1];
        match scope.variables.get(&id_ref.0) {
            Some((name, _)) => Ok(name),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the name of a variable declared in the current scope only
    fn get_variable_name_direct(&self, id: ir::VariableId) -> Result<&str, ExportError> {
        self.get_variable_name(ir::VariableRef(id, ir::ScopeRef(0)))
    }

    /// Begin a new line and indent up to the current level of indentation
    fn new_line(&self, output: &mut String) {
        // Skip new lines when we are starting the file as there is nothing before us to separate from
        if output.is_empty() {
            return;
        }

        // Remove previous indentation on empty lines - or trailing whitespace
        let trimmed = output.trim_end_matches(|c| c == ' ');
        if output.len() != trimmed.len() {
            output.truncate(trimmed.len());
        }

        // Push the newline
        output.push('\n');

        // Indent to current indentation level
        for _ in 0..self.indent {
            output.push_str("    ");
        }
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
