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
        | ir::RootDefinition::Function(_) => {}
        ir::RootDefinition::ConstantBuffer(id) => {
            let cb = &context.module.cbuffer_registry[id.0 as usize];
            if let Some(api_slot) = cb.api_binding {
                let lang_slot = cb
                    .lang_binding
                    .expect("Lang slot expected to be present when api slot is present");
                assert_eq!(lang_slot.set, api_slot.set);

                let binding = DescriptorBinding {
                    name: context.get_constant_buffer_name(cb.id)?.to_string(),
                    lang_binding: lang_slot.index,
                    api_binding: api_slot.location,
                    descriptor_type: DescriptorType::ConstantBuffer,
                };

                context.register_binding(api_slot.set, binding);
            }
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let decl = &context.module.global_registry[id.0 as usize];
            let descriptor_type = match decl.global_type.0 {
                ir::TypeLayout::Object(ir::ObjectType::ConstantBuffer(_)) => {
                    DescriptorType::ConstantBuffer
                }
                ir::TypeLayout::Object(ir::ObjectType::ByteAddressBuffer) => {
                    DescriptorType::ByteBuffer
                }
                ir::TypeLayout::Object(ir::ObjectType::RWByteAddressBuffer) => {
                    DescriptorType::RwByteBuffer
                }
                ir::TypeLayout::Object(ir::ObjectType::BufferAddress) => {
                    DescriptorType::BufferAddress
                }
                ir::TypeLayout::Object(ir::ObjectType::RWBufferAddress) => {
                    DescriptorType::RwBufferAddress
                }
                ir::TypeLayout::Object(ir::ObjectType::StructuredBuffer(_)) => {
                    DescriptorType::StructuredBuffer
                }
                ir::TypeLayout::Object(ir::ObjectType::RWStructuredBuffer(_)) => {
                    DescriptorType::RwStructuredBuffer
                }
                ir::TypeLayout::Object(ir::ObjectType::Buffer(_)) => DescriptorType::TexelBuffer,
                ir::TypeLayout::Object(ir::ObjectType::RWBuffer(_)) => {
                    DescriptorType::RwTexelBuffer
                }
                ir::TypeLayout::Object(ir::ObjectType::Texture2D(_)) => DescriptorType::Texture2d,
                ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(_)) => {
                    DescriptorType::RwTexture2d
                }
                _ => DescriptorType::PushConstants,
            };

            if let Some(api_slot) = decl.api_slot {
                let lang_slot = decl
                    .lang_slot
                    .expect("Lang slot expected to be present when api slot is present");
                assert_eq!(lang_slot.set, api_slot.set);

                let binding = DescriptorBinding {
                    name: context.get_global_name(decl.id)?.to_string(),
                    lang_binding: lang_slot.index,
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
        ir::RootDefinition::ConstantBuffer(id) => {
            let cb = &module.cbuffer_registry[id.0 as usize];
            export_constant_buffer(cb, output, context)?;
        }
        ir::RootDefinition::GlobalVariable(id) => {
            let decl = &module.global_registry[id.0 as usize];
            export_global_variable(decl, output, context)?;
        }
        ir::RootDefinition::Function(id) => {
            let fd = &module.function_registry[id.0 as usize];
            export_function(fd.as_ref().unwrap(), output, context)?;
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
    let is_extern = decl.global_type.1 == ir::GlobalStorage::Extern;
    if is_extern && context.module.flags.requires_vk_binding {
        export_vk_binding_annotation(&decl.api_slot, output)?;
    }

    match decl.global_type.1 {
        ir::GlobalStorage::Extern => output.push_str("extern "),
        ir::GlobalStorage::Static => output.push_str("static "),
        ir::GlobalStorage::GroupShared => output.push_str("groupshared "),
    }

    let mut array_part = String::new();
    export_type_for_def(&decl.global_type.0, output, &mut array_part, context)?;

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
        write!(output, " = g_inlineDescriptor{}.{}", set, name).unwrap();
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
            Some(ir::RegisterType::T) => output.push('t'),
            Some(ir::RegisterType::U) => output.push('u'),
            Some(ir::RegisterType::S) => output.push('s'),
            Some(ir::RegisterType::B) => output.push('b'),
            None => panic!("Exporter requires register types in api binding metadata"),
        }
        match slot.location {
            ApiLocation::Index(index) => write!(output, "{}", index).unwrap(),
            ApiLocation::InlineConstant(_) => {
                panic!("export_register_annotation did not expect an inline constant")
            }
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
                    write!(output, "[[vk::binding({}, {})]] ", index, slot.set).unwrap()
                } else {
                    write!(output, "[[vk::binding({})]] ", index).unwrap()
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
    decl: &ir::FunctionDefinition,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    for attribute in &decl.attributes {
        export_function_attribute(attribute, output, context)?;
    }

    export_type(&decl.returntype.return_type, output, context)?;
    output.push(' ');

    output.push_str(context.get_function_name(decl.id)?);

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

    export_semantic_annotation(&decl.returntype.semantic, output)?;

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
    match param.param_type.1 {
        ir::InputModifier::In => {}
        ir::InputModifier::Out => output.push_str("out "),
        ir::InputModifier::InOut => output.push_str("inout "),
    }
    if param.param_type.2.is_some() {
        todo!("Interpolation modifier: {:?}", param.param_type.2);
    }
    let mut array_part = String::new();
    export_type_for_def(&param.param_type.0, output, &mut array_part, context)?;

    output.push(' ');

    output.push_str(context.get_variable_name_direct(param.id)?);
    output.push_str(&array_part);

    export_semantic_annotation(&param.semantic, output)?;

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
            ir::Semantic::Target(i) => write!(output, "SV_Target{}", i).unwrap(),
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
    ty: &ir::TypeLayout,
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
    ty: &ir::TypeLayout,
    output: &mut String,
    output_array: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    export_type_layout_for_def(ty, output, output_array, context)?;
    Ok(())
}

/// Export ir type layout to HLSL
fn export_type_layout(
    tyl: &ir::TypeLayout,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    let mut array_part = String::new();
    export_type_layout_for_def(tyl, output, &mut array_part, context)?;
    output.push_str(&array_part);
    Ok(())
}

/// Export ir type layout to HLSL
fn export_type_layout_for_def(
    tyl: &ir::TypeLayout,
    output: &mut String,
    output_array: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match *tyl {
        ir::TypeLayout::Void => write!(output, "void").unwrap(),
        ir::TypeLayout::Scalar(st) => write!(output, "{}", export_scalar_type(st)?).unwrap(),
        ir::TypeLayout::Vector(st, x) => {
            write!(output, "{}{}", export_scalar_type(st)?, x).unwrap()
        }
        ir::TypeLayout::Matrix(st, x, y) => {
            write!(output, "{}{}x{}", export_scalar_type(st)?, x, y).unwrap()
        }
        ir::TypeLayout::Struct(id) => {
            write!(output, "{}", context.get_struct_name_full(id)?).unwrap()
        }
        ir::TypeLayout::Object(ref ot) => {
            match ot {
                ir::ObjectType::Buffer(dt) => {
                    output.push_str("Buffer<");
                    export_type(&(*dt).into(), output, context)?;
                    output.push('>');
                }
                ir::ObjectType::RWBuffer(dt) => {
                    output.push_str("RWBuffer<");
                    export_type(&(*dt).into(), output, context)?;
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
                ir::ObjectType::StructuredBuffer(st) => {
                    output.push_str("StructuredBuffer<");
                    export_type(&st.clone().into(), output, context)?;
                    output.push('>');
                }
                ir::ObjectType::RWStructuredBuffer(st) => {
                    output.push_str("RWStructuredBuffer<");
                    export_type(&st.clone().into(), output, context)?;
                    output.push('>');
                }
                ir::ObjectType::Texture2D(dt) => {
                    output.push_str("Texture2D<");
                    export_type(&(*dt).into(), output, context)?;
                    output.push('>');
                }
                ir::ObjectType::RWTexture2D(dt) => {
                    output.push_str("RWTexture2D<");
                    export_type(&(*dt).into(), output, context)?;
                    output.push('>');
                }
                ir::ObjectType::ConstantBuffer(st) => {
                    output.push_str("ConstantBuffer<");
                    export_type(&st.clone().into(), output, context)?;
                    output.push('>');
                }
            };
        }
        ir::TypeLayout::Array(ref ty, ref len) => {
            export_type_layout_for_def(ty, output, output_array, context)?;
            write!(output_array, "[{}]", len).unwrap();
        }
        ir::TypeLayout::Modifier(ref modifier, ref ty) => {
            write!(output, "{:?}", modifier).unwrap();
            export_type_layout_for_def(ty, output, output_array, context)?;
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
        ir::TypeOrConstant::Type(ty) => export_type(ty, output, context),
        ir::TypeOrConstant::Constant(_) => todo!("Non-type template arguments"),
    }
}

/// Export ir literal to HLSL
fn export_literal(literal: &ir::Literal, output: &mut String) -> Result<(), ExportError> {
    match literal {
        ir::Literal::Bool(true) => output.push_str("true"),
        ir::Literal::Bool(false) => output.push_str("false"),
        // There is no signed integer suffix - but the way we generate code should avoid overload issues with unsigned integers
        ir::Literal::UntypedInt(v) | ir::Literal::Int(v) => write!(output, "{}", v).unwrap(),
        ir::Literal::UInt(v) => write!(output, "{}u", v).unwrap(),
        ir::Literal::Long(v) => write!(output, "{}l", v).unwrap(),
        ir::Literal::Half(v) => write!(output, "{}h", v).unwrap(),
        ir::Literal::Float(v) if *v == (*v as i64 as f32) => {
            write!(output, "{}.0", *v as i64).unwrap()
        }
        ir::Literal::Float(v) => write!(output, "{}", v).unwrap(),
        ir::Literal::Double(v) => write!(output, "{}L", v).unwrap(),
    }
    Ok(())
}

/// Export ir statement to HLSL
fn export_statement(
    statement: &ir::Statement,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    context.new_line(output);
    match statement {
        ir::Statement::Expression(expr) => {
            export_expression(expr, output, context)?;
            output.push(';');
        }
        ir::Statement::Var(def) => {
            export_variable_definition(def, output, context)?;
            output.push(';');
        }
        ir::Statement::Block(block) => {
            enter_scope_block(block, context);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::If(cond, block) => {
            enter_scope_block(block, context);
            output.push_str("if (");
            export_expression(cond, output, context)?;
            output.push(')');
            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::IfElse(cond, block_true, block_false) => {
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
        ir::Statement::For(init, cond, inc, block) => {
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
        ir::Statement::While(cond, block) => {
            enter_scope_block(block, context);

            output.push_str("while (");
            export_expression(cond, output, context)?;
            output.push(')');

            context.new_line(output);
            export_scope_block(block, output, context)?;
            context.pop_scope();
        }
        ir::Statement::Break => output.push_str("break;"),
        ir::Statement::Continue => output.push_str("continue;"),
        ir::Statement::Discard => output.push_str("discard;"),
        ir::Statement::Return(expr_opt) => {
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

/// Export ir variable definition to HLSL
fn export_variable_definition(
    def: &ir::VarDef,
    output: &mut String,
    context: &mut ExportContext,
) -> Result<(), ExportError> {
    match def.local_type.1 {
        ir::LocalStorage::Local => {}
        ir::LocalStorage::Static => output.push_str("static "),
    }
    let mut array_part = String::new();
    export_type_for_def(&def.local_type.0, output, &mut array_part, context)?;
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
                &head.local_type.0,
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
                    &def.local_type.0,
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
        ir::Expression::Constructor(tyl, args) => {
            export_type_layout(tyl, output, context)?;
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
        ir::Expression::Cast(ty, expr) => {
            output.push('(');
            export_type(ty, output, context)?;
            output.push(')');
            export_subexpression(expr, prec, OperatorSide::Right, output, context)?;
        }
        ir::Expression::SizeOf(ty) => {
            output.push_str("sizeof(");
            export_type(ty, output, context)?;
            output.push(')');
        }
        ir::Expression::Member(expr, name) => {
            export_subexpression(expr, prec, OperatorSide::Left, output, context)?;
            output.push('.');
            output.push_str(name)
        }
        ir::Expression::Call(id, ct, tys, exprs) => {
            let (object, arguments) = match ct {
                ir::CallType::FreeFunction | ir::CallType::MethodInternal => {
                    (None, exprs.as_slice())
                }
                ir::CallType::MethodExternal => (Some(&exprs[0]), &exprs[1..]),
            };
            if let Some(object) = object {
                export_subexpression(object, 2, OperatorSide::Left, output, context)?;
                output.push('.');
            }
            if *ct == ir::CallType::FreeFunction {
                write!(output, "{}", context.get_function_name_full(*id)?).unwrap();
            } else {
                // Assume all method calls (both on an object and internally) have
                // sufficient qualification that they do not need the full name
                write!(output, "{}", context.get_function_name(*id)?).unwrap();
            }
            export_template_type_args(tys.as_slice(), output, context)?;
            export_invocation_args(arguments, output, context)?;
        }
        ir::Expression::Intrinsic(intrinsic, tys, exprs) => {
            enum Form {
                Unary(&'static str),
                UnaryPostfix(&'static str),
                Binary(&'static str),
                Invoke(&'static str),
                Method(&'static str),
                AddressMethod(&'static str, &'static str),
            }

            use ir::Intrinsic::*;
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

                AllMemoryBarrier => Form::Invoke("AllMemoryBarrier"),
                AllMemoryBarrierWithGroupSync => Form::Invoke("AllMemoryBarrierWithGroupSync"),
                DeviceMemoryBarrier => Form::Invoke("DeviceMemoryBarrier"),
                DeviceMemoryBarrierWithGroupSync => {
                    Form::Invoke("DeviceMemoryBarrierWithGroupSync")
                }
                GroupMemoryBarrier => Form::Invoke("GroupMemoryBarrier"),
                GroupMemoryBarrierWithGroupSync => Form::Invoke("GroupMemoryBarrierWithGroupSync"),

                AsInt => Form::Invoke("asint"),
                AsUInt => Form::Invoke("asuint"),
                AsFloat => Form::Invoke("asfloat"),
                AsDouble => Form::Invoke("asdouble"),

                All => Form::Invoke("all"),
                Any => Form::Invoke("any"),

                Abs => Form::Invoke("abs"),

                // Transcendental functions
                Acos => Form::Invoke("acos"),
                Asin => Form::Invoke("asin"),
                Cos => Form::Invoke("cos"),
                Sin => Form::Invoke("sin"),
                Exp => Form::Invoke("exp"),
                Sqrt => Form::Invoke("sqrt"),
                Pow => Form::Invoke("pow"),
                Sincos => Form::Invoke("sincos"),

                F16ToF32 => Form::Invoke("f16tof32"),
                F32ToF16 => Form::Invoke("f32tof16"),

                Floor => Form::Invoke("floor"),

                IsNaN => Form::Invoke("isnan"),

                Length => Form::Invoke("length"),
                Normalize => Form::Invoke("normalize"),

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

                BufferLoad => Form::Method("Load"),
                RWBufferLoad => Form::Method("Load"),

                StructuredBufferLoad => Form::Method("Load"),
                RWStructuredBufferLoad => Form::Method("Load"),

                ByteAddressBufferLoad => Form::Method("Load"),
                ByteAddressBufferLoad2 => Form::Method("Load2"),
                ByteAddressBufferLoad3 => Form::Method("Load3"),
                ByteAddressBufferLoad4 => Form::Method("Load4"),
                ByteAddressBufferLoadT => Form::Method("Load"),

                RWByteAddressBufferLoad => Form::Method("Load"),
                RWByteAddressBufferLoad2 => Form::Method("Load2"),
                RWByteAddressBufferLoad3 => Form::Method("Load3"),
                RWByteAddressBufferLoad4 => Form::Method("Load4"),
                RWByteAddressBufferStore => Form::Method("Store"),
                RWByteAddressBufferStore2 => Form::Method("Store2"),
                RWByteAddressBufferStore3 => Form::Method("Store3"),
                RWByteAddressBufferStore4 => Form::Method("Store4"),
                RWByteAddressBufferInterlockedAdd => Form::Method("InterlockedAdd"),

                BufferAddressLoad => Form::AddressMethod("Load", "vk::RawBufferLoad"),

                RWBufferAddressLoad => Form::AddressMethod("Load", "vk::RawBufferLoad"),
                RWBufferAddressStore => Form::AddressMethod("Store", "vk::RawBufferStore"),

                Texture2DLoad => Form::Method("Load"),
                Texture2DSample => Form::Method("Sample"),

                RWTexture2DLoad => Form::Method("Load"),
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
                Form::Invoke(s) => {
                    output.push_str(s);
                    export_template_type_args(tys.as_slice(), output, context)?;
                    export_invocation_args(exprs, output, context)?;
                }
                Form::AddressMethod(_, s) if context.module.flags.requires_buffer_address => {
                    assert!(
                        exprs.len() >= 2,
                        "Buffer address intrinsic expects at least an address and offset"
                    );

                    output.push_str(s);
                    export_template_type_args(tys.as_slice(), output, context)?;
                    output.push('(');
                    if let [addr, offset, rest @ ..] = exprs.as_slice() {
                        export_subexpression(addr, 6, OperatorSide::Left, output, context)?;
                        output.push_str(" + uint64_t(");
                        export_subexpression(offset, 17, OperatorSide::Middle, output, context)?;
                        output.push(')');

                        for expr in rest {
                            output.push_str(", ");
                            export_subexpression(
                                expr,
                                17,
                                OperatorSide::CommaList,
                                output,
                                context,
                            )?;
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
                    export_template_type_args(tys.as_slice(), output, context)?;
                    export_invocation_args(&exprs[1..], output, context)?;
                }
            }
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
        | ir::Expression::Global(_) => 0,

        ir::Expression::ConstantVariable(_, _) => 1,
        ir::Expression::TernaryConditional(_, _, _) => 16,
        ir::Expression::Sequence(_) => 17,
        ir::Expression::Swizzle(_, _) => 2,
        ir::Expression::ArraySubscript(_, _) => 2,
        ir::Expression::Constructor(_, _) => 2,
        ir::Expression::Cast(_, _) => 3,
        ir::Expression::SizeOf(_) => 3,
        ir::Expression::Member(_, _) => 2,
        ir::Expression::Call(_, _, _, _) => 2,
        ir::Expression::Intrinsic(intrinsic, _, _) => {
            use ir::Intrinsic::*;
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

                _ => 2,
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
    template_args: &[Located<ir::TypeOrConstant>],
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
        let mut array_part = String::new();
        export_type_for_def(&member.typename, output, &mut array_part, context)?;
        output.push(' ');
        output.push_str(&member.name);
        output.push_str(&array_part);
        output.push(';');
    }

    for method in &decl.methods {
        context.new_line(output);
        context.new_line(output);
        let fd = context.module.function_registry[method.0 as usize]
            .as_ref()
            .unwrap();
        export_function(fd, output, context)?;
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
        export_type_for_def(&member.typename, output, &mut array_part, context)?;
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
        match self.module.function_name_registry.get(id.0 as usize) {
            Some(name) => Ok(name.name.as_str()),
            None => Err(ExportError::NamelessId),
        }
    }

    /// Get the full name of a function
    fn get_function_name_full(&self, id: ir::FunctionId) -> Result<&ir::ScopedName, ExportError> {
        match self.module.function_name_registry.get(id.0 as usize) {
            Some(name) => Ok(&name.full_name),
            None => Err(ExportError::NamelessId),
        }
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
