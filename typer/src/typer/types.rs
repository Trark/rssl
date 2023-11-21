use super::declarations::parse_declarator;
use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use crate::evaluator::evaluate_constexpr;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::{Locate, Located, SourceLocation};

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum TypePosition {
    /// Type is not for a definition or does not support any special modifiers
    Free,

    /// Type is for a local variable
    Local,

    /// Type is for a function parameter
    Parameter,

    /// Type is for a function return
    Return,

    /// Type is for a member of a struct
    StructMember,

    /// Type is for a global definition
    Global,

    /// Type is for a member of a constant buffer
    ConstantBufferMember,
}

/// Attempt to get an ir type from an ast type
pub fn parse_type(ty: &ast::Type, context: &mut Context) -> TyperResult<ir::TypeId> {
    parse_type_for_usage(ty, TypePosition::Free, context)
}

/// Attempt to get an ir type from an ast type
pub fn parse_type_for_usage(
    ty: &ast::Type,
    position: TypePosition,
    context: &mut Context,
) -> TyperResult<ir::TypeId> {
    let parsed_id = parse_typelayout(&ty.layout, context)?;

    // Process the primary type modifiers
    let direct_modifier = parse_type_modifier(&ty.modifiers, parsed_id, position, context)?;

    // Storage classes only exist on certain definition types
    // This does not check the storage class is valid for the types where any are valid
    if !matches!(
        position,
        TypePosition::Local
            | TypePosition::Global
            | TypePosition::StructMember
            | TypePosition::Return
    ) {
        deny_storage_class(&ty.modifiers, position)?;
    }

    // Only function parameters may have input modifiers
    if !matches!(position, TypePosition::Parameter) {
        deny_input_modifier(&ty.modifiers, position)?;
    }

    // Only function parameters and struct members may have interpolation modifiers
    if !matches!(
        position,
        TypePosition::Parameter | TypePosition::StructMember
    ) {
        deny_interpolation_modifier(&ty.modifiers, position)?;
    }

    // Only function parameters may have mesh shader modifiers
    if !matches!(position, TypePosition::Parameter) {
        deny_mesh_shader_modifiers(&ty.modifiers, position)?;
    }

    // Allow precise in only some declaration positions
    // Purposefully deny global variables and typedefs - which compiles without warning in HLSL but does not function
    // Purposefully deny uses like casts - which may warn in HLSL but does not do anything
    if !matches!(
        position,
        TypePosition::Local
            | TypePosition::Parameter
            | TypePosition::Return
            | TypePosition::StructMember
            | TypePosition::ConstantBufferMember
    ) {
        deny_precise(&ty.modifiers, position)?;
    }

    let (ir_ty, base_modifier) = context.module.type_registry.extract_modifier(parsed_id);
    let modifier = base_modifier.combine(direct_modifier);
    let ty = context
        .module
        .type_registry
        .combine_modifier(ir_ty, modifier);

    Ok(ty)
}

/// Attempt to get an ir type layout from an ast type layout
pub fn parse_typelayout(ty: &ast::TypeLayout, context: &mut Context) -> TyperResult<ir::TypeId> {
    Ok(match *ty {
        ast::TypeLayout(ref name, ref args) => {
            // Translate type arguments first
            let mut ir_args = Vec::with_capacity(args.len());
            for arg in args.as_ref() {
                ir_args.push(parse_expression_or_type(arg, context)?);
            }

            // Special case all the built in types

            if let Some(tyl) = parse_voidtype(name, &ir_args) {
                let id = context.module.type_registry.register_type(tyl);
                return Ok(id);
            }

            if let Some(id) = parse_data_layout(name, &ir_args, context) {
                return Ok(id);
            }

            if let Some(tyl) = parse_object_type(name, &ir_args, context) {
                let id = context.module.type_registry.register_type(tyl);
                return Ok(id);
            }

            // Find the type from the scope stack
            context.find_type_id(name, &ir_args)?
        }
    })
}

/// Attempt to get an ir type or expression from an ast type or expression
pub fn parse_expression_or_type(
    arg: &ast::ExpressionOrType,
    context: &mut Context,
) -> TyperResult<ir::TypeOrConstant> {
    match arg {
        ast::ExpressionOrType::Type(ast_ty) => {
            let ir_ty = parse_type(ast_ty, context)?;
            Ok(ir::TypeOrConstant::Type(ir_ty))
        }
        ast::ExpressionOrType::Expression(ast_expr) => {
            let ir_const = parse_and_evaluate_constant_expression(ast_expr, context)?;
            Ok(ir::TypeOrConstant::Constant(ir_const))
        }
        ast::ExpressionOrType::Either(ast_expr, ast_ty) => {
            if let Ok(ir_ty) = parse_type(ast_ty, context) {
                Ok(ir::TypeOrConstant::Type(ir_ty))
            } else {
                let ir_const = parse_and_evaluate_constant_expression(ast_expr, context)?;
                Ok(ir::TypeOrConstant::Constant(ir_const))
            }
        }
    }
}

/// Parse a type layout for a basic data type
pub fn parse_data_layout(
    name: &ast::ScopedIdentifier,
    template_args: &[ir::TypeOrConstant],
    context: &mut Context,
) -> Option<ir::TypeId> {
    if let Some(name) = name.try_trivial() {
        if template_args.is_empty() {
            let numeric = ir::NumericType::from_str(name)?;
            let id = context.module.type_registry.register_numeric_type(numeric);
            Some(id)
        } else {
            match name.node.as_str() {
                "vector" => {
                    if template_args.len() == 2 {
                        let type_id = match template_args[0] {
                            ir::TypeOrConstant::Type(id) => id,
                            _ => return None,
                        };

                        // Type modifiers not supported inside arguments
                        match context.module.type_registry.get_type_layer(type_id) {
                            ir::TypeLayer::Scalar(_) => {}
                            ir::TypeLayer::TemplateParam(_) => {}
                            _ => return None,
                        }

                        let dim_x = match template_args[1]
                            .as_constant()
                            .map(ir::RestrictedConstant::to_uint64)
                        {
                            Some(Some(v)) if (1..=4).contains(&v) => v,
                            _ => return None,
                        };

                        let tyl = ir::TypeLayer::Vector(type_id, dim_x as u32);
                        let id = context.module.type_registry.register_type(tyl);

                        Some(id)
                    } else {
                        None
                    }
                }
                "matrix" => {
                    if template_args.len() == 3 {
                        let type_id = match template_args[0] {
                            ir::TypeOrConstant::Type(id) => id,
                            _ => return None,
                        };

                        // Type modifiers not supported inside arguments
                        match context.module.type_registry.get_type_layer(type_id) {
                            ir::TypeLayer::Scalar(_) => {}
                            ir::TypeLayer::TemplateParam(_) => {}
                            _ => return None,
                        }

                        let dim_x = match template_args[1]
                            .as_constant()
                            .map(ir::RestrictedConstant::to_uint64)
                        {
                            Some(Some(v)) if (1..=4).contains(&v) => v,
                            _ => return None,
                        };

                        let dim_y = match template_args[2]
                            .as_constant()
                            .map(ir::RestrictedConstant::to_uint64)
                        {
                            Some(Some(v)) if (1..=4).contains(&v) => v,
                            _ => return None,
                        };

                        let tyl = ir::TypeLayer::Matrix(type_id, dim_x as u32, dim_y as u32);
                        let id = context.module.type_registry.register_type(tyl);

                        Some(id)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
    } else {
        None
    }
}

/// Parse the void type
fn parse_voidtype(
    name: &ast::ScopedIdentifier,
    template_args: &[ir::TypeOrConstant],
) -> Option<ir::TypeLayer> {
    if let Some(name) = name.try_trivial() {
        if name.node == "void" && template_args.is_empty() {
            Some(ir::TypeLayer::Void)
        } else {
            None
        }
    } else {
        None
    }
}

/// Attempt to turn an ast type into one of the built in object types
fn parse_object_type(
    name: &ast::ScopedIdentifier,
    template_args: &[ir::TypeOrConstant],
    context: &mut Context,
) -> Option<ir::TypeLayer> {
    fn get_data_type(
        args: &[ir::TypeOrConstant],
        default_float4: bool,
        context: &mut Context,
    ) -> Option<ir::TypeId> {
        if let [ir::TypeOrConstant::Type(id)] = args {
            let id_nomod = context.module.type_registry.remove_modifier(*id);
            let layer = context.module.type_registry.get_type_layer(id_nomod);
            match layer {
                ir::TypeLayer::Scalar(_) | ir::TypeLayer::Vector(_, _) => Some(*id),
                _ => None,
            }
        } else if default_float4 {
            let f = context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Float32));
            let f4 = context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Vector(f, 4));
            Some(f4)
        } else {
            None
        }
    }

    fn get_structured_type(
        args: &[ir::TypeOrConstant],
        context: &mut Context,
    ) -> Option<ir::TypeId> {
        if let [ir::TypeOrConstant::Type(id)] = args {
            let id_nomod = context.module.type_registry.remove_modifier(*id);
            let layer = context.module.type_registry.get_type_layer(id_nomod);
            match layer {
                ir::TypeLayer::Scalar(_)
                | ir::TypeLayer::Vector(_, _)
                | ir::TypeLayer::Matrix(_, _, _)
                | ir::TypeLayer::Struct(_) => Some(*id),
                _ => None,
            }
        } else {
            None
        }
    }

    fn get_uint(args: &[ir::TypeOrConstant]) -> Option<u32> {
        match args {
            [ir::TypeOrConstant::Constant(ir::RestrictedConstant::UInt32(v))] => Some(*v),
            [ir::TypeOrConstant::Constant(ir::RestrictedConstant::IntLiteral(v))] => {
                Some(*v as u32)
            }
            _ => None,
        }
    }

    // Special object types are all unscoped names
    if name.identifiers.len() != 1 {
        return None;
    }

    match name.identifiers[0].node.as_str() {
        "Buffer" => Some(ir::TypeLayer::Object(ir::ObjectType::Buffer(
            get_data_type(template_args, true, context)?,
        ))),
        "RWBuffer" => Some(ir::TypeLayer::Object(ir::ObjectType::RWBuffer(
            get_data_type(template_args, false, context)?,
        ))),
        "ByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayer::Object(ir::ObjectType::ByteAddressBuffer))
        }
        "RWByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayer::Object(ir::ObjectType::RWByteAddressBuffer))
        }
        "BufferAddress" if template_args.is_empty() => {
            Some(ir::TypeLayer::Object(ir::ObjectType::BufferAddress))
        }
        "RWBufferAddress" if template_args.is_empty() => {
            Some(ir::TypeLayer::Object(ir::ObjectType::RWBufferAddress))
        }
        "Texture2D" => Some(ir::TypeLayer::Object(ir::ObjectType::Texture2D(
            get_data_type(template_args, true, context)?,
        ))),
        "Texture2DArray" => Some(ir::TypeLayer::Object(ir::ObjectType::Texture2DArray(
            get_data_type(template_args, true, context)?,
        ))),
        "RWTexture2D" => Some(ir::TypeLayer::Object(ir::ObjectType::RWTexture2D(
            get_data_type(template_args, false, context)?,
        ))),
        "RWTexture2DArray" => Some(ir::TypeLayer::Object(ir::ObjectType::RWTexture2DArray(
            get_data_type(template_args, false, context)?,
        ))),
        "TextureCube" => Some(ir::TypeLayer::Object(ir::ObjectType::TextureCube(
            get_data_type(template_args, true, context)?,
        ))),
        "TextureCubeArray" => Some(ir::TypeLayer::Object(ir::ObjectType::TextureCubeArray(
            get_data_type(template_args, true, context)?,
        ))),
        "Texture3D" => Some(ir::TypeLayer::Object(ir::ObjectType::Texture3D(
            get_data_type(template_args, true, context)?,
        ))),
        "RWTexture3D" => Some(ir::TypeLayer::Object(ir::ObjectType::RWTexture3D(
            get_data_type(template_args, false, context)?,
        ))),
        "ConstantBuffer" => Some(ir::TypeLayer::Object(ir::ObjectType::ConstantBuffer(
            get_structured_type(template_args, context)?,
        ))),
        "StructuredBuffer" => Some(ir::TypeLayer::Object(ir::ObjectType::StructuredBuffer(
            get_structured_type(template_args, context)?,
        ))),
        "RWStructuredBuffer" => Some(ir::TypeLayer::Object(ir::ObjectType::RWStructuredBuffer(
            get_structured_type(template_args, context)?,
        ))),
        "SamplerState" => Some(ir::TypeLayer::Object(ir::ObjectType::SamplerState)),
        "SamplerComparisonState" => Some(ir::TypeLayer::Object(
            ir::ObjectType::SamplerComparisonState,
        )),
        "TriangleStream" => Some(ir::TypeLayer::Object(ir::ObjectType::TriangleStream(
            get_structured_type(template_args, context)?,
        ))),
        "RaytracingAccelerationStructure" => Some(ir::TypeLayer::Object(
            ir::ObjectType::RaytracingAccelerationStructure,
        )),
        "RayQuery" => Some(ir::TypeLayer::Object(ir::ObjectType::RayQuery(get_uint(
            template_args,
        )?))),
        "RayDesc" if template_args.is_empty() => {
            Some(ir::TypeLayer::Object(ir::ObjectType::RayDesc))
        }
        _ => None,
    }
}

/// Generate the primary type modifiers from the ast type modifiers
fn parse_type_modifier(
    modifiers: &ast::TypeModifierSet,
    applied_type: ir::TypeId,
    position: TypePosition,
    context: &Context,
) -> TyperResult<ir::TypeModifier> {
    let tyl = context.module.type_registry.get_type_layer(applied_type);

    let mut full_modifier = ir::TypeModifier::new();
    for modifier in &modifiers.modifiers {
        match &modifier.node {
            ast::TypeModifier::Const => {
                full_modifier.is_const = true;
            }
            ast::TypeModifier::Volatile => {
                if matches!(
                    position,
                    TypePosition::Return
                        | TypePosition::StructMember
                        | TypePosition::Global
                        | TypePosition::ConstantBufferMember
                ) {
                    return Err(TyperError::ModifierNotSupported(
                        modifier.node,
                        modifier.location,
                        position,
                    ));
                }
                full_modifier.volatile = true;
            }
            ast::TypeModifier::RowMajor => {
                if full_modifier.column_major {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        ast::TypeModifier::ColumnMajor,
                    ));
                }
                if !matches!(tyl, ir::TypeLayer::Matrix(..)) {
                    return Err(TyperError::MatrixOrderRequiresMatrixType(
                        modifier.node,
                        modifier.location,
                        applied_type,
                    ));
                }
                full_modifier.row_major = true;
            }
            ast::TypeModifier::ColumnMajor => {
                if full_modifier.row_major {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        ast::TypeModifier::RowMajor,
                    ));
                }
                if !matches!(tyl, ir::TypeLayer::Matrix(..)) {
                    return Err(TyperError::MatrixOrderRequiresMatrixType(
                        modifier.node,
                        modifier.location,
                        applied_type,
                    ));
                }
                full_modifier.column_major = true;
            }
            ast::TypeModifier::Unorm => {
                if full_modifier.snorm {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        ast::TypeModifier::Snorm,
                    ));
                }
                if !matches!(
                    context.module.type_registry.extract_scalar(applied_type),
                    Some(ir::ScalarType::Float32)
                ) {
                    return Err(TyperError::ModifierRequiresFloatType(
                        modifier.node,
                        modifier.location,
                        applied_type,
                    ));
                }
                full_modifier.unorm = true;
            }
            ast::TypeModifier::Snorm => {
                if full_modifier.unorm {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        ast::TypeModifier::Unorm,
                    ));
                }
                if !matches!(
                    context.module.type_registry.extract_scalar(applied_type),
                    Some(ir::ScalarType::Float32)
                ) {
                    return Err(TyperError::ModifierRequiresFloatType(
                        modifier.node,
                        modifier.location,
                        applied_type,
                    ));
                }
                full_modifier.snorm = true;
            }
            _ => continue,
        }

        // TODO: Warn for duplicate modifier
    }
    Ok(full_modifier)
}

/// Ensure storage class modifiers do not appear as a type modifier
fn deny_storage_class(modifiers: &ast::TypeModifierSet, position: TypePosition) -> TyperResult<()> {
    for modifier in &modifiers.modifiers {
        if matches!(
            &modifier.node,
            ast::TypeModifier::Extern | ast::TypeModifier::Static | ast::TypeModifier::GroupShared
        ) {
            return Err(TyperError::ModifierNotSupported(
                modifier.node,
                modifier.location,
                position,
            ));
        }
    }
    Ok(())
}

/// Generate the input modifier from the type modifiers
pub fn parse_input_modifier(
    modifiers: &ast::TypeModifierSet,
) -> TyperResult<Option<ir::InputModifier>> {
    let mut current_modifier = None;
    for modifier in &modifiers.modifiers {
        let next_im = match &modifier.node {
            ast::TypeModifier::In => ir::InputModifier::In,
            ast::TypeModifier::Out => ir::InputModifier::Out,
            ast::TypeModifier::InOut => ir::InputModifier::InOut,
            _ => continue,
        };

        if let Some((current_im, current_source)) = current_modifier {
            if current_im == next_im {
                // TODO: Warn for duplicate modifier
            } else {
                return Err(TyperError::ModifierConflict(
                    modifier.node,
                    modifier.location,
                    current_source,
                ));
            }
        } else {
            current_modifier = Some((next_im, modifier.node));
        }
    }
    let input_modifier = current_modifier.map(|(im, _)| im);
    Ok(input_modifier)
}

/// Ensure input modifiers do not appear as a type modifier
fn deny_input_modifier(
    modifiers: &ast::TypeModifierSet,
    position: TypePosition,
) -> TyperResult<()> {
    for modifier in &modifiers.modifiers {
        if matches!(
            &modifier.node,
            ast::TypeModifier::In | ast::TypeModifier::Out | ast::TypeModifier::InOut
        ) {
            return Err(TyperError::ModifierNotSupported(
                modifier.node,
                modifier.location,
                position,
            ));
        }
    }
    Ok(())
}

/// Generate the interpolation modifier from the type modifiers
pub fn parse_interpolation_modifier(
    modifiers: &ast::TypeModifierSet,
) -> TyperResult<Option<(ir::InterpolationModifier, SourceLocation)>> {
    #[derive(PartialEq)]
    enum LocationMode {
        Center,
        Centroid,
        Sample,
    }

    #[derive(PartialEq)]
    enum PerspectiveMode {
        Linear,
        NoPerspective,
    }

    let mut current_location: Option<(LocationMode, Located<ast::TypeModifier>)> = None;
    let mut current_perspective: Option<(PerspectiveMode, Located<ast::TypeModifier>)> = None;
    let mut current_immediate: Option<(ir::InterpolationModifier, Located<ast::TypeModifier>)> =
        None;

    for modifier in &modifiers.modifiers {
        let location_mode = match modifier.node {
            ast::TypeModifier::Centroid => Some(LocationMode::Centroid),
            ast::TypeModifier::Sample => Some(LocationMode::Sample),
            _ => None,
        };

        if let Some(location_mode) = location_mode {
            if let Some(ref current_location) = current_location {
                if current_location.0 == location_mode {
                    // TODO: Warn for duplicate modifier
                } else {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        current_location.1.node,
                    ));
                }
            } else {
                current_location = Some((location_mode, modifier.clone()));
            }
        }

        let perpective_mode = match modifier.node {
            ast::TypeModifier::Linear => Some(PerspectiveMode::Linear),
            ast::TypeModifier::NoPerspective => Some(PerspectiveMode::NoPerspective),
            _ => None,
        };

        if let Some(perpective_mode) = perpective_mode {
            if let Some(ref current_perspective) = current_perspective {
                if current_perspective.0 == perpective_mode {
                    // TODO: Warn for duplicate modifier
                } else {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        current_perspective.1.node,
                    ));
                }
            } else {
                current_perspective = Some((perpective_mode, modifier.clone()));
            }
        }

        let immediate_modifier = match modifier.node {
            ast::TypeModifier::NoInterpolation => Some(ir::InterpolationModifier::Flat),
            ast::TypeModifier::Point => Some(ir::InterpolationModifier::Point),
            ast::TypeModifier::Line => Some(ir::InterpolationModifier::Line),
            ast::TypeModifier::Triangle => Some(ir::InterpolationModifier::Triangle),
            ast::TypeModifier::LineAdj => Some(ir::InterpolationModifier::LineAdj),
            ast::TypeModifier::TriangleAdj => Some(ir::InterpolationModifier::TriangleAdj),
            ast::TypeModifier::Vertices => Some(ir::InterpolationModifier::Vertices),
            ast::TypeModifier::Primitives => Some(ir::InterpolationModifier::Primitives),
            ast::TypeModifier::Indices => Some(ir::InterpolationModifier::Indices),
            ast::TypeModifier::Payload => Some(ir::InterpolationModifier::Payload),
            _ => None,
        };

        if let Some(immediate_modifier) = immediate_modifier {
            if let Some((current_im, ref current_source)) = current_immediate {
                if current_im == immediate_modifier {
                    // TODO: Warn for duplicate modifier
                } else {
                    return Err(TyperError::ModifierConflict(
                        modifier.node,
                        modifier.location,
                        current_source.node,
                    ));
                }
            } else {
                current_immediate = Some((immediate_modifier, modifier.clone()));
            }
        }
    }

    let traditional_mode = if current_location.is_some() || current_perspective.is_some() {
        // Get the source modifier with the last position for error reporting
        let location_source = current_location.as_ref().map(|p| p.1.clone());
        let perspective_source = current_perspective.as_ref().map(|p| p.1.clone());
        let source_location = match (location_source, perspective_source) {
            (None, None) => unreachable!(),
            (Some(left), None) => left,
            (None, Some(right)) => right,
            (Some(left), Some(right)) => {
                if left.location < right.location {
                    right
                } else {
                    left
                }
            }
        };

        let current_location = current_location.map_or(LocationMode::Center, |(mode, _)| mode);
        let current_perspective =
            current_perspective.map_or(PerspectiveMode::Linear, |(mode, _)| mode);

        let im = match (current_location, current_perspective) {
            (LocationMode::Center, PerspectiveMode::Linear) => {
                ir::InterpolationModifier::CenterPerspective
            }
            (LocationMode::Center, PerspectiveMode::NoPerspective) => {
                ir::InterpolationModifier::CenterNoPerspective
            }
            (LocationMode::Centroid, PerspectiveMode::Linear) => {
                ir::InterpolationModifier::CentroidPerspective
            }
            (LocationMode::Centroid, PerspectiveMode::NoPerspective) => {
                ir::InterpolationModifier::CentroidNoPerspective
            }
            (LocationMode::Sample, PerspectiveMode::Linear) => {
                ir::InterpolationModifier::SamplePerspective
            }
            (LocationMode::Sample, PerspectiveMode::NoPerspective) => {
                ir::InterpolationModifier::SampleNoPerspective
            }
        };

        Some((im, source_location))
    } else {
        None
    };

    match (traditional_mode, current_immediate) {
        (Some(t), Some(m)) => {
            // Currently we always error as if the traditional interpolator was applied after
            Err(TyperError::ModifierConflict(
                t.1.node,
                t.1.location,
                m.1.node,
            ))
        }
        (Some((t, l)), None) => Ok(Some((t, l.location))),
        (None, Some((m, l))) => Ok(Some((m, l.location))),
        (None, None) => Ok(None),
    }
}

/// Ensure interpolation modifiers do not appear as a type modifier
pub fn deny_interpolation_modifier(
    modifiers: &ast::TypeModifierSet,
    position: TypePosition,
) -> TyperResult<()> {
    for modifier in &modifiers.modifiers {
        if matches!(
            &modifier.node,
            ast::TypeModifier::NoInterpolation
                | ast::TypeModifier::Linear
                | ast::TypeModifier::Centroid
                | ast::TypeModifier::NoPerspective
                | ast::TypeModifier::Sample
        ) {
            return Err(TyperError::ModifierNotSupported(
                modifier.node,
                modifier.location,
                position,
            ));
        }
    }
    Ok(())
}

/// Ensure mesh entry point modifiers do not appear as a type modifier
/// And also the geometry shader triangle modifier
pub fn deny_mesh_shader_modifiers(
    modifiers: &ast::TypeModifierSet,
    position: TypePosition,
) -> TyperResult<()> {
    for modifier in &modifiers.modifiers {
        if matches!(
            &modifier.node,
            ast::TypeModifier::Point
                | ast::TypeModifier::Line
                | ast::TypeModifier::Triangle
                | ast::TypeModifier::LineAdj
                | ast::TypeModifier::TriangleAdj
                | ast::TypeModifier::Vertices
                | ast::TypeModifier::Primitives
                | ast::TypeModifier::Indices
                | ast::TypeModifier::Payload
        ) {
            return Err(TyperError::ModifierNotSupported(
                modifier.node,
                modifier.location,
                position,
            ));
        }
    }
    Ok(())
}

/// Calculate if a definition is precise from the type modifiers
pub fn parse_precise(modifiers: &ast::TypeModifierSet) -> TyperResult<Option<SourceLocation>> {
    let mut first_modifier: Option<Located<ast::TypeModifier>> = None;
    for modifier in &modifiers.modifiers {
        if let ast::TypeModifier::Precise = modifier.node {
            // TODO: Warn for duplicate modifier
            if first_modifier.is_none() {
                first_modifier = Some(modifier.clone());
            }
        }
    }
    Ok(first_modifier.map(|m| m.location))
}

/// Ensure precise does not appear as a type modifier
pub fn deny_precise(modifiers: &ast::TypeModifierSet, position: TypePosition) -> TyperResult<()> {
    for modifier in &modifiers.modifiers {
        if matches!(&modifier.node, ast::TypeModifier::Precise) {
            return Err(TyperError::ModifierNotSupported(
                modifier.node,
                modifier.location,
                position,
            ));
        }
    }
    Ok(())
}

/// Replace instances of a template type parameter in a type with a concrete type
pub fn apply_template_type_substitution(
    source_type: ir::TypeId,
    remap: &[Located<ir::TypeOrConstant>],
    context: &mut Context,
) -> ir::TypeId {
    match context.module.type_registry.get_type_layer(source_type) {
        ir::TypeLayer::Modifier(modifier, tyl) => {
            let inner_ty = apply_template_type_substitution(tyl, remap, context);
            context
                .module
                .type_registry
                .combine_modifier(inner_ty, modifier)
        }
        ir::TypeLayer::TemplateParam(ref p) => {
            let index = context
                .module
                .type_registry
                .get_template_type(*p)
                .positional_index;
            match &remap[index as usize].node {
                ir::TypeOrConstant::Type(ty) => *ty,
                ir::TypeOrConstant::Constant(_) => todo!("Non-type template arguments"),
            }
        }
        ir::TypeLayer::Vector(ty, x) => {
            let inner_ty = apply_template_type_substitution(ty, remap, context);
            let layer = ir::TypeLayer::Vector(inner_ty, x);
            context.module.type_registry.register_type(layer)
        }
        ir::TypeLayer::Matrix(ty, x, y) => {
            let inner_ty = apply_template_type_substitution(ty, remap, context);
            let layer = ir::TypeLayer::Matrix(inner_ty, x, y);
            context.module.type_registry.register_type(layer)
        }
        ir::TypeLayer::Array(tyl, len) => {
            let inner_ty = apply_template_type_substitution(tyl, remap, context);
            let layer = ir::TypeLayer::Array(inner_ty, len);
            context.module.type_registry.register_type(layer)
        }
        _ => source_type,
    }
}

/// Attempt to get an ir expression from an ast expression then evaluate it as a constant expression
fn parse_and_evaluate_constant_expression(
    expr: &Located<ast::Expression>,
    context: &mut Context,
) -> TyperResult<ir::RestrictedConstant> {
    let ir_expr = parse_expr(expr, context)?;

    let constant = match evaluate_constexpr(&ir_expr.0, &mut context.module) {
        Ok(constant) => constant,
        Err(()) => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
    };

    let restricted = match constant {
        ir::Constant::Bool(v) => ir::RestrictedConstant::Bool(v),
        ir::Constant::IntLiteral(v) => ir::RestrictedConstant::IntLiteral(v),
        ir::Constant::UInt32(v) => ir::RestrictedConstant::UInt32(v),
        ir::Constant::Int32(v) => ir::RestrictedConstant::Int32(v),
        ir::Constant::UInt64(v) => ir::RestrictedConstant::UInt64(v),
        ir::Constant::Int64(v) => ir::RestrictedConstant::Int64(v),
        // If the constant type can not be used for a template argument then claim it is not a constant expression
        _ => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
    };

    Ok(restricted)
}

/// Process a typedef
pub fn parse_rootdefinition_typedef(td: &ast::Typedef, context: &mut Context) -> TyperResult<()> {
    // Parse the base type
    let base_type = parse_type(&td.source, context)?;

    // If the parameter has type information bound to the name then apply it to the type now
    let (type_id, scoped_name) = parse_declarator(&td.declarator, base_type, None, false, context)?;

    // Ensure the name is unqualified
    let name = match scoped_name.try_trivial() {
        Some(name) => name.clone(),
        _ => return Err(TyperError::IllegalTypedefName(scoped_name.get_location())),
    };

    // Deny restricted non-keyword names
    if is_illegal_variable_name(&name) {
        return Err(TyperError::IllegalTypedefName(name.location));
    }

    // Register the typedef
    context.register_typedef(name, type_id)?;

    // We do not emit any global definitions as typedefs do not declare anything new

    Ok(())
}

/// Check if a name is forbidden for a variable / function / typedef
pub fn is_illegal_variable_name(name: &Located<String>) -> bool {
    matches!(
        name.as_str(),
        "nointerpolation"
            | "linear"
            | "centroid"
            | "noperspective"
            | "point"
            | "line"
            | "triangle"
            | "lineadj"
            | "triangleadj"
    )
}

/// Check if a name is forbidden for a struct / enum / using
pub fn is_illegal_type_name(name: &Located<String>) -> bool {
    matches!(
        name.as_str(),
        "precise"
            | "nointerpolation"
            | "linear"
            | "centroid"
            | "noperspective"
            | "sample"
            | "point"
            | "line"
            | "triangle"
            | "lineadj"
            | "triangleadj"
            | "vertices"
            | "primitives"
            | "indices"
            | "payload"
    )
}
