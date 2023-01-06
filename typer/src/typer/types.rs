use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use super::statements::apply_variable_bind;
use crate::evaluator::evaluate_constexpr;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::{Located, SourceLocation};

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
                let id = context.module.type_registry.register_type_layer(tyl);
                return Ok(id);
            }

            if let Some(id) = parse_data_layout(name, &ir_args, context) {
                return Ok(id);
            }

            if let Some(tyl) = parse_object_type(name, &ir_args, context) {
                let id = context.module.type_registry.register_type_layer(tyl);
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
            let scalar = context
                .module
                .type_registry
                .register_type_layer(ir::TypeLayer::Scalar(numeric.scalar));
            let id = match numeric.dimension {
                ir::NumericDimension::Scalar => scalar,
                ir::NumericDimension::Vector(x) => context
                    .module
                    .type_registry
                    .register_type_layer(ir::TypeLayer::Vector(scalar, x)),
                ir::NumericDimension::Matrix(x, y) => context
                    .module
                    .type_registry
                    .register_type_layer(ir::TypeLayer::Matrix(scalar, x, y)),
            };
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
                        let id = context.module.type_registry.register_type_layer(tyl);

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
                        let id = context.module.type_registry.register_type_layer(tyl);

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
                .register_type_layer(ir::TypeLayer::Scalar(ir::ScalarType::Float));
            let f4 = context
                .module
                .type_registry
                .register_type_layer(ir::TypeLayer::Vector(f, 4));
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
                | ir::TypeLayer::Struct(_) => Some(*id),
                _ => None,
            }
        } else {
            None
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
        "RWTexture2D" => Some(ir::TypeLayer::Object(ir::ObjectType::RWTexture2D(
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
                    Some(ir::ScalarType::Float)
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
                    Some(ir::ScalarType::Float)
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
    let mut current_modifier: Option<(ir::InterpolationModifier, Located<ast::TypeModifier>)> =
        None;
    for modifier in &modifiers.modifiers {
        let next_im = match &modifier.node {
            ast::TypeModifier::NoInterpolation => ir::InterpolationModifier::NoInterpolation,
            ast::TypeModifier::Linear => ir::InterpolationModifier::Linear,
            ast::TypeModifier::Centroid => ir::InterpolationModifier::Centroid,
            ast::TypeModifier::NoPerspective => ir::InterpolationModifier::NoPerspective,
            ast::TypeModifier::Sample => ir::InterpolationModifier::Sample,
            ast::TypeModifier::Vertices => ir::InterpolationModifier::Vertices,
            ast::TypeModifier::Primitives => ir::InterpolationModifier::Primitives,
            ast::TypeModifier::Indices => ir::InterpolationModifier::Indices,
            ast::TypeModifier::Payload => ir::InterpolationModifier::Payload,
            _ => continue,
        };

        if let Some((current_im, ref current_source)) = current_modifier {
            if current_im == next_im {
                // TODO: Warn for duplicate modifier
            } else {
                return Err(TyperError::ModifierConflict(
                    modifier.node,
                    modifier.location,
                    current_source.node,
                ));
            }
        } else {
            current_modifier = Some((next_im, modifier.clone()));
        }
    }
    Ok(current_modifier.map(|(im, m)| (im, m.location)))
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
pub fn deny_mesh_shader_modifiers(
    modifiers: &ast::TypeModifierSet,
    position: TypePosition,
) -> TyperResult<()> {
    for modifier in &modifiers.modifiers {
        if matches!(
            &modifier.node,
            ast::TypeModifier::Vertices
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
        ir::TypeLayer::TemplateParam(ref p) => match &remap[p.0 as usize].node {
            ir::TypeOrConstant::Type(ty) => *ty,
            ir::TypeOrConstant::Constant(_) => todo!("Non-type template arguments"),
        },
        ir::TypeLayer::Array(tyl, len) => {
            let inner_ty = apply_template_type_substitution(tyl, remap, context);
            let layer = ir::TypeLayer::Array(inner_ty, len);
            context.module.type_registry.register_type_layer(layer)
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
        ir::Constant::UntypedInt(v) => ir::RestrictedConstant::UntypedInt(v),
        ir::Constant::Int(v) => ir::RestrictedConstant::Int(v),
        ir::Constant::UInt(v) => ir::RestrictedConstant::UInt(v),
        ir::Constant::Long(v) => ir::RestrictedConstant::Long(v),
        // If the constant type can not be used for a template argument then claim it is not a constant expression
        _ => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
    };

    Ok(restricted)
}

/// Process a typedef
pub fn parse_rootdefinition_typedef(td: &ast::Typedef, context: &mut Context) -> TyperResult<()> {
    // Deny restricted non-keyword names
    if is_illegal_variable_name(&td.name) {
        return Err(TyperError::IllegalTypedefName(td.name.location));
    }

    // Parse the base type
    let base_type = parse_type(&td.source, context)?;
    let base_type_layout = context
        .module
        .type_registry
        .get_type_layout(base_type)
        .clone();

    // Apply the array modifier
    let type_layout = apply_variable_bind(base_type_layout, &td.bind, &None, context)?;
    let type_id = context.module.type_registry.register_type(type_layout);

    // Register the typedef
    context.register_typedef(td.name.clone(), type_id)?;

    // We do not emit any global definitions as typedefs do not declare anything new

    Ok(())
}

/// Check if a name is forbidden for a variable / function / typedef
pub fn is_illegal_variable_name(name: &Located<String>) -> bool {
    matches!(
        name.as_str(),
        "nointerpolation" | "linear" | "centroid" | "noperspective"
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
            | "vertices"
            | "primitives"
            | "indices"
            | "payload"
    )
}
