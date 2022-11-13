use super::errors::*;
use super::scopes::*;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;

/// Attempt to get an ir type from an ast type
pub fn parse_type(ty: &ast::Type, context: &mut Context) -> TyperResult<ir::Type> {
    let ast::Type(ast_tyl, direct_modifier) = ty;
    let ir::Type(ir_tyl, base_modifier) = parse_typelayout(ast_tyl, context)?;
    // Matrix ordering not properly handled
    assert_eq!(base_modifier.row_order, direct_modifier.row_order);
    let modifier = ir::TypeModifier {
        is_const: base_modifier.is_const || direct_modifier.is_const,
        row_order: direct_modifier.row_order,
        precise: base_modifier.precise || direct_modifier.precise,
        volatile: base_modifier.volatile || direct_modifier.volatile,
    };
    Ok(ir::Type(ir_tyl, modifier))
}

/// Attempt to get an ir type layout from an ast type layout
pub fn parse_typelayout(ty: &ast::TypeLayout, context: &mut Context) -> TyperResult<ir::Type> {
    Ok(match *ty {
        ast::TypeLayout::Void => ir::Type::void(),
        ast::TypeLayout::Scalar(scalar) => ir::Type::from_scalar(scalar),
        ast::TypeLayout::Vector(scalar, x) => ir::Type::from_vector(scalar, x),
        ast::TypeLayout::Matrix(scalar, x, y) => ir::Type::from_matrix(scalar, x, y),
        ast::TypeLayout::Custom(ref name, ref args) => {
            let mut ir_args = Vec::with_capacity(args.len());
            for arg in args {
                ir_args.push(parse_type(arg, context)?);
            }

            // Special case all the object types for now
            if let Some(object_type) = parse_object_type(name, &ir_args) {
                return Ok(ir::Type::from_layout(object_type));
            }

            // Translate type arguments first
            let mut ir_args = Vec::with_capacity(args.len());
            for arg in args {
                ir_args.push(parse_type(arg, context)?);
            }

            context.find_type_id(name, &ir_args)?
        }
    })
}

/// Attempt to turn an ast type into one of the built in object types
fn parse_object_type(
    name: &ast::ScopedIdentifier,
    template_args: &[ir::Type],
) -> Option<ir::TypeLayout> {
    let get_data_type = |args: &[ir::Type], default_float4: bool| match args {
        [ir::Type(ir::TypeLayout::Scalar(st), modifier)] => {
            Some(ir::DataType(ir::DataLayout::Scalar(*st), *modifier))
        }
        [ir::Type(ir::TypeLayout::Vector(st, x), modifier)] => {
            Some(ir::DataType(ir::DataLayout::Vector(*st, *x), *modifier))
        }
        [] if default_float4 => Some(ir::DataType(
            ir::DataLayout::Vector(ir::ScalarType::Float, 4),
            ir::TypeModifier::default(),
        )),
        _ => None,
    };
    let get_structured_type = |args: &[ir::Type]| match args {
        [ir::Type(ir::TypeLayout::Scalar(st), modifier)] => Some(ir::StructuredType(
            ir::StructuredLayout::Scalar(*st),
            *modifier,
        )),
        [ir::Type(ir::TypeLayout::Vector(st, x), modifier)] => Some(ir::StructuredType(
            ir::StructuredLayout::Vector(*st, *x),
            *modifier,
        )),
        [ir::Type(ir::TypeLayout::Struct(id), modifier)] => Some(ir::StructuredType(
            ir::StructuredLayout::Struct(*id),
            *modifier,
        )),
        _ => None,
    };

    // Special object types are all unscoped names
    if name.identifiers.len() != 1 {
        return None;
    }

    match name.identifiers[0].node.as_str() {
        "Buffer" => Some(ir::TypeLayout::Object(ir::ObjectType::Buffer(
            get_data_type(template_args, true)?,
        ))),
        "RWBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::RWBuffer(
            get_data_type(template_args, false)?,
        ))),
        "ByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::ByteAddressBuffer))
        }
        "RWByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::RWByteAddressBuffer))
        }
        "Texture2D" => Some(ir::TypeLayout::Object(ir::ObjectType::Texture2D(
            get_data_type(template_args, true)?,
        ))),
        "RWTexture2D" => Some(ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(
            get_data_type(template_args, false)?,
        ))),
        "ConstantBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::ConstantBuffer(
            get_structured_type(template_args)?,
        ))),
        "StructuredBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::StructuredBuffer(
            get_structured_type(template_args)?,
        ))),
        "RWStructuredBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::RWStructuredBuffer(
            get_structured_type(template_args)?,
        ))),
        _ => None,
    }
}

/// Replace instances of a template type parameter in a type with a concrete type
pub fn apply_template_type_substitution(
    source_type: ir::Type,
    remap: &[Located<ir::Type>],
) -> ir::Type {
    match source_type {
        ir::Type(ir::TypeLayout::TemplateParam(ref p), _) => remap[p.0 as usize].node.clone(),
        ir::Type(ir::TypeLayout::Array(tyl, len), modifier) => {
            // Arrays currently can only contain types without modifiers - add default modifier
            let tyl_as_ty = ir::Type::from_layout(*tyl);
            let inner_ty = apply_template_type_substitution(tyl_as_ty, remap);
            // TODO: modifiers inside array elements
            assert_eq!(inner_ty.1, ir::TypeModifier::default());
            ir::Type(ir::TypeLayout::Array(Box::new(inner_ty.0), len), modifier)
        }
        t => t,
    }
}
