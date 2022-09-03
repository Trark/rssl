use super::errors::*;
use super::scopes::*;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::Located;

/// Attempt to get an ir type from an ast type
pub fn parse_type(ty: &ast::Type, context: &Context) -> TyperResult<ir::Type> {
    let &ast::Type(ref tyl, modifier) = ty;
    Ok(ir::Type(parse_typelayout(tyl, context)?, modifier))
}

/// Attempt to get an ir type layout from an ast type layout
pub fn parse_typelayout(ty: &ast::TypeLayout, context: &Context) -> TyperResult<ir::TypeLayout> {
    Ok(match *ty {
        ast::TypeLayout::Void => ir::TypeLayout::Void,
        ast::TypeLayout::Scalar(scalar) => ir::TypeLayout::Scalar(scalar),
        ast::TypeLayout::Vector(scalar, x) => ir::TypeLayout::Vector(scalar, x),
        ast::TypeLayout::Matrix(scalar, x, y) => ir::TypeLayout::Matrix(scalar, x, y),
        ast::TypeLayout::Custom(ref name, ref args) => {
            let mut ir_args = Vec::with_capacity(args.len());
            for arg in args {
                ir_args.push(parse_type(arg, context)?);
            }

            // Special case all the object types for now
            if let Some(object_type) = parse_object_type(name, &ir_args) {
                return Ok(object_type);
            }

            // No support for template args on generic types
            if !args.is_empty() {
                unimplemented!("Template args not implemented for {}", name)
            }

            context.find_type_id(name)?
        }
    })
}

/// Attempt to turn an ast type into one of the built in object types
fn parse_object_type(name: &str, template_args: &[ir::Type]) -> Option<ir::TypeLayout> {
    let get_data_type = |args: &[ir::Type]| match args {
        [ir::Type(ir::TypeLayout::Scalar(st), modifier)] => {
            Some(ir::DataType(ir::DataLayout::Scalar(*st), *modifier))
        }
        [ir::Type(ir::TypeLayout::Vector(st, x), modifier)] => {
            Some(ir::DataType(ir::DataLayout::Vector(*st, *x), *modifier))
        }
        [] => Some(ir::DataType(
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
    match name {
        "Buffer" => Some(ir::TypeLayout::Object(ir::ObjectType::Buffer(
            get_data_type(template_args)?,
        ))),
        "RWBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::RWBuffer(
            get_data_type(template_args)?,
        ))),
        "ByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::ByteAddressBuffer))
        }
        "RWByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::RWByteAddressBuffer))
        }
        "Texture2D" => Some(ir::TypeLayout::Object(ir::ObjectType::Texture2D(
            get_data_type(template_args)?,
        ))),
        "RWTexture2D" => Some(ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(
            get_data_type(template_args)?,
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
        ir::Type(ir::TypeLayout::Array(_, _), _) => {
            todo!("Arrays of templated types are not implemented")
        }
        t => t,
    }
}
