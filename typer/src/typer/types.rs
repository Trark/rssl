use super::errors::*;
use super::scopes::*;
use rssl_ast as ast;
use rssl_ir as ir;

/// Attempt to get an ir type from an ast type
pub fn parse_type(ty: &ast::Type, struct_finder: &dyn StructIdFinder) -> TyperResult<ir::Type> {
    let &ast::Type(ref tyl, modifier) = ty;
    Ok(ir::Type(parse_typelayout(tyl, struct_finder)?, modifier))
}

/// Attempt to get an ir type layout from an ast type layout
fn parse_typelayout(
    ty: &ast::TypeLayout,
    struct_finder: &dyn StructIdFinder,
) -> TyperResult<ir::TypeLayout> {
    Ok(match *ty {
        ast::TypeLayout::Void => ir::TypeLayout::Void,
        ast::TypeLayout::Scalar(scalar) => ir::TypeLayout::Scalar(scalar),
        ast::TypeLayout::Vector(scalar, x) => ir::TypeLayout::Vector(scalar, x),
        ast::TypeLayout::Matrix(scalar, x, y) => ir::TypeLayout::Matrix(scalar, x, y),
        ast::TypeLayout::Custom(ref name) => {
            ir::TypeLayout::Struct(struct_finder.find_struct_id(name)?)
        }
        ast::TypeLayout::SamplerState => ir::TypeLayout::SamplerState,
        ast::TypeLayout::Object(ref object_type) => {
            ir::TypeLayout::Object(parse_objecttype(object_type, struct_finder)?)
        }
    })
}

/// Attempt to get an ir structured type from an ast structured type
fn parse_structuredtype(
    ty: &ast::StructuredType,
    struct_finder: &dyn StructIdFinder,
) -> TyperResult<ir::StructuredType> {
    let &ast::StructuredType(ref tyl, modifier) = ty;
    Ok(ir::StructuredType(
        parse_structuredlayout(tyl, struct_finder)?,
        modifier,
    ))
}

/// Attempt to get an ir structured type layout from an ast structured type layout
fn parse_structuredlayout(
    ty: &ast::StructuredLayout,
    struct_finder: &dyn StructIdFinder,
) -> TyperResult<ir::StructuredLayout> {
    Ok(match *ty {
        ast::StructuredLayout::Scalar(scalar) => ir::StructuredLayout::Scalar(scalar),
        ast::StructuredLayout::Vector(scalar, x) => ir::StructuredLayout::Vector(scalar, x),
        ast::StructuredLayout::Matrix(scalar, x, y) => ir::StructuredLayout::Matrix(scalar, x, y),
        ast::StructuredLayout::Custom(ref name) => {
            ir::StructuredLayout::Struct(struct_finder.find_struct_id(name)?)
        }
    })
}

/// Attempt to get an ir object type from an ast object type
fn parse_objecttype(
    ty: &ast::ObjectType,
    struct_finder: &dyn StructIdFinder,
) -> TyperResult<ir::ObjectType> {
    Ok(match *ty {
        ast::ObjectType::Buffer(data_type) => ir::ObjectType::Buffer(data_type),
        ast::ObjectType::RWBuffer(data_type) => ir::ObjectType::RWBuffer(data_type),
        ast::ObjectType::ByteAddressBuffer => ir::ObjectType::ByteAddressBuffer,
        ast::ObjectType::RWByteAddressBuffer => ir::ObjectType::RWByteAddressBuffer,
        ast::ObjectType::StructuredBuffer(ref structured_type) => {
            ir::ObjectType::StructuredBuffer(parse_structuredtype(structured_type, struct_finder)?)
        }
        ast::ObjectType::RWStructuredBuffer(ref structured_type) => {
            ir::ObjectType::RWStructuredBuffer(parse_structuredtype(
                structured_type,
                struct_finder,
            )?)
        }
        ast::ObjectType::AppendStructuredBuffer(ref structured_type) => {
            ir::ObjectType::AppendStructuredBuffer(parse_structuredtype(
                structured_type,
                struct_finder,
            )?)
        }
        ast::ObjectType::ConsumeStructuredBuffer(ref structured_type) => {
            ir::ObjectType::ConsumeStructuredBuffer(parse_structuredtype(
                structured_type,
                struct_finder,
            )?)
        }
        ast::ObjectType::Texture1D(data_type) => ir::ObjectType::Texture1D(data_type),
        ast::ObjectType::Texture1DArray(data_type) => ir::ObjectType::Texture1DArray(data_type),
        ast::ObjectType::Texture2D(data_type) => ir::ObjectType::Texture2D(data_type),
        ast::ObjectType::Texture2DArray(data_type) => ir::ObjectType::Texture2DArray(data_type),
        ast::ObjectType::Texture2DMS(data_type) => ir::ObjectType::Texture2DMS(data_type),
        ast::ObjectType::Texture2DMSArray(data_type) => ir::ObjectType::Texture2DMSArray(data_type),
        ast::ObjectType::Texture3D(data_type) => ir::ObjectType::Texture3D(data_type),
        ast::ObjectType::TextureCube(data_type) => ir::ObjectType::TextureCube(data_type),
        ast::ObjectType::TextureCubeArray(data_type) => ir::ObjectType::TextureCubeArray(data_type),
        ast::ObjectType::RWTexture1D(data_type) => ir::ObjectType::RWTexture1D(data_type),
        ast::ObjectType::RWTexture1DArray(data_type) => ir::ObjectType::RWTexture1DArray(data_type),
        ast::ObjectType::RWTexture2D(data_type) => ir::ObjectType::RWTexture2D(data_type),
        ast::ObjectType::RWTexture2DArray(data_type) => ir::ObjectType::RWTexture2DArray(data_type),
        ast::ObjectType::RWTexture3D(data_type) => ir::ObjectType::RWTexture3D(data_type),
        ast::ObjectType::ConstantBuffer(ref structured_type) => {
            ir::ObjectType::ConstantBuffer(parse_structuredtype(structured_type, struct_finder)?)
        }
        ast::ObjectType::InputPatch => ir::ObjectType::InputPatch,
        ast::ObjectType::OutputPatch => ir::ObjectType::OutputPatch,
    })
}
