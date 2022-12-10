use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::{Located, SourceLocation};

/// Attempt to get an ir type from an ast type
pub fn parse_type(ty: &ast::Type, context: &mut Context) -> TyperResult<ir::TypeId> {
    let direct_modifier = ty.modifier;
    let parsed_id = parse_typelayout(&ty.layout, context)?;
    let (ir_ty, base_modifier) = context.module.type_registry.extract_modifier(parsed_id);
    // Matrix ordering not properly handled
    assert_eq!(base_modifier.row_order, direct_modifier.row_order);
    let modifier = ir::TypeModifier {
        is_const: base_modifier.is_const || direct_modifier.is_const,
        row_order: direct_modifier.row_order,
        volatile: base_modifier.volatile || direct_modifier.volatile,
    };
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

            if let Some(ty) = parse_voidtype(name, &ir_args) {
                let id = context.module.type_registry.register_type(ty);
                return Ok(id);
            }

            if let Some(ty) = parse_data_layout(name, &ir_args, context) {
                let id = context.module.type_registry.register_type(ty);
                return Ok(id);
            }

            if let Some(object_type) = parse_object_type(name, &ir_args, context) {
                let id = context.module.type_registry.register_type(object_type);
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

enum PrimitiveTypeError {
    UnexpectedCharacter,
    EndOfStream,
}

// Parse scalar type as part of a string
fn parse_scalartype_str(input: &[u8]) -> Result<(&[u8], ir::ScalarType), PrimitiveTypeError> {
    match input {
        [b'b', b'o', b'o', b'l', rest @ ..] => Ok((rest, ir::ScalarType::Bool)),
        [b'i', b'n', b't', rest @ ..] => Ok((rest, ir::ScalarType::Int)),
        [b'u', b'i', b'n', b't', rest @ ..] => Ok((rest, ir::ScalarType::UInt)),
        [b'd', b'w', b'o', b'r', b'd', rest @ ..] => Ok((rest, ir::ScalarType::UInt)),
        [b'h', b'a', b'l', b'f', rest @ ..] => Ok((rest, ir::ScalarType::Half)),
        [b'f', b'l', b'o', b'a', b't', rest @ ..] => Ok((rest, ir::ScalarType::Float)),
        [b'd', b'o', b'u', b'b', b'l', b'e', rest @ ..] => Ok((rest, ir::ScalarType::Double)),
        _ => Err(PrimitiveTypeError::UnexpectedCharacter),
    }
}

// Parse data type as part of a string
fn parse_datalayout_str(typename: &str) -> Option<ir::TypeLayout> {
    fn digit(input: &[u8]) -> Result<(&[u8], u32), PrimitiveTypeError> {
        // Handle end of stream
        if input.is_empty() {
            return Err(PrimitiveTypeError::EndOfStream);
        };

        // Match on the next character
        let n = match input[0] {
            b'1' => 1,
            b'2' => 2,
            b'3' => 3,
            b'4' => 4,
            _ => {
                // Not a digit
                return Err(PrimitiveTypeError::UnexpectedCharacter);
            }
        };

        // Success
        Ok((&input[1..], n))
    }

    fn parse_str(input: &[u8]) -> Result<(&[u8], ir::TypeLayout), PrimitiveTypeError> {
        let (rest, ty) = parse_scalartype_str(input)?;
        if rest.is_empty() {
            return Ok((&[], ir::TypeLayout::Scalar(ty)));
        }

        let (rest, x) = digit(rest)?;
        if rest.is_empty() {
            return Ok((&[], ir::TypeLayout::Vector(ty, x)));
        }

        let rest = match rest.first() {
            Some(b'x') => &rest[1..],
            _ => return Err(PrimitiveTypeError::UnexpectedCharacter),
        };

        let (rest, y) = digit(rest)?;
        if rest.is_empty() {
            return Ok((&[], ir::TypeLayout::Matrix(ty, x, y)));
        }

        Err(PrimitiveTypeError::UnexpectedCharacter)
    }

    match parse_str(typename[..].as_bytes()) {
        Ok((rest, ty)) => {
            assert_eq!(rest.len(), 0);
            Some(ty)
        }
        Err(_) => None,
    }
}

#[test]
fn test_parse_datalayout_str() {
    assert_eq!(
        parse_datalayout_str("float"),
        Some(ir::TypeLayout::Scalar(ir::ScalarType::Float))
    );
    assert_eq!(
        parse_datalayout_str("uint3"),
        Some(ir::TypeLayout::Vector(ir::ScalarType::UInt, 3))
    );
    assert_eq!(
        parse_datalayout_str("bool2x3"),
        Some(ir::TypeLayout::Matrix(ir::ScalarType::Bool, 2, 3))
    );

    assert_eq!(parse_datalayout_str(""), None);
    assert_eq!(parse_datalayout_str("float5"), None);
    assert_eq!(parse_datalayout_str("float2x"), None);
}

/// Parse a type layout for a basic data type
pub fn parse_data_layout(
    name: &ast::ScopedIdentifier,
    template_args: &[ir::TypeOrConstant],
    context: &mut Context,
) -> Option<ir::TypeLayout> {
    if let Some(name) = name.try_trivial() {
        if template_args.is_empty() {
            parse_datalayout_str(name.as_str())
        } else {
            match name.node.as_str() {
                "vector" => {
                    if template_args.len() == 2 {
                        let type_id = match template_args[0] {
                            ir::TypeOrConstant::Type(id) => id,
                            _ => return None,
                        };

                        // Type modifiers not supported inside arguments
                        let s = match context.module.type_registry.get_type_layer(type_id) {
                            ir::TypeLayer::Scalar(s) => *s,
                            _ => return None,
                        };

                        let dim_x = match template_args[1] {
                            ir::TypeOrConstant::Constant(ir::Constant::UntypedInt(v))
                            | ir::TypeOrConstant::Constant(ir::Constant::Int(v))
                            | ir::TypeOrConstant::Constant(ir::Constant::UInt(v))
                                if (1..=4).contains(&v) =>
                            {
                                v
                            }
                            _ => return None,
                        };

                        Some(ir::TypeLayout::Vector(s, dim_x as u32))
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
                        let s = match context.module.type_registry.get_type_layer(type_id) {
                            ir::TypeLayer::Scalar(s) => *s,
                            _ => return None,
                        };

                        let dim_x = match template_args[1] {
                            ir::TypeOrConstant::Constant(ir::Constant::UntypedInt(v))
                            | ir::TypeOrConstant::Constant(ir::Constant::Int(v))
                            | ir::TypeOrConstant::Constant(ir::Constant::UInt(v))
                                if (1..=4).contains(&v) =>
                            {
                                v
                            }
                            _ => return None,
                        };

                        let dim_y = match template_args[2] {
                            ir::TypeOrConstant::Constant(ir::Constant::UntypedInt(v))
                            | ir::TypeOrConstant::Constant(ir::Constant::Int(v))
                            | ir::TypeOrConstant::Constant(ir::Constant::UInt(v))
                                if (1..=4).contains(&v) =>
                            {
                                v
                            }
                            _ => return None,
                        };

                        Some(ir::TypeLayout::Matrix(s, dim_x as u32, dim_y as u32))
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
) -> Option<ir::TypeLayout> {
    if let Some(name) = name.try_trivial() {
        if name.node == "void" && template_args.is_empty() {
            Some(ir::TypeLayout::Void)
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
) -> Option<ir::TypeLayout> {
    fn get_data_type(
        args: &[ir::TypeOrConstant],
        default_float4: bool,
        context: &mut Context,
    ) -> Option<ir::DataType> {
        if let [ir::TypeOrConstant::Type(id)] = args {
            let (id, modifier) = context.module.type_registry.extract_modifier(*id);
            let layer = context.module.type_registry.get_type_layer(id);
            match layer {
                ir::TypeLayer::Scalar(st) => {
                    Some(ir::DataType(ir::DataLayout::Scalar(*st), modifier))
                }
                ir::TypeLayer::Vector(st, x) => {
                    Some(ir::DataType(ir::DataLayout::Vector(*st, *x), modifier))
                }
                _ => None,
            }
        } else if default_float4 {
            Some(ir::DataType(
                ir::DataLayout::Vector(ir::ScalarType::Float, 4),
                ir::TypeModifier::default(),
            ))
        } else {
            None
        }
    }

    fn get_structured_type(
        args: &[ir::TypeOrConstant],
        context: &mut Context,
    ) -> Option<ir::StructuredType> {
        if let [ir::TypeOrConstant::Type(id)] = args {
            let (id, modifier) = context.module.type_registry.extract_modifier(*id);
            let layer = context.module.type_registry.get_type_layer(id);
            match layer {
                ir::TypeLayer::Scalar(st) => Some(ir::StructuredType(
                    ir::StructuredLayout::Scalar(*st),
                    modifier,
                )),
                ir::TypeLayer::Vector(st, x) => Some(ir::StructuredType(
                    ir::StructuredLayout::Vector(*st, *x),
                    modifier,
                )),
                ir::TypeLayer::Struct(id) => Some(ir::StructuredType(
                    ir::StructuredLayout::Struct(*id),
                    modifier,
                )),
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
        "Buffer" => Some(ir::TypeLayout::Object(ir::ObjectType::Buffer(
            get_data_type(template_args, true, context)?,
        ))),
        "RWBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::RWBuffer(
            get_data_type(template_args, false, context)?,
        ))),
        "ByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::ByteAddressBuffer))
        }
        "RWByteAddressBuffer" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::RWByteAddressBuffer))
        }
        "BufferAddress" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::BufferAddress))
        }
        "RWBufferAddress" if template_args.is_empty() => {
            Some(ir::TypeLayout::Object(ir::ObjectType::RWBufferAddress))
        }
        "Texture2D" => Some(ir::TypeLayout::Object(ir::ObjectType::Texture2D(
            get_data_type(template_args, true, context)?,
        ))),
        "RWTexture2D" => Some(ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(
            get_data_type(template_args, false, context)?,
        ))),
        "ConstantBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::ConstantBuffer(
            get_structured_type(template_args, context)?,
        ))),
        "StructuredBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::StructuredBuffer(
            get_structured_type(template_args, context)?,
        ))),
        "RWStructuredBuffer" => Some(ir::TypeLayout::Object(ir::ObjectType::RWStructuredBuffer(
            get_structured_type(template_args, context)?,
        ))),
        "SamplerState" => Some(ir::TypeLayout::Object(ir::ObjectType::SamplerState)),
        _ => None,
    }
}

/// Replace instances of a template type parameter in a type with a concrete type
pub fn apply_template_type_substitution(
    source_type: ir::TypeId,
    remap: &[Located<ir::TypeOrConstant>],
    context: &mut Context,
) -> ir::TypeId {
    match *context.module.type_registry.get_type_layer(source_type) {
        ir::TypeLayer::Modifier(modifier, tyl) => {
            let inner_ty = apply_template_type_substitution(tyl, remap, context);
            let layer = ir::TypeLayer::Modifier(modifier, inner_ty);
            context.module.type_registry.register_type_layer(layer)
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
) -> TyperResult<ir::Constant> {
    let ir_expr = parse_expr(expr, context)?;
    evaluate_constant_expression(&ir_expr.0, expr.location, context)
}

/// Attempt to evaluate an expression as a constant expression
fn evaluate_constant_expression(
    expr: &ir::Expression,
    source_location: SourceLocation,
    _: &mut Context,
) -> TyperResult<ir::Constant> {
    let c = match *expr {
        ir::Expression::Literal(ir::Literal::Bool(v)) => ir::Constant::Bool(v),
        ir::Expression::Literal(ir::Literal::UntypedInt(v)) => ir::Constant::UntypedInt(v),
        ir::Expression::Literal(ir::Literal::Int(v)) => ir::Constant::Int(v),
        ir::Expression::Literal(ir::Literal::UInt(v)) => ir::Constant::UInt(v),
        _ => {
            return Err(TyperError::ExpressionIsNotConstantExpression(
                source_location,
            ))
        }
    };
    Ok(c)
}
