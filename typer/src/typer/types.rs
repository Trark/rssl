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

// Parse numeric type as part of a string
fn parse_numeric_str(typename: &str) -> Option<ir::TypeLayout> {
    let layer = ir::TypeLayer::from_numeric_str(typename)?;
    let layout = ir::TypeLayout::from_numeric_layer(layer)?;
    Some(layout)
}

/// Parse a type layout for a basic data type
pub fn parse_data_layout(
    name: &ast::ScopedIdentifier,
    template_args: &[ir::TypeOrConstant],
    context: &mut Context,
) -> Option<ir::TypeLayout> {
    if let Some(name) = name.try_trivial() {
        if template_args.is_empty() {
            parse_numeric_str(name.as_str())
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
                            ir::TypeLayer::Scalar(s) => s,
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
                            ir::TypeLayer::Scalar(s) => s,
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
    ) -> Option<ir::TypeId> {
        if let [ir::TypeOrConstant::Type(id)] = args {
            let id_nomod = context.module.type_registry.remove_modifier(*id);
            let layer = context.module.type_registry.get_type_layer(id_nomod);
            match layer {
                ir::TypeLayer::Scalar(_) | ir::TypeLayer::Vector(_, _) => Some(*id),
                _ => None,
            }
        } else if default_float4 {
            let f4 = context
                .module
                .type_registry
                .register_type_layer(ir::TypeLayer::Vector(ir::ScalarType::Float, 4));
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
    match context.module.type_registry.get_type_layer(source_type) {
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
