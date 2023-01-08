use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
use crate::casting::ImplicitConversion;
use crate::evaluator::evaluate_constexpr;
use rssl_ast as ast;
use rssl_ir as ir;

/// Process an enum definition
pub fn parse_rootdefinition_enum(
    sd: &ast::EnumDefinition,
    context: &mut Context,
) -> TyperResult<ir::RootDefinition> {
    // Register the enumf
    let name = &sd.name;
    let id = match context.begin_enum(name.clone()) {
        Ok(id) => id,
        Err(id) => return Err(TyperError::TypeAlreadyDefined(name.clone(), id)),
    };

    let mut last_value: Option<(ir::Constant, ir::TypeId)> = None;
    for member in &sd.values {
        let (value, ty) = if let Some(expr) = &member.value {
            let mut expr_ir = parse_expr(expr, context)?;

            let unmodified_id = context.module.type_registry.remove_modifier(expr_ir.1 .0);
            match context.module.type_registry.get_type_layer(unmodified_id) {
                ir::TypeLayer::Scalar(ir::ScalarType::Bool)
                | ir::TypeLayer::Scalar(ir::ScalarType::UntypedInt)
                | ir::TypeLayer::Scalar(ir::ScalarType::Int)
                | ir::TypeLayer::Scalar(ir::ScalarType::UInt) => {
                    // Integer types are allowed
                }
                ir::TypeLayer::Enum(id) => {
                    // Enum types are allowed - treat them as if they cast to their underlying type
                    let underlying_type = context.module.enum_registry.get_underlying_type_id(id);
                    let cast = ImplicitConversion::find(
                        expr_ir.1,
                        underlying_type.to_rvalue(),
                        &mut context.module,
                    )
                    .unwrap();
                    expr_ir.0 = cast.apply(expr_ir.0, &mut context.module)
                }
                _ => {
                    // Other types are not allowed
                    return Err(TyperError::EnumValueMustBeInteger(expr.location));
                }
            }

            let evaluated = match evaluate_constexpr(&expr_ir.0, &mut context.module) {
                Ok(value) => value,
                Err(_) => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
            };
            (evaluated, expr_ir.1 .0)
        } else {
            match last_value {
                None => (
                    ir::Constant::Int(0),
                    context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Int)),
                ),
                Some(last_value) => {
                    let next_value = match last_value.0 {
                        ir::Constant::UntypedInt(v) => ir::Constant::UntypedInt(v + 1),
                        ir::Constant::Int(v) => ir::Constant::Int(v + 1),
                        ir::Constant::UInt(v) => ir::Constant::UInt(v + 1),
                        _ => panic!("Unexpected constant type in enum value"),
                    };
                    (next_value, last_value.1)
                }
            }
        };

        context.register_enum_value(id, member.name.clone(), value.clone(), ty)?;

        last_value = Some((value, ty));
    }

    context.end_enum()?;

    Ok(ir::RootDefinition::Enum(id))
}
