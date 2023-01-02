use super::errors::*;
use super::expressions::parse_expr;
use super::scopes::*;
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
            let expr_ir = parse_expr(expr, context)?;
            let evaluated = match evaluate_constexpr(&expr_ir.0, &mut context.module) {
                Ok(value) => value,
                Err(_) => return Err(TyperError::ExpressionIsNotConstantExpression(expr.location)),
            };
            (evaluated, expr_ir.1 .0)
        } else {
            match last_value {
                None => (
                    ir::Constant::UntypedInt(0),
                    context
                        .module
                        .type_registry
                        .register_type_layer(ir::TypeLayer::Scalar(ir::ScalarType::UntypedInt)),
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

    context.end_enum();

    Ok(ir::RootDefinition::Enum(id))
}
