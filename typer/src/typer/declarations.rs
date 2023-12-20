use super::errors::*;
use super::scopes::*;
use crate::evaluator::evaluate_constexpr;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;

use super::expressions::parse_expr;

/// Process a declarator in a declaration
pub fn parse_declarator(
    declarator: &ast::Declarator,
    base_id: ir::TypeId,
    init: Option<&ast::Initializer>,
    allow_unbounded: bool,
    context: &mut Context,
) -> TyperResult<(ir::TypeId, ast::ScopedIdentifier)> {
    fn visit_declarator(
        declarator: &ast::Declarator,
    ) -> (Vec<&ast::Declarator>, ast::ScopedIdentifier) {
        match declarator {
            ast::Declarator::Empty => panic!("Empty declarator in type checking"),
            ast::Declarator::Identifier(scoped_name, _) => (Vec::new(), scoped_name.clone()),
            ast::Declarator::Pointer(ast::PointerDeclarator { inner, .. }) => {
                let (mut declarators, scoped_name) = visit_declarator(inner);
                declarators.push(declarator);
                (declarators, scoped_name)
            }
            ast::Declarator::Reference(ast::ReferenceDeclarator { inner, .. }) => {
                let (mut declarators, scoped_name) = visit_declarator(inner);
                declarators.push(declarator);
                (declarators, scoped_name)
            }
            ast::Declarator::Array(ast::ArrayDeclarator { inner, .. }) => {
                let (mut declarators, scoped_name) = visit_declarator(inner);
                declarators.push(declarator);
                (declarators, scoped_name)
            }
        }
    }

    // Accumulate declarators into a stack so we can apply them in reverse
    // Also retrieve the name from the bottom of the chain
    let (stack, scoped_name) = visit_declarator(declarator);

    // Get the location of the endpoint name for use when reporting errors
    let loc = scoped_name.get_location();

    let mut current_type = base_id;
    for declarator in stack.iter().rev() {
        let is_last = std::ptr::eq(declarator, stack.first().unwrap());
        match declarator {
            ast::Declarator::Empty | ast::Declarator::Identifier(_, _) => unreachable!(),
            ast::Declarator::Pointer(_) => return Err(TyperError::PointersNotSupported(loc)),
            ast::Declarator::Reference(_) => return Err(TyperError::PointersNotSupported(loc)),
            ast::Declarator::Array(ast::ArrayDeclarator { array_size, .. }) => {
                let constant_dim = match *array_size {
                    Some(ref dim_expr) => {
                        let expr_ir = parse_expr(dim_expr, context)?.0;
                        let value = match evaluate_constexpr(&expr_ir, &mut context.module) {
                            Ok(ir::Constant::Enum(_, val)) => val.to_uint64(),
                            Ok(val) => val.to_uint64(),
                            Err(()) => None,
                        };
                        match value {
                            Some(0) => {
                                return Err(TyperError::ArrayDimensionsMustBeNonZero(
                                    dim_expr.get_location(),
                                ))
                            }
                            Some(val) => Some(val),
                            None => {
                                let p = (***dim_expr).clone();
                                return Err(TyperError::ArrayDimensionsMustBeConstantExpression(
                                    p,
                                    dim_expr.get_location(),
                                ));
                            }
                        }
                    }
                    None => {
                        if is_last {
                            // The "first" array dimension (closest to name) can be taken from initializer length
                            match init {
                                Some(ast::Initializer::Aggregate(ref exprs))
                                    if exprs.is_empty() =>
                                {
                                    return Err(TyperError::ArrayDimensionsMustBeNonZero(loc))
                                }
                                Some(ast::Initializer::Aggregate(ref exprs)) => {
                                    Some(exprs.len() as u64)
                                }
                                _ if allow_unbounded => None,
                                _ => return Err(TyperError::ArrayDimensionNotSpecified(loc)),
                            }
                        } else {
                            // Multidimensional arrays must declare all dimensions except the first
                            return Err(TyperError::ArrayDimensionNotSpecified(loc));
                        }
                    }
                };

                current_type = context
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::Array(current_type, constant_dim));
            }
        }
    }

    Ok((current_type, scoped_name))
}
