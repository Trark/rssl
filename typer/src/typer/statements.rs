use super::errors::*;
use super::scopes::*;
use crate::casting::ImplicitConversion;
use ir::{ExpressionType, ToExpressionType};
use rssl_ast as ast;
use rssl_ir as ir;

use super::expressions::parse_expr;
use super::types::parse_type;

/// Type check a list of ast statements into ir statements
pub fn parse_statement_list(
    ast: &[ast::Statement],
    context: &mut Context,
) -> TyperResult<Vec<ir::Statement>> {
    let mut body_ir = vec![];
    for statement_ast in ast {
        let mut statement_ir_vec = parse_statement(statement_ast, context)?;
        body_ir.append(&mut statement_ir_vec);
    }
    Ok(body_ir)
}

/// Parse the statement inside a block caused by an statement (if, for, while, etc),
/// with the intention of merging the scope created by the outer statement and the
/// scope of it's inner statement. This is to force a scope on single statements
/// inside the outer statement and cause a name conflict on using the same variable
/// in the inner statement as in the init expression of a for loop
///
/// A block statement will be parsed as if it doesn't create a new scope
///
/// Any other statement will be parsed normally (with the provided ScopeContext),
/// meaning any declarations it makes will end up in the outer statements scoped
/// declarations.
///
/// The given context is the newly created scoped context for the outer statement.
/// block_context is consumed and turned into a declarations list because all uses
/// of this execute it on the inner statement as the last operation in parsing a
/// loop
fn parse_scopeblock(ast: &ast::Statement, context: &mut Context) -> TyperResult<ir::ScopeBlock> {
    match *ast {
        ast::Statement::Block(ref statement_vec) => {
            let statements = parse_statement_list(statement_vec, context)?;
            Ok(ir::ScopeBlock(statements, context.pop_scope_with_locals()))
        }
        _ => {
            let ir_statements = parse_statement(ast, context)?;
            Ok(ir::ScopeBlock(
                ir_statements,
                context.pop_scope_with_locals(),
            ))
        }
    }
}

/// Process a single statement
fn parse_statement(ast: &ast::Statement, context: &mut Context) -> TyperResult<Vec<ir::Statement>> {
    match ast {
        ast::Statement::Empty => Ok(vec![]),
        ast::Statement::Expression(ref expr) => {
            let (expr_ir, _) = parse_expr(expr, context)?;
            Ok(vec![ir::Statement::Expression(expr_ir)])
        }
        ast::Statement::Var(ref vd) => {
            let vd_ir = parse_vardef(vd, context)?;
            let vars = vd_ir
                .into_iter()
                .map(ir::Statement::Var)
                .collect::<Vec<_>>();
            Ok(vars)
        }
        ast::Statement::Block(ref statement_vec) => {
            context.push_scope();
            let statements = parse_statement_list(statement_vec, context)?;
            let decls = context.pop_scope_with_locals();
            Ok(vec![ir::Statement::Block(ir::ScopeBlock(
                statements, decls,
            ))])
        }
        ast::Statement::If(ref cond, ref statement) => {
            context.push_scope();
            let cond_ir = parse_expr(cond, context)?.0;
            let scope_block = parse_scopeblock(statement, context)?;
            Ok(vec![ir::Statement::If(cond_ir, scope_block)])
        }
        ast::Statement::IfElse(ref cond, ref true_statement, ref false_statement) => {
            context.push_scope();
            let cond_ir = parse_expr(cond, context)?.0;
            let scope_block = parse_scopeblock(true_statement, context)?;
            context.push_scope();
            let else_block = parse_scopeblock(false_statement, context)?;
            Ok(vec![ir::Statement::IfElse(
                cond_ir,
                scope_block,
                else_block,
            )])
        }
        ast::Statement::For(ref init, ref cond, ref iter, ref statement) => {
            context.push_scope();
            let init_ir = parse_for_init(init, context)?;
            let cond_ir = parse_expr(cond, context)?.0;
            let iter_ir = parse_expr(iter, context)?.0;
            let scope_block = parse_scopeblock(statement, context)?;
            Ok(vec![ir::Statement::For(
                init_ir,
                cond_ir,
                iter_ir,
                scope_block,
            )])
        }
        ast::Statement::While(ref cond, ref statement) => {
            context.push_scope();
            let cond_ir = parse_expr(cond, context)?.0;
            let scope_block = parse_scopeblock(statement, context)?;
            Ok(vec![ir::Statement::While(cond_ir, scope_block)])
        }
        ast::Statement::Break => Ok(vec![ir::Statement::Break]),
        ast::Statement::Continue => Ok(vec![ir::Statement::Continue]),
        ast::Statement::Return(Some(ref expr)) => {
            let (expr_ir, expr_ty) = parse_expr(expr, context)?;
            let expected_ety = context.get_current_return_type().to_rvalue();
            match ImplicitConversion::find(&expr_ty, &expected_ety) {
                Ok(rhs_cast) => Ok(vec![ir::Statement::Return(Some(rhs_cast.apply(expr_ir)))]),
                Err(()) => Err(TyperError::WrongTypeInReturnStatement(
                    expr_ty.0,
                    expected_ety.0,
                )),
            }
        }
        ast::Statement::Return(None) => {
            let expected_type = context.get_current_return_type();
            if expected_type.is_void() {
                Ok(Vec::from([ir::Statement::Return(None)]))
            } else {
                Err(TyperError::WrongTypeInReturnStatement(
                    ir::Type::void(),
                    expected_type,
                ))
            }
        }
    }
}

/// Process a variable definition
fn parse_vardef(ast: &ast::VarDef, context: &mut Context) -> TyperResult<Vec<ir::VarDef>> {
    let base_type = parse_localtype(&ast.local_type, context)?;

    // Build multiple output VarDefs for each variable inside the source VarDef
    let mut vardefs = vec![];
    for local_variable in &ast.defs {
        // Get variable name
        let var_name = &local_variable.name.clone();

        // Build type from ast type + bind
        let ir::LocalType(lty, ls) = base_type.clone();
        let bind = &local_variable.bind;
        let lv_tyl = apply_variable_bind(lty, bind, &local_variable.init)?;
        let lv_type = ir::LocalType(lv_tyl, ls);

        // Parse the initializer
        let var_init = parse_initializer_opt(&local_variable.init, &(lv_type.0).0, context)?;

        // Register the variable
        let var_id = context.insert_variable(var_name.clone(), lv_type.0.clone())?;

        // Add the variables creation node
        vardefs.push(ir::VarDef {
            id: var_id,
            local_type: lv_type,
            init: var_init,
        });
    }

    Ok(vardefs)
}

/// Convert a type for a local variable
fn parse_localtype(
    local_type: &ast::LocalType,
    context: &mut Context,
) -> TyperResult<ir::LocalType> {
    let ty = parse_type(&local_type.0, context)?;
    Ok(ir::LocalType(ty, local_type.1.clone()))
}

/// Apply part of type applied to variable name onto the type itself
pub fn apply_variable_bind(
    ty: ir::Type,
    bind: &ast::VariableBind,
    init: &Option<ast::Initializer>,
) -> TyperResult<ir::Type> {
    match *bind {
        ast::VariableBind::Array(ref dim) => {
            let ir::Type(layout, modifiers) = ty;

            let constant_dim = match *dim {
                Some(ref dim_expr) => match evaluate_constexpr_int(&**dim_expr) {
                    Ok(val) => val,
                    Err(()) => {
                        let p = (**dim_expr).clone();
                        return Err(TyperError::ArrayDimensionsMustBeConstantExpression(p));
                    }
                },
                None => match *init {
                    Some(ast::Initializer::Aggregate(ref exprs)) => exprs.len() as u64,
                    _ => return Err(TyperError::ArrayDimensionNotSpecified),
                },
            };

            Ok(ir::Type(
                ir::TypeLayout::Array(Box::new(layout), constant_dim),
                modifiers,
            ))
        }
        ast::VariableBind::Normal => Ok(ty),
    }
}

/// Evaluate a subset of possible constant expressions
fn evaluate_constexpr_int(expr: &ast::Expression) -> Result<u64, ()> {
    Ok(match *expr {
        ast::Expression::Literal(ast::Literal::UntypedInt(i)) => i,
        ast::Expression::Literal(ast::Literal::Int(i)) => i,
        ast::Expression::Literal(ast::Literal::UInt(i)) => i,
        ast::Expression::BinaryOperation(ref op, ref left, ref right) => {
            let lc = evaluate_constexpr_int(left)?;
            let rc = evaluate_constexpr_int(right)?;
            match *op {
                ast::BinOp::Add => lc + rc,
                ast::BinOp::Subtract => lc - rc,
                ast::BinOp::Multiply => lc * rc,
                ast::BinOp::Divide => lc / rc,
                ast::BinOp::Modulus => lc % rc,
                ast::BinOp::LeftShift => lc << rc,
                ast::BinOp::RightShift => lc >> rc,
                _ => return Err(()),
            }
        }
        _ => return Err(()),
    })
}

/// Process an optional variable initialisation expression
pub fn parse_initializer_opt(
    init_opt: &Option<ast::Initializer>,
    tyl: &ir::TypeLayout,
    context: &mut Context,
) -> TyperResult<Option<ir::Initializer>> {
    Ok(match *init_opt {
        Some(ref init) => Some(parse_initializer(init, tyl, context)?),
        None => None,
    })
}

/// Process a variable initialisation expression
fn parse_initializer(
    init: &ast::Initializer,
    tyl: &ir::TypeLayout,
    context: &mut Context,
) -> TyperResult<ir::Initializer> {
    Ok(match *init {
        ast::Initializer::Expression(ref expr) => {
            let ety = ir::Type::from_layout(tyl.clone()).to_rvalue();
            let (expr_ir, expr_ty) = parse_expr(expr, context)?;
            match ImplicitConversion::find(&expr_ty, &ety) {
                Ok(rhs_cast) => ir::Initializer::Expression(rhs_cast.apply(expr_ir)),
                Err(()) => {
                    return Err(TyperError::InitializerExpressionWrongType(
                        expr_ty.0,
                        ety.0,
                        expr.location,
                    ))
                }
            }
        }
        ast::Initializer::Aggregate(ref exprs) => {
            fn build_elements(
                ety: &ExpressionType,
                inits: &[ast::Initializer],
                context: &mut Context,
            ) -> TyperResult<Vec<ir::Initializer>> {
                let mut elements = Vec::with_capacity(inits.len());
                for init in inits {
                    let element = parse_initializer(init, &(ety.0).0, context)?;
                    elements.push(element);
                }
                Ok(elements)
            }
            match *tyl {
                ir::TypeLayout::Scalar(_) => {
                    if exprs.len() as u32 != 1 {
                        return Err(TyperError::InitializerAggregateWrongDimension);
                    }

                    // Reparse as if it was a single expression instead of a 1 element aggregate
                    // Meaning '{ x }' is read as if it were 'x'
                    // Will also reduce '{{ x }}' to 'x'
                    parse_initializer(&exprs[0], tyl, context)?
                }
                ir::TypeLayout::Vector(ref scalar, ref dim) => {
                    if exprs.len() as u32 != *dim {
                        return Err(TyperError::InitializerAggregateWrongDimension);
                    }

                    let ety = ir::Type::from_scalar(*scalar).to_rvalue();
                    let elements = build_elements(&ety, exprs, context)?;

                    ir::Initializer::Aggregate(elements)
                }
                ir::TypeLayout::Array(ref inner, ref dim) => {
                    if exprs.len() as u64 != *dim {
                        return Err(TyperError::InitializerAggregateWrongDimension);
                    }

                    let ety = ir::Type::from_layout(*inner.clone()).to_rvalue();
                    let elements = build_elements(&ety, exprs, context)?;

                    ir::Initializer::Aggregate(elements)
                }
                _ => return Err(TyperError::InitializerAggregateDoesNotMatchType),
            }
        }
    })
}

/// Process a for loop init expression
fn parse_for_init(ast: &ast::InitStatement, context: &mut Context) -> TyperResult<ir::ForInit> {
    match *ast {
        ast::InitStatement::Empty => Ok(ir::ForInit::Empty),
        ast::InitStatement::Expression(ref expr) => {
            let expr_ir = parse_expr(expr, context)?.0;
            Ok(ir::ForInit::Expression(expr_ir))
        }
        ast::InitStatement::Declaration(ref vd) => {
            let vd_ir = parse_vardef(vd, context)?;
            Ok(ir::ForInit::Definitions(vd_ir))
        }
    }
}
