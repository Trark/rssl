use super::errors::*;
use super::scopes::*;
use crate::casting::ImplicitConversion;
use crate::evaluator::evaluate_constexpr;
use ir::ExpressionType;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_text::*;

use super::expressions::parse_expr;
use super::types::{is_illegal_variable_name, parse_type_for_usage, TypePosition};

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
    match ast.kind {
        ast::StatementKind::Block(ref statement_vec) => {
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
    // Parse all attributes - ignoring if it makes sense for the statement kind
    let attributes = parse_statement_attributes(&ast.attributes, context)?;

    match ast.kind {
        ast::StatementKind::Empty => Ok(Vec::new()),
        ast::StatementKind::Expression(ref expr) => {
            let (expr_ir, _) = parse_expr(expr, context)?;
            Ok(Vec::from([ir::Statement {
                kind: ir::StatementKind::Expression(expr_ir),
                location: ast.location,
                attributes,
            }]))
        }
        ast::StatementKind::Var(ref vd) => {
            let vd_ir = parse_vardef(vd, context)?;
            let vars = vd_ir
                .into_iter()
                .map(|vd| ir::Statement {
                    kind: ir::StatementKind::Var(vd),
                    location: ast.location,
                    attributes: attributes.clone(),
                })
                .collect::<Vec<_>>();
            Ok(vars)
        }
        ast::StatementKind::Block(ref statement_vec) => {
            context.push_scope();
            let statements = parse_statement_list(statement_vec, context)?;
            let decls = context.pop_scope_with_locals();
            Ok(Vec::from([ir::Statement {
                kind: ir::StatementKind::Block(ir::ScopeBlock(statements, decls)),
                location: ast.location,
                attributes,
            }]))
        }
        ast::StatementKind::If(ref cond, ref statement) => {
            context.push_scope();
            let cond_ir = parse_expr(cond, context)?.0;
            let scope_block = parse_scopeblock(statement, context)?;
            Ok(Vec::from([ir::Statement {
                kind: ir::StatementKind::If(cond_ir, scope_block),
                location: ast.location,
                attributes,
            }]))
        }
        ast::StatementKind::IfElse(ref cond, ref true_statement, ref false_statement) => {
            context.push_scope();
            let cond_ir = parse_expr(cond, context)?.0;
            let scope_block = parse_scopeblock(true_statement, context)?;
            context.push_scope();
            let else_block = parse_scopeblock(false_statement, context)?;
            Ok(Vec::from([ir::Statement {
                kind: ir::StatementKind::IfElse(cond_ir, scope_block, else_block),
                location: ast.location,
                attributes,
            }]))
        }
        ast::StatementKind::For(ref init, ref cond, ref iter, ref statement) => {
            context.push_scope();
            let init_ir = parse_for_init(init, context)?;
            let cond_ir = parse_expr(cond, context)?.0;
            let iter_ir = parse_expr(iter, context)?.0;
            let scope_block = parse_scopeblock(statement, context)?;
            Ok(Vec::from([ir::Statement {
                kind: ir::StatementKind::For(init_ir, cond_ir, iter_ir, scope_block),
                location: ast.location,
                attributes,
            }]))
        }
        ast::StatementKind::While(ref cond, ref statement) => {
            context.push_scope();
            let cond_ir = parse_expr(cond, context)?.0;
            let scope_block = parse_scopeblock(statement, context)?;
            Ok(Vec::from([ir::Statement {
                kind: ir::StatementKind::While(cond_ir, scope_block),
                location: ast.location,
                attributes,
            }]))
        }
        ast::StatementKind::Break => Ok(Vec::from([ir::Statement {
            kind: ir::StatementKind::Break,
            location: ast.location,
            attributes,
        }])),
        ast::StatementKind::Continue => Ok(Vec::from([ir::Statement {
            kind: ir::StatementKind::Continue,
            location: ast.location,
            attributes,
        }])),
        ast::StatementKind::Discard => Ok(Vec::from([ir::Statement {
            kind: ir::StatementKind::Discard,
            location: ast.location,
            attributes,
        }])),
        ast::StatementKind::Return(Some(ref expr)) => {
            let (expr_ir, expr_ty) = parse_expr(expr, context)?;
            let expected_type = context.get_current_return_type();
            let expected_ety = expected_type.to_rvalue();
            match ImplicitConversion::find(expr_ty, expected_ety, &mut context.module) {
                Ok(rhs_cast) => Ok(Vec::from([ir::Statement {
                    kind: ir::StatementKind::Return(Some(
                        rhs_cast.apply(expr_ir, &mut context.module),
                    )),
                    location: ast.location,
                    attributes,
                }])),
                Err(()) => Err(TyperError::WrongTypeInReturnStatement(
                    expr_ty.0,
                    expected_type,
                    expr.get_location(),
                )),
            }
        }
        ast::StatementKind::Return(None) => {
            let expected_type = context.get_current_return_type();
            let expected_type_layout = context.module.type_registry.get_type_layout(expected_type);
            if expected_type_layout.is_void() {
                Ok(Vec::from([ir::Statement {
                    kind: ir::StatementKind::Return(None),
                    location: ast.location,
                    attributes,
                }]))
            } else {
                Err(TyperError::WrongTypeInReturnStatement(
                    context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayout::void()),
                    expected_type,
                    ast.location,
                ))
            }
        }
    }
}

/// Process a set of attributes for a statement
fn parse_statement_attributes(
    attributes: &[ast::Attribute],
    context: &mut Context,
) -> TyperResult<Vec<ir::StatementAttribute>> {
    let mut output = Vec::new();
    for attribute in attributes {
        output.push(parse_statement_attribute(attribute, context)?);
    }
    Ok(output)
}

/// Process an attribute for a statement
fn parse_statement_attribute(
    attribute: &ast::Attribute,
    context: &mut Context,
) -> TyperResult<ir::StatementAttribute> {
    let lower_name = attribute.name.to_lowercase();

    // First map all the attributes with no arguments
    let trivial_opt = match lower_name.as_str() {
        "branch" => Some(ir::StatementAttribute::Branch),
        "flatten" => Some(ir::StatementAttribute::Flatten),
        "loop" => Some(ir::StatementAttribute::Loop),
        "fastopt" => Some(ir::StatementAttribute::Fastopt),
        "allow_uav_condition" => Some(ir::StatementAttribute::AllowUavCondition),
        _ => None,
    };

    if let Some(trivial) = trivial_opt {
        if attribute.arguments.is_empty() {
            Ok(trivial)
        } else {
            Err(TyperError::StatementAttributeUnexpectedArgumentCount(
                attribute.name.node.clone(),
                attribute.name.location,
            ))
        }
    } else {
        match lower_name.as_str() {
            "unroll" => {
                if attribute.arguments.is_empty() {
                    Ok(ir::StatementAttribute::Unroll(None))
                } else if attribute.arguments.len() == 1 {
                    let expr = parse_expr(&attribute.arguments[0], context)?.0;
                    let value = match evaluate_constexpr(&expr, &mut context.module) {
                        Ok(constant) => constant,
                        Err(_) => {
                            return Err(TyperError::AttributeUnrollArgumentMustBeIntegerConstant(
                                attribute.name.location,
                            ))
                        }
                    };
                    let value = match value.to_uint64() {
                        Some(value) => value,
                        None => {
                            return Err(TyperError::AttributeUnrollArgumentMustBeIntegerConstant(
                                attribute.name.location,
                            ))
                        }
                    };
                    Ok(ir::StatementAttribute::Unroll(Some(value)))
                } else {
                    Err(TyperError::StatementAttributeUnexpectedArgumentCount(
                        attribute.name.node.clone(),
                        attribute.name.location,
                    ))
                }
            }
            _ => Err(TyperError::StatementAttributeUnknown(
                attribute.name.node.clone(),
                attribute.name.location,
            )),
        }
    }
}

/// Process a variable definition
fn parse_vardef(ast: &ast::VarDef, context: &mut Context) -> TyperResult<Vec<ir::VarDef>> {
    let (base_id, storage_class) = parse_localtype(&ast.local_type, context)?;

    let base_type_layout = context
        .module
        .type_registry
        .get_type_layout(base_id)
        .clone();

    // Build multiple output VarDefs for each variable inside the source VarDef
    let mut vardefs = vec![];
    for local_variable in &ast.defs {
        // Deny restricted non-keyword names
        if is_illegal_variable_name(&local_variable.name) {
            return Err(TyperError::IllegalVariableName(
                local_variable.name.get_location(),
            ));
        }

        // Build type from ast type + bind
        let type_layout = apply_variable_bind(
            base_type_layout.clone(),
            &local_variable.bind,
            &local_variable.init,
            context,
        )?;

        // Register the type
        let type_id = context.module.type_registry.register_type(type_layout);

        // Parse the initializer
        let var_init = parse_initializer_opt(
            &local_variable.init,
            type_id,
            local_variable.name.get_location(),
            context,
        )?;

        // Register the variable
        let var_id = context.insert_variable(local_variable.name.clone(), type_id)?;

        // Add the variables creation node
        vardefs.push(ir::VarDef {
            id: var_id,
            type_id,
            storage_class,
            init: var_init,
        });
    }

    Ok(vardefs)
}

/// Convert a type for a local variable
fn parse_localtype(
    local_type: &ast::Type,
    context: &mut Context,
) -> TyperResult<(ir::TypeId, ir::LocalStorage)> {
    let ty = parse_type_for_usage(local_type, TypePosition::Local, context)?;

    // Calculate the local storage type
    let mut local_storage = None;
    for modifier in &local_type.modifiers.modifiers {
        let next_ls = match &modifier.node {
            ast::TypeModifier::Static => ir::LocalStorage::Static,
            ast::TypeModifier::Extern | ast::TypeModifier::GroupShared => {
                return Err(TyperError::ModifierNotSupported(
                    modifier.node,
                    modifier.get_location(),
                    TypePosition::Local,
                ))
            }
            _ => continue,
        };

        if let Some((current_ls, _)) = local_storage {
            if current_ls == next_ls {
                // TODO: Warn for duplicate modifier
            } else {
                // These are two local storage variants - but only one valid modifier
                // Every iteration of this loop must return the same modifier to set so we can never have a conflict
                unreachable!("multiple different local storage modifiers detected")
            }
        } else {
            local_storage = Some((next_ls, modifier.node));
        }
    }
    let local_storage = local_storage
        .map(|(ls, _)| ls)
        .unwrap_or(ir::LocalStorage::Local);

    let ty_unmodified = context.module.type_registry.remove_modifier(ty);
    let ty_layout_unmodified = context.module.type_registry.get_type_layout(ty_unmodified);
    if ty_layout_unmodified.is_void() {
        return Err(TyperError::VariableHasIncompleteType(
            ty,
            local_type.location,
        ));
    }

    Ok((ty, local_storage))
}

/// Apply part of type applied to variable name onto the type itself
pub fn apply_variable_bind(
    mut ty: ir::TypeLayout,
    bind: &ast::VariableBind,
    init: &Option<ast::Initializer>,
    context: &mut Context,
) -> TyperResult<ir::TypeLayout> {
    for dim in &bind.0 {
        let (layout, modifiers) = ty.extract_modifier();

        let constant_dim = match *dim {
            Some(ref dim_expr) => {
                let expr_ir = parse_expr(dim_expr, context)?.0;
                let value = match evaluate_constexpr(&expr_ir, &mut context.module) {
                    Ok(val) => val.to_uint64(),
                    Err(()) => None,
                };
                match value {
                    Some(0) => {
                        return Err(TyperError::ArrayDimensionsMustBeNonZero(
                            dim_expr.get_location(),
                        ))
                    }
                    Some(val) => val,
                    None => {
                        let p = (**dim_expr).clone();
                        return Err(TyperError::ArrayDimensionsMustBeConstantExpression(
                            p,
                            dim_expr.get_location(),
                        ));
                    }
                }
            }
            None => match *init {
                Some(ast::Initializer::Aggregate(ref exprs)) if exprs.is_empty() => {
                    return Err(TyperError::ArrayDimensionsMustBeNonZero(
                        SourceLocation::UNKNOWN,
                    ))
                }
                Some(ast::Initializer::Aggregate(ref exprs)) => exprs.len() as u64,
                _ => {
                    return Err(TyperError::ArrayDimensionNotSpecified(
                        SourceLocation::UNKNOWN,
                    ))
                }
            },
        };

        ty = ir::TypeLayout::Array(Box::new(layout), constant_dim).combine_modifier(modifiers);
    }

    Ok(ty)
}

/// Process an optional variable initialisation expression
pub fn parse_initializer_opt(
    init_opt: &Option<ast::Initializer>,
    ty: ir::TypeId,
    variable_location: SourceLocation,
    context: &mut Context,
) -> TyperResult<Option<ir::Initializer>> {
    Ok(match *init_opt {
        Some(ref init) => Some(parse_initializer(init, ty, variable_location, context)?),
        None => None,
    })
}

/// Process a variable initialisation expression
fn parse_initializer(
    init: &ast::Initializer,
    ty: ir::TypeId,
    variable_location: SourceLocation,
    context: &mut Context,
) -> TyperResult<ir::Initializer> {
    Ok(match *init {
        ast::Initializer::Expression(ref expr) => {
            let ety = context.module.type_registry.remove_modifier(ty).to_rvalue();
            let (expr_ir, expr_ty) = parse_expr(expr, context)?;
            match ImplicitConversion::find(expr_ty, ety, &mut context.module) {
                Ok(rhs_cast) => {
                    ir::Initializer::Expression(rhs_cast.apply(expr_ir, &mut context.module))
                }
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
                variable_location: SourceLocation,
                context: &mut Context,
            ) -> TyperResult<Vec<ir::Initializer>> {
                let mut elements = Vec::with_capacity(inits.len());
                for init in inits {
                    let element = parse_initializer(init, ety.0, variable_location, context)?;
                    elements.push(element);
                }
                Ok(elements)
            }
            let ty = context.module.type_registry.remove_modifier(ty);
            let tyl = context.module.type_registry.get_type_layer(ty);
            match tyl {
                ir::TypeLayer::Scalar(_) => {
                    if exprs.len() as u32 != 1 {
                        return Err(TyperError::InitializerAggregateWrongDimension(
                            variable_location,
                        ));
                    }

                    // Reparse as if it was a single expression instead of a 1 element aggregate
                    // Meaning '{ x }' is read as if it were 'x'
                    // Will also reduce '{{ x }}' to 'x'
                    parse_initializer(&exprs[0], ty, variable_location, context)?
                }
                ir::TypeLayer::Vector(ref scalar, ref dim) => {
                    if exprs.len() as u32 != *dim {
                        return Err(TyperError::InitializerAggregateWrongDimension(
                            variable_location,
                        ));
                    }

                    let ty = context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayout::from_scalar(*scalar));
                    let ety = ty.to_rvalue();
                    let elements = build_elements(&ety, exprs, variable_location, context)?;

                    ir::Initializer::Aggregate(elements)
                }
                ir::TypeLayer::Array(ref inner, ref dim) => {
                    if exprs.len() as u64 != *dim {
                        return Err(TyperError::InitializerAggregateWrongDimension(
                            variable_location,
                        ));
                    }

                    let ety = inner.to_rvalue();
                    let elements = build_elements(&ety, exprs, variable_location, context)?;

                    ir::Initializer::Aggregate(elements)
                }
                ir::TypeLayer::Struct(id) => {
                    let sd = &context.module.struct_registry[id.0 as usize];
                    if exprs.len() != sd.members.len() {
                        return Err(TyperError::InitializerAggregateWrongDimension(
                            variable_location,
                        ));
                    }

                    let mut elements = Vec::with_capacity(sd.members.len());
                    for (member, member_init) in sd.members.clone().iter().zip(exprs) {
                        let element = parse_initializer(
                            member_init,
                            member.type_id,
                            variable_location,
                            context,
                        )?;
                        elements.push(element);
                    }
                    ir::Initializer::Aggregate(elements)
                }
                _ => {
                    return Err(TyperError::InitializerAggregateDoesNotMatchType(
                        ty,
                        variable_location,
                    ))
                }
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
