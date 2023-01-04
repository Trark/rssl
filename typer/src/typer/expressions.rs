use super::errors::*;
use super::scopes::*;
use super::types::{
    apply_template_type_substitution, parse_expression_or_type, parse_type, parse_typelayout,
};
use crate::casting::{ConversionPriority, ImplicitConversion};
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::ExpressionType;
use rssl_text::{Locate, Located, SourceLocation};

/// Result of a variable query
pub enum VariableExpression {
    Local(ir::VariableRef, ir::TypeId),
    Member(String, ir::TypeId),
    Global(ir::GlobalId, ir::TypeId),
    ConstantBufferMember(ir::ConstantBufferId, String, ir::TypeId),
    EnumValueUntyped(ir::Constant, ir::TypeId),
    EnumValue(ir::EnumValueId, ir::TypeId),
    Function(UnresolvedFunction),
    Method(UnresolvedFunction),
    Type(ir::TypeId),
}

/// Set of overloaded functions
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct UnresolvedFunction {
    pub overloads: Vec<ir::FunctionId>,
}

#[derive(PartialEq, Debug, Clone)]
struct UnresolvedMethod {
    object_type: ir::TypeId,
    overloads: Vec<ir::FunctionId>,
    object_value: ir::Expression,
}

#[derive(PartialEq, Debug, Clone)]
enum TypedExpression {
    // Expression + Type
    Value(ir::Expression, ExpressionType),
    // Set of function overloads
    Function(UnresolvedFunction),
    // Set of method overloads for an object local method call with no object information
    MethodInternal(UnresolvedFunction),
    // Set of method overloads + object
    Method(UnresolvedMethod),
}

impl ToErrorType for TypedExpression {
    fn to_error_type(&self) -> ErrorType {
        match *self {
            TypedExpression::Value(_, ref ety) => ety.to_error_type(),
            TypedExpression::Function(UnresolvedFunction { ref overloads }) => {
                ErrorType::Function(overloads.clone())
            }
            TypedExpression::MethodInternal(UnresolvedFunction { ref overloads }) => {
                ErrorType::Function(overloads.clone())
            }
            TypedExpression::Method(UnresolvedMethod {
                ref object_type,
                ref overloads,
                ..
            }) => ErrorType::Method(*object_type, overloads.clone()),
        }
    }
}

fn parse_identifier(
    id: &ast::ScopedIdentifier,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    Ok(match context.find_identifier(id)? {
        VariableExpression::Local(var, ty) => {
            TypedExpression::Value(ir::Expression::Variable(var), ty.to_lvalue())
        }
        VariableExpression::Member(name, ty) => {
            TypedExpression::Value(ir::Expression::MemberVariable(name), ty.to_lvalue())
        }
        VariableExpression::Global(id, ty) => {
            TypedExpression::Value(ir::Expression::Global(id), ty.to_lvalue())
        }
        VariableExpression::ConstantBufferMember(id, name, ty) => {
            TypedExpression::Value(ir::Expression::ConstantVariable(id, name), ty.to_lvalue())
        }
        VariableExpression::EnumValueUntyped(c, ty) => {
            TypedExpression::Value(ir::Expression::Literal(c), ty.to_rvalue())
        }
        VariableExpression::EnumValue(id, ty) => {
            TypedExpression::Value(ir::Expression::EnumValue(id), ty.to_rvalue())
        }
        VariableExpression::Function(func) => TypedExpression::Function(func),
        VariableExpression::Method(func) => TypedExpression::MethodInternal(func),
        VariableExpression::Type(ty) => {
            return Err(TyperError::ExpectedExpressionReceivedType(id.clone(), ty))
        }
    })
}

/// Attempt to match a function against a set of argument and return the casts required to invoke it
fn find_overload_casts(
    mut id: ir::FunctionId,
    template_args: &[Located<ir::TypeOrConstant>],
    param_types: &[ExpressionType],
    context: &mut Context,
) -> Result<(ir::FunctionId, Vec<ImplicitConversion>), ()> {
    let mut overload_casts = Vec::with_capacity(param_types.len());

    // Resolve template arguments
    // Require user to specify all arguments
    let signature = context.module.function_registry.get_function_signature(id);
    if signature.template_params.0 != 0 {
        if signature.template_params.0 as usize != template_args.len() {
            // Wrong number of template arguments provided
            return Err(());
        }

        if !template_args.is_empty() {
            let new_id = if context
                .module
                .function_registry
                .get_intrinsic_data(id)
                .is_some()
            {
                context.build_intrinsic_template(id, template_args)
            } else {
                context.build_function_template_signature(id, template_args)
            };

            id = new_id;
        }
    } else if !template_args.is_empty() {
        // Template args given to non template function
        return Err(());
    }

    let signature = context
        .module
        .function_registry
        .get_function_signature(id)
        .clone();
    for (required_type, source_type) in signature.param_types.iter().zip(param_types.iter()) {
        if required_type.interpolation_modifier.is_some() {
            return Err(());
        };
        let ety = ExpressionType(required_type.type_id, required_type.input_modifier.into());
        if let Ok(cast) = ImplicitConversion::find(*source_type, ety, &mut context.module) {
            overload_casts.push(cast)
        } else {
            return Err(());
        }
    }
    Ok((id, overload_casts))
}

/// Attempt to find the best function from a set of overloads that can satisfy a set of arguments
fn find_function_type(
    overloads: &Vec<ir::FunctionId>,
    template_args: &[Located<ir::TypeOrConstant>],
    param_types: &[ExpressionType],
    call_location: SourceLocation,
    context: &mut Context,
) -> TyperResult<(ir::FunctionId, Vec<ImplicitConversion>)> {
    use crate::casting::VectorRank;
    let mut casts = Vec::with_capacity(overloads.len());
    for overload in overloads {
        let signature = context
            .module
            .function_registry
            .get_function_signature(*overload);
        if param_types.len() == signature.param_types.len() {
            if let Ok((new_id, param_casts)) =
                find_overload_casts(*overload, template_args, param_types, context)
            {
                casts.push((new_id, param_casts))
            }
        }
    }

    // Cull everything that isn't equal best at matching numeric type
    let mut winning_numeric_casts = Vec::with_capacity(1);
    for &(ref candidate, ref candidate_casts) in &casts {
        let mut winning = true;
        for &(ref against, ref against_casts) in &casts {
            if candidate == against {
                continue;
            }
            assert_eq!(candidate_casts.len(), against_casts.len());
            let mut not_worse_than = true;
            for (candidate_cast, against_cast) in candidate_casts.iter().zip(against_casts) {
                let candidate_rank = candidate_cast.get_rank().get_numeric_rank().clone();
                let against_rank = against_cast.get_rank().get_numeric_rank().clone();
                match candidate_rank.compare(&against_rank) {
                    ConversionPriority::Better => {}
                    ConversionPriority::Equal => {}
                    ConversionPriority::Worse => not_worse_than = false,
                };
            }
            if !not_worse_than {
                winning = false;
                break;
            }
        }
        if winning {
            winning_numeric_casts.push((*candidate, candidate_casts.clone()));
        }
    }

    if !winning_numeric_casts.is_empty() {
        fn count_by_rank(casts: &[ImplicitConversion], rank: &VectorRank) -> usize {
            casts
                .iter()
                .filter(|cast| cast.get_rank().get_vector_rank() == rank)
                .count()
        }

        let map_order = |(overload, casts): (_, Vec<ImplicitConversion>)| {
            let order = VectorRank::worst_to_best()
                .iter()
                .map(|rank| count_by_rank(&casts, rank))
                .collect::<Vec<_>>();
            (overload, casts, order)
        };

        let casts = winning_numeric_casts
            .into_iter()
            .map(map_order)
            .collect::<Vec<_>>();

        let mut best_order = casts[0].2.clone();
        for &(_, _, ref order) in &casts {
            if *order < best_order {
                best_order = order.clone();
            }
        }

        let casts = casts
            .into_iter()
            .filter(|&(_, _, ref order)| *order == best_order)
            .collect::<Vec<_>>();

        if casts.len() == 1 {
            let (candidate, casts, _) = casts[0].clone();
            return Ok((candidate, casts));
        }

        if casts.len() > 1 {
            let ambiguous_overloads = casts.iter().map(|c| c.0).collect::<Vec<_>>();
            return Err(TyperError::FunctionArgumentTypeMismatch(
                ambiguous_overloads,
                param_types.to_vec(),
                call_location,
                true,
            ));
        }
    }

    Err(TyperError::FunctionArgumentTypeMismatch(
        overloads.clone(),
        param_types.to_vec(),
        call_location,
        false,
    ))
}

fn apply_casts(
    casts: Vec<ImplicitConversion>,
    values: Vec<ir::Expression>,
    context: &mut Context,
) -> Vec<ir::Expression> {
    assert_eq!(casts.len(), values.len());
    values
        .into_iter()
        .enumerate()
        .map(|(index, value)| casts[index].apply(value, &mut context.module))
        .collect::<Vec<_>>()
}

fn write_function(
    unresolved: UnresolvedFunction,
    template_args: &[Located<ir::TypeOrConstant>],
    param_types: &[ExpressionType],
    param_values: Vec<ir::Expression>,
    call_location: SourceLocation,
    call_type: ir::CallType,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Find the matching function overload
    let (id, casts) = find_function_type(
        &unresolved.overloads,
        template_args,
        param_types,
        call_location,
        context,
    )?;
    // Apply implicit casts
    let param_values = apply_casts(casts, param_values, context);

    let return_type = context
        .module
        .function_registry
        .get_function_signature(id)
        .return_type
        .return_type
        .to_rvalue();

    if context
        .module
        .function_registry
        .get_intrinsic_data(id)
        .is_some()
    {
        Ok(TypedExpression::Value(
            create_intrinsic(id, ir::CallType::FreeFunction, &param_values),
            return_type,
        ))
    } else {
        let id = if !template_args.is_empty() {
            // Now we have to make the actual instance of that template
            context.build_function_template_body(id)?
        } else {
            id
        };
        // TODO: Call will not need template args if we can encode it all in id
        Ok(TypedExpression::Value(
            ir::Expression::Call(id, call_type, param_values),
            return_type,
        ))
    }
}

fn write_method(
    unresolved: UnresolvedMethod,
    template_args: &[Located<ir::TypeOrConstant>],
    param_types: &[ExpressionType],
    param_values: Vec<ir::Expression>,
    call_location: SourceLocation,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Find the matching method overload
    let (id, casts) = find_function_type(
        &unresolved.overloads,
        template_args,
        param_types,
        call_location,
        context,
    )?;
    // Apply implicit casts
    let mut param_values = apply_casts(casts, param_values, context);
    // Add struct as implied first argument
    param_values.insert(0, unresolved.object_value);

    let return_type = context
        .module
        .function_registry
        .get_function_signature(id)
        .return_type
        .return_type
        .to_rvalue();

    if context
        .module
        .function_registry
        .get_intrinsic_data(id)
        .is_some()
    {
        Ok(TypedExpression::Value(
            create_intrinsic(id, ir::CallType::MethodExternal, &param_values),
            return_type,
        ))
    } else {
        let id = if !template_args.is_empty() {
            // Now we have to make the actual instance of that template
            context.build_function_template_body(id)?
        } else {
            id
        };
        // TODO: Call will not need template args if we can encode it all in id
        Ok(TypedExpression::Value(
            ir::Expression::Call(id, ir::CallType::MethodExternal, param_values),
            return_type,
        ))
    }
}

/// Make an intrinsic expression node
fn create_intrinsic(
    id: ir::FunctionId,
    call_type: ir::CallType,
    param_values: &[ir::Expression],
) -> ir::Expression {
    let mut exprs = Vec::with_capacity(param_values.len());
    for param_value in param_values {
        exprs.push(param_value.clone());
    }
    ir::Expression::Call(id, call_type, exprs)
}

fn parse_literal(ast: &ast::Literal, context: &mut Context) -> TyperResult<TypedExpression> {
    let constant = match ast {
        ast::Literal::Bool(b) => ir::Constant::Bool(*b),
        ast::Literal::UntypedInt(i) => ir::Constant::UntypedInt(*i as i128),
        ast::Literal::Int(i) => ir::Constant::Int(*i as i32),
        ast::Literal::UInt(i) => ir::Constant::UInt(*i as u32),
        ast::Literal::Long(i) => ir::Constant::Long(*i),
        ast::Literal::Half(f) => ir::Constant::Half(*f),
        ast::Literal::Float(f) => ir::Constant::Float(*f),
        ast::Literal::Double(f) => ir::Constant::Double(*f),
        ast::Literal::String(_) => {
            return Err(TyperError::StringNotSupported(SourceLocation::UNKNOWN))
        }
    };

    let ty = get_constant_type(&constant, context);

    Ok(TypedExpression::Value(
        ir::Expression::Literal(constant),
        ty,
    ))
}

fn parse_expr_unaryop(
    op: &ast::UnaryOp,
    expr: &Located<ast::Expression>,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Use argument expression as the error location
    let base_location = expr.get_location();

    match parse_expr_internal(expr, context)? {
        TypedExpression::Value(expr_ir, expr_ty) => {
            fn enforce_increment_type(
                ety: ExpressionType,
                op: &ast::UnaryOp,
                base_location: SourceLocation,
                context: &mut Context,
            ) -> TyperResult<()> {
                let ExpressionType(ty, vt) = ety;
                if vt == ir::ValueType::Rvalue {
                    return Err(TyperError::UnaryOperationWrongTypes(
                        op.clone(),
                        ErrorType::Unknown,
                        base_location,
                    ));
                }
                let (tyl, modifier) = context.module.type_registry.extract_modifier(ty);
                if modifier.is_const {
                    return Err(TyperError::UnaryOperationWrongTypes(
                        op.clone(),
                        ErrorType::Unknown,
                        base_location,
                    ));
                }
                match context.module.type_registry.extract_scalar(tyl) {
                    Some(ir::ScalarType::Bool) => Err(TyperError::UnaryOperationWrongTypes(
                        op.clone(),
                        ErrorType::Unknown,
                        base_location,
                    )),
                    _ => Ok(()),
                }
            }
            let (intrinsic, eir, ety) = match *op {
                ast::UnaryOp::PrefixIncrement => {
                    enforce_increment_type(expr_ty, op, base_location, context)?;
                    (ir::IntrinsicOp::PrefixIncrement, expr_ir, expr_ty)
                }
                ast::UnaryOp::PrefixDecrement => {
                    enforce_increment_type(expr_ty, op, base_location, context)?;
                    (ir::IntrinsicOp::PrefixDecrement, expr_ir, expr_ty)
                }
                ast::UnaryOp::PostfixIncrement => {
                    enforce_increment_type(expr_ty, op, base_location, context)?;
                    (
                        ir::IntrinsicOp::PostfixIncrement,
                        expr_ir,
                        expr_ty.0.to_rvalue(),
                    )
                }
                ast::UnaryOp::PostfixDecrement => {
                    enforce_increment_type(expr_ty, op, base_location, context)?;
                    (
                        ir::IntrinsicOp::PostfixDecrement,
                        expr_ir,
                        expr_ty.0.to_rvalue(),
                    )
                }
                ast::UnaryOp::Plus => (ir::IntrinsicOp::Plus, expr_ir, expr_ty.0.to_rvalue()),
                ast::UnaryOp::Minus => (ir::IntrinsicOp::Minus, expr_ir, expr_ty.0.to_rvalue()),
                ast::UnaryOp::LogicalNot => {
                    let input_ty_id = context.module.type_registry.remove_modifier(expr_ty.0);
                    let tyl =
                        match context.module.type_registry.get_type_layer(input_ty_id) {
                            ir::TypeLayer::Scalar(_) => ir::TypeLayer::Scalar(ir::ScalarType::Bool),
                            ir::TypeLayer::Vector(_, x) => ir::TypeLayer::Vector(
                                context.module.type_registry.register_type_layer(
                                    ir::TypeLayer::Scalar(ir::ScalarType::Bool),
                                ),
                                x,
                            ),
                            ir::TypeLayer::Matrix(_, x, y) => ir::TypeLayer::Matrix(
                                context.module.type_registry.register_type_layer(
                                    ir::TypeLayer::Scalar(ir::ScalarType::Bool),
                                ),
                                x,
                                y,
                            ),
                            _ => {
                                return Err(TyperError::UnaryOperationWrongTypes(
                                    op.clone(),
                                    ErrorType::Unknown,
                                    base_location,
                                ))
                            }
                        };
                    let ty = context.module.type_registry.register_type_layer(tyl);
                    let ety = ty.to_rvalue();
                    (ir::IntrinsicOp::LogicalNot, expr_ir, ety)
                }
                ast::UnaryOp::BitwiseNot => {
                    let input_ty_id = context.module.type_registry.remove_modifier(expr_ty.0);
                    match context.module.type_registry.get_type_layer(input_ty_id) {
                        ir::TypeLayer::Scalar(ir::ScalarType::Int)
                        | ir::TypeLayer::Scalar(ir::ScalarType::UInt) => {
                            (ir::IntrinsicOp::BitwiseNot, expr_ir, expr_ty.0.to_rvalue())
                        }
                        _ => {
                            return Err(TyperError::UnaryOperationWrongTypes(
                                op.clone(),
                                ErrorType::Unknown,
                                base_location,
                            ))
                        }
                    }
                }
            };
            Ok(TypedExpression::Value(
                ir::Expression::IntrinsicOp(intrinsic, Vec::new(), Vec::from([eir])),
                ety,
            ))
        }
        _ => Err(TyperError::UnaryOperationWrongTypes(
            op.clone(),
            ErrorType::Unknown,
            base_location,
        )),
    }
}

fn most_sig_scalar(left: ir::ScalarType, right: ir::ScalarType) -> ir::ScalarType {
    use rssl_ir::ScalarType;

    // The limited number of hlsl types means these happen to always have one
    // type being the common type
    fn get_order(ty: &ScalarType) -> Option<u32> {
        match *ty {
            ScalarType::Bool => Some(0),
            ScalarType::Int => Some(1),
            ScalarType::UInt => Some(2),
            ScalarType::Half => Some(3),
            ScalarType::Float => Some(4),
            ScalarType::Double => Some(5),
            _ => None,
        }
    }

    let left = match left {
        ScalarType::UntypedInt => ScalarType::Int,
        scalar => scalar,
    };
    let right = match right {
        ScalarType::UntypedInt => ScalarType::Int,
        scalar => scalar,
    };

    let left_order = match get_order(&left) {
        Some(order) => order,
        None => panic!("unknown scalar type"),
    };
    let right_order = match get_order(&right) {
        Some(order) => order,
        None => panic!("unknown scalar type"),
    };

    if left_order > right_order {
        left
    } else {
        right
    }
}

fn resolve_arithmetic_types(
    binop: &ast::BinOp,
    left: ExpressionType,
    right: ExpressionType,
    source_location: SourceLocation,
    context: &mut Context,
) -> TyperResult<(ImplicitConversion, ImplicitConversion, ir::IntrinsicOp)> {
    use rssl_ir::ScalarType;
    use rssl_ir::TypeLayout;

    fn common_real_type(left: ScalarType, right: ScalarType) -> Result<ir::ScalarType, ()> {
        Ok(most_sig_scalar(left, right))
    }

    // Calculate the output type from the input type and operation
    fn output_type(left: &TypeLayout, right: &TypeLayout, op: &ast::BinOp) -> ir::IntrinsicOp {
        // Assert input validity
        {
            let ls = left
                .to_scalar()
                .expect("non-numeric type in binary operation (lhs)");
            let rs = right
                .to_scalar()
                .expect("non-numeric type in binary operation (rhs)");
            match *op {
                ast::BinOp::LeftShift
                | ast::BinOp::RightShift
                | ast::BinOp::BitwiseAnd
                | ast::BinOp::BitwiseOr
                | ast::BinOp::BitwiseXor => {
                    assert!(
                        ls == ScalarType::Int || ls == ScalarType::UInt,
                        "non-integer source in bitwise op (lhs)"
                    );
                    assert!(
                        rs == ScalarType::Int || rs == ScalarType::UInt,
                        "non-integer source in bitwise op (rhs)"
                    );
                }
                ast::BinOp::BooleanAnd | ast::BinOp::BooleanOr => {
                    assert!(
                        ls == ScalarType::Bool,
                        "non-boolean source in boolean op (lhs)"
                    );
                    assert!(
                        rs == ScalarType::Bool,
                        "non-boolean source in boolean op (rhs)"
                    );
                }
                _ => {}
            }
        }

        match *op {
            ast::BinOp::Add => ir::IntrinsicOp::Add,
            ast::BinOp::Subtract => ir::IntrinsicOp::Subtract,
            ast::BinOp::Multiply => ir::IntrinsicOp::Multiply,
            ast::BinOp::Divide => ir::IntrinsicOp::Divide,
            ast::BinOp::Modulus => ir::IntrinsicOp::Modulus,
            ast::BinOp::LeftShift => ir::IntrinsicOp::LeftShift,
            ast::BinOp::RightShift => ir::IntrinsicOp::RightShift,
            ast::BinOp::BitwiseAnd => ir::IntrinsicOp::BitwiseAnd,
            ast::BinOp::BitwiseOr => ir::IntrinsicOp::BitwiseOr,
            ast::BinOp::BitwiseXor => ir::IntrinsicOp::BitwiseXor,
            ast::BinOp::LessThan => ir::IntrinsicOp::LessThan,
            ast::BinOp::LessEqual => ir::IntrinsicOp::LessEqual,
            ast::BinOp::GreaterThan => ir::IntrinsicOp::GreaterThan,
            ast::BinOp::GreaterEqual => ir::IntrinsicOp::GreaterEqual,
            ast::BinOp::Equality => ir::IntrinsicOp::Equality,
            ast::BinOp::Inequality => ir::IntrinsicOp::Inequality,
            ast::BinOp::BooleanAnd => ir::IntrinsicOp::BooleanAnd,
            ast::BinOp::BooleanOr => ir::IntrinsicOp::BooleanOr,
            _ => panic!("unexpected binop in resolve_arithmetic_types"),
        }
    }

    fn do_noerror(
        op: &ast::BinOp,
        left: ExpressionType,
        right: ExpressionType,
        context: &mut Context,
    ) -> Result<(ImplicitConversion, ImplicitConversion, ir::IntrinsicOp), ()> {
        let left_base_id = context.module.type_registry.remove_modifier(left.0);
        let right_base_id = context.module.type_registry.remove_modifier(right.0);
        let left_base_tyl = context.module.type_registry.get_type_layer(left_base_id);
        let right_base_tyl = context.module.type_registry.get_type_layer(right_base_id);
        let ls = match context.module.type_registry.extract_scalar(left_base_id) {
            Some(s) => s,
            None => return Err(()),
        };
        let rs = match context.module.type_registry.extract_scalar(right_base_id) {
            Some(s) => s,
            None => return Err(()),
        };
        let (ltl, rtl) = match (left_base_tyl, right_base_tyl) {
            (ir::TypeLayer::Scalar(_), ir::TypeLayer::Scalar(_)) => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_scalar(common_scalar);
                let common_right = common_left.clone();
                (common_left, common_right)
            }
            (ir::TypeLayer::Scalar(_), ir::TypeLayer::Vector(_, x2)) => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_scalar(common_scalar);
                let common_right = ir::TypeLayout::from_vector(common_scalar, x2);
                (common_left, common_right)
            }
            (ir::TypeLayer::Vector(_, x1), ir::TypeLayer::Scalar(_)) => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_vector(common_scalar, x1);
                let common_right = ir::TypeLayout::from_scalar(common_scalar);
                (common_left, common_right)
            }
            (ir::TypeLayer::Vector(_, x1), ir::TypeLayer::Vector(_, x2))
                if x1 == x2 || x1 == 1 || x2 == 1 =>
            {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_vector(common_scalar, x1);
                let common_right = ir::TypeLayout::from_vector(common_scalar, x2);
                (common_left, common_right)
            }
            (ir::TypeLayer::Matrix(_, x1, y1), ir::TypeLayer::Matrix(_, x2, y2))
                if x1 == x2 && y1 == y2 =>
            {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_matrix(common_scalar, x2, y2);
                let common_right = common_left.clone();
                (common_left, common_right)
            }
            _ => return Err(()),
        };
        let output_type = output_type(&ltl, &rtl, op);
        let left_out_id = context.module.type_registry.register_type(ltl);
        let right_out_id = context.module.type_registry.register_type(rtl);
        let elt = left_out_id.to_rvalue();
        let lc = ImplicitConversion::find(left, elt, &mut context.module)?;
        let ert = right_out_id.to_rvalue();
        let rc = ImplicitConversion::find(right, ert, &mut context.module)?;
        Ok((lc, rc, output_type))
    }

    match do_noerror(binop, left, right, context) {
        Ok(res) => Ok(res),
        Err(_) => Err(TyperError::BinaryOperationWrongTypes(
            binop.clone(),
            left.to_error_type(),
            right.to_error_type(),
            source_location,
        )),
    }
}

fn parse_expr_binop(
    op: &ast::BinOp,
    lhs: &Located<ast::Expression>,
    rhs: &Located<ast::Expression>,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Use left side location for general error position
    let base_location = lhs.get_location();

    let lhs_texp = parse_expr_internal(lhs, context)?;
    let rhs_texp = parse_expr_internal(rhs, context)?;
    let lhs_pt = lhs_texp.to_error_type();
    let rhs_pt = rhs_texp.to_error_type();
    let err_bad_type = Err(TyperError::BinaryOperationWrongTypes(
        op.clone(),
        lhs_pt,
        rhs_pt,
        base_location,
    ));
    let (lhs_ir, lhs_type) = match lhs_texp {
        TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
        _ => return err_bad_type,
    };
    let (rhs_ir, rhs_type) = match rhs_texp {
        TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
        _ => return err_bad_type,
    };
    match *op {
        ast::BinOp::Add
        | ast::BinOp::Subtract
        | ast::BinOp::Multiply
        | ast::BinOp::Divide
        | ast::BinOp::Modulus
        | ast::BinOp::LessThan
        | ast::BinOp::LessEqual
        | ast::BinOp::GreaterThan
        | ast::BinOp::GreaterEqual
        | ast::BinOp::Equality
        | ast::BinOp::Inequality
        | ast::BinOp::LeftShift
        | ast::BinOp::RightShift => {
            if *op == ast::BinOp::LeftShift || *op == ast::BinOp::RightShift {
                fn is_integer(ety: ExpressionType, context: &Context) -> bool {
                    let base_id = context.module.type_registry.remove_modifier(ety.0);
                    let sty = match context.module.type_registry.extract_scalar(base_id) {
                        Some(s) => s,
                        None => return false,
                    };
                    sty == ir::ScalarType::Int
                        || sty == ir::ScalarType::UInt
                        || sty == ir::ScalarType::UntypedInt
                }
                if !is_integer(lhs_type, context) || !is_integer(rhs_type, context) {
                    return err_bad_type;
                }
            }
            let types = resolve_arithmetic_types(op, lhs_type, rhs_type, base_location, context)?;
            let (lhs_cast, rhs_cast, output_intrinsic) = types;
            let lhs_final = lhs_cast.apply(lhs_ir, &mut context.module);
            let rhs_final = rhs_cast.apply(rhs_ir, &mut context.module);
            let lhs_target = lhs_cast.get_target_type(&mut context.module);
            let rhs_target = rhs_cast.get_target_type(&mut context.module);
            let output_type =
                output_intrinsic.get_return_type(&[lhs_target, rhs_target], &mut context.module);
            let node = ir::Expression::IntrinsicOp(
                output_intrinsic,
                Vec::new(),
                Vec::from([lhs_final, rhs_final]),
            );
            Ok(TypedExpression::Value(node, output_type))
        }
        ast::BinOp::BitwiseAnd
        | ast::BinOp::BitwiseOr
        | ast::BinOp::BitwiseXor
        | ast::BinOp::BooleanAnd
        | ast::BinOp::BooleanOr => {
            let left_base = context.module.type_registry.remove_modifier(lhs_type.0);
            let right_base = context.module.type_registry.remove_modifier(rhs_type.0);
            let lhs_tyl = context.module.type_registry.get_type_layer(left_base);
            let rhs_tyl = context.module.type_registry.get_type_layer(right_base);
            let scalar = if *op == ast::BinOp::BooleanAnd || *op == ast::BinOp::BooleanOr {
                ir::ScalarType::Bool
            } else {
                let lhs_scalar = match context.module.type_registry.extract_scalar(left_base) {
                    Some(s) => s,
                    None => return Err(TyperError::BinaryOperationNonNumericType(base_location)),
                };
                let rhs_scalar = match context.module.type_registry.extract_scalar(right_base) {
                    Some(s) => s,
                    None => return Err(TyperError::BinaryOperationNonNumericType(base_location)),
                };
                match (lhs_scalar, rhs_scalar) {
                    (ir::ScalarType::Int, ir::ScalarType::Int) => ir::ScalarType::Int,
                    (ir::ScalarType::Int, ir::ScalarType::UInt) => ir::ScalarType::UInt,
                    (ir::ScalarType::UInt, ir::ScalarType::Int) => ir::ScalarType::UInt,
                    (ir::ScalarType::UInt, ir::ScalarType::UInt) => ir::ScalarType::UInt,
                    (ir::ScalarType::UntypedInt, ir::ScalarType::Int) => ir::ScalarType::Int,
                    (ir::ScalarType::UntypedInt, ir::ScalarType::UInt) => ir::ScalarType::UInt,
                    (ir::ScalarType::Int, ir::ScalarType::UntypedInt) => ir::ScalarType::Int,
                    (ir::ScalarType::UInt, ir::ScalarType::UntypedInt) => ir::ScalarType::UInt,
                    (ir::ScalarType::UntypedInt, ir::ScalarType::UntypedInt) => {
                        ir::ScalarType::UntypedInt
                    }
                    _ => return err_bad_type,
                }
            };
            let x = ir::TypeLayout::max_dim(lhs_tyl.to_x(), rhs_tyl.to_x());
            let y = ir::TypeLayout::max_dim(lhs_tyl.to_y(), rhs_tyl.to_y());
            let tyl = ir::TypeLayout::from_numeric_parts(scalar, x, y);
            let ty = context.module.type_registry.register_type(tyl);
            let ety = ty.to_rvalue();
            let lhs_cast = match ImplicitConversion::find(lhs_type, ety, &mut context.module) {
                Ok(cast) => cast,
                Err(()) => return err_bad_type,
            };
            let rhs_cast = match ImplicitConversion::find(rhs_type, ety, &mut context.module) {
                Ok(cast) => cast,
                Err(()) => return err_bad_type,
            };
            assert_eq!(
                lhs_cast.get_target_type(&mut context.module),
                rhs_cast.get_target_type(&mut context.module),
            );
            let lhs_final = lhs_cast.apply(lhs_ir, &mut context.module);
            let rhs_final = rhs_cast.apply(rhs_ir, &mut context.module);
            let i = match *op {
                ast::BinOp::BitwiseAnd => ir::IntrinsicOp::BitwiseAnd,
                ast::BinOp::BitwiseOr => ir::IntrinsicOp::BitwiseOr,
                ast::BinOp::BitwiseXor => ir::IntrinsicOp::BitwiseXor,
                ast::BinOp::BooleanAnd => ir::IntrinsicOp::BooleanAnd,
                ast::BinOp::BooleanOr => ir::IntrinsicOp::BooleanOr,
                _ => unreachable!(),
            };
            let lhs_target = lhs_cast.get_target_type(&mut context.module);
            let rhs_target = rhs_cast.get_target_type(&mut context.module);
            let output_type = i.get_return_type(&[lhs_target, rhs_target], &mut context.module);
            let node =
                ir::Expression::IntrinsicOp(i, Vec::new(), Vec::from([lhs_final, rhs_final]));
            Ok(TypedExpression::Value(node, output_type))
        }
        ast::BinOp::Assignment
        | ast::BinOp::SumAssignment
        | ast::BinOp::DifferenceAssignment
        | ast::BinOp::ProductAssignment
        | ast::BinOp::QuotientAssignment
        | ast::BinOp::RemainderAssignment => {
            if context
                .module
                .type_registry
                .extract_modifier(lhs_type.0)
                .1
                .is_const
            {
                return Err(TyperError::MutableRequired(lhs.get_location()));
            }
            let required_rtype = match lhs_type.1 {
                ir::ValueType::Lvalue => ExpressionType(lhs_type.0, ir::ValueType::Rvalue),
                _ => return Err(TyperError::LvalueRequired(lhs.get_location())),
            };
            match ImplicitConversion::find(rhs_type, required_rtype, &mut context.module) {
                Ok(rhs_cast) => {
                    let rhs_final = rhs_cast.apply(rhs_ir, &mut context.module);
                    let i = match *op {
                        ast::BinOp::Assignment => ir::IntrinsicOp::Assignment,
                        ast::BinOp::SumAssignment => ir::IntrinsicOp::SumAssignment,
                        ast::BinOp::DifferenceAssignment => ir::IntrinsicOp::DifferenceAssignment,
                        ast::BinOp::ProductAssignment => ir::IntrinsicOp::ProductAssignment,
                        ast::BinOp::QuotientAssignment => ir::IntrinsicOp::QuotientAssignment,
                        ast::BinOp::RemainderAssignment => ir::IntrinsicOp::RemainderAssignment,
                        _ => unreachable!(),
                    };
                    let rhs_type = rhs_cast.get_target_type(&mut context.module);
                    let output_type = i.get_return_type(&[lhs_type, rhs_type], &mut context.module);
                    let node =
                        ir::Expression::IntrinsicOp(i, Vec::new(), Vec::from([lhs_ir, rhs_final]));
                    Ok(TypedExpression::Value(node, output_type))
                }
                Err(()) => err_bad_type,
            }
        }
        ast::BinOp::Sequence => {
            let node = ir::Expression::Sequence(Vec::from([lhs_ir, rhs_ir]));
            Ok(TypedExpression::Value(node, rhs_type))
        }
    }
}

fn parse_expr_ternary(
    cond: &Located<ast::Expression>,
    lhs: &Located<ast::Expression>,
    rhs: &Located<ast::Expression>,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    let cond_location = cond.get_location();

    // Generate sub expressions
    let (cond, cond_ety) = parse_expr_value_only(cond, context)?;
    let (lhs, lhs_ety) = parse_expr_value_only(lhs, context)?;
    let (rhs, rhs_ety) = parse_expr_value_only(rhs, context)?;

    let lhs_ty = lhs_ety.0;
    let rhs_ty = rhs_ety.0;
    let wrong_types_err = Err(TyperError::TernaryArmsMustHaveSameType(
        lhs_ty.to_error_type(),
        rhs_ty.to_error_type(),
        cond_location,
    ));
    let (lhs_ty_base, lhs_mod) = context.module.type_registry.extract_modifier(lhs_ty);
    let (rhs_ty_base, _) = context.module.type_registry.extract_modifier(rhs_ty);
    let lhs_tyl = context.module.type_registry.get_type_layout(lhs_ty_base);
    let rhs_tyl = context.module.type_registry.get_type_layout(rhs_ty_base);

    // Attempt to find best scalar match between match arms
    // This will return None for non-numeric types
    let st = match (lhs_tyl.to_scalar(), rhs_tyl.to_scalar()) {
        (Some(left_scalar), Some(right_scalar)) => Some(most_sig_scalar(left_scalar, right_scalar)),
        _ => None,
    };

    // Attempt to find best vector match
    // This will return None for non-numeric types
    // This may return None for some combinations of numeric layouts
    let nd = ir::TypeLayout::most_significant_dimension(lhs_tyl, rhs_tyl);

    // Transform the types
    let (lhs_target_tyl, rhs_target_tyl) = match (st, nd) {
        (Some(st), Some(nd)) => {
            let tyl = ir::TypeLayout::from_numeric_dimensions(st, nd);
            (tyl.clone(), tyl)
        }
        (Some(st), None) => {
            let left = lhs_tyl.clone().transform_scalar(st);
            let right = rhs_tyl.clone().transform_scalar(st);
            (left, right)
        }
        (None, Some(_)) => {
            panic!(
                "internal error: most_sig_scalar failed where most_significant_dimension succeeded"
            )
        }
        (None, None) => (lhs_tyl.clone(), rhs_tyl.clone()),
    };

    let comb_tyl = if lhs_target_tyl == rhs_target_tyl {
        lhs_target_tyl
    } else {
        return wrong_types_err;
    };

    let target_mod = ir::TypeModifier {
        is_const: false,
        volatile: false,
        row_major: lhs_mod.row_major,
        column_major: lhs_mod.column_major,
        unorm: false,
        snorm: false,
    };

    let comb_unmodified_ty_id = context.module.type_registry.register_type(comb_tyl);
    let comb_ty_id = context
        .module
        .type_registry
        .combine_modifier(comb_unmodified_ty_id, target_mod);
    let ety_target = comb_ty_id.to_rvalue();

    let left_cast = match ImplicitConversion::find(lhs_ety, ety_target, &mut context.module) {
        Ok(cast) => cast,
        Err(()) => return wrong_types_err,
    };
    let right_cast = match ImplicitConversion::find(rhs_ety, ety_target, &mut context.module) {
        Ok(cast) => cast,
        Err(()) => return wrong_types_err,
    };

    let lhs_casted = Box::new(left_cast.apply(lhs, &mut context.module));
    let rhs_casted = Box::new(right_cast.apply(rhs, &mut context.module));
    assert_eq!(
        left_cast.get_target_type(&mut context.module),
        right_cast.get_target_type(&mut context.module)
    );
    let final_type = left_cast.get_target_type(&mut context.module);

    let bool_ty = context
        .module
        .type_registry
        .register_type(ir::TypeLayout::bool());

    // Cast the condition
    let cond_cast =
        match ImplicitConversion::find(cond_ety, bool_ty.to_rvalue(), &mut context.module) {
            Ok(cast) => cast,
            Err(()) => {
                return Err(TyperError::TernaryConditionRequiresBoolean(
                    cond_ety.to_error_type(),
                    cond_location,
                ))
            }
        };
    let cond_casted = Box::new(cond_cast.apply(cond, &mut context.module));

    let node = ir::Expression::TernaryConditional(cond_casted, lhs_casted, rhs_casted);
    Ok(TypedExpression::Value(node, final_type))
}

/// Reduce value type of a swizzle if it uses the same slot multiple times
fn get_swizzle_vt(swizzle: &Vec<ir::SwizzleSlot>, mut vt: ir::ValueType) -> ir::ValueType {
    for i in 0..swizzle.len() {
        for j in 0..i {
            if swizzle[i] == swizzle[j] {
                vt = ir::ValueType::Rvalue;
            }
        }
    }
    vt
}

fn parse_expr_unchecked(
    ast: &ast::Expression,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    match *ast {
        ast::Expression::Literal(ref lit) => parse_literal(lit, context),
        ast::Expression::Identifier(ref id) => parse_identifier(id, context),
        ast::Expression::UnaryOperation(ref op, ref expr) => parse_expr_unaryop(op, expr, context),
        ast::Expression::BinaryOperation(ref op, ref lhs, ref rhs) => {
            parse_expr_binop(op, lhs, rhs, context)
        }
        ast::Expression::TernaryConditional(ref cond, ref lhs, ref rhs) => {
            parse_expr_ternary(cond, lhs, rhs, context)
        }
        ast::Expression::ArraySubscript(ref array, ref subscript) => {
            let array_texp = parse_expr_internal(array, context)?;
            let subscript_texp = parse_expr_internal(subscript, context)?;
            let (array_ir, array_ty) = match array_texp {
                TypedExpression::Value(array_ir, array_ty) => (array_ir, array_ty),
                _ => return Err(TyperError::ArrayIndexingNonArrayType(array.location)),
            };
            let (subscript_ir, subscript_ty) = match subscript_texp {
                TypedExpression::Value(subscript_ir, subscript_ty) => (subscript_ir, subscript_ty),
                _ => return Err(TyperError::ArrayIndexingNonArrayType(array.location)),
            };
            let ty_nomod = context.module.type_registry.remove_modifier(array_ty.0);
            let tyl_nomod = context.module.type_registry.get_type_layout(ty_nomod);
            let node = match tyl_nomod {
                ir::TypeLayout::Array(_, _)
                | ir::TypeLayout::Vector(_, _)
                | ir::TypeLayout::Object(ir::ObjectType::Buffer(_))
                | ir::TypeLayout::Object(ir::ObjectType::RWBuffer(_))
                | ir::TypeLayout::Object(ir::ObjectType::StructuredBuffer(_))
                | ir::TypeLayout::Object(ir::ObjectType::RWStructuredBuffer(_)) => {
                    let index_type = context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayout::uint());
                    let index = index_type.to_rvalue();
                    let cast_to_int_result =
                        ImplicitConversion::find(subscript_ty, index, &mut context.module);
                    let subscript_final = match cast_to_int_result {
                        Err(_) => {
                            return Err(TyperError::ArraySubscriptIndexNotInteger(
                                subscript.get_location(),
                            ))
                        }
                        Ok(cast) => cast.apply(subscript_ir, &mut context.module),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                ir::TypeLayout::Object(
                    ir::ObjectType::Texture2D(_) | ir::ObjectType::Texture2DMipsSlice(_),
                ) => {
                    let index_type = context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayout::uintn(2));
                    let index = index_type.to_rvalue();
                    let cast = ImplicitConversion::find(subscript_ty, index, &mut context.module);
                    let subscript_final = match cast {
                        Err(_) => {
                            return Err(TyperError::ArraySubscriptIndexNotInteger(
                                subscript.get_location(),
                            ))
                        }
                        Ok(cast) => cast.apply(subscript_ir, &mut context.module),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                ir::TypeLayout::Object(ir::ObjectType::Texture2DMips(_)) => {
                    let index_type = context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayout::uint());
                    let index = index_type.to_rvalue();
                    let cast = ImplicitConversion::find(subscript_ty, index, &mut context.module);
                    let subscript_final = match cast {
                        Err(_) => {
                            return Err(TyperError::ArraySubscriptIndexNotInteger(
                                subscript.get_location(),
                            ))
                        }
                        Ok(cast) => cast.apply(subscript_ir, &mut context.module),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(_)) => {
                    let index_type = context
                        .module
                        .type_registry
                        .register_type(ir::TypeLayout::uintn(2));
                    let index = index_type.to_rvalue();
                    let cast = ImplicitConversion::find(subscript_ty, index, &mut context.module);
                    let subscript_final = match cast {
                        Err(_) => {
                            return Err(TyperError::ArraySubscriptIndexNotInteger(
                                subscript.get_location(),
                            ))
                        }
                        Ok(cast) => cast.apply(subscript_ir, &mut context.module),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                _ => Err(TyperError::ArrayIndexingNonArrayType(array.location)),
            }?;
            let ety = match get_expression_type(&node, context) {
                Ok(ety) => ety,
                Err(err) => panic!("internal error: type unknown ({:?}", err),
            };
            Ok(TypedExpression::Value(node, ety))
        }
        ast::Expression::Member(ref composite, ref member) => {
            let error_name = member;
            let composite_texp = parse_expr_internal(composite, context)?;
            let composite_pt = composite_texp.to_error_type();
            let (composite_ir, composite_ety) = match composite_texp {
                TypedExpression::Value(composite_ir, composite_type) => {
                    (composite_ir, composite_type)
                }
                _ => {
                    return Err(TyperError::TypeDoesNotHaveMembers(
                        composite_pt,
                        composite.get_location(),
                    ))
                }
            };
            let ExpressionType(composite_ty, vt) = composite_ety;

            let (mut composite_ty_nomod, composite_mod) =
                context.module.type_registry.extract_modifier(composite_ty);
            let mut composite_tyl_nomod = context
                .module
                .type_registry
                .get_type_layer(composite_ty_nomod);

            // If it is a constant buffer then auto unwrap the inner type
            if let ir::TypeLayer::Object(ir::ObjectType::ConstantBuffer(inner)) =
                composite_tyl_nomod
            {
                composite_ty_nomod = context.module.type_registry.remove_modifier(inner);
                composite_tyl_nomod = context
                    .module
                    .type_registry
                    .get_type_layer(composite_ty_nomod);
            }

            match composite_tyl_nomod {
                ir::TypeLayer::Struct(id) => {
                    assert!(!member.identifiers.is_empty());
                    let member_name = &member.identifiers.last().unwrap().node;

                    // Ensure the path is for the correct type
                    // We have no inheritance / data hiding here so this is purely validation - it will always link back to the provided struct
                    if member.try_trivial().is_none() {
                        let mut path = member.clone();
                        path.identifiers.pop();
                        match context.find_identifier(&path) {
                            Ok(VariableExpression::Type(ty)) => {
                                let ty_unmod = context.module.type_registry.remove_modifier(ty);
                                let tyl_unmod =
                                    context.module.type_registry.get_type_layer(ty_unmod);
                                match tyl_unmod {
                                    ir::TypeLayer::Struct(found_id) => {
                                        if id != found_id {
                                            return Err(TyperError::MemberIsForDifferentType(
                                                composite_ty,
                                                ty,
                                                path,
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(TyperError::IdentifierIsNotAMember(
                                            composite_ty,
                                            path,
                                        ))
                                    }
                                }
                            }
                            Ok(_) => {
                                return Err(TyperError::IdentifierIsNotAMember(composite_ty, path))
                            }
                            Err(err) => return Err(err),
                        }
                    };

                    match context.get_struct_member_expression(id, member_name) {
                        Ok(StructMemberValue::Variable(ty)) => {
                            let composite = Box::new(composite_ir);
                            let member = ir::Expression::Member(composite, member_name.clone());
                            Ok(TypedExpression::Value(member, ty.to_lvalue()))
                        }
                        Ok(StructMemberValue::Method(overloads)) => {
                            Ok(TypedExpression::Method(UnresolvedMethod {
                                object_type: composite_ty,
                                overloads,
                                object_value: composite_ir,
                            }))
                        }
                        Err(err) => Err(err),
                    }
                }
                ir::TypeLayer::Scalar(_) => {
                    let member = match member.try_trivial() {
                        Some(member) => member,
                        None => {
                            return Err(TyperError::MemberDoesNotExist(
                                composite_ty,
                                member.clone(),
                            ))
                        }
                    };

                    let mut swizzle_slots = Vec::with_capacity(member.len());
                    for c in member.chars() {
                        swizzle_slots.push(match c {
                            'x' | 'r' => ir::SwizzleSlot::X,
                            // Assume we were not trying to swizzle and accidentally accessed a member of a scalar
                            _ => {
                                return Err(TyperError::TypeDoesNotHaveMembers(
                                    composite_pt,
                                    composite.get_location(),
                                ))
                            }
                        });
                    }
                    let vt = get_swizzle_vt(&swizzle_slots, vt);
                    let ty_unmod = if swizzle_slots.len() == 1 {
                        composite_ty_nomod
                    } else {
                        context
                            .module
                            .type_registry
                            .register_type_layer(ir::TypeLayer::Vector(
                                composite_ty_nomod,
                                swizzle_slots.len() as u32,
                            ))
                    };
                    let ty = context
                        .module
                        .type_registry
                        .combine_modifier(ty_unmod, composite_mod);
                    let ety = ExpressionType(ty, vt);
                    let node = ir::Expression::Swizzle(Box::new(composite_ir), swizzle_slots);
                    Ok(TypedExpression::Value(node, ety))
                }
                ir::TypeLayer::Vector(scalar, x) => {
                    let member = match member.try_trivial() {
                        Some(member) => member,
                        None => {
                            return Err(TyperError::MemberDoesNotExist(
                                composite_ty,
                                member.clone(),
                            ))
                        }
                    };

                    let mut swizzle_slots = Vec::with_capacity(member.len());
                    for c in member.chars() {
                        swizzle_slots.push(match c {
                            'x' | 'r' if x >= 1 => ir::SwizzleSlot::X,
                            'y' | 'g' if x >= 2 => ir::SwizzleSlot::Y,
                            'z' | 'b' if x >= 3 => ir::SwizzleSlot::Z,
                            'w' | 'a' if x >= 4 => ir::SwizzleSlot::W,
                            _ => {
                                return Err(TyperError::InvalidSwizzle(
                                    composite_ty,
                                    member.node.clone(),
                                    member.get_location(),
                                ))
                            }
                        });
                    }
                    let vt = get_swizzle_vt(&swizzle_slots, vt);
                    // Lets say single element swizzles go to scalars
                    // Technically they might be going to 1 element vectors
                    // that then get downcasted
                    // But it's hard to tell as scalars + single element vectors
                    // have the same overload precedence
                    let ty_unmod = if swizzle_slots.len() == 1 {
                        scalar
                    } else {
                        context
                            .module
                            .type_registry
                            .register_type_layer(ir::TypeLayer::Vector(
                                scalar,
                                swizzle_slots.len() as u32,
                            ))
                    };
                    let ty = context
                        .module
                        .type_registry
                        .combine_modifier(ty_unmod, composite_mod);
                    let ety = ExpressionType(ty, vt);
                    let node = ir::Expression::Swizzle(Box::new(composite_ir), swizzle_slots);
                    Ok(TypedExpression::Value(node, ety))
                }
                ir::TypeLayer::Object(object_type) => {
                    // We do not currently support checking complex identifier patterns
                    let member = match member.try_trivial() {
                        Some(member) => member,
                        None => {
                            return Err(TyperError::MemberDoesNotExist(
                                composite_ty,
                                member.clone(),
                            ))
                        }
                    };

                    // Handle mips mips member explicitly as it is the only member on an intrinsic object type
                    if let ir::ObjectType::Texture2D(ty) = object_type {
                        if member.node == "mips" {
                            let composite = Box::new(composite_ir);
                            let member = ir::Expression::Member(composite, member.node.clone());
                            let mips_oty = ir::ObjectType::Texture2DMips(ty);
                            let mips_tyl = ir::TypeLayer::Object(mips_oty);
                            let mips_ty =
                                context.module.type_registry.register_type_layer(mips_tyl);
                            return Ok(TypedExpression::Value(member, mips_ty.to_lvalue()));
                        }
                    }

                    // Get the object id or register it if it was not already seen
                    let obj_id = context.module.register_object(object_type);

                    let mut overloads = Vec::new();
                    for func_id in context.module.type_registry.get_object_functions(obj_id) {
                        if context.module.function_registry.get_function_name(*func_id)
                            == member.node
                        {
                            overloads.push(*func_id)
                        }
                    }

                    if overloads.is_empty() {
                        Err(TyperError::MemberDoesNotExist(
                            composite_ty,
                            error_name.clone(),
                        ))
                    } else {
                        Ok(TypedExpression::Method(UnresolvedMethod {
                            object_type: composite_ty,
                            overloads,
                            object_value: composite_ir,
                        }))
                    }
                }
                // Todo: Matrix components + Object members
                _ => Err(TyperError::TypeDoesNotHaveMembers(
                    composite_pt,
                    composite.get_location(),
                )),
            }
        }
        ast::Expression::Call(ref func, ref template_args, ref args) => {
            // If we are a simple identifier then check of we are a type
            if let ast::Expression::Identifier(ref name) = func.node {
                // Build an ast type from the pure identifier
                let candidate_type = ast::Type::from_layout(ast::TypeLayout(
                    name.clone(),
                    template_args.clone().into_boxed_slice(),
                ));
                // Attempt to convert the ast type to an ir type
                if let Ok(ty) = parse_type(&candidate_type, context) {
                    // If we are successful then this Call node is actually for a constructor
                    return parse_expr_constructor(ty, args, context);
                }
            }

            // If we are not a constructor then process this as a normal call
            parse_expr_call(func, template_args, args, context)
        }
        ast::Expression::Cast(ref ty, ref expr) => {
            let expr_texp = parse_expr_internal(expr, context)?;
            let expr_pt = expr_texp.to_error_type();
            match expr_texp {
                TypedExpression::Value(expr_ir, _) => {
                    let ir_type = parse_type(ty, context)?;
                    Ok(TypedExpression::Value(
                        ir::Expression::Cast(ir_type, Box::new(expr_ir)),
                        ir_type.to_rvalue(),
                    ))
                }
                _ => Err(TyperError::InvalidCast(
                    expr_pt,
                    ErrorType::Untyped(ty.clone()),
                    expr.get_location(),
                )),
            }
        }
        ast::Expression::SizeOf(ref ty) => {
            let ir_type = parse_type(ty, context)?;
            let uint_ty = context
                .module
                .type_registry
                .register_type(ir::TypeLayout::uint());
            Ok(TypedExpression::Value(
                ir::Expression::SizeOf(ir_type),
                uint_ty.to_rvalue(),
            ))
        }
        ast::Expression::AmbiguousParseBranch(ref constrained_exprs) => {
            let (last, main) = constrained_exprs
                .split_last()
                .expect("AmbiguousParseBranch should not be empty");
            // Try to take each path in turn if the names are types
            for constrained_expr in main {
                let mut valid = true;
                for type_name in &constrained_expr.expected_type_names {
                    // Unfortunate copy to resolve the type name
                    // Moving the registered types into a more central location and including built ins
                    // should make this simpler
                    let ast_tyl = ast::TypeLayout(type_name.clone(), Default::default());
                    if parse_typelayout(&ast_tyl, context).is_err() {
                        valid = false;
                    }
                }
                if valid {
                    return parse_expr_internal(&constrained_expr.expr, context);
                }
            }
            // Always fall back to last expression even if it has a type requirement
            // We should then fail to parse and return an error with a hopefully more accurate location
            parse_expr_internal(&last.expr, context)
        }
    }
}

/// Process types for a call invocation
fn parse_expr_call(
    func: &Located<ast::Expression>,
    template_args: &[ast::ExpressionOrType],
    args: &[Located<ast::Expression>],
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    let func_texp = parse_expr_internal(func, context)?;

    // Process template arguments
    let mut template_args_ir = Vec::new();
    for template_arg in template_args {
        let ty = parse_expression_or_type(template_arg, context)?;
        template_args_ir.push(Located::new(ty, template_arg.get_location()));
    }

    // Process arguments
    let mut args_ir: Vec<ir::Expression> = vec![];
    let mut args_types: Vec<ExpressionType> = vec![];
    for arg in args {
        let expr_texp = parse_expr_internal(arg, context)?;
        let (expr_ir, expr_ty) = match expr_texp {
            TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
            texp => {
                return Err(TyperError::FunctionPassedToAnotherFunction(
                    func_texp.to_error_type(),
                    texp.to_error_type(),
                    arg.get_location(),
                ))
            }
        };
        args_ir.push(expr_ir);
        args_types.push(expr_ty);
    }

    match func_texp {
        TypedExpression::Function(unresolved) => write_function(
            unresolved,
            &template_args_ir,
            &args_types,
            args_ir,
            func.location,
            ir::CallType::FreeFunction,
            context,
        ),
        TypedExpression::MethodInternal(unresolved) => write_function(
            unresolved,
            &template_args_ir,
            &args_types,
            args_ir,
            func.location,
            ir::CallType::MethodInternal,
            context,
        ),
        TypedExpression::Method(unresolved) => write_method(
            unresolved,
            &template_args_ir,
            &args_types,
            args_ir,
            func.location,
            context,
        ),
        _ => Err(TyperError::CallOnNonFunction(func.get_location())),
    }
}

/// Process types for a constructor invocation
fn parse_expr_constructor(
    ty: ir::TypeId,
    args: &[Located<ast::Expression>],
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    let error_location = args
        .get(0)
        .map(|e| e.location)
        .unwrap_or(SourceLocation::UNKNOWN);
    let ty_nomod = context.module.type_registry.remove_modifier(ty);
    let tyl = context.module.type_registry.get_type_layer(ty_nomod);
    let expected_num_elements = tyl.get_num_elements();
    let target_scalar = match context.module.type_registry.extract_scalar(ty_nomod) {
        Some(st) => st,
        None => return Err(TyperError::ConstructorWrongArgumentCount(error_location)),
    };
    let mut slots: Vec<ir::ConstructorSlot> = vec![];
    let mut total_arity = 0;
    for param in args {
        let expr_texp = parse_expr_internal(param, context)?;
        let (expr_base, ety) = match expr_texp {
            TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
            _ => return Err(TyperError::FunctionNotCalled(param.get_location())),
        };
        let expr_ty_nomod = context.module.type_registry.remove_modifier(ety.0);
        let expr_tyl = context.module.type_registry.get_type_layer(expr_ty_nomod);
        let arity = expr_tyl.get_num_elements();
        total_arity += arity;
        let target_scalar_ty = context
            .module
            .type_registry
            .register_type_layer(ir::TypeLayer::Scalar(target_scalar));
        let target_ty = match expr_tyl {
            ir::TypeLayer::Scalar(_) => target_scalar_ty,
            ir::TypeLayer::Vector(_, x) => context
                .module
                .type_registry
                .register_type_layer(ir::TypeLayer::Vector(target_scalar_ty, x)),
            ir::TypeLayer::Matrix(_, x, y) => context
                .module
                .type_registry
                .register_type_layer(ir::TypeLayer::Matrix(target_scalar_ty, x, y)),
            _ => return Err(TyperError::WrongTypeInConstructor(param.get_location())),
        };
        let target_type = target_ty.to_rvalue();
        let cast = match ImplicitConversion::find(ety, target_type, &mut context.module) {
            Ok(cast) => cast,
            Err(()) => return Err(TyperError::WrongTypeInConstructor(param.get_location())),
        };
        let expr = cast.apply(expr_base, &mut context.module);
        slots.push(ir::ConstructorSlot { arity, expr });
    }
    let ety = ty.to_rvalue();
    if total_arity == expected_num_elements {
        let cons = ir::Expression::Constructor(ety.0, slots);
        Ok(TypedExpression::Value(cons, ety))
    } else {
        Err(TyperError::ConstructorWrongArgumentCount(error_location))
    }
}

/// Parse an expression internally within the expression parser
/// This allows intermediate values for processing function overloads
fn parse_expr_internal(
    expr: &ast::Expression,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    let texp = parse_expr_unchecked(expr, context)?;
    match texp {
        #[cfg(debug_assertions)]
        TypedExpression::Value(ref expr, ref ty_expected) => {
            let ty_query_res = get_expression_type(expr, context);
            let ty_query = ty_query_res.expect("type unknown");
            assert!(
                ty_query == *ty_expected,
                "[{:?}, {:?}] != [{:?}, {:?}]: {:?}",
                context.module.type_registry.get_type_layout(ty_query.0),
                ty_query.1,
                context.module.type_registry.get_type_layout(ty_expected.0),
                ty_expected.1,
                expr,
            );
        }
        _ => {}
    };
    Ok(texp)
}

/// Parse an expression that is not within another expression
fn parse_expr_value_only(
    expr: &ast::Expression,
    context: &mut Context,
) -> TyperResult<(ir::Expression, ExpressionType)> {
    let expr_ir = parse_expr_internal(expr, context)?;
    match expr_ir {
        TypedExpression::Value(expr, ety) => Ok((expr, ety)),
        TypedExpression::Function(_) => Err(TyperError::FunctionNotCalled(SourceLocation::UNKNOWN)),
        TypedExpression::MethodInternal(_) => {
            Err(TyperError::FunctionNotCalled(SourceLocation::UNKNOWN))
        }
        TypedExpression::Method(_) => Err(TyperError::FunctionNotCalled(SourceLocation::UNKNOWN)),
    }
}

/// Parse an expression
pub fn parse_expr(
    expr: &ast::Expression,
    context: &mut Context,
) -> TyperResult<(ir::Expression, ExpressionType)> {
    // Type errors should error out here
    let (expr_ir, expr_ety) = parse_expr_value_only(expr, context)?;

    // Ensure the returned type is the same as the type we can query
    {
        let ety_res = get_expression_type(&expr_ir, context);
        let ety = ety_res.expect("type unknown");
        assert!(
            ety == expr_ety,
            "{:?} == {:?}: {:?}",
            ety,
            expr_ety,
            expr_ir
        );
    }

    Ok((expr_ir, expr_ety))
}

/// Find the type of a constant
fn get_constant_type(literal: &ir::Constant, context: &mut Context) -> ExpressionType {
    let tyl = match *literal {
        ir::Constant::Bool(_) => ir::TypeLayout::bool(),
        ir::Constant::UntypedInt(_) => ir::TypeLayout::from_scalar(ir::ScalarType::UntypedInt),
        ir::Constant::Int(_) => ir::TypeLayout::int(),
        ir::Constant::UInt(_) => ir::TypeLayout::uint(),
        ir::Constant::Long(_) => unimplemented!(),
        ir::Constant::Half(_) => ir::TypeLayout::from_scalar(ir::ScalarType::Half),
        ir::Constant::Float(_) => ir::TypeLayout::float(),
        ir::Constant::Double(_) => ir::TypeLayout::double(),
        ir::Constant::String(_) => panic!("strings not supported"),
        ir::Constant::Enum(_, _) => panic!("enum not expected"),
    };
    context.module.type_registry.register_type(tyl).to_rvalue()
}

/// Find the type of an expression
fn get_expression_type(
    expression: &ir::Expression,
    context: &mut Context,
) -> TyperResult<ExpressionType> {
    match *expression {
        ir::Expression::Literal(ref lit) => Ok(get_constant_type(lit, context)),
        ir::Expression::Variable(var_ref) => context.get_type_of_variable(var_ref),
        ir::Expression::MemberVariable(ref name) => {
            let struct_id = context.get_current_owning_struct();
            context.get_type_of_struct_member(struct_id, name)
        }
        ir::Expression::Global(id) => context.get_type_of_global(id),
        ir::Expression::ConstantVariable(id, ref name) => context.get_type_of_constant(id, name),
        ir::Expression::EnumValue(id) => Ok(context
            .module
            .enum_registry
            .get_enum_value(id)
            .type_id
            .to_rvalue()),
        ir::Expression::TernaryConditional(_, ref expr_left, ref expr_right) => {
            // Ensure the layouts of each side are the same
            // Value types + modifiers can be different
            {
                let ty_left_mod = get_expression_type(expr_left, context)?.0;
                let ty_right_mod = get_expression_type(expr_right, context)?.0;
                let ty_left = context.module.type_registry.remove_modifier(ty_left_mod);
                let ty_right = context.module.type_registry.remove_modifier(ty_right_mod);
                assert_eq!(ty_left, ty_right,);
            }
            let ety = get_expression_type(expr_left, context)?;
            Ok(ety.0.to_rvalue())
        }
        ir::Expression::Sequence(ref chain) => {
            let last = chain
                .last()
                .expect("Sequence must have at least one expression");
            let ety = get_expression_type(last, context)?;
            Ok(ety)
        }
        ir::Expression::Swizzle(ref vec, ref swizzle) => {
            let ExpressionType(vec_ty, vec_vt) = get_expression_type(vec, context)?;
            let (vec_ty_nomod, vec_mod) = context.module.type_registry.extract_modifier(vec_ty);
            let vec_tyl_nomod = context.module.type_registry.get_type_layer(vec_ty_nomod);
            let vt = get_swizzle_vt(swizzle, vec_vt);
            let ty = match vec_tyl_nomod {
                ir::TypeLayer::Scalar(_) => {
                    if swizzle.len() == 1 {
                        vec_ty_nomod
                    } else {
                        context
                            .module
                            .type_registry
                            .register_type_layer(ir::TypeLayer::Vector(
                                vec_ty_nomod,
                                swizzle.len() as u32,
                            ))
                    }
                }
                ir::TypeLayer::Vector(scalar, _) => {
                    if swizzle.len() == 1 {
                        scalar
                    } else {
                        context
                            .module
                            .type_registry
                            .register_type_layer(ir::TypeLayer::Vector(
                                scalar,
                                swizzle.len() as u32,
                            ))
                    }
                }
                _ => {
                    return Err(TyperError::InvalidTypeForSwizzle(
                        vec_ty_nomod,
                        SourceLocation::UNKNOWN,
                    ))
                }
            };
            let ty = context.module.type_registry.combine_modifier(ty, vec_mod);
            Ok(ExpressionType(ty, vt))
        }
        ir::Expression::ArraySubscript(ref array, _) => {
            let array_ty = get_expression_type(array, context)?;
            // Todo: Modifiers on object type template parameters
            let array_ty_nomod = context.module.type_registry.remove_modifier(array_ty.0);
            let array_tyl_nomod = context.module.type_registry.get_type_layer(array_ty_nomod);
            let ty = match array_tyl_nomod {
                ir::TypeLayer::Array(element, _) => element,
                ir::TypeLayer::Vector(st, _) => st,
                ir::TypeLayer::Object(ir::ObjectType::Buffer(ty))
                | ir::TypeLayer::Object(ir::ObjectType::StructuredBuffer(ty))
                | ir::TypeLayer::Object(ir::ObjectType::Texture2D(ty))
                | ir::TypeLayer::Object(ir::ObjectType::Texture2DMipsSlice(ty)) => {
                    context.module.type_registry.make_const(ty)
                }
                ir::TypeLayer::Object(ir::ObjectType::RWBuffer(ty))
                | ir::TypeLayer::Object(ir::ObjectType::RWStructuredBuffer(ty))
                | ir::TypeLayer::Object(ir::ObjectType::RWTexture2D(ty)) => ty,
                ir::TypeLayer::Object(ir::ObjectType::Texture2DMips(ty)) => {
                    let tyl = ir::TypeLayer::Object(ir::ObjectType::Texture2DMipsSlice(ty));
                    context.module.type_registry.register_type_layer(tyl)
                }
                _ => {
                    return Err(TyperError::ArrayIndexMustBeUsedOnArrayType(
                        array_ty_nomod,
                        SourceLocation::UNKNOWN,
                    ))
                }
            };
            Ok(ty.to_lvalue())
        }
        ir::Expression::Member(ref expr, ref name) => {
            let expr_type = get_expression_type(expr, context)?;

            let mut ty = context.module.type_registry.remove_modifier(expr_type.0);
            let mut tyl = context.module.type_registry.get_type_layer(ty);

            // Handle mips member of Texture2D
            if let ir::TypeLayer::Object(ir::ObjectType::Texture2D(inner)) = tyl {
                if name == "mips" {
                    let mips_oty = ir::ObjectType::Texture2DMips(inner);
                    let mips_tyl = ir::TypeLayer::Object(mips_oty);
                    let mips_ty = context.module.type_registry.register_type_layer(mips_tyl);
                    return Ok(mips_ty.to_lvalue());
                }
            }

            // If it is a constant buffer then auto unwrap the inner type
            if let ir::TypeLayer::Object(ir::ObjectType::ConstantBuffer(inner)) = tyl {
                ty = context.module.type_registry.remove_modifier(inner);
                tyl = context.module.type_registry.get_type_layer(ty);
            }

            let id = match tyl {
                ir::TypeLayer::Struct(id) => id,
                _ => {
                    return Err(TyperError::MemberNodeMustBeUsedOnStruct(
                        ty,
                        name.clone(),
                        SourceLocation::UNKNOWN,
                    ))
                }
            };
            context.get_type_of_struct_member(id, name)
        }
        ir::Expression::Call(id, _, _) => Ok(context
            .module
            .function_registry
            .get_function_signature(id)
            .return_type
            .return_type
            .to_rvalue()),
        ir::Expression::Constructor(ty, _) => Ok(ty.to_rvalue()),
        ir::Expression::Cast(ty, _) => Ok(ty.to_rvalue()),
        ir::Expression::SizeOf(_) => {
            let uint_ty = context
                .module
                .type_registry
                .register_type(ir::TypeLayout::uint());
            Ok(uint_ty.to_rvalue())
        }
        ir::Expression::IntrinsicOp(ref intrinsic, ref template_args, ref args) => {
            let mut arg_types = Vec::with_capacity(args.len());
            for arg in args {
                arg_types.push(get_expression_type(arg, context)?);
            }
            let ety = intrinsic.get_return_type(&arg_types, &mut context.module);
            let ty_sub = apply_template_type_substitution(ety.0, template_args, context);
            Ok(ExpressionType(ty_sub, ety.1))
        }
    }
}
