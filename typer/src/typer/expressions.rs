use super::errors::*;
use super::scopes::*;
use super::types::{parse_expression_or_type, parse_type, parse_typelayout};
use crate::casting::{ConversionPriority, ImplicitConversion};
use crate::evaluator::evaluate_constexpr;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::ExpressionType;
use rssl_text::{Locate, Located, SourceLocation};

/// Result of a variable query
pub enum VariableExpression {
    Constant(ir::Constant),
    Local(ir::VariableId, ir::TypeId),
    Member(ir::StructId, u32, ir::TypeId),
    Global(ir::GlobalId, ir::TypeId),
    ConstantBufferMember(ir::ConstantBufferMemberId, ir::TypeId),
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
        VariableExpression::Constant(c) => {
            let ty = c.get_type(&context.module);
            TypedExpression::Value(ir::Expression::Literal(c), ty)
        }
        VariableExpression::Local(var, ty) => {
            TypedExpression::Value(ir::Expression::Variable(var), ty.to_lvalue())
        }
        VariableExpression::Member(id, member_index, ty) => TypedExpression::Value(
            ir::Expression::MemberVariable(id, member_index),
            ty.to_lvalue(),
        ),
        VariableExpression::Global(id, ty) => {
            TypedExpression::Value(ir::Expression::Global(id), ty.to_lvalue())
        }
        VariableExpression::ConstantBufferMember(id, ty) => {
            TypedExpression::Value(ir::Expression::ConstantVariable(id), ty.to_lvalue())
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
    if !signature.template_params.is_empty() {
        if template_args.len() > signature.template_params.len() {
            // Too many template arguments provided
            return Err(());
        }

        // Gather template arguments and infer unspecified arguments
        let arg_count = signature.template_params.len();
        let mut inferred_args = Vec::with_capacity(arg_count);
        for i in 0..arg_count {
            let mut arg = if i < template_args.len() {
                // Argument given explicitly
                template_args[i].clone()
            } else {
                let signature = context.module.function_registry.get_function_signature(id);
                let template_type_id = match signature.template_params[i] {
                    ir::TemplateParam::Type(id) => id,
                    // No support for inferring value arguments
                    ir::TemplateParam::Value(_) => return Err(()),
                };

                // Argument missing - infer from other parameters
                let mut found_arg = None;
                for (required_type, source_type) in
                    signature.param_types.clone().iter().zip(param_types.iter())
                {
                    if let Some(ty) = try_infer_template_type(
                        template_type_id,
                        required_type.type_id,
                        source_type.0,
                        context,
                    ) {
                        found_arg = Some(ty);
                        break;
                    }
                }

                match found_arg {
                    Some(arg) => Located::none(ir::TypeOrConstant::Type(arg)),
                    None => return Err(()),
                }
            };

            // Transform template types into simpler form
            arg.node = match arg.node {
                ir::TypeOrConstant::Type(ty) => {
                    ir::TypeOrConstant::Type(normalize_template_type(ty, context))
                }
                ir::TypeOrConstant::Constant(c) => ir::TypeOrConstant::Constant(c),
            };

            inferred_args.push(arg);
        }

        assert!(!inferred_args.is_empty());

        let new_id = if context
            .module
            .function_registry
            .get_intrinsic_data(id)
            .is_some()
        {
            context.build_intrinsic_template(id, &inferred_args)
        } else {
            match context.build_function_template_signature(id, &inferred_args) {
                Some(id) => id,
                None => return Err(()),
            }
        };

        id = new_id;
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
        let ety = ExpressionType(required_type.type_id, required_type.input_modifier.into());
        if let Ok(cast) = ImplicitConversion::find(*source_type, ety, &mut context.module) {
            overload_casts.push(cast)
        } else {
            return Err(());
        }
    }
    Ok((id, overload_casts))
}

/// Attempt to match a function against a set of argument and return the casts required to invoke it
fn try_infer_template_type(
    template_type_id: ir::TemplateTypeId,
    required_type_id: ir::TypeId,
    input_type_id: ir::TypeId,
    context: &mut Context,
) -> Option<ir::TypeId> {
    let layer_required = context
        .module
        .type_registry
        .get_type_layer(required_type_id);

    // If we have reached the template type then return the matched type
    if let ir::TypeLayer::TemplateParam(id) = layer_required {
        if id == template_type_id {
            return Some(input_type_id);
        }
    }

    // Drill down into the type
    let layer_input = context.module.type_registry.get_type_layer(input_type_id);
    match (layer_required, layer_input) {
        (ir::TypeLayer::Vector(id_req, x_req), ir::TypeLayer::Vector(id_in, x_in))
            if x_req == x_in =>
        {
            if let Some(ty) = try_infer_template_type(template_type_id, id_req, id_in, context) {
                return Some(ty);
            }
        }
        (ir::TypeLayer::Matrix(id_req, x_req, y_req), ir::TypeLayer::Matrix(id_in, x_in, y_in))
            if x_req == x_in && y_req == y_in =>
        {
            if let Some(ty) = try_infer_template_type(template_type_id, id_req, id_in, context) {
                return Some(ty);
            }
        }
        (ir::TypeLayer::Array(id_req, len_req), ir::TypeLayer::Array(id_in, len_in))
            if len_req == len_in =>
        {
            if let Some(ty) = try_infer_template_type(template_type_id, id_req, id_in, context) {
                return Some(ty);
            }
        }
        _ => {}
    }

    None
}

/// Ensure a type is simple enou Remove literal float and literal int from a type
fn normalize_template_type(type_id: ir::TypeId, context: &mut Context) -> ir::TypeId {
    let layer = context.module.type_registry.get_type_layer(type_id);
    match layer {
        ir::TypeLayer::Scalar(ir::ScalarType::IntLiteral) => {
            // Int literals turn into int
            context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Int32))
        }
        ir::TypeLayer::Scalar(ir::ScalarType::FloatLiteral) => {
            // float literals turn into float
            context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Float32))
        }
        ir::TypeLayer::Vector(id, x) => {
            // Process inner of vector types
            let id = normalize_template_type(id, context);
            context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Vector(id, x))
        }
        ir::TypeLayer::Matrix(id, x, y) => {
            // Process inner of matrix types
            let id = normalize_template_type(id, context);
            context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Matrix(id, x, y))
        }
        ir::TypeLayer::Array(id, len) => {
            // Process inner of array types
            let id = normalize_template_type(id, context);
            context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Array(id, len))
        }
        ir::TypeLayer::Modifier(_, id) => {
            // Remove any modifiers
            normalize_template_type(id, context)
        }
        _ => type_id,
    }
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
        if param_types.len() <= signature.param_types.len()
            && param_types.len() >= signature.non_default_params
        {
            if let Ok((new_id, param_casts)) =
                find_overload_casts(*overload, template_args, param_types, context)
            {
                casts.push((new_id, param_casts))
            }
        }
    }

    // Cull everything that isn't equal best at matching numeric type
    let mut winning_numeric_casts = Vec::with_capacity(1);
    for (candidate, candidate_casts) in &casts {
        let mut winning = true;
        for (against, against_casts) in &casts {
            if candidate == against {
                continue;
            }
            assert_eq!(candidate_casts.len(), against_casts.len());
            let mut not_worse_than = true;
            for (candidate_cast, against_cast) in candidate_casts.iter().zip(against_casts) {
                let candidate_rank = *candidate_cast.get_rank().get_numeric_rank();
                let against_rank = *against_cast.get_rank().get_numeric_rank();
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
        for (_, _, order) in &casts {
            if *order < best_order {
                best_order = order.clone();
            }
        }

        let casts = casts
            .into_iter()
            .filter(|(_, _, order)| *order == best_order)
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
        if context
            .module
            .function_registry
            .get_template_instantiation_data(id)
            .is_some()
        {
            // Now we have to make the actual instance of that template
            context.build_function_template_body(id)?
        };

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
        if context
            .module
            .function_registry
            .get_template_instantiation_data(id)
            .is_some()
        {
            // Now we have to make the actual instance of that template
            context.build_function_template_body(id)?
        };

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
        ast::Literal::IntUntyped(i) => ir::Constant::IntLiteral(*i as i128),
        ast::Literal::IntUnsigned32(i) => ir::Constant::UInt32(*i as u32),
        ast::Literal::IntUnsigned64(i) => ir::Constant::UInt64(*i),
        ast::Literal::IntSigned64(i) => ir::Constant::Int64(*i),
        ast::Literal::FloatUntyped(f) => ir::Constant::FloatLiteral(*f),
        ast::Literal::Float16(f) => ir::Constant::Float16(*f),
        ast::Literal::Float32(f) => ir::Constant::Float32(*f),
        ast::Literal::Float64(f) => ir::Constant::Float64(*f),
        ast::Literal::String(_) => {
            return Err(TyperError::StringNotSupported(SourceLocation::UNKNOWN))
        }
    };

    let ty = constant.get_type(&context.module);

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

            let mut is_trivial = false;

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
                ast::UnaryOp::Plus | ast::UnaryOp::Minus => {
                    let input_ty_id = context.module.type_registry.remove_modifier(expr_ty.0);
                    let (op_output_ety, op_input_ety) =
                        match context.module.type_registry.get_type_layer(input_ty_id) {
                            ir::TypeLayer::Scalar(_)
                            | ir::TypeLayer::Vector(..)
                            | ir::TypeLayer::Matrix(..) => {
                                // Input is uncasted
                                // Output has const / lvalue removed
                                (input_ty_id.to_rvalue(), expr_ty)
                            }
                            ir::TypeLayer::Enum(id)
                                if matches!(
                                    context.module.enum_registry.get_underlying_scalar(id),
                                    ir::ScalarType::Int32 | ir::ScalarType::UInt32
                                ) =>
                            {
                                let op_ety =
                                    context.module.enum_registry.get_type_id(id).to_rvalue();

                                // Output is the same as input
                                (op_ety, op_ety)
                            }
                            _ => {
                                return Err(TyperError::UnaryOperationWrongTypes(
                                    op.clone(),
                                    ErrorType::Unknown,
                                    base_location,
                                ))
                            }
                        };

                    // Cast input expression to operator input type
                    let expr_ir = if expr_ty != op_input_ety {
                        let cast =
                            ImplicitConversion::find(expr_ty, op_input_ety, &mut context.module)
                                .unwrap();
                        cast.apply(expr_ir, &mut context.module)
                    } else {
                        expr_ir
                    };

                    let ir_op = match op {
                        ast::UnaryOp::Plus => ir::IntrinsicOp::Plus,
                        ast::UnaryOp::Minus => ir::IntrinsicOp::Minus,
                        _ => unreachable!(),
                    };

                    is_trivial = ir_op == ir::IntrinsicOp::Minus
                        && matches!(expr_ir, ir::Expression::Literal(_));

                    (ir_op, expr_ir, op_output_ety)
                }
                ast::UnaryOp::LogicalNot => {
                    let input_ty_id = context.module.type_registry.remove_modifier(expr_ty.0);
                    let (op_output_ety, op_input_ety) =
                        match context.module.type_registry.get_type_layer(input_ty_id) {
                            ir::TypeLayer::Scalar(_)
                            | ir::TypeLayer::Vector(..)
                            | ir::TypeLayer::Matrix(..) => {
                                if let Some(ir::ScalarType::Bool) =
                                    context.module.type_registry.extract_scalar(input_ty_id)
                                {
                                    // Input is uncasted
                                    // Output has const / lvalue removed
                                    (input_ty_id.to_rvalue(), expr_ty)
                                } else {
                                    let op_ety = context
                                        .module
                                        .type_registry
                                        .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Bool))
                                        .to_rvalue();

                                    // Input is casted to bool rvalue
                                    // Output is the same as input
                                    (op_ety, op_ety)
                                }
                            }
                            ir::TypeLayer::Enum(id)
                                if matches!(
                                    context.module.enum_registry.get_underlying_scalar(id),
                                    ir::ScalarType::Int32 | ir::ScalarType::UInt32
                                ) =>
                            {
                                let op_ety = context
                                    .module
                                    .type_registry
                                    .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Bool))
                                    .to_rvalue();

                                // Input is casted to bool rvalue
                                // Output is the same as input
                                (op_ety, op_ety)
                            }
                            _ => {
                                return Err(TyperError::UnaryOperationWrongTypes(
                                    op.clone(),
                                    ErrorType::Unknown,
                                    base_location,
                                ))
                            }
                        };

                    // Cast input expression to operator input type
                    let expr_ir = if expr_ty != op_input_ety {
                        let cast =
                            ImplicitConversion::find(expr_ty, op_input_ety, &mut context.module)
                                .unwrap();
                        cast.apply(expr_ir, &mut context.module)
                    } else {
                        expr_ir
                    };

                    (ir::IntrinsicOp::LogicalNot, expr_ir, op_output_ety)
                }
                ast::UnaryOp::BitwiseNot => {
                    let input_ty_id = context.module.type_registry.remove_modifier(expr_ty.0);
                    // We currently do not support vector arguments
                    let (op_output_ety, op_input_ety) =
                        match context.module.type_registry.get_type_layer(input_ty_id) {
                            ir::TypeLayer::Scalar(ir::ScalarType::IntLiteral)
                            | ir::TypeLayer::Scalar(ir::ScalarType::Int32)
                            | ir::TypeLayer::Scalar(ir::ScalarType::UInt32) => {
                                // Input is uncasted
                                // Output has const / lvalue removed
                                (input_ty_id.to_rvalue(), expr_ty)
                            }
                            ir::TypeLayer::Scalar(ir::ScalarType::Bool) => {
                                let op_ety = context
                                    .module
                                    .type_registry
                                    .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Int32))
                                    .to_rvalue();

                                // Input is casted to int rvalue
                                // Output is the same as input
                                (op_ety, op_ety)
                            }
                            ir::TypeLayer::Enum(id)
                                if matches!(
                                    context.module.enum_registry.get_underlying_scalar(id),
                                    ir::ScalarType::Int32 | ir::ScalarType::UInt32
                                ) =>
                            {
                                let op_ety =
                                    context.module.enum_registry.get_type_id(id).to_rvalue();

                                // Output is the same as input
                                (op_ety, op_ety)
                            }
                            _ => {
                                return Err(TyperError::UnaryOperationWrongTypes(
                                    op.clone(),
                                    ErrorType::Unknown,
                                    base_location,
                                ))
                            }
                        };

                    // Cast input expression to operator input type
                    let expr_ir = if expr_ty != op_input_ety {
                        let cast =
                            ImplicitConversion::find(expr_ty, op_input_ety, &mut context.module)
                                .unwrap();
                        cast.apply(expr_ir, &mut context.module)
                    } else {
                        expr_ir
                    };

                    (ir::IntrinsicOp::BitwiseNot, expr_ir, op_output_ety)
                }
            };

            let mut expr_with_op = ir::Expression::IntrinsicOp(intrinsic, Vec::from([eir]));

            // Collapse some simple operations - so for example negated literals become literals with negative values
            // We do not do this in the general case as we do not need to simplify the tree as much as possible
            // But removing certain patterns can make the tree feel more natural
            if is_trivial {
                if let Ok(value) = evaluate_constexpr(&expr_with_op, &mut context.module) {
                    expr_with_op = ir::Expression::Literal(value);
                }
            }

            Ok(TypedExpression::Value(expr_with_op, ety))
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
            ScalarType::IntLiteral => Some(1),
            ScalarType::Int32 => Some(2),
            ScalarType::UInt32 => Some(3),
            ScalarType::FloatLiteral => Some(4),
            ScalarType::Float16 => Some(5),
            ScalarType::Float32 => Some(6),
            ScalarType::Float64 => Some(7),
        }
    }

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
        | ast::BinOp::RightShift
        | ast::BinOp::BitwiseAnd
        | ast::BinOp::BitwiseOr
        | ast::BinOp::BitwiseXor
        | ast::BinOp::BooleanAnd
        | ast::BinOp::BooleanOr => {
            let left_base = context.module.type_registry.remove_modifier(lhs_type.0);
            let right_base = context.module.type_registry.remove_modifier(rhs_type.0);
            let lhs_tyl = context.module.type_registry.get_type_layer(left_base);
            let rhs_tyl = context.module.type_registry.get_type_layer(right_base);
            let target_nv_id = if *op == ast::BinOp::BooleanAnd || *op == ast::BinOp::BooleanOr {
                let lhs_is_vector = matches!(
                    lhs_tyl,
                    ir::TypeLayer::Vector(..) | ir::TypeLayer::Matrix(..),
                );
                let rhs_is_vector = matches!(
                    rhs_tyl,
                    ir::TypeLayer::Vector(..) | ir::TypeLayer::Matrix(..),
                );
                if lhs_is_vector || rhs_is_vector {
                    return Err(TyperError::ShortCircuitingVector(base_location));
                }
                context
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Bool))
            } else {
                let lhs_nv_id = context.module.type_registry.get_non_vector_id(left_base);
                let rhs_nv_id = context.module.type_registry.get_non_vector_id(right_base);

                // Require the inputs to be integer (or almost integer)
                let require_integer = matches!(
                    op,
                    ast::BinOp::LeftShift
                        | ast::BinOp::RightShift
                        | ast::BinOp::BitwiseAnd
                        | ast::BinOp::BitwiseOr
                        | ast::BinOp::BitwiseXor
                );
                if require_integer {
                    if !is_integer_or_bool_or_enum(lhs_nv_id, context) {
                        return Err(TyperError::IntegerTypeExpected(lhs.location));
                    }

                    if !is_integer_or_bool_or_enum(rhs_nv_id, context) {
                        return Err(TyperError::IntegerTypeExpected(rhs.location));
                    }
                }

                let mut target = most_significant_non_vector(
                    lhs_nv_id,
                    rhs_nv_id,
                    lhs.location,
                    rhs.location,
                    &mut context.module,
                )?;

                // Remap all bool types to int
                if let Some(scalar) = context.module.type_registry.extract_scalar(target) {
                    if scalar == ir::ScalarType::Bool {
                        target = context
                            .module
                            .type_registry
                            .transform_scalar(target, ir::ScalarType::Int32)
                    }
                }

                target
            };

            // Most operators apply component-wise - we select the dimensions required after vector expansion / truncation
            // This will expand scalars to a vector / matrix and then do the operation on that
            // TODO: It would be more natural to support native scalar vs vector/matrix operations without the cast
            let dim = match select_vector_rank(lhs_tyl, rhs_tyl) {
                Ok(res) => res,
                Err(_) => {
                    return Err(TyperError::BinaryOperationWrongTypes(
                        op.clone(),
                        lhs_type.to_error_type(),
                        rhs_type.to_error_type(),
                        base_location,
                    ))
                }
            };

            // Apply the found dimension (to both sides of input)
            let ty = match dim {
                ir::NumericDimension::Scalar => target_nv_id,
                ir::NumericDimension::Vector(x) => context
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::Vector(target_nv_id, x)),
                ir::NumericDimension::Matrix(x, y) => context
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::Matrix(target_nv_id, x, y)),
            };

            // Cast the input expressions to the required input types
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
                ast::BinOp::Add => ir::IntrinsicOp::Add,
                ast::BinOp::Subtract => ir::IntrinsicOp::Subtract,
                ast::BinOp::Multiply => ir::IntrinsicOp::Multiply,
                ast::BinOp::Divide => ir::IntrinsicOp::Divide,
                ast::BinOp::Modulus => ir::IntrinsicOp::Modulus,
                ast::BinOp::LessThan => ir::IntrinsicOp::LessThan,
                ast::BinOp::LessEqual => ir::IntrinsicOp::LessEqual,
                ast::BinOp::GreaterThan => ir::IntrinsicOp::GreaterThan,
                ast::BinOp::GreaterEqual => ir::IntrinsicOp::GreaterEqual,
                ast::BinOp::Equality => ir::IntrinsicOp::Equality,
                ast::BinOp::Inequality => ir::IntrinsicOp::Inequality,
                ast::BinOp::LeftShift => ir::IntrinsicOp::LeftShift,
                ast::BinOp::RightShift => ir::IntrinsicOp::RightShift,
                ast::BinOp::BitwiseAnd => ir::IntrinsicOp::BitwiseAnd,
                ast::BinOp::BitwiseOr => ir::IntrinsicOp::BitwiseOr,
                ast::BinOp::BitwiseXor => ir::IntrinsicOp::BitwiseXor,
                ast::BinOp::BooleanAnd => ir::IntrinsicOp::BooleanAnd,
                ast::BinOp::BooleanOr => ir::IntrinsicOp::BooleanOr,
                _ => unreachable!(),
            };
            let lhs_target = lhs_cast.get_target_type(&mut context.module);
            let rhs_target = rhs_cast.get_target_type(&mut context.module);
            let output_type = i.get_return_type(&[lhs_target, rhs_target], &context.module);
            let node = ir::Expression::IntrinsicOp(i, Vec::from([lhs_final, rhs_final]));
            Ok(TypedExpression::Value(node, output_type))
        }
        ast::BinOp::Assignment
        | ast::BinOp::SumAssignment
        | ast::BinOp::DifferenceAssignment
        | ast::BinOp::ProductAssignment
        | ast::BinOp::QuotientAssignment
        | ast::BinOp::RemainderAssignment
        | ast::BinOp::LeftShiftAssignment
        | ast::BinOp::RightShiftAssignment
        | ast::BinOp::BitwiseAndAssignment
        | ast::BinOp::BitwiseOrAssignment
        | ast::BinOp::BitwiseXorAssignment => {
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
                        ast::BinOp::LeftShiftAssignment => ir::IntrinsicOp::LeftShiftAssignment,
                        ast::BinOp::RightShiftAssignment => ir::IntrinsicOp::RightShiftAssignment,
                        ast::BinOp::BitwiseAndAssignment => ir::IntrinsicOp::BitwiseAndAssignment,
                        ast::BinOp::BitwiseOrAssignment => ir::IntrinsicOp::BitwiseOrAssignment,
                        ast::BinOp::BitwiseXorAssignment => ir::IntrinsicOp::BitwiseXorAssignment,
                        _ => unreachable!(),
                    };
                    let rhs_type = rhs_cast.get_target_type(&mut context.module);
                    let output_type = i.get_return_type(&[lhs_type, rhs_type], &context.module);
                    let node = ir::Expression::IntrinsicOp(i, Vec::from([lhs_ir, rhs_final]));
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

/// Get the type which is used for a binary operator
fn most_significant_non_vector(
    left: ir::TypeId,
    right: ir::TypeId,
    left_location: SourceLocation,
    right_location: SourceLocation,
    module: &mut ir::Module,
) -> TyperResult<ir::TypeId> {
    let left_order = match get_non_vector_conversion_rank(left, module) {
        Some(order) => order,
        None => return Err(TyperError::NumericTypeExpected(left_location)),
    };

    let right_order = match get_non_vector_conversion_rank(right, module) {
        Some(order) => order,
        None => return Err(TyperError::NumericTypeExpected(right_location)),
    };

    if left_order > right_order {
        Ok(left)
    } else {
        Ok(right)
    }
}

/// Get the priority for which side of an operator is more significant
fn get_non_vector_conversion_rank(id: ir::TypeId, module: &mut ir::Module) -> Option<u32> {
    match module.type_registry.get_type_layer(id) {
        ir::TypeLayer::Void => None,
        ir::TypeLayer::Scalar(scalar) => match scalar {
            ir::ScalarType::Bool => Some(1),
            ir::ScalarType::IntLiteral => Some(2),
            ir::ScalarType::Int32 => Some(3),
            ir::ScalarType::UInt32 => Some(4),
            ir::ScalarType::FloatLiteral => Some(5),
            ir::ScalarType::Float16 => Some(6),
            ir::ScalarType::Float32 => Some(7),
            ir::ScalarType::Float64 => Some(8),
        },
        ir::TypeLayer::Vector(..) => panic!("vector not expected"),
        ir::TypeLayer::Matrix(..) => panic!("matrix not expected"),
        ir::TypeLayer::Struct(_) => None,
        ir::TypeLayer::StructTemplate(_) => None,
        // TODO: enum class
        ir::TypeLayer::Enum(_) => Some(0),
        ir::TypeLayer::Object(..) => None,
        ir::TypeLayer::Array(..) => None,
        ir::TypeLayer::TemplateParam(_) => panic!("template type not expected"),
        ir::TypeLayer::Modifier(..) => panic!("modifier not expected"),
    }
}

/// Check if a type is an integer-like type that can be used for bit operators
fn is_integer_or_bool_or_enum(id: ir::TypeId, context: &Context) -> bool {
    match context.module.type_registry.get_type_layer(id) {
        ir::TypeLayer::Scalar(scalar) => matches!(
            scalar,
            ir::ScalarType::Bool
                | ir::ScalarType::IntLiteral
                | ir::ScalarType::Int32
                | ir::ScalarType::UInt32
        ),
        // TODO: enum class
        ir::TypeLayer::Enum(_) => true,
        _ => false,
    }
}

/// Get the vector or matrix dimension for an operator which applies piecewise to components
fn select_vector_rank(
    lhs_tyl: ir::TypeLayer,
    rhs_tyl: ir::TypeLayer,
) -> Result<ir::NumericDimension, ()> {
    let left_dim = ir::NumericDimension::from_parts(lhs_tyl.to_x(), lhs_tyl.to_y());
    let right_dim = ir::NumericDimension::from_parts(rhs_tyl.to_x(), rhs_tyl.to_y());
    let operation_dim = match (left_dim, right_dim) {
        (ir::NumericDimension::Scalar, ir::NumericDimension::Scalar) => left_dim,
        (ir::NumericDimension::Scalar, ir::NumericDimension::Vector(_)) => right_dim,
        (ir::NumericDimension::Vector(_), ir::NumericDimension::Scalar) => left_dim,
        (ir::NumericDimension::Vector(_), ir::NumericDimension::Vector(1)) => left_dim,
        (ir::NumericDimension::Vector(1), ir::NumericDimension::Vector(_)) => right_dim,
        (ir::NumericDimension::Vector(x1), ir::NumericDimension::Vector(x2)) if x1 < x2 => left_dim,
        (ir::NumericDimension::Vector(_), ir::NumericDimension::Vector(_)) => right_dim,
        (ir::NumericDimension::Scalar, ir::NumericDimension::Matrix(_, _)) => right_dim,
        (ir::NumericDimension::Matrix(_, _), ir::NumericDimension::Scalar) => left_dim,
        (ir::NumericDimension::Matrix(x1, y1), ir::NumericDimension::Matrix(x2, y2))
            if x1 == x2 && y1 == y2 =>
        {
            left_dim
        }
        _ => return Err(()),
    };
    Ok(operation_dim)
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
    let lhs_tyl = context.module.type_registry.get_type_layer(lhs_ty_base);
    let rhs_tyl = context.module.type_registry.get_type_layer(rhs_ty_base);
    let lhs_scalar = context.module.type_registry.extract_scalar(lhs_ty_base);
    let rhs_scalar = context.module.type_registry.extract_scalar(rhs_ty_base);

    // Attempt to find best scalar match between match arms
    // This will return None for non-numeric types
    let st = match (lhs_scalar, rhs_scalar) {
        (Some(left_scalar), Some(right_scalar)) => Some(most_sig_scalar(left_scalar, right_scalar)),
        _ => None,
    };

    // Attempt to find best vector match
    // This will return None for non-numeric types
    // This may return None for some combinations of numeric layouts
    let nd = ir::TypeLayer::most_significant_dimension(lhs_tyl, rhs_tyl);

    // Transform the types
    let (lhs_target_tyl, rhs_target_tyl) = match (st, nd) {
        (Some(st), Some(nd)) => {
            let id = context
                .module
                .type_registry
                .register_numeric_type(ir::NumericType {
                    scalar: st,
                    dimension: nd,
                });
            (id, id)
        }
        (Some(st), None) => {
            let left = context
                .module
                .type_registry
                .transform_scalar(lhs_ty_base, st);
            let right = context
                .module
                .type_registry
                .transform_scalar(rhs_ty_base, st);
            (left, right)
        }
        (None, Some(_)) => {
            panic!(
                "internal error: most_sig_scalar failed where most_significant_dimension succeeded"
            )
        }
        (None, None) => (lhs_ty_base, rhs_ty_base),
    };

    let comb_unmodified_ty_id = if lhs_target_tyl == rhs_target_tyl {
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
        .register_type(ir::TypeLayer::Scalar(ir::ScalarType::Bool));

    // Check for vector values in the condition
    {
        let cond_input_base = context.module.type_registry.remove_modifier(cond_ety.0);
        let cond_input_tyl = context.module.type_registry.get_type_layer(cond_input_base);
        let cond_input_is_vector = matches!(
            cond_input_tyl,
            ir::TypeLayer::Vector(..) | ir::TypeLayer::Matrix(..),
        );

        if cond_input_is_vector {
            return Err(TyperError::ShortCircuitingVector(cond_location));
        }
    }

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

/// Parse a set of matrix components
fn read_matrix_subscript(
    type_id: ir::TypeId,
    x: u32,
    y: u32,
    member: &Located<String>,
) -> TyperResult<Vec<ir::MatrixSwizzleSlot>> {
    let mut swizzle_slots = Vec::new();

    let make_err = || {
        Err(TyperError::InvalidSwizzle(
            type_id,
            member.node.clone(),
            member.get_location(),
        ))
    };

    let mut seen_without_m = false;
    let mut seen_with_m = false;

    let mut state = None;
    for c in member.chars() {
        match state {
            None => match c {
                '_' => state = Some((false, None)),
                _ => return make_err(),
            },
            Some((false, None)) if c == 'm' => {
                state = Some((true, None));
            }
            Some((is_m, first_value)) => {
                let l = if first_value.is_none() { x } else { y };
                let component = if is_m {
                    match c {
                        '0' if l >= 1 => ir::ComponentIndex::First,
                        '1' if l >= 2 => ir::ComponentIndex::Second,
                        '2' if l >= 3 => ir::ComponentIndex::Third,
                        '3' if l >= 4 => ir::ComponentIndex::Forth,
                        _ => return make_err(),
                    }
                } else {
                    match c {
                        '1' if l >= 1 => ir::ComponentIndex::First,
                        '2' if l >= 2 => ir::ComponentIndex::Second,
                        '3' if l >= 3 => ir::ComponentIndex::Third,
                        '4' if l >= 4 => ir::ComponentIndex::Forth,
                        _ => return make_err(),
                    }
                };
                match first_value {
                    None => {
                        state = Some((is_m, Some(component)));
                    }
                    Some(first_component) => {
                        // Ensure _ vs _m usage is the same for all slots
                        if is_m {
                            if seen_without_m {
                                return make_err();
                            }
                            seen_with_m = true;
                        } else {
                            if seen_with_m {
                                return make_err();
                            }
                            seen_without_m = true;
                        }

                        swizzle_slots.push(ir::MatrixSwizzleSlot(first_component, component));
                        state = None;
                    }
                }
            }
        }
    }

    if state.is_some() || swizzle_slots.is_empty() || swizzle_slots.len() > 4 {
        return make_err();
    }

    Ok(swizzle_slots)
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
            let tyl_nomod = context.module.type_registry.get_type_layer(ty_nomod);
            let uint_ty = context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::UInt32));

            let index_type = match tyl_nomod {
                ir::TypeLayer::Array(_, _)
                | ir::TypeLayer::Vector(_, _)
                | ir::TypeLayer::Matrix(_, _, _)
                | ir::TypeLayer::Object(
                    ir::ObjectType::Buffer(_)
                    | ir::ObjectType::RWBuffer(_)
                    | ir::ObjectType::StructuredBuffer(_)
                    | ir::ObjectType::RWStructuredBuffer(_)
                    | ir::ObjectType::Texture2DMips(_)
                    | ir::ObjectType::Texture2DArrayMips(_)
                    | ir::ObjectType::Texture3DMips(_),
                ) => uint_ty,
                ir::TypeLayer::Object(
                    ir::ObjectType::Texture2D(_)
                    | ir::ObjectType::Texture2DMipsSlice(_)
                    | ir::ObjectType::RWTexture2D(_),
                ) => context
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::Vector(uint_ty, 2)),
                ir::TypeLayer::Object(
                    ir::ObjectType::Texture2DArray(_)
                    | ir::ObjectType::Texture2DArrayMipsSlice(_)
                    | ir::ObjectType::RWTexture2DArray(_)
                    | ir::ObjectType::Texture3D(_)
                    | ir::ObjectType::Texture3DMipsSlice(_)
                    | ir::ObjectType::RWTexture3D(_),
                ) => context
                    .module
                    .type_registry
                    .register_type(ir::TypeLayer::Vector(uint_ty, 3)),
                _ => return Err(TyperError::ArrayIndexingNonArrayType(array.location)),
            };

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
            let node = ir::Expression::ArraySubscript(array, sub);

            let ety = match get_expression_type(&node, context) {
                Ok(ety) => ety,
                Err(err) => panic!("internal error: type unknown ({err:?}"),
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
                    let member_name = member.identifiers.last().unwrap();

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
                        Ok(StructMemberValue::Variable(ty, id, member_index)) => {
                            let composite = Box::new(composite_ir);
                            let member = ir::Expression::StructMember(composite, id, member_index);
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
                    let vt = ir::get_swizzle_value_type(&swizzle_slots, vt);
                    let ty_unmod = if swizzle_slots.len() == 1 {
                        composite_ty_nomod
                    } else {
                        context
                            .module
                            .type_registry
                            .register_type(ir::TypeLayer::Vector(
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
                    let vt = ir::get_swizzle_value_type(&swizzle_slots, vt);
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
                            .register_type(ir::TypeLayer::Vector(
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
                ir::TypeLayer::Matrix(scalar, x, y) => {
                    let member = match member.try_trivial() {
                        Some(member) => member,
                        None => {
                            return Err(TyperError::MemberDoesNotExist(
                                composite_ty,
                                member.clone(),
                            ))
                        }
                    };

                    let swizzle_slots = read_matrix_subscript(composite_ty, x, y, member)?;
                    let vt = ir::get_matrix_swizzle_value_type(&swizzle_slots, vt);
                    let ty_unmod = if swizzle_slots.len() == 1 {
                        scalar
                    } else {
                        context
                            .module
                            .type_registry
                            .register_type(ir::TypeLayer::Vector(
                                scalar,
                                swizzle_slots.len() as u32,
                            ))
                    };
                    let ty = context
                        .module
                        .type_registry
                        .combine_modifier(ty_unmod, composite_mod);
                    let ety = ExpressionType(ty, vt);
                    let node = ir::Expression::MatrixSwizzle(Box::new(composite_ir), swizzle_slots);
                    Ok(TypedExpression::Value(node, ety))
                }
                ir::TypeLayer::Object(ir::ObjectType::RayDesc) => {
                    let name = match member.try_trivial() {
                        Some(member) => member,
                        None => {
                            return Err(TyperError::MemberDoesNotExist(
                                composite_ty,
                                member.clone(),
                            ))
                        }
                    };

                    if matches!(name.node.as_str(), "Origin" | "TMin" | "Direction" | "TMax") {
                        let composite = Box::new(composite_ir);
                        let node = ir::Expression::ObjectMember(composite, name.node.clone());
                        let ety = match get_expression_type(&node, context) {
                            Ok(ety) => ety,
                            Err(err) => panic!("internal error: type unknown ({err:?}"),
                        };
                        Ok(TypedExpression::Value(node, ety))
                    } else {
                        Err(TyperError::MemberDoesNotExist(composite_ty, member.clone()))
                    }
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
                            let member =
                                ir::Expression::ObjectMember(composite, member.node.clone());
                            let mips_oty = ir::ObjectType::Texture2DMips(ty);
                            let mips_tyl = ir::TypeLayer::Object(mips_oty);
                            let mips_ty = context.module.type_registry.register_type(mips_tyl);
                            return Ok(TypedExpression::Value(member, mips_ty.to_lvalue()));
                        }
                    }

                    // Handle mips mips member explicitly as it is the only member on an intrinsic object type
                    if let ir::ObjectType::Texture2DArray(ty) = object_type {
                        if member.node == "mips" {
                            let composite = Box::new(composite_ir);
                            let member =
                                ir::Expression::ObjectMember(composite, member.node.clone());
                            let mips_oty = ir::ObjectType::Texture2DArrayMips(ty);
                            let mips_tyl = ir::TypeLayer::Object(mips_oty);
                            let mips_ty = context.module.type_registry.register_type(mips_tyl);
                            return Ok(TypedExpression::Value(member, mips_ty.to_lvalue()));
                        }
                    }

                    if let ir::ObjectType::Texture3D(ty) = object_type {
                        if member.node == "mips" {
                            let composite = Box::new(composite_ir);
                            let member =
                                ir::Expression::ObjectMember(composite, member.node.clone());
                            let mips_oty = ir::ObjectType::Texture3DMips(ty);
                            let mips_tyl = ir::TypeLayer::Object(mips_oty);
                            let mips_ty = context.module.type_registry.register_type(mips_tyl);
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
                // Attempt to match special functions
                if let Some(trivial_name) = name.try_trivial() {
                    match trivial_name.as_str() {
                        "assert_type" => {
                            return parse_assert_type(
                                func.get_location(),
                                template_args,
                                args,
                                context,
                            )
                        }
                        "assert_eval" => {
                            return parse_assert_eval(
                                func.get_location(),
                                template_args,
                                args,
                                context,
                            )
                        }
                        _ => {}
                    }
                }

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
        ast::Expression::SizeOf(ref expr_or_ty) => {
            let (ty, loc) = match **expr_or_ty {
                ast::ExpressionOrType::Type(ref ast_ty) => {
                    let ty = parse_type(ast_ty, context)?;
                    (ty, ast_ty.get_location())
                }
                ast::ExpressionOrType::Expression(ref ast_expr) => {
                    let ty = parse_expr(ast_expr, context)?.1 .0;
                    (ty, ast_expr.get_location())
                }
                ast::ExpressionOrType::Either(ref ast_expr, ref ast_ty) => {
                    // TODO: Only try the path that should succeed based on known types
                    if let Ok(ir_ty) = parse_type(ast_ty, context) {
                        (ir_ty, ast_ty.get_location())
                    } else {
                        let ty = parse_expr(ast_expr, context)?.1 .0;
                        (ty, ast_expr.get_location())
                    }
                }
            };

            // Forbid sizeof() on untyped literals
            let ty_nomod = context.module.type_registry.remove_modifier(ty);
            if let Some(scalar) = context.module.type_registry.extract_scalar(ty_nomod) {
                if scalar == ir::ScalarType::IntLiteral {
                    return Err(TyperError::SizeOfHasLiteralType(ty, loc));
                }
            }

            let uint_ty = context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Scalar(ir::ScalarType::UInt32));
            Ok(TypedExpression::Value(
                ir::Expression::SizeOf(ty),
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
            .register_type(ir::TypeLayer::Scalar(target_scalar));
        let target_ty = match expr_tyl {
            ir::TypeLayer::Scalar(_) => target_scalar_ty,
            ir::TypeLayer::Vector(_, x) => context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Vector(target_scalar_ty, x)),
            ir::TypeLayer::Matrix(_, x, y) => context
                .module
                .type_registry
                .register_type(ir::TypeLayer::Matrix(target_scalar_ty, x, y)),
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

/// Process and evaluate an assert_type
fn parse_assert_type(
    call_location: SourceLocation,
    template_args: &[ast::ExpressionOrType],
    args: &[Located<ast::Expression>],
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    if template_args.len() != 1 || args.len() != 1 {
        return Err(TyperError::AssertTypeInvalid(call_location));
    }

    // Process type argument
    let ty = match parse_expression_or_type(&template_args[0], context)? {
        ir::TypeOrConstant::Type(ty) => ty,
        ir::TypeOrConstant::Constant(_) => {
            return Err(TyperError::AssertTypeInvalid(call_location))
        }
    };

    // Process expression argument
    let (expr_ir, expr_ty) = match parse_expr_internal(&args[0], context)? {
        TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
        texp => {
            return Err(TyperError::FunctionPassedToAnotherFunction(
                ErrorType::Unknown,
                texp.to_error_type(),
                args[0].get_location(),
            ))
        }
    };

    // Check the generated expression has the same type as the type argument
    if ty != expr_ty.0 {
        return Err(TyperError::AssertTypeFailed(
            args[0].get_location(),
            ty,
            expr_ty.0,
        ));
    }

    // Return the inner expression as the result for the generated module
    Ok(TypedExpression::Value(expr_ir, expr_ty))
}

/// Process and evaluate an assert_eval
fn parse_assert_eval(
    call_location: SourceLocation,
    template_args: &[ast::ExpressionOrType],
    args: &[Located<ast::Expression>],
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    if template_args.len() > 1 || args.len() != 2 {
        return Err(TyperError::AssertEvalInvalid(call_location));
    }

    // Process type argument if it exists
    let ty = if template_args.is_empty() {
        None
    } else {
        match parse_expression_or_type(&template_args[0], context)? {
            ir::TypeOrConstant::Type(ty) => Some(ty),
            ir::TypeOrConstant::Constant(_) => {
                return Err(TyperError::AssertEvalInvalid(call_location))
            }
        }
    };

    // Process tested expression argument
    let (left_expr_ir, left_expr_ty) = match parse_expr_internal(&args[0], context)? {
        TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
        texp => {
            return Err(TyperError::FunctionPassedToAnotherFunction(
                ErrorType::Unknown,
                texp.to_error_type(),
                args[0].get_location(),
            ))
        }
    };

    // Process reference expression argument
    let (right_expr_ir, right_expr_ty) = match parse_expr_internal(&args[1], context)? {
        TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
        texp => {
            return Err(TyperError::FunctionPassedToAnotherFunction(
                ErrorType::Unknown,
                texp.to_error_type(),
                args[0].get_location(),
            ))
        }
    };

    // Check the generated expression has the same type as the type argument if it exists
    if let Some(ty) = ty {
        if ty != left_expr_ty.0 {
            return Err(TyperError::AssertTypeFailed(
                args[0].get_location(),
                ty,
                left_expr_ty.0,
            ));
        }

        if ty != right_expr_ty.0 {
            return Err(TyperError::AssertTypeFailed(
                args[1].get_location(),
                ty,
                right_expr_ty.0,
            ));
        }
    }

    // Evaluate the left value - which is the value we expect to be generated from a complex expression
    let generated_value = match evaluate_constexpr(&left_expr_ir, &mut context.module) {
        Ok(value) => value,
        Err(_) => {
            return Err(TyperError::ExpressionIsNotConstantExpression(
                args[0].get_location(),
            ))
        }
    };

    // Evaluate the right value - which is the value we expect to be a trivial reference result
    let reference_value = match evaluate_constexpr(&right_expr_ir, &mut context.module) {
        Ok(value) => value,
        Err(_) => {
            return Err(TyperError::ExpressionIsNotConstantExpression(
                args[1].get_location(),
            ))
        }
    };

    // Check the evaluated values are the same and throw an error if not
    if generated_value != reference_value {
        return Err(TyperError::AssertEvalFailed(
            args[0].get_location(),
            reference_value,
            generated_value,
        ));
    }

    // Return the primary expression as the result for the generated module
    // The reference expression is discarded
    Ok(TypedExpression::Value(left_expr_ir, left_expr_ty))
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
                "[{}, {:?}] != [{}, {:?}]: {:?}",
                context.module.get_type_name_short(ty_query.0),
                ty_query.1,
                context.module.get_type_name_short(ty_expected.0),
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
        assert!(ety == expr_ety, "{ety:?} == {expr_ety:?}: {expr_ir:?}");
    }

    Ok((expr_ir, expr_ety))
}

/// Find the type of an expression
fn get_expression_type(
    expression: &ir::Expression,
    context: &mut Context,
) -> TyperResult<ExpressionType> {
    match expression.get_type(&context.module) {
        Ok(ty) => Ok(ty),
        Err(_) => Err(TyperError::InternalError(SourceLocation::UNKNOWN)),
    }
}
