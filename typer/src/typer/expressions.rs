use super::errors::*;
use super::functions::{Callable, FunctionOverload, FunctionSignature};
use super::scopes::*;
use super::types::{apply_template_type_substitution, parse_type, parse_typelayout};
use crate::casting::{ConversionPriority, ImplicitConversion};
use crate::intrinsics;
use rssl_ast as ast;
use rssl_ir as ir;
use rssl_ir::ExpressionType;
use rssl_ir::ToExpressionType;
use rssl_text::{Located, SourceLocation};

/// Result of a variable query
pub enum VariableExpression {
    Local(ir::VariableRef, ir::Type),
    Member(String, ir::Type),
    Global(ir::GlobalId, ir::Type),
    Constant(ir::ConstantBufferId, String, ir::Type),
    Function(UnresolvedFunction),
    Method(UnresolvedFunction),
}

/// Set of overloaded functions
#[derive(PartialEq, Debug, Clone)]
pub struct UnresolvedFunction {
    pub overloads: Vec<FunctionOverload>,
}

#[derive(PartialEq, Debug, Clone)]
struct UnresolvedMethod {
    object_type: ir::Type,
    overloads: Vec<FunctionOverload>,
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
            }) => ErrorType::Method(object_type.clone(), overloads.clone()),
        }
    }
}

fn parse_variable(name: &str, context: &Context) -> TyperResult<TypedExpression> {
    Ok(match context.find_variable(name)? {
        VariableExpression::Local(var, ty) => {
            TypedExpression::Value(ir::Expression::Variable(var), ty.to_lvalue())
        }
        VariableExpression::Member(name, ty) => {
            TypedExpression::Value(ir::Expression::MemberVariable(name), ty.to_lvalue())
        }
        VariableExpression::Global(id, ty) => {
            TypedExpression::Value(ir::Expression::Global(id), ty.to_lvalue())
        }
        VariableExpression::Constant(id, name, ty) => {
            TypedExpression::Value(ir::Expression::ConstantVariable(id, name), ty.to_lvalue())
        }
        VariableExpression::Function(func) => TypedExpression::Function(func),
        VariableExpression::Method(func) => TypedExpression::MethodInternal(func),
    })
}

fn find_function_type(
    overloads: &Vec<FunctionOverload>,
    template_args: &[Located<ir::Type>],
    param_types: &[ExpressionType],
    call_location: SourceLocation,
) -> TyperResult<(FunctionOverload, Vec<ImplicitConversion>)> {
    use crate::casting::VectorRank;
    fn find_overload_casts(
        overload: &FunctionOverload,
        template_args: &[Located<ir::Type>],
        param_types: &[ExpressionType],
    ) -> Result<(ir::Type, Vec<ImplicitConversion>), ()> {
        let mut overload_casts = Vec::with_capacity(param_types.len());

        let mut signature = overload.1.clone();

        // Resolve template arguments
        // Require user to specify all arguments
        if overload.1.template_params.0 != 0 {
            if overload.1.template_params.0 as usize != template_args.len() {
                // Wrong number of template arguments provided
                return Err(());
            }

            signature = signature.apply_templates(template_args);
        } else if !template_args.is_empty() {
            // Template args given to non template function
            return Err(());
        }

        for (required_type, source_type) in signature.param_types.iter().zip(param_types.iter()) {
            match required_type.2 {
                Some(_) => return Err(()),
                None => {}
            };
            let ety = required_type.clone().into();
            if let Ok(cast) = ImplicitConversion::find(source_type, &ety) {
                overload_casts.push(cast)
            } else {
                return Err(());
            }
        }
        Ok((signature.return_type.return_type, overload_casts))
    }

    let mut casts = Vec::with_capacity(overloads.len());
    for overload in overloads {
        if param_types.len() == overload.1.param_types.len() {
            if let Ok((return_type, param_casts)) =
                find_overload_casts(overload, template_args, param_types)
            {
                let mut overload = overload.clone();
                overload.1.return_type.return_type = return_type;
                casts.push((overload, param_casts))
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
            winning_numeric_casts.push((candidate.clone(), candidate_casts.clone()));
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
            let ambiguous_overloads = casts.iter().map(|c| c.0.clone()).collect::<Vec<_>>();
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

fn apply_casts(casts: Vec<ImplicitConversion>, values: Vec<ir::Expression>) -> Vec<ir::Expression> {
    assert_eq!(casts.len(), values.len());
    values
        .into_iter()
        .enumerate()
        .map(|(index, value)| casts[index].apply(value))
        .collect::<Vec<_>>()
}

fn write_function(
    unresolved: UnresolvedFunction,
    template_args: &[Located<ir::Type>],
    param_types: &[ExpressionType],
    param_values: Vec<ir::Expression>,
    call_location: SourceLocation,
    call_type: ir::CallType,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Find the matching function overload
    let (FunctionOverload(name, FunctionSignature { return_type, .. }), casts) =
        find_function_type(
            &unresolved.overloads,
            template_args,
            param_types,
            call_location,
        )?;
    // Apply implicit casts
    let param_values = apply_casts(casts, param_values);
    let return_type = return_type.return_type.to_rvalue();

    match name {
        Callable::Intrinsic(factory) => Ok(TypedExpression::Value(
            factory.create_intrinsic(template_args, &param_values),
            return_type,
        )),
        Callable::Function(id) => {
            let id = if !template_args.is_empty() {
                // Now we have to make the actual instance of that template
                context.build_function_template(id, template_args)?
            } else {
                id
            };
            // TODO: Call will not need template args if we can encode it all in id
            Ok(TypedExpression::Value(
                ir::Expression::Call(id, call_type, template_args.to_vec(), param_values),
                return_type,
            ))
        }
    }
}

fn write_method(
    unresolved: UnresolvedMethod,
    template_args: &[Located<ir::Type>],
    param_types: &[ExpressionType],
    param_values: Vec<ir::Expression>,
    call_location: SourceLocation,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Find the matching method overload
    let (FunctionOverload(name, FunctionSignature { return_type, .. }), casts) =
        find_function_type(
            &unresolved.overloads,
            template_args,
            param_types,
            call_location,
        )?;
    // Apply implicit casts
    let mut param_values = apply_casts(casts, param_values);
    // Add struct as implied first argument
    param_values.insert(0, unresolved.object_value);
    let return_type = return_type.return_type.to_rvalue();

    match name {
        Callable::Intrinsic(factory) => Ok(TypedExpression::Value(
            factory.create_intrinsic(template_args, &param_values),
            return_type,
        )),
        Callable::Function(id) => {
            let id = if !template_args.is_empty() {
                // Now we have to make the actual instance of that template
                context.build_function_template(id, template_args)?
            } else {
                id
            };
            // TODO: Call will not need template args if we can encode it all in id
            Ok(TypedExpression::Value(
                ir::Expression::Call(
                    id,
                    ir::CallType::MethodExternal,
                    template_args.to_vec(),
                    param_values,
                ),
                return_type,
            ))
        }
    }
}

fn parse_literal(ast: &ast::Literal) -> TypedExpression {
    match ast {
        ast::Literal::Bool(b) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::Bool(*b)),
            ir::Type::bool().to_rvalue(),
        ),
        ast::Literal::UntypedInt(i) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::UntypedInt(*i)),
            ir::Type::from_scalar(ir::ScalarType::UntypedInt).to_rvalue(),
        ),
        ast::Literal::Int(i) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::Int(*i)),
            ir::Type::int().to_rvalue(),
        ),
        ast::Literal::UInt(i) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::UInt(*i)),
            ir::Type::uint().to_rvalue(),
        ),
        ast::Literal::Long(i) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::Long(*i)),
            ir::Type::from_scalar(ir::ScalarType::UntypedInt).to_rvalue(),
        ),
        ast::Literal::Half(f) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::Half(*f)),
            ir::Type::float().to_rvalue(),
        ),
        ast::Literal::Float(f) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::Float(*f)),
            ir::Type::float().to_rvalue(),
        ),
        ast::Literal::Double(f) => TypedExpression::Value(
            ir::Expression::Literal(ir::Literal::Double(*f)),
            ir::Type::double().to_rvalue(),
        ),
    }
}

fn parse_expr_unaryop(
    op: &ast::UnaryOp,
    expr: &ast::Expression,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    match parse_expr_internal(expr, context)? {
        TypedExpression::Value(expr_ir, expr_ty) => {
            fn enforce_increment_type(ety: &ExpressionType, op: &ast::UnaryOp) -> TyperResult<()> {
                match *ety {
                    ExpressionType(_, ir::ValueType::Rvalue) => Err(
                        TyperError::UnaryOperationWrongTypes(op.clone(), ErrorType::Unknown),
                    ),
                    ExpressionType(
                        ir::Type(ir::TypeLayout::Scalar(ir::ScalarType::Bool), _),
                        _,
                    ) => Err(TyperError::UnaryOperationWrongTypes(
                        op.clone(),
                        ErrorType::Unknown,
                    )),
                    ExpressionType(
                        ir::Type(ir::TypeLayout::Vector(ir::ScalarType::Bool, _), _),
                        _,
                    ) => Err(TyperError::UnaryOperationWrongTypes(
                        op.clone(),
                        ErrorType::Unknown,
                    )),
                    ExpressionType(
                        ir::Type(ir::TypeLayout::Matrix(ir::ScalarType::Bool, _, _), _),
                        _,
                    ) => Err(TyperError::UnaryOperationWrongTypes(
                        op.clone(),
                        ErrorType::Unknown,
                    )),
                    _ => Ok(()),
                }
            }
            let (intrinsic, eir, ety) = match *op {
                ast::UnaryOp::PrefixIncrement => {
                    enforce_increment_type(&expr_ty, op)?;
                    (ir::Intrinsic::PrefixIncrement, expr_ir, expr_ty)
                }
                ast::UnaryOp::PrefixDecrement => {
                    enforce_increment_type(&expr_ty, op)?;
                    (ir::Intrinsic::PrefixDecrement, expr_ir, expr_ty)
                }
                ast::UnaryOp::PostfixIncrement => {
                    enforce_increment_type(&expr_ty, op)?;
                    (ir::Intrinsic::PostfixIncrement, expr_ir, expr_ty)
                }
                ast::UnaryOp::PostfixDecrement => {
                    enforce_increment_type(&expr_ty, op)?;
                    (ir::Intrinsic::PostfixDecrement, expr_ir, expr_ty)
                }
                ast::UnaryOp::Plus => (ir::Intrinsic::Plus, expr_ir, expr_ty.0.to_rvalue()),
                ast::UnaryOp::Minus => (ir::Intrinsic::Minus, expr_ir, expr_ty.0.to_rvalue()),
                ast::UnaryOp::LogicalNot => {
                    let ty = match expr_ty.0 {
                        ir::Type(ir::TypeLayout::Scalar(_), _) => {
                            ir::Type::from_layout(ir::TypeLayout::Scalar(ir::ScalarType::Bool))
                        }
                        ir::Type(ir::TypeLayout::Vector(_, x), _) => {
                            ir::Type::from_layout(ir::TypeLayout::Vector(ir::ScalarType::Bool, x))
                        }
                        ir::Type(ir::TypeLayout::Matrix(_, x, y), _) => ir::Type::from_layout(
                            ir::TypeLayout::Matrix(ir::ScalarType::Bool, x, y),
                        ),
                        _ => {
                            return Err(TyperError::UnaryOperationWrongTypes(
                                op.clone(),
                                ErrorType::Unknown,
                            ))
                        }
                    };
                    let ety = ty.to_rvalue();
                    (ir::Intrinsic::LogicalNot, expr_ir, ety)
                }
                ast::UnaryOp::BitwiseNot => match (expr_ty.0).0 {
                    ir::TypeLayout::Scalar(ir::ScalarType::Int)
                    | ir::TypeLayout::Scalar(ir::ScalarType::UInt) => {
                        (ir::Intrinsic::BitwiseNot, expr_ir, expr_ty.0.to_rvalue())
                    }
                    _ => {
                        return Err(TyperError::UnaryOperationWrongTypes(
                            op.clone(),
                            ErrorType::Unknown,
                        ))
                    }
                },
            };
            Ok(TypedExpression::Value(
                ir::Expression::Intrinsic(intrinsic, Vec::new(), Vec::from([eir])),
                ety,
            ))
        }
        _ => Err(TyperError::UnaryOperationWrongTypes(
            op.clone(),
            ErrorType::Unknown,
        )),
    }
}

fn most_sig_scalar(left: &ir::ScalarType, right: &ir::ScalarType) -> ir::ScalarType {
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

    let left = match *left {
        ScalarType::UntypedInt => ScalarType::Int,
        scalar => scalar,
    };
    let right = match *right {
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
    left: &ExpressionType,
    right: &ExpressionType,
) -> TyperResult<(ImplicitConversion, ImplicitConversion, ir::Intrinsic)> {
    use rssl_ir::ScalarType;
    use rssl_ir::Type;

    fn common_real_type(left: &ScalarType, right: &ScalarType) -> Result<ir::ScalarType, ()> {
        Ok(most_sig_scalar(left, right))
    }

    // Calculate the output type from the input type and operation
    fn output_type(left: Type, right: Type, op: &ast::BinOp) -> ir::Intrinsic {
        // Assert input validity
        {
            let ls = left
                .0
                .to_scalar()
                .expect("non-numeric type in binary operation (lhs)");
            let rs = right
                .0
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
                        "hir: non-integer source in bitwise op (lhs)"
                    );
                    assert!(
                        rs == ScalarType::Int || rs == ScalarType::UInt,
                        "hir: non-integer source in bitwise op (rhs)"
                    );
                }
                ast::BinOp::BooleanAnd | ast::BinOp::BooleanOr => {
                    assert!(
                        ls == ScalarType::Bool,
                        "hir: non-boolean source in boolean op (lhs)"
                    );
                    assert!(
                        rs == ScalarType::Bool,
                        "hir: non-boolean source in boolean op (rhs)"
                    );
                }
                _ => {}
            }
        }

        match *op {
            ast::BinOp::Add => ir::Intrinsic::Add,
            ast::BinOp::Subtract => ir::Intrinsic::Subtract,
            ast::BinOp::Multiply => ir::Intrinsic::Multiply,
            ast::BinOp::Divide => ir::Intrinsic::Divide,
            ast::BinOp::Modulus => ir::Intrinsic::Modulus,
            ast::BinOp::LeftShift => ir::Intrinsic::LeftShift,
            ast::BinOp::RightShift => ir::Intrinsic::RightShift,
            ast::BinOp::BitwiseAnd => ir::Intrinsic::BitwiseAnd,
            ast::BinOp::BitwiseOr => ir::Intrinsic::BitwiseOr,
            ast::BinOp::BitwiseXor => ir::Intrinsic::BitwiseXor,
            ast::BinOp::LessThan => ir::Intrinsic::LessThan,
            ast::BinOp::LessEqual => ir::Intrinsic::LessEqual,
            ast::BinOp::GreaterThan => ir::Intrinsic::GreaterThan,
            ast::BinOp::GreaterEqual => ir::Intrinsic::GreaterEqual,
            ast::BinOp::Equality => ir::Intrinsic::Equality,
            ast::BinOp::Inequality => ir::Intrinsic::Inequality,
            ast::BinOp::BooleanAnd => ir::Intrinsic::BooleanAnd,
            ast::BinOp::BooleanOr => ir::Intrinsic::BooleanOr,
            _ => panic!("unexpected binop in resolve_arithmetic_types"),
        }
    }

    fn do_noerror(
        op: &ast::BinOp,
        left: &ExpressionType,
        right: &ExpressionType,
    ) -> Result<(ImplicitConversion, ImplicitConversion, ir::Intrinsic), ()> {
        let &ExpressionType(ir::Type(ref left_l, ref modl), _) = left;
        let &ExpressionType(ir::Type(ref right_l, ref modr), _) = right;
        let (ltl, rtl) = match (left_l, right_l) {
            (&ir::TypeLayout::Scalar(ref ls), &ir::TypeLayout::Scalar(ref rs)) => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_scalar(common_scalar);
                let common_right = common_left.clone();
                (common_left, common_right)
            }
            (&ir::TypeLayout::Scalar(ref ls), &ir::TypeLayout::Vector(ref rs, ref x2)) => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_scalar(common_scalar);
                let common_right = ir::TypeLayout::from_vector(common_scalar, *x2);
                (common_left, common_right)
            }
            (&ir::TypeLayout::Vector(ref ls, ref x1), &ir::TypeLayout::Scalar(ref rs)) => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_vector(common_scalar, *x1);
                let common_right = ir::TypeLayout::from_scalar(common_scalar);
                (common_left, common_right)
            }
            (&ir::TypeLayout::Vector(ref ls, ref x1), &ir::TypeLayout::Vector(ref rs, ref x2))
                if x1 == x2 || *x1 == 1 || *x2 == 1 =>
            {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_vector(common_scalar, *x1);
                let common_right = ir::TypeLayout::from_vector(common_scalar, *x2);
                (common_left, common_right)
            }
            (
                &ir::TypeLayout::Matrix(ref ls, ref x1, ref y1),
                &ir::TypeLayout::Matrix(ref rs, ref x2, ref y2),
            ) if x1 == x2 && y1 == y2 => {
                let common_scalar = common_real_type(ls, rs)?;
                let common_left = ir::TypeLayout::from_matrix(common_scalar, *x2, *y2);
                let common_right = common_left.clone();
                (common_left, common_right)
            }
            _ => return Err(()),
        };
        let out_mod = ir::TypeModifier {
            is_const: false,
            row_order: ir::RowOrder::Column,
            precise: modl.precise || modr.precise,
            volatile: false,
        };
        let candidate_left = Type(ltl.clone(), out_mod);
        let candidate_right = Type(rtl.clone(), out_mod);
        let output_type = output_type(candidate_left, candidate_right, op);
        let elt = ExpressionType(ir::Type(ltl, out_mod), ir::ValueType::Rvalue);
        let lc = ImplicitConversion::find(left, &elt)?;
        let ert = ExpressionType(ir::Type(rtl, out_mod), ir::ValueType::Rvalue);
        let rc = ImplicitConversion::find(right, &ert)?;
        Ok((lc, rc, output_type))
    }

    match do_noerror(binop, left, right) {
        Ok(res) => Ok(res),
        Err(_) => Err(TyperError::BinaryOperationWrongTypes(
            binop.clone(),
            left.to_error_type(),
            right.to_error_type(),
        )),
    }
}

fn parse_expr_binop(
    op: &ast::BinOp,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    let lhs_texp = parse_expr_internal(lhs, context)?;
    let rhs_texp = parse_expr_internal(rhs, context)?;
    let lhs_pt = lhs_texp.to_error_type();
    let rhs_pt = rhs_texp.to_error_type();
    let err_bad_type = Err(TyperError::BinaryOperationWrongTypes(
        op.clone(),
        lhs_pt,
        rhs_pt,
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
                fn is_integer(ety: &ExpressionType) -> bool {
                    let sty = match (ety.0).0.to_scalar() {
                        Some(sty) => sty,
                        None => return false,
                    };
                    sty == ir::ScalarType::Int
                        || sty == ir::ScalarType::UInt
                        || sty == ir::ScalarType::UntypedInt
                }
                if !is_integer(&lhs_type) || !is_integer(&rhs_type) {
                    return err_bad_type;
                }
            }
            let types = resolve_arithmetic_types(op, &lhs_type, &rhs_type)?;
            let (lhs_cast, rhs_cast, output_intrinsic) = types;
            let lhs_final = lhs_cast.apply(lhs_ir);
            let rhs_final = rhs_cast.apply(rhs_ir);
            let output_type = output_intrinsic
                .get_return_type(&[lhs_cast.get_target_type(), rhs_cast.get_target_type()]);
            let node = ir::Expression::Intrinsic(
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
            let lhs_tyl = &(lhs_type.0).0;
            let rhs_tyl = &(rhs_type.0).0;
            let lhs_mod = &(lhs_type.0).1;
            let rhs_mod = &(rhs_type.0).1;
            let scalar = if *op == ast::BinOp::BooleanAnd || *op == ast::BinOp::BooleanOr {
                ir::ScalarType::Bool
            } else {
                let lhs_scalar = lhs_tyl
                    .to_scalar()
                    .ok_or(TyperError::BinaryOperationNonNumericType)?;
                let rhs_scalar = rhs_tyl
                    .to_scalar()
                    .ok_or(TyperError::BinaryOperationNonNumericType)?;
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
            let tyl = ir::TypeLayout::from_numeric(scalar, x, y);
            let out_mod = ir::TypeModifier {
                is_const: false,
                row_order: ir::RowOrder::Column,
                precise: lhs_mod.precise || rhs_mod.precise,
                volatile: false,
            };
            let ty = ir::Type(tyl, out_mod).to_rvalue();
            let lhs_cast = match ImplicitConversion::find(&lhs_type, &ty) {
                Ok(cast) => cast,
                Err(()) => return err_bad_type,
            };
            let rhs_cast = match ImplicitConversion::find(&rhs_type, &ty) {
                Ok(cast) => cast,
                Err(()) => return err_bad_type,
            };
            assert_eq!(lhs_cast.get_target_type(), rhs_cast.get_target_type());
            let lhs_final = lhs_cast.apply(lhs_ir);
            let rhs_final = rhs_cast.apply(rhs_ir);
            let i = match *op {
                ast::BinOp::BitwiseAnd => ir::Intrinsic::BitwiseAnd,
                ast::BinOp::BitwiseOr => ir::Intrinsic::BitwiseOr,
                ast::BinOp::BitwiseXor => ir::Intrinsic::BitwiseXor,
                ast::BinOp::BooleanAnd => ir::Intrinsic::BooleanAnd,
                ast::BinOp::BooleanOr => ir::Intrinsic::BooleanOr,
                _ => unreachable!(),
            };
            let output_type =
                i.get_return_type(&[lhs_cast.get_target_type(), rhs_cast.get_target_type()]);
            let node = ir::Expression::Intrinsic(i, Vec::new(), Vec::from([lhs_final, rhs_final]));
            Ok(TypedExpression::Value(node, output_type))
        }
        ast::BinOp::Assignment
        | ast::BinOp::SumAssignment
        | ast::BinOp::DifferenceAssignment
        | ast::BinOp::ProductAssignment
        | ast::BinOp::QuotientAssignment
        | ast::BinOp::RemainderAssignment => {
            if lhs_type.0.is_const() {
                return Err(TyperError::MutableRequired);
            }
            let required_rtype = match lhs_type.1 {
                ir::ValueType::Lvalue => ExpressionType(lhs_type.0.clone(), ir::ValueType::Rvalue),
                _ => return Err(TyperError::LvalueRequired),
            };
            match ImplicitConversion::find(&rhs_type, &required_rtype) {
                Ok(rhs_cast) => {
                    let rhs_final = rhs_cast.apply(rhs_ir);
                    let i = match *op {
                        ast::BinOp::Assignment => ir::Intrinsic::Assignment,
                        ast::BinOp::SumAssignment => ir::Intrinsic::SumAssignment,
                        ast::BinOp::DifferenceAssignment => ir::Intrinsic::DifferenceAssignment,
                        ast::BinOp::ProductAssignment => ir::Intrinsic::ProductAssignment,
                        ast::BinOp::QuotientAssignment => ir::Intrinsic::QuotientAssignment,
                        ast::BinOp::RemainderAssignment => ir::Intrinsic::RemainderAssignment,
                        _ => unreachable!(),
                    };
                    let output_type = i.get_return_type(&[lhs_type, rhs_cast.get_target_type()]);
                    let node =
                        ir::Expression::Intrinsic(i, Vec::new(), Vec::from([lhs_ir, rhs_final]));
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
    cond: &ast::Expression,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    context: &mut Context,
) -> TyperResult<TypedExpression> {
    // Generate sub expressions
    let (cond, cond_ety) = parse_expr_value_only(cond, context)?;
    let (lhs, lhs_ety) = parse_expr_value_only(lhs, context)?;
    let (rhs, rhs_ety) = parse_expr_value_only(rhs, context)?;

    let ExpressionType(lhs_ty, _) = lhs_ety.clone();
    let ExpressionType(rhs_ty, _) = rhs_ety.clone();
    let wrong_types_err = Err(TyperError::TernaryArmsMustHaveSameType(
        lhs_ty.to_error_type(),
        rhs_ty.to_error_type(),
    ));
    let ir::Type(lhs_tyl, lhs_mod) = lhs_ty;
    let ir::Type(rhs_tyl, rhs_mod) = rhs_ty;

    // Attempt to find best scalar match between match arms
    // This will return None for non-numeric types
    let st = match (lhs_tyl.to_scalar(), rhs_tyl.to_scalar()) {
        (Some(left_scalar), Some(right_scalar)) => {
            Some(most_sig_scalar(&left_scalar, &right_scalar))
        }
        _ => None,
    };

    // Attempt to find best vector match
    // This will return None for non-numeric types
    // This may return None for some combinations of numeric layouts
    let nd = ir::TypeLayout::most_significant_dimension(&lhs_tyl, &rhs_tyl);

    // Transform the types
    let (lhs_target_tyl, rhs_target_tyl) = match (st, nd) {
        (Some(st), Some(nd)) => {
            let dtyl = ir::DataLayout::new(st, nd);
            let tyl = ir::TypeLayout::from_data(dtyl);
            (tyl.clone(), tyl)
        }
        (Some(st), None) => {
            let left = lhs_tyl.transform_scalar(st);
            let right = rhs_tyl.transform_scalar(st);
            (left, right)
        }
        (None, Some(_)) => {
            panic!(
                "internal error: most_sig_scalar failed where most_significant_dimension succeeded"
            )
        }
        (None, None) => (lhs_tyl, rhs_tyl),
    };

    let comb_tyl = if lhs_target_tyl == rhs_target_tyl {
        lhs_target_tyl
    } else {
        return wrong_types_err;
    };

    let target_mod = ir::TypeModifier {
        is_const: false,
        row_order: lhs_mod.row_order, // TODO: ???
        precise: lhs_mod.precise || rhs_mod.precise,
        volatile: false,
    };

    let ety_target = ir::Type(comb_tyl, target_mod).to_rvalue();

    let left_cast = match ImplicitConversion::find(&lhs_ety, &ety_target) {
        Ok(cast) => cast,
        Err(()) => return wrong_types_err,
    };
    let right_cast = match ImplicitConversion::find(&rhs_ety, &ety_target) {
        Ok(cast) => cast,
        Err(()) => return wrong_types_err,
    };

    let lhs_casted = Box::new(left_cast.apply(lhs));
    let rhs_casted = Box::new(right_cast.apply(rhs));
    assert_eq!(left_cast.get_target_type(), right_cast.get_target_type());
    let final_type = left_cast.get_target_type();

    // Cast the condition
    let cond_cast = match ImplicitConversion::find(&cond_ety, &ir::Type::bool().to_rvalue()) {
        Ok(cast) => cast,
        Err(()) => {
            return Err(TyperError::TernaryConditionRequiresBoolean(
                cond_ety.to_error_type(),
            ))
        }
    };
    let cond_casted = Box::new(cond_cast.apply(cond));

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
        ast::Expression::Literal(ref lit) => Ok(parse_literal(lit)),
        ast::Expression::Variable(ref s) => parse_variable(s, context),
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
                _ => return Err(TyperError::ArrayIndexingNonArrayType),
            };
            let (subscript_ir, subscript_ty) = match subscript_texp {
                TypedExpression::Value(subscript_ir, subscript_ty) => (subscript_ir, subscript_ty),
                _ => return Err(TyperError::ArrayIndexingNonArrayType),
            };
            let ExpressionType(ir::Type(array_tyl, _), _) = array_ty;
            let node = match array_tyl {
                ir::TypeLayout::Array(_, _)
                | ir::TypeLayout::Object(ir::ObjectType::Buffer(_))
                | ir::TypeLayout::Object(ir::ObjectType::RWBuffer(_))
                | ir::TypeLayout::Object(ir::ObjectType::StructuredBuffer(_))
                | ir::TypeLayout::Object(ir::ObjectType::RWStructuredBuffer(_)) => {
                    let index = ir::Type::uint().to_rvalue();
                    let cast_to_int_result = ImplicitConversion::find(&subscript_ty, &index);
                    let subscript_final = match cast_to_int_result {
                        Err(_) => return Err(TyperError::ArraySubscriptIndexNotInteger),
                        Ok(cast) => cast.apply(subscript_ir),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                ir::TypeLayout::Object(ir::ObjectType::Texture2D(_)) => {
                    let index = ir::Type::uintn(2).to_rvalue();
                    let cast = ImplicitConversion::find(&subscript_ty, &index);
                    let subscript_final = match cast {
                        Err(_) => return Err(TyperError::ArraySubscriptIndexNotInteger),
                        Ok(cast) => cast.apply(subscript_ir),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(_)) => {
                    let index = ir::Type::uintn(2).to_rvalue();
                    let cast = ImplicitConversion::find(&subscript_ty, &index);
                    let subscript_final = match cast {
                        Err(_) => return Err(TyperError::ArraySubscriptIndexNotInteger),
                        Ok(cast) => cast.apply(subscript_ir),
                    };
                    let array = Box::new(array_ir);
                    let sub = Box::new(subscript_final);
                    let sub_node = ir::Expression::ArraySubscript(array, sub);
                    Ok(sub_node)
                }
                _ => Err(TyperError::ArrayIndexingNonArrayType),
            }?;
            let ety = match get_expression_type(&node, context) {
                Ok(ety) => ety,
                Err(err) => panic!("internal error: type unknown ({:?}", err),
            };
            Ok(TypedExpression::Value(node, ety))
        }
        ast::Expression::Member(ref composite, ref member) => {
            let composite_texp = parse_expr_internal(composite, context)?;
            let composite_pt = composite_texp.to_error_type();
            let (composite_ir, composite_ty) = match composite_texp {
                TypedExpression::Value(composite_ir, composite_type) => {
                    (composite_ir, composite_type)
                }
                _ => return Err(TyperError::TypeDoesNotHaveMembers(composite_pt)),
            };
            let ExpressionType(ir::Type(composite_tyl, composite_mod), vt) = composite_ty;
            match composite_tyl {
                ir::TypeLayout::Struct(id)
                | ir::TypeLayout::Object(ir::ObjectType::ConstantBuffer(ir::StructuredType(
                    ir::StructuredLayout::Struct(id),
                    _,
                ))) => match context.get_struct_member_expression(id, member) {
                    Ok(StructMemberValue::Variable(ty)) => {
                        let composite = Box::new(composite_ir);
                        let member = ir::Expression::Member(composite, member.clone());
                        Ok(TypedExpression::Value(member, ty.to_lvalue()))
                    }
                    Ok(StructMemberValue::Method(overloads)) => {
                        Ok(TypedExpression::Method(UnresolvedMethod {
                            object_type: ir::Type(composite_tyl, composite_mod),
                            overloads,
                            object_value: composite_ir,
                        }))
                    }
                    Err(err) => Err(err),
                },
                ir::TypeLayout::Scalar(scalar) => {
                    let mut swizzle_slots = Vec::with_capacity(member.len());
                    for c in member.chars() {
                        swizzle_slots.push(match c {
                            'x' | 'r' => ir::SwizzleSlot::X,
                            // Assume we were not trying to swizzle and accidentally accessed a member of a scalar
                            _ => return Err(TyperError::TypeDoesNotHaveMembers(composite_pt)),
                        });
                    }
                    let vt = get_swizzle_vt(&swizzle_slots, vt);
                    let ty = if swizzle_slots.len() == 1 {
                        ir::TypeLayout::Scalar(scalar)
                    } else {
                        ir::TypeLayout::Vector(scalar, swizzle_slots.len() as u32)
                    };
                    let ety = ExpressionType(ir::Type(ty, composite_mod), vt);
                    let node = ir::Expression::Swizzle(Box::new(composite_ir), swizzle_slots);
                    Ok(TypedExpression::Value(node, ety))
                }
                ir::TypeLayout::Vector(scalar, x) => {
                    let mut swizzle_slots = Vec::with_capacity(member.len());
                    for c in member.chars() {
                        swizzle_slots.push(match c {
                            'x' | 'r' if x >= 1 => ir::SwizzleSlot::X,
                            'y' | 'g' if x >= 2 => ir::SwizzleSlot::Y,
                            'z' | 'b' if x >= 3 => ir::SwizzleSlot::Z,
                            'w' | 'a' if x >= 4 => ir::SwizzleSlot::W,
                            _ => {
                                return Err(TyperError::InvalidSwizzle(
                                    composite_pt,
                                    member.clone(),
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
                    let ty = if swizzle_slots.len() == 1 {
                        ir::TypeLayout::Scalar(scalar)
                    } else {
                        ir::TypeLayout::Vector(scalar, swizzle_slots.len() as u32)
                    };
                    let ety = ExpressionType(ir::Type(ty, composite_mod), vt);
                    let node = ir::Expression::Swizzle(Box::new(composite_ir), swizzle_slots);
                    Ok(TypedExpression::Value(node, ety))
                }
                ir::TypeLayout::Object(ref object_type) => {
                    match intrinsics::get_method(object_type, member) {
                        Ok(intrinsics::MethodDefinition(object_type, _, method_overloads)) => {
                            let ty = ir::Type::from_object(object_type);
                            let overloads = method_overloads
                                .iter()
                                .map(|&(template_types, ref param_types, ref factory)| {
                                    let return_type = ir::FunctionReturn {
                                        return_type: factory
                                            .get_method_return_type(ty.clone().to_rvalue())
                                            .0,
                                    };
                                    FunctionOverload(
                                        Callable::Intrinsic(factory.clone()),
                                        FunctionSignature {
                                            return_type,
                                            template_params: template_types,
                                            param_types: param_types.clone(),
                                        },
                                    )
                                })
                                .collect::<Vec<_>>();
                            Ok(TypedExpression::Method(UnresolvedMethod {
                                object_type: ty,
                                overloads,
                                object_value: composite_ir,
                            }))
                        }
                        Err(()) => Err(TyperError::UnknownTypeMember(composite_pt, member.clone())),
                    }
                }
                // Todo: Matrix components + Object members
                _ => Err(TyperError::TypeDoesNotHaveMembers(composite_pt)),
            }
        }
        ast::Expression::Call(ref func, ref template_args, ref args) => {
            let func_texp = parse_expr_internal(func, context)?;

            // Process template arguments
            let mut template_args_ir = Vec::new();
            for template_arg in template_args {
                let ty = parse_type(&template_arg.node, context)?;
                template_args_ir.push(Located::new(ty, template_arg.location));
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
                _ => Err(TyperError::CallOnNonFunction),
            }
        }
        ast::Expression::Constructor(ref ast_layout, ref params) => {
            let target_scalar = match ast_layout {
                ast::TypeLayout::Scalar(st)
                | ast::TypeLayout::Vector(st, _)
                | ast::TypeLayout::Matrix(st, _, _) => *st,
                _ => return Err(TyperError::ConstructorWrongArgumentCount),
            };
            let mut slots: Vec<ir::ConstructorSlot> = vec![];
            let mut total_arity = 0;
            for param in params {
                let expr_texp = parse_expr_internal(param, context)?;
                let (expr_base, ety) = match expr_texp {
                    TypedExpression::Value(expr_ir, expr_ty) => (expr_ir, expr_ty),
                    _ => return Err(TyperError::FunctionNotCalled),
                };
                let &ExpressionType(ir::Type(ref expr_tyl, _), _) = &ety;
                let arity = expr_tyl.get_num_elements();
                total_arity += arity;
                let s = target_scalar;
                let target_tyl = match *expr_tyl {
                    ir::TypeLayout::Scalar(_) => ir::TypeLayout::Scalar(s),
                    ir::TypeLayout::Vector(_, ref x) => ir::TypeLayout::Vector(s, *x),
                    ir::TypeLayout::Matrix(_, ref x, ref y) => ir::TypeLayout::Matrix(s, *x, *y),
                    _ => return Err(TyperError::WrongTypeInConstructor),
                };
                let target_type = ir::Type::from_layout(target_tyl).to_rvalue();
                let cast = match ImplicitConversion::find(&ety, &target_type) {
                    Ok(cast) => cast,
                    Err(()) => return Err(TyperError::WrongTypeInConstructor),
                };
                let expr = cast.apply(expr_base);
                slots.push(ir::ConstructorSlot { arity, expr });
            }
            let ty = parse_typelayout(ast_layout, context).expect("Data layouts should never fail");
            let expected_layout = ty.0.get_num_elements();
            let ety = ty.to_rvalue();
            if total_arity == expected_layout {
                let cons = ir::Expression::Constructor(ety.0 .0.clone(), slots);
                Ok(TypedExpression::Value(cons, ety))
            } else {
                Err(TyperError::ConstructorWrongArgumentCount)
            }
        }
        ast::Expression::Cast(ref ty, ref expr) => {
            let expr_texp = parse_expr_internal(expr, context)?;
            let expr_pt = expr_texp.to_error_type();
            match expr_texp {
                TypedExpression::Value(expr_ir, _) => {
                    let ir_type = parse_type(ty, context)?;
                    Ok(TypedExpression::Value(
                        ir::Expression::Cast(ir_type.clone(), Box::new(expr_ir)),
                        ir_type.to_rvalue(),
                    ))
                }
                _ => Err(TyperError::InvalidCast(
                    expr_pt,
                    ErrorType::Untyped(ty.node.clone()),
                )),
            }
        }
        ast::Expression::SizeOf(ref ty) => {
            let ir_type = parse_type(ty, context)?;
            Ok(TypedExpression::Value(
                ir::Expression::SizeOf(ir_type),
                ir::Type::uint().to_rvalue(),
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
                    if context.find_type_id(type_name, &[]).is_err() {
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
            let ty_res = get_expression_type(expr, context);
            let ty = ty_res.expect("type unknown");
            assert!(
                ty == *ty_expected,
                "{:?} == {:?}: {:?}",
                ty,
                *ty_expected,
                expr
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
        TypedExpression::Function(_) => Err(TyperError::FunctionNotCalled),
        TypedExpression::MethodInternal(_) => Err(TyperError::FunctionNotCalled),
        TypedExpression::Method(_) => Err(TyperError::FunctionNotCalled),
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

/// Find the type of a literal
fn get_literal_type(literal: &ir::Literal) -> ExpressionType {
    (match *literal {
        ir::Literal::Bool(_) => ir::Type::bool(),
        ir::Literal::UntypedInt(_) => ir::Type::from_scalar(ir::ScalarType::UntypedInt),
        ir::Literal::Int(_) => ir::Type::int(),
        ir::Literal::UInt(_) => ir::Type::uint(),
        ir::Literal::Long(_) => unimplemented!(),
        ir::Literal::Half(_) => ir::Type::from_scalar(ir::ScalarType::Half),
        ir::Literal::Float(_) => ir::Type::float(),
        ir::Literal::Double(_) => ir::Type::double(),
    })
    .to_rvalue()
}

/// Find the type of an expression
fn get_expression_type(
    expression: &ir::Expression,
    context: &Context,
) -> TyperResult<ExpressionType> {
    match *expression {
        ir::Expression::Literal(ref lit) => Ok(get_literal_type(lit)),
        ir::Expression::Variable(var_ref) => context.get_type_of_variable(var_ref),
        ir::Expression::MemberVariable(ref name) => {
            let struct_id = context.get_current_owning_struct();
            context.get_type_of_struct_member(struct_id, name)
        }
        ir::Expression::Global(id) => context.get_type_of_global(id),
        ir::Expression::ConstantVariable(id, ref name) => context.get_type_of_constant(id, name),
        ir::Expression::TernaryConditional(_, ref expr_left, ref expr_right) => {
            // Ensure the layouts of each side are the same
            // Value types + modifiers can be different
            assert_eq!(
                (get_expression_type(expr_left, context)?.0).0,
                (get_expression_type(expr_right, context)?.0).0
            );
            let ety = get_expression_type(expr_left, context)?;
            Ok(ety.0.to_rvalue())
        }
        ir::Expression::Sequence(ref chain) => {
            let last = chain
                .last()
                .expect("Sequence must have at least one expression");
            let ety = get_expression_type(last, context)?;
            Ok(ety.0.to_rvalue())
        }
        ir::Expression::Swizzle(ref vec, ref swizzle) => {
            let ExpressionType(ir::Type(vec_tyl, vec_mod), vec_vt) =
                get_expression_type(vec, context)?;
            let vt = get_swizzle_vt(swizzle, vec_vt);
            let tyl = match vec_tyl {
                ir::TypeLayout::Scalar(scalar) | ir::TypeLayout::Vector(scalar, _) => {
                    if swizzle.len() == 1 {
                        ir::TypeLayout::Scalar(scalar)
                    } else {
                        ir::TypeLayout::Vector(scalar, swizzle.len() as u32)
                    }
                }
                _ => return Err(TyperError::InvalidTypeForSwizzle(vec_tyl)),
            };
            Ok(ExpressionType(ir::Type(tyl, vec_mod), vt))
        }
        ir::Expression::ArraySubscript(ref array, _) => {
            let array_ty = get_expression_type(array, context)?;
            // Todo: Modifiers on object type template parameters
            Ok(match (array_ty.0).0 {
                ir::TypeLayout::Array(ref element, _) => {
                    ir::Type::from_layout(*element.clone()).to_lvalue()
                }
                ir::TypeLayout::Object(ir::ObjectType::Buffer(data_type)) => {
                    ir::Type::from_data(data_type.as_const()).to_lvalue()
                }
                ir::TypeLayout::Object(ir::ObjectType::RWBuffer(data_type)) => {
                    ir::Type::from_data(data_type).to_lvalue()
                }
                ir::TypeLayout::Object(ir::ObjectType::StructuredBuffer(structured_type)) => {
                    ir::Type::from_structured(structured_type.as_const()).to_lvalue()
                }
                ir::TypeLayout::Object(ir::ObjectType::RWStructuredBuffer(structured_type)) => {
                    ir::Type::from_structured(structured_type).to_lvalue()
                }
                ir::TypeLayout::Object(ir::ObjectType::Texture2D(data_type)) => {
                    ir::Type::from_data(data_type.as_const()).to_lvalue()
                }
                ir::TypeLayout::Object(ir::ObjectType::RWTexture2D(data_type)) => {
                    ir::Type::from_data(data_type).to_lvalue()
                }
                tyl => return Err(TyperError::ArrayIndexMustBeUsedOnArrayType(tyl)),
            })
        }
        ir::Expression::Member(ref expr, ref name) => {
            let expr_type = get_expression_type(expr, context)?;
            let id = match (expr_type.0).0 {
                ir::TypeLayout::Struct(id) => id,
                ir::TypeLayout::Object(ir::ObjectType::ConstantBuffer(ir::StructuredType(
                    ir::StructuredLayout::Struct(id),
                    _,
                ))) => id,
                tyl => return Err(TyperError::MemberNodeMustBeUsedOnStruct(tyl, name.clone())),
            };
            context.get_type_of_struct_member(id, name)
        }
        ir::Expression::Call(id, _, ref template_args, _) => {
            context.get_type_of_function_return(id, template_args)
        }
        ir::Expression::Constructor(ref tyl, _) => {
            Ok(ir::Type::from_layout(tyl.clone()).to_rvalue())
        }
        ir::Expression::Cast(ref ty, _) => Ok(ty.to_rvalue()),
        ir::Expression::SizeOf(_) => Ok(ir::Type::uint().to_rvalue()),
        ir::Expression::Intrinsic(ref intrinsic, ref template_args, ref args) => {
            let mut arg_types = Vec::with_capacity(args.len());
            for arg in args {
                arg_types.push(get_expression_type(arg, context)?);
            }
            let mut ety = intrinsic.get_return_type(&arg_types);
            ety.0 = apply_template_type_substitution(ety.0, template_args);
            Ok(ety)
        }
    }
}
