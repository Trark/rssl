use rssl_ir as ir;

/// Evaluate a subset of possible constant expressions
pub fn evaluate_constexpr(
    expr: &ir::Expression,
    module: &mut ir::Module,
) -> Result<ir::Constant, ()> {
    Ok(match *expr {
        ir::Expression::Literal(ref v) => v.clone(),
        ir::Expression::Variable(id) => {
            if let Some(value) = &module
                .variable_registry
                .get_local_variable(id)
                .constexpr_value
            {
                value.clone()
            } else {
                return Err(());
            }
        }
        ir::Expression::Global(id) => {
            if let Some(value) = &module.global_registry[id.0 as usize].constexpr_value {
                value.clone()
            } else {
                return Err(());
            }
        }
        ir::Expression::EnumValue(id) => {
            let value = module.enum_registry.get_enum_value(id);
            ir::Constant::Enum(value.enum_id, Box::new(value.value.clone()))
        }
        ir::Expression::Cast(target, ref inner) => {
            let unmodified = module.type_registry.remove_modifier(target);
            let inner_value = evaluate_constexpr(inner, module)?;
            evaluate_cast(unmodified, inner_value, module)?
        }
        ir::Expression::SizeOf(id) => {
            let unmodified = module.type_registry.remove_modifier(id);

            match module.type_registry.get_type_layer(unmodified) {
                ir::TypeLayer::Scalar(scalar) => match scalar.get_size() {
                    Some(size) => ir::Constant::UInt32(size),
                    None => return Err(()),
                },
                ir::TypeLayer::Enum(enum_id) => {
                    let scalar = module.enum_registry.get_underlying_scalar(enum_id);
                    ir::Constant::UInt32(scalar.get_size().unwrap())
                }
                _ => return Err(()),
            }
        }
        ir::Expression::IntrinsicOp(ref op, _, ref args) => evaluate_operator(op, args, module)?,
        _ => return Err(()),
    })
}

/// Evaluate an operator
fn evaluate_operator(
    op: &ir::IntrinsicOp,
    args: &[ir::Expression],
    module: &mut ir::Module,
) -> Result<ir::Constant, ()> {
    let mut arg_values = Vec::with_capacity(args.len());
    let mut enum_wrap = None;
    for arg in args {
        let value = evaluate_constexpr(arg, module)?;
        // If it is an enum then extract the underlying integer to do the operation
        if let ir::Constant::Enum(enum_id, underlying) = value {
            assert!(enum_wrap.is_none() || enum_wrap == Some(enum_id));
            enum_wrap = Some(enum_id);
            arg_values.push(*underlying);
        } else {
            assert!(enum_wrap.is_none());
            arg_values.push(value);
        }
    }
    let result = match *op {
        ir::IntrinsicOp::PrefixIncrement => match arg_values[0] {
            ir::Constant::Int32(input) => ir::Constant::Int32(input + 1),
            ir::Constant::UInt32(input) => ir::Constant::UInt32(input + 1),
            _ => return Err(()),
        },
        ir::IntrinsicOp::PrefixDecrement => match arg_values[0] {
            ir::Constant::Int32(input) => ir::Constant::Int32(input - 1),
            ir::Constant::UInt32(input) => ir::Constant::UInt32(input - 1),
            _ => return Err(()),
        },
        ir::IntrinsicOp::PostfixIncrement => match arg_values[0] {
            ir::Constant::Int32(input) => ir::Constant::Int32(input + 1),
            ir::Constant::UInt32(input) => ir::Constant::UInt32(input + 1),
            _ => return Err(()),
        },
        ir::IntrinsicOp::PostfixDecrement => match arg_values[0] {
            ir::Constant::Int32(input) => ir::Constant::Int32(input - 1),
            ir::Constant::UInt32(input) => ir::Constant::UInt32(input - 1),
            _ => return Err(()),
        },
        ir::IntrinsicOp::Plus => match arg_values[0] {
            // Enum should have been pre-casted to an integer
            ir::Constant::Enum(_, _) => panic!("unexpected enum type in Plus"),
            // Valid types will just be passing through - all work will be done by implicit casts
            ref value => value.clone(),
        },
        ir::IntrinsicOp::Minus => match arg_values[0] {
            ir::Constant::Int32(input) => ir::Constant::Int32(-input),
            ir::Constant::IntLiteral(input) => ir::Constant::IntLiteral(-input),
            ir::Constant::Float16(input) => ir::Constant::Float16(-input),
            ir::Constant::FloatLiteral(input) => ir::Constant::FloatLiteral(-input),
            ir::Constant::Float32(input) => ir::Constant::Float32(-input),
            ir::Constant::Float64(input) => ir::Constant::Float64(-input),
            // Enum should have been pre-casted to an integer
            ir::Constant::Enum(_, _) => panic!("unexpected enum type in Minus"),
            _ => return Err(()),
        },
        ir::IntrinsicOp::LogicalNot => match arg_values[0] {
            ir::Constant::Bool(input) => ir::Constant::Bool(!input),
            _ => return Err(()),
        },
        ir::IntrinsicOp::BitwiseNot => match arg_values[0] {
            ir::Constant::IntLiteral(input) => ir::Constant::IntLiteral(!input),
            ir::Constant::Int32(input) => ir::Constant::Int32(!input),
            ir::Constant::UInt32(input) => ir::Constant::UInt32(!input),
            _ => panic!("unexpected type in BitwiseNot"),
        },
        ir::IntrinsicOp::Add => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs + rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs + rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs + rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::Subtract => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs - rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs - rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs - rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::Multiply => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs * rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs * rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs * rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::Divide => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(match lhs.checked_div(*rhs) {
                    Some(v) => v,
                    None => return Err(()),
                })
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => {
                ir::Constant::Int32(match lhs.checked_div(*rhs) {
                    Some(v) => v,
                    None => return Err(()),
                })
            }
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(match lhs.checked_div(*rhs) {
                    Some(v) => v,
                    None => return Err(()),
                })
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::Modulus => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                if *rhs == 0 {
                    return Err(());
                }
                ir::Constant::IntLiteral(lhs % rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => {
                if *rhs == 0 {
                    return Err(());
                }
                ir::Constant::Int32(lhs % rhs)
            }
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                if *rhs == 0 {
                    return Err(());
                }
                ir::Constant::UInt32(lhs % rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::LeftShift => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs << rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs << rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs << rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::RightShift => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs >> rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs >> rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs >> rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::BitwiseAnd => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs & rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs & rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs & rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::BitwiseOr => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs | rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs | rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs | rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::BitwiseXor => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::IntLiteral(lhs ^ rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Int32(lhs ^ rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::UInt32(lhs ^ rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::BooleanAnd => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => ir::Constant::Bool(*lhs && *rhs),
            _ => return Err(()),
        },
        ir::IntrinsicOp::BooleanOr => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => ir::Constant::Bool(*lhs || *rhs),
            _ => return Err(()),
        },
        ir::IntrinsicOp::LessThan => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => ir::Constant::Bool(lhs < rhs),
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::Bool(lhs < rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Bool(lhs < rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => ir::Constant::Bool(lhs < rhs),
            (ir::Constant::Int64(lhs), ir::Constant::Int64(rhs)) => ir::Constant::Bool(lhs < rhs),
            (ir::Constant::UInt64(lhs), ir::Constant::UInt64(rhs)) => ir::Constant::Bool(lhs < rhs),
            (ir::Constant::FloatLiteral(lhs), ir::Constant::FloatLiteral(rhs)) => {
                ir::Constant::Bool(lhs < rhs)
            }
            (ir::Constant::Float16(lhs), ir::Constant::Float16(rhs)) => {
                ir::Constant::Bool(lhs < rhs)
            }
            (ir::Constant::Float32(lhs), ir::Constant::Float32(rhs)) => {
                ir::Constant::Bool(lhs < rhs)
            }
            (ir::Constant::Float64(lhs), ir::Constant::Float64(rhs)) => {
                ir::Constant::Bool(lhs < rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::LessEqual => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => ir::Constant::Bool(lhs <= rhs),
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Bool(lhs <= rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            (ir::Constant::Int64(lhs), ir::Constant::Int64(rhs)) => ir::Constant::Bool(lhs <= rhs),
            (ir::Constant::UInt64(lhs), ir::Constant::UInt64(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            (ir::Constant::FloatLiteral(lhs), ir::Constant::FloatLiteral(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            (ir::Constant::Float16(lhs), ir::Constant::Float16(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            (ir::Constant::Float32(lhs), ir::Constant::Float32(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            (ir::Constant::Float64(lhs), ir::Constant::Float64(rhs)) => {
                ir::Constant::Bool(lhs <= rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::GreaterThan => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => ir::Constant::Bool(lhs > rhs),
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::Bool(lhs > rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Bool(lhs > rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => ir::Constant::Bool(lhs > rhs),
            (ir::Constant::Int64(lhs), ir::Constant::Int64(rhs)) => ir::Constant::Bool(lhs > rhs),
            (ir::Constant::UInt64(lhs), ir::Constant::UInt64(rhs)) => ir::Constant::Bool(lhs > rhs),
            (ir::Constant::FloatLiteral(lhs), ir::Constant::FloatLiteral(rhs)) => {
                ir::Constant::Bool(lhs > rhs)
            }
            (ir::Constant::Float16(lhs), ir::Constant::Float16(rhs)) => {
                ir::Constant::Bool(lhs > rhs)
            }
            (ir::Constant::Float32(lhs), ir::Constant::Float32(rhs)) => {
                ir::Constant::Bool(lhs > rhs)
            }
            (ir::Constant::Float64(lhs), ir::Constant::Float64(rhs)) => {
                ir::Constant::Bool(lhs > rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::GreaterEqual => match (&arg_values[0], &arg_values[1]) {
            (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => ir::Constant::Bool(lhs >= rhs),
            (ir::Constant::IntLiteral(lhs), ir::Constant::IntLiteral(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            (ir::Constant::Int32(lhs), ir::Constant::Int32(rhs)) => ir::Constant::Bool(lhs >= rhs),
            (ir::Constant::UInt32(lhs), ir::Constant::UInt32(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            (ir::Constant::Int64(lhs), ir::Constant::Int64(rhs)) => ir::Constant::Bool(lhs >= rhs),
            (ir::Constant::UInt64(lhs), ir::Constant::UInt64(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            (ir::Constant::FloatLiteral(lhs), ir::Constant::FloatLiteral(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            (ir::Constant::Float16(lhs), ir::Constant::Float16(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            (ir::Constant::Float32(lhs), ir::Constant::Float32(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            (ir::Constant::Float64(lhs), ir::Constant::Float64(rhs)) => {
                ir::Constant::Bool(lhs >= rhs)
            }
            _ => return Err(()),
        },
        ir::IntrinsicOp::Equality => ir::Constant::Bool(arg_values[0] == arg_values[1]),
        ir::IntrinsicOp::Inequality => ir::Constant::Bool(arg_values[0] != arg_values[1]),
        _ => return Err(()),
    };

    // Do not wrap in enum if the intrinsic outputs to another type
    if matches!(
        op,
        ir::IntrinsicOp::LessThan
            | ir::IntrinsicOp::LessEqual
            | ir::IntrinsicOp::GreaterThan
            | ir::IntrinsicOp::GreaterEqual
            | ir::IntrinsicOp::Equality
            | ir::IntrinsicOp::Inequality
    ) {
        enum_wrap = None;
    }

    // Wrap back in an enum if required
    Ok(if let Some(enum_id) = enum_wrap {
        ir::Constant::Enum(enum_id, Box::new(result))
    } else {
        result
    })
}

/// Evaluate a cast between a value and a type
fn evaluate_cast(
    ty: ir::TypeId,
    inner_value: ir::Constant,
    module: &mut ir::Module,
) -> Result<ir::Constant, ()> {
    let tyl = module.type_registry.get_type_layer(ty);
    let result = match tyl {
        ir::TypeLayer::Scalar(ir::ScalarType::Bool) => {
            let inner_value = match inner_value {
                ir::Constant::Enum(_, inner) => *inner,
                other => other,
            };
            match inner_value {
                ir::Constant::Bool(v) => ir::Constant::Bool(v),
                ir::Constant::IntLiteral(v) => ir::Constant::Bool(v != 0),
                ir::Constant::Int32(v) => ir::Constant::Bool(v != 0),
                ir::Constant::UInt32(v) => ir::Constant::Bool(v != 0),
                ir::Constant::FloatLiteral(v) => ir::Constant::Bool(v != 0.0),
                ir::Constant::Float16(v) => ir::Constant::Bool(v != 0.0),
                ir::Constant::Float32(v) => ir::Constant::Bool(v != 0.0),
                ir::Constant::Float64(v) => ir::Constant::Bool(v != 0.0),
                ir::Constant::Enum(_, _) => unreachable!(),
                _ => return Err(()),
            }
        }
        ir::TypeLayer::Scalar(ir::ScalarType::Int32) => {
            let inner_value = match inner_value {
                ir::Constant::Enum(_, inner) => *inner,
                other => other,
            };
            match inner_value {
                ir::Constant::Bool(v) => ir::Constant::Int32(i32::from(v)),
                ir::Constant::IntLiteral(v) => ir::Constant::Int32(v as i32),
                ir::Constant::Int32(v) => ir::Constant::Int32(v),
                ir::Constant::UInt32(v) => ir::Constant::Int32(v as i32),
                ir::Constant::FloatLiteral(v) => ir::Constant::Int32(v as i32),
                ir::Constant::Float16(v) => ir::Constant::Int32(v as i32),
                ir::Constant::Float32(v) => ir::Constant::Int32(v as i32),
                ir::Constant::Float64(v) => ir::Constant::Int32(v as i32),
                ir::Constant::Enum(_, _) => unreachable!(),
                _ => return Err(()),
            }
        }
        ir::TypeLayer::Scalar(ir::ScalarType::UInt64) => {
            let inner_value = match inner_value {
                ir::Constant::Enum(_, inner) => *inner,
                other => other,
            };
            match inner_value {
                ir::Constant::Bool(v) => ir::Constant::UInt32(u32::from(v)),
                ir::Constant::IntLiteral(v) => ir::Constant::UInt32(v as u32),
                ir::Constant::Int32(v) => ir::Constant::UInt32(v as u32),
                ir::Constant::UInt32(v) => ir::Constant::UInt32(v),
                ir::Constant::FloatLiteral(v) => ir::Constant::UInt32(v as u32),
                ir::Constant::Float16(v) => ir::Constant::UInt32(v as u32),
                ir::Constant::Float32(v) => ir::Constant::UInt32(v as u32),
                ir::Constant::Float64(v) => ir::Constant::UInt32(v as u32),
                ir::Constant::Enum(_, _) => unreachable!(),
                _ => return Err(()),
            }
        }
        ir::TypeLayer::Scalar(ir::ScalarType::Float16) => {
            let inner_value = match inner_value {
                ir::Constant::Enum(_, inner) => *inner,
                other => other,
            };
            match inner_value {
                ir::Constant::Bool(v) => ir::Constant::Float16(if v { 1.0 } else { 0.0 }),
                ir::Constant::IntLiteral(v) => ir::Constant::Float16(v as f32),
                ir::Constant::Int32(v) => ir::Constant::Float16(v as f32),
                ir::Constant::UInt32(v) => ir::Constant::Float16(v as f32),
                ir::Constant::FloatLiteral(v) => ir::Constant::Float16(v as f32),
                ir::Constant::Float16(v) => ir::Constant::Float16(v),
                ir::Constant::Float32(v) => ir::Constant::Float16(v),
                ir::Constant::Float64(v) => ir::Constant::Float16(v as f32),
                ir::Constant::Enum(_, _) => unreachable!(),
                _ => return Err(()),
            }
        }
        ir::TypeLayer::Scalar(ir::ScalarType::Float32) => {
            let inner_value = match inner_value {
                ir::Constant::Enum(_, inner) => *inner,
                other => other,
            };
            match inner_value {
                ir::Constant::Bool(v) => ir::Constant::Float32(if v { 1.0 } else { 0.0 }),
                ir::Constant::IntLiteral(v) => ir::Constant::Float32(v as f32),
                ir::Constant::Int32(v) => ir::Constant::Float32(v as f32),
                ir::Constant::UInt32(v) => ir::Constant::Float32(v as f32),
                ir::Constant::FloatLiteral(v) => ir::Constant::Float32(v as f32),
                ir::Constant::Float16(v) => ir::Constant::Float32(v),
                ir::Constant::Float32(v) => ir::Constant::Float32(v),
                ir::Constant::Float64(v) => ir::Constant::Float32(v as f32),
                ir::Constant::Enum(_, _) => unreachable!(),
                _ => return Err(()),
            }
        }
        ir::TypeLayer::Scalar(ir::ScalarType::Float64) => {
            let inner_value = match inner_value {
                ir::Constant::Enum(_, inner) => *inner,
                other => other,
            };
            match inner_value {
                ir::Constant::Bool(v) => ir::Constant::Float64(if v { 1.0 } else { 0.0 }),
                ir::Constant::IntLiteral(v) => ir::Constant::Float64(v as f64),
                ir::Constant::Int32(v) => ir::Constant::Float64(v as f64),
                ir::Constant::UInt32(v) => ir::Constant::Float64(v as f64),
                ir::Constant::FloatLiteral(v) => ir::Constant::Float64(v),
                ir::Constant::Float16(v) => ir::Constant::Float64(v as f64),
                ir::Constant::Float32(v) => ir::Constant::Float64(v as f64),
                ir::Constant::Float64(v) => ir::Constant::Float64(v),
                ir::Constant::Enum(_, _) => unreachable!(),
                _ => return Err(()),
            }
        }
        ir::TypeLayer::Enum(enum_id) => {
            let underlying = module.enum_registry.get_underlying_type_id(enum_id);
            let underlying_value = evaluate_cast(underlying, inner_value, module)?;
            ir::Constant::Enum(enum_id, Box::new(underlying_value))
        }
        _ => return Err(()),
    };
    Ok(result)
}
