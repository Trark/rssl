use rssl_ir as ir;

/// Evaluate a subset of possible constant expressions
pub fn evaluate_constexpr(
    expr: &ir::Expression,
    module: &mut ir::Module,
) -> Result<ir::Constant, ()> {
    Ok(match *expr {
        ir::Expression::Literal(ref v) => v.clone(),
        ir::Expression::IntrinsicOp(ref op, _, ref args) => {
            let mut arg_values = Vec::with_capacity(args.len());
            for arg in args {
                arg_values.push(evaluate_constexpr(arg, module)?);
            }
            match *op {
                ir::IntrinsicOp::PrefixIncrement => match arg_values[0] {
                    ir::Constant::Int(input) => ir::Constant::Int(input + 1),
                    ir::Constant::UInt(input) => ir::Constant::UInt(input + 1),
                    _ => return Err(()),
                },
                ir::IntrinsicOp::PrefixDecrement => match arg_values[0] {
                    ir::Constant::Int(input) => ir::Constant::Int(input - 1),
                    ir::Constant::UInt(input) => ir::Constant::UInt(input - 1),
                    _ => return Err(()),
                },
                ir::IntrinsicOp::PostfixIncrement => match arg_values[0] {
                    ir::Constant::Int(input) => ir::Constant::Int(input + 1),
                    ir::Constant::UInt(input) => ir::Constant::UInt(input + 1),
                    _ => return Err(()),
                },
                ir::IntrinsicOp::PostfixDecrement => match arg_values[0] {
                    ir::Constant::Int(input) => ir::Constant::Int(input - 1),
                    ir::Constant::UInt(input) => ir::Constant::UInt(input - 1),
                    _ => return Err(()),
                },
                ir::IntrinsicOp::Plus => match arg_values[0] {
                    // Enum should have been pre-casted to an integer
                    ir::Constant::Enum(_, _) => panic!("unexpected enum type in Plus"),
                    // Valid types will just be passing through - all work will be done by implicit casts
                    ref value => value.clone(),
                },
                ir::IntrinsicOp::Minus => match arg_values[0] {
                    ir::Constant::Int(input) => ir::Constant::Int(-input),
                    ir::Constant::UntypedInt(input) => ir::Constant::UntypedInt(-input),
                    ir::Constant::Half(input) => ir::Constant::Half(-input),
                    ir::Constant::Float(input) => ir::Constant::Float(-input),
                    ir::Constant::Double(input) => ir::Constant::Double(-input),
                    // Enum should have been pre-casted to an integer
                    ir::Constant::Enum(_, _) => panic!("unexpected enum type in Minus"),
                    _ => return Err(()),
                },
                ir::IntrinsicOp::LogicalNot => match arg_values[0] {
                    ir::Constant::Bool(input) => ir::Constant::Bool(!input),
                    _ => return Err(()),
                },
                ir::IntrinsicOp::BitwiseNot => match arg_values[0] {
                    ir::Constant::Int(input) => ir::Constant::Int(!input),
                    ir::Constant::UInt(input) => ir::Constant::UInt(!input),
                    _ => panic!("unexpected type in BitwiseNot"),
                },
                ir::IntrinsicOp::Add => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs + rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs + rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::Subtract => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs - rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs - rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::Multiply => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs * rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs * rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::Divide => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(match lhs.checked_div(*rhs) {
                            Some(v) => v,
                            None => return Err(()),
                        })
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(match lhs.checked_div(*rhs) {
                            Some(v) => v,
                            None => return Err(()),
                        })
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::Modulus => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        if *rhs == 0 {
                            return Err(());
                        }
                        ir::Constant::Int(lhs % rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        if *rhs == 0 {
                            return Err(());
                        }
                        ir::Constant::UInt(lhs % rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::LeftShift => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs << rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs << rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::RightShift => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs >> rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs >> rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::BitwiseAnd => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs & rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs & rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::BitwiseOr => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs | rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs | rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::BitwiseXor => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Int(lhs ^ rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::UInt(lhs ^ rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::BooleanAnd => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => {
                        ir::Constant::Bool(*lhs && *rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::BooleanOr => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => {
                        ir::Constant::Bool(*lhs || *rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::LessThan => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::UntypedInt(lhs), ir::Constant::UntypedInt(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::Long(lhs), ir::Constant::Long(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::Half(lhs), ir::Constant::Half(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::Float(lhs), ir::Constant::Float(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    (ir::Constant::Double(lhs), ir::Constant::Double(rhs)) => {
                        ir::Constant::Bool(lhs < rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::LessEqual => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::UntypedInt(lhs), ir::Constant::UntypedInt(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::Long(lhs), ir::Constant::Long(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::Half(lhs), ir::Constant::Half(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::Float(lhs), ir::Constant::Float(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    (ir::Constant::Double(lhs), ir::Constant::Double(rhs)) => {
                        ir::Constant::Bool(lhs <= rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::GreaterThan => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::UntypedInt(lhs), ir::Constant::UntypedInt(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::Long(lhs), ir::Constant::Long(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::Half(lhs), ir::Constant::Half(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::Float(lhs), ir::Constant::Float(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    (ir::Constant::Double(lhs), ir::Constant::Double(rhs)) => {
                        ir::Constant::Bool(lhs > rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::GreaterEqual => match (&arg_values[0], &arg_values[1]) {
                    (ir::Constant::Bool(lhs), ir::Constant::Bool(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::UntypedInt(lhs), ir::Constant::UntypedInt(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::Int(lhs), ir::Constant::Int(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::UInt(lhs), ir::Constant::UInt(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::Long(lhs), ir::Constant::Long(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::Half(lhs), ir::Constant::Half(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::Float(lhs), ir::Constant::Float(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    (ir::Constant::Double(lhs), ir::Constant::Double(rhs)) => {
                        ir::Constant::Bool(lhs >= rhs)
                    }
                    _ => return Err(()),
                },
                ir::IntrinsicOp::Equality => ir::Constant::Bool(arg_values[0] == arg_values[1]),
                ir::IntrinsicOp::Inequality => ir::Constant::Bool(arg_values[0] != arg_values[1]),
                _ => return Err(()),
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
            let tyl = module.type_registry.get_type_layer(unmodified);
            let inner_value = evaluate_constexpr(inner, module)?;
            match tyl {
                ir::TypeLayer::Scalar(ir::ScalarType::Bool) => {
                    let inner_value = match inner_value {
                        ir::Constant::Enum(_, inner) => *inner,
                        other => other,
                    };
                    match inner_value {
                        ir::Constant::Bool(v) => ir::Constant::Bool(v),
                        ir::Constant::UntypedInt(v) => ir::Constant::Bool(v != 0),
                        ir::Constant::Int(v) => ir::Constant::Bool(v != 0),
                        ir::Constant::UInt(v) => ir::Constant::Bool(v != 0),
                        ir::Constant::Half(v) => ir::Constant::Bool(v != 0.0),
                        ir::Constant::Float(v) => ir::Constant::Bool(v != 0.0),
                        ir::Constant::Double(v) => ir::Constant::Bool(v != 0.0),
                        ir::Constant::Enum(_, _) => unreachable!(),
                        _ => return Err(()),
                    }
                }
                ir::TypeLayer::Scalar(ir::ScalarType::Int) => {
                    let inner_value = match inner_value {
                        ir::Constant::Enum(_, inner) => *inner,
                        other => other,
                    };
                    match inner_value {
                        ir::Constant::Bool(v) => ir::Constant::Int(i32::from(v)),
                        ir::Constant::UntypedInt(v) => ir::Constant::Int(v as i32),
                        ir::Constant::Int(v) => ir::Constant::Int(v),
                        ir::Constant::UInt(v) => ir::Constant::Int(v as i32),
                        ir::Constant::Half(v) => ir::Constant::Int(v as i32),
                        ir::Constant::Float(v) => ir::Constant::Int(v as i32),
                        ir::Constant::Double(v) => ir::Constant::Int(v as i32),
                        ir::Constant::Enum(_, _) => unreachable!(),
                        _ => return Err(()),
                    }
                }
                ir::TypeLayer::Scalar(ir::ScalarType::UInt) => {
                    let inner_value = match inner_value {
                        ir::Constant::Enum(_, inner) => *inner,
                        other => other,
                    };
                    match inner_value {
                        ir::Constant::Bool(v) => ir::Constant::UInt(u32::from(v)),
                        ir::Constant::UntypedInt(v) => ir::Constant::UInt(v as u32),
                        ir::Constant::Int(v) => ir::Constant::UInt(v as u32),
                        ir::Constant::UInt(v) => ir::Constant::UInt(v),
                        ir::Constant::Half(v) => ir::Constant::UInt(v as u32),
                        ir::Constant::Float(v) => ir::Constant::UInt(v as u32),
                        ir::Constant::Double(v) => ir::Constant::UInt(v as u32),
                        ir::Constant::Enum(_, _) => unreachable!(),
                        _ => return Err(()),
                    }
                }
                ir::TypeLayer::Scalar(ir::ScalarType::Half) => {
                    let inner_value = match inner_value {
                        ir::Constant::Enum(_, inner) => *inner,
                        other => other,
                    };
                    match inner_value {
                        ir::Constant::Bool(v) => ir::Constant::Half(if v { 1.0 } else { 0.0 }),
                        ir::Constant::UntypedInt(v) => ir::Constant::Half(v as f32),
                        ir::Constant::Int(v) => ir::Constant::Half(v as f32),
                        ir::Constant::UInt(v) => ir::Constant::Half(v as f32),
                        ir::Constant::Half(v) => ir::Constant::Half(v),
                        ir::Constant::Float(v) => ir::Constant::Half(v),
                        ir::Constant::Double(v) => ir::Constant::Half(v as f32),
                        ir::Constant::Enum(_, _) => unreachable!(),
                        _ => return Err(()),
                    }
                }
                ir::TypeLayer::Scalar(ir::ScalarType::Float) => {
                    let inner_value = match inner_value {
                        ir::Constant::Enum(_, inner) => *inner,
                        other => other,
                    };
                    match inner_value {
                        ir::Constant::Bool(v) => ir::Constant::Float(if v { 1.0 } else { 0.0 }),
                        ir::Constant::UntypedInt(v) => ir::Constant::Float(v as f32),
                        ir::Constant::Int(v) => ir::Constant::Float(v as f32),
                        ir::Constant::UInt(v) => ir::Constant::Float(v as f32),
                        ir::Constant::Half(v) => ir::Constant::Float(v),
                        ir::Constant::Float(v) => ir::Constant::Float(v),
                        ir::Constant::Double(v) => ir::Constant::Float(v as f32),
                        ir::Constant::Enum(_, _) => unreachable!(),
                        _ => return Err(()),
                    }
                }
                ir::TypeLayer::Scalar(ir::ScalarType::Double) => {
                    let inner_value = match inner_value {
                        ir::Constant::Enum(_, inner) => *inner,
                        other => other,
                    };
                    match inner_value {
                        ir::Constant::Bool(v) => ir::Constant::Double(if v { 1.0 } else { 0.0 }),
                        ir::Constant::UntypedInt(v) => ir::Constant::Double(v as f64),
                        ir::Constant::Int(v) => ir::Constant::Double(v as f64),
                        ir::Constant::UInt(v) => ir::Constant::Double(v as f64),
                        ir::Constant::Half(v) => ir::Constant::Double(v as f64),
                        ir::Constant::Float(v) => ir::Constant::Double(v as f64),
                        ir::Constant::Double(v) => ir::Constant::Double(v),
                        ir::Constant::Enum(_, _) => unreachable!(),
                        _ => return Err(()),
                    }
                }
                _ => return Err(()),
            }
        }
        _ => return Err(()),
    })
}
