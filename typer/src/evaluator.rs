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