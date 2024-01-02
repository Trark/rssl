use crate::*;

/// A typed RSSL expression
#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Literal(Constant),
    /// Reference to a variable in a local scope
    Variable(VariableId),
    /// Reference to a variable in the struct that owns this expression
    MemberVariable(StructId, u32),
    /// Reference to a variable in a global scope
    Global(GlobalId),
    /// Reference to a variable in a constant buffer global
    ConstantVariable(ConstantBufferMemberId),
    /// Reference to an enum value
    EnumValue(EnumValueId),
    TernaryConditional(Box<Expression>, Box<Expression>, Box<Expression>),
    /// Chain of expressions
    Sequence(Vec<Expression>),
    Swizzle(Box<Expression>, Vec<SwizzleSlot>),
    MatrixSwizzle(Box<Expression>, Vec<MatrixSwizzleSlot>),
    ArraySubscript(Box<Expression>, Box<Expression>),
    /// Member expression to access a data member of a struct
    StructMember(Box<Expression>, StructId, u32),
    /// Member expression to access a data member of an intrinsic type
    /// TODO: Non-string identifiers
    ObjectMember(Box<Expression>, String),
    Call(FunctionId, CallType, Vec<Expression>),
    Constructor(TypeId, Vec<ConstructorSlot>),
    Cast(TypeId, Box<Expression>),
    SizeOf(TypeId),
    IntrinsicOp(IntrinsicOp, Vec<Expression>),
}

/// A single part of a vector or scalar swizzle operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum SwizzleSlot {
    X, // x or r
    Y, // y or g
    Z, // z or b
    W, // w or a
}

/// A component in a single dimension of a matrix
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum ComponentIndex {
    First,
    Second,
    Third,
    Forth,
}

/// A single part of a matrix swizzle operation
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MatrixSwizzleSlot(pub ComponentIndex, pub ComponentIndex);

/// Element passed to a numeric constructor
/// Constructors can take variable numbers of arguments depending on dimensions
/// of the types of the input expressions
#[derive(PartialEq, Debug, Clone)]
pub struct ConstructorSlot {
    /// Vector dimension or Matrix total element count or 1 for scalars
    pub arity: u32,
    /// The expression argument for this slot
    /// The type of this expression must be the scalar type of the numeric
    /// constructor this is used in with the arity above
    pub expr: Expression,
}

/// The form of a function invocation
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum CallType {
    /// Call is for a free function
    FreeFunction,

    /// Call is for a method invocation where the first argument is the object
    MethodExternal,

    /// Call is for a method invocation from within a class scope
    MethodInternal,
}

/// Error type when attempting to query the type of an expression
#[derive(Debug)]
pub enum EvaluateTypeError {
    InvalidModule,
}

impl Expression {
    /// Find the type of an expression
    pub fn get_type(&self, module: &Module) -> Result<ExpressionType, EvaluateTypeError> {
        match *self {
            Expression::Literal(ref lit) => Ok(lit.get_type(module)),
            Expression::Variable(id) => Ok(module
                .variable_registry
                .get_local_variable(id)
                .type_id
                .to_lvalue()),
            Expression::MemberVariable(id, member_index) => {
                assert!(id.0 < module.struct_registry.len() as u32);
                let def = &module.struct_registry[id.0 as usize];
                assert!(member_index < def.members.len() as u32);

                let member_type = def.members[member_index as usize].type_id;
                Ok(member_type.to_lvalue())
            }
            Expression::Global(id) => {
                assert!(id.0 < module.global_registry.len() as u32);
                let type_id = module.global_registry[id.0 as usize].type_id;
                Ok(type_id.to_lvalue())
            }
            Expression::ConstantVariable(id) => {
                let parent_id = id.0;
                let def = &module.cbuffer_registry[parent_id.0 as usize].members[id.1 as usize];
                Ok(def.type_id.to_lvalue())
            }
            Expression::EnumValue(id) => {
                Ok(module.enum_registry.get_enum_value(id).type_id.to_rvalue())
            }
            Expression::TernaryConditional(_, ref expr_left, ref expr_right) => {
                // Ensure the layouts of each side are the same
                // Value types + modifiers can be different
                {
                    let ty_left_mod = expr_left.get_type(module)?.0;
                    let ty_right_mod = expr_right.get_type(module)?.0;
                    let ty_left = module.type_registry.remove_modifier(ty_left_mod);
                    let ty_right = module.type_registry.remove_modifier(ty_right_mod);
                    assert_eq!(ty_left, ty_right);
                }
                let ety = expr_left.get_type(module)?;
                Ok(ety.0.to_rvalue())
            }
            Expression::Sequence(ref chain) => {
                let last = chain
                    .last()
                    .expect("Sequence must have at least one expression");
                let ety = last.get_type(module)?;
                Ok(ety)
            }
            Expression::Swizzle(ref vec, ref swizzle) => {
                let ExpressionType(vec_ty, vec_vt) = vec.get_type(module)?;
                let (vec_ty_nomod, vec_mod) = module.type_registry.extract_modifier(vec_ty);
                let vec_tyl_nomod = module.type_registry.get_type_layer(vec_ty_nomod);
                let vt = get_swizzle_value_type(swizzle, vec_vt);
                let ty = match vec_tyl_nomod {
                    TypeLayer::Scalar(_) => {
                        if swizzle.len() == 1 {
                            vec_ty_nomod
                        } else {
                            module.type_registry.register_type(TypeLayer::Vector(
                                vec_ty_nomod,
                                swizzle.len() as u32,
                            ))
                        }
                    }
                    TypeLayer::Vector(scalar, _) => {
                        if swizzle.len() == 1 {
                            scalar
                        } else {
                            module
                                .type_registry
                                .register_type(TypeLayer::Vector(scalar, swizzle.len() as u32))
                        }
                    }
                    _ => return Err(EvaluateTypeError::InvalidModule),
                };
                let ty = module.type_registry.combine_modifier(ty, vec_mod);
                Ok(ExpressionType(ty, vt))
            }
            Expression::MatrixSwizzle(ref mat, ref swizzle) => {
                let ExpressionType(mat_ty, mat_vt) = mat.get_type(module)?;
                let (mat_ty_nomod, mat_mod) = module.type_registry.extract_modifier(mat_ty);
                let mat_tyl_nomod = module.type_registry.get_type_layer(mat_ty_nomod);
                let vt = get_matrix_swizzle_value_type(swizzle, mat_vt);
                let ty = match mat_tyl_nomod {
                    TypeLayer::Matrix(scalar, _, _) => {
                        if swizzle.len() == 1 {
                            scalar
                        } else {
                            module
                                .type_registry
                                .register_type(TypeLayer::Vector(scalar, swizzle.len() as u32))
                        }
                    }
                    _ => return Err(EvaluateTypeError::InvalidModule),
                };
                let ty = module.type_registry.combine_modifier(ty, mat_mod);
                Ok(ExpressionType(ty, vt))
            }
            Expression::ArraySubscript(ref array, _) => {
                let array_ty = array.get_type(module)?;
                // Todo: Modifiers on object type template parameters
                let (array_ty_nomod, modifer) = module.type_registry.extract_modifier(array_ty.0);
                let array_tyl_nomod = module.type_registry.get_type_layer(array_ty_nomod);
                let ty = match array_tyl_nomod {
                    TypeLayer::Array(element, _) => element,
                    TypeLayer::Vector(st, _) => module.type_registry.combine_modifier(st, modifer),
                    TypeLayer::Matrix(st, _, y) => {
                        let ty = module.type_registry.register_type(TypeLayer::Vector(st, y));
                        module.type_registry.combine_modifier(ty, modifer)
                    }
                    TypeLayer::Object(ObjectType::Buffer(ty))
                    | TypeLayer::Object(ObjectType::StructuredBuffer(ty))
                    | TypeLayer::Object(ObjectType::Texture2D(ty))
                    | TypeLayer::Object(ObjectType::Texture2DMipsSlice(ty))
                    | TypeLayer::Object(ObjectType::Texture2DArray(ty))
                    | TypeLayer::Object(ObjectType::Texture2DArrayMipsSlice(ty))
                    | TypeLayer::Object(ObjectType::Texture3D(ty))
                    | TypeLayer::Object(ObjectType::Texture3DMipsSlice(ty)) => {
                        module.type_registry.make_const(ty)
                    }
                    TypeLayer::Object(ObjectType::RWBuffer(ty))
                    | TypeLayer::Object(ObjectType::RWStructuredBuffer(ty))
                    | TypeLayer::Object(ObjectType::RWTexture2D(ty))
                    | TypeLayer::Object(ObjectType::RWTexture2DArray(ty))
                    | TypeLayer::Object(ObjectType::RWTexture3D(ty)) => ty,
                    TypeLayer::Object(ObjectType::Texture2DMips(ty)) => {
                        let tyl = TypeLayer::Object(ObjectType::Texture2DMipsSlice(ty));
                        module.type_registry.register_type(tyl)
                    }
                    TypeLayer::Object(ObjectType::Texture2DArrayMips(ty)) => {
                        let tyl = TypeLayer::Object(ObjectType::Texture2DArrayMipsSlice(ty));
                        module.type_registry.register_type(tyl)
                    }
                    TypeLayer::Object(ObjectType::Texture3DMips(ty)) => {
                        let tyl = TypeLayer::Object(ObjectType::Texture3DMipsSlice(ty));
                        module.type_registry.register_type(tyl)
                    }
                    _ => return Err(EvaluateTypeError::InvalidModule),
                };
                Ok(ty.to_lvalue())
            }
            Expression::StructMember(ref expr, id, member_index) => {
                let expr_type = expr.get_type(module)?;

                assert!(id.0 < module.struct_registry.len() as u32);
                let def = &module.struct_registry[id.0 as usize];
                assert!(member_index < def.members.len() as u32);

                let member_type = def.members[member_index as usize].type_id;
                Ok(ExpressionType(member_type, expr_type.1))
            }
            Expression::ObjectMember(ref expr, ref name) => {
                let expr_type = expr.get_type(module)?;

                let mut ty = module.type_registry.remove_modifier(expr_type.0);
                let mut tyl = module.type_registry.get_type_layer(ty);

                // Handle mips member of Texture2D
                if let TypeLayer::Object(ObjectType::Texture2D(inner)) = tyl {
                    if name == "mips" {
                        let mips_oty = ObjectType::Texture2DMips(inner);
                        let mips_tyl = TypeLayer::Object(mips_oty);
                        let mips_ty = module.type_registry.register_type(mips_tyl);
                        return Ok(mips_ty.to_lvalue());
                    }
                }

                // Handle mips member of Texture2DArray
                if let TypeLayer::Object(ObjectType::Texture2DArray(inner)) = tyl {
                    if name == "mips" {
                        let mips_oty = ObjectType::Texture2DArrayMips(inner);
                        let mips_tyl = TypeLayer::Object(mips_oty);
                        let mips_ty = module.type_registry.register_type(mips_tyl);
                        return Ok(mips_ty.to_lvalue());
                    }
                }

                // Handle mips member of Texture3D
                if let TypeLayer::Object(ObjectType::Texture3D(inner)) = tyl {
                    if name == "mips" {
                        let mips_oty = ObjectType::Texture3DMips(inner);
                        let mips_tyl = TypeLayer::Object(mips_oty);
                        let mips_ty = module.type_registry.register_type(mips_tyl);
                        return Ok(mips_ty.to_lvalue());
                    }
                }

                // If it is a constant buffer then auto unwrap the inner type
                if let TypeLayer::Object(ObjectType::ConstantBuffer(inner)) = tyl {
                    ty = module.type_registry.remove_modifier(inner);
                    tyl = module.type_registry.get_type_layer(ty);
                }

                // RayDesc is not a real struct so custom check each of its members
                if let TypeLayer::Object(ObjectType::RayDesc) = tyl {
                    return match name.as_str() {
                        "Origin" | "Direction" => {
                            let f = module
                                .type_registry
                                .register_type(TypeLayer::Scalar(ScalarType::Float32));
                            let f3 = module.type_registry.register_type(TypeLayer::Vector(f, 3));
                            let ety = ExpressionType(f3, expr_type.1);
                            Ok(ety)
                        }
                        "TMin" | "TMax" => {
                            let f = module
                                .type_registry
                                .register_type(TypeLayer::Scalar(ScalarType::Float32));
                            let ety = ExpressionType(f, expr_type.1);
                            Ok(ety)
                        }
                        _ => Err(EvaluateTypeError::InvalidModule),
                    };
                }

                Err(EvaluateTypeError::InvalidModule)
            }
            Expression::Call(id, _, _) => Ok(module
                .function_registry
                .get_function_signature(id)
                .return_type
                .return_type
                .to_rvalue()),
            Expression::Constructor(ty, _) => Ok(ty.to_rvalue()),
            Expression::Cast(ty, _) => Ok(ty.to_rvalue()),
            Expression::SizeOf(_) => {
                let uint_ty = module
                    .type_registry
                    .register_type(TypeLayer::Scalar(ScalarType::UInt32));
                Ok(uint_ty.to_rvalue())
            }
            Expression::IntrinsicOp(ref intrinsic, ref args) => {
                let mut arg_types = Vec::with_capacity(args.len());
                for arg in args {
                    arg_types.push(arg.get_type(module)?);
                }
                let ety = intrinsic.get_return_type(&arg_types, module);
                Ok(ety)
            }
        }
    }
}
