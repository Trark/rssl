use rssl_ir::ScalarType::*;
use rssl_ir::ValueType::Lvalue;
use rssl_ir::ValueType::Rvalue;
use rssl_ir::*;

// Overload priority
// =================
//
// Testing the priority of functions when passed arguments of a given type:
//
// bool(1)              bool     -> uint/int/float/double                              -> half
// int(2)               int      -> uint                  -> bool -> float/double      -> half
// untyped int literal:             uint/int              -> bool -> float/double/half
// uint(1)              uint     -> int                   -> bool -> float/double      -> half
// half:                half     -> float/double                  -> bool/int/uint
// float:               float    -> double                        -> bool/int/uint/half
// double:              double                                    -> bool/int/uint/float/half
//
// (1): Including exact literals
// (2): Including type casted int literals
//
// lvalue / rvalue didn't seem to make a difference. These don't seem consistent
// with C or C++ rules.
//
// Priority:
// 1) Exact matches fit first (untyped int literal could be either)
// 2) Promotion
// 3) Bool if the source is an int
// 4) Any convertible type, except:
// 5) Halfs if you're an bool/int/uint, unless it's an untyped literal
//    (something to do with halfs being smaller than int/uint but not literals???)
//
// I was going to try to implement nice logic in here, but I honestly have no
// idea what rules govern these priorities.

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ConversionRank(NumericRank, VectorRank);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NumericRank {
    Exact,
    Promotion,
    IntToBool,
    Conversion,
    HalfIsASecondClassCitizen,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum VectorRank {
    /// Same dimension
    Exact,
    /// Expand a scalar to fill all slots in a vector
    Expand,
    /// Cull the later elements in the vector
    Contract,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum ConversionPriority {
    Better,
    Equal,
    Worse,
}

#[derive(PartialEq, Debug, Clone)]
struct DimensionCast(NumericDimension, NumericDimension);

#[derive(PartialEq, Debug, Clone)]
struct NumericCast(ScalarType, ScalarType);

#[derive(PartialEq, Debug, Clone)]
struct ValueTypeCast(TypeId, ValueType, ValueType);

#[derive(PartialEq, Debug, Clone)]
struct ModifierCast(TypeModifier);

#[derive(PartialEq, Debug, Clone)]
pub struct ImplicitConversion(
    ExpressionType,
    Option<ValueTypeCast>,
    Option<DimensionCast>,
    Option<NumericCast>,
    Option<ModifierCast>,
);

impl ConversionRank {
    pub fn get_numeric_rank(&self) -> &NumericRank {
        &self.0
    }
    pub fn get_vector_rank(&self) -> &VectorRank {
        &self.1
    }
}

impl NumericRank {
    pub fn compare(&self, other: &NumericRank) -> ConversionPriority {
        let my_order = self.order();
        let other_order = other.order();
        match (my_order < other_order, my_order <= other_order) {
            (false, false) => ConversionPriority::Worse,
            (false, true) => ConversionPriority::Equal,
            (true, false) => unreachable!(),
            (true, true) => ConversionPriority::Better,
        }
    }

    fn order(&self) -> u32 {
        match *self {
            NumericRank::Exact => 0,
            NumericRank::Promotion => 1,
            NumericRank::IntToBool => 2,
            NumericRank::Conversion => 3,
            NumericRank::HalfIsASecondClassCitizen => 4,
        }
    }
}

impl VectorRank {
    pub fn worst_to_best() -> &'static [VectorRank] {
        const PRIO: &[VectorRank] = &[VectorRank::Contract, VectorRank::Expand, VectorRank::Exact];
        PRIO
    }
}

impl NumericCast {
    fn new(source: ScalarType, dest: ScalarType) -> Result<NumericCast, ()> {
        match dest {
            _ if source == dest => Err(()),
            Bool | Int | UInt | Half | Float | Double => Ok(NumericCast(source, dest)),
            UntypedInt => Err(()),
        }
    }

    fn get_rank(&self) -> NumericRank {
        match self {
            NumericCast(Bool, dest) => match dest {
                Bool => NumericRank::Exact,
                Int | UInt | Float | Double => NumericRank::Conversion,
                Half => NumericRank::HalfIsASecondClassCitizen,
                UntypedInt => panic!(),
            },
            NumericCast(UntypedInt, dest) => match dest {
                Int | UInt => NumericRank::Promotion,
                Bool => NumericRank::IntToBool,
                Float | Double | Half => NumericRank::Conversion,
                UntypedInt => panic!(),
            },
            NumericCast(Int, dest) => match dest {
                Int => NumericRank::Exact,
                UInt => NumericRank::Promotion,
                Bool => NumericRank::IntToBool,
                Float | Double => NumericRank::Conversion,
                Half => NumericRank::HalfIsASecondClassCitizen,
                UntypedInt => panic!(),
            },
            NumericCast(UInt, dest) => match dest {
                UInt => NumericRank::Exact,
                Int => NumericRank::Promotion,
                Bool => NumericRank::IntToBool,
                Float | Double => NumericRank::Conversion,
                Half => NumericRank::HalfIsASecondClassCitizen,
                UntypedInt => panic!(),
            },
            NumericCast(Half, dest) => match dest {
                Half => NumericRank::Exact,
                Float | Double => NumericRank::Promotion,
                Bool | Int | UInt => NumericRank::Conversion,
                UntypedInt => panic!(),
            },
            NumericCast(Float, dest) => match dest {
                Float => NumericRank::Exact,
                Double => NumericRank::Promotion,
                Bool | Int | UInt | Half => NumericRank::Conversion,
                UntypedInt => panic!(),
            },
            NumericCast(Double, dest) => match dest {
                Double => NumericRank::Exact,
                Bool | Int | UInt | Half | Float => NumericRank::Conversion,
                UntypedInt => panic!(),
            },
        }
    }

    fn get_target_type(&self, module: &mut Module, dim: NumericDimension) -> ExpressionType {
        let tyl = match dim {
            NumericDimension::Scalar => TypeLayer::Scalar(self.1),
            NumericDimension::Vector(x) => TypeLayer::Vector(self.1, x),
            NumericDimension::Matrix(x, y) => TypeLayer::Matrix(self.1, x, y),
        };
        let id = module.type_registry.register_type_layer(tyl);
        id.to_rvalue()
    }
}

impl ValueTypeCast {
    fn get_target_type(&self) -> ExpressionType {
        ExpressionType(self.0, self.2)
    }
}

impl ModifierCast {
    fn modify(&self, module: &mut Module, ty: ExpressionType) -> ExpressionType {
        let ExpressionType(ty, vt) = ty;
        let ty = module.type_registry.remove_modifier(ty);
        let ty = module.type_registry.combine_modifier(ty, self.0);
        ExpressionType(ty, vt)
    }
}

impl ImplicitConversion {
    pub fn find(
        source: ExpressionType,
        dest: ExpressionType,
        module: &mut Module,
    ) -> Result<ImplicitConversion, ()> {
        let (source_type, dest_type, value_type_cast) = match (&source.1, &dest.1) {
            (&Rvalue, &Lvalue) => return Err(()),
            (&Rvalue, &Rvalue) | (&Lvalue, &Lvalue) => (source.0, dest.0, None),
            (&Lvalue, &Rvalue) => (
                source.0,
                dest.0,
                Some(ValueTypeCast(source.0, Lvalue, Rvalue)),
            ),
        };

        let (source_id, mods) = module.type_registry.extract_modifier(source_type);
        let (dest_id, modd) = module.type_registry.extract_modifier(dest_type);

        let source_l = module.type_registry.get_type_layout(source_id);
        let dest_l = module.type_registry.get_type_layout(dest_id);

        let dimension_cast = match (&source_l, &dest_l, dest.1 == Lvalue) {
            (ty1, ty2, _) if ty1 == ty2 => None,
            // Scalar to scalar
            (TypeLayout::Scalar(_), TypeLayout::Scalar(_), false) => None,
            // Scalar to vector1 of same type (works for lvalues)
            (TypeLayout::Scalar(s1), TypeLayout::Vector(s2, x2), _) if s1 == s2 && *x2 == 1 => {
                Some(DimensionCast(
                    NumericDimension::Scalar,
                    NumericDimension::Vector(1),
                ))
            }
            // vector1 to scalar of same type (works for lvalues)
            (TypeLayout::Vector(s1, x1), TypeLayout::Scalar(s2), _) if s1 == s2 && *x1 == 1 => {
                Some(DimensionCast(
                    NumericDimension::Vector(1),
                    NumericDimension::Scalar,
                ))
            }
            // Scalar to vector (mirror)
            (TypeLayout::Scalar(_), TypeLayout::Vector(_, x2), false) => Some(DimensionCast(
                NumericDimension::Scalar,
                NumericDimension::Vector(*x2),
            )),
            // Single vector to vector (mirror)
            (TypeLayout::Vector(_, 1), TypeLayout::Vector(_, x2), false) => Some(DimensionCast(
                NumericDimension::Vector(1),
                NumericDimension::Vector(*x2),
            )),
            // Vector first element to scalar
            (TypeLayout::Vector(_, x1), TypeLayout::Scalar(_), false) => Some(DimensionCast(
                NumericDimension::Vector(*x1),
                NumericDimension::Scalar,
            )),
            // Vector same dimension
            (TypeLayout::Vector(_, x1), TypeLayout::Vector(_, x2), false) if x1 == x2 => None,
            // Vector cull additional elements
            (TypeLayout::Vector(_, x1), TypeLayout::Vector(_, x2), false) if x2 < x1 => Some(
                DimensionCast(NumericDimension::Vector(*x1), NumericDimension::Vector(*x2)),
            ),
            // Matrix same dimension
            (TypeLayout::Matrix(_, x1, y1), TypeLayout::Matrix(_, x2, y2), false)
                if x1 == x2 && y1 == y2 =>
            {
                None
            }
            // Vector <-> Matrix casts not implemented
            // Struct casts only supported for same type structs
            _ => return Err(()),
        };

        let numeric_cast = if source_l == dest_l {
            None
        } else {
            let source_scalar = match *source_l {
                TypeLayout::Scalar(s) => s,
                TypeLayout::Vector(s, _) => s,
                TypeLayout::Matrix(s, _, _) => s,
                _ => return Err(()),
            };
            let dest_scalar = match *dest_l {
                TypeLayout::Scalar(s) => s,
                TypeLayout::Vector(s, _) => s,
                TypeLayout::Matrix(s, _, _) => s,
                _ => return Err(()),
            };
            if source_scalar == dest_scalar {
                None
            } else {
                let cast = NumericCast::new(source_scalar, dest_scalar)?;
                Some(cast)
            }
        };

        let modifier_cast = if mods != modd {
            // Can't remove important modifiers from lvalues
            // If they're rvalues we're implicitly creating a new
            // unmodified rvalues from the source lvalue/rvalue
            // This should let us use consts + volatiles as normal
            // typed inputs, but not as out params
            if dest.1 == Lvalue {
                if mods.is_const && !modd.is_const {
                    return Err(());
                }
                if mods.volatile && !modd.volatile {
                    return Err(());
                }
            }
            Some(ModifierCast(modd))
        } else {
            None
        };

        Ok(ImplicitConversion(
            source,
            value_type_cast,
            dimension_cast,
            numeric_cast,
            modifier_cast,
        ))
    }

    pub fn get_rank(&self) -> ConversionRank {
        use rssl_ir::NumericDimension::Scalar;
        use rssl_ir::NumericDimension::Vector;
        let &ImplicitConversion(_, _, ref dim_cast, ref num_cast, _) = self;
        let vec = match *dim_cast {
            None
            | Some(DimensionCast(Scalar, Vector(1)))
            | Some(DimensionCast(Vector(1), Scalar)) => VectorRank::Exact,
            Some(DimensionCast(Scalar, Vector(_))) | Some(DimensionCast(Vector(1), Vector(_))) => {
                VectorRank::Expand
            }
            Some(DimensionCast(Vector(_), Scalar)) => VectorRank::Contract,
            Some(DimensionCast(Vector(ref l), Vector(ref r))) if l > r => VectorRank::Contract,
            Some(DimensionCast(from, to)) => panic!("invalid vector cast {:?} {:?}", from, to),
        };
        let num = match *num_cast {
            Some(ref numeric) => numeric.get_rank(),
            None => NumericRank::Exact,
        };
        ConversionRank(num, vec)
    }

    pub fn get_target_type(&self, module: &mut Module) -> ExpressionType {
        let &ImplicitConversion(
            source_type,
            ref value_type_cast,
            ref dimension_cast,
            ref numeric_cast,
            ref mod_cast,
        ) = self;
        let ty = match *value_type_cast {
            Some(ref vtc) => vtc.get_target_type(),
            None => source_type,
        };
        let dim = match *dimension_cast {
            Some(DimensionCast(_, ref dim)) => Some(*dim),
            None => {
                let ty_unmodified = module.type_registry.remove_modifier(ty.0);
                match module.type_registry.get_type_layer(ty_unmodified) {
                    TypeLayer::Scalar(_) => Some(NumericDimension::Scalar),
                    TypeLayer::Vector(_, x) => Some(NumericDimension::Vector(x)),
                    TypeLayer::Matrix(_, x, y) => Some(NumericDimension::Matrix(x, y)),
                    _ => None,
                }
            }
        };
        let ty = match *numeric_cast {
            Some(ref nc) => nc.get_target_type(
                module,
                dim.expect("expecting numeric cast to operate on numeric type"),
            ),
            None => match dim {
                Some(dim) => {
                    let ExpressionType(ty_modified, vt) = ty;
                    let (ty, m) = module.type_registry.extract_modifier(ty_modified);
                    let scalar = match module.type_registry.get_type_layer(ty) {
                        TypeLayer::Scalar(scalar) => scalar,
                        TypeLayer::Vector(scalar, _) => scalar,
                        TypeLayer::Matrix(scalar, _, _) => scalar,
                        _ => panic!("dimension cast on non numeric type"),
                    };
                    let tyl_dim = match dim {
                        NumericDimension::Scalar => TypeLayer::Scalar(scalar),
                        NumericDimension::Vector(x) => TypeLayer::Vector(scalar, x),
                        NumericDimension::Matrix(x, y) => TypeLayer::Matrix(scalar, x, y),
                    };
                    let id = module.type_registry.register_type_layer(tyl_dim);
                    let id = module.type_registry.combine_modifier(id, m);
                    ExpressionType(id, vt)
                }
                None => ty,
            },
        };
        match *mod_cast {
            Some(ref mc) => mc.modify(module, ty),
            None => ty,
        }
    }

    pub fn apply(&self, expr: Expression, module: &mut Module) -> Expression {
        // If there was no conversion then return the given expression
        if let ImplicitConversion(_, _, None, None, None) = *self {
            return expr;
        }

        let target_type = self.get_target_type(module);

        // If the type was an untyped literal int then instead convert the literal type
        // This simplifies the expressions we generate so we don't have to clean it up later
        if let Expression::Literal(Constant::UntypedInt(v)) = expr {
            let target_type_unmodified = module.type_registry.remove_modifier(target_type.0);
            match module.type_registry.get_type_layer(target_type_unmodified) {
                TypeLayer::Scalar(ScalarType::Bool) => {
                    return Expression::Literal(Constant::Bool(v != 0))
                }
                TypeLayer::Scalar(ScalarType::UInt) => {
                    return Expression::Literal(Constant::UInt(v as u32))
                }
                TypeLayer::Scalar(ScalarType::Int) => {
                    return Expression::Literal(Constant::Int(v as i32))
                }
                TypeLayer::Scalar(ScalarType::Half) => {
                    return Expression::Literal(Constant::Half(v as f32))
                }
                TypeLayer::Scalar(ScalarType::Float) => {
                    return Expression::Literal(Constant::Float(v as f32))
                }
                TypeLayer::Scalar(ScalarType::Double) => {
                    return Expression::Literal(Constant::Double(v as f64))
                }
                _ => {}
            }
        }

        // Emit a cast operation on the given expression
        Expression::Cast(target_type.0, Box::new(expr))
    }
}

#[test]
fn test_implicitconversion() {
    let mut module = Module::create();
    let bool_ty = module.type_registry.register_type(TypeLayout::bool());
    let int_ty = module.type_registry.register_type(TypeLayout::int());
    let int1_ty = module.type_registry.register_type(TypeLayout::intn(1));
    let uint_ty = module.type_registry.register_type(TypeLayout::uint());
    let uint1_ty = module.type_registry.register_type(TypeLayout::uintn(1));
    let uint4_ty = module.type_registry.register_type(TypeLayout::uintn(4));
    let float_ty = module.type_registry.register_type(TypeLayout::float());
    let float4_ty = module.type_registry.register_type(TypeLayout::floatn(4));
    let sampler_state_ty = module
        .type_registry
        .register_type(TypeLayout::Object(ObjectType::SamplerState));
    let buffer_f4_ty = module
        .type_registry
        .register_type(TypeLayout::from_object(ObjectType::Buffer(float4_ty)));

    let basic_types = &[bool_ty, int_ty, uint_ty, float_ty, float4_ty];

    for ty in basic_types {
        assert_eq!(
            ImplicitConversion::find(ty.to_rvalue(), ty.to_rvalue(), &mut module),
            Ok(ImplicitConversion(ty.to_rvalue(), None, None, None, None))
        );
        assert_eq!(
            ImplicitConversion::find(ty.to_lvalue(), ty.to_rvalue(), &mut module),
            Ok(ImplicitConversion(
                ty.to_lvalue(),
                Some(ValueTypeCast(*ty, Lvalue, Rvalue)),
                None,
                None,
                None
            ))
        );
        assert_eq!(
            ImplicitConversion::find(ty.to_rvalue(), ty.to_lvalue(), &mut module),
            Err(())
        );
        assert_eq!(
            ImplicitConversion::find(ty.to_lvalue(), ty.to_lvalue(), &mut module),
            Ok(ImplicitConversion(ty.to_lvalue(), None, None, None, None))
        );
    }

    assert_eq!(
        ImplicitConversion::find(
            sampler_state_ty.to_lvalue(),
            uint_ty.to_lvalue(),
            &mut module
        ),
        Err(())
    );
    assert_eq!(
        ImplicitConversion::find(buffer_f4_ty.to_lvalue(), uint_ty.to_lvalue(), &mut module),
        Err(())
    );

    assert_eq!(
        ImplicitConversion::find(int_ty.to_rvalue(), int1_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            int_ty.to_rvalue(),
            None,
            Some(DimensionCast(
                NumericDimension::Scalar,
                NumericDimension::Vector(1)
            )),
            None,
            None
        ))
    );
    assert_eq!(
        ImplicitConversion::find(int1_ty.to_rvalue(), int_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            int1_ty.to_rvalue(),
            None,
            Some(DimensionCast(
                NumericDimension::Vector(1),
                NumericDimension::Scalar
            )),
            None,
            None
        ))
    );
    assert_eq!(
        ImplicitConversion::find(uint_ty.to_rvalue(), uint4_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            uint_ty.to_rvalue(),
            None,
            Some(DimensionCast(
                NumericDimension::Scalar,
                NumericDimension::Vector(4)
            )),
            None,
            None
        ))
    );
    assert_eq!(
        ImplicitConversion::find(uint1_ty.to_rvalue(), uint4_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            uint1_ty.to_rvalue(),
            None,
            Some(DimensionCast(
                NumericDimension::Vector(1),
                NumericDimension::Vector(4)
            )),
            None,
            None
        ))
    );

    assert_eq!(
        ImplicitConversion::find(int_ty.to_lvalue(), int1_ty.to_lvalue(), &mut module),
        Ok(ImplicitConversion(
            int_ty.to_lvalue(),
            None,
            Some(DimensionCast(
                NumericDimension::Scalar,
                NumericDimension::Vector(1)
            )),
            None,
            None
        ))
    );
    assert_eq!(
        ImplicitConversion::find(int1_ty.to_lvalue(), int_ty.to_lvalue(), &mut module),
        Ok(ImplicitConversion(
            int1_ty.to_lvalue(),
            None,
            Some(DimensionCast(
                NumericDimension::Vector(1),
                NumericDimension::Scalar
            )),
            None,
            None
        ))
    );
}

#[test]
fn test_get_rank() {
    let mut module = Module::create();
    let uint_ty = module.type_registry.register_type(TypeLayout::uint());
    let uint1_ty = module.type_registry.register_type(TypeLayout::uintn(1));
    let uint4_ty = module.type_registry.register_type(TypeLayout::uintn(4));

    assert_eq!(
        ImplicitConversion::find(uint_ty.to_rvalue(), uint1_ty.to_rvalue(), &mut module)
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Exact)
    );
    assert_eq!(
        ImplicitConversion::find(uint1_ty.to_rvalue(), uint_ty.to_rvalue(), &mut module)
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Exact)
    );
    assert_eq!(
        ImplicitConversion::find(uint_ty.to_rvalue(), uint4_ty.to_rvalue(), &mut module)
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Expand)
    );
    assert_eq!(
        ImplicitConversion::find(uint1_ty.to_rvalue(), uint4_ty.to_rvalue(), &mut module)
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Expand)
    );
}

#[test]
fn test_const() {
    let mut module = Module::create();
    let int_ty = module
        .type_registry
        .register_type(TypeLayout::from_scalar(Int));
    let const_int_ty = module
        .type_registry
        .register_type(TypeLayout::from_scalar(Int).combine_modifier(TypeModifier::const_only()));

    // Non-const to const rvalue
    assert_eq!(
        ImplicitConversion::find(int_ty.to_rvalue(), const_int_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            int_ty.to_rvalue(),
            None,
            None,
            None,
            Some(ModifierCast(TypeModifier::const_only()))
        ))
    );
    assert_eq!(
        ImplicitConversion::find(int_ty.to_lvalue(), const_int_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            int_ty.to_lvalue(),
            Some(ValueTypeCast(int_ty, Lvalue, Rvalue)),
            None,
            None,
            Some(ModifierCast(TypeModifier::const_only()))
        ))
    );
    // Const to const rvalue
    assert_eq!(
        ImplicitConversion::find(
            const_int_ty.to_rvalue(),
            const_int_ty.to_rvalue(),
            &mut module
        ),
        Ok(ImplicitConversion(
            const_int_ty.to_rvalue(),
            None,
            None,
            None,
            None
        ))
    );
    assert_eq!(
        ImplicitConversion::find(
            const_int_ty.to_lvalue(),
            const_int_ty.to_rvalue(),
            &mut module
        ),
        Ok(ImplicitConversion(
            const_int_ty.to_lvalue(),
            Some(ValueTypeCast(const_int_ty, Lvalue, Rvalue)),
            None,
            None,
            None
        ))
    );
    // Const removing from rvalue
    assert_eq!(
        ImplicitConversion::find(const_int_ty.to_rvalue(), int_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            const_int_ty.to_rvalue(),
            None,
            None,
            None,
            Some(ModifierCast(TypeModifier::default()))
        ))
    );
    assert_eq!(
        ImplicitConversion::find(const_int_ty.to_lvalue(), int_ty.to_rvalue(), &mut module),
        Ok(ImplicitConversion(
            const_int_ty.to_lvalue(),
            Some(ValueTypeCast(const_int_ty, Lvalue, Rvalue)),
            None,
            None,
            Some(ModifierCast(TypeModifier::default()))
        ))
    );

    // Non-const to const lvalue
    assert_eq!(
        ImplicitConversion::find(int_ty.to_lvalue(), const_int_ty.to_lvalue(), &mut module),
        Ok(ImplicitConversion(
            int_ty.to_lvalue(),
            None,
            None,
            None,
            Some(ModifierCast(TypeModifier::const_only()))
        ))
    );
    // const to const lvalue
    assert_eq!(
        ImplicitConversion::find(
            const_int_ty.to_lvalue(),
            const_int_ty.to_lvalue(),
            &mut module
        ),
        Ok(ImplicitConversion(
            const_int_ty.to_lvalue(),
            None,
            None,
            None,
            None
        ))
    );
    // const to non-const lvalue
    assert_eq!(
        ImplicitConversion::find(const_int_ty.to_lvalue(), int_ty.to_lvalue(), &mut module),
        Err(())
    );
}
