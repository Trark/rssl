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
struct ValueTypeCast(Type, ValueType, ValueType);

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

    fn get_target_type(&self, dim: NumericDimension) -> ExpressionType {
        Type::from_layout(match dim {
            NumericDimension::Scalar => TypeLayout::Scalar(self.1),
            NumericDimension::Vector(x) => TypeLayout::Vector(self.1, x),
            NumericDimension::Matrix(x, y) => TypeLayout::Matrix(self.1, x, y),
        })
        .to_rvalue()
    }
}

impl ValueTypeCast {
    fn get_target_type(&self) -> ExpressionType {
        ExpressionType(self.0.clone(), self.2.clone())
    }
}

impl ModifierCast {
    fn modify(&self, ty: ExpressionType) -> ExpressionType {
        let ExpressionType(ty, vt) = ty;
        let (ty, _) = ty.extract_modifier();
        ExpressionType(Type(ty).combine_modifier(self.0), vt)
    }
}

impl ImplicitConversion {
    pub fn find(source: &ExpressionType, dest: &ExpressionType) -> Result<ImplicitConversion, ()> {
        let source_copy = source.clone();
        let (source_type, dest_type, value_type_cast) = match (&source.1, &dest.1) {
            (&Rvalue, &Lvalue) => return Err(()),
            (&Rvalue, &Rvalue) | (&Lvalue, &Lvalue) => (&source.0, &dest.0, None),
            (&Lvalue, &Rvalue) => (
                &source.0,
                &dest.0,
                Some(ValueTypeCast(source.0.clone(), Lvalue, Rvalue)),
            ),
        };

        let (source_l, mods) = source_type.clone().extract_modifier();
        let (dest_l, modd) = dest_type.clone().extract_modifier();

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
            let source_scalar = match source_l {
                TypeLayout::Scalar(s) => s,
                TypeLayout::Vector(s, _) => s,
                TypeLayout::Matrix(s, _, _) => s,
                _ => return Err(()),
            };
            let dest_scalar = match dest_l {
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
            source_copy,
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

    pub fn get_target_type(&self) -> ExpressionType {
        let &ImplicitConversion(
            ref source_type,
            ref value_type_cast,
            ref dimension_cast,
            ref numeric_cast,
            ref mod_cast,
        ) = self;
        let ty = source_type.clone();
        let ty = match *value_type_cast {
            Some(ref vtc) => vtc.get_target_type(),
            None => ty,
        };
        let dim = match *dimension_cast {
            Some(DimensionCast(_, ref dim)) => Some(*dim),
            None => match (ty.0).0 {
                TypeLayout::Scalar(_) => Some(NumericDimension::Scalar),
                TypeLayout::Vector(_, ref x) => Some(NumericDimension::Vector(*x)),
                TypeLayout::Matrix(_, ref x, ref y) => Some(NumericDimension::Matrix(*x, *y)),
                _ => None,
            },
        };
        let ty = match *numeric_cast {
            Some(ref nc) => {
                nc.get_target_type(dim.expect("expecting numeric cast to operate on numeric type"))
            }
            None => match dim {
                Some(dim) => {
                    let ExpressionType(ty_modified, vt) = ty;
                    let (tyl, m) = ty_modified.extract_modifier();
                    let scalar = match tyl {
                        TypeLayout::Scalar(scalar) => scalar,
                        TypeLayout::Vector(scalar, _) => scalar,
                        TypeLayout::Matrix(scalar, _, _) => scalar,
                        _ => panic!("dimension cast on non numeric type"),
                    };
                    let tyl_dim = match dim {
                        NumericDimension::Scalar => TypeLayout::Scalar(scalar),
                        NumericDimension::Vector(x) => TypeLayout::Vector(scalar, x),
                        NumericDimension::Matrix(x, y) => TypeLayout::Matrix(scalar, x, y),
                    };
                    ExpressionType(Type(tyl_dim).combine_modifier(m), vt)
                }
                None => ty,
            },
        };
        match *mod_cast {
            Some(ref mc) => mc.modify(ty),
            None => ty,
        }
    }

    pub fn apply(&self, expr: Expression) -> Expression {
        // If there was no conversion then return the given expression
        if let ImplicitConversion(_, _, None, None, None) = *self {
            return expr;
        }

        let target_type = self.get_target_type();

        // If the type was an untyped literal int then instead convert the literal type
        // This simplifies the expressions we generate so we don't have to clean it up later
        if let Expression::Literal(Literal::UntypedInt(v)) = expr {
            let (target_ty, _) = target_type.0.clone().extract_modifier();
            match target_ty {
                TypeLayout::Scalar(ScalarType::Bool) => {
                    return Expression::Literal(Literal::Bool(v != 0))
                }
                TypeLayout::Scalar(ScalarType::UInt) => {
                    return Expression::Literal(Literal::UInt(v))
                }
                TypeLayout::Scalar(ScalarType::Int) => return Expression::Literal(Literal::Int(v)),
                TypeLayout::Scalar(ScalarType::Half) => {
                    return Expression::Literal(Literal::Half(v as f32))
                }
                TypeLayout::Scalar(ScalarType::Float) => {
                    return Expression::Literal(Literal::Float(v as f32))
                }
                TypeLayout::Scalar(ScalarType::Double) => {
                    return Expression::Literal(Literal::Double(v as f64))
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
    let basic_types = &[
        Type::bool(),
        Type::int(),
        Type::uint(),
        Type::float(),
        Type::floatn(4),
    ];

    for ty in basic_types {
        assert_eq!(
            ImplicitConversion::find(&ty.to_rvalue(), &ty.to_rvalue()),
            Ok(ImplicitConversion(ty.to_rvalue(), None, None, None, None))
        );
        assert_eq!(
            ImplicitConversion::find(&ty.to_lvalue(), &ty.to_rvalue()),
            Ok(ImplicitConversion(
                ty.to_lvalue(),
                Some(ValueTypeCast(ty.clone(), Lvalue, Rvalue)),
                None,
                None,
                None
            ))
        );
        assert_eq!(
            ImplicitConversion::find(&ty.to_rvalue(), &ty.to_lvalue()),
            Err(())
        );
        assert_eq!(
            ImplicitConversion::find(&ty.to_lvalue(), &ty.to_lvalue()),
            Ok(ImplicitConversion(ty.to_lvalue(), None, None, None, None))
        );
    }

    assert_eq!(
        ImplicitConversion::find(
            &Type::from_layout(TypeLayout::SamplerState).to_lvalue(),
            &Type::uint().to_lvalue()
        ),
        Err(())
    );
    let f4dty = DataType(DataLayout::Vector(Float, 4), TypeModifier::default());
    assert_eq!(
        ImplicitConversion::find(
            &Type::from_object(ObjectType::Buffer(f4dty)).to_lvalue(),
            &Type::uint().to_lvalue()
        ),
        Err(())
    );

    assert_eq!(
        ImplicitConversion::find(&Type::int().to_rvalue(), &Type::intn(1).to_rvalue()),
        Ok(ImplicitConversion(
            Type::int().to_rvalue(),
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
        ImplicitConversion::find(&Type::intn(1).to_rvalue(), &Type::int().to_rvalue()),
        Ok(ImplicitConversion(
            Type::intn(1).to_rvalue(),
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
        ImplicitConversion::find(&Type::uint().to_rvalue(), &Type::uintn(4).to_rvalue()),
        Ok(ImplicitConversion(
            Type::uint().to_rvalue(),
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
        ImplicitConversion::find(&Type::uintn(1).to_rvalue(), &Type::uintn(4).to_rvalue()),
        Ok(ImplicitConversion(
            Type::uintn(1).to_rvalue(),
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
        ImplicitConversion::find(&Type::int().to_lvalue(), &Type::intn(1).to_lvalue()),
        Ok(ImplicitConversion(
            Type::int().to_lvalue(),
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
        ImplicitConversion::find(&Type::intn(1).to_lvalue(), &Type::int().to_lvalue()),
        Ok(ImplicitConversion(
            Type::intn(1).to_lvalue(),
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
    assert_eq!(
        ImplicitConversion::find(&Type::uint().to_rvalue(), &Type::uintn(1).to_rvalue())
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Exact)
    );
    assert_eq!(
        ImplicitConversion::find(&Type::uintn(1).to_rvalue(), &Type::uint().to_rvalue())
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Exact)
    );
    assert_eq!(
        ImplicitConversion::find(&Type::uint().to_rvalue(), &Type::uintn(4).to_rvalue())
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Expand)
    );
    assert_eq!(
        ImplicitConversion::find(&Type::uintn(1).to_rvalue(), &Type::uintn(4).to_rvalue())
            .unwrap()
            .get_rank(),
        ConversionRank(NumericRank::Exact, VectorRank::Expand)
    );
}

#[test]
fn test_const() {
    // Non-const to const rvalue
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int)).to_rvalue()),
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_rvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(Type(TypeLayout::from_scalar(Int)), Rvalue),
            None,
            None,
            None,
            Some(ModifierCast(TypeModifier::const_only()))
        ))
    );
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int)).to_lvalue()),
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_rvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(Type(TypeLayout::from_scalar(Int)), Lvalue),
            Some(ValueTypeCast(Type::int(), Lvalue, Rvalue)),
            None,
            None,
            Some(ModifierCast(TypeModifier::const_only()))
        ))
    );
    // Const to const rvalue
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_rvalue()),
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_rvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Rvalue
            ),
            None,
            None,
            None,
            None
        ))
    );
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_lvalue()),
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_rvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Lvalue
            ),
            Some(ValueTypeCast(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Lvalue,
                Rvalue
            )),
            None,
            None,
            None
        ))
    );
    // Const removing from rvalue
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_rvalue()),
            &(Type(TypeLayout::from_scalar(Int)).to_rvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Rvalue
            ),
            None,
            None,
            None,
            Some(ModifierCast(TypeModifier::default()))
        ))
    );
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_lvalue()),
            &(Type(TypeLayout::from_scalar(Int)).to_rvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Lvalue
            ),
            Some(ValueTypeCast(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Lvalue,
                Rvalue
            )),
            None,
            None,
            Some(ModifierCast(TypeModifier::default()))
        ))
    );

    // Non-const to const lvalue
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int)).to_lvalue()),
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_lvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(Type(TypeLayout::from_scalar(Int)), Lvalue),
            None,
            None,
            None,
            Some(ModifierCast(TypeModifier::const_only()))
        ))
    );
    // const to const lvalue
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_lvalue()),
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_lvalue())
        ),
        Ok(ImplicitConversion(
            ExpressionType(
                Type(TypeLayout::from_scalar(Int)).combine_modifier(TypeModifier::const_only()),
                Lvalue
            ),
            None,
            None,
            None,
            None
        ))
    );
    // const to non-const lvalue
    assert_eq!(
        ImplicitConversion::find(
            &(Type(TypeLayout::from_scalar(Int))
                .combine_modifier(TypeModifier::const_only())
                .to_lvalue()),
            &(Type(TypeLayout::from_scalar(Int)).to_lvalue())
        ),
        Err(())
    );
}
