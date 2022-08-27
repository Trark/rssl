use crate::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Intrinsic0 {
    AllMemoryBarrier,
    AllMemoryBarrierWithGroupSync,
    DeviceMemoryBarrier,
    DeviceMemoryBarrierWithGroupSync,
    GroupMemoryBarrier,
    GroupMemoryBarrierWithGroupSync,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Intrinsic1 {
    // Unary operations
    PrefixIncrement(Type),
    PrefixDecrement(Type),
    PostfixIncrement(Type),
    PostfixDecrement(Type),
    Plus(Type),
    Minus(Type),
    LogicalNot(Type),
    BitwiseNot(Type),

    All(NumericDimension),
    Any(NumericDimension),

    AbsI,
    AbsI2,
    AbsI3,
    AbsI4,
    AbsF,
    AbsF2,
    AbsF3,
    AbsF4,

    Acos,
    Acos2,
    Acos3,
    Acos4,

    Asin,
    Asin2,
    Asin3,
    Asin4,

    AsIntU,
    AsIntU2,
    AsIntU3,
    AsIntU4,
    AsIntF,
    AsIntF2,
    AsIntF3,
    AsIntF4,

    AsUIntI,
    AsUIntI2,
    AsUIntI3,
    AsUIntI4,
    AsUIntF,
    AsUIntF2,
    AsUIntF3,
    AsUIntF4,

    AsFloatI,
    AsFloatI2,
    AsFloatI3,
    AsFloatI4,
    AsFloatU,
    AsFloatU2,
    AsFloatU3,
    AsFloatU4,
    AsFloatF,
    AsFloatF2,
    AsFloatF3,
    AsFloatF4,

    Cos,
    Cos2,
    Cos3,
    Cos4,

    Exp,
    Exp2,
    Exp3,
    Exp4,

    F16ToF32,
    F32ToF16,

    Floor,
    Floor2,
    Floor3,
    Floor4,

    IsNaN,
    IsNaN2,
    IsNaN3,
    IsNaN4,

    Length1,
    Length2,
    Length3,
    Length4,

    Normalize1,
    Normalize2,
    Normalize3,
    Normalize4,

    Saturate,
    Saturate2,
    Saturate3,
    Saturate4,

    SignI,
    SignI2,
    SignI3,
    SignI4,
    SignF,
    SignF2,
    SignF3,
    SignF4,

    Sin,
    Sin2,
    Sin3,
    Sin4,

    Sqrt,
    Sqrt2,
    Sqrt3,
    Sqrt4,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Intrinsic2 {
    // Binary operations
    Add(DataType),
    Subtract(DataType),
    Multiply(DataType),
    Divide(DataType),
    Modulus(DataType),
    LeftShift(DataType),
    RightShift(DataType),
    BitwiseAnd(DataType),
    BitwiseOr(DataType),
    BitwiseXor(DataType),
    BooleanAnd(DataType),
    BooleanOr(DataType),
    LessThan(DataType),
    LessEqual(DataType),
    GreaterThan(DataType),
    GreaterEqual(DataType),
    Equality(DataType),
    Inequality(DataType),
    Assignment(Type),
    AssignSwizzle(Type, Vec<SwizzleSlot>),
    SumAssignment(DataType),
    DifferenceAssignment(DataType),
    ProductAssignment(DataType),
    QuotientAssignment(DataType),
    RemainderAssignment(DataType),

    AsDouble,

    Cross,

    Distance1,
    Distance2,
    Distance3,
    Distance4,

    DotI1,
    DotI2,
    DotI3,
    DotI4,
    DotF1,
    DotF2,
    DotF3,
    DotF4,

    MulMV(ScalarType, u32, u32),

    MinI,
    MinI2,
    MinI3,
    MinI4,
    MinF,
    MinF2,
    MinF3,
    MinF4,

    MaxI,
    MaxI2,
    MaxI3,
    MaxI4,
    MaxF,
    MaxF2,
    MaxF3,
    MaxF4,

    Pow,
    Pow2,
    Pow3,
    Pow4,

    Step,
    Step2,
    Step3,
    Step4,

    BufferLoad(DataType),
    RWBufferLoad(DataType),
    StructuredBufferLoad(StructuredType),
    RWStructuredBufferLoad(StructuredType),
    Texture2DLoad(DataType),
    RWTexture2DLoad(DataType),

    // ByteAddressBuffer methods
    ByteAddressBufferLoad,
    ByteAddressBufferLoad2,
    ByteAddressBufferLoad3,
    ByteAddressBufferLoad4,
    RWByteAddressBufferLoad,
    RWByteAddressBufferLoad2,
    RWByteAddressBufferLoad3,
    RWByteAddressBufferLoad4,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Intrinsic3 {
    ClampI,
    ClampI2,
    ClampI3,
    ClampI4,
    ClampF,
    ClampF2,
    ClampF3,
    ClampF4,

    Lerp,
    Lerp2,
    Lerp3,
    Lerp4,

    Sincos,
    Sincos2,
    Sincos3,
    Sincos4,

    SmoothStep,
    SmoothStep2,
    SmoothStep3,
    SmoothStep4,

    Texture2DSample(DataType),

    // RWTexture2D methods
    RWTexture2DStore(DataType),

    // ByteAddressBuffer methods
    RWByteAddressBufferStore,
    RWByteAddressBufferStore2,
    RWByteAddressBufferStore3,
    RWByteAddressBufferStore4,
}

pub trait Intrinsic {
    fn get_return_type(&self) -> ExpressionType;
}

impl Intrinsic for Intrinsic0 {
    fn get_return_type(&self) -> ExpressionType {
        match *self {
            Intrinsic0::AllMemoryBarrier => Type::void().to_rvalue(),
            Intrinsic0::AllMemoryBarrierWithGroupSync => Type::void().to_rvalue(),
            Intrinsic0::DeviceMemoryBarrier => Type::void().to_rvalue(),
            Intrinsic0::DeviceMemoryBarrierWithGroupSync => Type::void().to_rvalue(),
            Intrinsic0::GroupMemoryBarrier => Type::void().to_rvalue(),
            Intrinsic0::GroupMemoryBarrierWithGroupSync => Type::void().to_rvalue(),
        }
    }
}

impl Intrinsic for Intrinsic1 {
    fn get_return_type(&self) -> ExpressionType {
        match *self {
            Intrinsic1::PrefixIncrement(ref ty) => ty.to_lvalue(),
            Intrinsic1::PrefixDecrement(ref ty) => ty.to_lvalue(),
            Intrinsic1::PostfixIncrement(ref ty) => ty.to_lvalue(),
            Intrinsic1::PostfixDecrement(ref ty) => ty.to_lvalue(),
            Intrinsic1::Plus(ref ty) => ty.to_rvalue(),
            Intrinsic1::Minus(ref ty) => ty.to_rvalue(),
            Intrinsic1::LogicalNot(ref ty) => match ty.0 {
                TypeLayout::Scalar(_) => Type::bool().to_rvalue(),
                TypeLayout::Vector(_, x) => Type::booln(x).to_rvalue(),
                _ => panic!("invalid logical not intrinsic"),
            },
            Intrinsic1::BitwiseNot(ref ty) => ty.to_rvalue(),
            Intrinsic1::All(_) => Type::bool().to_rvalue(),
            Intrinsic1::Any(_) => Type::bool().to_rvalue(),
            Intrinsic1::AbsI => Type::int().to_rvalue(),
            Intrinsic1::AbsI2 => Type::intn(2).to_rvalue(),
            Intrinsic1::AbsI3 => Type::intn(3).to_rvalue(),
            Intrinsic1::AbsI4 => Type::intn(4).to_rvalue(),
            Intrinsic1::AbsF => Type::float().to_rvalue(),
            Intrinsic1::AbsF2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::AbsF3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::AbsF4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::Acos => Type::float().to_rvalue(),
            Intrinsic1::Acos2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Acos3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Acos4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::Asin => Type::float().to_rvalue(),
            Intrinsic1::Asin2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Asin3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Asin4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::AsIntU => Type::int().to_rvalue(),
            Intrinsic1::AsIntU2 => Type::intn(2).to_rvalue(),
            Intrinsic1::AsIntU3 => Type::intn(3).to_rvalue(),
            Intrinsic1::AsIntU4 => Type::intn(4).to_rvalue(),
            Intrinsic1::AsIntF => Type::int().to_rvalue(),
            Intrinsic1::AsIntF2 => Type::intn(2).to_rvalue(),
            Intrinsic1::AsIntF3 => Type::intn(3).to_rvalue(),
            Intrinsic1::AsIntF4 => Type::intn(4).to_rvalue(),
            Intrinsic1::AsUIntI => Type::uint().to_rvalue(),
            Intrinsic1::AsUIntI2 => Type::uintn(2).to_rvalue(),
            Intrinsic1::AsUIntI3 => Type::uintn(3).to_rvalue(),
            Intrinsic1::AsUIntI4 => Type::uintn(4).to_rvalue(),
            Intrinsic1::AsUIntF => Type::uint().to_rvalue(),
            Intrinsic1::AsUIntF2 => Type::uintn(2).to_rvalue(),
            Intrinsic1::AsUIntF3 => Type::uintn(3).to_rvalue(),
            Intrinsic1::AsUIntF4 => Type::uintn(4).to_rvalue(),
            Intrinsic1::AsFloatI => Type::float().to_rvalue(),
            Intrinsic1::AsFloatI2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::AsFloatI3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::AsFloatI4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::AsFloatU => Type::float().to_rvalue(),
            Intrinsic1::AsFloatU2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::AsFloatU3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::AsFloatU4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::AsFloatF => Type::float().to_rvalue(),
            Intrinsic1::AsFloatF2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::AsFloatF3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::AsFloatF4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::Cos => Type::float().to_rvalue(),
            Intrinsic1::Cos2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Cos3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Cos4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::Exp => Type::float().to_rvalue(),
            Intrinsic1::Exp2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Exp3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Exp4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::F16ToF32 => Type::float().to_rvalue(),
            Intrinsic1::F32ToF16 => Type::uint().to_rvalue(),
            Intrinsic1::Floor => Type::float().to_rvalue(),
            Intrinsic1::Floor2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Floor3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Floor4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::IsNaN => Type::bool().to_rvalue(),
            Intrinsic1::IsNaN2 => Type::booln(2).to_rvalue(),
            Intrinsic1::IsNaN3 => Type::booln(3).to_rvalue(),
            Intrinsic1::IsNaN4 => Type::booln(4).to_rvalue(),
            Intrinsic1::Length1 => Type::float().to_rvalue(),
            Intrinsic1::Length2 => Type::float().to_rvalue(),
            Intrinsic1::Length3 => Type::float().to_rvalue(),
            Intrinsic1::Length4 => Type::float().to_rvalue(),
            Intrinsic1::Normalize1 => Type::floatn(1).to_rvalue(),
            Intrinsic1::Normalize2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Normalize3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Normalize4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::Saturate => Type::float().to_rvalue(),
            Intrinsic1::Saturate2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Saturate3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Saturate4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::SignI => Type::int().to_rvalue(),
            Intrinsic1::SignI2 => Type::intn(2).to_rvalue(),
            Intrinsic1::SignI3 => Type::intn(3).to_rvalue(),
            Intrinsic1::SignI4 => Type::intn(4).to_rvalue(),
            Intrinsic1::SignF => Type::int().to_rvalue(),
            Intrinsic1::SignF2 => Type::intn(2).to_rvalue(),
            Intrinsic1::SignF3 => Type::intn(3).to_rvalue(),
            Intrinsic1::SignF4 => Type::intn(4).to_rvalue(),
            Intrinsic1::Sin => Type::float().to_rvalue(),
            Intrinsic1::Sin2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Sin3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Sin4 => Type::floatn(4).to_rvalue(),
            Intrinsic1::Sqrt => Type::float().to_rvalue(),
            Intrinsic1::Sqrt2 => Type::floatn(2).to_rvalue(),
            Intrinsic1::Sqrt3 => Type::floatn(3).to_rvalue(),
            Intrinsic1::Sqrt4 => Type::floatn(4).to_rvalue(),
        }
    }
}

impl Intrinsic for Intrinsic2 {
    fn get_return_type(&self) -> ExpressionType {
        match *self {
            Intrinsic2::Add(dty)
            | Intrinsic2::Subtract(dty)
            | Intrinsic2::Multiply(dty)
            | Intrinsic2::Divide(dty)
            | Intrinsic2::Modulus(dty)
            | Intrinsic2::LeftShift(dty)
            | Intrinsic2::RightShift(dty)
            | Intrinsic2::BitwiseAnd(dty)
            | Intrinsic2::BitwiseOr(dty)
            | Intrinsic2::BitwiseXor(dty)
            | Intrinsic2::BooleanAnd(dty)
            | Intrinsic2::BooleanOr(dty) => {
                // dty is the type of the arguments operated on, which
                // is the same as the return value
                Type::from_data(dty).to_rvalue()
            }
            Intrinsic2::LessThan(dty)
            | Intrinsic2::LessEqual(dty)
            | Intrinsic2::GreaterThan(dty)
            | Intrinsic2::GreaterEqual(dty)
            | Intrinsic2::Equality(dty)
            | Intrinsic2::Inequality(dty) => {
                // dty is the type of the arguments operated on, so the return
                // value is a bool with the same dimensions as dty
                Type::from_data(dty)
                    .transform_scalar(ScalarType::Bool)
                    .to_rvalue()
            }
            Intrinsic2::Assignment(ref ty) | Intrinsic2::AssignSwizzle(ref ty, _) => {
                // ty is the type of the assigned value, so it the return value
                ty.clone().to_lvalue()
            }
            Intrinsic2::SumAssignment(dty)
            | Intrinsic2::DifferenceAssignment(dty)
            | Intrinsic2::ProductAssignment(dty)
            | Intrinsic2::QuotientAssignment(dty)
            | Intrinsic2::RemainderAssignment(dty) => {
                // dty is the type of the assigned value, so it the return value
                Type::from_data(dty).to_lvalue()
            }
            Intrinsic2::AsDouble => Type::double().to_rvalue(),
            Intrinsic2::Cross => Type::floatn(3).to_rvalue(),
            Intrinsic2::Distance1 => Type::float().to_rvalue(),
            Intrinsic2::Distance2 => Type::float().to_rvalue(),
            Intrinsic2::Distance3 => Type::float().to_rvalue(),
            Intrinsic2::Distance4 => Type::float().to_rvalue(),
            Intrinsic2::DotI1 => Type::int().to_rvalue(),
            Intrinsic2::DotI2 => Type::int().to_rvalue(),
            Intrinsic2::DotI3 => Type::int().to_rvalue(),
            Intrinsic2::DotI4 => Type::int().to_rvalue(),
            Intrinsic2::DotF1 => Type::float().to_rvalue(),
            Intrinsic2::DotF2 => Type::float().to_rvalue(),
            Intrinsic2::DotF3 => Type::float().to_rvalue(),
            Intrinsic2::DotF4 => Type::float().to_rvalue(),
            Intrinsic2::MulMV(st, r, _) => Type::from_vector(st, r).to_rvalue(),
            Intrinsic2::MinI => Type::int().to_rvalue(),
            Intrinsic2::MinI2 => Type::intn(2).to_rvalue(),
            Intrinsic2::MinI3 => Type::intn(3).to_rvalue(),
            Intrinsic2::MinI4 => Type::intn(4).to_rvalue(),
            Intrinsic2::MinF => Type::float().to_rvalue(),
            Intrinsic2::MinF2 => Type::floatn(2).to_rvalue(),
            Intrinsic2::MinF3 => Type::floatn(3).to_rvalue(),
            Intrinsic2::MinF4 => Type::floatn(4).to_rvalue(),
            Intrinsic2::MaxI => Type::int().to_rvalue(),
            Intrinsic2::MaxI2 => Type::intn(2).to_rvalue(),
            Intrinsic2::MaxI3 => Type::intn(3).to_rvalue(),
            Intrinsic2::MaxI4 => Type::intn(4).to_rvalue(),
            Intrinsic2::MaxF => Type::float().to_rvalue(),
            Intrinsic2::MaxF2 => Type::floatn(2).to_rvalue(),
            Intrinsic2::MaxF3 => Type::floatn(3).to_rvalue(),
            Intrinsic2::MaxF4 => Type::floatn(4).to_rvalue(),
            Intrinsic2::Pow => Type::float().to_rvalue(),
            Intrinsic2::Pow2 => Type::floatn(2).to_rvalue(),
            Intrinsic2::Pow3 => Type::floatn(3).to_rvalue(),
            Intrinsic2::Pow4 => Type::floatn(4).to_rvalue(),
            Intrinsic2::Step => Type::float().to_rvalue(),
            Intrinsic2::Step2 => Type::floatn(2).to_rvalue(),
            Intrinsic2::Step3 => Type::floatn(3).to_rvalue(),
            Intrinsic2::Step4 => Type::floatn(4).to_rvalue(),
            Intrinsic2::BufferLoad(dty) => Type::from_data(dty).to_rvalue(),
            Intrinsic2::RWBufferLoad(dty) => Type::from_data(dty).to_rvalue(),
            Intrinsic2::StructuredBufferLoad(ref sty) => {
                Type::from_structured(sty.clone()).to_rvalue()
            }
            Intrinsic2::RWStructuredBufferLoad(ref sty) => {
                Type::from_structured(sty.clone()).to_rvalue()
            }
            Intrinsic2::Texture2DLoad(dty) => Type::from_data(dty).to_rvalue(),
            Intrinsic2::RWTexture2DLoad(dty) => Type::from_data(dty).to_rvalue(),
            Intrinsic2::ByteAddressBufferLoad => Type::uint().to_rvalue(),
            Intrinsic2::ByteAddressBufferLoad2 => Type::uintn(2).to_rvalue(),
            Intrinsic2::ByteAddressBufferLoad3 => Type::uintn(3).to_rvalue(),
            Intrinsic2::ByteAddressBufferLoad4 => Type::uintn(4).to_rvalue(),
            Intrinsic2::RWByteAddressBufferLoad => Type::uint().to_rvalue(),
            Intrinsic2::RWByteAddressBufferLoad2 => Type::uintn(2).to_rvalue(),
            Intrinsic2::RWByteAddressBufferLoad3 => Type::uintn(3).to_rvalue(),
            Intrinsic2::RWByteAddressBufferLoad4 => Type::uintn(4).to_rvalue(),
        }
    }
}

impl Intrinsic for Intrinsic3 {
    fn get_return_type(&self) -> ExpressionType {
        match *self {
            Intrinsic3::ClampI => Type::int().to_rvalue(),
            Intrinsic3::ClampI2 => Type::intn(2).to_rvalue(),
            Intrinsic3::ClampI3 => Type::intn(3).to_rvalue(),
            Intrinsic3::ClampI4 => Type::intn(4).to_rvalue(),
            Intrinsic3::ClampF => Type::float().to_rvalue(),
            Intrinsic3::ClampF2 => Type::floatn(2).to_rvalue(),
            Intrinsic3::ClampF3 => Type::floatn(3).to_rvalue(),
            Intrinsic3::ClampF4 => Type::floatn(4).to_rvalue(),
            Intrinsic3::Lerp => Type::float().to_rvalue(),
            Intrinsic3::Lerp2 => Type::floatn(2).to_rvalue(),
            Intrinsic3::Lerp3 => Type::floatn(3).to_rvalue(),
            Intrinsic3::Lerp4 => Type::floatn(4).to_rvalue(),
            Intrinsic3::Sincos => Type::void().to_rvalue(),
            Intrinsic3::Sincos2 => Type::void().to_rvalue(),
            Intrinsic3::Sincos3 => Type::void().to_rvalue(),
            Intrinsic3::Sincos4 => Type::void().to_rvalue(),
            Intrinsic3::SmoothStep => Type::float().to_rvalue(),
            Intrinsic3::SmoothStep2 => Type::floatn(2).to_rvalue(),
            Intrinsic3::SmoothStep3 => Type::floatn(3).to_rvalue(),
            Intrinsic3::SmoothStep4 => Type::floatn(4).to_rvalue(),
            Intrinsic3::Texture2DSample(dty) => Type::from_data(dty).to_rvalue(),
            Intrinsic3::RWTexture2DStore(_) => Type::void().to_rvalue(),
            Intrinsic3::RWByteAddressBufferStore => Type::void().to_rvalue(),
            Intrinsic3::RWByteAddressBufferStore2 => Type::void().to_rvalue(),
            Intrinsic3::RWByteAddressBufferStore3 => Type::void().to_rvalue(),
            Intrinsic3::RWByteAddressBufferStore4 => Type::void().to_rvalue(),
        }
    }
}

impl Intrinsic1 {
    pub fn get_param1_input_modifier(&self) -> InputModifier {
        match *self {
            Intrinsic1::Plus(_)
            | Intrinsic1::Minus(_)
            | Intrinsic1::LogicalNot(_)
            | Intrinsic1::BitwiseNot(_)
            | Intrinsic1::All(_)
            | Intrinsic1::Any(_)
            | Intrinsic1::AbsI
            | Intrinsic1::AbsI2
            | Intrinsic1::AbsI3
            | Intrinsic1::AbsI4
            | Intrinsic1::AbsF
            | Intrinsic1::AbsF2
            | Intrinsic1::AbsF3
            | Intrinsic1::AbsF4
            | Intrinsic1::Acos
            | Intrinsic1::Acos2
            | Intrinsic1::Acos3
            | Intrinsic1::Acos4
            | Intrinsic1::Asin
            | Intrinsic1::Asin2
            | Intrinsic1::Asin3
            | Intrinsic1::Asin4
            | Intrinsic1::AsIntU
            | Intrinsic1::AsIntU2
            | Intrinsic1::AsIntU3
            | Intrinsic1::AsIntU4
            | Intrinsic1::AsIntF
            | Intrinsic1::AsIntF2
            | Intrinsic1::AsIntF3
            | Intrinsic1::AsIntF4
            | Intrinsic1::AsUIntI
            | Intrinsic1::AsUIntI2
            | Intrinsic1::AsUIntI3
            | Intrinsic1::AsUIntI4
            | Intrinsic1::AsUIntF
            | Intrinsic1::AsUIntF2
            | Intrinsic1::AsUIntF3
            | Intrinsic1::AsUIntF4
            | Intrinsic1::AsFloatI
            | Intrinsic1::AsFloatI2
            | Intrinsic1::AsFloatI3
            | Intrinsic1::AsFloatI4
            | Intrinsic1::AsFloatU
            | Intrinsic1::AsFloatU2
            | Intrinsic1::AsFloatU3
            | Intrinsic1::AsFloatU4
            | Intrinsic1::AsFloatF
            | Intrinsic1::AsFloatF2
            | Intrinsic1::AsFloatF3
            | Intrinsic1::AsFloatF4
            | Intrinsic1::Cos
            | Intrinsic1::Cos2
            | Intrinsic1::Cos3
            | Intrinsic1::Cos4
            | Intrinsic1::Exp
            | Intrinsic1::Exp2
            | Intrinsic1::Exp3
            | Intrinsic1::Exp4
            | Intrinsic1::F16ToF32
            | Intrinsic1::F32ToF16
            | Intrinsic1::Floor
            | Intrinsic1::Floor2
            | Intrinsic1::Floor3
            | Intrinsic1::Floor4
            | Intrinsic1::IsNaN
            | Intrinsic1::IsNaN2
            | Intrinsic1::IsNaN3
            | Intrinsic1::IsNaN4
            | Intrinsic1::Length1
            | Intrinsic1::Length2
            | Intrinsic1::Length3
            | Intrinsic1::Length4
            | Intrinsic1::Normalize1
            | Intrinsic1::Normalize2
            | Intrinsic1::Normalize3
            | Intrinsic1::Normalize4
            | Intrinsic1::Saturate
            | Intrinsic1::Saturate2
            | Intrinsic1::Saturate3
            | Intrinsic1::Saturate4
            | Intrinsic1::SignI
            | Intrinsic1::SignI2
            | Intrinsic1::SignI3
            | Intrinsic1::SignI4
            | Intrinsic1::SignF
            | Intrinsic1::SignF2
            | Intrinsic1::SignF3
            | Intrinsic1::SignF4
            | Intrinsic1::Sin
            | Intrinsic1::Sin2
            | Intrinsic1::Sin3
            | Intrinsic1::Sin4
            | Intrinsic1::Sqrt
            | Intrinsic1::Sqrt2
            | Intrinsic1::Sqrt3
            | Intrinsic1::Sqrt4 => InputModifier::In,

            Intrinsic1::PrefixIncrement(_)
            | Intrinsic1::PrefixDecrement(_)
            | Intrinsic1::PostfixIncrement(_)
            | Intrinsic1::PostfixDecrement(_) => InputModifier::InOut,
        }
    }
}

impl Intrinsic2 {
    pub fn get_param1_input_modifier(&self) -> InputModifier {
        match *self {
            Intrinsic2::Add(_)
            | Intrinsic2::Subtract(_)
            | Intrinsic2::Multiply(_)
            | Intrinsic2::Divide(_)
            | Intrinsic2::Modulus(_)
            | Intrinsic2::LeftShift(_)
            | Intrinsic2::RightShift(_)
            | Intrinsic2::BitwiseAnd(_)
            | Intrinsic2::BitwiseOr(_)
            | Intrinsic2::BitwiseXor(_)
            | Intrinsic2::BooleanAnd(_)
            | Intrinsic2::BooleanOr(_)
            | Intrinsic2::LessThan(_)
            | Intrinsic2::LessEqual(_)
            | Intrinsic2::GreaterThan(_)
            | Intrinsic2::GreaterEqual(_)
            | Intrinsic2::Equality(_)
            | Intrinsic2::Inequality(_)
            | Intrinsic2::AsDouble
            | Intrinsic2::Cross
            | Intrinsic2::Distance1
            | Intrinsic2::Distance2
            | Intrinsic2::Distance3
            | Intrinsic2::Distance4
            | Intrinsic2::DotI1
            | Intrinsic2::DotI2
            | Intrinsic2::DotI3
            | Intrinsic2::DotI4
            | Intrinsic2::DotF1
            | Intrinsic2::DotF2
            | Intrinsic2::DotF3
            | Intrinsic2::DotF4
            | Intrinsic2::MulMV(..)
            | Intrinsic2::MinI
            | Intrinsic2::MinI2
            | Intrinsic2::MinI3
            | Intrinsic2::MinI4
            | Intrinsic2::MinF
            | Intrinsic2::MinF2
            | Intrinsic2::MinF3
            | Intrinsic2::MinF4
            | Intrinsic2::MaxI
            | Intrinsic2::MaxI2
            | Intrinsic2::MaxI3
            | Intrinsic2::MaxI4
            | Intrinsic2::MaxF
            | Intrinsic2::MaxF2
            | Intrinsic2::MaxF3
            | Intrinsic2::MaxF4
            | Intrinsic2::Pow
            | Intrinsic2::Pow2
            | Intrinsic2::Pow3
            | Intrinsic2::Pow4
            | Intrinsic2::Step
            | Intrinsic2::Step2
            | Intrinsic2::Step3
            | Intrinsic2::Step4
            | Intrinsic2::BufferLoad(_)
            | Intrinsic2::RWBufferLoad(_)
            | Intrinsic2::StructuredBufferLoad(_)
            | Intrinsic2::RWStructuredBufferLoad(_)
            | Intrinsic2::Texture2DLoad(_)
            | Intrinsic2::RWTexture2DLoad(_)
            | Intrinsic2::ByteAddressBufferLoad
            | Intrinsic2::ByteAddressBufferLoad2
            | Intrinsic2::ByteAddressBufferLoad3
            | Intrinsic2::ByteAddressBufferLoad4
            | Intrinsic2::RWByteAddressBufferLoad
            | Intrinsic2::RWByteAddressBufferLoad2
            | Intrinsic2::RWByteAddressBufferLoad3
            | Intrinsic2::RWByteAddressBufferLoad4 => InputModifier::In,

            Intrinsic2::Assignment(_) | Intrinsic2::AssignSwizzle(_, _) => InputModifier::Out,

            Intrinsic2::SumAssignment(_)
            | Intrinsic2::DifferenceAssignment(_)
            | Intrinsic2::ProductAssignment(_)
            | Intrinsic2::QuotientAssignment(_)
            | Intrinsic2::RemainderAssignment(_) => InputModifier::InOut,
        }
    }

    pub fn get_param2_input_modifier(&self) -> InputModifier {
        match *self {
            Intrinsic2::Add(_)
            | Intrinsic2::Subtract(_)
            | Intrinsic2::Multiply(_)
            | Intrinsic2::Divide(_)
            | Intrinsic2::Modulus(_)
            | Intrinsic2::LeftShift(_)
            | Intrinsic2::RightShift(_)
            | Intrinsic2::BitwiseAnd(_)
            | Intrinsic2::BitwiseOr(_)
            | Intrinsic2::BitwiseXor(_)
            | Intrinsic2::BooleanAnd(_)
            | Intrinsic2::BooleanOr(_)
            | Intrinsic2::LessThan(_)
            | Intrinsic2::LessEqual(_)
            | Intrinsic2::GreaterThan(_)
            | Intrinsic2::GreaterEqual(_)
            | Intrinsic2::Equality(_)
            | Intrinsic2::Inequality(_)
            | Intrinsic2::Assignment(_)
            | Intrinsic2::AssignSwizzle(_, _)
            | Intrinsic2::SumAssignment(_)
            | Intrinsic2::DifferenceAssignment(_)
            | Intrinsic2::ProductAssignment(_)
            | Intrinsic2::QuotientAssignment(_)
            | Intrinsic2::RemainderAssignment(_)
            | Intrinsic2::AsDouble
            | Intrinsic2::Cross
            | Intrinsic2::Distance1
            | Intrinsic2::Distance2
            | Intrinsic2::Distance3
            | Intrinsic2::Distance4
            | Intrinsic2::DotI1
            | Intrinsic2::DotI2
            | Intrinsic2::DotI3
            | Intrinsic2::DotI4
            | Intrinsic2::DotF1
            | Intrinsic2::DotF2
            | Intrinsic2::DotF3
            | Intrinsic2::DotF4
            | Intrinsic2::MulMV(..)
            | Intrinsic2::MinI
            | Intrinsic2::MinI2
            | Intrinsic2::MinI3
            | Intrinsic2::MinI4
            | Intrinsic2::MinF
            | Intrinsic2::MinF2
            | Intrinsic2::MinF3
            | Intrinsic2::MinF4
            | Intrinsic2::MaxI
            | Intrinsic2::MaxI2
            | Intrinsic2::MaxI3
            | Intrinsic2::MaxI4
            | Intrinsic2::MaxF
            | Intrinsic2::MaxF2
            | Intrinsic2::MaxF3
            | Intrinsic2::MaxF4
            | Intrinsic2::Pow
            | Intrinsic2::Pow2
            | Intrinsic2::Pow3
            | Intrinsic2::Pow4
            | Intrinsic2::Step
            | Intrinsic2::Step2
            | Intrinsic2::Step3
            | Intrinsic2::Step4
            | Intrinsic2::BufferLoad(_)
            | Intrinsic2::RWBufferLoad(_)
            | Intrinsic2::StructuredBufferLoad(_)
            | Intrinsic2::RWStructuredBufferLoad(_)
            | Intrinsic2::Texture2DLoad(_)
            | Intrinsic2::RWTexture2DLoad(_)
            | Intrinsic2::ByteAddressBufferLoad
            | Intrinsic2::ByteAddressBufferLoad2
            | Intrinsic2::ByteAddressBufferLoad3
            | Intrinsic2::ByteAddressBufferLoad4
            | Intrinsic2::RWByteAddressBufferLoad
            | Intrinsic2::RWByteAddressBufferLoad2
            | Intrinsic2::RWByteAddressBufferLoad3
            | Intrinsic2::RWByteAddressBufferLoad4 => InputModifier::In,
        }
    }
}

impl Intrinsic3 {
    pub fn get_param1_input_modifier(&self) -> InputModifier {
        match *self {
            Intrinsic3::ClampI
            | Intrinsic3::ClampI2
            | Intrinsic3::ClampI3
            | Intrinsic3::ClampI4
            | Intrinsic3::ClampF
            | Intrinsic3::ClampF2
            | Intrinsic3::ClampF3
            | Intrinsic3::ClampF4
            | Intrinsic3::Lerp
            | Intrinsic3::Lerp2
            | Intrinsic3::Lerp3
            | Intrinsic3::Lerp4
            | Intrinsic3::Sincos
            | Intrinsic3::Sincos2
            | Intrinsic3::Sincos3
            | Intrinsic3::Sincos4
            | Intrinsic3::SmoothStep
            | Intrinsic3::SmoothStep2
            | Intrinsic3::SmoothStep3
            | Intrinsic3::SmoothStep4
            | Intrinsic3::Texture2DSample(_)
            | Intrinsic3::RWTexture2DStore(_)
            | Intrinsic3::RWByteAddressBufferStore
            | Intrinsic3::RWByteAddressBufferStore2
            | Intrinsic3::RWByteAddressBufferStore3
            | Intrinsic3::RWByteAddressBufferStore4 => InputModifier::In,
        }
    }

    pub fn get_param2_input_modifier(&self) -> InputModifier {
        match *self {
            Intrinsic3::ClampI
            | Intrinsic3::ClampI2
            | Intrinsic3::ClampI3
            | Intrinsic3::ClampI4
            | Intrinsic3::ClampF
            | Intrinsic3::ClampF2
            | Intrinsic3::ClampF3
            | Intrinsic3::ClampF4
            | Intrinsic3::Lerp
            | Intrinsic3::Lerp2
            | Intrinsic3::Lerp3
            | Intrinsic3::Lerp4
            | Intrinsic3::SmoothStep
            | Intrinsic3::SmoothStep2
            | Intrinsic3::SmoothStep3
            | Intrinsic3::SmoothStep4
            | Intrinsic3::Texture2DSample(_)
            | Intrinsic3::RWTexture2DStore(_)
            | Intrinsic3::RWByteAddressBufferStore
            | Intrinsic3::RWByteAddressBufferStore2
            | Intrinsic3::RWByteAddressBufferStore3
            | Intrinsic3::RWByteAddressBufferStore4 => InputModifier::In,

            Intrinsic3::Sincos
            | Intrinsic3::Sincos2
            | Intrinsic3::Sincos3
            | Intrinsic3::Sincos4 => InputModifier::Out,
        }
    }

    pub fn get_param3_input_modifier(&self) -> InputModifier {
        match *self {
            Intrinsic3::ClampI
            | Intrinsic3::ClampI2
            | Intrinsic3::ClampI3
            | Intrinsic3::ClampI4
            | Intrinsic3::ClampF
            | Intrinsic3::ClampF2
            | Intrinsic3::ClampF3
            | Intrinsic3::ClampF4
            | Intrinsic3::Lerp
            | Intrinsic3::Lerp2
            | Intrinsic3::Lerp3
            | Intrinsic3::Lerp4
            | Intrinsic3::SmoothStep
            | Intrinsic3::SmoothStep2
            | Intrinsic3::SmoothStep3
            | Intrinsic3::SmoothStep4
            | Intrinsic3::Texture2DSample(_)
            | Intrinsic3::RWTexture2DStore(_)
            | Intrinsic3::RWByteAddressBufferStore
            | Intrinsic3::RWByteAddressBufferStore2
            | Intrinsic3::RWByteAddressBufferStore3
            | Intrinsic3::RWByteAddressBufferStore4 => InputModifier::In,

            Intrinsic3::Sincos
            | Intrinsic3::Sincos2
            | Intrinsic3::Sincos3
            | Intrinsic3::Sincos4 => InputModifier::Out,
        }
    }
}
