use crate::*;

/// An intrinsic built in function
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub enum Intrinsic {
    // Unary operations
    PrefixIncrement,
    PrefixDecrement,
    PostfixIncrement,
    PostfixDecrement,
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,

    // Binary operations
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BooleanAnd,
    BooleanOr,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equality,
    Inequality,
    Assignment,
    SumAssignment,
    DifferenceAssignment,
    ProductAssignment,
    QuotientAssignment,
    RemainderAssignment,

    // Synchronization
    AllMemoryBarrier,
    AllMemoryBarrierWithGroupSync,
    DeviceMemoryBarrier,
    DeviceMemoryBarrierWithGroupSync,
    GroupMemoryBarrier,
    GroupMemoryBarrierWithGroupSync,

    // Bit casting
    AsInt,
    AsUInt,
    AsFloat,
    AsDouble,

    All,
    Any,

    Abs,

    // Transcendental functions
    Acos,
    Asin,
    Cos,
    Sin,
    Exp,
    Sqrt,
    Pow,
    Sincos,

    F16ToF32,
    F32ToF16,

    Floor,

    IsNaN,

    Length,
    Normalize,

    Saturate,

    Sign,

    Cross,
    Distance,
    Dot,

    Mul,

    Min,
    Max,

    Step,

    Clamp,
    Lerp,
    SmoothStep,

    // Buffer methods
    BufferLoad,
    RWBufferLoad,

    // StructuredBuffer methods
    StructuredBufferLoad,
    RWStructuredBufferLoad,

    // ByteAddressBuffer methods
    ByteAddressBufferLoad,
    ByteAddressBufferLoad2,
    ByteAddressBufferLoad3,
    ByteAddressBufferLoad4,
    ByteAddressBufferLoadT,

    // RWByteAddressBuffer methods
    RWByteAddressBufferLoad,
    RWByteAddressBufferLoad2,
    RWByteAddressBufferLoad3,
    RWByteAddressBufferLoad4,
    RWByteAddressBufferStore,
    RWByteAddressBufferStore2,
    RWByteAddressBufferStore3,
    RWByteAddressBufferStore4,
    RWByteAddressBufferInterlockedAdd,

    // Texture2D methods
    Texture2DLoad,
    Texture2DSample,

    // RWTexture2D methods
    RWTexture2DLoad,
}

impl Intrinsic {
    pub fn get_return_type(&self, param_types: &[ExpressionType]) -> ExpressionType {
        use Intrinsic::*;
        match *self {
            PrefixIncrement | PrefixDecrement => {
                assert_eq!(param_types.len(), 1);
                param_types[0].clone()
            }
            PostfixIncrement | PostfixDecrement | Plus | Minus => {
                assert_eq!(param_types.len(), 1);
                param_types[0].0.clone().to_rvalue()
            }
            LogicalNot => {
                assert_eq!(param_types.len(), 1);
                match param_types[0].0 .0 {
                    TypeLayout::Scalar(_) => Type::bool().to_rvalue(),
                    TypeLayout::Vector(_, x) => Type::booln(x).to_rvalue(),
                    _ => panic!("invalid logical not intrinsic"),
                }
            }
            BitwiseNot => {
                assert_eq!(param_types.len(), 1);
                param_types[0].0.clone().to_rvalue()
            }

            Add | Subtract | Multiply | Divide | Modulus | LeftShift | RightShift | BitwiseAnd
            | BitwiseOr | BitwiseXor | BooleanAnd | BooleanOr => {
                assert_eq!(param_types.len(), 2);
                Type::most_significant_data_type(&param_types[0].0, &param_types[1].0).to_rvalue()
            }
            LessThan | LessEqual | GreaterThan | GreaterEqual | Equality | Inequality => {
                assert_eq!(param_types.len(), 2);
                Type::most_significant_data_type(&param_types[0].0, &param_types[1].0)
                    .transform_scalar(ScalarType::Bool)
                    .to_rvalue()
            }
            Assignment | SumAssignment | DifferenceAssignment | ProductAssignment
            | QuotientAssignment | RemainderAssignment => {
                assert_eq!(param_types.len(), 2);
                assert_eq!(param_types[0].0, param_types[1].0);
                assert_eq!(param_types[0].1, ValueType::Lvalue);
                param_types[0].clone()
            }
            AllMemoryBarrier
            | AllMemoryBarrierWithGroupSync
            | DeviceMemoryBarrier
            | DeviceMemoryBarrierWithGroupSync
            | GroupMemoryBarrier
            | GroupMemoryBarrierWithGroupSync => Type::void().to_rvalue(),
            AsInt => {
                assert_eq!(param_types.len(), 1);
                param_types[0]
                    .0
                    .clone()
                    .transform_scalar(ScalarType::Int)
                    .to_rvalue()
            }
            AsUInt => {
                assert_eq!(param_types.len(), 1);
                param_types[0]
                    .0
                    .clone()
                    .transform_scalar(ScalarType::UInt)
                    .to_rvalue()
            }
            AsFloat => {
                assert_eq!(param_types.len(), 1);
                param_types[0]
                    .0
                    .clone()
                    .transform_scalar(ScalarType::Float)
                    .to_rvalue()
            }
            AsDouble => {
                assert_eq!(param_types.len(), 2);
                Type::double().to_rvalue()
            }
            All | Any => Type::bool().to_rvalue(),
            Abs => {
                assert_eq!(param_types.len(), 1);
                param_types[0].0.clone().to_rvalue()
            }
            Acos | Asin | Cos | Exp | Sin | Sqrt => {
                assert_eq!(param_types.len(), 1);
                param_types[0].0.clone().to_rvalue()
            }
            Pow => {
                assert_eq!(param_types.len(), 2);
                param_types[0].0.clone().to_rvalue()
            }
            Sincos => Type::void().to_rvalue(),
            F16ToF32 => {
                assert_eq!(param_types.len(), 1);
                Type::float().to_rvalue()
            }
            F32ToF16 => {
                assert_eq!(param_types.len(), 1);
                Type::uint().to_rvalue()
            }
            Floor => {
                assert_eq!(param_types.len(), 1);
                param_types[0].0.clone().to_rvalue()
            }
            IsNaN => {
                assert_eq!(param_types.len(), 1);
                param_types[0]
                    .0
                    .clone()
                    .transform_scalar(ScalarType::Bool)
                    .to_rvalue()
            }
            Length => {
                assert_eq!(param_types.len(), 1);
                Type::float().to_rvalue()
            }
            Normalize | Saturate => {
                assert_eq!(param_types.len(), 1);
                param_types[0].0.clone().to_rvalue()
            }
            Sign => {
                assert_eq!(param_types.len(), 1);
                param_types[0]
                    .0
                    .clone()
                    .transform_scalar(ScalarType::Int)
                    .to_rvalue()
            }
            Cross => {
                assert_eq!(param_types.len(), 2);
                Type::floatn(3).to_rvalue()
            }
            Distance => {
                assert_eq!(param_types.len(), 2);
                Type::float().to_rvalue()
            }
            Dot => {
                assert_eq!(param_types.len(), 2);
                let Type(tyl, modifier) = param_types[0].0.clone();
                let tyl = match tyl {
                    TypeLayout::Scalar(st) => TypeLayout::Scalar(st),
                    TypeLayout::Vector(st, _) => TypeLayout::Scalar(st),
                    _ => panic!("Invalid dot"),
                };
                Type(tyl, modifier).to_rvalue()
            }
            Mul => {
                assert_eq!(param_types.len(), 2);
                let Type(tyl0, _) = param_types[0].0.clone();
                let Type(tyl1, mod1) = param_types[1].0.clone();
                let tyl = match (tyl0, tyl1) {
                    (
                        TypeLayout::Matrix(ScalarType::Float, 3, 3),
                        TypeLayout::Vector(ScalarType::Float, 3),
                    ) => TypeLayout::Vector(ScalarType::Float, 3),
                    (
                        TypeLayout::Matrix(ScalarType::Float, 4, 4),
                        TypeLayout::Vector(ScalarType::Float, 4),
                    ) => TypeLayout::Vector(ScalarType::Float, 4),
                    _ => panic!("Invalid mul"),
                };
                Type(tyl, mod1).to_rvalue()
            }
            Min | Max | Step => {
                assert_eq!(param_types.len(), 2);
                param_types[0].0.clone().to_rvalue()
            }
            Clamp | Lerp | SmoothStep => {
                assert_eq!(param_types.len(), 3);
                param_types[0].0.clone().to_rvalue()
            }
            BufferLoad => {
                assert_eq!(param_types.len(), 2);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::Buffer(dty)) => Type::from_data(dty).to_rvalue(),
                    _ => panic!("Invalid BufferLoad"),
                }
            }
            RWBufferLoad => {
                assert_eq!(param_types.len(), 2);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::RWBuffer(dty)) => {
                        Type::from_data(dty).to_rvalue()
                    }
                    _ => panic!("Invalid RWBufferLoad"),
                }
            }
            StructuredBufferLoad => {
                assert_eq!(param_types.len(), 2);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::StructuredBuffer(ref sty)) => {
                        Type::from_structured(sty.clone()).to_rvalue()
                    }
                    _ => panic!("Invalid StructuredBufferLoad"),
                }
            }
            RWStructuredBufferLoad => {
                assert_eq!(param_types.len(), 2);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::RWStructuredBuffer(ref sty)) => {
                        Type::from_structured(sty.clone()).to_rvalue()
                    }
                    _ => panic!("Invalid RWStructuredBufferLoad"),
                }
            }
            ByteAddressBufferLoad => {
                assert_eq!(param_types.len(), 2);
                Type::uint().to_rvalue()
            }
            ByteAddressBufferLoad2 => {
                assert_eq!(param_types.len(), 2);
                Type::uintn(2).to_rvalue()
            }
            ByteAddressBufferLoad3 => {
                assert_eq!(param_types.len(), 2);
                Type::uintn(3).to_rvalue()
            }
            ByteAddressBufferLoad4 => {
                assert_eq!(param_types.len(), 2);
                Type::uintn(4).to_rvalue()
            }
            ByteAddressBufferLoadT => {
                assert_eq!(param_types.len(), 2);
                Type::from_layout(TypeLayout::TemplateParam(TemplateTypeId(0))).to_rvalue()
            }
            RWByteAddressBufferLoad => {
                assert_eq!(param_types.len(), 2);
                Type::uint().to_rvalue()
            }
            RWByteAddressBufferLoad2 => {
                assert_eq!(param_types.len(), 2);
                Type::uintn(2).to_rvalue()
            }
            RWByteAddressBufferLoad3 => {
                assert_eq!(param_types.len(), 2);
                Type::uintn(3).to_rvalue()
            }
            RWByteAddressBufferLoad4 => {
                assert_eq!(param_types.len(), 2);
                Type::uintn(4).to_rvalue()
            }
            RWByteAddressBufferStore
            | RWByteAddressBufferStore2
            | RWByteAddressBufferStore3
            | RWByteAddressBufferStore4 => {
                assert_eq!(param_types.len(), 3);
                Type::void().to_rvalue()
            }
            RWByteAddressBufferInterlockedAdd => {
                assert_eq!(param_types.len(), 4);
                Type::void().to_rvalue()
            }
            Texture2DLoad => {
                assert_eq!(param_types.len(), 2);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::Texture2D(dty)) => {
                        Type::from_data(dty).to_rvalue()
                    }
                    _ => panic!("Invalid Texture2DLoad"),
                }
            }
            Texture2DSample => {
                assert_eq!(param_types.len(), 3);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::RWTexture2D(dty)) => {
                        Type::from_data(dty).to_rvalue()
                    }
                    _ => panic!("Invalid Texture2DSample"),
                }
            }
            RWTexture2DLoad => {
                assert_eq!(param_types.len(), 2);
                match param_types[0].0 .0 {
                    TypeLayout::Object(ObjectType::RWTexture2D(dty)) => {
                        Type::from_data(dty).to_rvalue()
                    }
                    _ => panic!("Invalid RWTexture2DLoad"),
                }
            }
        }
    }
}
