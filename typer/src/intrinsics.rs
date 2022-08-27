use rssl_ir::*;

/// Creates intrinsic nodes from argument expressions
#[derive(PartialEq, Debug, Clone)]
pub enum IntrinsicFactory {
    Intrinsic0(Intrinsic0),
    Intrinsic1(Intrinsic1),
    Intrinsic2(Intrinsic2),
    Intrinsic3(Intrinsic3),
}

impl IntrinsicFactory {
    pub fn create_intrinsic(&self, param_values: &[Expression]) -> Expression {
        match *self {
            IntrinsicFactory::Intrinsic0(ref i) => {
                assert_eq!(param_values.len(), 0);
                Expression::Intrinsic0(i.clone())
            }
            IntrinsicFactory::Intrinsic1(ref i) => {
                assert_eq!(param_values.len(), 1);
                let p1 = Box::new(param_values[0].clone());
                Expression::Intrinsic1(i.clone(), p1)
            }
            IntrinsicFactory::Intrinsic2(ref i) => {
                assert_eq!(param_values.len(), 2);
                let p1 = Box::new(param_values[0].clone());
                let p2 = Box::new(param_values[1].clone());
                Expression::Intrinsic2(i.clone(), p1, p2)
            }
            IntrinsicFactory::Intrinsic3(ref i) => {
                assert_eq!(param_values.len(), 3);
                let p1 = Box::new(param_values[0].clone());
                let p2 = Box::new(param_values[1].clone());
                let p3 = Box::new(param_values[2].clone());
                Expression::Intrinsic3(i.clone(), p1, p2, p3)
            }
        }
    }
}

pub type IntrinsicDefinition = (&'static str, &'static [ParamType], IntrinsicFactory);

const T_MOD: TypeModifier = TypeModifier {
    is_const: false,
    row_order: RowOrder::Column,
    precise: false,
    volatile: false,
};

const T_BOOL_TY: Type = Type(TypeLayout::Scalar(ScalarType::Bool), T_MOD);
const T_BOOL1_TY: Type = Type(TypeLayout::Vector(ScalarType::Bool, 1), T_MOD);
const T_BOOL2_TY: Type = Type(TypeLayout::Vector(ScalarType::Bool, 2), T_MOD);
const T_BOOL3_TY: Type = Type(TypeLayout::Vector(ScalarType::Bool, 3), T_MOD);
const T_BOOL4_TY: Type = Type(TypeLayout::Vector(ScalarType::Bool, 4), T_MOD);
const T_INT_TY: Type = Type(TypeLayout::Scalar(ScalarType::Int), T_MOD);
const T_INT1_TY: Type = Type(TypeLayout::Vector(ScalarType::Int, 1), T_MOD);
const T_INT2_TY: Type = Type(TypeLayout::Vector(ScalarType::Int, 2), T_MOD);
const T_INT3_TY: Type = Type(TypeLayout::Vector(ScalarType::Int, 3), T_MOD);
const T_INT4_TY: Type = Type(TypeLayout::Vector(ScalarType::Int, 4), T_MOD);
const T_UINT_TY: Type = Type(TypeLayout::Scalar(ScalarType::UInt), T_MOD);
const T_UINT2_TY: Type = Type(TypeLayout::Vector(ScalarType::UInt, 2), T_MOD);
const T_UINT3_TY: Type = Type(TypeLayout::Vector(ScalarType::UInt, 3), T_MOD);
const T_UINT4_TY: Type = Type(TypeLayout::Vector(ScalarType::UInt, 4), T_MOD);
const T_FLOAT_TY: Type = Type(TypeLayout::Scalar(ScalarType::Float), T_MOD);
const T_FLOAT1_TY: Type = Type(TypeLayout::Vector(ScalarType::Float, 1), T_MOD);
const T_FLOAT2_TY: Type = Type(TypeLayout::Vector(ScalarType::Float, 2), T_MOD);
const T_FLOAT3_TY: Type = Type(TypeLayout::Vector(ScalarType::Float, 3), T_MOD);
const T_FLOAT4_TY: Type = Type(TypeLayout::Vector(ScalarType::Float, 4), T_MOD);
const T_SAMPLER_TY: Type = Type(TypeLayout::SamplerState, T_MOD);

const T_BOOL: ParamType = ParamType(T_BOOL_TY, InputModifier::In, None);
const T_BOOL1: ParamType = ParamType(T_BOOL1_TY, InputModifier::In, None);
const T_BOOL2: ParamType = ParamType(T_BOOL2_TY, InputModifier::In, None);
const T_BOOL3: ParamType = ParamType(T_BOOL3_TY, InputModifier::In, None);
const T_BOOL4: ParamType = ParamType(T_BOOL4_TY, InputModifier::In, None);
const T_INT: ParamType = ParamType(T_INT_TY, InputModifier::In, None);
const T_INT1: ParamType = ParamType(T_INT1_TY, InputModifier::In, None);
const T_INT2: ParamType = ParamType(T_INT2_TY, InputModifier::In, None);
const T_INT3: ParamType = ParamType(T_INT3_TY, InputModifier::In, None);
const T_INT4: ParamType = ParamType(T_INT4_TY, InputModifier::In, None);
const T_UINT: ParamType = ParamType(T_UINT_TY, InputModifier::In, None);
const T_UINT2: ParamType = ParamType(T_UINT2_TY, InputModifier::In, None);
const T_UINT3: ParamType = ParamType(T_UINT3_TY, InputModifier::In, None);
const T_UINT4: ParamType = ParamType(T_UINT4_TY, InputModifier::In, None);
const T_FLOAT: ParamType = ParamType(T_FLOAT_TY, InputModifier::In, None);
const T_FLOAT1: ParamType = ParamType(T_FLOAT1_TY, InputModifier::In, None);
const T_FLOAT2: ParamType = ParamType(T_FLOAT2_TY, InputModifier::In, None);
const T_FLOAT3: ParamType = ParamType(T_FLOAT3_TY, InputModifier::In, None);
const T_FLOAT4: ParamType = ParamType(T_FLOAT4_TY, InputModifier::In, None);
const T_FLOAT_OUT: ParamType = ParamType(T_FLOAT_TY, InputModifier::Out, None);
const T_FLOAT2_OUT: ParamType = ParamType(T_FLOAT2_TY, InputModifier::Out, None);
const T_FLOAT3_OUT: ParamType = ParamType(T_FLOAT3_TY, InputModifier::Out, None);
const T_FLOAT4_OUT: ParamType = ParamType(T_FLOAT4_TY, InputModifier::Out, None);
const T_SAMPLER: ParamType = ParamType(T_SAMPLER_TY, InputModifier::In, None);

use self::IntrinsicFactory::Intrinsic0 as I0;
use self::IntrinsicFactory::Intrinsic1 as I1;
use self::IntrinsicFactory::Intrinsic2 as I2;
use self::IntrinsicFactory::Intrinsic3 as I3;

#[rustfmt::skip]
const INTRINSICS: &[IntrinsicDefinition] = &[
    ("AllMemoryBarrier", &[], I0(Intrinsic0::AllMemoryBarrier)),
    ("AllMemoryBarrierWithGroupSync", &[], I0(Intrinsic0::AllMemoryBarrierWithGroupSync)),
    ("DeviceMemoryBarrier", &[], I0(Intrinsic0::DeviceMemoryBarrier)),
    ("DeviceMemoryBarrierWithGroupSync", &[], I0(Intrinsic0::DeviceMemoryBarrierWithGroupSync)),
    ("GroupMemoryBarrier", &[], I0(Intrinsic0::GroupMemoryBarrier)),
    ("GroupMemoryBarrierWithGroupSync", &[], I0(Intrinsic0::GroupMemoryBarrierWithGroupSync)),

    ("all", &[T_BOOL], I1(Intrinsic1::All(NumericDimension::Scalar))),
    ("all", &[T_BOOL1], I1(Intrinsic1::All(NumericDimension::Vector(1)))),
    ("all", &[T_BOOL2], I1(Intrinsic1::All(NumericDimension::Vector(2)))),
    ("all", &[T_BOOL3], I1(Intrinsic1::All(NumericDimension::Vector(3)))),
    ("all", &[T_BOOL4], I1(Intrinsic1::All(NumericDimension::Vector(4)))),

    ("any", &[T_BOOL], I1(Intrinsic1::Any(NumericDimension::Scalar))),
    ("any", &[T_BOOL1], I1(Intrinsic1::Any(NumericDimension::Vector(1)))),
    ("any", &[T_BOOL2], I1(Intrinsic1::Any(NumericDimension::Vector(2)))),
    ("any", &[T_BOOL3], I1(Intrinsic1::Any(NumericDimension::Vector(3)))),
    ("any", &[T_BOOL4], I1(Intrinsic1::Any(NumericDimension::Vector(4)))),

    ("abs", &[T_INT], I1(Intrinsic1::AbsI)),
    ("abs", &[T_INT2], I1(Intrinsic1::AbsI2)),
    ("abs", &[T_INT3], I1(Intrinsic1::AbsI3)),
    ("abs", &[T_INT4], I1(Intrinsic1::AbsI4)),
    ("abs", &[T_FLOAT], I1(Intrinsic1::AbsF)),
    ("abs", &[T_FLOAT2], I1(Intrinsic1::AbsF2)),
    ("abs", &[T_FLOAT3], I1(Intrinsic1::AbsF3)),
    ("abs", &[T_FLOAT4], I1(Intrinsic1::AbsF4)),

    ("acos", &[T_FLOAT], I1(Intrinsic1::Acos)),
    ("acos", &[T_FLOAT2], I1(Intrinsic1::Acos2)),
    ("acos", &[T_FLOAT3], I1(Intrinsic1::Acos3)),
    ("acos", &[T_FLOAT4], I1(Intrinsic1::Acos4)),

    ("asin", &[T_FLOAT], I1(Intrinsic1::Asin)),
    ("asin", &[T_FLOAT2], I1(Intrinsic1::Asin2)),
    ("asin", &[T_FLOAT3], I1(Intrinsic1::Asin3)),
    ("asin", &[T_FLOAT4], I1(Intrinsic1::Asin4)),

    ("asint", &[T_UINT], I1(Intrinsic1::AsIntU)),
    ("asint", &[T_UINT2], I1(Intrinsic1::AsIntU2)),
    ("asint", &[T_UINT3], I1(Intrinsic1::AsIntU3)),
    ("asint", &[T_UINT4], I1(Intrinsic1::AsIntU4)),
    ("asint", &[T_FLOAT], I1(Intrinsic1::AsIntF)),
    ("asint", &[T_FLOAT2], I1(Intrinsic1::AsIntF2)),
    ("asint", &[T_FLOAT3], I1(Intrinsic1::AsIntF3)),
    ("asint", &[T_FLOAT4], I1(Intrinsic1::AsIntF4)),

    ("asuint", &[T_INT], I1(Intrinsic1::AsUIntI)),
    ("asuint", &[T_INT2], I1(Intrinsic1::AsUIntI2)),
    ("asuint", &[T_INT3], I1(Intrinsic1::AsUIntI3)),
    ("asuint", &[T_INT4], I1(Intrinsic1::AsUIntI4)),
    ("asuint", &[T_FLOAT], I1(Intrinsic1::AsUIntF)),
    ("asuint", &[T_FLOAT2], I1(Intrinsic1::AsUIntF2)),
    ("asuint", &[T_FLOAT3], I1(Intrinsic1::AsUIntF3)),
    ("asuint", &[T_FLOAT4], I1(Intrinsic1::AsUIntF4)),

    ("asfloat", &[T_INT], I1(Intrinsic1::AsFloatI)),
    ("asfloat", &[T_INT2], I1(Intrinsic1::AsFloatI2)),
    ("asfloat", &[T_INT3], I1(Intrinsic1::AsFloatI3)),
    ("asfloat", &[T_INT4], I1(Intrinsic1::AsFloatI4)),
    ("asfloat", &[T_UINT], I1(Intrinsic1::AsFloatU)),
    ("asfloat", &[T_UINT2], I1(Intrinsic1::AsFloatU2)),
    ("asfloat", &[T_UINT3], I1(Intrinsic1::AsFloatU3)),
    ("asfloat", &[T_UINT4], I1(Intrinsic1::AsFloatU4)),
    ("asfloat", &[T_FLOAT], I1(Intrinsic1::AsFloatF)),
    ("asfloat", &[T_FLOAT2], I1(Intrinsic1::AsFloatF2)),
    ("asfloat", &[T_FLOAT3], I1(Intrinsic1::AsFloatF3)),
    ("asfloat", &[T_FLOAT4], I1(Intrinsic1::AsFloatF4)),

    ("exp", &[T_FLOAT], I1(Intrinsic1::Exp)),
    ("exp", &[T_FLOAT2], I1(Intrinsic1::Exp2)),
    ("exp", &[T_FLOAT3], I1(Intrinsic1::Exp3)),
    ("exp", &[T_FLOAT4], I1(Intrinsic1::Exp4)),

    ("asdouble", &[T_UINT, T_UINT], I2(Intrinsic2::AsDouble)),

    ("clamp", &[T_INT, T_INT, T_INT], I3(Intrinsic3::ClampI)),
    ("clamp", &[T_INT2, T_INT2, T_INT2], I3(Intrinsic3::ClampI2)),
    ("clamp", &[T_INT3, T_INT3, T_INT3], I3(Intrinsic3::ClampI3)),
    ("clamp", &[T_INT4, T_INT4, T_INT4], I3(Intrinsic3::ClampI4)),
    ("clamp", &[T_FLOAT, T_FLOAT, T_FLOAT], I3(Intrinsic3::ClampF)),
    ("clamp", &[T_FLOAT2, T_FLOAT2, T_FLOAT2], I3(Intrinsic3::ClampF2)),
    ("clamp", &[T_FLOAT3, T_FLOAT3, T_FLOAT3], I3(Intrinsic3::ClampF3)),
    ("clamp", &[T_FLOAT4, T_FLOAT4, T_FLOAT4], I3(Intrinsic3::ClampF4)),

    ("cos", &[T_FLOAT],  I1(Intrinsic1::Cos)),
    ("cos", &[T_FLOAT2], I1(Intrinsic1::Cos2)),
    ("cos", &[T_FLOAT3], I1(Intrinsic1::Cos3)),
    ("cos", &[T_FLOAT4], I1(Intrinsic1::Cos4)),

    ("cross", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::Cross)),

    ("distance", &[T_FLOAT1, T_FLOAT1], I2(Intrinsic2::Distance1)),
    ("distance", &[T_FLOAT2, T_FLOAT2], I2(Intrinsic2::Distance2)),
    ("distance", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::Distance3)),
    ("distance", &[T_FLOAT4, T_FLOAT4], I2(Intrinsic2::Distance4)),

    ("dot", &[T_INT1, T_INT1], I2(Intrinsic2::DotI1)),
    ("dot", &[T_INT2, T_INT2], I2(Intrinsic2::DotI2)),
    ("dot", &[T_INT3, T_INT3], I2(Intrinsic2::DotI3)),
    ("dot", &[T_INT4, T_INT4], I2(Intrinsic2::DotI4)),
    ("dot", &[T_FLOAT1, T_FLOAT1], I2(Intrinsic2::DotF1)),
    ("dot", &[T_FLOAT2, T_FLOAT2], I2(Intrinsic2::DotF2)),
    ("dot", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::DotF3)),
    ("dot", &[T_FLOAT4, T_FLOAT4], I2(Intrinsic2::DotF4)),

    ("mul", &[ParamType(Type(TypeLayout::Matrix(ScalarType::Float, 3, 3), T_MOD), InputModifier::In, None), T_FLOAT3], I2(Intrinsic2::MulMV(ScalarType::Float, 3, 3))),
    ("mul", &[ParamType(Type(TypeLayout::Matrix(ScalarType::Float, 4, 4), T_MOD), InputModifier::In, None), T_FLOAT4], I2(Intrinsic2::MulMV(ScalarType::Float, 4, 4))),

    ("f16tof32", &[T_UINT], I1(Intrinsic1::F16ToF32)),
    ("f32tof16", &[T_FLOAT], I1(Intrinsic1::F32ToF16)),

    ("floor", &[T_FLOAT], I1(Intrinsic1::Floor)),
    ("floor", &[T_FLOAT2], I1(Intrinsic1::Floor2)),
    ("floor", &[T_FLOAT3], I1(Intrinsic1::Floor3)),
    ("floor", &[T_FLOAT4], I1(Intrinsic1::Floor4)),

    ("lerp", &[T_FLOAT, T_FLOAT, T_FLOAT], I3(Intrinsic3::Lerp)),
    ("lerp", &[T_FLOAT2, T_FLOAT2, T_FLOAT2], I3(Intrinsic3::Lerp2)),
    ("lerp", &[T_FLOAT3, T_FLOAT3, T_FLOAT3], I3(Intrinsic3::Lerp3)),
    ("lerp", &[T_FLOAT4, T_FLOAT4, T_FLOAT4], I3(Intrinsic3::Lerp4)),

    ("isnan", &[T_FLOAT], I1(Intrinsic1::IsNaN)),
    ("isnan", &[T_FLOAT2], I1(Intrinsic1::IsNaN2)),
    ("isnan", &[T_FLOAT3], I1(Intrinsic1::IsNaN3)),
    ("isnan", &[T_FLOAT4], I1(Intrinsic1::IsNaN4)),

    ("length", &[T_FLOAT1], I1(Intrinsic1::Length1)),
    ("length", &[T_FLOAT2], I1(Intrinsic1::Length2)),
    ("length", &[T_FLOAT3], I1(Intrinsic1::Length3)),
    ("length", &[T_FLOAT4], I1(Intrinsic1::Length4)),

    ("min", &[T_INT, T_INT], I2(Intrinsic2::MinI)),
    ("min", &[T_INT2, T_INT2], I2(Intrinsic2::MinI2)),
    ("min", &[T_INT3, T_INT3], I2(Intrinsic2::MinI3)),
    ("min", &[T_INT4, T_INT4], I2(Intrinsic2::MinI4)),
    ("min", &[T_FLOAT, T_FLOAT], I2(Intrinsic2::MinF)),
    ("min", &[T_FLOAT2, T_FLOAT2], I2(Intrinsic2::MinF2)),
    ("min", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::MinF3)),
    ("min", &[T_FLOAT4, T_FLOAT4], I2(Intrinsic2::MinF4)),

    ("max", &[T_INT, T_INT], I2(Intrinsic2::MaxI)),
    ("max", &[T_INT2, T_INT2], I2(Intrinsic2::MaxI2)),
    ("max", &[T_INT3, T_INT3], I2(Intrinsic2::MaxI3)),
    ("max", &[T_INT4, T_INT4], I2(Intrinsic2::MaxI4)),
    ("max", &[T_FLOAT, T_FLOAT], I2(Intrinsic2::MaxF)),
    ("max", &[T_FLOAT2, T_FLOAT2], I2(Intrinsic2::MaxF2)),
    ("max", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::MaxF3)),
    ("max", &[T_FLOAT4, T_FLOAT4], I2(Intrinsic2::MaxF4)),

    ("normalize", &[T_FLOAT1], I1(Intrinsic1::Normalize1)),
    ("normalize", &[T_FLOAT2], I1(Intrinsic1::Normalize2)),
    ("normalize", &[T_FLOAT3], I1(Intrinsic1::Normalize3)),
    ("normalize", &[T_FLOAT4], I1(Intrinsic1::Normalize4)),

    ("pow", &[T_FLOAT, T_FLOAT], I2(Intrinsic2::Pow)),
    ("pow", &[T_FLOAT2, T_FLOAT2], I2(Intrinsic2::Pow2)),
    ("pow", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::Pow3)),
    ("pow", &[T_FLOAT4, T_FLOAT4], I2(Intrinsic2::Pow4)),

    ("saturate", &[T_FLOAT], I1(Intrinsic1::Saturate)),
    ("saturate", &[T_FLOAT2], I1(Intrinsic1::Saturate2)),
    ("saturate", &[T_FLOAT3], I1(Intrinsic1::Saturate3)),
    ("saturate", &[T_FLOAT4], I1(Intrinsic1::Saturate4)),

    ("sign", &[T_INT], I1(Intrinsic1::SignI)),
    ("sign", &[T_INT2], I1(Intrinsic1::SignI2)),
    ("sign", &[T_INT3], I1(Intrinsic1::SignI3)),
    ("sign", &[T_INT4], I1(Intrinsic1::SignI4)),
    ("sign", &[T_FLOAT], I1(Intrinsic1::SignF)),
    ("sign", &[T_FLOAT2], I1(Intrinsic1::SignF2)),
    ("sign", &[T_FLOAT3], I1(Intrinsic1::SignF3)),
    ("sign", &[T_FLOAT4], I1(Intrinsic1::SignF4)),

    ("sin", &[T_FLOAT],  I1(Intrinsic1::Sin)),
    ("sin", &[T_FLOAT2], I1(Intrinsic1::Sin2)),
    ("sin", &[T_FLOAT3], I1(Intrinsic1::Sin3)),
    ("sin", &[T_FLOAT4], I1(Intrinsic1::Sin4)),

    ("sincos", &[T_FLOAT, T_FLOAT_OUT, T_FLOAT_OUT], I3(Intrinsic3::Sincos)),
    ("sincos", &[T_FLOAT2, T_FLOAT2_OUT, T_FLOAT2_OUT], I3(Intrinsic3::Sincos2)),
    ("sincos", &[T_FLOAT3, T_FLOAT3_OUT, T_FLOAT3_OUT], I3(Intrinsic3::Sincos3)),
    ("sincos", &[T_FLOAT4, T_FLOAT4_OUT, T_FLOAT4_OUT], I3(Intrinsic3::Sincos4)),

    ("smoothstep", &[T_FLOAT, T_FLOAT, T_FLOAT], I3(Intrinsic3::SmoothStep)),
    ("smoothstep", &[T_FLOAT2, T_FLOAT2, T_FLOAT2], I3(Intrinsic3::SmoothStep2)),
    ("smoothstep", &[T_FLOAT3, T_FLOAT3, T_FLOAT3], I3(Intrinsic3::SmoothStep3)),
    ("smoothstep", &[T_FLOAT4, T_FLOAT4, T_FLOAT4], I3(Intrinsic3::SmoothStep4)),

    ("sqrt", &[T_FLOAT], I1(Intrinsic1::Sqrt)),
    ("sqrt", &[T_FLOAT2], I1(Intrinsic1::Sqrt2)),
    ("sqrt", &[T_FLOAT3], I1(Intrinsic1::Sqrt3)),
    ("sqrt", &[T_FLOAT4], I1(Intrinsic1::Sqrt4)),

    ("step", &[T_FLOAT, T_FLOAT], I2(Intrinsic2::Step)),
    ("step", &[T_FLOAT2, T_FLOAT2], I2(Intrinsic2::Step2)),
    ("step", &[T_FLOAT3, T_FLOAT3], I2(Intrinsic2::Step3)),
    ("step", &[T_FLOAT4, T_FLOAT4], I2(Intrinsic2::Step4)),
];

pub fn get_intrinsics() -> &'static [IntrinsicDefinition] {
    INTRINSICS
}

pub struct MethodDefinition(
    pub ObjectType,
    pub String,
    pub Vec<(Vec<ParamType>, IntrinsicFactory)>,
);

#[rustfmt::skip]
pub fn get_method(object: &ObjectType, name: &str) -> Result<MethodDefinition, ()> {

    type MethodT = (&'static str, Vec<ParamType>, IntrinsicFactory);
    type FmResult = Result<MethodDefinition, ()>;
    fn find_method(object: &ObjectType, defs: &[MethodT], name: &str) -> FmResult {
        let mut methods = vec![];
        for &(method_name, ref param_types, ref factory) in defs {
            if method_name == name {
                methods.push((param_types.to_vec(), factory.clone()));
            };
        }
        if !methods.is_empty() {
            Ok(MethodDefinition(object.clone(), name.to_string(), methods))
        } else {
            Err(())
        }
    }

    use rssl_ir::Intrinsic2::*;
    use rssl_ir::Intrinsic3::*;

    match *object {
        ObjectType::Buffer(data_type) => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_INT], I2(BufferLoad(data_type))),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWBuffer(data_type) => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_INT], I2(RWBufferLoad(data_type))),
            ];
            find_method(object, methods, name)
        }
        ObjectType::StructuredBuffer(ref structured_type) => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_INT], I2(StructuredBufferLoad(structured_type.clone()))),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWStructuredBuffer(ref structured_type) => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_INT], I2(RWStructuredBufferLoad(structured_type.clone()))),
            ];
            find_method(object, methods, name)
        }
        ObjectType::Texture2D(data_type) => {
            let methods: &[MethodT] = &[
                ("Sample", vec![T_SAMPLER, T_FLOAT2], I3(Texture2DSample(data_type))),
                ("Load", vec![T_INT3], I2(Texture2DLoad(data_type))),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWTexture2D(data_type) => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_INT2], I2(RWTexture2DLoad(data_type))),
            ];
            find_method(object, methods, name)
        }
        ObjectType::ByteAddressBuffer => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_UINT], I2(ByteAddressBufferLoad)),
                ("Load2", vec![T_UINT], I2(ByteAddressBufferLoad2)),
                ("Load3", vec![T_UINT], I2(ByteAddressBufferLoad3)),
                ("Load4", vec![T_UINT], I2(ByteAddressBufferLoad4)),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWByteAddressBuffer => {
            let methods: &[MethodT] = &[
                ("Load", vec![T_UINT], I2(RWByteAddressBufferLoad)),
                ("Load2", vec![T_UINT], I2(RWByteAddressBufferLoad2)),
                ("Load3", vec![T_UINT], I2(RWByteAddressBufferLoad3)),
                ("Load4", vec![T_UINT], I2(RWByteAddressBufferLoad4)),
                ("Store", vec![T_UINT, T_UINT], I3(RWByteAddressBufferStore)),
                ("Store2", vec![T_UINT, T_UINT2], I3(RWByteAddressBufferStore2)),
                ("Store3", vec![T_UINT, T_UINT3], I3(RWByteAddressBufferStore3)),
                ("Store4", vec![T_UINT, T_UINT4], I3(RWByteAddressBufferStore4)),
            ];
            find_method(object, methods, name)
        }
        _ => Err(())
    }
}

#[test]
fn test_param_count() {
    fn param_count(factory: &IntrinsicFactory) -> usize {
        match *factory {
            IntrinsicFactory::Intrinsic0(_) => 0,
            IntrinsicFactory::Intrinsic1(_) => 1,
            IntrinsicFactory::Intrinsic2(_) => 2,
            IntrinsicFactory::Intrinsic3(_) => 3,
        }
    }

    for &(_, ref types, ref factory) in INTRINSICS {
        assert_eq!(types.len(), param_count(factory));
    }
}
