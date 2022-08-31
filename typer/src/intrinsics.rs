use rssl_ir::*;

/// Creates intrinsic nodes from argument expressions
#[derive(PartialEq, Debug, Clone)]
pub enum IntrinsicFactory {
    Function(Intrinsic, &'static [ParamType]),
    Method(Intrinsic, &'static [ParamType]),
}

impl IntrinsicFactory {
    pub fn create_intrinsic(&self, param_values: &[Expression]) -> Expression {
        match *self {
            IntrinsicFactory::Function(ref i, param_types) => {
                assert_eq!(param_values.len(), param_types.len());
                let mut exprs = Vec::with_capacity(param_values.len());
                for param_value in param_values {
                    exprs.push(param_value.clone());
                }
                Expression::Intrinsic(i.clone(), exprs)
            }
            IntrinsicFactory::Method(ref i, param_types) => {
                assert_eq!(param_values.len(), param_types.len() + 1);
                let mut exprs = Vec::with_capacity(param_values.len());
                for param_value in param_values {
                    exprs.push(param_value.clone());
                }
                Expression::Intrinsic(i.clone(), exprs)
            }
        }
    }

    pub fn get_return_type(&self) -> ExpressionType {
        match *self {
            IntrinsicFactory::Function(ref i, param_types) => {
                let mut expr_types = Vec::with_capacity(param_types.len());
                for param_type in param_types {
                    expr_types.push(param_type.clone().into());
                }
                i.get_return_type(&expr_types)
            }
            IntrinsicFactory::Method(..) => {
                panic!("get_return_type called on method")
            }
        }
    }

    pub fn get_method_return_type(&self, object_type: ExpressionType) -> ExpressionType {
        match *self {
            IntrinsicFactory::Function(..) => {
                panic!("get_method_return_type called on non-method")
            }
            IntrinsicFactory::Method(ref i, param_types) => {
                let mut expr_types = Vec::with_capacity(param_types.len() + 1);
                expr_types.push(object_type);
                for param_type in param_types {
                    expr_types.push(param_type.clone().into());
                }
                i.get_return_type(&expr_types)
            }
        }
    }
}

pub type IntrinsicDefinition = (&'static str, Intrinsic, &'static [ParamType]);

const T_MOD: TypeModifier = TypeModifier {
    is_const: false,
    row_order: RowOrder::Column,
    precise: false,
    volatile: false,
};

const T_BOOL_TY: Type = Type(TypeLayout::Scalar(ScalarType::Bool), T_MOD);
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

#[rustfmt::skip]
const INTRINSICS: &[IntrinsicDefinition] = &[
    ("AllMemoryBarrier", Intrinsic::AllMemoryBarrier, &[]),
    ("AllMemoryBarrierWithGroupSync", Intrinsic::AllMemoryBarrierWithGroupSync, &[]),
    ("DeviceMemoryBarrier", Intrinsic::DeviceMemoryBarrier, &[]),
    ("DeviceMemoryBarrierWithGroupSync", Intrinsic::DeviceMemoryBarrierWithGroupSync, &[]),
    ("GroupMemoryBarrier", Intrinsic::GroupMemoryBarrier, &[]),
    ("GroupMemoryBarrierWithGroupSync", Intrinsic::GroupMemoryBarrierWithGroupSync, &[]),

    ("all", Intrinsic::All, &[T_BOOL]),
    ("all", Intrinsic::All, &[T_BOOL2]),
    ("all", Intrinsic::All, &[T_BOOL3]),
    ("all", Intrinsic::All, &[T_BOOL4]),

    ("any", Intrinsic::Any, &[T_BOOL]),
    ("any", Intrinsic::Any, &[T_BOOL2]),
    ("any", Intrinsic::Any, &[T_BOOL3]),
    ("any", Intrinsic::Any, &[T_BOOL4]),

    ("abs", Intrinsic::Abs, &[T_INT]),
    ("abs", Intrinsic::Abs, &[T_INT2]),
    ("abs", Intrinsic::Abs, &[T_INT3]),
    ("abs", Intrinsic::Abs, &[T_INT4]),
    ("abs", Intrinsic::Abs, &[T_FLOAT]),
    ("abs", Intrinsic::Abs, &[T_FLOAT2]),
    ("abs", Intrinsic::Abs, &[T_FLOAT3]),
    ("abs", Intrinsic::Abs, &[T_FLOAT4]),

    ("acos", Intrinsic::Acos, &[T_FLOAT]),
    ("acos", Intrinsic::Acos, &[T_FLOAT2]),
    ("acos", Intrinsic::Acos, &[T_FLOAT3]),
    ("acos", Intrinsic::Acos, &[T_FLOAT4]),

    ("asin", Intrinsic::Asin, &[T_FLOAT]),
    ("asin", Intrinsic::Asin, &[T_FLOAT2]),
    ("asin", Intrinsic::Asin, &[T_FLOAT3]),
    ("asin", Intrinsic::Asin, &[T_FLOAT4]),

    ("asint", Intrinsic::AsInt, &[T_UINT]),
    ("asint", Intrinsic::AsInt, &[T_UINT2]),
    ("asint", Intrinsic::AsInt, &[T_UINT3]),
    ("asint", Intrinsic::AsInt, &[T_UINT4]),
    ("asint", Intrinsic::AsInt, &[T_FLOAT]),
    ("asint", Intrinsic::AsInt, &[T_FLOAT2]),
    ("asint", Intrinsic::AsInt, &[T_FLOAT3]),
    ("asint", Intrinsic::AsInt, &[T_FLOAT4]),

    ("asuint", Intrinsic::AsUInt, &[T_INT]),
    ("asuint", Intrinsic::AsUInt, &[T_INT2]),
    ("asuint", Intrinsic::AsUInt, &[T_INT3]),
    ("asuint", Intrinsic::AsUInt, &[T_INT4]),
    ("asuint", Intrinsic::AsUInt, &[T_FLOAT]),
    ("asuint", Intrinsic::AsUInt, &[T_FLOAT2]),
    ("asuint", Intrinsic::AsUInt, &[T_FLOAT3]),
    ("asuint", Intrinsic::AsUInt, &[T_FLOAT4]),

    ("asfloat", Intrinsic::AsFloat, &[T_INT]),
    ("asfloat", Intrinsic::AsFloat, &[T_INT2]),
    ("asfloat", Intrinsic::AsFloat, &[T_INT3]),
    ("asfloat", Intrinsic::AsFloat, &[T_INT4]),
    ("asfloat", Intrinsic::AsFloat, &[T_UINT]),
    ("asfloat", Intrinsic::AsFloat, &[T_UINT2]),
    ("asfloat", Intrinsic::AsFloat, &[T_UINT3]),
    ("asfloat", Intrinsic::AsFloat, &[T_UINT4]),
    ("asfloat", Intrinsic::AsFloat, &[T_FLOAT]),
    ("asfloat", Intrinsic::AsFloat, &[T_FLOAT2]),
    ("asfloat", Intrinsic::AsFloat, &[T_FLOAT3]),
    ("asfloat", Intrinsic::AsFloat, &[T_FLOAT4]),

    ("exp", Intrinsic::Exp, &[T_FLOAT]),
    ("exp", Intrinsic::Exp, &[T_FLOAT2]),
    ("exp", Intrinsic::Exp, &[T_FLOAT3]),
    ("exp", Intrinsic::Exp, &[T_FLOAT4]),

    ("asdouble", Intrinsic::AsDouble, &[T_UINT, T_UINT]),

    ("clamp", Intrinsic::Clamp, &[T_INT, T_INT, T_INT]),
    ("clamp", Intrinsic::Clamp, &[T_INT2, T_INT2, T_INT2]),
    ("clamp", Intrinsic::Clamp, &[T_INT3, T_INT3, T_INT3]),
    ("clamp", Intrinsic::Clamp, &[T_INT4, T_INT4, T_INT4]),
    ("clamp", Intrinsic::Clamp, &[T_FLOAT, T_FLOAT, T_FLOAT]),
    ("clamp", Intrinsic::Clamp, &[T_FLOAT2, T_FLOAT2, T_FLOAT2]),
    ("clamp", Intrinsic::Clamp, &[T_FLOAT3, T_FLOAT3, T_FLOAT3]),
    ("clamp", Intrinsic::Clamp, &[T_FLOAT4, T_FLOAT4, T_FLOAT4]),

    ("cos", Intrinsic::Cos, &[T_FLOAT]),
    ("cos", Intrinsic::Cos, &[T_FLOAT2]),
    ("cos", Intrinsic::Cos, &[T_FLOAT3]),
    ("cos", Intrinsic::Cos, &[T_FLOAT4]),

    ("cross", Intrinsic::Cross, &[T_FLOAT3, T_FLOAT3]),

    ("distance", Intrinsic::Distance, &[T_FLOAT1, T_FLOAT1]),
    ("distance", Intrinsic::Distance, &[T_FLOAT2, T_FLOAT2]),
    ("distance", Intrinsic::Distance, &[T_FLOAT3, T_FLOAT3]),
    ("distance", Intrinsic::Distance, &[T_FLOAT4, T_FLOAT4]),

    ("dot", Intrinsic::Dot, &[T_INT1, T_INT1]),
    ("dot", Intrinsic::Dot, &[T_INT2, T_INT2]),
    ("dot", Intrinsic::Dot, &[T_INT3, T_INT3]),
    ("dot", Intrinsic::Dot, &[T_INT4, T_INT4]),
    ("dot", Intrinsic::Dot, &[T_FLOAT1, T_FLOAT1]),
    ("dot", Intrinsic::Dot, &[T_FLOAT2, T_FLOAT2]),
    ("dot", Intrinsic::Dot, &[T_FLOAT3, T_FLOAT3]),
    ("dot", Intrinsic::Dot, &[T_FLOAT4, T_FLOAT4]),

    ("mul", Intrinsic::Mul, &[ParamType(Type(TypeLayout::Matrix(ScalarType::Float, 3, 3), T_MOD), InputModifier::In, None), T_FLOAT3]),
    ("mul",Intrinsic::Mul, &[ParamType(Type(TypeLayout::Matrix(ScalarType::Float, 4, 4), T_MOD), InputModifier::In, None), T_FLOAT4]),

    ("f16tof32", Intrinsic::F16ToF32, &[T_UINT]),
    ("f32tof16", Intrinsic::F32ToF16, &[T_FLOAT]),

    ("floor", Intrinsic::Floor, &[T_FLOAT]),
    ("floor", Intrinsic::Floor, &[T_FLOAT2]),
    ("floor", Intrinsic::Floor, &[T_FLOAT3]),
    ("floor", Intrinsic::Floor, &[T_FLOAT4]),

    ("lerp", Intrinsic::Lerp, &[T_FLOAT, T_FLOAT, T_FLOAT]),
    ("lerp", Intrinsic::Lerp, &[T_FLOAT2, T_FLOAT2, T_FLOAT2]),
    ("lerp", Intrinsic::Lerp, &[T_FLOAT3, T_FLOAT3, T_FLOAT3]),
    ("lerp", Intrinsic::Lerp, &[T_FLOAT4, T_FLOAT4, T_FLOAT4]),

    ("isnan", Intrinsic::IsNaN, &[T_FLOAT]),
    ("isnan", Intrinsic::IsNaN, &[T_FLOAT2]),
    ("isnan", Intrinsic::IsNaN, &[T_FLOAT3]),
    ("isnan", Intrinsic::IsNaN, &[T_FLOAT4]),

    ("length", Intrinsic::Length, &[T_FLOAT1]),
    ("length", Intrinsic::Length, &[T_FLOAT2]),
    ("length", Intrinsic::Length, &[T_FLOAT3]),
    ("length", Intrinsic::Length, &[T_FLOAT4]),

    ("min", Intrinsic::Min, &[T_INT, T_INT]),
    ("min", Intrinsic::Min, &[T_INT2, T_INT2]),
    ("min", Intrinsic::Min, &[T_INT3, T_INT3]),
    ("min", Intrinsic::Min, &[T_INT4, T_INT4]),
    ("min", Intrinsic::Min, &[T_FLOAT, T_FLOAT]),
    ("min", Intrinsic::Min, &[T_FLOAT2, T_FLOAT2]),
    ("min", Intrinsic::Min, &[T_FLOAT3, T_FLOAT3]),
    ("min", Intrinsic::Min, &[T_FLOAT4, T_FLOAT4]),

    ("max", Intrinsic::Max, &[T_INT, T_INT]),
    ("max", Intrinsic::Max, &[T_INT2, T_INT2]),
    ("max", Intrinsic::Max, &[T_INT3, T_INT3]),
    ("max", Intrinsic::Max, &[T_INT4, T_INT4]),
    ("max", Intrinsic::Max, &[T_FLOAT, T_FLOAT]),
    ("max", Intrinsic::Max, &[T_FLOAT2, T_FLOAT2]),
    ("max", Intrinsic::Max, &[T_FLOAT3, T_FLOAT3]),
    ("max", Intrinsic::Max, &[T_FLOAT4, T_FLOAT4]),

    ("normalize", Intrinsic::Normalize, &[T_FLOAT1]),
    ("normalize", Intrinsic::Normalize, &[T_FLOAT2]),
    ("normalize", Intrinsic::Normalize, &[T_FLOAT3]),
    ("normalize", Intrinsic::Normalize, &[T_FLOAT4]),

    ("pow", Intrinsic::Pow, &[T_FLOAT, T_FLOAT]),
    ("pow", Intrinsic::Pow, &[T_FLOAT2, T_FLOAT2]),
    ("pow", Intrinsic::Pow, &[T_FLOAT3, T_FLOAT3]),
    ("pow", Intrinsic::Pow, &[T_FLOAT4, T_FLOAT4]),

    ("saturate", Intrinsic::Saturate, &[T_FLOAT]),
    ("saturate", Intrinsic::Saturate, &[T_FLOAT2]),
    ("saturate", Intrinsic::Saturate, &[T_FLOAT3]),
    ("saturate", Intrinsic::Saturate, &[T_FLOAT4]),

    ("sign", Intrinsic::Sign, &[T_INT]),
    ("sign", Intrinsic::Sign, &[T_INT2]),
    ("sign", Intrinsic::Sign, &[T_INT3]),
    ("sign", Intrinsic::Sign, &[T_INT4]),
    ("sign", Intrinsic::Sign, &[T_FLOAT]),
    ("sign", Intrinsic::Sign, &[T_FLOAT2]),
    ("sign", Intrinsic::Sign, &[T_FLOAT3]),
    ("sign", Intrinsic::Sign, &[T_FLOAT4]),

    ("sin", Intrinsic::Sin, &[T_FLOAT]),
    ("sin", Intrinsic::Sin, &[T_FLOAT2]),
    ("sin", Intrinsic::Sin, &[T_FLOAT3]),
    ("sin", Intrinsic::Sin, &[T_FLOAT4]),

    ("sincos", Intrinsic::Sincos, &[T_FLOAT, T_FLOAT_OUT, T_FLOAT_OUT]),
    ("sincos", Intrinsic::Sincos, &[T_FLOAT2, T_FLOAT2_OUT, T_FLOAT2_OUT]),
    ("sincos", Intrinsic::Sincos, &[T_FLOAT3, T_FLOAT3_OUT, T_FLOAT3_OUT]),
    ("sincos", Intrinsic::Sincos, &[T_FLOAT4, T_FLOAT4_OUT, T_FLOAT4_OUT]),

    ("smoothstep", Intrinsic::SmoothStep, &[T_FLOAT, T_FLOAT, T_FLOAT]),
    ("smoothstep", Intrinsic::SmoothStep, &[T_FLOAT2, T_FLOAT2, T_FLOAT2]),
    ("smoothstep", Intrinsic::SmoothStep, &[T_FLOAT3, T_FLOAT3, T_FLOAT3]),
    ("smoothstep", Intrinsic::SmoothStep, &[T_FLOAT4, T_FLOAT4, T_FLOAT4]),

    ("sqrt", Intrinsic::Sqrt, &[T_FLOAT]),
    ("sqrt", Intrinsic::Sqrt, &[T_FLOAT2]),
    ("sqrt", Intrinsic::Sqrt, &[T_FLOAT3]),
    ("sqrt", Intrinsic::Sqrt, &[T_FLOAT4]),

    ("step", Intrinsic::Step, &[T_FLOAT, T_FLOAT]),
    ("step", Intrinsic::Step, &[T_FLOAT2, T_FLOAT2]),
    ("step", Intrinsic::Step, &[T_FLOAT3, T_FLOAT3]),
    ("step", Intrinsic::Step, &[T_FLOAT4, T_FLOAT4]),
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

    type MethodT = (&'static str, Intrinsic, &'static [ParamType]);
    type FmResult = Result<MethodDefinition, ()>;
    fn find_method(object: &ObjectType, defs: &[MethodT], name: &str) -> FmResult {
        let mut methods = vec![];
        for &(method_name, ref intrinsic, param_types) in defs {
            if method_name == name {
                methods.push((param_types.to_vec(), IntrinsicFactory::Method(intrinsic.clone(), param_types)));
            };
        }
        if !methods.is_empty() {
            Ok(MethodDefinition(object.clone(), name.to_string(), methods))
        } else {
            Err(())
        }
    }

    use rssl_ir::Intrinsic::*;

    match *object {
        ObjectType::Buffer(_) => {
            let methods: &[MethodT] = &[
                ("Load", BufferLoad, &[T_INT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWBuffer(_) => {
            let methods: &[MethodT] = &[
                ("Load", RWBufferLoad, &[T_INT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::StructuredBuffer(_) => {
            let methods: &[MethodT] = &[
                ("Load", StructuredBufferLoad, &[T_INT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWStructuredBuffer(_) => {
            let methods: &[MethodT] = &[
                ("Load", RWStructuredBufferLoad, &[T_INT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::Texture2D(_) => {
            let methods: &[MethodT] = &[
                ("Sample", Texture2DSample, &[T_SAMPLER, T_FLOAT2]),
                ("Load", Texture2DLoad, &[T_INT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWTexture2D(_) => {
            let methods: &[MethodT] = &[
                ("Load", RWTexture2DLoad, &[T_INT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::ByteAddressBuffer => {
            let methods: &[MethodT] = &[
                ("Load", ByteAddressBufferLoad, &[T_UINT]),
                ("Load2", ByteAddressBufferLoad2, &[T_UINT]),
                ("Load3", ByteAddressBufferLoad3, &[T_UINT]),
                ("Load4", ByteAddressBufferLoad4, &[T_UINT]),
            ];
            find_method(object, methods, name)
        }
        ObjectType::RWByteAddressBuffer => {
            let methods: &[MethodT] = &[
                ("Load", RWByteAddressBufferLoad, &[T_UINT]),
                ("Load2", RWByteAddressBufferLoad2, &[T_UINT]),
                ("Load3", RWByteAddressBufferLoad3, &[T_UINT]),
                ("Load4", RWByteAddressBufferLoad4, &[T_UINT]),
                ("Store", RWByteAddressBufferStore, &[T_UINT, T_UINT]),
                ("Store2", RWByteAddressBufferStore2, &[T_UINT, T_UINT]),
                ("Store3", RWByteAddressBufferStore3, &[T_UINT, T_UINT]),
                ("Store4", RWByteAddressBufferStore4, &[T_UINT, T_UINT]),
            ];
            find_method(object, methods, name)
        }
        _ => Err(())
    }
}
