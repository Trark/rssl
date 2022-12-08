use crate::*;
use rssl_text::Located;

type IntrinsicDefinitionNoTemplates = (&'static str, Intrinsic, TypeLayout, &'static [ParamDef]);

type IntrinsicDefinition = (
    &'static str,
    Intrinsic,
    u32,
    TypeLayout,
    &'static [ParamDef],
);

struct ParamDef(pub TypeLayout, pub InputModifier);

use TypeLayout::Void;

const T_BOOL_TY: TypeLayout = TypeLayout::Scalar(ScalarType::Bool);
const T_BOOL2_TY: TypeLayout = TypeLayout::Vector(ScalarType::Bool, 2);
const T_BOOL3_TY: TypeLayout = TypeLayout::Vector(ScalarType::Bool, 3);
const T_BOOL4_TY: TypeLayout = TypeLayout::Vector(ScalarType::Bool, 4);
const T_INT_TY: TypeLayout = TypeLayout::Scalar(ScalarType::Int);
const T_INT1_TY: TypeLayout = TypeLayout::Vector(ScalarType::Int, 1);
const T_INT2_TY: TypeLayout = TypeLayout::Vector(ScalarType::Int, 2);
const T_INT3_TY: TypeLayout = TypeLayout::Vector(ScalarType::Int, 3);
const T_INT4_TY: TypeLayout = TypeLayout::Vector(ScalarType::Int, 4);
const T_UINT_TY: TypeLayout = TypeLayout::Scalar(ScalarType::UInt);
const T_UINT2_TY: TypeLayout = TypeLayout::Vector(ScalarType::UInt, 2);
const T_UINT3_TY: TypeLayout = TypeLayout::Vector(ScalarType::UInt, 3);
const T_UINT4_TY: TypeLayout = TypeLayout::Vector(ScalarType::UInt, 4);
const T_FLOAT_TY: TypeLayout = TypeLayout::Scalar(ScalarType::Float);
const T_FLOAT1_TY: TypeLayout = TypeLayout::Vector(ScalarType::Float, 1);
const T_FLOAT2_TY: TypeLayout = TypeLayout::Vector(ScalarType::Float, 2);
const T_FLOAT3_TY: TypeLayout = TypeLayout::Vector(ScalarType::Float, 3);
const T_FLOAT4_TY: TypeLayout = TypeLayout::Vector(ScalarType::Float, 4);
const T_DOUBLE_TY: TypeLayout = TypeLayout::Scalar(ScalarType::Double);
const T_SAMPLER_TY: TypeLayout = TypeLayout::Object(ObjectType::SamplerState);
const T_TEMPLATE0_TY: TypeLayout = TypeLayout::TemplateParam(TemplateTypeId(0));
const T_OBJECT_ARG_TY: TypeLayout = TypeLayout::TemplateParam(TemplateTypeId(u32::MAX));

const T_BOOL: ParamDef = ParamDef(T_BOOL_TY, InputModifier::In);
const T_BOOL2: ParamDef = ParamDef(T_BOOL2_TY, InputModifier::In);
const T_BOOL3: ParamDef = ParamDef(T_BOOL3_TY, InputModifier::In);
const T_BOOL4: ParamDef = ParamDef(T_BOOL4_TY, InputModifier::In);
const T_INT: ParamDef = ParamDef(T_INT_TY, InputModifier::In);
const T_INT1: ParamDef = ParamDef(T_INT1_TY, InputModifier::In);
const T_INT2: ParamDef = ParamDef(T_INT2_TY, InputModifier::In);
const T_INT3: ParamDef = ParamDef(T_INT3_TY, InputModifier::In);
const T_INT4: ParamDef = ParamDef(T_INT4_TY, InputModifier::In);
const T_UINT: ParamDef = ParamDef(T_UINT_TY, InputModifier::In);
const T_UINT2: ParamDef = ParamDef(T_UINT2_TY, InputModifier::In);
const T_UINT3: ParamDef = ParamDef(T_UINT3_TY, InputModifier::In);
const T_UINT4: ParamDef = ParamDef(T_UINT4_TY, InputModifier::In);
const T_UINT_OUT: ParamDef = ParamDef(T_UINT_TY, InputModifier::Out);
const T_FLOAT: ParamDef = ParamDef(T_FLOAT_TY, InputModifier::In);
const T_FLOAT1: ParamDef = ParamDef(T_FLOAT1_TY, InputModifier::In);
const T_FLOAT2: ParamDef = ParamDef(T_FLOAT2_TY, InputModifier::In);
const T_FLOAT3: ParamDef = ParamDef(T_FLOAT3_TY, InputModifier::In);
const T_FLOAT4: ParamDef = ParamDef(T_FLOAT4_TY, InputModifier::In);
const T_FLOAT_OUT: ParamDef = ParamDef(T_FLOAT_TY, InputModifier::Out);
const T_FLOAT2_OUT: ParamDef = ParamDef(T_FLOAT2_TY, InputModifier::Out);
const T_FLOAT3_OUT: ParamDef = ParamDef(T_FLOAT3_TY, InputModifier::Out);
const T_FLOAT4_OUT: ParamDef = ParamDef(T_FLOAT4_TY, InputModifier::Out);
const T_SAMPLER: ParamDef = ParamDef(T_SAMPLER_TY, InputModifier::In);
const T_TEMPLATE0: ParamDef = ParamDef(T_TEMPLATE0_TY, InputModifier::In);

#[rustfmt::skip]
const INTRINSICS: &[IntrinsicDefinitionNoTemplates] = &[
    ("AllMemoryBarrier", Intrinsic::AllMemoryBarrier, Void, &[]),
    ("AllMemoryBarrierWithGroupSync", Intrinsic::AllMemoryBarrierWithGroupSync, Void, &[]),
    ("DeviceMemoryBarrier", Intrinsic::DeviceMemoryBarrier, Void, &[]),
    ("DeviceMemoryBarrierWithGroupSync", Intrinsic::DeviceMemoryBarrierWithGroupSync, Void, &[]),
    ("GroupMemoryBarrier", Intrinsic::GroupMemoryBarrier, Void, &[]),
    ("GroupMemoryBarrierWithGroupSync", Intrinsic::GroupMemoryBarrierWithGroupSync, Void, &[]),

    ("all", Intrinsic::All, T_BOOL_TY, &[T_BOOL]),
    ("all", Intrinsic::All, T_BOOL_TY, &[T_BOOL2]),
    ("all", Intrinsic::All, T_BOOL_TY, &[T_BOOL3]),
    ("all", Intrinsic::All, T_BOOL_TY, &[T_BOOL4]),

    ("any", Intrinsic::Any, T_BOOL_TY, &[T_BOOL]),
    ("any", Intrinsic::Any, T_BOOL_TY, &[T_BOOL2]),
    ("any", Intrinsic::Any, T_BOOL_TY, &[T_BOOL3]),
    ("any", Intrinsic::Any, T_BOOL_TY, &[T_BOOL4]),

    ("abs", Intrinsic::Abs, T_INT_TY, &[T_INT]),
    ("abs", Intrinsic::Abs, T_INT2_TY, &[T_INT2]),
    ("abs", Intrinsic::Abs, T_INT3_TY, &[T_INT3]),
    ("abs", Intrinsic::Abs, T_INT4_TY, &[T_INT4]),
    ("abs", Intrinsic::Abs, T_FLOAT_TY, &[T_FLOAT]),
    ("abs", Intrinsic::Abs, T_FLOAT2_TY, &[T_FLOAT2]),
    ("abs", Intrinsic::Abs, T_FLOAT3_TY, &[T_FLOAT3]),
    ("abs", Intrinsic::Abs, T_FLOAT4_TY, &[T_FLOAT4]),

    ("acos", Intrinsic::Acos, T_FLOAT_TY, &[T_FLOAT]),
    ("acos", Intrinsic::Acos, T_FLOAT2_TY, &[T_FLOAT2]),
    ("acos", Intrinsic::Acos, T_FLOAT3_TY, &[T_FLOAT3]),
    ("acos", Intrinsic::Acos, T_FLOAT4_TY, &[T_FLOAT4]),

    ("asin", Intrinsic::Asin, T_FLOAT_TY, &[T_FLOAT]),
    ("asin", Intrinsic::Asin, T_FLOAT2_TY, &[T_FLOAT2]),
    ("asin", Intrinsic::Asin, T_FLOAT3_TY, &[T_FLOAT3]),
    ("asin", Intrinsic::Asin, T_FLOAT4_TY, &[T_FLOAT4]),

    ("asint", Intrinsic::AsInt, T_INT_TY, &[T_UINT]),
    ("asint", Intrinsic::AsInt, T_INT2_TY, &[T_UINT2]),
    ("asint", Intrinsic::AsInt, T_INT3_TY, &[T_UINT3]),
    ("asint", Intrinsic::AsInt, T_INT4_TY, &[T_UINT4]),
    ("asint", Intrinsic::AsInt, T_INT_TY, &[T_FLOAT]),
    ("asint", Intrinsic::AsInt, T_INT2_TY, &[T_FLOAT2]),
    ("asint", Intrinsic::AsInt, T_INT3_TY, &[T_FLOAT3]),
    ("asint", Intrinsic::AsInt, T_INT4_TY, &[T_FLOAT4]),

    ("asuint", Intrinsic::AsUInt, T_UINT_TY, &[T_INT]),
    ("asuint", Intrinsic::AsUInt, T_UINT2_TY, &[T_INT2]),
    ("asuint", Intrinsic::AsUInt, T_UINT3_TY, &[T_INT3]),
    ("asuint", Intrinsic::AsUInt, T_UINT4_TY, &[T_INT4]),
    ("asuint", Intrinsic::AsUInt, T_UINT_TY, &[T_FLOAT]),
    ("asuint", Intrinsic::AsUInt, T_UINT2_TY, &[T_FLOAT2]),
    ("asuint", Intrinsic::AsUInt, T_UINT3_TY, &[T_FLOAT3]),
    ("asuint", Intrinsic::AsUInt, T_UINT4_TY, &[T_FLOAT4]),

    ("asfloat", Intrinsic::AsFloat, T_FLOAT_TY, &[T_INT]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT2_TY, &[T_INT2]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT3_TY, &[T_INT3]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT4_TY, &[T_INT4]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT_TY, &[T_UINT]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT2_TY, &[T_UINT2]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT3_TY, &[T_UINT3]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT4_TY, &[T_UINT4]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT_TY, &[T_FLOAT]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT2_TY, &[T_FLOAT2]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT3_TY, &[T_FLOAT3]),
    ("asfloat", Intrinsic::AsFloat, T_FLOAT4_TY, &[T_FLOAT4]),

    ("exp", Intrinsic::Exp, T_FLOAT_TY, &[T_FLOAT]),
    ("exp", Intrinsic::Exp, T_FLOAT2_TY, &[T_FLOAT2]),
    ("exp", Intrinsic::Exp, T_FLOAT3_TY, &[T_FLOAT3]),
    ("exp", Intrinsic::Exp, T_FLOAT4_TY, &[T_FLOAT4]),

    ("asdouble", Intrinsic::AsDouble, T_DOUBLE_TY, &[T_UINT, T_UINT]),

    ("clamp", Intrinsic::Clamp, T_INT_TY, &[T_INT, T_INT, T_INT]),
    ("clamp", Intrinsic::Clamp, T_INT2_TY, &[T_INT2, T_INT2, T_INT2]),
    ("clamp", Intrinsic::Clamp, T_INT3_TY, &[T_INT3, T_INT3, T_INT3]),
    ("clamp", Intrinsic::Clamp, T_INT4_TY, &[T_INT4, T_INT4, T_INT4]),
    ("clamp", Intrinsic::Clamp, T_FLOAT_TY, &[T_FLOAT, T_FLOAT, T_FLOAT]),
    ("clamp", Intrinsic::Clamp, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2, T_FLOAT2]),
    ("clamp", Intrinsic::Clamp, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3, T_FLOAT3]),
    ("clamp", Intrinsic::Clamp, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4, T_FLOAT4]),

    ("cos", Intrinsic::Cos, T_FLOAT_TY, &[T_FLOAT]),
    ("cos", Intrinsic::Cos, T_FLOAT2_TY, &[T_FLOAT2]),
    ("cos", Intrinsic::Cos, T_FLOAT3_TY, &[T_FLOAT3]),
    ("cos", Intrinsic::Cos, T_FLOAT4_TY, &[T_FLOAT4]),

    ("cross", Intrinsic::Cross, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3]),

    ("distance", Intrinsic::Distance, T_FLOAT_TY, &[T_FLOAT1, T_FLOAT1]),
    ("distance", Intrinsic::Distance, T_FLOAT_TY, &[T_FLOAT2, T_FLOAT2]),
    ("distance", Intrinsic::Distance, T_FLOAT_TY, &[T_FLOAT3, T_FLOAT3]),
    ("distance", Intrinsic::Distance, T_FLOAT_TY, &[T_FLOAT4, T_FLOAT4]),

    ("dot", Intrinsic::Dot, T_INT_TY, &[T_INT1, T_INT1]),
    ("dot", Intrinsic::Dot, T_INT_TY, &[T_INT2, T_INT2]),
    ("dot", Intrinsic::Dot, T_INT_TY, &[T_INT3, T_INT3]),
    ("dot", Intrinsic::Dot, T_INT_TY, &[T_INT4, T_INT4]),
    ("dot", Intrinsic::Dot, T_FLOAT_TY, &[T_FLOAT1, T_FLOAT1]),
    ("dot", Intrinsic::Dot, T_FLOAT_TY, &[T_FLOAT2, T_FLOAT2]),
    ("dot", Intrinsic::Dot, T_FLOAT_TY, &[T_FLOAT3, T_FLOAT3]),
    ("dot", Intrinsic::Dot, T_FLOAT_TY, &[T_FLOAT4, T_FLOAT4]),

    ("mul", Intrinsic::Mul, T_FLOAT3_TY, &[ParamDef(TypeLayout::Matrix(ScalarType::Float, 3, 3), InputModifier::In), T_FLOAT3]),
    ("mul", Intrinsic::Mul, T_FLOAT4_TY, &[ParamDef(TypeLayout::Matrix(ScalarType::Float, 4, 4), InputModifier::In), T_FLOAT4]),

    ("f16tof32", Intrinsic::F16ToF32, T_FLOAT_TY, &[T_UINT]),
    ("f32tof16", Intrinsic::F32ToF16, T_UINT_TY, &[T_FLOAT]),

    ("floor", Intrinsic::Floor, T_FLOAT_TY, &[T_FLOAT]),
    ("floor", Intrinsic::Floor, T_FLOAT2_TY, &[T_FLOAT2]),
    ("floor", Intrinsic::Floor, T_FLOAT3_TY, &[T_FLOAT3]),
    ("floor", Intrinsic::Floor, T_FLOAT4_TY, &[T_FLOAT4]),

    ("lerp", Intrinsic::Lerp, T_FLOAT_TY, &[T_FLOAT, T_FLOAT, T_FLOAT]),
    ("lerp", Intrinsic::Lerp, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2, T_FLOAT2]),
    ("lerp", Intrinsic::Lerp, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3, T_FLOAT3]),
    ("lerp", Intrinsic::Lerp, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4, T_FLOAT4]),

    ("isnan", Intrinsic::IsNaN, T_BOOL_TY, &[T_FLOAT]),
    ("isnan", Intrinsic::IsNaN, T_BOOL2_TY, &[T_FLOAT2]),
    ("isnan", Intrinsic::IsNaN, T_BOOL3_TY, &[T_FLOAT3]),
    ("isnan", Intrinsic::IsNaN, T_BOOL4_TY, &[T_FLOAT4]),

    ("length", Intrinsic::Length, T_FLOAT_TY, &[T_FLOAT1]),
    ("length", Intrinsic::Length, T_FLOAT_TY, &[T_FLOAT2]),
    ("length", Intrinsic::Length, T_FLOAT_TY, &[T_FLOAT3]),
    ("length", Intrinsic::Length, T_FLOAT_TY, &[T_FLOAT4]),

    ("min", Intrinsic::Min, T_INT_TY, &[T_INT, T_INT]),
    ("min", Intrinsic::Min, T_INT2_TY, &[T_INT2, T_INT2]),
    ("min", Intrinsic::Min, T_INT3_TY, &[T_INT3, T_INT3]),
    ("min", Intrinsic::Min, T_INT4_TY, &[T_INT4, T_INT4]),
    ("min", Intrinsic::Min, T_FLOAT_TY, &[T_FLOAT, T_FLOAT]),
    ("min", Intrinsic::Min, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2]),
    ("min", Intrinsic::Min, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3]),
    ("min", Intrinsic::Min, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4]),

    ("max", Intrinsic::Max, T_INT_TY, &[T_INT, T_INT]),
    ("max", Intrinsic::Max, T_INT2_TY, &[T_INT2, T_INT2]),
    ("max", Intrinsic::Max, T_INT3_TY, &[T_INT3, T_INT3]),
    ("max", Intrinsic::Max, T_INT4_TY, &[T_INT4, T_INT4]),
    ("max", Intrinsic::Max, T_FLOAT_TY, &[T_FLOAT, T_FLOAT]),
    ("max", Intrinsic::Max, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2]),
    ("max", Intrinsic::Max, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3]),
    ("max", Intrinsic::Max, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4]),

    ("normalize", Intrinsic::Normalize, T_FLOAT1_TY, &[T_FLOAT1]),
    ("normalize", Intrinsic::Normalize, T_FLOAT2_TY, &[T_FLOAT2]),
    ("normalize", Intrinsic::Normalize, T_FLOAT3_TY, &[T_FLOAT3]),
    ("normalize", Intrinsic::Normalize, T_FLOAT4_TY, &[T_FLOAT4]),

    ("pow", Intrinsic::Pow, T_FLOAT_TY, &[T_FLOAT, T_FLOAT]),
    ("pow", Intrinsic::Pow, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2]),
    ("pow", Intrinsic::Pow, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3]),
    ("pow", Intrinsic::Pow, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4]),

    ("saturate", Intrinsic::Saturate, T_FLOAT_TY, &[T_FLOAT]),
    ("saturate", Intrinsic::Saturate, T_FLOAT2_TY, &[T_FLOAT2]),
    ("saturate", Intrinsic::Saturate, T_FLOAT3_TY, &[T_FLOAT3]),
    ("saturate", Intrinsic::Saturate, T_FLOAT4_TY, &[T_FLOAT4]),

    ("sign", Intrinsic::Sign, T_INT_TY, &[T_INT]),
    ("sign", Intrinsic::Sign, T_INT2_TY, &[T_INT2]),
    ("sign", Intrinsic::Sign, T_INT3_TY, &[T_INT3]),
    ("sign", Intrinsic::Sign, T_INT4_TY, &[T_INT4]),
    ("sign", Intrinsic::Sign, T_INT_TY, &[T_FLOAT]),
    ("sign", Intrinsic::Sign, T_INT2_TY, &[T_FLOAT2]),
    ("sign", Intrinsic::Sign, T_INT3_TY, &[T_FLOAT3]),
    ("sign", Intrinsic::Sign, T_INT4_TY, &[T_FLOAT4]),

    ("sin", Intrinsic::Sin, T_FLOAT_TY, &[T_FLOAT]),
    ("sin", Intrinsic::Sin, T_FLOAT2_TY, &[T_FLOAT2]),
    ("sin", Intrinsic::Sin, T_FLOAT3_TY, &[T_FLOAT3]),
    ("sin", Intrinsic::Sin, T_FLOAT4_TY, &[T_FLOAT4]),

    ("sincos", Intrinsic::Sincos, Void, &[T_FLOAT, T_FLOAT_OUT, T_FLOAT_OUT]),
    ("sincos", Intrinsic::Sincos, Void, &[T_FLOAT2, T_FLOAT2_OUT, T_FLOAT2_OUT]),
    ("sincos", Intrinsic::Sincos, Void, &[T_FLOAT3, T_FLOAT3_OUT, T_FLOAT3_OUT]),
    ("sincos", Intrinsic::Sincos, Void, &[T_FLOAT4, T_FLOAT4_OUT, T_FLOAT4_OUT]),

    ("smoothstep", Intrinsic::SmoothStep, T_FLOAT_TY, &[T_FLOAT, T_FLOAT, T_FLOAT]),
    ("smoothstep", Intrinsic::SmoothStep, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2, T_FLOAT2]),
    ("smoothstep", Intrinsic::SmoothStep, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3, T_FLOAT3]),
    ("smoothstep", Intrinsic::SmoothStep, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4, T_FLOAT4]),

    ("sqrt", Intrinsic::Sqrt, T_FLOAT_TY, &[T_FLOAT]),
    ("sqrt", Intrinsic::Sqrt, T_FLOAT2_TY, &[T_FLOAT2]),
    ("sqrt", Intrinsic::Sqrt, T_FLOAT3_TY, &[T_FLOAT3]),
    ("sqrt", Intrinsic::Sqrt, T_FLOAT4_TY, &[T_FLOAT4]),

    ("step", Intrinsic::Step, T_FLOAT_TY, &[T_FLOAT, T_FLOAT]),
    ("step", Intrinsic::Step, T_FLOAT2_TY, &[T_FLOAT2, T_FLOAT2]),
    ("step", Intrinsic::Step, T_FLOAT3_TY, &[T_FLOAT3, T_FLOAT3]),
    ("step", Intrinsic::Step, T_FLOAT4_TY, &[T_FLOAT4, T_FLOAT4]),
];

/// Create a collection of all the intrinsic functions
pub fn add_intrinsics(module: &mut Module) {
    for &(ref name, ref intrinsic, ref return_type, param_type_defs) in INTRINSICS {
        // Fetch the param types
        let param_types = param_type_defs
            .iter()
            .map(|p| ParamType(module.type_registry.register_type(p.0.clone()), p.1, None))
            .collect::<Vec<_>>();

        // Check generated return type for consistency
        {
            let mut expr_types = Vec::with_capacity(param_types.len());
            for param_type_def in param_type_defs {
                expr_types.push(ExpressionType(
                    param_type_def.0.clone(),
                    param_type_def.1.into(),
                ));
            }
            let generated_return_type = intrinsic.get_return_type(&expr_types).0;
            assert_eq!(*return_type, generated_return_type);
        }

        // Register the return type
        let return_type = FunctionReturn {
            return_type: module.type_registry.register_type(return_type.clone()),
            semantic: None,
        };

        // Make the signature
        let signature = FunctionSignature {
            return_type,
            template_params: TemplateParamCount(0),
            param_types,
        };

        // All intrinsic functions are in root namespace
        let full_name = ScopedName::unscoped(name.to_string());

        // Register the intrinsic as a function
        let id = module.function_registry.register_function(
            FunctionNameDefinition {
                name: Located::none(name.to_string()),
                full_name,
            },
            signature,
        );

        module
            .function_registry
            .set_intrinsic_data(id, intrinsic.clone());
    }
}

const BUFFER_INTRINSICS: &[IntrinsicDefinition] =
    &[("Load", Intrinsic::BufferLoad, 0, T_OBJECT_ARG_TY, &[T_INT])];

const RWBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[(
    "Load",
    Intrinsic::RWBufferLoad,
    0,
    T_OBJECT_ARG_TY,
    &[T_INT],
)];

const STRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[(
    "Load",
    Intrinsic::StructuredBufferLoad,
    0,
    T_OBJECT_ARG_TY,
    &[T_INT],
)];

const RWSTRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[(
    "Load",
    Intrinsic::RWStructuredBufferLoad,
    0,
    T_OBJECT_ARG_TY,
    &[T_INT],
)];

const TEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[
    (
        "Sample",
        Intrinsic::Texture2DSample,
        0,
        T_OBJECT_ARG_TY,
        &[T_SAMPLER, T_FLOAT2],
    ),
    (
        "Load",
        Intrinsic::Texture2DLoad,
        0,
        T_OBJECT_ARG_TY,
        &[T_INT3],
    ),
];

const RWTEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[(
    "Load",
    Intrinsic::RWTexture2DLoad,
    0,
    T_OBJECT_ARG_TY,
    &[T_INT2],
)];

const BYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    (
        "Load",
        Intrinsic::ByteAddressBufferLoad,
        0,
        T_UINT_TY,
        &[T_UINT],
    ),
    (
        "Load2",
        Intrinsic::ByteAddressBufferLoad2,
        0,
        T_UINT2_TY,
        &[T_UINT],
    ),
    (
        "Load3",
        Intrinsic::ByteAddressBufferLoad3,
        0,
        T_UINT3_TY,
        &[T_UINT],
    ),
    (
        "Load4",
        Intrinsic::ByteAddressBufferLoad4,
        0,
        T_UINT4_TY,
        &[T_UINT],
    ),
    (
        "Load",
        Intrinsic::ByteAddressBufferLoadT,
        1,
        T_TEMPLATE0_TY,
        &[T_UINT],
    ),
];

const RWBYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    (
        "Load",
        Intrinsic::RWByteAddressBufferLoad,
        0,
        T_UINT_TY,
        &[T_UINT],
    ),
    (
        "Load2",
        Intrinsic::RWByteAddressBufferLoad2,
        0,
        T_UINT2_TY,
        &[T_UINT],
    ),
    (
        "Load3",
        Intrinsic::RWByteAddressBufferLoad3,
        0,
        T_UINT3_TY,
        &[T_UINT],
    ),
    (
        "Load4",
        Intrinsic::RWByteAddressBufferLoad4,
        0,
        T_UINT4_TY,
        &[T_UINT],
    ),
    (
        "Store",
        Intrinsic::RWByteAddressBufferStore,
        0,
        Void,
        &[T_UINT, T_UINT],
    ),
    (
        "Store2",
        Intrinsic::RWByteAddressBufferStore2,
        0,
        Void,
        &[T_UINT, T_UINT2],
    ),
    (
        "Store3",
        Intrinsic::RWByteAddressBufferStore3,
        0,
        Void,
        &[T_UINT, T_UINT3],
    ),
    (
        "Store4",
        Intrinsic::RWByteAddressBufferStore4,
        0,
        Void,
        &[T_UINT, T_UINT4],
    ),
    (
        "InterlockedAdd",
        Intrinsic::RWByteAddressBufferInterlockedAdd,
        0,
        Void,
        &[T_UINT, T_UINT, T_UINT_OUT],
    ),
];
const BUFFERADDRESS_INTRINSICS: &[IntrinsicDefinition] = &[(
    "Load",
    Intrinsic::BufferAddressLoad,
    1,
    T_TEMPLATE0_TY,
    &[T_UINT],
)];
const RWBUFFERADDRESS_INTRINSICS: &[IntrinsicDefinition] = &[
    (
        "Load",
        Intrinsic::RWBufferAddressLoad,
        1,
        T_TEMPLATE0_TY,
        &[T_UINT],
    ),
    (
        "Store",
        Intrinsic::RWBufferAddressStore,
        1,
        Void,
        &[T_UINT, T_TEMPLATE0],
    ),
];

pub struct MethodDefinition {
    pub name: String,
    pub intrinsic: Intrinsic,
    pub signature: FunctionSignature,
}

/// Get the intrinsic methods for an intrinsic object
pub fn get_methods(module: &mut Module, object: ObjectType) -> Vec<MethodDefinition> {
    // Pick the functions based on the object base type
    let method_defs = match &object {
        ObjectType::Buffer(_) => BUFFER_INTRINSICS,
        ObjectType::RWBuffer(_) => RWBUFFER_INTRINSICS,
        ObjectType::StructuredBuffer(_) => STRUCTUREDBUFFER_INTRINSICS,
        ObjectType::RWStructuredBuffer(_) => RWSTRUCTUREDBUFFER_INTRINSICS,
        ObjectType::Texture2D(_) => TEXTURE2D_INTRINSICS,
        ObjectType::RWTexture2D(_) => RWTEXTURE2D_INTRINSICS,
        ObjectType::ByteAddressBuffer => BYTEADDRESSBUFFER_INTRINSICS,
        ObjectType::RWByteAddressBuffer => RWBYTEADDRESSBUFFER_INTRINSICS,
        ObjectType::BufferAddress => BUFFERADDRESS_INTRINSICS,
        ObjectType::RWBufferAddress => RWBUFFERADDRESS_INTRINSICS,
        _ => return Vec::new(),
    };

    // Get the object template type
    let inner_type = match &object {
        ObjectType::Buffer(dty)
        | ObjectType::RWBuffer(dty)
        | ObjectType::Texture2D(dty)
        | ObjectType::RWTexture2D(dty) => Some(TypeLayout::from(*dty)),
        ObjectType::StructuredBuffer(sty) | ObjectType::RWStructuredBuffer(sty) => {
            Some(TypeLayout::from(sty.clone()))
        }
        _ => None,
    };

    let mut methods = Vec::new();
    for &(method_name, ref intrinsic, template_param_count, ref return_type, param_type_defs) in
        method_defs
    {
        // Replace the object template type with the type arg
        let remap_inner = |ty| {
            if let TypeLayout::TemplateParam(v) = ty {
                if v.0 == u32::MAX {
                    return inner_type.clone().unwrap();
                }
            };
            ty
        };

        // Fetch and remap the param types
        let param_types = param_type_defs
            .iter()
            .map(|p| ParamType(module.type_registry.register_type(p.0.clone()), p.1, None))
            .collect::<Vec<_>>();

        // Fetch and remap the return type
        let return_type_layout = remap_inner(return_type.clone());

        // Check generated return type for consistency
        {
            let mut expr_types = Vec::with_capacity(param_types.len());
            expr_types.push(TypeLayout::Object(object.clone()).to_rvalue());
            for param_type_def in param_type_defs {
                expr_types.push(ExpressionType(
                    param_type_def.0.clone(),
                    param_type_def.1.into(),
                ));
            }
            let generated_return_type = intrinsic.get_return_type(&expr_types).0;
            assert_eq!(return_type_layout, generated_return_type);
        }

        methods.push(MethodDefinition {
            name: method_name.to_string(),
            intrinsic: intrinsic.clone(),
            signature: FunctionSignature {
                return_type: FunctionReturn {
                    return_type: module.type_registry.register_type(return_type_layout),
                    semantic: None,
                },
                template_params: TemplateParamCount(template_param_count),
                param_types,
            },
        });
    }

    methods
}
