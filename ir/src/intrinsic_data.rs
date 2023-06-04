use crate::*;
use rssl_text::Located;

struct IntrinsicDefinition {
    function_name: &'static str,
    intrinsic: Intrinsic,
    return_type: TypeDef,
    param_types: &'static [ParamDef],
    multi_types: &'static [TypeDef],
}

struct ParamDef(pub TypeDef, pub InputModifier);

enum TypeDef {
    Void,
    Numeric(NumericType),
    Object(ObjectType),
    FunctionTemplateArgument,
    ObjectTemplateArgument,
    MultiArgument,
}

const fn numeric_from_str(name: &str) -> TypeDef {
    let ty = match NumericType::from_str(name) {
        Some(ty) => ty,
        None => panic!(),
    };

    TypeDef::Numeric(ty)
}

macro_rules! return_type {
    (T) => {
        TypeDef::FunctionTemplateArgument
    };

    (D) => {
        TypeDef::ObjectTemplateArgument
    };

    (M) => {
        TypeDef::MultiArgument
    };

    (void) => {
        TypeDef::Void
    };

    ($ty:ident) => {
        numeric_from_str(stringify!($ty))
    };
}

macro_rules! param_type {
    (T) => {
        ParamDef(TypeDef::FunctionTemplateArgument, InputModifier::In)
    };

    (D) => {
        ParamDef(TypeDef::ObjectTemplateArgument, InputModifier::In)
    };

    (M) => {
        ParamDef(TypeDef::MultiArgument, InputModifier::In)
    };

    (out M) => {
        ParamDef(TypeDef::MultiArgument, InputModifier::Out)
    };

    (SamplerState) => {
        ParamDef(TypeDef::Object(ObjectType::SamplerState), InputModifier::In)
    };

    (SamplerComparisonState) => {
        ParamDef(
            TypeDef::Object(ObjectType::SamplerComparisonState),
            InputModifier::In,
        )
    };

    (RaytracingAccelerationStructure) => {
        ParamDef(
            TypeDef::Object(ObjectType::RaytracingAccelerationStructure),
            InputModifier::In,
        )
    };

    (RayDesc) => {
        ParamDef(TypeDef::Object(ObjectType::RayDesc), InputModifier::In)
    };

    ($ty:ident) => {
        ParamDef(numeric_from_str(stringify!($ty)), InputModifier::In)
    };

    (out $ty:ident) => {
        ParamDef(numeric_from_str(stringify!($ty)), InputModifier::Out)
    };

    (inout $ty:ident) => {
        ParamDef(numeric_from_str(stringify!($ty)), InputModifier::InOut)
    };

    ($ty:expr) => {
        ParamDef($ty, InputModifier::In)
    };
}

macro_rules! f {
    ($return_type:tt $name:tt ($($($param_type:ident)+),*) => $intrinsic:ident $(| $($($multi_types:ident)+),*)?) => {
        IntrinsicDefinition {
            function_name: stringify!($name),
            intrinsic: Intrinsic::$intrinsic,
            return_type: return_type!($return_type),
            param_types: &[$(param_type!($($param_type)+)),*],
            multi_types: &[$($(numeric_from_str(stringify!($($multi_types)+))),*)?],
        }
    };
}

#[rustfmt::skip]
const INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void AllMemoryBarrier() => AllMemoryBarrier },
    f! { void AllMemoryBarrierWithGroupSync() => AllMemoryBarrierWithGroupSync },
    f! { void DeviceMemoryBarrier() => DeviceMemoryBarrier },
    f! { void DeviceMemoryBarrierWithGroupSync() => DeviceMemoryBarrierWithGroupSync },
    f! { void GroupMemoryBarrier() => GroupMemoryBarrier },
    f! { void GroupMemoryBarrierWithGroupSync() => GroupMemoryBarrierWithGroupSync },

    f! { bool all(M) => All | bool, bool2, bool3, bool4 },
    f! { bool any(M) => Any | bool, bool2, bool3, bool4 },
    f! { M and(M, M) => And | bool, bool2, bool3, bool4 },
    f! { M or(M, M) => Or | bool, bool2, bool3, bool4 },

    // Common select variants
    // We do not currently support any template type deduction so are limited in what types are can support
    f! { M select(bool, M, M) => Select | bool, float, int, uint, double, half },
    f! { M select(bool2, M, M) => Select | bool2, float2, int2, uint2, double2, half2 },
    f! { M select(bool3, M, M) => Select | bool3, float3, int3, uint3, double3, half3 },
    f! { M select(bool4, M, M) => Select | bool4, float4, int4, uint4, double4, half4 },

    f! { M abs(M) => Abs | int, int2, int3, int4, float, float2, float3, float4 },

    f! { int asint(uint) => AsInt },
    f! { int2 asint(uint2) => AsInt },
    f! { int3 asint(uint3) => AsInt },
    f! { int4 asint(uint4) => AsInt },
    f! { int asint(float) => AsInt },
    f! { int2 asint(float2) => AsInt },
    f! { int3 asint(float3) => AsInt },
    f! { int4 asint(float4) => AsInt },

    f! { uint asuint(int) => AsUInt },
    f! { uint2 asuint(int2) => AsUInt },
    f! { uint3 asuint(int3) => AsUInt },
    f! { uint4 asuint(int4) => AsUInt },
    f! { uint asuint(float) => AsUInt },
    f! { uint2 asuint(float2) => AsUInt },
    f! { uint3 asuint(float3) => AsUInt },
    f! { uint4 asuint(float4) => AsUInt },

    f! { float asfloat(uint) => AsFloat },
    f! { float2 asfloat(uint2) => AsFloat },
    f! { float3 asfloat(uint3) => AsFloat },
    f! { float4 asfloat(uint4) => AsFloat },
    f! { float asfloat(float) => AsFloat },
    f! { float2 asfloat(float2) => AsFloat },
    f! { float3 asfloat(float3) => AsFloat },
    f! { float4 asfloat(float4) => AsFloat },

    f! { M acos(M) => Acos | float, float2, float3, float4 },
    f! { M asin(M) => Asin | float, float2, float3, float4 },
    f! { M atan(M) => Atan | float, float2, float3, float4 },
    f! { M atan2(M, M) => Atan2 | float, float2, float3, float4 },
    f! { M cos(M) => Cos | float, float2, float3, float4 },
    f! { M cosh(M) => Cosh | float, float2, float3, float4 },
    f! { M sin(M) => Sin | float, float2, float3, float4 },
    f! { M sinh(M) => Sinh | float, float2, float3, float4 },
    f! { void sincos(M, out M, out M) => Sincos | float, float2, float3, float4 },
    f! { M tan(M) => Tan | float, float2, float3, float4 },
    f! { M tanh(M) => Tanh | float, float2, float3, float4 },
    f! { M sqrt(M) => Sqrt | float, float2, float3, float4 },
    f! { M rsqrt(M) => RcpSqrt | float, float2, float3, float4 },
    f! { M pow(M, M) => Pow | float, float2, float3, float4 },
    f! { M exp(M) => Exp | float, float2, float3, float4 },
    f! { M exp2(M) => Exp2 | float, float2, float3, float4 },
    f! { M log(M) => Log | float, float2, float3, float4 },
    f! { M log2(M) => Log2 | float, float2, float3, float4 },
    f! { M log10(M) => Log10 | float, float2, float3, float4 },

    f! { double asdouble(uint, uint) => AsDouble },

    f! { M clamp(M, M, M) => Clamp | int, int2, int3, int4, float, float2, float3, float4 },

    f! { float3 cross(float3, float3) => Cross },

    f! { float distance(M, M) => Distance | float1, float2, float3, float4 },

    f! { int dot(M, M) => Dot | int1, int2, int3, int4 },
    f! { float dot(M, M) => Dot | float1, float2, float3, float4 },

    f! { float3 mul(float3x3, float3) => Mul },
    f! { float4 mul(float4x4, float4) => Mul },
    f! { float3 mul(float3, float3x3) => Mul },
    f! { float4 mul(float4, float4x4) => Mul },

    f! { float f16tof32(uint) => F16ToF32 },
    f! { float2 f16tof32(uint2) => F16ToF32 },
    f! { float3 f16tof32(uint3) => F16ToF32 },
    f! { float4 f16tof32(uint4) => F16ToF32 },
    f! { uint f32tof16(float) => F32ToF16 },
    f! { uint2 f32tof16(float2) => F32ToF16 },
    f! { uint3 f32tof16(float3) => F32ToF16 },
    f! { uint4 f32tof16(float4) => F32ToF16 },

    f! { M floor(M) => Floor | float, float2, float3, float4 },
    f! { M ceil(M) => Ceil | float, float2, float3, float4 },
    f! { M trunc(M) => Trunc | float, float2, float3, float4 },
    f! { M round(M) => Round | float, float2, float3, float4 },
    f! { M frac(M) => Frac | float, float2, float3, float4 },
    f! { M modf(M, out M) => Modf | float, float2, float3, float4, int, int2, int3, int4 },
    f! { M fmod(M, M) => Fmod | float, float2, float3, float4 },

    f! { M lerp(M, M, M) => Lerp | float, float2, float3, float4 },

    f! { bool isnan(float) => IsNaN },
    f! { bool2 isnan(float2) => IsNaN },
    f! { bool3 isnan(float3) => IsNaN },
    f! { bool4 isnan(float4) => IsNaN },

    f! { bool isinf(float) => IsInfinite },
    f! { bool2 isinf(float2) => IsInfinite },
    f! { bool3 isinf(float3) => IsInfinite },
    f! { bool4 isinf(float4) => IsInfinite },

    f! { bool isfinite(float) => IsFinite },
    f! { bool2 isfinite(float2) => IsFinite },
    f! { bool3 isfinite(float3) => IsFinite },
    f! { bool4 isfinite(float4) => IsFinite },

    f! { float length(M) => Length | float1, float2, float3, float4 },

    f! { M min(M, M) => Min | int, int2, int3, int4, float, float2, float3, float4 },
    f! { float min(int, float) => Min },
    f! { float min(float, int) => Min },
    f! { M max(M, M) => Max | int, int2, int3, int4, float, float2, float3, float4 },
    f! { float max(int, float) => Max },
    f! { float max(float, int) => Max },

    f! { M normalize(M) => Normalize | float1, float2, float3, float4 },
    f! { M rcp(M) => Rcp | float, float2, float3, float4 },

    f! { M reflect(M, M) => Reflect | float, float2, float3, float4 },
    f! { M refract(M, M, float) => Refract | float, float2, float3, float4 },

    f! { M countbits(M) => CountBits | uint, uint2, uint3, uint4 },
    f! { M reversebits(M) => ReverseBits | uint, uint2, uint3, uint4 },
    f! { M firstbithigh(M) => FirstBitHigh | uint, uint2, uint3, uint4, int, int2, int3, int4 },
    f! { M firstbitlow(M) => FirstBitLow | uint, uint2, uint3, uint4, int, int2, int3, int4 },

    f! { M saturate(M) => Saturate | float, float2, float3, float4 },

    f! { int sign(int) => Sign },
    f! { int2 sign(int2) => Sign },
    f! { int3 sign(int3) => Sign },
    f! { int4 sign(int4) => Sign },
    f! { int sign(float) => Sign },
    f! { int2 sign(float2) => Sign },
    f! { int3 sign(float3) => Sign },
    f! { int4 sign(float4) => Sign },

    f! { M smoothstep(M, M, M) => SmoothStep | float, float2, float3, float4 },

    f! { M step(M, M) => Step | float, float2, float3, float4 },

    f! { M transpose(M) => Transpose | float2x2, float3x3, float4x4 },

    f! { M ddx(M) => DDX | float, float2, float3, float4 },
    f! { M ddx_coarse(M) => DDXCoarse | float, float2, float3, float4 },
    f! { M ddx_fine(M) => DDXFine | float, float2, float3, float4 },
    f! { M ddy(M) => DDY | float, float2, float3, float4 },
    f! { M ddy_coarse(M) => DDYCoarse | float, float2, float3, float4 },
    f! { M ddy_fine(M) => DDYFine | float, float2, float3, float4 },

    f! { void InterlockedAdd(M, M, out M) => InterlockedAdd | uint, int },
    f! { void InterlockedAdd(M, M) => InterlockedAdd | uint, int },
    f! { void InterlockedAnd(M, M, out M) => InterlockedAnd | uint, int },
    f! { void InterlockedAnd(M, M) => InterlockedAnd | uint, int },
    f! { void InterlockedCompareExchange(M, M, M, out M) => InterlockedCompareExchange | uint, int },
    f! { void InterlockedCompareStore(M, M, M) => InterlockedCompareStore | uint, int },
    f! { void InterlockedExchange(M, M, out M) => InterlockedExchange | uint, int },
    f! { void InterlockedMax(M, M, out M) => InterlockedMax | uint, int },
    f! { void InterlockedMax(M, M) => InterlockedMax | uint, int },
    f! { void InterlockedMin(M, M, out M) => InterlockedMin | uint, int },
    f! { void InterlockedMin(M, M) => InterlockedMin | uint, int },
    f! { void InterlockedOr(M, M, out M) => InterlockedOr | uint, int },
    f! { void InterlockedOr(M, M) => InterlockedOr | uint, int },
    f! { void InterlockedXor(M, M, out M) => InterlockedXor | uint, int },
    f! { void InterlockedXor(M, M) => InterlockedXor | uint, int },

    f! { uint NonUniformResourceIndex(uint) => NonUniformResourceIndex },

    f! { uint WaveGetLaneCount() => WaveGetLaneCount },
    f! { uint WaveGetLaneIndex() => WaveGetLaneIndex },
    f! { bool WaveIsFirstLane() => WaveIsFirstLane },

    f! { bool WaveActiveAnyTrue(bool) => WaveActiveAnyTrue },
    f! { bool WaveActiveAllTrue(bool) => WaveActiveAllTrue },
    f! { uint4 WaveActiveBallot(bool) => WaveActiveBallot },

    f! { M WaveReadLaneAt(M, uint) => WaveReadLaneAt | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveReadLaneFirst(M) => WaveReadLaneFirst | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { bool WaveActiveAllEqual(M) => WaveActiveAllEqual | uint, int, float },
    f! { bool2 WaveActiveAllEqual(M) => WaveActiveAllEqual | uint2, int2, float2 },
    f! { bool3 WaveActiveAllEqual(M) => WaveActiveAllEqual | uint3, int3, float3 },
    f! { bool4 WaveActiveAllEqual(M) => WaveActiveAllEqual | uint4, int4, float4 },
    f! { uint WaveActiveCountBits(bool) => WaveActiveCountBits },
    f! { M WaveActiveSum(M) => WaveActiveSum | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveActiveProduct(M) => WaveActiveProduct | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveActiveBitAnd(M) => WaveActiveBitAnd | uint, uint2, uint3, uint4 },
    f! { M WaveActiveBitOr(M) => WaveActiveBitOr | uint, uint2, uint3, uint4 },
    f! { M WaveActiveBitXor(M) => WaveActiveBitXor | uint, uint2, uint3, uint4 },
    f! { M WaveActiveMin(M) => WaveActiveMin | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveActiveMax(M) => WaveActiveMax | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { uint WavePrefixCountBits(bool) => WavePrefixCountBits },
    f! { M WavePrefixProduct(M) => WavePrefixProduct | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WavePrefixSum(M) => WavePrefixSum | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { M QuadReadAcrossX(M) => QuadReadAcrossX | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M QuadReadAcrossY(M) => QuadReadAcrossY | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M QuadReadAcrossDiagonal(M) => QuadReadAcrossDiagonal | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M QuadReadLaneAt(M, uint) => QuadReadLaneAt | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { void SetMeshOutputCounts(uint, uint) => SetMeshOutputCounts },
    f! { void DispatchMesh(uint, uint, uint, T) => DispatchMesh },
];

/// Create a collection of all the intrinsic functions
pub fn add_intrinsics(module: &mut Module) {
    // Add builtin enum variants
    {
        fn add_value(module: &mut Module, name: &str, type_id: TypeId, value: Option<Constant>) {
            let id = GlobalId(module.global_registry.len() as u32);
            module.global_registry.push(GlobalVariable {
                id,
                name: Located::none(name.to_string()),
                full_name: ScopedName::unscoped(name.to_string()),
                type_id,
                storage_class: GlobalStorage::Static,
                lang_slot: LanguageBinding::default(),
                api_slot: None,
                init: None,
                constexpr_value: value,
            });
        }

        const RAY_FLAGS_VARIANTS: &[(&str, u32)] = &[
            ("RAY_FLAG_NONE", 0x00),
            ("RAY_FLAG_FORCE_OPAQUE", 0x01),
            ("RAY_FLAG_FORCE_NON_OPAQUE", 0x02),
            ("RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH", 0x04),
            ("RAY_FLAG_SKIP_CLOSEST_HIT_SHADER", 0x08),
            ("RAY_FLAG_CULL_BACK_FACING_TRIANGLES", 0x10),
            ("RAY_FLAG_CULL_FRONT_FACING_TRIANGLES", 0x20),
            ("RAY_FLAG_CULL_OPAQUE", 0x40),
            ("RAY_FLAG_CULL_NON_OPAQUE", 0x80),
            ("RAY_FLAG_SKIP_TRIANGLES", 0x100),
            ("RAY_FLAG_SKIP_PROCEDURAL_PRIMITIVES", 0x200),
        ];

        const COMMITTED_STATUS_VARIANTS: &[&str] = &[
            "COMMITTED_NOTHING",
            "COMMITTED_TRIANGLE_HIT",
            "COMMITTED_PROCEDURAL_PRIMITIVE_HIT",
        ];

        const CANDIDATE_TYPE_VARIANTS: &[&str] = &[
            "CANDIDATE_NON_OPAQUE_TRIANGLE",
            "CANDIDATE_PROCEDURAL_PRIMITIVE",
        ];

        // We copy HLSL behaviour where these have uint type instead of an enum type like the documentation presents them
        let type_id = module
            .type_registry
            .register_type(TypeLayer::Scalar(ScalarType::Int32));

        for (flag, value) in RAY_FLAGS_VARIANTS {
            add_value(module, flag, type_id, Some(Constant::UInt32(*value)));
        }

        for flag in COMMITTED_STATUS_VARIANTS {
            add_value(module, flag, type_id, None);
        }

        for flag in CANDIDATE_TYPE_VARIANTS {
            add_value(module, flag, type_id, None);
        }
    }

    for def in INTRINSICS {
        let multi_types = if def.multi_types.is_empty() {
            &[TypeDef::Void]
        } else {
            def.multi_types
        };
        for multi_type in multi_types {
            let multi_type_id = if def.multi_types.is_empty() {
                None
            } else {
                Some(get_type_id(multi_type, module, None, None))
            };

            // Replace the multi types
            let mut remap_inner = |ty| get_type_id(ty, module, None, multi_type_id);

            // Fetch and remap the param types
            let mut param_types = Vec::with_capacity(def.param_types.len());
            for param_type_def in def.param_types {
                param_types.push(ParamType {
                    type_id: remap_inner(&param_type_def.0),
                    input_modifier: param_type_def.1,
                });
            }

            // Register the return type
            let return_type = FunctionReturn {
                return_type: remap_inner(&def.return_type),
                semantic: None,
            };

            // Generate the template parameterse from the main parameters
            let template_params =
                get_template_params(module, return_type.return_type, &param_types);

            // Make the signature
            let signature = FunctionSignature {
                return_type,
                template_params,
                param_types,
            };

            // All intrinsic functions are in root namespace
            let full_name = ScopedName::unscoped(def.function_name.to_string());

            // Register the intrinsic as a function
            let id = module.function_registry.register_function(
                FunctionNameDefinition {
                    name: Located::none(def.function_name.to_string()),
                    full_name,
                },
                signature,
            );

            module
                .function_registry
                .set_intrinsic_data(id, def.intrinsic.clone());
        }
    }
}

#[test]
fn test_intrinsic_duplicates() {
    let module = Module::create();
    let intrinsic_count = module.function_registry.get_function_count();
    for index_first in 1..intrinsic_count {
        let id_first = FunctionId(index_first);
        let signature_first = module.function_registry.get_function_signature(id_first);
        let name_first = module.function_registry.get_function_name(id_first);
        for index_second in 0..index_first {
            let id_second = FunctionId(index_second);
            let signature_second = module.function_registry.get_function_signature(id_second);
            let name_second = module.function_registry.get_function_name(id_second);
            if name_first == name_second
                && signature_first.param_types == signature_second.param_types
            {
                panic!("Duplicate intrinsic function signature: {name_first}");
            }
        }
    }
}

const BUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => BufferGetDimensions },
    f! { D Load(int) => BufferLoad },
    f! { D Load(int, out uint) => BufferLoad },
];

const RWBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => RWBufferGetDimensions },
    f! { D Load(int) => RWBufferLoad },
    f! { D Load(int, out uint) => RWBufferLoad },
];

const STRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint) => StructuredBufferGetDimensions },
    f! { D Load(int) => StructuredBufferLoad },
    f! { D Load(int, out uint) => StructuredBufferLoad },
];

const RWSTRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint) => RWStructuredBufferGetDimensions },
    f! { D Load(int) => RWStructuredBufferLoad },
    f! { D Load(int, out uint) => RWStructuredBufferLoad },
];

const TEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(uint, out uint, out uint, out uint) => Texture2DGetDimensions },
    f! { void GetDimensions(out uint, out uint) => Texture2DGetDimensions },
    f! { void GetDimensions(uint, out float, out float, out float) => Texture2DGetDimensions },
    f! { void GetDimensions(out float, out float) => Texture2DGetDimensions },
    f! { D Load(int3) => Texture2DLoad },
    f! { D Load(int3, int2) => Texture2DLoad },
    f! { D Load(int3, int2, out uint) => Texture2DLoad },
    f! { D Sample(SamplerState, float2) => Texture2DSample },
    f! { D Sample(SamplerState, float2, int2) => Texture2DSample },
    f! { D Sample(SamplerState, float2, int2, float) => Texture2DSample },
    f! { D Sample(SamplerState, float2, int2, float, out uint) => Texture2DSample },
    f! { D SampleBias(SamplerState, float2, float) => Texture2DSampleBias },
    f! { D SampleBias(SamplerState, float2, float, int2) => Texture2DSampleBias },
    f! { D SampleBias(SamplerState, float2, float, int2, float) => Texture2DSampleBias },
    f! { D SampleBias(SamplerState, float2, float, int2, float, out uint) => Texture2DSampleBias },
    f! { D SampleCmp(SamplerComparisonState, float2, float, int2) => Texture2DSampleCmp },
    f! { D SampleCmp(SamplerComparisonState, float2, float, int2, float) => Texture2DSampleCmp },
    f! { D SampleCmp(SamplerComparisonState, float2, float, int2, float, out uint) => Texture2DSampleCmp },
    f! { D SampleCmpLevelZero(SamplerComparisonState, float2, float, int2) => Texture2DSampleCmpLevelZero },
    f! { D SampleCmpLevelZero(SamplerComparisonState, float2, float, int2, out uint) => Texture2DSampleCmpLevelZero },
    f! { D SampleGrad(SamplerState, float2, float, float) => Texture2DSampleGrad },
    f! { D SampleGrad(SamplerState, float2, float, float, int2) => Texture2DSampleGrad },
    f! { D SampleGrad(SamplerState, float2, float, float, int2, float) => Texture2DSampleGrad },
    f! { D SampleGrad(SamplerState, float2, float, float, int2, float, out uint) => Texture2DSampleGrad },
    f! { D SampleLevel(SamplerState, float2, float) => Texture2DSampleLevel },
    f! { D SampleLevel(SamplerState, float2, float, int2) => Texture2DSampleLevel },
    f! { D SampleLevel(SamplerState, float2, float, int2, out uint) => Texture2DSampleLevel },
    f! { D Gather(SamplerState, float2, int2) => Texture2DGatherRed },
    f! { D Gather(SamplerState, float2, int2, out uint) => Texture2DGatherRed },
    f! { D GatherRed(SamplerState, float2, int2) => Texture2DGatherRed },
    f! { D GatherRed(SamplerState, float2, int2, out uint) => Texture2DGatherRed },
    f! { D GatherRed(SamplerState, float2, int2, int2, int2, int2) => Texture2DGatherRed },
    f! { D GatherRed(SamplerState, float2, int2, int2, int2, int2, out uint) => Texture2DGatherRed },
    f! { D GatherGreen(SamplerState, float2, int2) => Texture2DGatherGreen },
    f! { D GatherGreen(SamplerState, float2, int2, out uint) => Texture2DGatherGreen },
    f! { D GatherGreen(SamplerState, float2, int2, int2, int2, int2) => Texture2DGatherGreen },
    f! { D GatherGreen(SamplerState, float2, int2, int2, int2, int2, out uint) => Texture2DGatherGreen },
    f! { D GatherBlue(SamplerState, float2, int2) => Texture2DGatherBlue },
    f! { D GatherBlue(SamplerState, float2, int2, out uint) => Texture2DGatherBlue },
    f! { D GatherBlue(SamplerState, float2, int2, int2, int2, int2) => Texture2DGatherBlue },
    f! { D GatherBlue(SamplerState, float2, int2, int2, int2, int2, out uint) => Texture2DGatherBlue },
    f! { D GatherAlpha(SamplerState, float2, int2) => Texture2DGatherAlpha },
    f! { D GatherAlpha(SamplerState, float2, int2, out uint) => Texture2DGatherAlpha },
    f! { D GatherAlpha(SamplerState, float2, int2, int2, int2, int2) => Texture2DGatherAlpha },
    f! { D GatherAlpha(SamplerState, float2, int2, int2, int2, int2, out uint) => Texture2DGatherAlpha },
    f! { D GatherCmp(SamplerComparisonState, float2, float, int2) => Texture2DGatherCmpRed },
    f! { D GatherCmp(SamplerComparisonState, float2, float, int2, out uint) => Texture2DGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float2, float, int2) => Texture2DGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float2, float, int2, out uint) => Texture2DGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float2, float, int2, int2, int2, int2) => Texture2DGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float2, float, int2, int2, int2, int2, out uint) => Texture2DGatherCmpRed },
    f! { D GatherCmpGreen(SamplerComparisonState, float2, float, int2) => Texture2DGatherCmpGreen },
    f! { D GatherCmpGreen(SamplerComparisonState, float2, float, int2, out uint) => Texture2DGatherCmpGreen },
    f! { D GatherCmpGreen(SamplerComparisonState, float2, float, int2, int2, int2, int2) => Texture2DGatherCmpGreen },
    f! { D GatherCmpGreen(SamplerComparisonState, float2, float, int2, int2, int2, int2, out uint) => Texture2DGatherCmpGreen },
    f! { D GatherCmpBlue(SamplerComparisonState, float2, float, int2) => Texture2DGatherCmpBlue },
    f! { D GatherCmpBlue(SamplerComparisonState, float2, float, int2, out uint) => Texture2DGatherCmpBlue },
    f! { D GatherCmpBlue(SamplerComparisonState, float2, float, int2, int2, int2, int2) => Texture2DGatherCmpBlue },
    f! { D GatherCmpBlue(SamplerComparisonState, float2, float, int2, int2, int2, int2, out uint) => Texture2DGatherCmpBlue },
    f! { D GatherCmpAlpha(SamplerComparisonState, float2, float, int2) => Texture2DGatherCmpAlpha },
    f! { D GatherCmpAlpha(SamplerComparisonState, float2, float, int2, out uint) => Texture2DGatherCmpAlpha },
    f! { D GatherCmpAlpha(SamplerComparisonState, float2, float, int2, int2, int2, int2) => Texture2DGatherCmpAlpha },
    f! { D GatherCmpAlpha(SamplerComparisonState, float2, float, int2, int2, int2, int2, out uint) => Texture2DGatherCmpAlpha },
];

const TEXTURE2DARRAY_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(uint, out uint, out uint, out uint, out uint) => Texture2DArrayGetDimensions },
    f! { void GetDimensions(out uint, out uint, out uint) => Texture2DArrayGetDimensions },
    f! { void GetDimensions(uint, out float, out float, out float, out float) => Texture2DArrayGetDimensions },
    f! { void GetDimensions(out float, out float, out float) => Texture2DArrayGetDimensions },
    f! { D Load(int4) => Texture2DArrayLoad },
    f! { D Load(int4, int2) => Texture2DArrayLoad },
    f! { D Load(int4, int2, out uint) => Texture2DArrayLoad },
    f! { D Sample(SamplerState, float3) => Texture2DArraySample },
    f! { D Sample(SamplerState, float3, int2) => Texture2DArraySample },
    f! { D Sample(SamplerState, float3, int2, float) => Texture2DArraySample },
    f! { D Sample(SamplerState, float3, int2, float, out uint) => Texture2DArraySample },
    f! { D SampleBias(SamplerState, float3, float) => Texture2DArraySampleBias },
    f! { D SampleBias(SamplerState, float3, float, int2) => Texture2DArraySampleBias },
    f! { D SampleBias(SamplerState, float3, float, int2, float) => Texture2DArraySampleBias },
    f! { D SampleBias(SamplerState, float3, float, int2, float, out uint) => Texture2DArraySampleBias },
    f! { D SampleCmp(SamplerComparisonState, float3, float, int2) => Texture2DArraySampleCmp },
    f! { D SampleCmp(SamplerComparisonState, float3, float, int2, float) => Texture2DArraySampleCmp },
    f! { D SampleCmp(SamplerComparisonState, float3, float, int2, float, out uint) => Texture2DArraySampleCmp },
    f! { D SampleCmpLevelZero(SamplerComparisonState, float3, float, int2) => Texture2DArraySampleCmpLevelZero },
    f! { D SampleCmpLevelZero(SamplerComparisonState, float3, float, int2, out uint) => Texture2DArraySampleCmpLevelZero },
    f! { D SampleGrad(SamplerState, float3, float, float) => Texture2DArraySampleGrad },
    f! { D SampleGrad(SamplerState, float3, float, float, int2) => Texture2DArraySampleGrad },
    f! { D SampleGrad(SamplerState, float3, float, float, int2, float) => Texture2DArraySampleGrad },
    f! { D SampleGrad(SamplerState, float3, float, float, int2, float, out uint) => Texture2DArraySampleGrad },
    f! { D SampleLevel(SamplerState, float3, float) => Texture2DArraySampleLevel },
    f! { D SampleLevel(SamplerState, float3, float, int2) => Texture2DArraySampleLevel },
    f! { D SampleLevel(SamplerState, float3, float, int2, out uint) => Texture2DArraySampleLevel },
    f! { D Gather(SamplerState, float3, int2) => Texture2DArrayGatherRed },
    f! { D Gather(SamplerState, float3, int2, out uint) => Texture2DArrayGatherRed },
    f! { D GatherRed(SamplerState, float3, int2) => Texture2DArrayGatherRed },
    f! { D GatherRed(SamplerState, float3, int2, out uint) => Texture2DArrayGatherRed },
    f! { D GatherRed(SamplerState, float3, int2, int2, int2, int2) => Texture2DArrayGatherRed },
    f! { D GatherRed(SamplerState, float3, int2, int2, int2, int2, out uint) => Texture2DArrayGatherRed },
    f! { D GatherGreen(SamplerState, float3, int2) => Texture2DArrayGatherGreen },
    f! { D GatherGreen(SamplerState, float3, int2, out uint) => Texture2DArrayGatherGreen },
    f! { D GatherGreen(SamplerState, float3, int2, int2, int2, int2) => Texture2DArrayGatherGreen },
    f! { D GatherGreen(SamplerState, float3, int2, int2, int2, int2, out uint) => Texture2DArrayGatherGreen },
    f! { D GatherBlue(SamplerState, float3, int2) => Texture2DArrayGatherBlue },
    f! { D GatherBlue(SamplerState, float3, int2, out uint) => Texture2DArrayGatherBlue },
    f! { D GatherBlue(SamplerState, float3, int2, int2, int2, int2) => Texture2DArrayGatherBlue },
    f! { D GatherBlue(SamplerState, float3, int2, int2, int2, int2, out uint) => Texture2DArrayGatherBlue },
    f! { D GatherAlpha(SamplerState, float3, int2) => Texture2DArrayGatherAlpha },
    f! { D GatherAlpha(SamplerState, float3, int2, out uint) => Texture2DArrayGatherAlpha },
    f! { D GatherAlpha(SamplerState, float3, int2, int2, int2, int2) => Texture2DArrayGatherAlpha },
    f! { D GatherAlpha(SamplerState, float3, int2, int2, int2, int2, out uint) => Texture2DArrayGatherAlpha },
    f! { D GatherCmp(SamplerComparisonState, float3, float, int2) => Texture2DArrayGatherCmpRed },
    f! { D GatherCmp(SamplerComparisonState, float3, float, int2, out uint) => Texture2DArrayGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float3, float, int2) => Texture2DArrayGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float3, float, int2, out uint) => Texture2DArrayGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float3, float, int2, int2, int2, int2) => Texture2DArrayGatherCmpRed },
    f! { D GatherCmpRed(SamplerComparisonState, float3, float, int2, int2, int2, int2, out uint) => Texture2DArrayGatherCmpRed },
    f! { D GatherCmpGreen(SamplerComparisonState, float3, float, int2) => Texture2DArrayGatherCmpGreen },
    f! { D GatherCmpGreen(SamplerComparisonState, float3, float, int2, out uint) => Texture2DArrayGatherCmpGreen },
    f! { D GatherCmpGreen(SamplerComparisonState, float3, float, int2, int2, int2, int2) => Texture2DArrayGatherCmpGreen },
    f! { D GatherCmpGreen(SamplerComparisonState, float3, float, int2, int2, int2, int2, out uint) => Texture2DArrayGatherCmpGreen },
    f! { D GatherCmpBlue(SamplerComparisonState, float3, float, int2) => Texture2DArrayGatherCmpBlue },
    f! { D GatherCmpBlue(SamplerComparisonState, float3, float, int2, out uint) => Texture2DArrayGatherCmpBlue },
    f! { D GatherCmpBlue(SamplerComparisonState, float3, float, int2, int2, int2, int2) => Texture2DArrayGatherCmpBlue },
    f! { D GatherCmpBlue(SamplerComparisonState, float3, float, int2, int2, int2, int2, out uint) => Texture2DArrayGatherCmpBlue },
    f! { D GatherCmpAlpha(SamplerComparisonState, float3, float, int2) => Texture2DArrayGatherCmpAlpha },
    f! { D GatherCmpAlpha(SamplerComparisonState, float3, float, int2, out uint) => Texture2DArrayGatherCmpAlpha },
    f! { D GatherCmpAlpha(SamplerComparisonState, float3, float, int2, int2, int2, int2) => Texture2DArrayGatherCmpAlpha },
    f! { D GatherCmpAlpha(SamplerComparisonState, float3, float, int2, int2, int2, int2, out uint) => Texture2DArrayGatherCmpAlpha },
];

const RWTEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint) => RWTexture2DGetDimensions },
    f! { void GetDimensions(out float, out float) => RWTexture2DGetDimensions },
    f! { D Load(int2) => RWTexture2DLoad },
    f! { D Load(int2, out uint) => RWTexture2DLoad },
];

const RWTEXTURE2DARRAY_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint, out uint) => RWTexture2DArrayGetDimensions },
    f! { void GetDimensions(out float, out float, out float) => RWTexture2DArrayGetDimensions },
    f! { D Load(int3) => RWTexture2DArrayLoad },
    f! { D Load(int3, out uint) => RWTexture2DArrayLoad },
];

const TEXTURECUBE_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { D Sample(SamplerState, float3) => TextureCubeSample },
    f! { D Sample(SamplerState, float3, float) => TextureCubeSample },
    f! { D Sample(SamplerState, float3, float, out uint) => TextureCubeSample },
    f! { D SampleLevel(SamplerState, float3, float) => TextureCubeSampleLevel },
    f! { D SampleLevel(SamplerState, float3, float, out uint) => TextureCubeSampleLevel },
];

const TEXTURECUBEARRAY_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { D Sample(SamplerState, float4) => TextureCubeArraySample },
    f! { D Sample(SamplerState, float4, float) => TextureCubeArraySample },
    f! { D Sample(SamplerState, float4, float, out uint) => TextureCubeArraySample },
    f! { D SampleLevel(SamplerState, float4, float) => TextureCubeArraySampleLevel },
    f! { D SampleLevel(SamplerState, float4, float, out uint) => TextureCubeArraySampleLevel },
];

const TEXTURE3D_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(uint, out uint, out uint, out uint, out uint) => Texture3DGetDimensions },
    f! { void GetDimensions(out uint, out uint, out uint) => Texture3DGetDimensions },
    f! { void GetDimensions(uint, out float, out float, out float, out float) => Texture3DGetDimensions },
    f! { void GetDimensions(out float, out float, out float) => Texture3DGetDimensions },
    f! { D Load(int4) => Texture3DLoad },
    f! { D Load(int4, int3) => Texture3DLoad },
    f! { D Load(int4, int3, out uint) => Texture3DLoad },
    f! { D Sample(SamplerState, float3) => Texture3DSample },
    f! { D Sample(SamplerState, float3, int3) => Texture3DSample },
    f! { D Sample(SamplerState, float3, int3, float) => Texture3DSample },
    f! { D Sample(SamplerState, float3, int3, float, out uint) => Texture3DSample },
    f! { D SampleBias(SamplerState, float3, float) => Texture3DSampleBias },
    f! { D SampleBias(SamplerState, float3, float, int3) => Texture3DSampleBias },
    f! { D SampleBias(SamplerState, float3, float, int3, float) => Texture3DSampleBias },
    f! { D SampleBias(SamplerState, float3, float, int3, float, out uint) => Texture3DSampleBias },
    f! { D SampleGrad(SamplerState, float3, float, float) => Texture3DSampleGrad },
    f! { D SampleGrad(SamplerState, float3, float, float, int3) => Texture3DSampleGrad },
    f! { D SampleGrad(SamplerState, float3, float, float, int3, float) => Texture3DSampleGrad },
    f! { D SampleGrad(SamplerState, float3, float, float, int3, float, out uint) => Texture3DSampleGrad },
    f! { D SampleLevel(SamplerState, float3, float) => Texture3DSampleLevel },
    f! { D SampleLevel(SamplerState, float3, float, int3) => Texture3DSampleLevel },
    f! { D SampleLevel(SamplerState, float3, float, int3, out uint) => Texture3DSampleLevel },
];

const RWTEXTURE3D_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint, out uint) => RWTexture3DGetDimensions },
    f! { void GetDimensions(out float, out float, out float) => RWTexture3DGetDimensions },
    f! { D Load(int3) => RWTexture3DLoad },
    f! { D Load(int3, out uint) => RWTexture3DLoad },
];

const BYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => ByteAddressBufferGetDimensions },
    f! { uint Load(uint) => ByteAddressBufferLoad },
    f! { uint Load(uint, out uint) => ByteAddressBufferLoad },
    f! { uint2 Load2(uint) => ByteAddressBufferLoad2 },
    f! { uint2 Load2(uint, out uint) => ByteAddressBufferLoad2 },
    f! { uint3 Load3(uint) => ByteAddressBufferLoad3 },
    f! { uint3 Load3(uint, out uint) => ByteAddressBufferLoad3 },
    f! { uint4 Load4(uint) => ByteAddressBufferLoad4 },
    f! { uint4 Load4(uint, out uint) => ByteAddressBufferLoad4 },
    f! { T Load(uint) => ByteAddressBufferLoadT },
    f! { T Load(uint, out uint) => ByteAddressBufferLoadT },
];

const RWBYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => RWByteAddressBufferGetDimensions },
    f! { uint Load(uint) => RWByteAddressBufferLoad },
    f! { uint Load(uint, out uint) => RWByteAddressBufferLoad },
    f! { uint2 Load2(uint) => RWByteAddressBufferLoad2 },
    f! { uint2 Load2(uint, out uint) => RWByteAddressBufferLoad2 },
    f! { uint3 Load3(uint) => RWByteAddressBufferLoad3 },
    f! { uint3 Load3(uint, out uint) => RWByteAddressBufferLoad3 },
    f! { uint4 Load4(uint) => RWByteAddressBufferLoad4 },
    f! { uint4 Load4(uint, out uint) => RWByteAddressBufferLoad4 },
    f! { T Load(uint) => RWByteAddressBufferLoadT },
    f! { T Load(uint, out uint) => RWByteAddressBufferLoadT },
    f! { void Store(uint, uint) => RWByteAddressBufferStore },
    f! { void Store2(uint, uint2) => RWByteAddressBufferStore2 },
    f! { void Store3(uint, uint3) => RWByteAddressBufferStore3 },
    f! { void Store4(uint, uint4) => RWByteAddressBufferStore4 },
    f! { void InterlockedAdd(uint, uint, out uint) => RWByteAddressBufferInterlockedAdd },
    f! { void InterlockedAnd(uint, uint, out uint) => RWByteAddressBufferInterlockedAnd },
    f! { void InterlockedCompareExchange(uint, uint, uint, out uint) => RWByteAddressBufferInterlockedCompareExchange },
    f! { void InterlockedCompareStore(uint, uint, uint) => RWByteAddressBufferInterlockedCompareStore },
    f! { void InterlockedExchange(uint, uint, out uint) => RWByteAddressBufferInterlockedExchange },
    f! { void InterlockedMax(uint, uint, out uint) => RWByteAddressBufferInterlockedMax },
    f! { void InterlockedMin(uint, uint, out uint) => RWByteAddressBufferInterlockedMin },
    f! { void InterlockedOr(uint, uint, out uint) => RWByteAddressBufferInterlockedOr },
    f! { void InterlockedXor(uint, uint, out uint) => RWByteAddressBufferInterlockedXor },
];
const BUFFERADDRESS_INTRINSICS: &[IntrinsicDefinition] =
    &[f! { T Load(uint) => BufferAddressLoad }];

const RWBUFFERADDRESS_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { T Load(uint) => RWBufferAddressLoad },
    f! { void Store(uint, T) => RWBufferAddressStore },
];

const TRIANGLESTREAM_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void Append(D) => TriangleStreamAppend },
    f! { void RestartStrip() => TriangleStreamRestartStrip },
];

const RAYQUERY_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void TraceRayInline(RaytracingAccelerationStructure, uint, uint, RayDesc) => RayQueryTraceRayInline },
    f! { bool Proceed() => RayQueryProceed },
    f! { void Abort() => RayQueryAbort },
    f! { uint CommittedStatus() => RayQueryCommittedStatus },
    f! { uint CandidateType() => RayQueryCandidateType },
    f! { bool CandidateProceduralPrimitiveNonOpaque() => RayQueryCandidateProceduralPrimitiveNonOpaque },
    f! { void CommitNonOpaqueTriangleHit() => RayQueryCommitNonOpaqueTriangleHit },
    f! { void CommitProceduralPrimitiveHit(float) => RayQueryCommitProceduralPrimitiveHit },
    f! { uint RayFlags() => RayQueryRayFlags },
    f! { float3 WorldRayOrigin() => RayQueryWorldRayOrigin },
    f! { float3 WorldRayDirection() => RayQueryWorldRayDirection },
    f! { float RayTMin() => RayQueryRayTMin },
    f! { float CandidateTriangleRayT() => RayQueryCandidateTriangleRayT },
    f! { float CommittedRayT() => RayQueryCommittedRayT },
    f! { uint CandidateInstanceIndex() => RayQueryCandidateInstanceIndex },
    f! { uint CandidateInstanceID() => RayQueryCandidateInstanceID },
    f! { uint CandidateInstanceContributionToHitGroupIndex() => RayQueryCandidateInstanceContributionToHitGroupIndex },
    f! { uint CandidateGeometryIndex() => RayQueryCandidateGeometryIndex },
    f! { uint CandidatePrimitiveIndex() => RayQueryCandidatePrimitiveIndex },
    f! { float3 CandidateObjectRayOrigin() => RayQueryCandidateObjectRayOrigin },
    f! { float3 CandidateObjectRayDirection() => RayQueryCandidateObjectRayDirection },
    f! { float3x4 CandidateObjectToWorld3x4() => RayQueryCandidateObjectToWorld3x4 },
    f! { float4x3 CandidateObjectToWorld4x3() => RayQueryCandidateObjectToWorld4x3 },
    f! { float3x4 CandidateWorldToObject3x4() => RayQueryCandidateWorldToObject3x4 },
    f! { float4x3 CandidateWorldToObject4x3() => RayQueryCandidateWorldToObject4x3 },
    f! { uint CommittedInstanceIndex() => RayQueryCommittedInstanceIndex },
    f! { uint CommittedInstanceID() => RayQueryCommittedInstanceID },
    f! { uint CommittedInstanceContributionToHitGroupIndex() => RayQueryCommittedInstanceContributionToHitGroupIndex },
    f! { uint CommittedGeometryIndex() => RayQueryCommittedGeometryIndex },
    f! { uint CommittedPrimitiveIndex() => RayQueryCommittedPrimitiveIndex },
    f! { float3 CommittedObjectRayOrigin() => RayQueryCommittedObjectRayOrigin },
    f! { float3 CommittedObjectRayDirection() => RayQueryCommittedObjectRayDirection },
    f! { float3x4 CommittedObjectToWorld3x4() => RayQueryCommittedObjectToWorld3x4 },
    f! { float4x3 CommittedObjectToWorld4x3() => RayQueryCommittedObjectToWorld4x3 },
    f! { float3x4 CommittedWorldToObject3x4() => RayQueryCommittedWorldToObject3x4 },
    f! { float4x3 CommittedWorldToObject4x3() => RayQueryCommittedWorldToObject4x3 },
    f! { float2 CandidateTriangleBarycentrics() => RayQueryCandidateTriangleBarycentrics },
    f! { bool CandidateTriangleFrontFace() => RayQueryCandidateTriangleFrontFace },
    f! { float2 CommittedTriangleBarycentrics() => RayQueryCommittedTriangleBarycentrics },
    f! { bool CommittedTriangleFrontFace() => RayQueryCommittedTriangleFrontFace },
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
        ObjectType::Texture2DArray(_) => TEXTURE2DARRAY_INTRINSICS,
        ObjectType::RWTexture2D(_) => RWTEXTURE2D_INTRINSICS,
        ObjectType::RWTexture2DArray(_) => RWTEXTURE2DARRAY_INTRINSICS,
        ObjectType::TextureCube(_) => TEXTURECUBE_INTRINSICS,
        ObjectType::TextureCubeArray(_) => TEXTURECUBEARRAY_INTRINSICS,
        ObjectType::Texture3D(_) => TEXTURE3D_INTRINSICS,
        ObjectType::RWTexture3D(_) => RWTEXTURE3D_INTRINSICS,
        ObjectType::ByteAddressBuffer => BYTEADDRESSBUFFER_INTRINSICS,
        ObjectType::RWByteAddressBuffer => RWBYTEADDRESSBUFFER_INTRINSICS,
        ObjectType::BufferAddress => BUFFERADDRESS_INTRINSICS,
        ObjectType::RWBufferAddress => RWBUFFERADDRESS_INTRINSICS,
        ObjectType::TriangleStream(_) => TRIANGLESTREAM_INTRINSICS,
        ObjectType::RayQuery(_) => RAYQUERY_INTRINSICS,
        _ => return Vec::new(),
    };

    // Get the object template type
    let inner_type = match object {
        ObjectType::Buffer(ty)
        | ObjectType::RWBuffer(ty)
        | ObjectType::Texture2D(ty)
        | ObjectType::Texture2DArray(ty)
        | ObjectType::RWTexture2D(ty)
        | ObjectType::RWTexture2DArray(ty)
        | ObjectType::TextureCube(ty)
        | ObjectType::TextureCubeArray(ty)
        | ObjectType::Texture3D(ty)
        | ObjectType::RWTexture3D(ty)
        | ObjectType::StructuredBuffer(ty)
        | ObjectType::RWStructuredBuffer(ty)
        | ObjectType::TriangleStream(ty) => Some(ty),
        _ => None,
    };

    let mut methods = Vec::new();
    for def in method_defs {
        assert!(def.multi_types.is_empty());

        // Replace the object template type with the type arg
        let mut remap_inner = |ty| get_type_id(ty, module, inner_type, None);

        // Fetch and remap the param types
        let mut param_types = Vec::with_capacity(def.param_types.len());
        for param_type_def in def.param_types {
            param_types.push(ParamType {
                type_id: remap_inner(&param_type_def.0),
                input_modifier: param_type_def.1,
            });
        }

        // Register the return type
        let return_type = FunctionReturn {
            return_type: remap_inner(&def.return_type),
            semantic: None,
        };

        // Generate the template parameterse from the main parameters
        let template_params = get_template_params(module, return_type.return_type, &param_types);

        methods.push(MethodDefinition {
            name: def.function_name.to_string(),
            intrinsic: def.intrinsic.clone(),
            signature: FunctionSignature {
                return_type,
                template_params,
                param_types,
            },
        });
    }

    methods
}

/// Get the type id from a type definition
fn get_type_id(
    ty: &TypeDef,
    module: &mut Module,
    object_template_type: Option<TypeId>,
    multi_type: Option<TypeId>,
) -> TypeId {
    match *ty {
        TypeDef::Void => module.type_registry.register_type(TypeLayer::Void),
        TypeDef::Numeric(ref num) => module.type_registry.register_numeric_type(*num),
        TypeDef::Object(ref obj) => module.type_registry.register_type(TypeLayer::Object(*obj)),
        TypeDef::FunctionTemplateArgument => {
            let id = module
                .type_registry
                .register_template_type(Located::none("T".to_string()), 0);
            module
                .type_registry
                .register_type(TypeLayer::TemplateParam(id))
        }
        TypeDef::ObjectTemplateArgument => object_template_type.unwrap(),
        TypeDef::MultiArgument => multi_type.unwrap(),
    }
}

/// Get the maximum template argument count within the function
fn get_template_params(
    module: &Module,
    return_type: TypeId,
    param_types: &[ParamType],
) -> Vec<TemplateParam> {
    let mut arg_collection = Vec::new();
    gather_template_args(module, return_type, &mut arg_collection);
    for param_type in param_types {
        gather_template_args(module, param_type.type_id, &mut arg_collection);
    }

    arg_collection.dedup();

    let mut args = Vec::with_capacity(arg_collection.len());
    for i in 0..arg_collection.len() as u32 {
        for arg in &arg_collection {
            let index = module
                .type_registry
                .get_template_type(*arg)
                .positional_index;
            if index == i {
                args.push(TemplateParam::Type(*arg));
                break;
            }
        }
    }

    args
}

/// Get the template arguments referenced in the normal arguments
fn gather_template_args(module: &Module, id: TypeId, template_params: &mut Vec<TemplateTypeId>) {
    let layer = module.type_registry.get_type_layer(id);
    match layer {
        TypeLayer::Void
        | TypeLayer::Scalar(_)
        | TypeLayer::Vector(_, _)
        | TypeLayer::Matrix(_, _, _)
        | TypeLayer::Struct(_)
        | TypeLayer::StructTemplate(_)
        | TypeLayer::Enum(_)
        | TypeLayer::Object(_) => {}
        TypeLayer::Array(inner, _) | TypeLayer::Modifier(_, inner) => {
            gather_template_args(module, inner, template_params);
        }
        TypeLayer::TemplateParam(template_arg) => template_params.push(template_arg),
    }
}
