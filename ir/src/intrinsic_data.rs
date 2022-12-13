use crate::*;
use rssl_text::Located;

struct IntrinsicDefinition {
    function_name: &'static str,
    intrinsic: Intrinsic,
    return_type: TypeLayout,
    param_types: &'static [ParamDef],
    multi_types: &'static [TypeLayout],
}

struct ParamDef(pub TypeLayout, pub InputModifier);

const fn type_from_str(name: &str) -> TypeLayout {
    let layer = match TypeLayer::from_numeric_str(name) {
        Some(layer) => layer,
        None => panic!(),
    };

    TypeLayout::from_numeric_layer_or_fail(layer)
}

macro_rules! return_type {
    (T) => {
        TypeLayout::TemplateParam(TemplateTypeId(0))
    };

    (D) => {
        TypeLayout::TemplateParam(TemplateTypeId(u32::MAX))
    };

    (M) => {
        TypeLayout::TemplateParam(TemplateTypeId(u32::MAX - 1))
    };

    (void) => {
        TypeLayout::Void
    };

    ($ty:ident) => {
        type_from_str(stringify!($ty))
    };
}

macro_rules! param_type {
    (T) => {
        ParamDef(
            TypeLayout::TemplateParam(TemplateTypeId(0)),
            InputModifier::In,
        )
    };

    (D) => {
        ParamDef(
            TypeLayout::TemplateParam(TemplateTypeId(u32::MAX)),
            InputModifier::In,
        )
    };

    (M) => {
        ParamDef(
            TypeLayout::TemplateParam(TemplateTypeId(u32::MAX - 1)),
            InputModifier::In,
        )
    };

    (out M) => {
        ParamDef(
            TypeLayout::TemplateParam(TemplateTypeId(u32::MAX - 1)),
            InputModifier::Out,
        )
    };

    (SamplerState) => {
        ParamDef(
            TypeLayout::Object(ObjectType::SamplerState),
            InputModifier::In,
        )
    };

    ($ty:ident) => {
        ParamDef(type_from_str(stringify!($ty)), InputModifier::In)
    };

    (out $ty:ident) => {
        ParamDef(type_from_str(stringify!($ty)), InputModifier::Out)
    };

    (inout $ty:ident) => {
        ParamDef(type_from_str(stringify!($ty)), InputModifier::InOut)
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
            multi_types: &[$($(type_from_str(stringify!($($multi_types)+))),*)?],
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

    f! { M abs(M) => Abs | int, int2, int3, int4, float, float2, float3, float4 },

    f! { M acos(M) => Acos | float, float2, float3, float4 },
    f! { M asin(M) => Asin | float, float2, float3, float4 },

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

    f! { float asfloat(int) => AsFloat },
    f! { float2 asfloat(int2) => AsFloat },
    f! { float3 asfloat(int3) => AsFloat },
    f! { float4 asfloat(int4) => AsFloat },
    f! { float asfloat(uint) => AsFloat },
    f! { float2 asfloat(uint2) => AsFloat },
    f! { float3 asfloat(uint3) => AsFloat },
    f! { float4 asfloat(uint4) => AsFloat },
    f! { float asfloat(float) => AsFloat },
    f! { float2 asfloat(float2) => AsFloat },
    f! { float3 asfloat(float3) => AsFloat },
    f! { float4 asfloat(float4) => AsFloat },

    f! { M exp(M) => Exp | float, float2, float3, float4 },

    f! { double asdouble(uint, uint) => AsDouble },

    f! { M clamp(M, M, M) => Clamp | int, int2, int3, int4, float, float2, float3, float4 },

    f! { M cos(M) => Cos | float, float2, float3, float4 },

    f! { float3 cross(float3, float3) => Cross },

    f! { float distance(M, M) => Distance | float1, float2, float3, float4 },

    f! { int dot(M, M) => Dot | int1, int2, int3, int4 },
    f! { float dot(M, M) => Dot | float1, float2, float3, float4 },

    f! { float3 mul(float3x3, float3) => Mul },
    f! { float4 mul(float4x4, float4) => Mul },

    f! { float f16tof32(uint) => F16ToF32 },
    f! { uint f32tof16(float) => F32ToF16 },

    f! { M floor(M) => Floor | float, float2, float3, float4 },

    f! { M lerp(M, M, M) => Lerp | float, float2, float3, float4 },

    f! { bool isnan(float) => IsNaN },
    f! { bool2 isnan(float2) => IsNaN },
    f! { bool3 isnan(float3) => IsNaN },
    f! { bool4 isnan(float4) => IsNaN },

    f! { float length(M) => Length | float1, float2, float3, float4 },

    f! { M min(M, M) => Min | int, int2, int3, int4, float, float2, float3, float4 },
    f! { M max(M, M) => Max | int, int2, int3, int4, float, float2, float3, float4 },

    f! { float1 normalize(float1) => Normalize },
    f! { float2 normalize(float2) => Normalize },
    f! { float3 normalize(float3) => Normalize },
    f! { float4 normalize(float4) => Normalize },

    f! { M pow(M, M) => Pow | float, float2, float3, float4 },

    f! { M saturate(M) => Saturate | float, float2, float3, float4 },

    f! { int sign(int) => Sign },
    f! { int2 sign(int2) => Sign },
    f! { int3 sign(int3) => Sign },
    f! { int4 sign(int4) => Sign },
    f! { int sign(float) => Sign },
    f! { int2 sign(float2) => Sign },
    f! { int3 sign(float3) => Sign },
    f! { int4 sign(float4) => Sign },

    f! { M sin(M) => Sin | float, float2, float3, float4 },

    f! { void sincos(M, out M, out M) => Sincos | float, float2, float3, float4 },

    f! { M smoothstep(M, M, M) => SmoothStep | float, float2, float3, float4 },

    f! { M sqrt(M) => Sqrt | float, float2, float3, float4 },

    f! { M step(M, M) => Step | float, float2, float3, float4 },

    f! { uint WaveGetLaneCount() => WaveGetLaneCount },
    f! { uint WaveGetLaneIndex() => WaveGetLaneIndex },
    f! { bool WaveIsFirstLane() => WaveIsFirstLane },

    f! { bool WaveActiveAnyTrue(bool) => WaveActiveAnyTrue },
    f! { bool WaveActiveAllTrue(bool) => WaveActiveAllTrue },
    f! { uint4 WaveActiveBallot(bool) => WaveActiveBallot },

    f! { M WaveReadLaneAt(M, uint) => WaveReadLaneAt | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveReadLaneFirst(M) => WaveReadLaneFirst | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { bool WaveActiveAllEqual(M) => WaveActiveAllEqual | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { uint WaveActiveCountBits(bool) => WaveActiveCountBits },
    f! { M WaveActiveSum(M) => WaveActiveSum | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveActiveProduct(M) => WaveActiveProduct | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveActiveBitAnd(M) => WaveActiveBitAnd | uint, uint2, uint3, uint4, int, int2, int3, int4 },
    f! { M WaveActiveBitOr(M) => WaveActiveBitOr | uint, uint2, uint3, uint4, int, int2, int3, int4 },
    f! { M WaveActiveBitXor(M) => WaveActiveBitXor | uint, uint2, uint3, uint4, int, int2, int3, int4 },
    f! { M WaveActiveMin(M) => WaveActiveMin | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WaveActiveMax(M) => WaveActiveMax | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { uint WavePrefixCountBits(bool) => WavePrefixCountBits },
    f! { M WavePrefixProduct(M) => WavePrefixProduct | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M WavePrefixSum(M) => WavePrefixSum | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },

    f! { M QuadReadAcrossX(M) => QuadReadAcrossX | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M QuadReadAcrossY(M) => QuadReadAcrossY | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M QuadReadAcrossDiagonal(M) => QuadReadAcrossDiagonal | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
    f! { M QuadReadLaneAt(M, uint) => QuadReadLaneAt | uint, uint2, uint3, uint4, int, int2, int3, int4, float, float2, float3, float4 },
];

/// Create a collection of all the intrinsic functions
pub fn add_intrinsics(module: &mut Module) {
    for def in INTRINSICS {
        let multi_types = if def.multi_types.is_empty() {
            &[TypeLayout::Void]
        } else {
            def.multi_types
        };
        for multi_type in multi_types {
            // Replace the multi types
            let remap_inner = |ty| {
                if let TypeLayout::TemplateParam(v) = ty {
                    if v.0 == u32::MAX - 1 {
                        return multi_type.clone();
                    }
                };
                ty
            };

            // Fetch and remap the param types
            let mut param_types = Vec::with_capacity(def.param_types.len());
            for param_type_def in def.param_types {
                param_types.push(ParamType(
                    module
                        .type_registry
                        .register_type(remap_inner(param_type_def.0.clone())),
                    param_type_def.1,
                    None,
                ));
            }

            // Register the return type
            let return_type = FunctionReturn {
                return_type: module
                    .type_registry
                    .register_type(remap_inner(def.return_type.clone())),
                semantic: None,
            };

            // Calculate the number of template arguments to the function
            let template_params =
                get_template_param_count(module, return_type.return_type, &param_types);

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

const BUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => BufferGetDimensions },
    f! { D Load(int) => BufferLoad },
    f! { D Load(int, uint) => BufferLoad },
];

const RWBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => RWBufferGetDimensions },
    f! { D Load(int) => RWBufferLoad },
    f! { D Load(int, uint) => RWBufferLoad },
];

const STRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint) => StructuredBufferGetDimensions },
    f! { D Load(int) => StructuredBufferLoad },
    f! { D Load(int, uint) => StructuredBufferLoad },
];

const RWSTRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint, out uint) => RWStructuredBufferGetDimensions },
    f! { D Load(int) => RWStructuredBufferLoad },
    f! { D Load(int, uint) => RWStructuredBufferLoad },
];

const TEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { D Sample(SamplerState, float2) => Texture2DSample },
    f! { D Load(int3) => Texture2DLoad },
];

const RWTEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[f! { D Load(int2) => RWTexture2DLoad }];

const BYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => ByteAddressBufferGetDimensions },
    f! { uint Load(uint) => ByteAddressBufferLoad },
    f! { uint Load(uint, uint) => ByteAddressBufferLoad },
    f! { uint2 Load2(uint) => ByteAddressBufferLoad2 },
    f! { uint2 Load2(uint, uint) => ByteAddressBufferLoad2 },
    f! { uint3 Load3(uint) => ByteAddressBufferLoad3 },
    f! { uint3 Load3(uint, uint) => ByteAddressBufferLoad3 },
    f! { uint4 Load4(uint) => ByteAddressBufferLoad4 },
    f! { uint4 Load4(uint, uint) => ByteAddressBufferLoad4 },
    f! { T Load(uint) => ByteAddressBufferLoadT },
    f! { T Load(uint, uint) => ByteAddressBufferLoadT },
];

const RWBYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { void GetDimensions(out uint) => RWByteAddressBufferGetDimensions },
    f! { uint Load(uint) => RWByteAddressBufferLoad },
    f! { uint Load(uint, uint) => RWByteAddressBufferLoad },
    f! { uint2 Load2(uint) => RWByteAddressBufferLoad2 },
    f! { uint2 Load2(uint, uint) => RWByteAddressBufferLoad2 },
    f! { uint3 Load3(uint) => RWByteAddressBufferLoad3 },
    f! { uint3 Load3(uint, uint) => RWByteAddressBufferLoad3 },
    f! { uint4 Load4(uint) => RWByteAddressBufferLoad4 },
    f! { uint4 Load4(uint, uint) => RWByteAddressBufferLoad4 },
    f! { T Load(uint) => RWByteAddressBufferLoadT },
    f! { T Load(uint, uint) => RWByteAddressBufferLoadT },
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
    let inner_type = match object {
        ObjectType::Buffer(ty)
        | ObjectType::RWBuffer(ty)
        | ObjectType::Texture2D(ty)
        | ObjectType::RWTexture2D(ty)
        | ObjectType::StructuredBuffer(ty)
        | ObjectType::RWStructuredBuffer(ty) => {
            Some(module.type_registry.get_type_layout(ty).clone())
        }
        _ => None,
    };

    let mut methods = Vec::new();
    for def in method_defs {
        assert!(def.multi_types.is_empty());

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
        let mut param_types = Vec::with_capacity(def.param_types.len());
        for param_type_def in def.param_types {
            param_types.push(ParamType(
                module
                    .type_registry
                    .register_type(remap_inner(param_type_def.0.clone())),
                param_type_def.1,
                None,
            ));
        }

        // Register the return type
        let return_type = FunctionReturn {
            return_type: module
                .type_registry
                .register_type(remap_inner(def.return_type.clone())),
            semantic: None,
        };

        // Calculate the number of template arguments to the function
        let template_params =
            get_template_param_count(module, return_type.return_type, &param_types);

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

/// Get the maximum template argument count within the function
fn get_template_param_count(
    module: &Module,
    return_type: TypeId,
    param_types: &[ParamType],
) -> TemplateParamCount {
    let mut count = get_max_template_arg(module, return_type);
    for param_type in param_types {
        count = std::cmp::max(count, get_max_template_arg(module, param_type.0))
    }
    TemplateParamCount(count)
}

/// Get the maximum template argument count within the type
fn get_max_template_arg(module: &Module, id: TypeId) -> u32 {
    let mut count = 0;
    let layer = module.type_registry.get_type_layer(id);
    match layer {
        TypeLayer::Void
        | TypeLayer::Scalar(_)
        | TypeLayer::Vector(_, _)
        | TypeLayer::Matrix(_, _, _)
        | TypeLayer::Struct(_)
        | TypeLayer::StructTemplate(_)
        | TypeLayer::Object(_) => {}
        TypeLayer::Array(inner, _) | TypeLayer::Modifier(_, inner) => {
            count = std::cmp::max(count, get_max_template_arg(module, inner))
        }
        TypeLayer::TemplateParam(template_arg) => count = std::cmp::max(count, template_arg.0 + 1),
    }
    count
}
