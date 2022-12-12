use crate::*;
use rssl_text::Located;

type IntrinsicDefinition = (&'static str, Intrinsic, TypeLayout, &'static [ParamDef]);

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
    ($return_type:tt $name:tt ($($($param_type:ident)+),*) => $intrinsic:ident) => {
        (
            stringify!($name),
            Intrinsic::$intrinsic,
            return_type!($return_type),
            &[$(param_type!($($param_type)+)),*],
        )
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

    f! { bool all(bool) => All },
    f! { bool all(bool2) => All },
    f! { bool all(bool3) => All },
    f! { bool all(bool4) => All },

    f! { bool any(bool) => Any },
    f! { bool any(bool2) => Any },
    f! { bool any(bool3) => Any },
    f! { bool any(bool4) => Any },

    f! { int abs(int) => Abs },
    f! { int2 abs(int2) => Abs },
    f! { int3 abs(int3) => Abs },
    f! { int4 abs(int4) => Abs },
    f! { float abs(float) => Abs },
    f! { float2 abs(float2) => Abs },
    f! { float3 abs(float3) => Abs },
    f! { float4 abs(float4) => Abs },

    f! { float acos(float) => Acos },
    f! { float2 acos(float2) => Acos },
    f! { float3 acos(float3) => Acos },
    f! { float4 acos(float4) => Acos },

    f! { float asin(float) => Asin },
    f! { float2 asin(float2) => Asin },
    f! { float3 asin(float3) => Asin },
    f! { float4 asin(float4) => Asin },

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

    f! { float exp(float) => Exp },
    f! { float2 exp(float2) => Exp },
    f! { float3 exp(float3) => Exp },
    f! { float4 exp(float4) => Exp },

    f! { double asdouble(uint, uint) => AsDouble },

    f! { int clamp(int, int, int) => Clamp },
    f! { int2 clamp(int2, int2, int2) => Clamp },
    f! { int3 clamp(int3, int3, int3) => Clamp },
    f! { int4 clamp(int4, int4, int4) => Clamp },
    f! { float clamp(float, float, float) => Clamp },
    f! { float2 clamp(float2, float2, float2) => Clamp },
    f! { float3 clamp(float3, float3, float3) => Clamp },
    f! { float4 clamp(float4, float4, float4) => Clamp },

    f! { float cos(float) => Cos },
    f! { float2 cos(float2) => Cos },
    f! { float3 cos(float3) => Cos },
    f! { float4 cos(float4) => Cos },

    f! { float3 cross(float3, float3) => Cross },

    f! { float distance(float1, float1) => Distance },
    f! { float distance(float2, float2) => Distance },
    f! { float distance(float3, float3) => Distance },
    f! { float distance(float4, float4) => Distance },

    f! { int dot(int1, int1) => Dot },
    f! { int dot(int2, int2) => Dot },
    f! { int dot(int3, int3) => Dot },
    f! { int dot(int4, int4) => Dot },
    f! { float dot(float1, float1) => Dot },
    f! { float dot(float2, float2) => Dot },
    f! { float dot(float3, float3) => Dot },
    f! { float dot(float4, float4) => Dot },

    f! { float3 mul(float3x3, float3) => Mul },
    f! { float4 mul(float4x4, float4) => Mul },

    f! { float f16tof32(uint) => F16ToF32 },
    f! { uint f32tof16(float) => F32ToF16 },

    f! { float floor(float) => Floor },
    f! { float2 floor(float2) => Floor },
    f! { float3 floor(float3) => Floor },
    f! { float4 floor(float4) => Floor },

    f! { float lerp(float, float, float) => Lerp },
    f! { float2 lerp(float2, float2, float2) => Lerp },
    f! { float3 lerp(float3, float3, float3) => Lerp },
    f! { float4 lerp(float4, float4, float4) => Lerp },

    f! { bool isnan(float) => IsNaN },
    f! { bool2 isnan(float2) => IsNaN },
    f! { bool3 isnan(float3) => IsNaN },
    f! { bool4 isnan(float4) => IsNaN },

    f! { float length(float1) => Length },
    f! { float length(float2) => Length },
    f! { float length(float3) => Length },
    f! { float length(float4) => Length },

    f! { int min(int, int) => Min },
    f! { int2 min(int2, int2) => Min },
    f! { int3 min(int3, int3) => Min },
    f! { int4 min(int4, int4) => Min },
    f! { float min(float, float) => Min },
    f! { float2 min(float2, float2) => Min },
    f! { float3 min(float3, float3) => Min },
    f! { float4 min(float4, float4) => Min },

    f! { int max(int, int) => Max },
    f! { int2 max(int2, int2) => Max },
    f! { int3 max(int3, int3) => Max },
    f! { int4 max(int4, int4) => Max },
    f! { float max(float, float) => Max },
    f! { float2 max(float2, float2) => Max },
    f! { float3 max(float3, float3) => Max },
    f! { float4 max(float4, float4) => Max },

    f! { float1 normalize(float1) => Normalize },
    f! { float2 normalize(float2) => Normalize },
    f! { float3 normalize(float3) => Normalize },
    f! { float4 normalize(float4) => Normalize },

    f! { float pow(float, float) => Pow },
    f! { float2 pow(float2, float2) => Pow },
    f! { float3 pow(float3, float3) => Pow },
    f! { float4 pow(float4, float4) => Pow },

    f! { float saturate(float) => Saturate },
    f! { float2 saturate(float2) => Saturate },
    f! { float3 saturate(float3) => Saturate },
    f! { float4 saturate(float4) => Saturate },

    f! { int sign(int) => Sign },
    f! { int2 sign(int2) => Sign },
    f! { int3 sign(int3) => Sign },
    f! { int4 sign(int4) => Sign },
    f! { int sign(float) => Sign },
    f! { int2 sign(float2) => Sign },
    f! { int3 sign(float3) => Sign },
    f! { int4 sign(float4) => Sign },

    f! { float sin(float) => Sin },
    f! { float2 sin(float2) => Sin },
    f! { float3 sin(float3) => Sin },
    f! { float4 sin(float4) => Sin },

    f! { void sincos(float, out float, out float) => Sincos },
    f! { void sincos(float2, out float2, out float2) => Sincos },
    f! { void sincos(float3, out float3, out float3) => Sincos },
    f! { void sincos(float4, out float4, out float4) => Sincos },

    f! { float smoothstep(float, float, float) => SmoothStep },
    f! { float2 smoothstep(float2, float2, float2) => SmoothStep },
    f! { float3 smoothstep(float3, float3, float3) => SmoothStep },
    f! { float4 smoothstep(float4, float4, float4) => SmoothStep },

    f! { float sqrt(float) => Sqrt },
    f! { float2 sqrt(float2) => Sqrt },
    f! { float3 sqrt(float3) => Sqrt },
    f! { float4 sqrt(float4) => Sqrt },

    f! { float step(float, float) => Step },
    f! { float2 step(float2, float2) => Step },
    f! { float3 step(float3, float3) => Step },
    f! { float4 step(float4, float4) => Step },
];

/// Create a collection of all the intrinsic functions
pub fn add_intrinsics(module: &mut Module) {
    for &(ref name, ref intrinsic, ref return_type, param_type_defs) in INTRINSICS {
        // Fetch the param types
        let param_types = param_type_defs
            .iter()
            .map(|p| ParamType(module.type_registry.register_type(p.0.clone()), p.1, None))
            .collect::<Vec<_>>();

        // Register the return type
        let return_type = FunctionReturn {
            return_type: module.type_registry.register_type(return_type.clone()),
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

const BUFFER_INTRINSICS: &[IntrinsicDefinition] = &[f! { D Load(int) => BufferLoad }];

const RWBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[f! { D Load(int) => RWBufferLoad }];

const STRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] =
    &[f! { D Load(int) => StructuredBufferLoad }];

const RWSTRUCTUREDBUFFER_INTRINSICS: &[IntrinsicDefinition] =
    &[f! { D Load(int) => RWStructuredBufferLoad }];

const TEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { D Sample(SamplerState, float2) => Texture2DSample },
    f! { D Load(int3) => Texture2DLoad },
];

const RWTEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[f! { D Load(int2) => RWTexture2DLoad }];

const BYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { uint Load(uint) => ByteAddressBufferLoad },
    f! { uint2 Load2(uint) => ByteAddressBufferLoad2 },
    f! { uint3 Load3(uint) => ByteAddressBufferLoad3 },
    f! { uint4 Load4(uint) => ByteAddressBufferLoad4 },
    f! { T Load(uint) => ByteAddressBufferLoadT },
];

const RWBYTEADDRESSBUFFER_INTRINSICS: &[IntrinsicDefinition] = &[
    f! { uint Load(uint) => RWByteAddressBufferLoad },
    f! { uint2 Load2(uint) => RWByteAddressBufferLoad2 },
    f! { uint3 Load3(uint) => RWByteAddressBufferLoad3 },
    f! { uint4 Load4(uint) => RWByteAddressBufferLoad4 },
    f! { void Store(uint, uint) => RWByteAddressBufferStore },
    f! { void Store2(uint, uint2) => RWByteAddressBufferStore2 },
    f! { void Store3(uint, uint3) => RWByteAddressBufferStore3 },
    f! { void Store4(uint, uint4) => RWByteAddressBufferStore4 },
    f! { void InterlockedAdd(uint, uint, out uint) => RWByteAddressBufferInterlockedAdd },
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
    for &(method_name, ref intrinsic, ref return_type, param_type_defs) in method_defs {
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
            .map(|p| {
                ParamType(
                    module.type_registry.register_type(remap_inner(p.0.clone())),
                    p.1,
                    None,
                )
            })
            .collect::<Vec<_>>();

        // Fetch and remap the return type
        let return_type_layout = remap_inner(return_type.clone());
        let return_type = module.type_registry.register_type(return_type_layout);

        // Calculate the number of template arguments to the function
        let template_params = get_template_param_count(module, return_type, &param_types);

        methods.push(MethodDefinition {
            name: method_name.to_string(),
            intrinsic: intrinsic.clone(),
            signature: FunctionSignature {
                return_type: FunctionReturn {
                    return_type,
                    semantic: None,
                },
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
