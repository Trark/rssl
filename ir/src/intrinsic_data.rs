use crate::*;

type IntrinsicDefinition = (
    &'static str,
    Intrinsic,
    u32,
    TypeLayout,
    &'static [ParamType],
);

use TypeLayout::Void;

const T_INT_TY: TypeLayout = TypeLayout::Scalar(ScalarType::Int);
const T_INT2_TY: TypeLayout = TypeLayout::Vector(ScalarType::Int, 2);
const T_INT3_TY: TypeLayout = TypeLayout::Vector(ScalarType::Int, 3);
const T_UINT_TY: TypeLayout = TypeLayout::Scalar(ScalarType::UInt);
const T_UINT2_TY: TypeLayout = TypeLayout::Vector(ScalarType::UInt, 2);
const T_UINT3_TY: TypeLayout = TypeLayout::Vector(ScalarType::UInt, 3);
const T_UINT4_TY: TypeLayout = TypeLayout::Vector(ScalarType::UInt, 4);
const T_FLOAT2_TY: TypeLayout = TypeLayout::Vector(ScalarType::Float, 2);
const T_FLOAT4_TY: TypeLayout = TypeLayout::Vector(ScalarType::Float, 4);
const T_SAMPLER_TY: TypeLayout = TypeLayout::Object(ObjectType::SamplerState);
const T_TEMPLATE0_TY: TypeLayout = TypeLayout::TemplateParam(TemplateTypeId(0));
const T_OBJECT_ARG_TY: TypeLayout = TypeLayout::TemplateParam(TemplateTypeId(u32::MAX));

const T_INT: ParamType = ParamType(T_INT_TY, InputModifier::In, None);
const T_INT2: ParamType = ParamType(T_INT2_TY, InputModifier::In, None);
const T_INT3: ParamType = ParamType(T_INT3_TY, InputModifier::In, None);
const T_UINT: ParamType = ParamType(T_UINT_TY, InputModifier::In, None);
const T_UINT2: ParamType = ParamType(T_UINT2_TY, InputModifier::In, None);
const T_UINT3: ParamType = ParamType(T_UINT3_TY, InputModifier::In, None);
const T_UINT4: ParamType = ParamType(T_UINT4_TY, InputModifier::In, None);
const T_UINT_OUT: ParamType = ParamType(T_UINT_TY, InputModifier::Out, None);
const T_FLOAT2: ParamType = ParamType(T_FLOAT2_TY, InputModifier::In, None);
const T_SAMPLER: ParamType = ParamType(T_SAMPLER_TY, InputModifier::In, None);
const T_TEMPLATE0: ParamType = ParamType(T_TEMPLATE0_TY, InputModifier::In, None);

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
        T_FLOAT4_TY,
        &[T_SAMPLER, T_FLOAT2],
    ),
    ("Load", Intrinsic::Texture2DLoad, 0, T_FLOAT4_TY, &[T_INT3]),
];

const RWTEXTURE2D_INTRINSICS: &[IntrinsicDefinition] = &[(
    "Load",
    Intrinsic::RWTexture2DLoad,
    0,
    T_FLOAT4_TY,
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
pub fn get_methods(object: &ObjectType) -> Vec<MethodDefinition> {
    // Pick the functions based on the object base type
    let method_defs = match object {
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
    for &(method_name, ref intrinsic, template_param_count, ref return_type, param_types) in
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
        let param_types = param_types
            .iter()
            .map(|p| {
                let mut p = p.clone();
                p.0 = remap_inner(p.0);
                p
            })
            .collect::<Vec<_>>();

        // Fetch and remap the return type
        let return_type = remap_inner(return_type.clone());

        methods.push(MethodDefinition {
            name: method_name.to_string(),
            intrinsic: intrinsic.clone(),
            signature: FunctionSignature {
                return_type: FunctionReturn {
                    return_type,
                    semantic: None,
                },
                template_params: TemplateParamCount(template_param_count),
                param_types,
            },
        });
    }

    methods
}
