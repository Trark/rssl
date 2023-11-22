pub use rssl_ir::name_generator::{NameMap, NameSymbol, ScopedName};

/// List of names we want to avoid in the generated code
pub const RESERVED_NAMES: &[&str] = &[
    "abs",
    "acos",
    "all",
    "AllMemoryBarrier",
    "AllMemoryBarrierWithGroupSync",
    "and",
    "any",
    "asdouble",
    "asfloat",
    "asin",
    "asint",
    "asuint",
    "atan",
    "atan2",
    "auto",
    "break",
    "Buffer",
    "ByteAddressBuffer",
    "CANDIDATE_NON_OPAQUE_TRIANGLE",
    "CANDIDATE_PROCEDURAL_PRIMITIVE",
    "case",
    "catch",
    "cbuffer",
    "ceil",
    "char",
    "clamp",
    "class",
    "column_major",
    "COMMITTED_NOTHING",
    "COMMITTED_PROCEDURAL_PRIMITIVE_HIT",
    "COMMITTED_TRIANGLE_HIT",
    "const",
    "const_cast",
    "ConstantBuffer",
    "constexpr",
    "continue",
    "cos",
    "cosh",
    "countbits",
    "cross",
    "ddx",
    "ddx_coarse",
    "ddx_fine",
    "ddy",
    "ddy_coarse",
    "ddy_fine",
    "decltype",
    "default",
    "delete",
    "DeviceMemoryBarrier",
    "DeviceMemoryBarrierWithGroupSync",
    "discard",
    "DispatchMesh",
    "distance",
    "do",
    "dot",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "exp",
    "exp2",
    "explicit",
    "extern",
    "f16tof32",
    "f32tof16",
    "false",
    "firstbithigh",
    "firstbitlow",
    "float",
    "float16_t",
    "floor",
    "fmod",
    "for",
    "frac",
    "friend",
    "goto",
    "GroupMemoryBarrier",
    "GroupMemoryBarrierWithGroupSync",
    "groupshared",
    "half",
    "if",
    "in",
    "inline",
    "inout",
    "int",
    "int64_t",
    "InterlockedAdd",
    "InterlockedAnd",
    "InterlockedCompareExchange",
    "InterlockedCompareStore",
    "InterlockedExchange",
    "InterlockedMax",
    "InterlockedMin",
    "InterlockedOr",
    "InterlockedXor",
    "isfinite",
    "isinf",
    "isnan",
    "length",
    "lerp",
    "log",
    "log10",
    "log2",
    "long",
    "matrix",
    "max",
    "min",
    "modf",
    "mul",
    "mutable",
    "namespace",
    "new",
    "NonUniformResourceIndex",
    "normalize",
    "operator",
    "or",
    "out",
    "packoffset",
    "pow",
    "private",
    "protected",
    "public",
    "QuadReadAcrossDiagonal",
    "QuadReadAcrossX",
    "QuadReadAcrossY",
    "QuadReadLaneAt",
    "RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH",
    "RAY_FLAG_CULL_BACK_FACING_TRIANGLES",
    "RAY_FLAG_CULL_FRONT_FACING_TRIANGLES",
    "RAY_FLAG_CULL_NON_OPAQUE",
    "RAY_FLAG_CULL_OPAQUE",
    "RAY_FLAG_FORCE_NON_OPAQUE",
    "RAY_FLAG_FORCE_OPAQUE",
    "RAY_FLAG_NONE",
    "RAY_FLAG_SKIP_CLOSEST_HIT_SHADER",
    "RAY_FLAG_SKIP_PROCEDURAL_PRIMITIVES",
    "RAY_FLAG_SKIP_TRIANGLES",
    "RayDesc",
    "RayQuery",
    "RaytracingAccelerationStructure",
    "rcp",
    "reflect",
    "refract",
    "register",
    "reinterpret_cast",
    "return",
    "reversebits",
    "round",
    "row_major",
    "rsqrt",
    "RWBuffer",
    "RWByteAddressBuffer",
    "RWStructuredBuffer",
    "RWTexture2D",
    "RWTexture2DArray",
    "RWTexture3D",
    "SamplerComparisonState",
    "SamplerState,",
    "saturate",
    "select",
    "SetMeshOutputCounts",
    "short",
    "sign",
    "signed",
    "sin",
    "sincos",
    "sinh",
    "sizeof",
    "smoothstep",
    "snorm",
    "sqrt",
    "static",
    "static_cast",
    "step",
    "struct",
    "StructuredBuffer",
    "switch",
    "tan",
    "tanh",
    "template",
    "Texture2D",
    "Texture2DArray",
    "Texture3D",
    "TextureCube",
    "TextureCubeArray",
    "this",
    "throw",
    "transpose",
    "TriangleStream",
    "true",
    "trunc",
    "try",
    "typedef",
    "typename",
    "uint",
    "uint64_t",
    "union",
    "unorm",
    "unsigned",
    "using",
    "vector",
    "virtual",
    "void",
    "volatile",
    "WaveActiveAllEqual",
    "WaveActiveAllTrue",
    "WaveActiveAnyTrue",
    "WaveActiveBallot",
    "WaveActiveBitAnd",
    "WaveActiveBitOr",
    "WaveActiveBitXor",
    "WaveActiveCountBits",
    "WaveActiveMax",
    "WaveActiveMin",
    "WaveActiveProduct",
    "WaveActiveSum",
    "WaveGetLaneCount",
    "WaveGetLaneIndex",
    "WaveIsFirstLane",
    "WavePrefixCountBits",
    "WavePrefixProduct",
    "WavePrefixSum",
    "WaveReadLaneAt",
    "WaveReadLaneFirst",
    "while",
];
