use crate::*;

/// An intrinsic built in operator
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum IntrinsicOp {
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
    LeftShiftAssignment,
    RightShiftAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,

    // Internal operations
    MakeSigned,
    MakeSignedPushZero,

    // MeshOutput methods
    MeshOutputSetVertex,
    MeshOutputSetPrimitive,
    MeshOutputSetIndices,
}

/// An intrinsic built in function
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Intrinsic {
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
    And,
    Or,
    Select,

    Abs,

    // Transcendental functions
    Acos,
    Asin,
    Atan,
    Atan2,
    Cos,
    Cosh,
    Sin,
    Sinh,
    Sincos,
    Tan,
    Tanh,
    Sqrt,
    RcpSqrt,
    Pow,
    Exp,
    Exp2,
    Log,
    Log2,
    Log10,

    F16ToF32,
    F32ToF16,

    Floor,
    Ceil,
    Trunc,
    Round,
    Frac,
    Modf,
    Fmod,

    IsNaN,
    IsInfinite,
    IsFinite,

    Length,
    Normalize,
    Rcp,

    Reflect,
    Refract,

    CountBits,
    ReverseBits,
    FirstBitHigh,
    FirstBitLow,

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

    Transpose,
    Determinant,

    // Derivatives
    DDX,
    DDXCoarse,
    DDXFine,
    DDY,
    DDYCoarse,
    DDYFine,

    // Atomics
    InterlockedAdd,
    InterlockedAnd,
    InterlockedCompareExchange,
    InterlockedCompareStore,
    InterlockedExchange,
    InterlockedMax,
    InterlockedMin,
    InterlockedOr,
    InterlockedXor,

    NonUniformResourceIndex,

    // Wave Query Intrinsics
    WaveGetLaneCount,
    WaveGetLaneIndex,
    WaveIsFirstLane,

    // Wave Vote Intrinsics
    WaveActiveAnyTrue,
    WaveActiveAllTrue,
    WaveActiveBallot,

    // Wave Broadcast Intrinsics
    WaveReadLaneAt,
    WaveReadLaneFirst,

    // Wave Reduction Intrinsics
    WaveActiveAllEqual,
    WaveActiveCountBits,
    WaveActiveSum,
    WaveActiveProduct,
    WaveActiveBitAnd,
    WaveActiveBitOr,
    WaveActiveBitXor,
    WaveActiveMin,
    WaveActiveMax,

    // Wave Scan/Prefix Intrinsics
    WavePrefixCountBits,
    WavePrefixProduct,
    WavePrefixSum,

    // Quad Shuffle Operations
    QuadReadAcrossX,
    QuadReadAcrossY,
    QuadReadAcrossDiagonal,
    QuadReadLaneAt,

    // Mesh/Task shader intrinsics
    SetMeshOutputCounts,
    DispatchMesh,

    // Buffer methods
    BufferGetDimensions,
    BufferLoad,

    // RWBuffer methods
    RWBufferGetDimensions,
    RWBufferLoad,
    RWBufferStore,

    // StructuredBuffer methods
    StructuredBufferGetDimensions,
    StructuredBufferLoad,

    // RWStructuredBuffer methods
    RWStructuredBufferGetDimensions,
    RWStructuredBufferLoad,
    RWStructuredBufferStore,

    // ByteAddressBuffer methods
    ByteAddressBufferGetDimensions,
    ByteAddressBufferLoad,
    ByteAddressBufferLoad2,
    ByteAddressBufferLoad3,
    ByteAddressBufferLoad4,
    ByteAddressBufferLoadT,

    // RWByteAddressBuffer methods
    RWByteAddressBufferGetDimensions,
    RWByteAddressBufferLoad,
    RWByteAddressBufferLoad2,
    RWByteAddressBufferLoad3,
    RWByteAddressBufferLoad4,
    RWByteAddressBufferLoadT,
    RWByteAddressBufferStore,
    RWByteAddressBufferStore2,
    RWByteAddressBufferStore3,
    RWByteAddressBufferStore4,
    RWByteAddressBufferInterlockedAdd,
    RWByteAddressBufferInterlockedAnd,
    RWByteAddressBufferInterlockedCompareExchange,
    RWByteAddressBufferInterlockedCompareStore,
    RWByteAddressBufferInterlockedExchange,
    RWByteAddressBufferInterlockedMax,
    RWByteAddressBufferInterlockedMin,
    RWByteAddressBufferInterlockedOr,
    RWByteAddressBufferInterlockedXor,

    // BufferAddress methods
    BufferAddressLoad,

    // RWBufferAddress methods
    RWBufferAddressLoad,
    RWBufferAddressStore,

    // Texture2D methods
    Texture2DGetDimensions,
    Texture2DLoad,
    Texture2DSample,
    Texture2DSampleBias,
    Texture2DSampleCmp,
    Texture2DSampleCmpLevelZero,
    Texture2DSampleGrad,
    Texture2DSampleLevel,
    Texture2DGatherRed,
    Texture2DGatherGreen,
    Texture2DGatherBlue,
    Texture2DGatherAlpha,
    Texture2DGatherCmpRed,
    Texture2DGatherCmpGreen,
    Texture2DGatherCmpBlue,
    Texture2DGatherCmpAlpha,

    // Texture2DArray methods
    Texture2DArrayGetDimensions,
    Texture2DArrayLoad,
    Texture2DArraySample,
    Texture2DArraySampleBias,
    Texture2DArraySampleCmp,
    Texture2DArraySampleCmpLevelZero,
    Texture2DArraySampleGrad,
    Texture2DArraySampleLevel,
    Texture2DArrayGatherRed,
    Texture2DArrayGatherGreen,
    Texture2DArrayGatherBlue,
    Texture2DArrayGatherAlpha,
    Texture2DArrayGatherCmpRed,
    Texture2DArrayGatherCmpGreen,
    Texture2DArrayGatherCmpBlue,
    Texture2DArrayGatherCmpAlpha,

    // RWTexture2D methods
    RWTexture2DGetDimensions,
    RWTexture2DLoad,
    RWTexture2DStore,

    // RWTexture2DArray methods
    RWTexture2DArrayGetDimensions,
    RWTexture2DArrayLoad,
    RWTexture2DArrayStore,

    // TextureCube methods
    TextureCubeSample,
    TextureCubeSampleLevel,

    // TextureCubeArray methods
    TextureCubeArraySample,
    TextureCubeArraySampleLevel,

    // Texture3D methods
    Texture3DGetDimensions,
    Texture3DLoad,
    Texture3DSample,
    Texture3DSampleBias,
    Texture3DSampleGrad,
    Texture3DSampleLevel,

    // RWTexture3D methods
    RWTexture3DGetDimensions,
    RWTexture3DLoad,
    RWTexture3DStore,

    // TriangleStream methods
    TriangleStreamAppend,
    TriangleStreamRestartStrip,

    // RayQuery methods
    RayQueryTraceRayInline,
    RayQueryProceed,
    RayQueryAbort,
    RayQueryCommittedStatus,
    RayQueryCandidateType,
    RayQueryCandidateProceduralPrimitiveNonOpaque,
    RayQueryCommitNonOpaqueTriangleHit,
    RayQueryCommitProceduralPrimitiveHit,
    RayQueryRayFlags,
    RayQueryWorldRayOrigin,
    RayQueryWorldRayDirection,
    RayQueryRayTMin,
    RayQueryCandidateTriangleRayT,
    RayQueryCommittedRayT,
    RayQueryCandidateInstanceIndex,
    RayQueryCandidateInstanceID,
    RayQueryCandidateInstanceContributionToHitGroupIndex,
    RayQueryCandidateGeometryIndex,
    RayQueryCandidatePrimitiveIndex,
    RayQueryCandidateObjectRayOrigin,
    RayQueryCandidateObjectRayDirection,
    RayQueryCandidateObjectToWorld3x4,
    RayQueryCandidateObjectToWorld4x3,
    RayQueryCandidateWorldToObject3x4,
    RayQueryCandidateWorldToObject4x3,
    RayQueryCommittedInstanceIndex,
    RayQueryCommittedInstanceID,
    RayQueryCommittedInstanceContributionToHitGroupIndex,
    RayQueryCommittedGeometryIndex,
    RayQueryCommittedPrimitiveIndex,
    RayQueryCommittedObjectRayOrigin,
    RayQueryCommittedObjectRayDirection,
    RayQueryCommittedObjectToWorld3x4,
    RayQueryCommittedObjectToWorld4x3,
    RayQueryCommittedWorldToObject3x4,
    RayQueryCommittedWorldToObject4x3,
    RayQueryCandidateTriangleBarycentrics,
    RayQueryCandidateTriangleFrontFace,
    RayQueryCommittedTriangleBarycentrics,
    RayQueryCommittedTriangleFrontFace,
}

impl IntrinsicOp {
    pub fn get_return_type(
        &self,
        param_types: &[ExpressionType],
        module: &Module,
    ) -> ExpressionType {
        use IntrinsicOp::*;
        match *self {
            PrefixIncrement | PrefixDecrement => {
                assert_eq!(param_types.len(), 1);
                param_types[0]
            }
            PostfixIncrement | PostfixDecrement | Plus | Minus => {
                assert_eq!(param_types.len(), 1);
                module
                    .type_registry
                    .remove_modifier(param_types[0].0)
                    .to_rvalue()
            }
            LogicalNot => {
                assert_eq!(param_types.len(), 1);
                let type_id = module.type_registry.remove_modifier(param_types[0].0);
                match module.type_registry.get_type_layer(type_id) {
                    TypeLayer::Scalar(_) => module
                        .type_registry
                        .register_type(TypeLayer::Scalar(ScalarType::Bool))
                        .to_rvalue(),
                    TypeLayer::Vector(_, x) => {
                        let bool_ty = module
                            .type_registry
                            .register_type(TypeLayer::Scalar(ScalarType::Bool));
                        module
                            .type_registry
                            .register_type(TypeLayer::Vector(bool_ty, x))
                            .to_rvalue()
                    }
                    _ => panic!("invalid logical not intrinsic"),
                }
            }
            BitwiseNot => {
                assert_eq!(param_types.len(), 1);
                module
                    .type_registry
                    .remove_modifier(param_types[0].0)
                    .to_rvalue()
            }
            Add | Subtract | Divide | Modulus | LeftShift | RightShift | BitwiseAnd | BitwiseOr
            | BitwiseXor => {
                assert_eq!(param_types.len(), 2);
                assert_eq!(
                    param_types[0].0,
                    param_types[1].0,
                    "{} != {}",
                    module.get_type_name_short(param_types[0].0),
                    module.get_type_name_short(param_types[1].0),
                );
                param_types[0].0.to_rvalue()
            }
            Multiply => {
                assert_eq!(param_types.len(), 2);
                // Eventually multiply should support vector <-> matrix operations, but currently it does not
                // So currently both sides of the operation have the same type (after vector expansion / scalar conversion)
                assert_eq!(
                    param_types[0].0,
                    param_types[1].0,
                    "{} != {}",
                    module.get_type_name_short(param_types[0].0),
                    module.get_type_name_short(param_types[1].0),
                );
                param_types[0].0.to_rvalue()
            }
            BooleanAnd | BooleanOr => {
                assert_eq!(param_types.len(), 2);
                assert_eq!(param_types[0].0, param_types[1].0);
                param_types[0].0.to_rvalue()
            }
            LessThan | LessEqual | GreaterThan | GreaterEqual | Equality | Inequality => {
                assert_eq!(param_types.len(), 2);
                assert_eq!(
                    param_types[0].0,
                    param_types[1].0,
                    "{} != {}",
                    module.get_type_name_short(param_types[0].0),
                    module.get_type_name_short(param_types[1].0),
                );
                let ty = module
                    .type_registry
                    .transform_scalar(param_types[0].0, ScalarType::Bool);
                ty.to_rvalue()
            }
            Assignment | SumAssignment | DifferenceAssignment | ProductAssignment
            | QuotientAssignment | RemainderAssignment | LeftShiftAssignment
            | RightShiftAssignment | BitwiseAndAssignment | BitwiseOrAssignment
            | BitwiseXorAssignment => {
                assert_eq!(param_types.len(), 2);
                assert_eq!(param_types[0].0, param_types[1].0);
                assert_eq!(param_types[0].1, ValueType::Lvalue);
                param_types[0]
            }
            MakeSigned => {
                assert_eq!(param_types.len(), 1);
                let ty = module
                    .type_registry
                    .transform_scalar(param_types[0].0, ScalarType::Int32);
                ty.to_rvalue()
            }
            MakeSignedPushZero => {
                assert_eq!(param_types.len(), 1);
                let tyl = module.type_registry.get_type_layer(param_types[0].0);
                let output_ty = match tyl {
                    TypeLayer::Vector(_, x) => {
                        let new_inner = module
                            .type_registry
                            .register_type(TypeLayer::Scalar(ScalarType::Int32));
                        module
                            .type_registry
                            .register_type(TypeLayer::Vector(new_inner, x + 1))
                    }
                    _ => panic!("Invalid {:?}", self),
                };
                output_ty.to_rvalue()
            }
            MeshOutputSetVertex | MeshOutputSetPrimitive | MeshOutputSetIndices => module
                .type_registry
                .register_type(TypeLayer::Void)
                .to_rvalue(),
        }
    }
}
