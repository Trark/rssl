pub use crate::ApiLocation;

/// Description of a shader pipeline
#[derive(Clone, PartialEq, Eq, Default, Debug)]
pub struct PipelineDescription {
    pub bind_groups: Vec<BindGroup>,
}

/// Group of bindings
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BindGroup {
    /// Bindings of resource views
    pub bindings: Vec<DescriptorBinding>,

    /// Inline constant buffer to support other bindings
    pub inline_constants: Option<InlineConstantBuffer>,
}

/// Individual binding of a resource
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DescriptorBinding {
    /// Name of the binding
    pub name: String,

    /// Slot index for the binding in the group for the destination api
    pub api_binding: ApiLocation,

    /// Type of resource view the binding takes
    pub descriptor_type: DescriptorType,

    /// Number of descriptors for the binding - or None for an unbounded array
    pub descriptor_count: Option<u32>,

    /// If the descriptor is intended for bindless usage
    pub is_bindless: bool,

    /// If the descriptor is used by the shader
    pub is_used: bool,
}

/// Type of a descriptor
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DescriptorType {
    /// A constant buffer object - for constants stored in a buffer
    ConstantBuffer,

    /// A push constants block - for constants stored in the root data
    PushConstants,

    /// An inline constant buffer - for constants stored in the parameter tables
    InlineConstants,

    /// A ByteAddressBuffer or SSBO resource view
    ByteBuffer,

    /// A RWByteAddressBuffer or SSBO resource view
    RwByteBuffer,

    /// Raw buffer address or ByteBuffer if raw addresses are disabled
    BufferAddress,

    /// Raw buffer address or RwByteBuffer if raw addresses are disabled
    RwBufferAddress,

    /// A StructuredBuffer or SSBO resource view
    StructuredBuffer,

    /// A RWStructuredBuffer or SSBO resource view
    RwStructuredBuffer,

    /// A read-only sampled typed buffer view
    TexelBuffer,

    /// A read-only storage typed buffer view
    RwTexelBuffer,

    /// A read-only sampled 2d texture
    Texture2d,

    /// A read-only sampled 2d texture array
    Texture2dArray,

    /// A read-write storage 2d texture
    RwTexture2d,

    /// A read-write storage 2d texture array
    RwTexture2dArray,

    /// A read-only sampled cube texture
    TextureCube,

    /// A read-only sampled cube texture array
    TextureCubeArray,

    /// A read-only sampled 3d texture
    Texture3d,

    /// A read-write storage 3d texture
    RwTexture3d,

    /// An acceleration structure for raytracing
    RaytracingAccelerationStructure,

    /// A sampler state object
    SamplerState,

    /// A sampler comparison state object
    SamplerComparisonState,
}

/// A definition of a constant buffer that serves other bindings
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct InlineConstantBuffer {
    /// Api binding slot
    pub api_location: u32,

    /// Size of the constant buffer
    pub size_in_bytes: u32,
}
