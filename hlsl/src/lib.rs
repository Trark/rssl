//! # RSSL - HLSL Exporter
//!
//! This library contains the logic to convert typed RSSL into HLSL source

mod exporter;

pub use exporter::{export_to_hlsl, ExportError, ExportedSource};

pub use rssl_ir::ApiLocation;

/// Description of a shader pipeline
#[derive(Clone, PartialEq, Eq, Debug)]
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

    /// A read-only sampled 3d texture
    Texture3d,

    /// A read-write storage 3d texture
    RwTexture3d,
}

/// A definition of a constant buffer that serves other bindings
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct InlineConstantBuffer {
    /// Api binding slot
    pub api_location: u32,

    /// Size of the constant buffer
    pub size_in_bytes: u32,
}
