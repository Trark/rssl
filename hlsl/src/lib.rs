//! # RSSL - HLSL Exporter
//!
//! This library contains the logic to convert typed RSSL into HLSL source

mod exporter;

pub use exporter::{export_to_hlsl, ExportError, ExportedSource};

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
}

/// Individual binding of a resource
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DescriptorBinding {
    /// Name of the binding
    pub name: String,

    /// Slot index for the binding in the group for the input descriptor
    pub lang_binding: u32,

    /// Slot index for the binding in the group for the destination api
    pub api_binding: u32,

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

    ByteBuffer,
    RwByteBuffer,

    /// Raw buffer address or ByteBuffer if raw addresses are disabled
    BufferAddress,

    /// Raw buffer address or RwByteBuffer if raw addresses are disabled
    RwBufferAddress,

    StructuredBuffer,
    RwStructuredBuffer,
    TexelBuffer,
    RwTexelBuffer,
    Texture2d,
    RwTexture2d,
}
