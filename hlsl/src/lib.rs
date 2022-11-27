//! # RSSL - HLSL Exporter
//!
//! This library contains the logic to convert typed RSSL into HLSL source

mod exporter;

pub use exporter::export_to_hlsl;

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

    /// Slot index for the binding in the group
    pub binding: u32,

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
    StructuredBuffer,
    RwStructuredBuffer,
    TexelBuffer,
    RwTexelBuffer,
    Texture2d,
    RwTexture2d,
}
