use crate::*;
use rssl_text::Located;

/// A definition for a shader pipeline
#[derive(PartialEq, Debug, Clone)]
pub struct PipelineDefinition {
    /// Name for the pipeline
    pub name: Located<String>,

    /// Binding group index to use for loose parameters
    pub default_bind_group_index: u32,

    /// Set of shader stages in the pipeline
    pub stages: Vec<PipelineStage>,

    /// State for graphics pipelines
    pub graphics_pipeline_state: Option<GraphicsPipelineState>,
}

/// A definition for a shader pipeline
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct PipelineStage {
    /// Shader type
    pub stage: ShaderStage,

    /// Entry point function
    pub entry_point: FunctionId,

    /// The number of threads per thread group if the stage has a set size
    pub thread_group_size: Option<(u32, u32, u32)>,
}

/// Identifier for the kind of shader in a stage of the pipeline
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum ShaderStage {
    /// Vertex shader
    Vertex,

    /// Task shader
    Task,

    /// Mesh shader
    Mesh,

    /// Pixel shader
    Pixel,

    /// Compute shader
    Compute,
}

/// Pipeline state for a graphics pipeline
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GraphicsPipelineState {
    /// Formats for color targets
    pub render_target_formats: Vec<Option<String>>,

    /// Format for depth/stencil target
    pub depth_target_format: Option<String>,

    /// Which triangles are culled due which face is forward
    pub cull_mode: CullMode,

    /// Winding order of front facing triangles
    pub winding_order: WindingOrder,

    /// Blend state parameters
    pub blend_state: BlendState,
}

/// Which triangles are culled due which face is forward
#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub enum CullMode {
    None,
    Front,
    #[default]
    Back,
}

/// Rotation of triangle that is considered the front face
#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub enum WindingOrder {
    #[default]
    CounterClockwise,
    Clockwise,
}

/// Blend state parameters
#[derive(Copy, Clone, PartialEq, Default, Debug)]
pub struct BlendState {
    pub attachments: [BlendAttachmentState; 8],
}

/// Blend state for a specific attachment
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct BlendAttachmentState {
    pub blend_enabled: bool,
    pub src_blend: BlendFactor,
    pub dst_blend: BlendFactor,
    pub blend_op: BlendOp,
    pub src_blend_alpha: BlendFactor,
    pub dst_blend_alpha: BlendFactor,
    pub blend_op_alpha: BlendOp,
    pub write_mask: ComponentMask,
}

impl Default for BlendAttachmentState {
    fn default() -> Self {
        BlendAttachmentState {
            blend_enabled: Default::default(),
            src_blend: BlendFactor::One,
            dst_blend: BlendFactor::Zero,
            blend_op: Default::default(),
            src_blend_alpha: BlendFactor::One,
            dst_blend_alpha: BlendFactor::Zero,
            blend_op_alpha: Default::default(),
            write_mask: Default::default(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BlendFactor {
    Zero,
    One,
    SrcColor,
    OneMinusSrcColor,
    DstColor,
    OneMinusDstColor,
    SrcAlpha,
    OneMinusSrcAlpha,
    DstAlpha,
    OneMinusDstAlpha,
    SrcAlphaSaturate,
    ConstantColor,
    OneMinusConstantColor,
    ConstantAlpha,
    OneMinusConstantAlpha,
    Src1Color,
    OneMinusSrc1Color,
    Src1Alpha,
    OneMinusSrc1Alpha,
}

#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub enum BlendOp {
    #[default]
    Add,
    Subtrack,
    RevSubtract,
    Min,
    Max,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ComponentMask(pub u8);

impl Default for ComponentMask {
    fn default() -> Self {
        ComponentMask(0xF)
    }
}
