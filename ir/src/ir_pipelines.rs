use crate::*;
use rssl_text::Located;

/// A definition for a shader pipeline
#[derive(PartialEq, Debug, Clone)]
pub struct PipelineDefinition {
    /// Name for the pipeline
    pub name: Located<String>,

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
}

/// Identifier for the kind of shader in a stage of the pipeline
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ShaderStage {
    /// Vertex shader
    Vertex,

    /// Pixel shader
    Pixel,

    /// Compute shader
    Compute,

    /// Task shader
    Task,

    /// Mesh shader
    Mesh,
}

/// Pipeline state for a graphics pipeline
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub struct GraphicsPipelineState {
    /// Formats for color targets
    pub render_target_formats: Vec<Option<String>>,

    /// Format for depth/stencil target
    pub depth_target_format: Option<String>,
}
