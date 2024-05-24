//! # RSSL - MSL Exporter
//!
//! This library contains the logic to convert typed RSSL into MSL source

mod generator;
mod names;
mod rewrite_mesh_output;
mod simplify_resource_subscript;

/// Export module to Metal Shading Language
///
/// We assume the generated code will be built with:
/// * -std=metal3.1
pub fn export_to_msl(module: &rssl_ir::Module) -> Result<ExportedSource, ExportError> {
    // TODO: Consume the module as we have to modify it anyway
    let mut module = module.clone();

    // Transform all special syntax cbuffers into struct / ConstantBuffer global variable pairs
    rssl_ir::simplify_cbuffers(&mut module);

    // Attempt to remove access to resources via references
    simplify_resource_subscript::simplify_resource_subscript(&mut module);

    // Attempt to rewrite mesh shader outputs
    let mut mesh_layout = None;
    if let Some(selected_pipeline) = module.selected_pipeline {
        let pipeline = &module.pipelines[selected_pipeline];
        let mut mesh_index = None;
        for (stage_index, stage) in &mut pipeline.stages.iter().enumerate() {
            if let rssl_ir::ShaderStage::Mesh = stage.stage {
                mesh_index = Some(stage_index);
            }
        }
        if let Some(mesh_index) = mesh_index {
            match rewrite_mesh_output::process_mesh_entry(
                &mut module,
                selected_pipeline,
                mesh_index,
            ) {
                Ok(layout) => mesh_layout = Some(layout),
                Err(err) => return Err(ExportError::GenerateError(err)),
            }
        }
    };

    // Generate MSL in the form of RSSL ast from RSSL ir module
    let generate_output = match generator::generate_module(&mut module, mesh_layout) {
        Ok(output) => output,
        Err(err) => return Err(ExportError::GenerateError(err)),
    };

    // Output MSL source by formatting the RSSL ast
    let target = rssl_formatter::Target::Msl;
    let source = match rssl_formatter::format(&generate_output.ast_module, target) {
        Ok(output) => output,
        Err(err) => return Err(ExportError::FormatError(err)),
    };

    Ok(ExportedSource {
        source,
        pipeline_description: generate_output.pipeline_description,
    })
}

pub struct ExportedSource {
    pub source: String,
    pub pipeline_description: rssl_ir::export::PipelineDescription,
}

/// Error result when exporting to MSL fails
#[derive(PartialEq, Debug)]
pub enum ExportError {
    GenerateError(GenerateError),
    FormatError(FormatError),
}

pub use generator::GenerateError;
pub use rssl_formatter::FormatError;

impl rssl_text::CompileError for ExportError {
    fn print(&self, w: &mut rssl_text::MessagePrinter) -> std::fmt::Result {
        use rssl_text::*;
        match self {
            ExportError::GenerateError(err) => err.print(w),
            ExportError::FormatError(err) => w.write_message(
                &|f| write!(f, "metal format: {:?}", err),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
        }
    }
}
