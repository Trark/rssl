//! # RSSL - MSL Exporter
//!
//! This library contains the logic to convert typed RSSL into MSL source

mod generator;
mod names;

/// Export module to Metal Shading Language
///
/// We assume the generated code will be built with:
/// * -std=metal3.1
pub fn export_to_msl(module: &rssl_ir::Module) -> Result<ExportedSource, ExportError> {
    // Generate MSL in the form of RSSL ast from RSSL ir module
    let generate_output = match generator::generate_module(module) {
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
