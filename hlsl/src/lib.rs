//! # RSSL - HLSL Exporter
//!
//! This library contains the logic to convert typed RSSL into HLSL source

mod ast_generate;
mod names;

/// Export ir module to HLSL
///
/// We assume the generated code will be built with:
/// * HLSL version 2021
/// * If using half: -enable-16bit-types
pub fn export_to_hlsl(
    module: &rssl_ir::Module,
    for_spirv: bool,
) -> Result<ExportedSource, ExportError> {
    // Generate HLSL in the form of RSSL ast from RSSL ir module
    let generate_output = match ast_generate::generate_module(module, for_spirv) {
        Ok(output) => output,
        Err(err) => return Err(ExportError::GenerateError(err)),
    };

    // Output HLSL source by formatting the RSSL ast
    let target = rssl_formatter::Target::Hlsl;
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

/// Error result when exporting to HLSL fails
#[derive(Debug)]
pub enum ExportError {
    GenerateError(ast_generate::GenerateError),
    FormatError(rssl_formatter::FormatError),
}

impl rssl_text::CompileError for ExportError {
    fn print(&self, w: &mut rssl_text::MessagePrinter) -> std::fmt::Result {
        use rssl_text::*;
        match self {
            ExportError::GenerateError(err) => w.write_message(
                &|f| write!(f, "hlsl generate: {:?}", err),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            ExportError::FormatError(err) => w.write_message(
                &|f| write!(f, "hlsl format: {:?}", err),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
        }
    }
}
