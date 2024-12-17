use rssl_msl::ExportError;
use rssl_text::*;

pub use rssl_msl::GenerateError;

/// Turn an rssl string into ir
#[track_caller]
fn parse_from_str(source: &str) -> (rssl_ir::Module, SourceManager) {
    // Create source manager to store the source into
    let mut source_manager = SourceManager::new();

    // Preprocess the text
    let tokens = match rssl_preprocess::preprocess_fragment(
        source,
        FileName("exporter_test.rssl".to_string()),
        &mut source_manager,
    ) {
        Ok(tokens) => tokens,
        Err(err) => panic!("{}", err.display(&source_manager)),
    };

    let tokens = rssl_preprocess::prepare_tokens(&tokens);

    // Run the parser & type checker
    let ir = match rssl_typer::parse(tokens) {
        Ok(ir) => ir,
        Err(err) => panic!("{}", err.display(&source_manager)),
    };

    // Select the pipeline if there is one
    let ir = match ir.pipelines.len() {
        0 => ir,
        1 => {
            let name = ir.pipelines[0].name.node.clone();
            ir.select_pipeline(&name).unwrap()
        }
        _ => panic!("Multiple pipelines in test file"),
    };

    // Assign api slots to bindings
    let mut ir = ir.assign_api_bindings(&rssl_ir::AssignBindingsParams {
        require_slot_type: false,
        support_buffer_address: false,
        metal_slot_layout: true,
    });

    // Remove explicit cbuffer as the exporter only supports object types
    rssl_ir::simplify_cbuffers(&mut ir);

    (ir, source_manager)
}

#[track_caller]
pub fn check(source_rssl: &str, expected_msl: &str) {
    validate_metal(expected_msl);

    let (ir, source_manager) = parse_from_str(source_rssl);

    match rssl_msl::export_to_msl(&ir) {
        Ok(output) => {
            let output_msl = output.source;
            let output_msl_lines = output_msl.lines();
            let expected_msl_lines = expected_msl.lines();
            for (output_msl_line, expected_msl_line) in output_msl_lines.zip(expected_msl_lines) {
                assert_eq!(output_msl_line, expected_msl_line);
            }
            assert_eq!(output_msl, expected_msl);
        }
        Err(err) => panic!("{}", err.display(&source_manager)),
    }
}

/// Ensure that
#[track_caller]
pub fn expect_generate_fail(source_rssl: &str, expected_err: GenerateError) {
    let (ir, _) = parse_from_str(source_rssl);

    match rssl_msl::export_to_msl(&ir) {
        Ok(_) => panic!("Expected generation to fail"),
        Err(err) => assert_eq!(err, ExportError::GenerateError(expected_err)),
    }
}

/// Check that the input text is valid metal shading language source
#[track_caller]
fn validate_metal(metal_source: &str) {
    use metal_invoker::*;

    if !std::env::vars().any(|v| v.0 == "VALIDATE_METAL" && v.1 != "0") {
        return;
    }

    let compiler = match MetalCompiler::find() {
        Ok(compiler) => compiler,
        Err(err) => panic!("{}", err),
    };

    let execute_result =
        compiler.execute(metal_source, &["-std=metal3.1", "-include", "metal_stdlib"]);

    // Ensure the build was a success and display the errors if not
    if let Err(err) = execute_result {
        panic!("{}", err);
    }
}
