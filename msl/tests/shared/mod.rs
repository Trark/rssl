use rssl_msl::ExportError;
use rssl_text::*;

pub use rssl_msl::{FormatError, GenerateError};

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

    // Run the parser
    let tree = match rssl_parser::parse(&tokens) {
        Ok(tree) => tree,
        Err(err) => panic!("{}", err.display(&source_manager)),
    };

    // Run the type checker
    let ir = match rssl_typer::type_check(&tree) {
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
    let ir = ir.assign_api_bindings(rssl_ir::AssignBindingsParams {
        require_slot_type: false,
        support_buffer_address: false,
    });

    (ir, source_manager)
}

#[track_caller]
pub fn check(source_rssl: &str, expected_msl: &str) {
    validate_metal(expected_msl);

    let (ir, _) = parse_from_str(source_rssl);

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
        Err(err) => {
            // TODO: Error printing
            panic!("{err:?}")
        }
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
    if !std::env::vars().any(|v| v.0 == "VALIDATE_METAL" && v.1 != "0") {
        return;
    }

    // Add a block to the start that is not currently output correctly by the exporter
    let prelude = "#include <metal_stdlib>\n";
    let final_source = prelude.to_string() + metal_source;

    // Get the path to the metal compiler
    let (program_path, mut args) = if cfg!(target_os = "macos") {
        // xcrun will find the metal compiler
        ("xcrun", Vec::from(["-sdk", "macosx", "metal"]))
    } else if cfg!(target_os = "windows") {
        // Default file path to the metal tools on Windows
        let metal_exe_path =
            "C:\\Program Files\\Metal Developer Tools\\metal\\macos\\bin\\metal.exe";
        (metal_exe_path, Vec::new())
    } else {
        panic!("Unknown metal compiler path");
    };

    args.push("-std=metal3.1");

    // Write output to stdout, which we will ignore
    args.push("-o");
    args.push("-");

    // Read input from stdin
    args.push("-x");
    args.push("metal");
    args.push("-c");
    args.push("-");

    // Spawn the compiler process with stdio piped into us
    let mut process = std::process::Command::new(program_path)
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    // Give the metal source to the compiler
    std::io::Write::write_all(
        &mut process.stdin.as_mut().unwrap(),
        final_source.as_bytes(),
    )
    .unwrap();

    // Wait for the compiler to finish
    let result = process.wait_with_output().expect("failed");

    // Ensure the build was a success and display the errors if not
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8(result.stderr).unwrap(),
    );
}
