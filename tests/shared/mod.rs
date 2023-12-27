#[track_caller]
#[allow(unused)]
pub fn check_hlsl(source_rssl: &str, expected_hlsl: &str) {
    check_for_target(source_rssl, expected_hlsl, rssl::Target::HlslForDirectX);
}

#[track_caller]
#[allow(unused)]
pub fn check_hlsl_vk(source_rssl: &str, expected_hlsl: &str) {
    check_for_target(source_rssl, expected_hlsl, rssl::Target::HlslForVulkan);
}

#[track_caller]
#[allow(unused)]
pub fn check_msl(source_rssl: &str, expected_msl: &str) {
    validate_metal(expected_msl);
    check_for_target(source_rssl, expected_msl, rssl::Target::Msl);
}

#[track_caller]
pub fn check_for_target(source_rssl: &str, expected: &str, target: rssl::Target) {
    let test_file_name = "test.rssl";
    let mut include_handler = [(test_file_name, source_rssl)];

    let compiled = match rssl::compile(
        rssl::CompileArgs::new(test_file_name, &mut include_handler, target)
            .no_pipeline_mode()
            .support_buffer_address(matches!(target, rssl::Target::HlslForVulkan)),
    ) {
        Ok(ok) => ok,
        Err(err) => panic!("{}", err),
    };

    let output = String::from_utf8(compiled[0].data.clone()).unwrap();

    let output_lines = output.lines();
    let expected_lines = expected.lines();
    for (output_line, expected_line) in output_lines.zip(expected_lines) {
        assert_eq!(output_line, expected_line);
    }
    assert_eq!(output, expected);
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
