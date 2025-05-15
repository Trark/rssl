use rssl_text::*;

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

    (ir, source_manager)
}

#[track_caller]
pub fn check_rssl_to_hlsl_params(
    source_rssl: &str,
    expected_hlsl: &str,
    assign_bindings_params: Option<rssl_ir::AssignBindingsParams>,
) {
    let (ir, source_manager) = parse_from_str(source_rssl);

    let trivial_bindings = assign_bindings_params.is_none();

    let ir = match ir.pipelines.len() {
        0 => ir,
        1 => {
            let name = ir.pipelines[0].name.node.clone();
            ir.select_pipeline(&name).unwrap()
        }
        _ => panic!("Multiple pipelines in test file"),
    };

    let ir = match assign_bindings_params {
        Some(params) => ir.assign_api_bindings(&params),
        None => ir,
    };

    match rssl_hlsl::export_to_hlsl(&ir) {
        Ok(output) => {
            let output_hlsl = output.source;
            let output_hlsl_lines = output_hlsl.lines();
            let expected_hlsl_lines = expected_hlsl.lines();
            for (output_hlsl_line, expected_hlsl_line) in output_hlsl_lines.zip(expected_hlsl_lines)
            {
                assert_eq!(output_hlsl_line, expected_hlsl_line);
            }
            assert_eq!(output_hlsl, expected_hlsl);

            if trivial_bindings {
                parse_from_str(&output_hlsl);
            }
        }
        Err(err) => panic!("{}", err.display(&source_manager)),
    }
}

#[track_caller]
pub fn check_rssl_to_hlsl(source_rssl: &str, expected_hlsl: &str) {
    check_rssl_to_hlsl_params(source_rssl, expected_hlsl, None)
}

#[track_caller]
pub fn check_rssl_to_hlsl_dx(source_rssl: &str, expected_hlsl: &str) {
    check_rssl_to_hlsl_params(
        source_rssl,
        expected_hlsl,
        Some(rssl_ir::AssignBindingsParams::default()),
    )
}

#[track_caller]
pub fn check_rssl_to_hlsl_vk(source_rssl: &str, expected_hlsl: &str) {
    check_rssl_to_hlsl_params(
        source_rssl,
        expected_hlsl,
        Some(rssl_ir::AssignBindingsParams {
            require_slot_type: false,
            support_buffer_address: true,
            metal_slot_layout: false,
            static_samplers_have_slots: true,
        }),
    )
}
