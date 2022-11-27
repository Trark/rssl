use rssl_text::*;

/// Turn an rssl string into ir
#[track_caller]
fn parse_from_str(source: &str) -> (rssl_ir::Module, SourceManager) {
    // Create source manager to store the source into
    let mut source_manager = SourceManager::new();

    // Add a newline to the end of every test string as the lexer requires a clean ending
    let modified_string = source.to_string() + "\n";

    // Preprocess the text
    let preprocessed_text = rssl_preprocess::preprocess_fragment(
        &modified_string,
        FileName("type_test.rssl".to_string()),
        &mut source_manager,
    )
    .expect("preprocess failed");

    // Run the lexer on the input
    let tokens = match rssl_lexer::lex(&preprocessed_text) {
        Ok(tokens) => tokens.stream,
        Err(err) => panic!("{}{:?}", err.display(&source_manager), err),
    };

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
pub fn check_rssl_to_hlsl(source_rssl: &str, expected_hlsl: &str) {
    let (ir, _) = parse_from_str(source_rssl);

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
        }
        Err(err) => {
            // TODO: Error printing
            panic!("{:?}", err)
        }
    }
}
