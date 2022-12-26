use rssl_text::*;

/// Turn a string into parse tree for a test
#[track_caller]
fn parse_from_str(source: &str) -> (rssl_ast::Module, SourceManager) {
    // Create source manager to store the source into
    let mut source_manager = SourceManager::new();

    // Preprocess the text
    let tokens = match rssl_preprocess::preprocess_fragment(
        source,
        FileName("type_test.rssl".to_string()),
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

    (tree, source_manager)
}

#[track_caller]
pub fn check_types(source: &str) {
    let (tree, source_manager) = parse_from_str(source);

    match rssl_typer::type_check(&tree) {
        Ok(_) => {}
        Err(err) => panic!("{}", err.display(&source_manager)),
    }
}

#[allow(unused)]
#[track_caller]
pub fn check_fail(source: &str) {
    let (tree, _) = parse_from_str(source);

    if rssl_typer::type_check(&tree).is_ok() {
        panic!("Expected type check to fail: {}", source);
    }
}

#[allow(unused)]
#[track_caller]
pub fn check_fail_message(source: &str, expected_message: &str) {
    let (tree, source_manager) = parse_from_str(source);

    match rssl_typer::type_check(&tree) {
        Ok(_) => panic!("Expected type check to fail: {}", source),
        Err(err) => {
            let error_print = err.display(&source_manager).to_string();
            assert_eq!(
                error_print,
                expected_message,
                "\n{2}\n{0}{2}\n{1}",
                error_print,
                expected_message,
                "-".repeat(80)
            );
        }
    }
}
