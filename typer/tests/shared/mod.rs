use rssl_text::*;

/// Turn a string into parse tree for a test
#[track_caller]
fn parse_from_str(source: &str) -> (rssl_ast::Module, SourceManager) {
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

#[track_caller]
pub fn check_fail(source: &str) {
    let (tree, _) = parse_from_str(source);

    match rssl_typer::type_check(&tree) {
        Ok(_) => panic!("Expected type check to fail: {}", source),
        Err(_) => {}
    }
}

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
