use rssl_text::*;

struct Minipath<'s>(Vec<&'s str>);

impl<'s> Minipath<'s> {
    fn new(name: &'s str) -> Self {
        Minipath(name.split('/').collect::<Vec<_>>())
    }

    fn push(&mut self, part: &'s str) {
        if part == ".." {
            self.0
                .pop()
                .expect("test code doesn't expect to make bad paths");
        } else {
            self.0.push(part)
        }
    }

    fn remove_end(mut self) -> Self {
        self.0.pop();
        self
    }

    fn join(mut self, other: Minipath<'s>) -> Self {
        for part in other.0 {
            self.push(part);
        }
        self
    }

    fn generate(&self) -> String {
        let mut s = String::new();
        if let Some((end, rest)) = self.0.split_last() {
            for part in rest {
                s.push_str(part);
                s.push('/');
            }
            s.push_str(end);
        }
        s
    }
}

#[track_caller]
fn compile_file(entry_file: &str, files: &'static [(&'static str, &'static str)]) {
    // Create source manager to store the source into
    let mut source_manager = SourceManager::new();

    /// A file loader that loads from static strings
    struct TestIncludeHandler {
        pub files: &'static [(&'static str, &'static str)],
    }

    impl IncludeHandler for TestIncludeHandler {
        fn load(&mut self, file_name: &str, parent_name: &str) -> Result<FileData, IncludeError> {
            let file_path = Minipath::new(file_name);
            let parent_path = Minipath::new(parent_name).remove_end();

            let absolute_name = file_path.generate();
            let relative_path = parent_path.join(file_path);
            let relative_name = relative_path.generate();
            assert_eq!(absolute_name, file_name);

            for &(entry_name, entry_contents) in self.files {
                if entry_name == relative_name {
                    return Ok(FileData {
                        real_name: entry_name.to_string(),
                        contents: entry_contents.to_string(),
                    });
                }
            }

            for &(entry_name, entry_contents) in self.files {
                if entry_name == absolute_name {
                    return Ok(FileData {
                        real_name: entry_name.to_string(),
                        contents: entry_contents.to_string(),
                    });
                }
            }

            Err(IncludeError::FileNotFound)
        }
    }

    let mut include_handler = TestIncludeHandler { files };

    // Preprocess the text
    let tokens = match rssl_preprocess::preprocess(
        entry_file,
        &mut source_manager,
        &mut include_handler,
        &[
            ("__HLSL_VERSION", "2021"),
            ("FFX_GPU", "1"),
            ("FFX_HLSL", "1"),
            ("globallycoherent", ""),
        ],
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

    let ir = ir.assign_api_bindings(rssl_ir::AssignBindingsParams::default());

    match rssl_hlsl::export_to_hlsl(&ir) {
        Ok(output) => {
            // Success!

            // Preprocess the text
            let tokens = match rssl_preprocess::preprocess_fragment(
                &output.source,
                FileName("generated".to_string()),
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
            if let Err(err) = rssl_typer::type_check(&tree) {
                panic!("{}", err.display(&source_manager))
            }
        }
        Err(err) => {
            // TODO: Error printing
            panic!("{err:?}")
        }
    }
}

macro_rules! source {
    ($file:expr) => {
        ($file, include_str!($file))
    };
}

mod capsaicin;

mod ffx_fsr2;
