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

    let compiled = match rssl::compile(
        rssl::CompileArgs::new(
            entry_file,
            &mut include_handler,
            rssl::Target::HlslForDirectX,
        )
        .defines(&[
            ("FFX_GPU", "1"),
            ("FFX_HLSL", "1"),
            ("globallycoherent", ""),
        ])
        .no_pipeline_mode(),
    ) {
        Ok(ok) => ok,
        Err(err) => panic!("{}", err),
    };

    // Run ourself on the output again to ensure it is vaguely valid HLSL
    {
        assert_eq!(compiled.len(), 1);
        let output_text = String::from_utf8(compiled[0].data.clone()).unwrap();

        // Preprocess the text
        let tokens = match rssl_preprocess::preprocess_fragment(
            &output_text,
            FileName("generated".to_string()),
            &mut source_manager,
        ) {
            Ok(tokens) => tokens,
            Err(err) => panic!("{}", err.display(&source_manager)),
        };

        let tokens = rssl_preprocess::prepare_tokens(&tokens);

        // Run the parser & type checker
        if let Err(err) = rssl_typer::parse(tokens) {
            panic!("{}", err.display(&source_manager))
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
