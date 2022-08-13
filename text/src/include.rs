/// Error cases for file loading
#[derive(PartialEq, Debug, Clone)]
pub enum IncludeError {
    FileNotFound,
    FileNotText,
}

/// Trait for loading files from #include directives
pub trait IncludeHandler {
    fn load(&mut self, file_name: &str) -> Result<String, IncludeError>;
}

/// A file loader that fails to load any files
pub struct NullIncludeHandler;

impl IncludeHandler for NullIncludeHandler {
    fn load(&mut self, _: &str) -> Result<String, IncludeError> {
        Err(IncludeError::FileNotFound)
    }
}
