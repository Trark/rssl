/// Error cases for file loading
#[derive(PartialEq, Debug, Clone)]
pub enum IncludeError {
    FileNotFound,
    FileNotText,
}

/// Return data after loading from a file
pub struct FileData {
    pub real_name: String,
    pub contents: String,
}

/// Trait for loading files from #include directives
pub trait IncludeHandler {
    fn load(&mut self, file_name: &str) -> Result<FileData, IncludeError>;
}

/// A file loader that fails to load any files
pub struct NullIncludeHandler;

impl IncludeHandler for NullIncludeHandler {
    fn load(&mut self, _: &str) -> Result<FileData, IncludeError> {
        Err(IncludeError::FileNotFound)
    }
}
