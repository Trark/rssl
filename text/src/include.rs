/// Error cases for file loading
#[derive(PartialEq, Debug, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
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
    fn load(&mut self, file_name: &str, parent_name: &str) -> Result<FileData, IncludeError>;
}

/// A file loader that fails to load any files
pub struct NullIncludeHandler;

impl IncludeHandler for NullIncludeHandler {
    fn load(&mut self, _: &str, _: &str) -> Result<FileData, IncludeError> {
        Err(IncludeError::FileNotFound)
    }
}

impl<'s, const N: usize> IncludeHandler for [(&'s str, &'s str); N] {
    fn load(&mut self, file_name: &str, _: &str) -> Result<FileData, IncludeError> {
        for (name, data) in self {
            if file_name == *name {
                return Ok(FileData {
                    real_name: name.to_string(),
                    contents: data.to_string(),
                });
            }
        }
        Err(IncludeError::FileNotFound)
    }
}
