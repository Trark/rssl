//! # RSSL - Preprocessor
//!
//! The preprocessor contains a C-like preprocessor to run on a set of raw RSSL source files.

mod condition_parser;
mod preprocess;

pub use preprocess::preprocess;
pub use preprocess::preprocess_direct;
pub use preprocess::preprocess_fragment;
pub use preprocess::PreprocessError;

#[cfg(test)]
mod tests;
