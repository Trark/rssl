//! # RSSL - Preprocessor & Lexer
//!
//! The preprocessor contains a C-like preprocessor to run on a set of raw RSSL source files.
//! The lexer converts from preprocessed source into a stream of lex tokens

mod condition_parser;
mod preprocess;

pub use preprocess::preprocess;
pub use preprocess::preprocess_direct;
pub use preprocess::preprocess_fragment;
pub use preprocess::PreprocessError;

mod lexer;

pub use lexer::lex;
pub use lexer::minilex;
pub use lexer::LexerError;

#[cfg(test)]
mod tests;
