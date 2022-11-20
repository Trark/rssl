//! # RSSL - Lexer
//!
//! The lexer converts from preprocessed source into a stream of lex tokens

mod lexer;

pub use lexer::lex;
pub use lexer::LexerError;
