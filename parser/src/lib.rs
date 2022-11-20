//! # RSSL - Parser
//!
//! The parser converts from a stream of lex tokens into an abstract syntax tree

mod parser;

pub use parser::parse;
pub use parser::ParseError;
