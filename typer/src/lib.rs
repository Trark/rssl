//! # RSSL - Type Checker
//!
//! The typer library can type check an RSSL abstract syntax tree to convert it into the intermediate representation

mod casting;
mod evaluator;
mod typer;

pub use typer::parse;
pub use typer::TyperError;
