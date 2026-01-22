//! # RSSL - Type Checker
//!
//! The typer library can type check an RSSL abstract syntax tree to convert it into the intermediate representation

mod casting;
mod evaluator;

#[allow(clippy::result_large_err)]
mod typer;

pub use typer::TyperError;
pub use typer::type_check;
