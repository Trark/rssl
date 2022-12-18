//! # RSSL - Source Processing Types
//!
//! The text library contains tools for managing source files.
//! * The [SourceLocation] struct is used to identify any location in a source file.
//! * The [SourceManager] acts as the owner for all files loaded into the compiler and gives meaning to [SourceLocation].
//! * The [tokens] module contains the definitions for tokens that have been generated from the raw text.

mod location;
pub use location::*;

mod errors;
pub use errors::*;

mod include;
pub use include::*;

pub mod tokens;
