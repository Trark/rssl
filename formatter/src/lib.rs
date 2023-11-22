//! # RSSL - Formatter
//!
//! Support for writing out RSSL ast back to text

mod formatter;

pub use formatter::format;
pub use formatter::FormatError;
pub use formatter::Target;
