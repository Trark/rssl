use crate::*;

/// Trait to be implemented by error types in each compilation step
pub trait CompileError {
    fn print(&self, w: &mut MessagePrinter) -> std::fmt::Result;
}

/// Formatter for printing compile errors with source references
pub struct MessagePrinter<'s, 'f> {
    source_manager: &'s SourceManager,
    formatter: &'s mut std::fmt::Formatter<'f>,
}

/// Error severity
pub enum Severity {
    Error,
    Note,
}

impl<'s, 'f> MessagePrinter<'s, 'f> {
    pub fn write_message(
        &mut self,
        write: &dyn Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
        loc: SourceLocation,
        sev: Severity,
    ) -> std::fmt::Result {
        let sev_str = match sev {
            Severity::Error => "error",
            Severity::Note => "note",
        };
        if loc != SourceLocation::UNKNOWN {
            // Get file location info
            let file_location = self.source_manager.get_file_location(loc);

            // Print basic failure reason
            write!(self.formatter, "{}: {}: ", file_location, sev_str)?;
            write(self.formatter)?;
            writeln!(self.formatter)?;

            // Print source that caused the error
            self.source_manager
                .write_source_for_error(self.formatter, Some(loc))
        } else {
            // Print basic failure reason
            write!(self.formatter, "{}: ", sev_str)?;
            write(self.formatter)?;
            writeln!(self.formatter)
        }
    }
}

/// Extension trait for [CompileError]
pub trait CompileErrorExt {
    /// Return a type that can be used with [Display]
    fn display<'p>(&'p self, source_manager: &'p SourceManager) -> CompileErrorPrinter<'p>;
}

impl<T: CompileError + Sized> CompileErrorExt for T {
    fn display<'p>(&'p self, source_manager: &'p SourceManager) -> CompileErrorPrinter<'p> {
        CompileErrorPrinter {
            error: self,
            source_manager,
        }
    }
}

/// Helper type that allows errors to be printed with [Display][std::fmt::Display]
pub struct CompileErrorPrinter<'p> {
    error: &'p dyn CompileError,
    source_manager: &'p SourceManager,
}

impl<'a> std::fmt::Display for CompileErrorPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut message_printer = MessagePrinter {
            source_manager: self.source_manager,
            formatter: f,
        };
        self.error.print(&mut message_printer)
    }
}
