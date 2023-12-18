//! # Metal shader compiler invoker
//!
//! Finds and invokes an instances of the metal shader compiler

/// Describes the location of a metal shader compiler program
#[derive(Debug)]
pub struct MetalCompiler {
    program_path: String,
    config_args: Vec<String>,
}

/// Error for [MetalCompiler::find]
#[derive(Debug)]
pub enum FindError {
    ProgramNotFound,
    NotSupported,
}

/// Error for [MetalCompiler::execute]
#[derive(Debug)]
pub enum ExecuteError {
    ProcessSpawnFailed,
    SendSourceFailed,
    ProcessJoinFailed,
    CompileError(String),
}

impl MetalCompiler {
    /// Attempt to find the path to the metal compiler
    pub fn find() -> Result<MetalCompiler, FindError> {
        if cfg!(target_os = "macos") {
            // xcrun will find the metal compiler
            Ok(MetalCompiler {
                program_path: String::from("xcrun"),
                config_args: Vec::from([
                    String::from("-sdk"),
                    String::from("macosx"),
                    String::from("metal"),
                ]),
            })
        } else if cfg!(target_os = "windows") {
            // Default file path to the metal tools on Windows
            let program_path =
                "C:\\Program Files\\Metal Developer Tools\\metal\\macos\\bin\\metal.exe";

            if !std::path::Path::new(program_path).exists() {
                return Err(FindError::ProgramNotFound);
            }

            Ok(MetalCompiler {
                program_path: String::from(program_path),
                config_args: Vec::new(),
            })
        } else {
            Err(FindError::NotSupported)
        }
    }

    /// Execute the metal compiler on shader source
    pub fn execute(&self, source: &str, args: &[&str]) -> Result<Vec<u8>, ExecuteError> {
        let mut process_args = Vec::new();

        for config_arg in &self.config_args {
            process_args.push(config_arg.as_str());
        }

        // Write output to stdout, which we will ignore
        process_args.push("-o");
        process_args.push("-");

        // Read input from stdin
        process_args.push("-x");
        process_args.push("metal");
        process_args.push("-c");
        process_args.push("-");

        for arg in args {
            process_args.push(arg);
        }

        // Spawn the compiler process with stdio piped into us
        let process = std::process::Command::new(self.program_path.as_str())
            .args(process_args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn();

        let mut process = match process {
            Ok(process) => process,
            Err(_) => return Err(ExecuteError::ProcessSpawnFailed),
        };

        // Give the metal source to the compiler
        if std::io::Write::write_all(&mut process.stdin.as_mut().unwrap(), source.as_bytes())
            .is_err()
        {
            return Err(ExecuteError::SendSourceFailed);
        };

        // Wait for the compiler to finish
        let result = match process.wait_with_output() {
            Ok(result) => result,
            Err(_) => return Err(ExecuteError::ProcessJoinFailed),
        };

        if !result.status.success() {
            return Err(ExecuteError::CompileError(
                String::from_utf8(result.stderr).unwrap_or(String::from("Invalid UTF-8")),
            ));
        }

        Ok(result.stdout)
    }
}

impl std::fmt::Display for FindError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FindError::ProgramNotFound => write!(f, "Program not found"),
            FindError::NotSupported => write!(f, "Not supported on the current platform"),
        }
    }
}

impl std::fmt::Display for ExecuteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecuteError::ProcessSpawnFailed => write!(f, "Process spawn failed"),
            ExecuteError::SendSourceFailed => write!(f, "Send source failed"),
            ExecuteError::ProcessJoinFailed => write!(f, "Process join failed"),
            ExecuteError::CompileError(error_text) => write!(f, "{}", error_text),
        }
    }
}
