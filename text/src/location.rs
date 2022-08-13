/// A file used as an input
#[derive(PartialEq, Debug, Clone)]
pub struct FileName(pub String);

/// A line number in a file
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Line(pub u64);

/// The column index in a line
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Column(pub u64);

/// Fully qualified location
#[derive(PartialEq, Debug, Clone)]
pub enum FileLocation {
    Known(
        // TODO: Avoid using a string here so this can be used where it is replicated many times
        FileName,
        Line,
        Column,
    ),
    Unknown,
}

/// The raw number of bytes from the start of a stream
#[derive(PartialEq, Debug, Copy, Clone)]
pub struct StreamLocation(pub u64);

/// Wrapper to pair a node with a FileLocation
#[derive(PartialEq, Debug, Clone)]
pub struct Located<T> {
    pub node: T,
    pub location: FileLocation,
}

impl<T> Located<T> {
    /// Create a located object with a location
    pub fn new(node: T, loc: FileLocation) -> Located<T> {
        Located {
            node: node,
            location: loc,
        }
    }

    // Extract the node and discard the location
    pub fn to_node(self) -> T {
        self.node
    }

    /// Create a located object with no location
    pub fn none(node: T) -> Located<T> {
        Located {
            node: node,
            location: FileLocation::Unknown,
        }
    }
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.node
    }
}

/// Text that has source line association
#[derive(PartialEq, Debug, Clone)]
pub struct PreprocessedText {
    code: String,
    debug_locations: LineMap,
}

impl PreprocessedText {
    pub fn new(text: String, locations: LineMap) -> Self {
        PreprocessedText {
            code: text,
            debug_locations: locations,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.code
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.code.as_bytes()
    }

    pub fn get_file_location(&self, stream_location: StreamLocation) -> Result<FileLocation, ()> {
        self.debug_locations.get_file_location(stream_location)
    }
}

/// Links streams offsets to source file locations
#[derive(PartialEq, Debug, Clone)]
pub struct LineMap {
    lines: Vec<(StreamLocation, FileLocation)>,
}

impl Default for LineMap {
    fn default() -> Self {
        LineMap { lines: Vec::new() }
    }
}

impl LineMap {
    /// Add a mapping from stream location back to source file location
    pub fn push(&mut self, stream_location: StreamLocation, file_location: FileLocation) {
        self.lines.push((stream_location, file_location))
    }

    /// Find the file location of a given stream position
    pub fn get_file_location(&self, stream_location: StreamLocation) -> Result<FileLocation, ()> {
        let mut lower = 0;
        let mut upper = self.lines.len();
        while lower < upper - 1 {
            let next_index = (lower + upper) / 2;
            assert!(next_index > lower);
            assert!(next_index <= upper);

            let &(ref line_stream, _) = &self.lines[next_index];
            let matches = line_stream.0 <= stream_location.0;

            if matches {
                lower = next_index;
            } else {
                upper = next_index;
            }
        }
        let last_line = if lower == self.lines.len() {
            None
        } else {
            Some(lower)
        };
        match last_line {
            Some(index) => {
                let (ref line_stream, ref line_file) = self.lines[index];
                Ok(match line_file {
                    FileLocation::Known(file, line, column) => {
                        let column = Column(column.0 + (stream_location.0 - line_stream.0));
                        FileLocation::Known(file.clone(), *line, column)
                    }
                    FileLocation::Unknown => FileLocation::Unknown,
                })
            }
            None => Err(()),
        }
    }
}
