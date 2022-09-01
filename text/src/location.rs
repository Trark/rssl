/// Source file location
/// Requires `SourceManager` to decode
#[derive(PartialEq, Eq, PartialOrd, Debug, Copy, Clone, Hash)]
pub struct SourceLocation(u32);

impl SourceLocation {
    /// Source location that represents an unknown source
    pub const UNKNOWN: SourceLocation = SourceLocation(u32::MAX);

    /// Create first source location
    pub fn first() -> Self {
        SourceLocation(0)
    }

    /// Add an offset to a source location
    /// Expects the offset to be within range of the stream
    pub fn offset(self, offset: u32) -> Self {
        if self == SourceLocation::UNKNOWN {
            self
        } else {
            SourceLocation(self.0 + offset)
        }
    }

    /// Get the raw integer inside the location
    pub fn get_raw(&self) -> u32 {
        self.0
    }
}

/// Owns all source files loaded into the compiler
pub struct SourceManager {
    files: Vec<SourceFile>,
    next_location: SourceLocation,
}

impl SourceManager {
    /// Create a new source manager with no files
    pub fn new() -> Self {
        SourceManager {
            files: Vec::new(),
            next_location: SourceLocation::first(),
        }
    }

    /// Add a file into the source manager
    pub fn add_file(&mut self, file_name: FileName, contents: String) -> FileId {
        assert!(contents.len() < u32::MAX as usize);
        assert!(self.files.len() < u32::MAX as usize);
        let file_id = FileId(self.files.len() as u32);
        let file_size = contents.len() as u32;
        self.files.push(SourceFile {
            file_name,
            file_size,
            contents,
            base_location: self.next_location,
        });
        // Base source location + file size is used for End-of-file token, so we reserve file size + 1 slots
        self.next_location = self.next_location.offset(file_size + 1);
        file_id
    }

    /// Get the full source for a given file
    pub fn get_contents(&self, file_id: FileId) -> &str {
        &self.files[file_id.0 as usize].contents
    }

    /// Get the source location from a certain position in a file
    pub fn get_source_location_from_file_offset(
        &self,
        file_id: FileId,
        stream_location: StreamLocation,
    ) -> SourceLocation {
        let source_file = &self.files[file_id.0 as usize];
        assert!(stream_location.0 < source_file.file_size + 1);
        source_file.base_location.offset(stream_location.0)
    }

    /// Get the file id and offset from a source location
    pub fn get_file_offset_from_source_location(
        &self,
        source_location: SourceLocation,
    ) -> Option<(FileId, StreamLocation)> {
        let mut current_offset = 0;
        for (i, source_file) in self.files.iter().enumerate() {
            let next_offset = current_offset + source_file.file_size + 1;
            assert_eq!(source_file.base_location.0, current_offset);
            if source_location.0 < next_offset {
                let file_id = FileId(i as u32);
                let source_offset = StreamLocation(source_location.0 - current_offset);
                return Some((file_id, source_offset));
            } else {
                current_offset = next_offset;
            }
        }
        None
    }

    /// Get the full file location information from a source location
    pub fn get_file_location(&self, source_location: SourceLocation) -> FileLocation {
        let mut current_offset = 0;
        for source_file in &self.files {
            let next_offset = current_offset + source_file.file_size + 1;
            assert_eq!(source_file.base_location.0, current_offset);
            if source_location.0 < next_offset {
                let source_offset = source_location.0 - current_offset;
                let mut line = Line::first();
                let mut column = Column::first();
                for c in &source_file.contents.as_bytes()[..(source_offset as usize)] {
                    match c {
                        b'\n' => {
                            line.increment();
                            column = Column::first();
                        }
                        _ => {
                            column.increment();
                        }
                    }
                }
                return FileLocation::Known(source_file.file_name.clone(), line, column);
            } else {
                current_offset = next_offset;
            }
        }
        FileLocation::Unknown
    }

    // Print file source around an error location
    pub fn write_source_for_error(
        &self,
        f: &mut std::fmt::Formatter,
        source_location: Option<SourceLocation>,
    ) -> std::fmt::Result {
        match source_location {
            Some(loc) => {
                let file_offset = self.get_file_offset_from_source_location(loc);
                if let Some((file_id, file_offset)) = file_offset {
                    let contents = self.get_contents(file_id);
                    let fail_index = file_offset.0 as usize;
                    let (before, after) = contents.split_at(fail_index);
                    let line_start = match before.rfind('\n') {
                        Some(i) => i + 1,
                        None => 0,
                    };
                    let line_end = fail_index + after.find('\n').unwrap_or(after.len());

                    // Print source line
                    writeln!(f, "{}", &contents[line_start..line_end])?;

                    if let FileLocation::Known(_, _, column) = self.get_file_location(loc) {
                        for _ in 1..(column.0) {
                            write!(f, " ")?;
                        }
                        writeln!(f, "^")?;
                    }

                    Ok(())
                } else {
                    writeln!(f, "Invalid source")
                }
            }
            None => writeln!(f, "No location available"),
        }
    }
}

impl Default for SourceManager {
    fn default() -> Self {
        Self::new()
    }
}

/// A buffer for a single loaded file
struct SourceFile {
    file_name: FileName,
    file_size: u32,
    contents: String,
    base_location: SourceLocation,
}

/// A source file identifier
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileId(u32);

/// A file used as an input
#[derive(PartialEq, Debug, Clone)]
pub struct FileName(pub String);

/// A line number in a file
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct Line(pub u32);

impl Line {
    /// Construct for the first line
    pub fn first() -> Self {
        Line(1)
    }

    /// Move to the next line
    pub fn increment(&mut self) {
        self.0 += 1
    }
}

/// The column index in a line
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct Column(pub u32);

impl Column {
    /// Construct for the first column
    pub fn first() -> Self {
        Column(1)
    }

    /// Move to the next column
    pub fn increment(&mut self) {
        self.0 += 1
    }
}

/// Fully qualified location
#[derive(PartialEq, Debug, Clone)]
pub enum FileLocation {
    Known(FileName, Line, Column),
    Unknown,
}

impl std::fmt::Display for FileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            FileLocation::Known(file_name, line, column) => {
                write!(f, "{}:{}:{}", file_name.0, line.0, column.0)
            }
            FileLocation::Unknown => write!(f, "<unknown>"),
        }
    }
}

/// The raw number of bytes from the start of a stream
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct StreamLocation(pub u32);

/// Wrapper to pair a node with a source location
#[derive(PartialEq, Eq, Clone, Hash)]
pub struct Located<T> {
    pub node: T,
    pub location: SourceLocation,
}

impl<T> Located<T> {
    /// Create a located object with a location
    pub fn new(node: T, loc: SourceLocation) -> Located<T> {
        Located {
            node,
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
            node,
            location: SourceLocation::UNKNOWN,
        }
    }
}

impl<T> std::ops::Deref for Located<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.node
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} @ {}", self.node, self.location.get_raw())
    }
}

/// Text that has source line association
#[derive(PartialEq, Debug, Clone)]
pub struct PreprocessedText {
    code: String,
    locations: StreamToSourceMap,
}

impl PreprocessedText {
    pub fn new(code: String, locations: StreamToSourceMap) -> Self {
        PreprocessedText { code, locations }
    }

    pub fn as_str(&self) -> &str {
        &self.code
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.code.as_bytes()
    }

    pub fn get_source_location(&self, stream_location: StreamLocation) -> SourceLocation {
        self.locations.get_source_location(stream_location)
    }

    /// Generate string from text with #line markers
    pub fn export_with_line_markers(&self, source_manager: &SourceManager) -> String {
        let mut output = Vec::with_capacity(self.code.len());

        let bytes = self.code.as_bytes();
        let mut processed = 0;
        for entry in &self.locations.remap {
            let next = entry.stream.0 as usize;
            assert!((processed == 0 && next == 0) || processed < next);
            let text_range = &bytes[processed..next];

            // Add the text from the previous locations to the output
            output.extend_from_slice(text_range);

            // Emit #line marker
            // If we are already at the start of a line we do not need to emit another before the #line
            let start_newline = match output.last() {
                Some(b'\n') => "\n",
                _ => "",
            };

            // Get the file / line from the location
            let file_location = source_manager.get_file_location(entry.source);
            let (file_name, line) = match &file_location {
                FileLocation::Known(file_name, line, _) => (file_name.0.as_str(), line.0),
                FileLocation::Unknown => ("unknown", 1),
            };

            // Add the #line marker to the string
            output.extend_from_slice(
                format!("{}#line {} \"{}\"\n", start_newline, line, file_name).as_bytes(),
            );

            processed = next;
        }

        // Add the remaining text to the output
        output.extend_from_slice(&bytes[processed..]);

        // Get as a string
        // This should always success as we should only put file location changes between valid UTF-8 chunks
        match String::from_utf8(output) {
            Ok(s) => s,
            Err(_) => panic!("Line markers split created invalid UTF-8"),
        }
    }
}

/// Links streams offsets to source file locations
#[derive(PartialEq, Debug, Default, Clone)]
pub struct StreamToSourceMap {
    remap: Vec<StreamToSourceEntry>,
}

#[derive(PartialEq, Debug, Clone)]
struct StreamToSourceEntry {
    stream: StreamLocation,
    source: SourceLocation,
}

impl StreamToSourceMap {
    /// Add a mapping from stream location back to source file location
    pub fn push(&mut self, stream_location: StreamLocation, source_location: SourceLocation) {
        // Ensure stream locations are in order and there are no duplicates
        if let Some(previous) = self.remap.last() {
            assert!(previous.stream < stream_location);
        } else {
            assert!(stream_location.0 == 0);
        }
        self.remap.push(StreamToSourceEntry {
            stream: stream_location,
            source: source_location,
        })
    }

    /// Get the source location for a position in a stream
    pub fn get_source_location(&self, stream_location: StreamLocation) -> SourceLocation {
        if self.remap.is_empty() {
            return SourceLocation::UNKNOWN;
        }

        assert_eq!(self.remap[0].stream.0, 0);

        let mut index = self.remap.len() - 1;
        for entry in self.remap.iter().rev() {
            if entry.stream <= stream_location {
                break;
            }
            index -= 1;
        }

        let previous = &self.remap[index];
        let offset = stream_location.0 - previous.stream.0;
        previous.source.offset(offset)
    }
}
