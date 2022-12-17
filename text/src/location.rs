/// Source file location
/// Requires `SourceManager` to decode
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Copy, Clone, Hash)]
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

    /// Add a snippet of text into the source manager
    pub fn add_fragment(&mut self, fragment: &str) -> (FileId, SourceLocation) {
        let file_id = self.add_file(FileName(String::new()), fragment.to_string());
        let base_location = self.files[file_id.0 as usize].base_location;
        (file_id, base_location)
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
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FileName(pub String);

/// A line number in a file
#[derive(PartialEq, Eq, PartialOrd, Debug, Copy, Clone)]
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
#[derive(PartialEq, Eq, PartialOrd, Debug, Copy, Clone)]
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
#[derive(PartialEq, Eq, Debug, Clone)]
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
#[derive(PartialEq, Eq, PartialOrd, Debug, Copy, Clone)]
pub struct StreamLocation(pub u32);

/// Types implementing `Locate` are able to fetch the source location that the node was parsed from
pub trait Locate {
    /// Retrieve the source location that this node is represented by
    fn get_location(&self) -> SourceLocation;
}

/// Types implementing `LocateEnd` are able to fetch the source location after the node
pub trait LocateEnd {
    /// Retrieve the end of the source location that this node is represented by
    fn get_end_location(&self) -> SourceLocation;
}

/// Wrapper to pair a node with a source location
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
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

impl<T> Locate for Located<T> {
    fn get_location(&self) -> SourceLocation {
        self.location
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
