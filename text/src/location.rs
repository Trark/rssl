/// A file used as an input
#[derive(PartialEq, Debug, Clone)]
pub struct FileName(pub String);

/// A line number in a file
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct Line(pub u64);

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
pub struct Column(pub u64);

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
    Known(
        // TODO: Avoid using a string here so this can be used where it is replicated many times
        FileName,
        Line,
        Column,
    ),
    Unknown,
}

impl FileLocation {
    /// Increase location by advancing through text
    pub fn advance(self, text_range: &[u8]) -> Self {
        match self {
            FileLocation::Known(file, line, column) => {
                let (line, column) = Self::advance_parts(line, column, text_range);
                FileLocation::Known(file, line, column)
            }
            FileLocation::Unknown => FileLocation::Unknown,
        }
    }

    /// Increase line/column parts by advancing through text
    pub fn advance_parts(mut line: Line, mut column: Column, text_range: &[u8]) -> (Line, Column) {
        for c in text_range {
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
        (line, column)
    }

    /// Increase location by advancing a number of columns
    pub fn advance_columns(self, num_columns: u64) -> Self {
        match self {
            FileLocation::Known(file, line, column) => {
                FileLocation::Known(file, line, Column(column.0 + num_columns))
            }
            FileLocation::Unknown => FileLocation::Unknown,
        }
    }
}

/// The raw number of bytes from the start of a stream
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
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
    debug_locations: StreamToFileMap,
}

impl PreprocessedText {
    pub fn new(text: String, locations: LineMap) -> Self {
        let debug_locations = StreamToFileMap::from_line_map(text.as_bytes(), locations);
        PreprocessedText {
            code: text,
            debug_locations,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.code
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.code.as_bytes()
    }

    pub fn get_file_location(
        &self,
        stream_location: StreamLocation,
    ) -> Result<FileLocation, NoFileLocation> {
        self.debug_locations
            .get_file_location(stream_location, self.as_bytes())
    }

    /// Generate string from text with #line markers
    pub fn export_with_line_markers(&self) -> String {
        let mut output = Vec::with_capacity(self.code.len());

        let bytes = self.code.as_bytes();
        let mut processed = 0;
        for (stream_location, file_location) in &self.debug_locations.remap {
            let next = stream_location.0 as usize;
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
            let (file_name, line) = match file_location {
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
/// Requires external source to decode line transitions
#[derive(PartialEq, Debug, Default, Clone)]
struct StreamToFileMap {
    remap: Vec<(StreamLocation, FileLocation)>,
}

/// Links streams offsets to source file locations
/// Expects unique entries for each line
#[derive(PartialEq, Debug, Default, Clone)]
pub struct LineMap {
    lines: StreamToFileMap,
}

/// Error type when we can not obtain source file info for a position
pub struct NoFileLocation;

impl StreamToFileMap {
    /// Construct stream to file map by simplifying a line map
    fn from_line_map(source: &[u8], line_map: LineMap) -> Self {
        // Extract current map which we will consume
        let old_line_map = line_map.lines.remap;

        // Build new map - assuming a size similar to the old one
        let mut new_line_map = Vec::with_capacity(old_line_map.len());

        // Remember previous stream/file locations as we process each entry
        let mut last_stream_location: Option<StreamLocation> = None;
        let mut last_file_location = FileLocation::Unknown;

        for (stream_location, file_location) in old_line_map {
            let mut keep = true;

            if let Some(last_stream_location) = last_stream_location {
                if let FileLocation::Known(ref last_file, last_line, last_column) =
                    last_file_location
                {
                    if let FileLocation::Known(ref next_file, next_line, next_column) =
                        &file_location
                    {
                        if last_file == next_file {
                            let start = last_stream_location.0 as usize;
                            let end = stream_location.0 as usize;

                            let (line, column) = FileLocation::advance_parts(
                                last_line,
                                last_column,
                                &source[start..end],
                            );

                            if line == *next_line && column == *next_column {
                                keep = false;
                            }
                        }
                    }
                }
            }

            // If the mapping still contains information then add it back to the new map
            if keep {
                last_stream_location = Some(stream_location);
                last_file_location = file_location.clone();
                new_line_map.push((stream_location, file_location));
            }
        }

        StreamToFileMap {
            remap: new_line_map,
        }
    }

    /// Find the file location of a given stream position
    fn get_file_location_parts(
        &self,
        stream_location: StreamLocation,
    ) -> Result<(FileLocation, u64), NoFileLocation> {
        let mut lower = 0;
        let mut upper = self.remap.len();
        while lower < upper - 1 {
            let next_index = (lower + upper) / 2;
            assert!(next_index > lower);
            assert!(next_index <= upper);

            let &(ref line_stream, _) = &self.remap[next_index];
            let matches = line_stream.0 <= stream_location.0;

            if matches {
                lower = next_index;
            } else {
                upper = next_index;
            }
        }
        let last_line = if lower == self.remap.len() {
            None
        } else {
            Some(lower)
        };
        match last_line {
            Some(index) => {
                let (line_stream, line_file) = self.remap[index].clone();
                Ok((line_file, stream_location.0 - line_stream.0))
            }
            None => Err(NoFileLocation),
        }
    }

    /// Find the file location of a given stream position
    fn get_file_location(
        &self,
        stream_location: StreamLocation,
        stream_source: &[u8],
    ) -> Result<FileLocation, NoFileLocation> {
        let (base_loc, offset) = self.get_file_location_parts(stream_location)?;
        let text_range =
            &stream_source[((stream_location.0 - offset) as usize)..(stream_location.0 as usize)];
        Ok(base_loc.advance(text_range))
    }
}

impl LineMap {
    /// Add a mapping from stream location back to source file location
    pub fn push(&mut self, stream_location: StreamLocation, file_location: FileLocation) {
        // Ensure stream locations are in order and there are no duplicates
        if let Some((previous_stream_location, _)) = self.lines.remap.last() {
            assert!(*previous_stream_location < stream_location);
        };
        self.lines.remap.push((stream_location, file_location))
    }

    /// Find the file location of a given stream position
    pub fn get_file_location(
        &self,
        stream_location: StreamLocation,
    ) -> Result<FileLocation, NoFileLocation> {
        let (base_loc, offset) = self.lines.get_file_location_parts(stream_location)?;
        Ok(base_loc.advance_columns(offset))
    }
}
