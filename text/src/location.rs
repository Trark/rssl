/// A file used as an input
#[derive(PartialEq, Debug, Clone)]
pub struct FileName(pub String);

/// A line number in a file
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct Line(pub u64);

impl Line {
    pub fn first() -> Self {
        Line(1)
    }

    pub fn increment(&mut self) {
        self.0 += 1
    }
}

/// The column index in a line
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub struct Column(pub u64);

impl Column {
    pub fn first() -> Self {
        Column(1)
    }

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
    debug_locations: LineMap,
}

impl PreprocessedText {
    pub fn new(text: String, locations: LineMap) -> Self {
        let mut pt = PreprocessedText {
            code: text,
            debug_locations: locations,
        };
        pt.simplify_locations();
        pt
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
        self.debug_locations.get_file_location(stream_location)
    }

    // Remove location mappings that are consistent with the previous mapping
    pub fn simplify_locations(&mut self) {
        // We will process the text as bytes
        let bytes = self.code.as_bytes();

        // Extract current map which we will consume
        let old_line_map = std::mem::take(&mut self.debug_locations.lines);

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
                            let text_range = &bytes[start..end];

                            let mut line = last_line;
                            let mut column = last_column;
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

        // Store the new map back into ourself
        std::mem::swap(&mut self.debug_locations.lines, &mut new_line_map);
    }

    /// Generate string from text with #line markers
    pub fn export_with_line_markers(&self) -> String {
        let mut output = Vec::with_capacity(self.code.len());

        let bytes = self.code.as_bytes();
        let mut processed = 0;
        for (stream_location, file_location) in &self.debug_locations.lines {
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
#[derive(PartialEq, Debug, Default, Clone)]
pub struct LineMap {
    lines: Vec<(StreamLocation, FileLocation)>,
}

/// Error type when we can not obtain source file info for a position
pub struct NoFileLocation;

impl LineMap {
    /// Add a mapping from stream location back to source file location
    pub fn push(&mut self, stream_location: StreamLocation, file_location: FileLocation) {
        // Ensure stream locations are in order and there are no duplicates
        if let Some((previous_stream_location, _)) = self.lines.last() {
            assert!(*previous_stream_location < stream_location);
        };
        self.lines.push((stream_location, file_location))
    }

    /// Find the file location of a given stream position
    pub fn get_file_location(
        &self,
        stream_location: StreamLocation,
    ) -> Result<FileLocation, NoFileLocation> {
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
            None => Err(NoFileLocation),
        }
    }
}
