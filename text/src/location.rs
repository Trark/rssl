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
#[derive(PartialEq, Debug, Clone)]
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
