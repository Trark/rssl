use rssl_text::*;
use std::collections::{HashMap, HashSet};

/// An error which occurred when attempting to preprocessa a file
#[derive(PartialEq, Debug, Clone)]
pub enum PreprocessError {
    UnknownCommand(String),
    InvalidInclude,
    InvalidDefine,
    MacroAlreadyDefined(String),
    MacroRequiresArguments(String),
    MacroArgumentsNeverEnd,
    MacroExpectsDifferentNumberOfArguments,
    FailedToFindFile(String, IncludeError),
    InvalidIf(String),
    FailedToParseIfCondition(String),
    InvalidIfndef(String),
    InvalidElse,
    InvalidElif(String),
    InvalidEndIf,
    ConditionChainNotFinished,
    ElseNotMatched,
    EndIfNotMatched,
    PragmaOnceInUnknownFile,
}

impl std::fmt::Display for PreprocessError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PreprocessError::UnknownCommand(s) => write!(f, "Unknown preprocessor command: {}", s),
            PreprocessError::InvalidInclude => write!(f, "Invalid #include command"),
            PreprocessError::InvalidDefine => write!(f, "Invalid #define command"),
            PreprocessError::MacroAlreadyDefined(s) => write!(f, "Macro already defined: {}", s),
            PreprocessError::MacroRequiresArguments(s) => {
                write!(f, "Macro function \"{}\" requires arguments", s)
            }
            PreprocessError::MacroArgumentsNeverEnd => write!(f, "expected end of macro arguments"),
            PreprocessError::MacroExpectsDifferentNumberOfArguments => {
                write!(f, "Macro requires different number of arguments")
            }
            PreprocessError::FailedToFindFile(name, _) => {
                write!(f, "Failed to load file: {}", name)
            }
            PreprocessError::InvalidIf(s) => write!(f, "invalid #if: {}", s),
            PreprocessError::FailedToParseIfCondition(_) => {
                write!(f, "#if condition parser failed")
            }
            PreprocessError::InvalidIfndef(s) => write!(f, "Invalid #ifndef: {}", s),
            PreprocessError::InvalidElse => write!(f, "Invalid #else"),
            PreprocessError::InvalidElif(s) => write!(f, "invalid #elif: {}", s),
            PreprocessError::InvalidEndIf => write!(f, "Invalid #endif"),
            PreprocessError::ConditionChainNotFinished => {
                write!(f, "Not enough #endif's encountered")
            }
            PreprocessError::ElseNotMatched => {
                write!(f, "Encountered #else but with no matching #if")
            }
            PreprocessError::EndIfNotMatched => {
                write!(f, "Encountered #endif but with no matching #if")
            }
            PreprocessError::PragmaOnceInUnknownFile => {
                write!(f, "Encountered #pragma once in an unknown file")
            }
        }
    }
}

struct IntermediateText {
    buffer: String,
    locations: StreamToSourceMap,
}

impl IntermediateText {
    fn new() -> IntermediateText {
        IntermediateText {
            buffer: String::new(),
            locations: StreamToSourceMap::default(),
        }
    }

    fn push_str(&mut self, segment: &str, location: SourceLocation) {
        assert!(!segment.is_empty());
        let stream_location_in_buffer = StreamLocation(self.buffer.len() as u32);
        self.locations.push(stream_location_in_buffer, location);
        self.buffer.push_str(segment);
    }
}

/// Manage files that are returned from the external include handler
struct FileLoader<'a> {
    file_name_remap: HashMap<String, FileId>,
    pragma_once_files: HashSet<FileId>,
    source_manager: &'a mut SourceManager,
    include_handler: &'a mut dyn IncludeHandler,
}

/// Loaded file that will be processed
struct InputFile {
    file_id: FileId,
    contents: String,
}

impl<'a> FileLoader<'a> {
    fn new(
        source_manager: &'a mut SourceManager,
        include_handler: &'a mut dyn IncludeHandler,
    ) -> Self {
        FileLoader {
            file_name_remap: HashMap::new(),
            pragma_once_files: HashSet::new(),
            source_manager,
            include_handler,
        }
    }

    fn load(&mut self, file_name: &str) -> Result<InputFile, IncludeError> {
        let id = match self.file_name_remap.get(file_name) {
            Some(id) => *id,
            None => {
                // Load the file
                let file_data = self.include_handler.load(file_name)?;

                // Add it to the source manager
                let id = self
                    .source_manager
                    .add_file(FileName(file_data.real_name), file_data.contents);

                // Remember the file id
                self.file_name_remap.insert(file_name.to_string(), id);

                id
            }
        };

        if self.pragma_once_files.contains(&id) {
            Ok(InputFile {
                file_id: id,
                contents: String::new(),
            })
        } else {
            let contents = self.source_manager.get_contents(id);
            Ok(InputFile {
                file_id: id,
                contents: contents.to_string(),
            })
        }
    }

    fn get_source_location_from_file_offset(
        &self,
        file_id: FileId,
        stream_location: StreamLocation,
    ) -> SourceLocation {
        self.source_manager
            .get_source_location_from_file_offset(file_id, stream_location)
    }

    fn mark_as_pragma_once(&mut self, file_id: FileId) {
        self.pragma_once_files.insert(file_id);
    }
}

fn is_identifier_char(c: char) -> bool {
    matches!(c, 'A'..='Z' | 'a'..='z' | '_' | '0'..='9')
}

#[derive(PartialEq, Debug, Clone)]
struct MacroArg(u64);

#[derive(PartialEq, Debug, Clone)]
enum MacroSegment {
    Text(String),
    Arg(MacroArg),
}

impl MacroSegment {
    fn split(self, arg: &str, index: u64, segments: &mut Vec<MacroSegment>) {
        match self {
            MacroSegment::Text(text) => {
                if let Some(sz) = find_macro(&text, arg) {
                    let before = &text[..sz];
                    let after_offset = sz + arg.len();
                    let after = &text[after_offset..];
                    assert_eq!(before.to_string() + arg + after, text);
                    if !before.is_empty() {
                        segments.push(MacroSegment::Text(before.to_string()));
                    }
                    segments.push(MacroSegment::Arg(MacroArg(index)));
                    if !after.is_empty() {
                        MacroSegment::Text(after.to_string()).split(arg, index, segments);
                    }
                    return;
                }
                segments.push(MacroSegment::Text(text))
            }
            MacroSegment::Arg(arg) => segments.push(MacroSegment::Arg(arg)),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Macro(String, u64, Vec<MacroSegment>, SourceLocation);

impl Macro {
    fn from_definition(
        head: &str,
        body: &str,
        location: SourceLocation,
    ) -> Result<Macro, PreprocessError> {
        Ok(match head.find('(') {
            Some(sz) => {
                let name = &head[..sz];
                let mut arg_names = vec![];
                let mut remaining = &head[(sz + 1)..];
                loop {
                    let (sz, last) = match remaining.find(',') {
                        Some(sz) => (sz, false),
                        None => match remaining.find(')') {
                            Some(sz) => (sz, true),
                            None => return Err(PreprocessError::InvalidDefine),
                        },
                    };
                    let arg_name = &remaining[..sz];
                    let arg_name = arg_name.trim();
                    remaining = remaining[(sz + 1)..].trim_start();
                    for c in arg_name.chars() {
                        if !is_identifier_char(c) {
                            return Err(PreprocessError::InvalidDefine);
                        }
                    }
                    arg_names.push(arg_name);
                    if last {
                        if !remaining.is_empty() {
                            return Err(PreprocessError::InvalidDefine);
                        }
                        break;
                    }
                }
                let mut last_segments = vec![MacroSegment::Text(body.to_string())];
                for (index, arg_name) in arg_names.iter().enumerate() {
                    let mut next_segments = vec![];
                    for segment in last_segments {
                        segment.split(arg_name, index as u64, &mut next_segments);
                    }
                    last_segments = next_segments;
                }
                Macro(
                    name.to_string(),
                    arg_names.len() as u64,
                    last_segments,
                    location,
                )
            }
            None => Macro(
                head.to_string(),
                0,
                vec![MacroSegment::Text(body.to_string())],
                location,
            ),
        })
    }
}

#[derive(PartialEq, Debug, Clone)]
enum SubstitutedSegment {
    Text(String, SourceLocation),
    Replaced(String, SourceLocation),
}

fn find_macro(text: &str, name: &str) -> Option<usize> {
    let mut subtext = text;
    loop {
        let sz = match subtext.find(name) {
            Some(sz) => sz,
            None => return None,
        };
        let before = &subtext[..sz];
        let after_offset = sz + name.len();
        let after = &subtext[after_offset..];

        let not_separated_before = match before.chars().last() {
            Some(c) => is_identifier_char(c),
            None => false,
        };
        let not_separated_after = match after.chars().next() {
            Some(c) => is_identifier_char(c),
            None => false,
        };

        if !not_separated_before && !not_separated_after {
            let final_sz = sz + text.len() - subtext.len();
            return Some(final_sz);
        }

        let mut subtext_chars = subtext[sz..].chars();
        subtext = match subtext_chars.next() {
            Some(_) => subtext_chars.as_str(),
            None => return None,
        };
    }
}

fn split_macro_args<'stream>(
    macro_name: &str,
    mut remaining: &'stream str,
) -> Result<(&'stream str, Vec<&'stream str>), PreprocessError> {
    // Consume the starting bracket
    let sz = match remaining.find(['(']) {
        Some(sz) => {
            let gap = remaining[..sz].trim();
            if !gap.is_empty() {
                return Err(PreprocessError::MacroRequiresArguments(
                    macro_name.to_string(),
                ));
            }
            sz
        }
        None => {
            return Err(PreprocessError::MacroRequiresArguments(
                macro_name.to_string(),
            ))
        }
    };
    remaining = &remaining[(sz + 1)..];

    let mut args = vec![];
    let mut brace_scope = 0;
    let mut remaining_offset = 0;
    loop {
        let next_pos_result = remaining[remaining_offset..].find([',', '(', ')']);
        let next_opt = next_pos_result.map(|next_pos| {
            (
                remaining.as_bytes()[next_pos + remaining_offset] as char,
                next_pos + remaining_offset,
            )
        });
        match next_opt {
            Some((',', pos)) => {
                if brace_scope == 0 {
                    // Next argument
                    let arg = remaining[..pos].trim();
                    args.push(arg);
                    remaining = &remaining[(pos + 1)..];
                    remaining_offset = 0;
                } else {
                    // Argument lists inside inner parenthesis
                    remaining_offset = pos + 1;
                }
            }
            Some(('(', pos)) => {
                // Start of inner parenthesis
                brace_scope += 1;
                remaining_offset = pos + 1;
            }
            Some((')', pos)) => {
                if brace_scope > 0 {
                    // End of inner parenthesis
                    brace_scope -= 1;
                    remaining_offset = pos + 1;
                } else {
                    // End of macro
                    let arg = remaining[..pos].trim();
                    args.push(arg);
                    remaining = &remaining[(pos + 1)..];
                    break;
                }
            }
            Some(_) => unreachable!(),
            None => {
                return Err(PreprocessError::MacroArgumentsNeverEnd);
            }
        }
    }
    Ok((remaining, args))
}

impl SubstitutedSegment {
    fn apply(
        self,
        macro_defs: &[Macro],
        output: &mut Vec<SubstitutedSegment>,
    ) -> Result<Option<SubstitutedSegment>, PreprocessError> {
        match self {
            SubstitutedSegment::Text(text, location) => {
                // Find the first macro that matches the text
                // We could try to apply defined() in this loop - but this is currently resolved in a prepass before all other macros
                let mut best_match_opt: Option<(&Macro, usize)> = None;
                let mut search_range = text.as_str();
                for macro_def in macro_defs {
                    if let Some(pos) = find_macro(search_range, &macro_def.0) {
                        // As we limit the search range we don't expect
                        assert!(if let Some(best_match) = best_match_opt {
                            pos < best_match.1
                        } else {
                            true
                        });

                        best_match_opt = Some((macro_def, pos));
                        // Limit search for future macros so we only get better matches
                        search_range = &text[..pos];
                    }
                }

                if let Some((macro_def, sz)) = best_match_opt {
                    let before = &text[..sz];
                    let after_offset = sz + macro_def.0.len();
                    let mut remaining = &text[after_offset..];

                    // Read macro arguments
                    let args = if macro_def.1 > 0 {
                        let (rest, args) = split_macro_args(&macro_def.0, remaining)?;
                        remaining = rest;
                        args
                    } else {
                        vec![]
                    };
                    let after = remaining;

                    if args.len() as u64 != macro_def.1 {
                        return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                    }

                    // Substitute macros inside macro arguments
                    let args = args.into_iter().fold(Ok(vec![]), |vec, arg| {
                        let mut vec = vec?;
                        let raw_text = SubstitutedText::new(arg, SourceLocation::UNKNOWN);
                        let subbed_text = raw_text.apply_all(macro_defs, false)?;
                        let final_text = subbed_text.resolve();
                        vec.push(final_text);
                        Ok(vec)
                    })?;

                    let after_location = location.offset((text.len() - after.len()) as u32);
                    if !before.is_empty() {
                        output.push(SubstitutedSegment::Text(before.to_string(), location));
                    }
                    let mut replaced_text = String::new();
                    for macro_segment in &macro_def.2 {
                        match *macro_segment {
                            MacroSegment::Text(ref text) => replaced_text.push_str(text),
                            MacroSegment::Arg(MacroArg(ref index)) => {
                                replaced_text.push_str(&args[*index as usize])
                            }
                        }
                    }
                    if !replaced_text.is_empty() {
                        output.push(SubstitutedSegment::Replaced(replaced_text, macro_def.3));
                    }
                    if !after.is_empty() {
                        Ok(Some(SubstitutedSegment::Text(
                            after.to_string(),
                            after_location,
                        )))
                    } else {
                        Ok(None)
                    }
                } else {
                    assert!(!text.is_empty());
                    output.push(SubstitutedSegment::Text(text, location));
                    Ok(None)
                }
            }
            SubstitutedSegment::Replaced(text, location) => {
                output.push(SubstitutedSegment::Replaced(text, location));
                Ok(None)
            }
        }
    }

    fn apply_defined(
        self,
        macro_defs: &[Macro],
        output: &mut Vec<SubstitutedSegment>,
    ) -> Result<(), PreprocessError> {
        let defined_name = "defined";
        match self {
            SubstitutedSegment::Text(text, location) => {
                if let Some(sz) = find_macro(&text, defined_name) {
                    let before = &text[..sz];
                    let after_offset = sz + defined_name.len();
                    let mut remaining = &text[after_offset..];

                    // Read macro arguments
                    let args = {
                        // Allow "defined A" instead of traditional defined(A)
                        if !remaining.is_empty() && remaining.as_bytes()[0] == b' ' {
                            remaining = &remaining[1..];
                            let next_non_identifier = remaining
                                .find(|c| !is_identifier_char(c))
                                .unwrap_or(remaining.len());
                            let arg = &remaining[..next_non_identifier];
                            remaining = &remaining[next_non_identifier..];
                            Vec::from([arg])
                        } else {
                            // Parse arguments like a normal macro called defined
                            let (rest, args) = split_macro_args(defined_name, remaining)?;
                            remaining = rest;
                            args
                        }
                    };

                    let after = remaining;

                    if args.len() as u64 != 1 {
                        return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                    }

                    let exists = macro_defs.iter().any(|m| m.0 == args[0]);

                    let after_location = location.offset((text.len() - after.len()) as u32);
                    if !before.is_empty() {
                        output.push(SubstitutedSegment::Text(before.to_string(), location));
                    }
                    output.push(SubstitutedSegment::Replaced(
                        if exists { "1" } else { "0" }.to_string(),
                        SourceLocation::UNKNOWN,
                    ));
                    if !after.is_empty() {
                        SubstitutedSegment::Text(after.to_string(), after_location)
                            .apply_defined(macro_defs, output)?;
                    }
                    return Ok(());
                }
                assert!(!text.is_empty());
                output.push(SubstitutedSegment::Text(text, location))
            }
            SubstitutedSegment::Replaced(text, location) => {
                output.push(SubstitutedSegment::Replaced(text, location))
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SubstitutedText(Vec<SubstitutedSegment>);

impl SubstitutedText {
    fn new(text: &str, location: SourceLocation) -> SubstitutedText {
        if text.is_empty() {
            SubstitutedText(Vec::new())
        } else {
            SubstitutedText(vec![SubstitutedSegment::Text(text.to_string(), location)])
        }
    }

    fn apply_all(
        self,
        macro_defs: &[Macro],
        apply_defined: bool,
    ) -> Result<SubstitutedText, PreprocessError> {
        let length = self.0.len();
        let segments_iter = self.0.into_iter();
        let vec = segments_iter.fold(Ok(Vec::with_capacity(length)), |vec_res, segment| {
            let mut vec = vec_res?;
            let mut last_segments = vec![segment];
            if apply_defined {
                let mut next_segments = Vec::with_capacity(last_segments.len());
                for substituted_segment in last_segments {
                    substituted_segment.apply_defined(macro_defs, &mut next_segments)?;
                }
                last_segments = next_segments;
            }
            {
                let mut next_segments = Vec::with_capacity(last_segments.len());
                for substituted_segment in last_segments {
                    let mut next_segment = Some(substituted_segment);
                    while let Some(next) = next_segment {
                        next_segment = next.apply(macro_defs, &mut next_segments)?;
                    }
                }
                last_segments = next_segments;
            }
            vec.append(&mut last_segments);
            Ok(vec)
        });
        Ok(SubstitutedText(vec?))
    }

    fn store(self, intermediate_text: &mut IntermediateText) {
        for substituted_segment in self.0 {
            match substituted_segment {
                SubstitutedSegment::Text(text, mut loc) => {
                    assert!(!text.is_empty());
                    let mut remaining = &text[..];
                    loop {
                        let (sz, last) = match remaining.find('\n') {
                            Some(sz) => (sz + 1, false),
                            None => (remaining.len(), true),
                        };
                        let before = &remaining[..sz];
                        intermediate_text.push_str(before, loc);
                        remaining = &remaining[sz..];
                        loc = loc.offset(sz as u32);
                        if last || remaining.is_empty() {
                            break;
                        }
                    }
                }
                SubstitutedSegment::Replaced(text, location) => {
                    assert!(!text.is_empty());
                    intermediate_text.push_str(&text, location)
                }
            }
        }
    }

    fn resolve(self) -> String {
        let mut output = String::new();
        for substituted_segment in self.0 {
            match substituted_segment {
                SubstitutedSegment::Text(text, _) | SubstitutedSegment::Replaced(text, _) => {
                    output.push_str(&text)
                }
            }
        }
        output
    }
}

#[test]
fn macro_from_definition() {
    assert_eq!(
        Macro::from_definition("B", "0", SourceLocation::UNKNOWN).unwrap(),
        Macro(
            "B".to_string(),
            0,
            vec![MacroSegment::Text("0".to_string())],
            SourceLocation::UNKNOWN
        )
    );
    assert_eq!(
        Macro::from_definition("B(x)", "x", SourceLocation::UNKNOWN).unwrap(),
        Macro(
            "B".to_string(),
            1,
            vec![MacroSegment::Arg(MacroArg(0))],
            SourceLocation::UNKNOWN
        )
    );
    assert_eq!(
        Macro::from_definition("B(x,y)", "x", SourceLocation::UNKNOWN).unwrap(),
        Macro(
            "B".to_string(),
            2,
            vec![MacroSegment::Arg(MacroArg(0))],
            SourceLocation::UNKNOWN
        )
    );
    assert_eq!(
        Macro::from_definition("B(x,y)", "y", SourceLocation::UNKNOWN).unwrap(),
        Macro(
            "B".to_string(),
            2,
            vec![MacroSegment::Arg(MacroArg(1))],
            SourceLocation::UNKNOWN
        )
    );
    assert_eq!(
        Macro::from_definition("B(x,xy)", "(x || xy)", SourceLocation::UNKNOWN).unwrap(),
        Macro(
            "B".to_string(),
            2,
            vec![
                MacroSegment::Text("(".to_string()),
                MacroSegment::Arg(MacroArg(0)),
                MacroSegment::Text(" || ".to_string()),
                MacroSegment::Arg(MacroArg(1)),
                MacroSegment::Text(")".to_string()),
            ],
            SourceLocation::UNKNOWN
        )
    );
}

#[test]
fn macro_resolve() {
    fn run(input: &str, macros: &[Macro], expected_output: &str) {
        let text = SubstitutedText::new(input, SourceLocation::UNKNOWN);
        let resolved_text = text.apply_all(macros, false).unwrap().resolve();
        assert_eq!(resolved_text, expected_output);
    }

    run(
        "(A || B) && BC",
        &[
            Macro::from_definition("B", "0", SourceLocation::UNKNOWN).unwrap(),
            Macro::from_definition("BC", "1", SourceLocation::UNKNOWN).unwrap(),
        ],
        "(A || 0) && 1",
    );

    run(
        "(A || B(0, 1)) && BC",
        &[
            Macro::from_definition("B(x, y)", "(x && y)", SourceLocation::UNKNOWN).unwrap(),
            Macro::from_definition("BC", "1", SourceLocation::UNKNOWN).unwrap(),
        ],
        "(A || (0 && 1)) && 1",
    );
}

/// Stores the active #if blocks
struct ConditionChain(Vec<ConditionState>);

#[derive(PartialEq, Eq, Copy, Clone)]
enum ConditionState {
    /// We are currently parsing code
    Enabled,

    /// We are not parsing code but we are parsing conditions
    DisabledInner,

    /// We are not parsing code and we are not parsing conditions
    DisabledOuter,
}

impl ConditionChain {
    fn new() -> ConditionChain {
        ConditionChain(vec![])
    }

    fn push(&mut self, gate: ConditionState) {
        self.0.push(gate);
    }

    fn switch(&mut self, active: bool) -> Result<(), PreprocessError> {
        match self.0.pop() {
            Some(val) => {
                self.0.push(match val {
                    ConditionState::Enabled => ConditionState::DisabledOuter,
                    ConditionState::DisabledInner if active => ConditionState::Enabled,
                    ConditionState::DisabledInner => ConditionState::DisabledInner,
                    ConditionState::DisabledOuter => ConditionState::DisabledOuter,
                });
                Ok(())
            }
            None => Err(PreprocessError::ElseNotMatched),
        }
    }

    fn pop(&mut self) -> Result<(), PreprocessError> {
        match self.0.pop() {
            Some(_) => Ok(()),
            None => Err(PreprocessError::EndIfNotMatched),
        }
    }

    fn is_active(&self) -> bool {
        self.0.iter().all(|gate| *gate == ConditionState::Enabled)
    }
}

// Function to find end of definition past escaped endlines
fn find_macro_end(mut remaining: &str) -> usize {
    let initial_length = remaining.len();
    loop {
        match remaining.find('\n') {
            Some(sz) => {
                let before_c0 = &remaining[..sz];
                let before_c1 = if sz > 0 { &remaining[..(sz - 1)] } else { "" };
                remaining = &remaining[(sz + 1)..];
                match (before_c0.chars().last(), before_c1.chars().last()) {
                    (Some(x), _) if x == '\\' => {}
                    (Some(x), Some(y)) if x == '\r' && y == '\\' => {}
                    _ => break,
                }
            }
            None => {
                return remaining.len();
            }
        }
    }
    initial_length - remaining.len() - 1
}

fn get_after_single_line(remaining: &str) -> &str {
    let len = match remaining.find('\n') {
        Some(sz) => sz + 1,
        None => remaining.len(),
    };
    &remaining[len..]
}

#[test]
fn test_get_after_single_line() {
    assert_eq!(get_after_single_line("one\ntwo"), "two");
    assert_eq!(get_after_single_line("one\ntwo\nthree"), "two\nthree");
    assert_eq!(get_after_single_line("one\\\ntwo\nthree"), "two\nthree");
    assert_eq!(get_after_single_line("one\r\ntwo\r\nthree"), "two\r\nthree");
    assert_eq!(
        get_after_single_line("one\\\r\ntwo\r\nthree"),
        "two\r\nthree"
    );
}

fn get_after_macro(remaining: &str) -> &str {
    let len = find_macro_end(remaining) + 1;
    &remaining[len..]
}

#[test]
fn test_get_after_macro() {
    assert_eq!(get_after_macro("one\ntwo"), "two");
    assert_eq!(get_after_macro("one\ntwo\nthree"), "two\nthree");
    assert_eq!(get_after_macro("one\\\ntwo\nthree"), "three");
    assert_eq!(get_after_macro("one\r\ntwo\r\nthree"), "two\r\nthree");
    assert_eq!(get_after_macro("one\\\r\ntwo\r\nthree"), "three");
}

fn get_macro_line(remaining: &str) -> &str {
    let len = find_macro_end(remaining);
    remaining[..len].trim_end()
}

#[test]
fn test_get_macro_line() {
    assert_eq!(get_macro_line("one\ntwo"), "one");
    assert_eq!(get_macro_line("one\ntwo\nthree"), "one");
    assert_eq!(get_macro_line("one\\\ntwo\nthree"), "one\\\ntwo");
    assert_eq!(get_macro_line("one\r\ntwo\r\nthree"), "one");
    assert_eq!(get_macro_line("one\\\r\ntwo\r\nthree"), "one\\\r\ntwo");
}

fn preprocess_command<'a>(
    buffer: &mut IntermediateText,
    file_loader: &mut FileLoader,
    command: &'a str,
    file_id: FileId,
    location: SourceLocation,
    macros: &mut Vec<Macro>,
    condition_chain: &mut ConditionChain,
) -> Result<&'a str, PreprocessError> {
    let skip = !condition_chain.is_active();
    if let Some(next) = command.strip_prefix("include") {
        if skip {
            return Ok(get_after_single_line(command));
        }
        match next.chars().next() {
            Some(' ') | Some('\t') | Some('"') | Some('<') => {
                let args = next.trim_start();
                let end = match args.chars().next() {
                    Some('"') => '"',
                    Some('<') => '>',
                    _ => return Err(PreprocessError::InvalidInclude),
                };
                let args = &args[1..];
                match args.find(end) {
                    Some(sz) => {
                        let file_name = &args[..sz];
                        if file_name.contains('\n') {
                            return Err(PreprocessError::InvalidInclude);
                        }

                        // Include the file
                        match file_loader.load(file_name) {
                            Ok(file) => {
                                preprocess_included_file(
                                    buffer,
                                    file_loader,
                                    file,
                                    macros,
                                    condition_chain,
                                )?;

                                let next = &args[(sz + 1)..];
                                let end = match next.find('\n') {
                                    Some(sz) => {
                                        // Push a new line so the last line of the include file is
                                        // on a separate line to the first line after the #include
                                        buffer.push_str("\n", location);
                                        sz + 1
                                    }
                                    None => next.len(),
                                };
                                let remains = &next[..end].trim();
                                if !remains.is_empty() && !remains.starts_with("//") {
                                    return Err(PreprocessError::InvalidInclude);
                                }

                                let next = &next[end..];

                                Ok(next)
                            }
                            Err(err) => Err(PreprocessError::FailedToFindFile(
                                file_name.to_string(),
                                err,
                            )),
                        }
                    }
                    None => Err(PreprocessError::InvalidInclude),
                }
            }
            _ => Err(PreprocessError::InvalidInclude),
        }
    } else if command.starts_with("ifdef") || command.starts_with("ifndef") {
        if skip {
            condition_chain.push(ConditionState::DisabledInner);
            return Ok(get_after_single_line(command));
        }
        let not = command.starts_with("ifndef");
        let next = if not { &command[6..] } else { &command[5..] };
        match next.chars().next() {
            Some(' ') | Some('\t') => {
                let args = next.trim_start();
                let end = match args.find('\n') {
                    Some(sz) => sz + 1,
                    _ => return Err(PreprocessError::InvalidIfndef(command.to_string())),
                };
                let body = &args[..end].trim();

                let exists = macros.iter().any(|m| &m.0 == body);
                let active = if not { !exists } else { exists };
                condition_chain.push(if active {
                    ConditionState::Enabled
                } else {
                    ConditionState::DisabledInner
                });

                let remaining = &args[end..];
                Ok(remaining)
            }
            _ => Err(PreprocessError::InvalidIfndef(command.to_string())),
        }
    } else if let Some(next) = command.strip_prefix("if") {
        if skip {
            condition_chain.push(ConditionState::DisabledInner);
            return Ok(get_after_single_line(command));
        }
        match next.chars().next() {
            Some(' ') | Some('\t') | Some('(') => {
                let args = next.trim_start();
                let end = match args.find('\n') {
                    Some(sz) => sz + 1,
                    _ => return Err(PreprocessError::InvalidIf(command.to_string())),
                };
                let body = &args[..end].trim();
                let remaining = &args[end..];

                let resolved = SubstitutedText::new(body, SourceLocation::UNKNOWN)
                    .apply_all(macros, true)?
                    .resolve();

                let resolved_str: &str = &resolved;
                // Sneaky hack to make `#if COND // comment` work
                let resolved_no_comment = match resolved_str.find("//") {
                    Some(sz) => resolved_str[..sz].trim(),
                    None => resolved_str,
                };

                let active = crate::condition_parser::parse(resolved_no_comment)?;
                condition_chain.push(if active {
                    ConditionState::Enabled
                } else {
                    ConditionState::DisabledInner
                });

                Ok(remaining)
            }
            _ => Err(PreprocessError::InvalidIf(command.to_string())),
        }
    } else if let Some(next) = command.strip_prefix("else") {
        match next.chars().next() {
            Some(' ') | Some('\t') | Some('\n') | Some('\r') => {
                let args = next;
                let end = match args.find('\n') {
                    Some(sz) => sz + 1,
                    _ => return Err(PreprocessError::InvalidElse),
                };
                let body = args[..end].trim();
                if !body.is_empty() && !body.starts_with("//") {
                    return Err(PreprocessError::InvalidElse);
                }

                condition_chain.switch(true)?;

                let remaining = &args[end..];
                Ok(remaining)
            }
            _ => Err(PreprocessError::InvalidElse),
        }
    } else if let Some(next) = command.strip_prefix("elif") {
        match next.chars().next() {
            Some(' ') | Some('\t') | Some('(') => {
                let args = next.trim_start();
                let end = match args.find('\n') {
                    Some(sz) => sz + 1,
                    _ => return Err(PreprocessError::InvalidElif(command.to_string())),
                };
                let body = &args[..end].trim();
                let remaining = &args[end..];

                let resolved = SubstitutedText::new(body, SourceLocation::UNKNOWN)
                    .apply_all(macros, true)?
                    .resolve();

                let resolved_str: &str = &resolved;
                // Sneaky hack to make `#if COND // comment` work
                let resolved_no_comment = match resolved_str.find("//") {
                    Some(sz) => resolved_str[..sz].trim(),
                    None => resolved_str,
                };

                let active = crate::condition_parser::parse(resolved_no_comment)?;
                condition_chain.switch(active)?;

                if skip {
                    Ok(get_after_single_line(command))
                } else {
                    Ok(remaining)
                }
            }
            _ => Err(PreprocessError::InvalidElif(command.to_string())),
        }
    } else if let Some(next) = command.strip_prefix("endif") {
        match next.chars().next() {
            Some(' ') | Some('\t') | Some('\n') | Some('\r') | None => {
                let args = next;
                let end = match args.find('\n') {
                    Some(sz) => sz + 1,
                    None => args.len(),
                };
                let body = &args[..end].trim();
                if !body.is_empty() && !body.starts_with("//") {
                    return Err(PreprocessError::InvalidEndIf);
                }

                condition_chain.pop()?;

                let remaining = &args[end..];
                Ok(remaining)
            }
            _ => Err(PreprocessError::InvalidEndIf),
        }
    } else if let Some(next) = command.strip_prefix("define") {
        if skip {
            return Ok(get_after_macro(command));
        }
        match next.chars().next() {
            Some(' ') | Some('\t') => {
                let mut remaining = next[1..].trim_start();

                // Consume define name
                let header_start = remaining;
                loop {
                    match remaining.chars().next() {
                        Some(c) if is_identifier_char(c) => {
                            remaining = &remaining[1..];
                        }
                        _ => break,
                    }
                }

                // Consume macro args
                if let Some('(') = remaining.chars().next() {
                    remaining = &remaining[1..];
                    match remaining.find(')') {
                        Some(sz) => {
                            remaining = &remaining[(sz + 1)..];
                        }
                        None => return Err(PreprocessError::InvalidDefine),
                    }
                }

                // Let the header be the name + args
                let header = &header_start[..(header_start.len() - remaining.len())];

                // Consume gap between macro name/args and body
                let (body, remaining) = match remaining.chars().next() {
                    Some(' ') | Some('\t') | Some('\r') => {
                        remaining = &remaining[1..];
                        let sz = find_macro_end(remaining);
                        let body = &remaining[..sz];
                        (body, &remaining[(sz + 1)..])
                    }
                    Some('\n') => (&remaining[..1], &remaining[1..]),
                    None => ("", ""),
                    _ => return Err(PreprocessError::InvalidDefine),
                };

                let body = body.trim().replace("\\\n", "\n").replace("\\\r\n", "\r\n");
                let subbed_body = SubstitutedText::new(&body, SourceLocation::UNKNOWN)
                    .apply_all(macros, false)?
                    .resolve();
                let macro_def = Macro::from_definition(header, &subbed_body, location)?;

                for current_macro in macros.iter() {
                    if *current_macro.0 == macro_def.0 {
                        return Err(PreprocessError::MacroAlreadyDefined(
                            current_macro.0.clone(),
                        ));
                    }
                }
                macros.push(macro_def);

                Ok(remaining)
            }
            _ => Err(PreprocessError::InvalidDefine),
        }
    } else if let Some(next) = command.strip_prefix("pragma") {
        if skip {
            return Ok(get_after_macro(command));
        }
        let pragma_command = get_macro_line(next).trim();
        if pragma_command == "once" {
            file_loader.mark_as_pragma_once(file_id);
            Ok(get_after_single_line(command))
        } else if pragma_command.starts_with("warning") {
            // Ignore warning disable commands for now
            Ok(get_after_single_line(command))
        } else {
            Err(PreprocessError::UnknownCommand(format!(
                "#pragma {}",
                pragma_command
            )))
        }
    } else if skip {
        return Ok(get_after_macro(command));
    } else {
        Err(PreprocessError::UnknownCommand(format!(
            "#{}",
            get_macro_line(command)
        )))
    }
}

/// Internal process a single file during preprocessing
fn preprocess_included_file(
    buffer: &mut IntermediateText,
    file_loader: &mut FileLoader,
    input_file: InputFile,
    macros: &mut Vec<Macro>,
    condition_chain: &mut ConditionChain,
) -> Result<(), PreprocessError> {
    let file_length = input_file.contents.len() as u32;

    let mut stream = input_file.contents.as_str();
    loop {
        let stream_location_in_file = StreamLocation(file_length - stream.len() as u32);
        let source_location = file_loader
            .get_source_location_from_file_offset(input_file.file_id, stream_location_in_file);
        let start_trimmed = stream.trim_start();
        if let Some(command) = start_trimmed.strip_prefix('#') {
            let command = command.trim_start();
            stream = preprocess_command(
                buffer,
                file_loader,
                command,
                input_file.file_id,
                source_location,
                macros,
                condition_chain,
            )?;
        } else {
            fn find_region(mut stream: &str) -> (usize, bool) {
                let mut size = 0;
                let mut final_segment;
                loop {
                    let (sz, fs) = match stream.find('\n') {
                        Some(sz) => (sz + 1, false),
                        None => (stream.len(), true),
                    };
                    size += sz;
                    final_segment = fs;
                    stream = &stream[sz..];
                    if final_segment || stream.trim_start().starts_with('#') {
                        break;
                    }
                }
                (size, final_segment)
            }

            let (sz, final_segment) = find_region(stream);
            let line = &stream[..sz];
            stream = &stream[sz..];
            if condition_chain.is_active() {
                SubstitutedText::new(line, source_location)
                    .apply_all(macros, false)?
                    .store(buffer);
            }
            if final_segment {
                break;
            }
        }
    }

    Ok(())
}

/// Preprocess a file after having set up the file loader
fn preprocess_initial_file(
    input_file: InputFile,
    file_loader: &mut FileLoader,
) -> Result<PreprocessedText, PreprocessError> {
    let mut intermediate_text = IntermediateText::new();
    let mut macros = vec![];
    let mut condition_chain = ConditionChain::new();

    preprocess_included_file(
        &mut intermediate_text,
        file_loader,
        input_file,
        &mut macros,
        &mut condition_chain,
    )?;

    if !condition_chain.0.is_empty() {
        return Err(PreprocessError::ConditionChainNotFinished);
    }

    Ok(PreprocessedText::new(
        intermediate_text.buffer,
        intermediate_text.locations,
    ))
}

/// Preprocess a file - starting from memory
pub fn preprocess_direct(
    input: &str,
    file_name: FileName,
    source_manager: &mut SourceManager,
    include_handler: &mut dyn IncludeHandler,
) -> Result<PreprocessedText, PreprocessError> {
    // Store the input in the source manager
    let file_id = source_manager.add_file(file_name, input.to_string());
    let input_file = InputFile {
        file_id,
        contents: source_manager.get_contents(file_id).to_string(),
    };

    let mut file_loader = FileLoader::new(source_manager, include_handler);
    preprocess_initial_file(input_file, &mut file_loader)
}

/// Preprocess a file - starting from include handler
pub fn preprocess(
    entry_file_name: &str,
    source_manager: &mut SourceManager,
    include_handler: &mut dyn IncludeHandler,
) -> Result<PreprocessedText, PreprocessError> {
    let mut file_loader = FileLoader::new(source_manager, include_handler);
    match file_loader.load(entry_file_name) {
        Ok(file) => preprocess_initial_file(file, &mut file_loader),
        Err(err) => Err(PreprocessError::FailedToFindFile(
            entry_file_name.to_string(),
            err,
        )),
    }
}

/// Preprocess a single block of text without any support for includes
pub fn preprocess_fragment(
    input: &str,
    file_name: FileName,
    source_manager: &mut SourceManager,
) -> Result<PreprocessedText, PreprocessError> {
    preprocess_direct(input, file_name, source_manager, &mut NullIncludeHandler)
}
