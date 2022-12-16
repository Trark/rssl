use crate::lexer::LexerError;
use rssl_text::tokens::*;
use rssl_text::*;
use std::collections::{HashMap, HashSet};

/// An error which occurred when attempting to preprocess a file
#[derive(PartialEq, Debug, Clone)]
pub enum PreprocessError {
    LexerError(LexerError),
    UnknownCommand(SourceLocation),
    InvalidInclude(SourceLocation),
    InvalidDefine(SourceLocation),
    MacroAlreadyDefined(String),
    MacroRequiresArguments(String),
    MacroArgumentsNeverEnd,
    MacroExpectsDifferentNumberOfArguments,
    FailedToFindFile(SourceLocation, String, IncludeError),
    FailedToParseIfCondition(SourceLocation),
    InvalidIfdef(SourceLocation),
    InvalidIfndef(SourceLocation),
    InvalidElse(SourceLocation),
    InvalidEndIf(SourceLocation),
    ConditionChainNotFinished,
    ElseNotMatched,
    EndIfNotMatched,
    UnknownPragma(SourceLocation),
    PragmaOnceInUnknownFile,
}

impl PreprocessError {
    /// Get formatter to print the error
    pub fn display<'a>(&'a self, source_manager: &'a SourceManager) -> PreprocessErrorPrinter<'a> {
        PreprocessErrorPrinter(self, source_manager)
    }
}

/// Prints preprocessor errors
pub struct PreprocessErrorPrinter<'a>(&'a PreprocessError, &'a SourceManager);

impl<'a> std::fmt::Display for PreprocessErrorPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let PreprocessErrorPrinter(err, source_manager) = self;

        // Shared error message printing logic
        let mut write_message = |write: &dyn Fn(&mut std::fmt::Formatter) -> std::fmt::Result,
                                 loc: SourceLocation| {
            if loc != SourceLocation::UNKNOWN {
                // Get file location info
                let file_location = source_manager.get_file_location(loc);

                // Print basic failure reason
                write!(f, "{}: error: ", file_location)?;
                write(f)?;
                writeln!(f)?;

                // Print source that caused the error
                source_manager.write_source_for_error(f, Some(loc))
            } else {
                // Print basic failure reason
                write!(f, "error: ")?;
                write(f)?;
                writeln!(f)
            }
        };

        match err {
            PreprocessError::LexerError(err) => {
                write_message(&|f| write!(f, "{}", err.reason), err.location)
            }
            PreprocessError::UnknownCommand(loc) => {
                write_message(&|f| write!(f, "unknown preprocessing directive"), *loc)
            }
            PreprocessError::UnknownPragma(loc) => {
                write_message(&|f| write!(f, "unknown pragma"), *loc)
            }
            PreprocessError::InvalidInclude(loc) => {
                write_message(&|f| write!(f, "invalid #include command"), *loc)
            }
            PreprocessError::InvalidDefine(loc) => {
                write_message(&|f| write!(f, "invalid #define command"), *loc)
            }
            PreprocessError::MacroAlreadyDefined(s) => write_message(
                &|f| write!(f, "macro '{}' already defined", s),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::MacroRequiresArguments(s) => write_message(
                &|f| write!(f, "macro function '{}' requires arguments", s),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::MacroArgumentsNeverEnd => write_message(
                &|f| write!(f, "expected end of macro arguments"),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::MacroExpectsDifferentNumberOfArguments => write_message(
                &|f| write!(f, "macro requires different number of arguments"),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::FailedToFindFile(loc, name, _) => {
                write_message(&|f| write!(f, "failed to load file: '{}'", name), *loc)
            }
            PreprocessError::FailedToParseIfCondition(loc) => {
                write_message(&|f| write!(f, "#if condition parser failed"), *loc)
            }
            PreprocessError::InvalidIfdef(loc) => {
                write_message(&|f| write!(f, "invalid #ifdef"), *loc)
            }
            PreprocessError::InvalidIfndef(loc) => {
                write_message(&|f| write!(f, "invalid #ifndef"), *loc)
            }
            PreprocessError::InvalidElse(loc) => {
                write_message(&|f| write!(f, "invalid #else"), *loc)
            }
            PreprocessError::InvalidEndIf(loc) => {
                write_message(&|f| write!(f, "invalid #endif"), *loc)
            }
            PreprocessError::ConditionChainNotFinished => write_message(
                &|f| write!(f, "not enough #endif's encountered"),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::ElseNotMatched => write_message(
                &|f| write!(f, "encountered #else but with no matching #if"),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::EndIfNotMatched => write_message(
                &|f| write!(f, "rncountered #endif but with no matching #if"),
                SourceLocation::UNKNOWN,
            ),
            PreprocessError::PragmaOnceInUnknownFile => write_message(
                &|f| write!(f, "encountered #pragma once in an unknown file"),
                SourceLocation::UNKNOWN,
            ),
        }
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

#[derive(PartialEq, Debug, Clone)]
struct MacroArg(u64);

#[derive(PartialEq, Debug, Clone)]
enum MacroSegment {
    Text(Vec<LexToken>),
    Arg(MacroArg),
}

impl MacroSegment {
    fn build_segments(params: &[String], body: &[LexToken]) -> Vec<MacroSegment> {
        let mut last_segments = Vec::from([MacroSegment::Text(body.to_vec())]);
        for (index, arg_name) in params.iter().enumerate() {
            let mut next_segments = Vec::new();
            for segment in last_segments {
                segment.split(arg_name, index as u64, &mut next_segments);
            }
            last_segments = next_segments;
        }
        last_segments
    }

    fn split(self, arg: &str, index: u64, segments: &mut Vec<MacroSegment>) {
        match self {
            MacroSegment::Text(stream) => {
                if let Some(sz) = find_macro(&stream, arg) {
                    let before = &stream[..sz];
                    let after = &stream[sz + 1..];
                    if !before.is_empty() {
                        segments.push(MacroSegment::Text(before.to_vec()));
                    }
                    segments.push(MacroSegment::Arg(MacroArg(index)));
                    if !after.is_empty() {
                        MacroSegment::Text(after.to_vec()).split(arg, index, segments);
                    }
                    return;
                }
                segments.push(MacroSegment::Text(stream))
            }
            MacroSegment::Arg(arg) => segments.push(MacroSegment::Arg(arg)),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct Macro {
    name: String,
    is_function: bool,
    num_params: u64,
    segments: Vec<MacroSegment>,
    location: SourceLocation,
}

impl Macro {
    fn parse(command: &[LexToken], macros: &[Macro]) -> Result<Macro, PreprocessError> {
        let command = trim_whitespace_start(command);

        let location = match command.first() {
            Some(tok) => tok.1,
            None => SourceLocation::UNKNOWN,
        };

        // Consume define name
        let (name, location, signature_and_body) =
            if let Some((LexToken(Token::Id(id), name_loc), rest)) = command.split_first() {
                (id.0.clone(), *name_loc, rest)
            } else {
                return Err(PreprocessError::InvalidDefine(location));
            };

        // Ensure the macro does not already exist
        for current_macro in macros.iter() {
            if *current_macro.name == name {
                return Err(PreprocessError::MacroAlreadyDefined(
                    current_macro.name.clone(),
                ));
            }
        }

        // Consume macro parameters from body
        let (param_tokens, is_function, body) =
            if let Some((LexToken(Token::LeftParen, _), rest)) = signature_and_body.split_first() {
                if let Some(pos) = rest.iter().position(|t| t.0 == Token::RightParen) {
                    (&rest[..pos], true, &rest[pos + 1..])
                } else {
                    return Err(PreprocessError::InvalidDefine(location));
                }
            } else {
                (&[][..], false, signature_and_body)
            };

        // Find macro parameter names
        let mut params = Vec::new();
        {
            let mut param_tokens = param_tokens;
            while !param_tokens.is_empty() {
                let next_param =
                    if let Some(pos) = param_tokens.iter().position(|t| t.0 == Token::Comma) {
                        let param = &param_tokens[..pos];
                        param_tokens = &param_tokens[pos + 1..];
                        param
                    } else {
                        let param = param_tokens;
                        param_tokens = &[][..];
                        param
                    };

                let next_param = trim_whitespace(next_param);

                // We expect a single name for all macro arguments
                // But a macro with zero arguments will have one empty item processed
                if let [LexToken(Token::Id(id), _)] = next_param {
                    params.push(id.0.clone());
                } else if !(params.is_empty() && param_tokens.is_empty()) {
                    return Err(PreprocessError::InvalidDefine(location));
                }
            }
        }

        // Remove whitespace at start and end of the body of the define
        let body = trim_whitespace(body);

        // Apply other defines into the body
        let subbed_body = SubstitutedText::new(body)
            .apply_all(macros, false)?
            .resolve();

        // Build segments of the body by splitting between parameters
        let segments = MacroSegment::build_segments(&params, &subbed_body);

        let def = Macro {
            name,
            is_function,
            num_params: params.len() as u64,
            segments,
            location,
        };

        Ok(def)
    }
}

#[derive(PartialEq, Debug, Clone)]
enum SubstitutedSegment {
    Text(Vec<LexToken>),
    Replaced(Vec<LexToken>),
}

/// Remove whitespace and comments from the start of a token stream - but not endlines
fn trim_whitespace_start(mut tokens: &[LexToken]) -> &[LexToken] {
    while let Some((LexToken(tok, _), rest)) = tokens.split_first() {
        if tok.is_whitespace() && *tok != Token::Endline {
            tokens = rest;
        } else {
            break;
        }
    }
    tokens
}

/// Remove whitespace and comments from the end of a token stream - but not endlines
fn trim_whitespace_end(mut tokens: &[LexToken]) -> &[LexToken] {
    while let Some((LexToken(tok, _), rest)) = tokens.split_last() {
        if tok.is_whitespace() && *tok != Token::Endline {
            tokens = rest;
        } else {
            break;
        }
    }
    tokens
}

/// Remove whitespace and comments from the start and end of a token stream - but not endlines
fn trim_whitespace(tokens: &[LexToken]) -> &[LexToken] {
    trim_whitespace_end(trim_whitespace_start(tokens))
}

fn find_macro(stream: &[LexToken], name: &str) -> Option<usize> {
    for (i, token) in stream.iter().enumerate() {
        if let LexToken(Token::Id(id), _) = token {
            if id.0 == name {
                return Some(i);
            }
        }
    }

    None
}

fn split_macro_args<'stream>(
    macro_name: &str,
    remaining: &'stream [LexToken],
) -> Result<(&'stream [LexToken], Vec<&'stream [LexToken]>), PreprocessError> {
    // Consume the starting bracket
    let remaining = trim_whitespace_start(remaining);
    let mut remaining = if let [LexToken(Token::LeftParen, _), rest @ ..] = remaining {
        rest
    } else {
        return Err(PreprocessError::MacroRequiresArguments(
            macro_name.to_string(),
        ));
    };

    let mut args = vec![];
    let mut brace_scope = 0;
    let mut remaining_offset = 0;
    loop {
        let next_pos_result = remaining[remaining_offset..]
            .iter()
            .position(|t| matches!(t.0, Token::Comma | Token::LeftParen | Token::RightParen));
        let next_opt = next_pos_result.map(|next_pos| {
            (
                remaining[next_pos + remaining_offset].0.clone(),
                next_pos + remaining_offset,
            )
        });
        match next_opt {
            Some((Token::Comma, pos)) => {
                if brace_scope == 0 {
                    // Next argument
                    let arg = trim_whitespace(&remaining[..pos]);
                    args.push(arg);
                    remaining = &remaining[(pos + 1)..];
                    remaining_offset = 0;
                } else {
                    // Argument lists inside inner parenthesis
                    remaining_offset = pos + 1;
                }
            }
            Some((Token::LeftParen, pos)) => {
                // Start of inner parenthesis
                brace_scope += 1;
                remaining_offset = pos + 1;
            }
            Some((Token::RightParen, pos)) => {
                if brace_scope > 0 {
                    // End of inner parenthesis
                    brace_scope -= 1;
                    remaining_offset = pos + 1;
                } else {
                    // End of macro
                    let arg = trim_whitespace(&remaining[..pos]);
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
            SubstitutedSegment::Text(text) => {
                // Find the first macro that matches the text
                // We could try to apply defined() in this loop - but this is currently resolved in a prepass before all other macros
                let mut best_match_opt: Option<(&Macro, usize)> = None;
                let mut search_range = text.as_slice();
                for macro_def in macro_defs {
                    if let Some(pos) = find_macro(search_range, &macro_def.name) {
                        // As we limit the search range we don't expect
                        assert!(if let Some(best_match) = best_match_opt {
                            pos < best_match.1
                        } else {
                            true
                        });

                        // Check we have the start of function parameters
                        let mut valid_macro = true;
                        if macro_def.is_function {
                            valid_macro = false;
                            if let [LexToken(Token::LeftParen, _), ..] =
                                trim_whitespace_start(&text[pos + 1..])
                            {
                                valid_macro = true;
                            }
                        }

                        if valid_macro {
                            best_match_opt = Some((macro_def, pos));
                            // Limit search for future macros so we only get better matches
                            search_range = &text[..pos];
                        }
                    }
                }

                if let Some((macro_def, sz)) = best_match_opt {
                    let before = &text[..sz];
                    let mut remaining = &text[sz + 1..];

                    // Read macro arguments
                    let args = if macro_def.is_function {
                        let (rest, args) = split_macro_args(&macro_def.name, remaining)?;
                        remaining = rest;

                        if macro_def.num_params == 0 {
                            if !(args.len() == 1 && args[0].is_empty()) {
                                return Err(
                                    PreprocessError::MacroExpectsDifferentNumberOfArguments,
                                );
                            }
                        } else if args.len() as u64 != macro_def.num_params {
                            return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                        }

                        args
                    } else {
                        vec![]
                    };
                    let after = remaining;

                    // Substitute macros inside macro arguments
                    let args = args.into_iter().fold(Ok(vec![]), |vec, arg| {
                        let mut vec = vec?;
                        let raw_text = SubstitutedText::new(arg);
                        let subbed_text = raw_text.apply_all(macro_defs, false)?;
                        let final_text = subbed_text.resolve();
                        vec.push(final_text);
                        Ok(vec)
                    })?;

                    if !before.is_empty() {
                        output.push(SubstitutedSegment::Text(before.to_vec()));
                    }
                    let mut replaced_tokens = Vec::new();
                    for macro_segment in &macro_def.segments {
                        match macro_segment {
                            MacroSegment::Text(tokens) => replaced_tokens.extend_from_slice(tokens),
                            MacroSegment::Arg(MacroArg(index)) => {
                                replaced_tokens.extend_from_slice(&args[*index as usize])
                            }
                        }
                    }
                    if !replaced_tokens.is_empty() {
                        output.push(SubstitutedSegment::Replaced(replaced_tokens));
                    }
                    if !after.is_empty() {
                        Ok(Some(SubstitutedSegment::Text(after.to_vec())))
                    } else {
                        Ok(None)
                    }
                } else {
                    assert!(!text.is_empty());
                    output.push(SubstitutedSegment::Text(text));
                    Ok(None)
                }
            }
            SubstitutedSegment::Replaced(text) => {
                output.push(SubstitutedSegment::Replaced(text));
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
            SubstitutedSegment::Text(text) => {
                if let Some(sz) = find_macro(&text, defined_name) {
                    let before = &text[..sz];
                    let mut remaining = &text[sz + 1..];

                    // Read argument
                    let arg = {
                        // Allow "defined A" instead of traditional defined(A)
                        if let [LexToken(Token::Whitespace, _), LexToken(arg @ Token::Id(_), _), rest @ ..] =
                            remaining
                        {
                            remaining = rest;
                            arg
                        } else {
                            // Parse arguments like a normal macro called defined
                            let (rest, args) = split_macro_args(defined_name, remaining)?;
                            remaining = rest;

                            // Expect one argument
                            if args.len() as u64 != 1 {
                                return Err(
                                    PreprocessError::MacroExpectsDifferentNumberOfArguments,
                                );
                            }

                            // Expect a single name as the argument
                            if let [LexToken(arg @ Token::Id(_), _)] = args[0] {
                                arg
                            } else {
                                return Err(
                                    PreprocessError::MacroExpectsDifferentNumberOfArguments,
                                );
                            }
                        }
                    };

                    let after = remaining;

                    let exists = macro_defs.iter().any(|m| {
                        if let Token::Id(id) = arg {
                            m.name == id.0
                        } else {
                            false
                        }
                    });

                    if !before.is_empty() {
                        output.push(SubstitutedSegment::Text(before.to_vec()));
                    }
                    output.push(SubstitutedSegment::Replaced(Vec::from([LexToken(
                        if exists {
                            Token::LiteralInt(1)
                        } else {
                            Token::LiteralInt(0)
                        },
                        SourceLocation::UNKNOWN,
                    )])));
                    if !after.is_empty() {
                        SubstitutedSegment::Text(after.to_vec())
                            .apply_defined(macro_defs, output)?;
                    }
                    return Ok(());
                }
                assert!(!text.is_empty());
                output.push(SubstitutedSegment::Text(text))
            }
            SubstitutedSegment::Replaced(text) => output.push(SubstitutedSegment::Replaced(text)),
        }
        Ok(())
    }
}

#[derive(Debug)]
struct SubstitutedText(Vec<SubstitutedSegment>);

impl SubstitutedText {
    fn new(stream: &[LexToken]) -> SubstitutedText {
        if stream.is_empty() {
            SubstitutedText(Vec::new())
        } else {
            SubstitutedText(vec![SubstitutedSegment::Text(stream.to_vec())])
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

    fn store(self, output: &mut Vec<LexToken>) {
        for substituted_segment in self.0 {
            match substituted_segment {
                SubstitutedSegment::Text(stream) | SubstitutedSegment::Replaced(stream) => {
                    assert!(!stream.is_empty());
                    output.extend(stream)
                }
            }
        }
    }

    fn resolve(self) -> Vec<LexToken> {
        let mut output = Vec::new();
        for substituted_segment in self.0 {
            match substituted_segment {
                SubstitutedSegment::Text(stream) | SubstitutedSegment::Replaced(stream) => {
                    assert!(!stream.is_empty());
                    output.extend(stream)
                }
            }
        }
        output
    }
}

#[test]
fn macro_from_definition() {
    let ll = |s: &str| {
        let mut source_manager = SourceManager::new();
        let (file_id, source_location) = source_manager.add_fragment(s);
        assert_eq!(source_location, SourceLocation::first());
        crate::lexer::lex_fragment(file_id, &source_manager).unwrap()
    };
    assert_eq!(
        Macro::parse(&ll("B 0"), &[]).unwrap(),
        Macro {
            name: "B".to_string(),
            is_function: false,
            num_params: 0,
            segments: Vec::from([MacroSegment::Text(Vec::from([LexToken(
                Token::LiteralInt(0),
                SourceLocation::first().offset(2)
            )]))]),
            location: SourceLocation::first(),
        }
    );
    assert_eq!(
        Macro::parse(&ll("B(x) x"), &[]).unwrap(),
        Macro {
            name: "B".to_string(),
            is_function: true,
            num_params: 1,
            segments: vec![MacroSegment::Arg(MacroArg(0))],
            location: SourceLocation::first(),
        }
    );
    assert_eq!(
        Macro::parse(&ll("B(x,y) x"), &[]).unwrap(),
        Macro {
            name: "B".to_string(),
            is_function: true,
            num_params: 2,
            segments: vec![MacroSegment::Arg(MacroArg(0))],
            location: SourceLocation::first(),
        }
    );
    assert_eq!(
        Macro::parse(&ll("B(x,y) y"), &[]).unwrap(),
        Macro {
            name: "B".to_string(),
            is_function: true,
            num_params: 2,
            segments: vec![MacroSegment::Arg(MacroArg(1))],
            location: SourceLocation::first(),
        }
    );
    assert_eq!(
        Macro::parse(&ll("B(x,xy) (x || xy)"), &[]).unwrap(),
        Macro {
            name: "B".to_string(),
            is_function: true,
            num_params: 2,
            segments: vec![
                MacroSegment::Text(Vec::from([LexToken(
                    Token::LeftParen,
                    SourceLocation::first().offset(8)
                )])),
                MacroSegment::Arg(MacroArg(0)),
                MacroSegment::Text(Vec::from([
                    LexToken(Token::Whitespace, SourceLocation::first().offset(10)),
                    LexToken(
                        Token::VerticalBarVerticalBar,
                        SourceLocation::first().offset(11)
                    ),
                    LexToken(Token::Whitespace, SourceLocation::first().offset(13)),
                ])),
                MacroSegment::Arg(MacroArg(1)),
                MacroSegment::Text(Vec::from([LexToken(
                    Token::RightParen,
                    SourceLocation::first().offset(16)
                )])),
            ],
            location: SourceLocation::first(),
        }
    );
}

#[test]
fn macro_resolve() {
    use crate::lexer::lex_fragment;
    use crate::unlexer::unlex;

    #[track_caller]
    fn run(
        input: &[LexToken],
        macros: &[Macro],
        expected_str: &str,
        expected_tokens: &[LexToken],
        source_manager: &SourceManager,
    ) {
        let text = SubstitutedText::new(input);
        let resolved_tokens = text.apply_all(macros, false).unwrap().resolve();
        assert_eq!(resolved_tokens, expected_tokens);

        let output_str = unlex(&resolved_tokens, source_manager);
        assert_eq!(output_str, expected_str);
    }

    {
        let mut source_manager = SourceManager::new();
        let (m1, m1_loc) = source_manager.add_fragment("B 0");
        let (m2, m2_loc) = source_manager.add_fragment("BC 1");
        let (main, main_loc) = source_manager.add_fragment("(A || B) && BC");

        let m1_tokens = lex_fragment(m1, &source_manager).unwrap();
        let m2_tokens = lex_fragment(m2, &source_manager).unwrap();
        let main_tokens = lex_fragment(main, &source_manager).unwrap();

        run(
            &main_tokens,
            &[
                Macro::parse(&m1_tokens, &[]).unwrap(),
                Macro::parse(&m2_tokens, &[]).unwrap(),
            ],
            "(A || 0) && 1",
            &[
                LexToken(Token::LeftParen, main_loc.offset(0)),
                LexToken(Token::Id(Identifier("A".to_string())), main_loc.offset(1)),
                LexToken(Token::Whitespace, main_loc.offset(2)),
                LexToken(Token::VerticalBarVerticalBar, main_loc.offset(3)),
                LexToken(Token::Whitespace, main_loc.offset(5)),
                LexToken(Token::LiteralInt(0), m1_loc.offset(2)),
                LexToken(Token::RightParen, main_loc.offset(7)),
                LexToken(Token::Whitespace, main_loc.offset(8)),
                LexToken(Token::AmpersandAmpersand, main_loc.offset(9)),
                LexToken(Token::Whitespace, main_loc.offset(11)),
                LexToken(Token::LiteralInt(1), m2_loc.offset(3)),
            ],
            &source_manager,
        );
    }

    {
        let mut source_manager = SourceManager::new();
        let (m1, m1_loc) = source_manager.add_fragment("BC 1");
        let (m2, m2_loc) = source_manager.add_fragment("B(x, y) (x && y)");
        let (main, main_loc) = source_manager.add_fragment("(A || B(0, 1)) && BC");

        let m1_tokens = lex_fragment(m1, &source_manager).unwrap();
        let m2_tokens = lex_fragment(m2, &source_manager).unwrap();
        let main_tokens = lex_fragment(main, &source_manager).unwrap();

        run(
            &main_tokens,
            &[
                Macro::parse(&m1_tokens, &[]).unwrap(),
                Macro::parse(&m2_tokens, &[]).unwrap(),
            ],
            "(A || (0 && 1)) && 1",
            &[
                LexToken(Token::LeftParen, main_loc.offset(0)),
                LexToken(Token::Id(Identifier("A".to_string())), main_loc.offset(1)),
                LexToken(Token::Whitespace, main_loc.offset(2)),
                LexToken(Token::VerticalBarVerticalBar, main_loc.offset(3)),
                LexToken(Token::Whitespace, main_loc.offset(5)),
                LexToken(Token::LeftParen, m2_loc.offset(8)),
                LexToken(Token::LiteralInt(0), main_loc.offset(8)),
                LexToken(Token::Whitespace, m2_loc.offset(10)),
                LexToken(Token::AmpersandAmpersand, m2_loc.offset(11)),
                LexToken(Token::Whitespace, m2_loc.offset(13)),
                LexToken(Token::LiteralInt(1), main_loc.offset(11)),
                LexToken(Token::RightParen, m2_loc.offset(15)),
                LexToken(Token::RightParen, main_loc.offset(13)),
                LexToken(Token::Whitespace, main_loc.offset(14)),
                LexToken(Token::AmpersandAmpersand, main_loc.offset(15)),
                LexToken(Token::Whitespace, main_loc.offset(17)),
                LexToken(Token::LiteralInt(1), m1_loc.offset(3)),
            ],
            &source_manager,
        );
    }
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

fn preprocess_command(
    buffer: &mut Vec<LexToken>,
    file_loader: &mut FileLoader,
    command: &[LexToken],
    file_id: FileId,
    macros: &mut Vec<Macro>,
    condition_chain: &mut ConditionChain,
) -> Result<(), PreprocessError> {
    let command_location = command[0].1;

    // Split the base command name
    let (command_name, command) = match command {
        [LexToken(Token::Id(id), _), rest @ ..] => (id.0.as_str(), rest),
        [LexToken(Token::If, _), rest @ ..] => ("if", rest),
        [LexToken(Token::Else, _), rest @ ..] => ("else", rest),
        _ => return Err(PreprocessError::UnknownCommand(command_location)),
    };

    let skip = !condition_chain.is_active();

    match command_name {
        "include" => {
            if skip {
                return Ok(());
            }
            let command = trim_whitespace(command);

            let file_name = match command {
                [LexToken(Token::LiteralString(s), _)] => s.clone(),
                [LexToken(Token::LeftAngleBracket(_), _), LexToken(Token::RightAngleBracket(_), _)] => {
                    return Err(PreprocessError::InvalidInclude(command_location))
                }
                [LexToken(Token::LeftAngleBracket(_), _), rest @ .., LexToken(Token::RightAngleBracket(_), end_loc)] =>
                {
                    let start_loc = rest[0].1;

                    // We do not have a token for header <> includes
                    // Attempt to reconstruct the original string by refetching the source range from the source manager

                    let (start_file, start_offset) = match file_loader
                        .source_manager
                        .get_file_offset_from_source_location(start_loc)
                    {
                        Some(ok) => ok,
                        None => return Err(PreprocessError::InvalidInclude(command_location)),
                    };

                    let (end_file, end_offset) = match file_loader
                        .source_manager
                        .get_file_offset_from_source_location(*end_loc)
                    {
                        Some(ok) => ok,
                        None => return Err(PreprocessError::InvalidInclude(command_location)),
                    };

                    if start_file != end_file || start_offset > end_offset {
                        return Err(PreprocessError::InvalidInclude(command_location));
                    }

                    let contents = file_loader.source_manager.get_contents(start_file);
                    let range_bytes =
                        &contents.as_bytes()[start_offset.0 as usize..end_offset.0 as usize];

                    let range = match std::str::from_utf8(range_bytes) {
                        Ok(range) => range,
                        Err(_) => return Err(PreprocessError::InvalidInclude(command_location)),
                    };

                    range.to_string()
                }
                _ => return Err(PreprocessError::InvalidInclude(command_location)),
            };

            // Include the file
            match file_loader.load(&file_name) {
                Ok(file) => {
                    preprocess_included_file(buffer, file_loader, file, macros, condition_chain)?;
                    Ok(())
                }
                Err(err) => Err(PreprocessError::FailedToFindFile(
                    command_location,
                    file_name.to_string(),
                    err,
                )),
            }
        }
        "ifdef" | "ifndef" => {
            if skip {
                condition_chain.push(ConditionState::DisabledInner);
                return Ok(());
            }
            let not = command_name == "ifndef";
            let command = trim_whitespace(command);
            if let [LexToken(Token::Id(id), _)] = command {
                let exists = macros.iter().any(|m| m.name == id.0);
                let active = if not { !exists } else { exists };
                condition_chain.push(if active {
                    ConditionState::Enabled
                } else {
                    ConditionState::DisabledInner
                });
                Ok(())
            } else if not {
                Err(PreprocessError::InvalidIfndef(command_location))
            } else {
                Err(PreprocessError::InvalidIfdef(command_location))
            }
        }
        "if" => {
            if skip {
                condition_chain.push(ConditionState::DisabledInner);
                return Ok(());
            }
            let command = trim_whitespace(command);
            let resolved = SubstitutedText::new(command)
                .apply_all(macros, true)?
                .resolve();

            let active = crate::condition_parser::parse(&resolved, command_location)?;
            condition_chain.push(if active {
                ConditionState::Enabled
            } else {
                ConditionState::DisabledInner
            });

            Ok(())
        }
        "elif" => {
            let command = trim_whitespace(command);
            let resolved = SubstitutedText::new(command)
                .apply_all(macros, true)?
                .resolve();

            let active = crate::condition_parser::parse(&resolved, command_location)?;
            condition_chain.switch(active)?;

            Ok(())
        }
        "else" => {
            let command = trim_whitespace_start(command);
            if !command.is_empty() {
                Err(PreprocessError::InvalidElse(command_location))
            } else {
                condition_chain.switch(true)?;
                Ok(())
            }
        }
        "endif" => {
            let command = trim_whitespace_start(command);
            if !command.is_empty() {
                Err(PreprocessError::InvalidEndIf(command_location))
            } else {
                condition_chain.pop()?;
                Ok(())
            }
        }
        "define" => {
            if skip {
                return Ok(());
            }

            let macro_def = Macro::parse(command, macros)?;
            macros.push(macro_def);

            Ok(())
        }
        "pragma" => {
            if skip {
                return Ok(());
            }
            let pragma_command = trim_whitespace(command);
            if let [LexToken(Token::Id(Identifier(s)), loc), ..] = pragma_command {
                match s.as_str() {
                    "once" => {
                        file_loader.mark_as_pragma_once(file_id);
                        Ok(())
                    }
                    "warning" => {
                        // Ignore warning disable commands for now
                        Ok(())
                    }
                    _ => Err(PreprocessError::UnknownPragma(*loc)),
                }
            } else {
                let pragma_command_location = match pragma_command.first() {
                    Some(tok) => tok.1,
                    None => SourceLocation::UNKNOWN,
                };
                Err(PreprocessError::UnknownPragma(pragma_command_location))
            }
        }
        _ if skip => Ok(()),
        _ => Err(PreprocessError::UnknownCommand(command_location)),
    }
}

/// Internal process a single file during preprocessing
fn preprocess_included_file(
    buffer: &mut Vec<LexToken>,
    file_loader: &mut FileLoader,
    input_file: InputFile,
    macros: &mut Vec<Macro>,
    condition_chain: &mut ConditionChain,
) -> Result<(), PreprocessError> {
    let file_source_location_base =
        file_loader.get_source_location_from_file_offset(input_file.file_id, StreamLocation(0));

    let input_tokens = match crate::lexer::lex(&input_file.contents, file_source_location_base) {
        Ok(tokens) => tokens,
        Err(err) => return Err(PreprocessError::LexerError(err)),
    };

    let mut stream = input_tokens.as_slice();
    loop {
        // Find the next region to process
        #[derive(PartialEq)]
        enum RegionType {
            None,
            Normal(usize),
            Command(usize, usize),
        }
        let mut region_type = RegionType::None;
        {
            let mut pos = 0;
            let mut at_start_of_line = true;
            while pos < stream.len() {
                let next = &stream[pos];
                if next.0 == Token::Endline {
                    assert!(
                        region_type == RegionType::None
                            || matches!(region_type,  RegionType::Normal(sz) if sz < pos + 1)
                    );
                    region_type = RegionType::Normal(pos + 1);
                    at_start_of_line = true;
                } else if at_start_of_line {
                    if next.0 == Token::Hash {
                        match region_type {
                            RegionType::None => {
                                // Get the size of the command from # too endline
                                // There should always be an endline at the end
                                let command_size = stream[pos..]
                                    .iter()
                                    .position(|t| t.0 == Token::Endline)
                                    .unwrap();

                                // Start the command range after the #
                                let start = pos + 1;

                                // Get the end position in the input stream
                                // This excludes the new line marker
                                let end = pos + command_size;
                                assert!(start < end);

                                region_type = RegionType::Command(start, end);
                                break;
                            }
                            RegionType::Normal(_) => {
                                break;
                            }
                            RegionType::Command(_, _) => unreachable!(),
                        }
                    } else if !next.0.is_whitespace() {
                        at_start_of_line = false;
                    }
                }
                pos += 1;
            }
        }

        match region_type {
            RegionType::None => {
                assert!(stream.is_empty());
                break;
            }
            RegionType::Normal(sz) => {
                if condition_chain.is_active() {
                    SubstitutedText::new(&stream[..sz])
                        .apply_all(macros, false)?
                        .store(buffer);
                }
                assert_ne!(sz, 0);
                stream = &stream[sz..];
            }
            RegionType::Command(start, end) => {
                preprocess_command(
                    buffer,
                    file_loader,
                    &stream[start..end],
                    input_file.file_id,
                    macros,
                    condition_chain,
                )?;

                assert_ne!(end, 0);
                assert_eq!(stream[end].0, Token::Endline);
                stream = &stream[end + 1..];
            }
        }
    }

    Ok(())
}

/// Preprocess a file after having set up the file loader
fn preprocess_initial_file(
    input_file: InputFile,
    file_loader: &mut FileLoader,
) -> Result<Vec<LexToken>, PreprocessError> {
    let mut tokens = Vec::<LexToken>::default();
    let mut macros = vec![];
    let mut condition_chain = ConditionChain::new();

    preprocess_included_file(
        &mut tokens,
        file_loader,
        input_file,
        &mut macros,
        &mut condition_chain,
    )?;

    if !condition_chain.0.is_empty() {
        return Err(PreprocessError::ConditionChainNotFinished);
    }

    Ok(tokens)
}

/// Preprocess a file - starting from memory
pub fn preprocess_direct(
    input: &str,
    file_name: FileName,
    source_manager: &mut SourceManager,
    include_handler: &mut dyn IncludeHandler,
) -> Result<Vec<LexToken>, PreprocessError> {
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
) -> Result<Vec<LexToken>, PreprocessError> {
    let mut file_loader = FileLoader::new(source_manager, include_handler);
    match file_loader.load(entry_file_name) {
        Ok(file) => preprocess_initial_file(file, &mut file_loader),
        Err(err) => Err(PreprocessError::FailedToFindFile(
            SourceLocation::UNKNOWN,
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
) -> Result<Vec<LexToken>, PreprocessError> {
    preprocess_direct(input, file_name, source_manager, &mut NullIncludeHandler)
}

/// Convert a stream of preprocessor tokens for parsing
pub fn prepare_tokens(source: &[LexToken]) -> Vec<LexToken> {
    let mut source = source
        .iter()
        .cloned()
        .filter(|t| !t.0.is_whitespace())
        .collect::<Vec<_>>();
    source.push(LexToken(Token::Eof, SourceLocation::UNKNOWN));
    source
}
