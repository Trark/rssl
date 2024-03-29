use crate::lexer::{LexerError, TokenStream};
use crate::unlex;
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
    InvalidUndef(SourceLocation),
    MacroRequiresArguments(String),
    MacroArgumentsNeverEnd,
    MacroExpectsDifferentNumberOfArguments,
    ConcatMissingLeftToken(SourceLocation),
    ConcatMissingRightToken(SourceLocation),
    ConcatFailed(SourceLocation),
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

impl CompileError for PreprocessError {
    fn print(&self, w: &mut MessagePrinter) -> std::fmt::Result {
        match self {
            PreprocessError::LexerError(err) => err.print(w),
            PreprocessError::UnknownCommand(loc) => w.write_message(
                &|f| write!(f, "unknown preprocessing directive"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::UnknownPragma(loc) => {
                w.write_message(&|f| write!(f, "unknown pragma"), *loc, Severity::Error)
            }
            PreprocessError::InvalidInclude(loc) => w.write_message(
                &|f| write!(f, "invalid #include command"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::InvalidDefine(loc) => w.write_message(
                &|f| write!(f, "invalid #define command"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::InvalidUndef(loc) => w.write_message(
                &|f| write!(f, "invalid #undef command"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::MacroRequiresArguments(s) => w.write_message(
                &|f| write!(f, "macro function '{s}' requires arguments"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            PreprocessError::MacroArgumentsNeverEnd => w.write_message(
                &|f| write!(f, "expected end of macro arguments"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            PreprocessError::MacroExpectsDifferentNumberOfArguments => w.write_message(
                &|f| write!(f, "macro requires different number of arguments"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            PreprocessError::ConcatMissingLeftToken(loc) => w.write_message(
                &|f| write!(f, "no token on left of ##"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::ConcatMissingRightToken(loc) => w.write_message(
                &|f| write!(f, "no token on right of ##"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::ConcatFailed(loc) => w.write_message(
                &|f| write!(f, "failed to merge tokens with ##"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::FailedToFindFile(loc, name, _) => w.write_message(
                &|f| write!(f, "failed to load file: '{name}'"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::FailedToParseIfCondition(loc) => w.write_message(
                &|f| write!(f, "#if condition parser failed"),
                *loc,
                Severity::Error,
            ),
            PreprocessError::InvalidIfdef(loc) => {
                w.write_message(&|f| write!(f, "invalid #ifdef"), *loc, Severity::Error)
            }
            PreprocessError::InvalidIfndef(loc) => {
                w.write_message(&|f| write!(f, "invalid #ifndef"), *loc, Severity::Error)
            }
            PreprocessError::InvalidElse(loc) => {
                w.write_message(&|f| write!(f, "invalid #else"), *loc, Severity::Error)
            }
            PreprocessError::InvalidEndIf(loc) => {
                w.write_message(&|f| write!(f, "invalid #endif"), *loc, Severity::Error)
            }
            PreprocessError::ConditionChainNotFinished => w.write_message(
                &|f| write!(f, "not enough #endif's encountered"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            PreprocessError::ElseNotMatched => w.write_message(
                &|f| write!(f, "encountered #else but with no matching #if"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            PreprocessError::EndIfNotMatched => w.write_message(
                &|f| write!(f, "rncountered #endif but with no matching #if"),
                SourceLocation::UNKNOWN,
                Severity::Error,
            ),
            PreprocessError::PragmaOnceInUnknownFile => w.write_message(
                &|f| write!(f, "encountered #pragma once in an unknown file"),
                SourceLocation::UNKNOWN,
                Severity::Error,
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

    fn load(
        &mut self,
        file_name: &str,
        parent_file: Option<FileId>,
    ) -> Result<InputFile, IncludeError> {
        let parent_name = match parent_file {
            Some(id) => self.source_manager.get_file_name(id),
            None => "",
        };

        let id = match self.file_name_remap.get(file_name) {
            Some(id) => *id,
            None => {
                // Load the file
                let file_data = self.include_handler.load(file_name, parent_name)?;

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
struct Macro {
    name: String,
    is_function: bool,
    num_params: u64,
    tokens: Vec<PreprocessToken>,
    location: SourceLocation,
}

impl Macro {
    fn parse(command: &[PreprocessToken]) -> Result<Macro, PreprocessError> {
        let command = trim_whitespace_start(command);

        let location = command.first().get_location();

        // Consume define name
        let (name, location, signature_and_body) =
            if let Some((PreprocessToken(Token::Id(id), ext), rest)) = command.split_first() {
                (id.0.clone(), ext.get_location(), rest)
            } else {
                return Err(PreprocessError::InvalidDefine(location));
            };

        // Consume macro parameters from body
        let (param_tokens, is_function, body) =
            if let Some((PreprocessToken(Token::LeftParen, _), rest)) =
                signature_and_body.split_first()
            {
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
            let mut last = false;
            while !last {
                let next_param =
                    if let Some(pos) = param_tokens.iter().position(|t| t.0 == Token::Comma) {
                        let param = &param_tokens[..pos];
                        param_tokens = &param_tokens[pos + 1..];
                        param
                    } else {
                        let param = param_tokens;
                        param_tokens = &[][..];
                        last = true;
                        param
                    };

                let next_param = trim_whitespace(next_param);

                // We expect a single name for all macro arguments
                // But a macro with zero arguments will have one empty item processed
                if let [PreprocessToken(Token::Id(id), _)] = next_param {
                    params.push(id.0.clone());
                } else if !(params.is_empty() && next_param.is_empty() && last) {
                    return Err(PreprocessError::InvalidDefine(location));
                }
            }
        }

        // Remove whitespace at start and end of the body of the define
        let body = trim_whitespace(body);

        // Replace identifiers to parameters with argument reference tokens
        let tokens = body
            .iter()
            .map(|t| {
                if let Token::Id(id) = &t.0 {
                    for (i, param) in params.iter().enumerate() {
                        if id.0 == *param {
                            return PreprocessToken(Token::MacroArg(i as u32), t.1.clone());
                        }
                    }
                } else if let Token::HashHash = &t.0 {
                    // Switch out normal tokens for ## with a special version when in a macro body
                    // This means we can distinguish ## from macro arguments from ## in the body
                    // Concatenation is only triggered from ## in the body - not the args
                    return PreprocessToken(Token::Concat, t.1.clone());
                }
                t.clone()
            })
            .collect::<Vec<_>>();

        let def = Macro {
            name,
            is_function,
            num_params: params.len() as u64,
            tokens,
            location,
        };

        Ok(def)
    }
}

/// Remove whitespace and comments from the start of a token stream - but not endlines
fn trim_whitespace_start(mut tokens: &[PreprocessToken]) -> &[PreprocessToken] {
    while let Some((PreprocessToken(tok, _), rest)) = tokens.split_first() {
        if tok.is_whitespace() && *tok != Token::Endline {
            tokens = rest;
        } else {
            break;
        }
    }
    tokens
}

/// Remove whitespace and comments from the end of a token stream - but not endlines
fn trim_whitespace_end(mut tokens: &[PreprocessToken]) -> &[PreprocessToken] {
    while let Some((PreprocessToken(tok, _), rest)) = tokens.split_last() {
        if tok.is_whitespace() && *tok != Token::Endline {
            tokens = rest;
        } else {
            break;
        }
    }
    tokens
}

/// Remove whitespace and comments from the start and end of a token stream - but not endlines
fn trim_whitespace(tokens: &[PreprocessToken]) -> &[PreprocessToken] {
    trim_whitespace_end(trim_whitespace_start(tokens))
}

fn split_macro_args<'stream>(
    macro_name: &str,
    remaining: &'stream [PreprocessToken],
) -> Result<(&'stream [PreprocessToken], Vec<&'stream [PreprocessToken]>), PreprocessError> {
    // Consume the starting bracket
    let remaining = trim_whitespace_start(remaining);
    let mut remaining = if let [PreprocessToken(Token::LeftParen, _), rest @ ..] = remaining {
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

/// Apply all macros and special functions to a block of tokens
fn apply_macros(
    tokens: &[PreprocessToken],
    macro_defs: &[Macro],
    apply_defined: bool,
    source_manager: &mut SourceManager,
) -> Result<Vec<PreprocessToken>, PreprocessError> {
    let mut macro_disabled = vec![false; macro_defs.len()];
    apply_macros_internal(
        tokens.to_vec(),
        macro_defs,
        &mut macro_disabled,
        apply_defined,
        source_manager,
    )
}

struct MacroSearchPosition {
    /// The main next position we will be parsing from
    next_pos: usize,

    /// The position we can search for macro function names from
    /// As long as the arguments are within next_pos we can invoke it
    early_function_pos: usize,

    /// The last function we applied - this can not be used in the early search region
    last_macro_function_index: usize,
}

/// Internal version of apply_macros that has macro disabled states
fn apply_macros_internal(
    tokens: Vec<PreprocessToken>,
    macro_defs: &[Macro],
    macro_disabled: &mut [bool],
    apply_defined: bool,
    source_manager: &mut SourceManager,
) -> Result<Vec<PreprocessToken>, PreprocessError> {
    // Make a vec from the source range which we will modify in place
    let mut tokens = tokens.to_vec();

    // Process the text one operation at a time - starting at the last applied point
    let mut pos = MacroSearchPosition {
        next_pos: 0,
        early_function_pos: 0,
        last_macro_function_index: usize::MAX,
    };
    while pos.next_pos < tokens.len() {
        // Apply the next operation we can find
        pos = apply_single_macro(
            &mut tokens,
            pos,
            macro_defs,
            macro_disabled,
            apply_defined,
            source_manager,
        )?;
    }
    Ok(tokens)
}

/// Attempt to replace the next macro in a region of text
fn apply_single_macro(
    tokens: &mut Vec<PreprocessToken>,
    search_pos: MacroSearchPosition,
    macro_defs: &[Macro],
    macro_disabled: &mut [bool],
    apply_defined: bool,
    source_manager: &mut SourceManager,
) -> Result<MacroSearchPosition, PreprocessError> {
    // Find the first macro or special operation that matches the text
    let found = find_single_macro(
        tokens,
        search_pos,
        macro_defs,
        macro_disabled,
        apply_defined,
    )?;
    match found {
        FoundMacro::User(macro_index, pos) => {
            let macro_def = &macro_defs[macro_index];

            // Read macro arguments
            let mut remaining = &tokens[pos + 1..];
            let args = if macro_def.is_function {
                let (rest, args) = split_macro_args(&macro_def.name, remaining)?;
                remaining = rest;

                if macro_def.num_params == 0 {
                    if !(args.len() == 1 && args[0].is_empty()) {
                        return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                    }
                } else if args.len() as u64 != macro_def.num_params {
                    return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                }

                args
            } else {
                Vec::new()
            };
            let end = tokens.len() - remaining.len();

            // Substitute macros inside macro arguments
            let args = args.into_iter().try_fold(Vec::new(), |mut vec, arg| {
                let subbed_text = apply_macros(arg, macro_defs, false, source_manager)?;
                vec.push(subbed_text);
                Ok(vec)
            })?;

            // Generate macro body into a local array
            let mut output = Vec::with_capacity(macro_def.tokens.len());
            for token in &macro_def.tokens {
                if let Token::MacroArg(i) = token.0 {
                    // If we are a macro arg then replace the token with the argument
                    output.extend_from_slice(&args[i as usize])
                } else {
                    // If we are a normal token then copy it across without modification
                    output.push(token.clone());
                }
            }

            // Suppress the current macro
            assert!(!macro_disabled[macro_index]);
            macro_disabled[macro_index] = true;

            // Apply macros to the inner region again
            let output =
                apply_macros_internal(output, macro_defs, macro_disabled, false, source_manager)?;

            // Enable the current macro
            assert!(macro_disabled[macro_index]);
            macro_disabled[macro_index] = false;

            assert!(end > pos);
            let tokens_added = output.len();

            // Replace the section of tokens
            tokens.splice(pos..end, output);

            let new_end = pos + tokens_added;

            // Continue parsing after the replaced tokens
            // With the exception that we start searching in the replaced region but only accept function invocations that reach into the next region
            // And which are for a different macro function than we just invoked
            Ok(MacroSearchPosition {
                next_pos: new_end,
                early_function_pos: pos,
                last_macro_function_index: if macro_def.is_function {
                    macro_index
                } else {
                    usize::MAX
                },
            })
        }
        FoundMacro::Defined(pos) => {
            // Read argument
            let mut remaining = &tokens[pos + 1..];
            let arg = {
                // Allow "defined A" instead of traditional defined(A)
                let remaining_trimmed = trim_whitespace_start(remaining);
                match remaining_trimmed {
                    [PreprocessToken(arg @ Token::Id(_), _), rest @ ..]
                        if remaining.len() != remaining_trimmed.len() =>
                    {
                        remaining = rest;
                        arg
                    }
                    _ => {
                        // Parse arguments like a normal macro called defined
                        let (rest, args) = split_macro_args("defined", remaining)?;
                        remaining = rest;

                        // Expect one argument
                        if args.len() as u64 != 1 {
                            return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                        }

                        // Expect a single name as the argument
                        if let [PreprocessToken(arg @ Token::Id(_), _)] = args[0] {
                            arg
                        } else {
                            return Err(PreprocessError::MacroExpectsDifferentNumberOfArguments);
                        }
                    }
                }
            };

            // We are about to make a fake token for the result
            // This will take the start location from the start of the "defined" token
            let start_location = tokens[pos].get_location();

            // There must be at least one token after the "defined" - as it requires arguments
            // The end location will be at the end of the arguments
            let end_location = tokens[tokens.len() - remaining.len() - 1].get_end_location();

            // All the argument tokens should be from the same file as the defined command is executed immediatly in a preprocessor command
            // This means constructing the range between the start and end location should be contiguous
            let location_size = end_location.get_raw() - start_location.get_raw();

            let exists = macro_defs.iter().any(|m| {
                if let Token::Id(id) = arg {
                    m.name == id.0
                } else {
                    false
                }
            });

            let generated_token = if exists {
                Token::LiteralInt(1)
            } else {
                Token::LiteralInt(0)
            };

            let output = Vec::from([PreprocessToken::new(
                generated_token,
                start_location,
                0,
                location_size,
            )]);

            let end = tokens.len() - remaining.len();

            // Replace the section of tokens
            tokens.splice(pos..end, output);

            // Continue from after the generated token
            Ok(MacroSearchPosition {
                next_pos: pos + 1,
                early_function_pos: pos + 1,
                last_macro_function_index: usize::MAX,
            })
        }
        FoundMacro::Concat(left_token_pos, right_token_pos) => {
            let left_token = &tokens[left_token_pos];
            let right_token = &tokens[right_token_pos];
            assert!(left_token_pos + 1 < right_token_pos);

            // Emit original strings from the tokens
            let left_string = unlex(std::slice::from_ref(left_token), source_manager);
            let right_string = unlex(std::slice::from_ref(right_token), source_manager);

            // Combine the strings
            let new_fragment = format!("{left_string}{right_string}");

            // Register the combined string as a file
            let file_id =
                source_manager.add_file(FileName("<scratch space>".to_string()), new_fragment);
            let source_location =
                source_manager.get_source_location_from_file_offset(file_id, StreamLocation(0));

            // String is now owned by the source manager - fetch a reference to it
            let new_fragment = source_manager.get_contents(file_id);

            // Lex the new combined string file
            let merged_token = match TokenStream::new(new_fragment, source_location).read_to_end() {
                Ok(tokens) => tokens,
                Err(_) => return Err(PreprocessError::ConcatFailed(source_location)),
            };

            // We expect a single token result with a new line marker after it
            let output =
                if let [token, PreprocessToken(Token::Endline, _)] = merged_token.as_slice() {
                    // Insert the new combined token into the output stream
                    Vec::from([token.clone()])
                } else {
                    return Err(PreprocessError::ConcatFailed(source_location));
                };

            // Replace the section of tokens
            tokens.splice(left_token_pos..=right_token_pos, output);

            // Continue from the new token - so the new token may also invoke a macro
            Ok(MacroSearchPosition {
                next_pos: left_token_pos,
                early_function_pos: left_token_pos,
                last_macro_function_index: usize::MAX,
            })
        }
        FoundMacro::None => Ok(MacroSearchPosition {
            next_pos: tokens.len(),
            early_function_pos: tokens.len(),
            last_macro_function_index: usize::MAX,
        }),
    }
}

/// Possible operations we can find to apply to a stream of tokens
enum FoundMacro {
    /// Found a normal macro
    User(usize, usize),

    /// Found a defined() operation
    Defined(usize),

    /// Found a ## operation
    Concat(usize, usize),

    /// Nothing found
    None,
}

/// Find the next instance of the named macro in a stream of tokens
fn find_single_macro(
    tokens: &[PreprocessToken],
    search_pos: MacroSearchPosition,
    macros: &[Macro],
    macro_disabled: &mut [bool],
    apply_defined: bool,
) -> Result<FoundMacro, PreprocessError> {
    assert!(search_pos.early_function_pos <= search_pos.next_pos);
    let mut i = search_pos.early_function_pos;
    while i < tokens.len() {
        if let Token::Id(id) = &tokens[i].0 {
            if i >= search_pos.next_pos && apply_defined && id.0 == "defined" {
                return Ok(FoundMacro::Defined(i));
            }

            for macro_index in 0..macros.len() {
                if macro_disabled[macro_index] {
                    continue;
                }
                if search_pos.last_macro_function_index == macro_index && i < search_pos.next_pos {
                    continue;
                }
                let macro_def = &macros[macro_index];
                if id.0 == macro_def.name {
                    let mut activate_pos = i;

                    // Check we have the start of function parameters
                    if macro_def.is_function {
                        let trimmed = trim_whitespace_start(&tokens[i + 1..]);
                        activate_pos = tokens.len() - trimmed.len();
                        let [PreprocessToken(Token::LeftParen, _), ..] = trimmed else {
                            continue;
                        };
                    }

                    if activate_pos < search_pos.next_pos {
                        continue;
                    }

                    return Ok(FoundMacro::User(macro_index, i));
                }
            }
        } else if let Token::Concat = &tokens[i].0 {
            if i < search_pos.next_pos {
                continue;
            }

            let concat_token = &tokens[i];

            // Get the left token to concat
            let left_token_pos = if let Some(left_position) =
                tokens[..i].iter().rev().position(|t| !t.0.is_whitespace())
            {
                i - left_position - 1
            } else {
                return Err(PreprocessError::ConcatMissingLeftToken(
                    concat_token.get_location(),
                ));
            };

            // Get the right token to concat
            let right_token_pos = if let Some(right_position) =
                tokens[i + 1..].iter().position(|t| !t.0.is_whitespace())
            {
                i + right_position + 1
            } else {
                return Err(PreprocessError::ConcatMissingRightToken(
                    concat_token.get_location(),
                ));
            };

            return Ok(FoundMacro::Concat(left_token_pos, right_token_pos));
        }
        i += 1;
    }
    Ok(FoundMacro::None)
}

#[test]
fn macro_from_definition() {
    fn ll(s: &str, source_manager: &mut SourceManager) -> Vec<PreprocessToken> {
        let (file_id, source_location) = source_manager.add_fragment(s);
        assert_eq!(source_location, SourceLocation::first());
        crate::lexer::lex_fragment(file_id, source_manager).unwrap()
    }

    {
        let mut source_manager = SourceManager::new();
        assert_eq!(
            Macro::parse(&ll("B 0", &mut source_manager)).unwrap(),
            Macro {
                name: "B".to_string(),
                is_function: false,
                num_params: 0,
                tokens: Vec::from([PreprocessToken::new(
                    Token::LiteralInt(0),
                    SourceLocation::first(),
                    2,
                    3
                )]),
                location: SourceLocation::first(),
            }
        );
    }

    {
        let mut source_manager = SourceManager::new();
        assert_eq!(
            Macro::parse(&ll("B(x) x", &mut source_manager)).unwrap(),
            Macro {
                name: "B".to_string(),
                is_function: true,
                num_params: 1,
                tokens: Vec::from([PreprocessToken::new(
                    Token::MacroArg(0),
                    SourceLocation::first(),
                    5,
                    6,
                )]),
                location: SourceLocation::first(),
            }
        );
    }

    {
        let mut source_manager = SourceManager::new();
        assert_eq!(
            Macro::parse(&ll("B(x,y) x", &mut source_manager)).unwrap(),
            Macro {
                name: "B".to_string(),
                is_function: true,
                num_params: 2,
                tokens: Vec::from([PreprocessToken::new(
                    Token::MacroArg(0),
                    SourceLocation::first(),
                    7,
                    8,
                )]),
                location: SourceLocation::first(),
            }
        );
    }

    {
        let mut source_manager = SourceManager::new();
        assert_eq!(
            Macro::parse(&ll("B(x,y) y", &mut source_manager)).unwrap(),
            Macro {
                name: "B".to_string(),
                is_function: true,
                num_params: 2,
                tokens: Vec::from([PreprocessToken::new(
                    Token::MacroArg(1),
                    SourceLocation::first(),
                    7,
                    8,
                )]),
                location: SourceLocation::first(),
            }
        );
    }

    {
        let mut source_manager = SourceManager::new();
        assert_eq!(
            Macro::parse(&ll("B(x,xy) (x || xy)", &mut source_manager)).unwrap(),
            Macro {
                name: "B".to_string(),
                is_function: true,
                num_params: 2,
                tokens: Vec::from([
                    PreprocessToken::new(Token::LeftParen, SourceLocation::first(), 8, 9),
                    PreprocessToken::new(Token::MacroArg(0), SourceLocation::first(), 9, 10),
                    PreprocessToken::new(Token::Whitespace, SourceLocation::first(), 10, 11),
                    PreprocessToken::new(
                        Token::VerticalBarVerticalBar,
                        SourceLocation::first(),
                        11,
                        13
                    ),
                    PreprocessToken::new(Token::Whitespace, SourceLocation::first(), 13, 14),
                    PreprocessToken::new(Token::MacroArg(1), SourceLocation::first(), 14, 16),
                    PreprocessToken::new(Token::RightParen, SourceLocation::first(), 16, 17),
                ]),
                location: SourceLocation::first(),
            }
        );
    }
}

#[test]
fn macro_resolve() {
    use crate::lexer::lex_fragment;
    use crate::unlexer::unlex;

    #[track_caller]
    fn run(
        input: &[PreprocessToken],
        macros: &[Macro],
        expected_str: &str,
        expected_tokens: &[PreprocessToken],
        source_manager: &mut SourceManager,
    ) {
        let resolved_tokens = apply_macros(input, macros, false, source_manager).unwrap();
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
                Macro::parse(&m1_tokens).unwrap(),
                Macro::parse(&m2_tokens).unwrap(),
            ],
            "(A || 0) && 1",
            &[
                PreprocessToken::new(Token::LeftParen, main_loc, 0, 1),
                PreprocessToken::new(Token::Id(Identifier("A".to_string())), main_loc, 1, 2),
                PreprocessToken::new(Token::Whitespace, main_loc, 2, 3),
                PreprocessToken::new(Token::VerticalBarVerticalBar, main_loc, 3, 5),
                PreprocessToken::new(Token::Whitespace, main_loc, 5, 6),
                PreprocessToken::new(Token::LiteralInt(0), m1_loc, 2, 3),
                PreprocessToken::new(Token::RightParen, main_loc, 7, 8),
                PreprocessToken::new(Token::Whitespace, main_loc, 8, 9),
                PreprocessToken::new(Token::AmpersandAmpersand, main_loc, 9, 11),
                PreprocessToken::new(Token::Whitespace, main_loc, 11, 12),
                PreprocessToken::new(Token::LiteralInt(1), m2_loc, 3, 4),
            ],
            &mut source_manager,
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
                Macro::parse(&m1_tokens).unwrap(),
                Macro::parse(&m2_tokens).unwrap(),
            ],
            "(A || (0 && 1)) && 1",
            &[
                PreprocessToken::new(Token::LeftParen, main_loc, 0, 1),
                PreprocessToken::new(Token::Id(Identifier("A".to_string())), main_loc, 1, 2),
                PreprocessToken::new(Token::Whitespace, main_loc, 2, 3),
                PreprocessToken::new(Token::VerticalBarVerticalBar, main_loc, 3, 5),
                PreprocessToken::new(Token::Whitespace, main_loc, 5, 6),
                PreprocessToken::new(Token::LeftParen, m2_loc, 8, 9),
                PreprocessToken::new(Token::LiteralInt(0), main_loc, 8, 9),
                PreprocessToken::new(Token::Whitespace, m2_loc, 10, 11),
                PreprocessToken::new(Token::AmpersandAmpersand, m2_loc, 11, 13),
                PreprocessToken::new(Token::Whitespace, m2_loc, 13, 14),
                PreprocessToken::new(Token::LiteralInt(1), main_loc, 11, 12),
                PreprocessToken::new(Token::RightParen, m2_loc, 15, 16),
                PreprocessToken::new(Token::RightParen, main_loc, 13, 14),
                PreprocessToken::new(Token::Whitespace, main_loc, 14, 15),
                PreprocessToken::new(Token::AmpersandAmpersand, main_loc, 15, 17),
                PreprocessToken::new(Token::Whitespace, main_loc, 17, 18),
                PreprocessToken::new(Token::LiteralInt(1), m1_loc, 3, 4),
            ],
            &mut source_manager,
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
    buffer: &mut Vec<PreprocessToken>,
    file_loader: &mut FileLoader,
    command: &[PreprocessToken],
    file_id: FileId,
    macros: &mut Vec<Macro>,
    condition_chain: &mut ConditionChain,
) -> Result<(), PreprocessError> {
    let command_location = command[0].get_location();

    // Split the base command name
    let (command_name, command) = match command {
        [PreprocessToken(Token::Id(id), _), rest @ ..] => (id.0.as_str(), rest),
        [PreprocessToken(Token::If, _), rest @ ..] => ("if", rest),
        [PreprocessToken(Token::Else, _), rest @ ..] => ("else", rest),
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
                [PreprocessToken(Token::LiteralString(s), _)] => s.clone(),
                [PreprocessToken(Token::HeaderName(s), _)] => s.clone(),
                _ => return Err(PreprocessError::InvalidInclude(command_location)),
            };

            // Include the file
            match file_loader.load(&file_name, Some(file_id)) {
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
            if let [PreprocessToken(Token::Id(id), _)] = command {
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
            let resolved = apply_macros(command, macros, true, file_loader.source_manager)?;
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
            let resolved = apply_macros(command, macros, true, file_loader.source_manager)?;
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

            let macro_def = Macro::parse(command)?;

            // Remove any existing macros with the same name
            macros.retain(|m| m.name != macro_def.name);

            macros.push(macro_def);

            Ok(())
        }
        "undef" => {
            if skip {
                return Ok(());
            }

            let command = trim_whitespace(command);

            if let [PreprocessToken(Token::Id(Identifier(s)), _)] = command {
                let previous_count = macros.len();
                macros.retain(|m| m.name != *s);
                let current_count = macros.len();

                if previous_count == current_count {
                    // No macro removed
                    Ok(())
                } else {
                    assert_eq!(current_count + 1, previous_count);
                    Ok(())
                }
            } else {
                Err(PreprocessError::InvalidUndef(command_location))
            }
        }
        "pragma" => {
            if skip {
                return Ok(());
            }
            let pragma_command = trim_whitespace(command);
            if let [PreprocessToken(Token::Id(Identifier(s)), ext), ..] = pragma_command {
                match s.as_str() {
                    "once" => {
                        file_loader.mark_as_pragma_once(file_id);
                        Ok(())
                    }
                    "warning" => {
                        // Ignore warning disable commands for now
                        Ok(())
                    }
                    _ => Err(PreprocessError::UnknownPragma(ext.get_location())),
                }
            } else {
                Err(PreprocessError::UnknownPragma(
                    pragma_command.first().get_location(),
                ))
            }
        }
        _ if skip => Ok(()),
        _ => Err(PreprocessError::UnknownCommand(command_location)),
    }
}

/// Internal process a single file during preprocessing
fn preprocess_included_file(
    buffer: &mut Vec<PreprocessToken>,
    file_loader: &mut FileLoader,
    input_file: InputFile,
    macros: &mut Vec<Macro>,
    condition_chain: &mut ConditionChain,
) -> Result<(), PreprocessError> {
    let file_source_location_base =
        file_loader.get_source_location_from_file_offset(input_file.file_id, StreamLocation(0));

    let mut token_stream = TokenStream::new(&input_file.contents, file_source_location_base);

    let mut active_tokens: Vec<PreprocessToken> = Vec::new();

    fn flush_normal(
        output_tokens: &mut Vec<PreprocessToken>,
        input_tokens: &mut Vec<PreprocessToken>,
        file_loader: &mut FileLoader,
        macros: &[Macro],
        condition_chain: &mut ConditionChain,
    ) -> Result<(), PreprocessError> {
        if condition_chain.is_active() {
            let next_tokens =
                apply_macros(input_tokens, macros, false, file_loader.source_manager)?;
            output_tokens.extend(next_tokens);
        }
        input_tokens.clear();
        Ok(())
    }

    // Find the next region to process
    enum CommandParseState {
        StartOfLine,
        CommandStart,
        CommandContents,
        NormalContents,
    }
    let mut command_state = CommandParseState::StartOfLine;
    let mut inside_include = false;
    while !token_stream.end_of_stream() {
        // Load the next token
        let next = match token_stream.next(inside_include) {
            Ok(next) => next,
            Err(err) => return Err(PreprocessError::LexerError(err)),
        };

        match (&next.0, &command_state) {
            (Token::Endline, CommandParseState::CommandContents) => {
                preprocess_command(
                    buffer,
                    file_loader,
                    &active_tokens,
                    input_file.file_id,
                    macros,
                    condition_chain,
                )?;

                active_tokens.clear();
                command_state = CommandParseState::StartOfLine;
                inside_include = false;
            }
            (Token::Endline, _) => {
                command_state = CommandParseState::StartOfLine;
                active_tokens.push(next)
            }
            (Token::Hash, CommandParseState::StartOfLine) => {
                // Remove whitespace in active tokens between start of line and #
                loop {
                    match active_tokens.last() {
                        Some(tok) if tok.0 != Token::Endline && tok.0.is_whitespace() => {
                            active_tokens.pop();
                        }
                        _ => break,
                    }
                }

                flush_normal(
                    buffer,
                    &mut active_tokens,
                    file_loader,
                    macros,
                    condition_chain,
                )?;

                command_state = CommandParseState::CommandStart;
            }
            (tok, CommandParseState::CommandStart) if !tok.is_whitespace() => {
                command_state = CommandParseState::CommandContents;

                // Enable special lex mode for #include
                if let Token::Id(id) = tok {
                    if id.0 == "include" {
                        inside_include = true;
                    }
                }

                // Discard the # and any whitespace between the # and command name
                active_tokens.clear();
                active_tokens.push(next);
            }
            (tok, CommandParseState::StartOfLine) => {
                if !tok.is_whitespace() {
                    command_state = CommandParseState::NormalContents;
                }
                active_tokens.push(next)
            }
            _ => active_tokens.push(next),
        };
    }

    flush_normal(
        buffer,
        &mut active_tokens,
        file_loader,
        macros,
        condition_chain,
    )?;

    Ok(())
}

/// Preprocess a file after having set up the file loader
fn preprocess_initial_file(
    input_file: InputFile,
    file_loader: &mut FileLoader,
    initial_defines: &[(&str, &str)],
) -> Result<Vec<PreprocessToken>, PreprocessError> {
    let mut tokens = Vec::<PreprocessToken>::default();
    let mut macros = Vec::new();
    let mut condition_chain = ConditionChain::new();

    // Add initial macros
    for (name, value) in initial_defines {
        let tokens = match TokenStream::new(value, SourceLocation::UNKNOWN)
            .suppress_trailing_endline()
            .read_to_end()
        {
            Ok(tokens) => tokens,
            Err(_) => return Err(PreprocessError::InvalidDefine(SourceLocation::UNKNOWN)),
        };

        macros.push(Macro {
            name: name.to_string(),
            is_function: false,
            num_params: 0,
            tokens,
            location: SourceLocation::UNKNOWN,
        });
    }

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

/// Preprocess a file - starting from include handler
pub fn preprocess(
    entry_file_name: &str,
    source_manager: &mut SourceManager,
    include_handler: &mut dyn IncludeHandler,
    initial_defines: &[(&str, &str)],
) -> Result<Vec<PreprocessToken>, PreprocessError> {
    let mut file_loader = FileLoader::new(source_manager, include_handler);
    match file_loader.load(entry_file_name, None) {
        Ok(file) => preprocess_initial_file(file, &mut file_loader, initial_defines),
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
) -> Result<Vec<PreprocessToken>, PreprocessError> {
    let mut files = [(file_name.0.as_ref(), input)];
    preprocess(
        &file_name.0,
        source_manager,
        &mut files,
        // We mirror the semantics of HLSL 2021 in main output - so set the define in test fragments as well
        &[("__HLSL_VERSION", "2021")],
    )
}

/// Convert a stream of preprocessor tokens for parsing
pub fn prepare_tokens(source: &[PreprocessToken]) -> Vec<LexToken> {
    let mut source = source
        .iter()
        .cloned()
        .filter_map(|t| {
            assert!(!matches!(t.0, Token::MacroArg(_)));
            if t.0.is_whitespace() {
                None
            } else {
                let loc = t.get_location();
                Some(LexToken(t.0, loc))
            }
        })
        .collect::<Vec<_>>();
    source.push(LexToken(Token::Eof, SourceLocation::UNKNOWN));
    source
}
