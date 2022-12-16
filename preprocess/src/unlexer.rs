use rssl_text::tokens::*;
use rssl_text::*;

/// Transforms tokens back into a string
pub fn unlex(tokens: &[LexToken], source_manager: &SourceManager) -> String {
    let mut output = String::new();
    for token in tokens {
        let location = token.1;
        let file_location = source_manager.get_file_offset_from_source_location(location);
        if let Some((file_id, offset)) = file_location {
            let file_contents = source_manager.get_contents(file_id);
            let start = &file_contents.as_bytes()[offset.0 as usize..];
            if start.is_empty() && token.0 == Token::Endline {
                // Emit a normal line ending if we were the auto inserted line ending
                output.push('\n')
            } else {
                let size = crate::lexer::get_next_token_size(start).unwrap();

                // Strip the \ from the non-logical line endings
                let range = if token.0 == Token::PhysicalEndline {
                    &start[1..size]
                } else {
                    &start[..size]
                };

                let original_string = std::str::from_utf8(range).unwrap();
                output.push_str(original_string);
            }
        } else {
            panic!("unlex does not support unlocated tokens")
        }
    }
    output
}
