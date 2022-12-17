use rssl_text::tokens::*;
use rssl_text::*;

/// Transforms tokens back into a string
pub fn unlex(tokens: &[PreprocessToken], source_manager: &SourceManager) -> String {
    let mut output = String::new();
    for token in tokens {
        let start_location = token.get_location();
        let end_location = token.get_end_location();
        let file_location = source_manager.get_file_offset_from_source_location(start_location);
        if let Some((file_id, offset)) = file_location {
            // Source locations are expected to be from the same file and represent a contiguous range of source tokens
            let size = (end_location.get_raw() - start_location.get_raw()) as usize;

            #[cfg(debug_assertions)]
            {
                let (end_file_id, end_offset) = source_manager
                    .get_file_offset_from_source_location(end_location)
                    .unwrap();
                assert_eq!(file_id, end_file_id);
                assert_eq!(end_offset.0 - offset.0, size as u32);
            }

            let file_contents = source_manager.get_contents(file_id);
            let start = &file_contents.as_bytes()[offset.0 as usize..];
            if start.is_empty() && token.0 == Token::Endline {
                // Emit a normal line ending if we were the auto inserted line ending
                output.push('\n')
            } else {
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
            // Preprocessor tokens should all have locations
            // This should only occur if the locations are invalid and are not in the source manager
            panic!("unlex does not support unlocated tokens")
        }
    }
    output
}
