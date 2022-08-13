use crate::preprocess::PreprocessError;

pub fn parse(condition: &str) -> Result<bool, PreprocessError> {
    Ok(match condition {
        "0" => false,
        "1" => true,
        "!0" => true,
        "!1" => false,
        _ => {
            return Err(PreprocessError::FailedToParseIfCondition(
                condition.to_string(),
            ))
        }
    })
}

#[test]
fn test_condition_parser() {
    assert_eq!(parse("0").unwrap(), false);
    assert_eq!(parse("1").unwrap(), true);
    assert_eq!(parse("!0").unwrap(), true);
    assert_eq!(parse("!1").unwrap(), false);
}
