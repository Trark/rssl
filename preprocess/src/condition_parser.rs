use crate::preprocess::PreprocessError;
use rssl_text::tokens::Token;

/// Parse a condition for an #if and return if it passes
pub fn parse(condition: &str) -> Result<bool, PreprocessError> {
    // Throw the line into the lexer to share all the literal, token, and whitespace parsing logic
    let tokens = match rssl_lexer::minilex(condition) {
        Ok(tokens) => tokens,
        Err(_) => {
            return Err(PreprocessError::FailedToParseIfCondition(
                condition.to_string(),
            ))
        }
    };

    match parse_p15(&tokens) {
        Ok((&[], value)) => Ok(value != 0),
        Ok((_, _)) => Err(PreprocessError::FailedToParseIfCondition(
            condition.to_string(),
        )),
        Err(_) => Err(PreprocessError::FailedToParseIfCondition(
            condition.to_string(),
        )),
    }
}

/// Type used for accumulating condition values
type ConditionValue = u64;

/// Error type for internal condition parsing errors
struct ConditionParseError;

fn parse_p15(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    let left = parse_p14(stream)?;
    let (stream, left_value) = left;

    if let Some((Token::VerticalBarVerticalBar, rest)) = stream.split_first() {
        let (rest, right_value) = parse_p14(rest)?;
        let value = u64::from(left_value != 0 || right_value != 0);
        return Ok((rest, value));
    }

    Ok(left)
}

fn parse_p14(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    let left = parse_p10(stream)?;
    let (stream, left_value) = left;

    if let Some((Token::AmpersandAmpersand, rest)) = stream.split_first() {
        let (rest, right_value) = parse_p10(rest)?;
        let value = u64::from(left_value != 0 && right_value != 0);
        return Ok((rest, value));
    }

    Ok(left)
}

fn parse_p10(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    let left = parse_p3(stream)?;
    let (stream, left_value) = left;

    if let Some((tok, rest)) = stream.split_first() {
        match tok {
            Token::EqualsEquals => {
                let (rest, right_value) = parse_p3(rest)?;
                let value = u64::from(left_value == right_value);
                return Ok((rest, value));
            }
            Token::ExclamationPointEquals => {
                let (rest, right_value) = parse_p3(rest)?;
                let value = u64::from(left_value != right_value);
                return Ok((rest, value));
            }
            _ => {}
        }
    }

    Ok(left)
}

fn parse_p3(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    if let Some((Token::ExclamationPoint, rest)) = stream.split_first() {
        let (rest, right_value) = parse_p3(rest)?;
        let value = u64::from(right_value == 0);
        return Ok((rest, value));
    }

    parse_leaf(stream)
}

fn parse_leaf(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    if let Some((tok, rest)) = stream.split_first() {
        match tok {
            Token::False => return Ok((rest, 0)),
            Token::True => return Ok((rest, 1)),
            Token::LiteralInt(v) => return Ok((rest, *v)),
            Token::LiteralUInt(v) => return Ok((rest, *v)),
            Token::LeftParen => {
                let (rest, inner) = parse_p15(rest)?;
                if let Some((Token::RightParen, rest)) = rest.split_first() {
                    return Ok((rest, inner));
                }
            }
            Token::Id(_) => return Ok((rest, 0)),
            _ => {}
        }
    }

    Err(ConditionParseError)
}

#[test]
#[allow(clippy::bool_assert_comparison)]
fn test_condition_parser() {
    assert_eq!(parse("0").unwrap(), false);
    assert_eq!(parse("1").unwrap(), true);
    assert_eq!(parse("!0").unwrap(), true);
    assert_eq!(parse("!1").unwrap(), false);
    assert_eq!(parse("0 && 0").unwrap(), false);
    assert_eq!(parse("0 && 1").unwrap(), false);
    assert_eq!(parse("1 && 0").unwrap(), false);
    assert_eq!(parse("1 && 1").unwrap(), true);
    assert_eq!(parse("0 || 0").unwrap(), false);
    assert_eq!(parse("0 || 1").unwrap(), true);
    assert_eq!(parse("1 || 0").unwrap(), true);
    assert_eq!(parse("1 || 1").unwrap(), true);
    assert_eq!(parse("0 && 0 || 1").unwrap(), true);
    assert_eq!(parse("0 && 1 || 1").unwrap(), true);
    assert_eq!(parse("1 && 0 || 1").unwrap(), true);
    assert_eq!(parse("1 && 1 || 1").unwrap(), true);
    assert_eq!(parse("0 && 0 || 0").unwrap(), false);
    assert_eq!(parse("0 && 1 || 0").unwrap(), false);
    assert_eq!(parse("1 && 0 || 0").unwrap(), false);
    assert_eq!(parse("1 && 1 || 0").unwrap(), true);
    assert_eq!(parse("0 && (0 || 1)").unwrap(), false);
    assert_eq!(parse("0 && (1 || 1)").unwrap(), false);
    assert_eq!(parse("1 && (0 || 1)").unwrap(), true);
    assert_eq!(parse("1 && (1 || 1)").unwrap(), true);
    assert_eq!(parse("0 && (0 || 0)").unwrap(), false);
    assert_eq!(parse("0 && (1 || 0)").unwrap(), false);
    assert_eq!(parse("1 && (0 || 0)").unwrap(), false);
    assert_eq!(parse("1 && (1 || 0)").unwrap(), true);
    assert_eq!(parse("0 || 0 && 1").unwrap(), false);
    assert_eq!(parse("0 || 1 && 1").unwrap(), true);
    assert_eq!(parse("1 || 0 && 1").unwrap(), true);
    assert_eq!(parse("1 || 1 && 1").unwrap(), true);
    assert_eq!(parse("0 || 0 && 0").unwrap(), false);
    assert_eq!(parse("0 || 1 && 0").unwrap(), false);
    assert_eq!(parse("1 || 0 && 0").unwrap(), true);
    assert_eq!(parse("1 || 1 && 0").unwrap(), true);
    assert_eq!(parse("(0 || 0) && 1").unwrap(), false);
    assert_eq!(parse("(0 || 1) && 1").unwrap(), true);
    assert_eq!(parse("(1 || 0) && 1").unwrap(), true);
    assert_eq!(parse("(1 || 1) && 1").unwrap(), true);
    assert_eq!(parse("(0 || 0) && 0").unwrap(), false);
    assert_eq!(parse("(0 || 1) && 0").unwrap(), false);
    assert_eq!(parse("(1 || 0) && 0").unwrap(), false);
    assert_eq!(parse("(1 || 1) && 0").unwrap(), false);
    assert_eq!(parse("0 == 0").unwrap(), true);
    assert_eq!(parse("0 == 1").unwrap(), false);
    assert_eq!(parse("1 == 0").unwrap(), false);
    assert_eq!(parse("1 == 1").unwrap(), true);
    assert_eq!(parse("0 != 0").unwrap(), false);
    assert_eq!(parse("0 != 1").unwrap(), true);
    assert_eq!(parse("1 != 0").unwrap(), true);
    assert_eq!(parse("1 != 1").unwrap(), false);
}
