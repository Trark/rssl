use crate::preprocess::PreprocessError;
use rssl_text::tokens::*;
use rssl_text::*;

/// Parse a condition for an #if and return if it passes
pub fn parse(
    tokens: &[PreprocessToken],
    base_location: SourceLocation,
) -> Result<bool, PreprocessError> {
    let location = match tokens.first() {
        Some(tok) => tok.get_location(),
        None => base_location,
    };

    let tokens = tokens
        .iter()
        .filter_map(|t| {
            assert!(!matches!(t.0, Token::MacroArg(_)));
            if t.0.is_whitespace() {
                assert_ne!(t.0, Token::Endline);
                None
            } else {
                Some(t.0.clone())
            }
        })
        .collect::<Vec<_>>();

    match parse_p12(&tokens) {
        Ok((&[], value)) => Ok(value != 0),
        Ok((_, _)) => Err(PreprocessError::FailedToParseIfCondition(location)),
        Err(_) => Err(PreprocessError::FailedToParseIfCondition(location)),
    }
}

/// Type used for accumulating condition values
type ConditionValue = u64;

/// Error type for internal condition parsing errors
struct ConditionParseError;

/// Any operator with two arguments that the condition parser supports
pub enum BinOp {
    BooleanAnd,
    BooleanOr,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equality,
    Inequality,
}

impl BinOp {
    /// Apply the operation to input values
    fn apply(&self, left: ConditionValue, right: ConditionValue) -> ConditionValue {
        match self {
            BinOp::BooleanAnd => u64::from(left != 0 && right != 0),
            BinOp::BooleanOr => u64::from(left != 0 || right != 0),
            BinOp::LessThan => u64::from(left < right),
            BinOp::LessEqual => u64::from(left <= right),
            BinOp::GreaterThan => u64::from(left > right),
            BinOp::GreaterEqual => u64::from(left >= right),
            BinOp::Equality => u64::from(left == right),
            BinOp::Inequality => u64::from(left != right),
        }
    }
}

/// Combine binary operations
fn combine_rights(left: ConditionValue, rights: Vec<(BinOp, ConditionValue)>) -> ConditionValue {
    let mut final_value = left;
    for val in rights.iter() {
        let (ref op, ref exp) = *val;
        final_value = op.apply(final_value, *exp);
    }
    final_value
}

/// Parse multiple binary operations
fn parse_binary_operations(
    operator_fn: impl Fn(&[Token]) -> Result<(&[Token], BinOp), ConditionParseError>,
    expression_fn: impl Fn(&[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError>,
    input: &[Token],
) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    let (input, left) = expression_fn(input)?;

    // Parse as many operators / right expressions as we can
    let mut input = input;
    let mut rights = Vec::new();
    // First attempt to parse an operator
    while let Ok((rest, op)) = operator_fn(input) {
        // Then after an operator is successfully parsed
        // Unconditionally parse right side
        let (rest, right) = expression_fn(rest)?;

        rights.push((op, right));
        input = rest;
    }

    let expr = combine_rights(left, rights);
    Ok((input, expr))
}

fn parse_p12(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    fn parse_op(input: &[Token]) -> Result<(&[Token], BinOp), ConditionParseError> {
        match input {
            [Token::VerticalBarVerticalBar, rest @ ..] => Ok((rest, BinOp::BooleanOr)),
            _ => Err(ConditionParseError),
        }
    }

    parse_binary_operations(parse_op, parse_p11, stream)
}

fn parse_p11(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    fn parse_op(input: &[Token]) -> Result<(&[Token], BinOp), ConditionParseError> {
        match input {
            [Token::AmpersandAmpersand, rest @ ..] => Ok((rest, BinOp::BooleanAnd)),
            _ => Err(ConditionParseError),
        }
    }

    parse_binary_operations(parse_op, parse_p7, stream)
}

fn parse_p7(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    fn parse_op(input: &[Token]) -> Result<(&[Token], BinOp), ConditionParseError> {
        match input {
            [Token::EqualsEquals, rest @ ..] => Ok((rest, BinOp::Equality)),
            [Token::ExclamationPointEquals, rest @ ..] => Ok((rest, BinOp::Inequality)),
            _ => Err(ConditionParseError),
        }
    }

    parse_binary_operations(parse_op, parse_p6, stream)
}

fn parse_p6(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    fn parse_op(input: &[Token]) -> Result<(&[Token], BinOp), ConditionParseError> {
        match input {
            [Token::LeftAngleBracket(FollowedBy::Token), Token::Equals, rest @ ..] => {
                Ok((rest, BinOp::LessEqual))
            }
            [Token::RightAngleBracket(FollowedBy::Token), Token::Equals, rest @ ..] => {
                Ok((rest, BinOp::GreaterEqual))
            }
            [Token::LeftAngleBracket(_), rest @ ..] => Ok((rest, BinOp::LessThan)),
            [Token::RightAngleBracket(_), rest @ ..] => Ok((rest, BinOp::GreaterThan)),
            _ => Err(ConditionParseError),
        }
    }

    parse_binary_operations(parse_op, parse_p2, stream)
}

fn parse_p2(stream: &[Token]) -> Result<(&[Token], ConditionValue), ConditionParseError> {
    if let Some((Token::ExclamationPoint, rest)) = stream.split_first() {
        let (rest, right_value) = parse_p2(rest)?;
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
                let (rest, inner) = parse_p12(rest)?;
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
    #[track_caller]
    fn eval(s: &str) -> bool {
        let mut source_manager = rssl_text::SourceManager::new();
        let (file_id, _) = source_manager.add_fragment(s);
        parse(
            &crate::lexer::lex_fragment(file_id, &source_manager).unwrap(),
            SourceLocation::UNKNOWN,
        )
        .unwrap()
    }

    assert_eq!(eval("0"), false);
    assert_eq!(eval("1"), true);
    assert_eq!(eval("!0"), true);
    assert_eq!(eval("!1"), false);
    assert_eq!(eval("0 && 0"), false);
    assert_eq!(eval("0 && 1"), false);
    assert_eq!(eval("1 && 0"), false);
    assert_eq!(eval("1 && 1"), true);
    assert_eq!(eval("1 && 1 && 1"), true);
    assert_eq!(eval("0 || 0"), false);
    assert_eq!(eval("0 || 1"), true);
    assert_eq!(eval("1 || 0"), true);
    assert_eq!(eval("1 || 1"), true);
    assert_eq!(eval("0 || 0 || 1"), true);
    assert_eq!(eval("0 && 0 || 1"), true);
    assert_eq!(eval("0 && 1 || 1"), true);
    assert_eq!(eval("1 && 0 || 1"), true);
    assert_eq!(eval("1 && 1 || 1"), true);
    assert_eq!(eval("0 && 0 || 0"), false);
    assert_eq!(eval("0 && 1 || 0"), false);
    assert_eq!(eval("1 && 0 || 0"), false);
    assert_eq!(eval("1 && 1 || 0"), true);
    assert_eq!(eval("0 && (0 || 1)"), false);
    assert_eq!(eval("0 && (1 || 1)"), false);
    assert_eq!(eval("1 && (0 || 1)"), true);
    assert_eq!(eval("1 && (1 || 1)"), true);
    assert_eq!(eval("0 && (0 || 0)"), false);
    assert_eq!(eval("0 && (1 || 0)"), false);
    assert_eq!(eval("1 && (0 || 0)"), false);
    assert_eq!(eval("1 && (1 || 0)"), true);
    assert_eq!(eval("0 || 0 && 1"), false);
    assert_eq!(eval("0 || 1 && 1"), true);
    assert_eq!(eval("1 || 0 && 1"), true);
    assert_eq!(eval("1 || 1 && 1"), true);
    assert_eq!(eval("0 || 0 && 0"), false);
    assert_eq!(eval("0 || 1 && 0"), false);
    assert_eq!(eval("1 || 0 && 0"), true);
    assert_eq!(eval("1 || 1 && 0"), true);
    assert_eq!(eval("(0 || 0) && 1"), false);
    assert_eq!(eval("(0 || 1) && 1"), true);
    assert_eq!(eval("(1 || 0) && 1"), true);
    assert_eq!(eval("(1 || 1) && 1"), true);
    assert_eq!(eval("(0 || 0) && 0"), false);
    assert_eq!(eval("(0 || 1) && 0"), false);
    assert_eq!(eval("(1 || 0) && 0"), false);
    assert_eq!(eval("(1 || 1) && 0"), false);
    assert_eq!(eval("0 == 0"), true);
    assert_eq!(eval("0 == 1"), false);
    assert_eq!(eval("1 == 0"), false);
    assert_eq!(eval("1 == 1"), true);
    assert_eq!(eval("0 != 0"), false);
    assert_eq!(eval("0 != 1"), true);
    assert_eq!(eval("1 != 0"), true);
    assert_eq!(eval("1 != 1"), false);
    assert_eq!(eval("0 < 0"), false);
    assert_eq!(eval("0 < 1"), true);
    assert_eq!(eval("1 <= 0"), false);
    assert_eq!(eval("1 <= 1"), true);
    assert_eq!(eval("1 > 0"), true);
    assert_eq!(eval("1 > 1"), false);
    assert_eq!(eval("1 >= 1"), true);
    assert_eq!(eval("1 >= 2"), false);
    assert_eq!(eval("1 < 5 <= 1 > 0 <= 0"), false);
}
