use nom::{error::ErrorKind, IResult, Needed};
use rssl_text::*;
use rssl_tok::*;

#[derive(PartialEq, Clone)]
pub enum LexError {
    Unknown,
    FailedToParse(Vec<u8>),
    UnexpectedEndOfStream,
}

impl std::fmt::Debug for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            LexError::Unknown => write!(f, "Unknown"),
            LexError::FailedToParse(ref data) => match std::str::from_utf8(data) {
                Ok(friendly) => {
                    let substr = match friendly.find('\n') {
                        Some(index) => &friendly[..index],
                        None => &friendly,
                    };
                    write!(f, "FailedToParse(\"{}\")", substr)
                }
                Err(_) => write!(f, "FailedToParse({:?})", data),
            },
            LexError::UnexpectedEndOfStream => write!(f, "UnexpectedEndOfStream"),
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            LexError::Unknown => write!(f, "unknown lexer error"),
            LexError::FailedToParse(ref rest) => {
                let next_space = rest
                    .iter()
                    .position(|c| *c == b' ' || *c == b'\t' || *c == b'\n' || *c == b'\r')
                    .unwrap_or(rest.len());
                match std::str::from_utf8(&rest[..next_space]) {
                    Ok(s) => write!(f, "Failed to parse tokens: {}", s),
                    _ => write!(f, "Failed to parse tokens: Invalid UTF-8"),
                }
            }
            LexError::UnexpectedEndOfStream => write!(f, "unexpected end of stream"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct IntermediateLocation(u64);

#[derive(PartialEq, Debug, Clone)]
struct IntermediateToken(Token, IntermediateLocation);

#[derive(PartialEq, Debug, Clone)]
struct StreamToken(pub Token, pub StreamLocation);

/// Parse a single decimal digit
fn digit(input: &[u8]) -> IResult<&[u8], u64> {
    // Handle end of stream
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    // Match on the next character
    let n = match input[0] {
        b'0' => 0,
        b'1' => 1,
        b'2' => 2,
        b'3' => 3,
        b'4' => 4,
        b'5' => 5,
        b'6' => 6,
        b'7' => 7,
        b'8' => 8,
        b'9' => 9,
        _ => {
            // Not a digit
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse multiple decimal digits into a 64-bit value
fn digits(input: &[u8]) -> IResult<&[u8], u64> {
    let (mut input, mut value) = digit(input)?;
    loop {
        match digit(input) {
            Ok((next_input, d)) => {
                input = next_input;
                value = value * 10;
                value += d;
            }
            _ => break,
        }
    }
    Ok((input, value))
}

#[test]
fn test_digits() {
    let p = digits;
    assert_eq!(p(b"086"), Ok((&b""[..], 86)));
    assert_eq!(p(b"086;"), Ok((&b";"[..], 86)));
}

/// Parse a single hexadecimal digit
fn digit_hex(input: &[u8]) -> IResult<&[u8], u64> {
    // Handle end of stream
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    // Match on the next character
    let n = match input[0] {
        b'0' => 0,
        b'1' => 1,
        b'2' => 2,
        b'3' => 3,
        b'4' => 4,
        b'5' => 5,
        b'6' => 6,
        b'7' => 7,
        b'8' => 8,
        b'9' => 9,
        b'A' => 10,
        b'a' => 10,
        b'B' => 11,
        b'b' => 11,
        b'C' => 12,
        b'c' => 12,
        b'D' => 13,
        b'd' => 13,
        b'E' => 14,
        b'e' => 14,
        b'F' => 15,
        b'f' => 15,
        _ => {
            // Not a digit
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse multiple hexadecimal digits into a 64-bit value
fn digits_hex(input: &[u8]) -> IResult<&[u8], u64> {
    let (mut input, mut value) = digit_hex(input)?;
    loop {
        match digit_hex(input) {
            Ok((next_input, d)) => {
                input = next_input;
                value = value * 16;
                value += d;
            }
            _ => break,
        }
    }
    Ok((input, value))
}

#[test]
fn test_digits_hex() {
    let p = digits_hex;
    assert_eq!(p(b"08a"), Ok((&b""[..], 138)));
    assert_eq!(p(b"08a;"), Ok((&b";"[..], 138)));
}

/// Parse a single octal digit
fn digit_octal(input: &[u8]) -> IResult<&[u8], u64> {
    // Handle end of stream
    if input.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    // Match on the next character
    let n = match input[0] {
        b'0' => 0,
        b'1' => 1,
        b'2' => 2,
        b'3' => 3,
        b'4' => 4,
        b'5' => 5,
        b'6' => 6,
        b'7' => 7,
        _ => {
            // Not a digit
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse multiple octal digits into a 64-bit value
fn digits_octal(input: &[u8]) -> IResult<&[u8], u64> {
    let (mut input, mut value) = digit_octal(input)?;
    loop {
        match digit_octal(input) {
            Ok((next_input, d)) => {
                input = next_input;
                value = value * 8;
                value += d;
            }
            _ => break,
        }
    }
    Ok((input, value))
}

#[test]
fn test_digits_octal() {
    let p = digits_octal;
    assert_eq!(p(b"071"), Ok((&b""[..], 57)));
    assert_eq!(p(b"071;"), Ok((&b";"[..], 57)));
}

/// Integer literal type
enum IntType {
    UInt,
    Long,
}

/// Parse an integer literal suffix
fn int_type(input: &[u8]) -> IResult<&[u8], IntType> {
    // Match on the first character
    let n = match input.first() {
        Some(b'u') | Some(b'U') => IntType::UInt,
        Some(b'l') | Some(b'L') => IntType::Long,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse a decimal literal
fn literal_decimal_int(input: &[u8]) -> IResult<&[u8], Token> {
    let (input, value) = digits(input)?;
    let (input, int_type_opt) = nom::combinator::opt(int_type)(input)?;
    let token = match int_type_opt {
        None => Token::LiteralInt(value),
        Some(IntType::UInt) => Token::LiteralUInt(value),
        Some(IntType::Long) => Token::LiteralLong(value),
    };
    Ok((input, token))
}

/// Parse a hexadecimal literal
fn literal_hex_int(input: &[u8]) -> IResult<&[u8], Token> {
    let (input, value) = digits_hex(input)?;
    let (input, int_type_opt) = nom::combinator::opt(int_type)(input)?;
    let token = match int_type_opt {
        None => Token::LiteralInt(value),
        Some(IntType::UInt) => Token::LiteralUInt(value),
        Some(IntType::Long) => Token::LiteralLong(value),
    };
    Ok((input, token))
}

/// Parse an octal literal
fn literal_octal_int(input: &[u8]) -> IResult<&[u8], Token> {
    let (input, value) = digits_octal(input)?;
    let (input, int_type_opt) = nom::combinator::opt(int_type)(input)?;
    let token = match int_type_opt {
        None => Token::LiteralInt(value),
        Some(IntType::UInt) => Token::LiteralUInt(value),
        Some(IntType::Long) => Token::LiteralLong(value),
    };
    Ok((input, token))
}

/// Parse an integer literal
fn literal_int(input: &[u8]) -> IResult<&[u8], Token> {
    if input.starts_with(b"0x") {
        literal_hex_int(&input[2..])
    } else if input.starts_with(b"0") && (digit_octal(&input[1..]).is_ok()) {
        literal_octal_int(&input[1..])
    } else {
        literal_decimal_int(input)
    }
}

#[test]
fn test_literal_int() {
    let p = literal_int;
    assert_eq!(p(b"0u"), Ok((&b""[..], Token::LiteralUInt(0))));
    assert_eq!(p(b"0 "), Ok((&b" "[..], Token::LiteralInt(0))));
    assert_eq!(p(b"12 "), Ok((&b" "[..], Token::LiteralInt(12))));
    assert_eq!(p(b"12u"), Ok((&b""[..], Token::LiteralUInt(12))));
    assert_eq!(p(b"12l"), Ok((&b""[..], Token::LiteralLong(12))));
    assert_eq!(p(b"12L"), Ok((&b""[..], Token::LiteralLong(12))));
    assert_eq!(p(b"0x3 "), Ok((&b" "[..], Token::LiteralInt(3))));
    assert_eq!(p(b"0xA1 "), Ok((&b" "[..], Token::LiteralInt(161))));
    assert_eq!(p(b"0xA1u"), Ok((&b""[..], Token::LiteralUInt(161))));
    assert_eq!(p(b"0123u"), Ok((&b""[..], Token::LiteralUInt(83))));
}

type DigitSequence = Vec<u64>;

/// Parse a sequence of digits into an array
fn digit_sequence(input: &[u8]) -> IResult<&[u8], DigitSequence> {
    nom::multi::many1(digit)(input)
}

#[derive(PartialEq, Debug, Clone)]
struct Fraction(DigitSequence, DigitSequence);

/// Parse the main fractional parts of a float literal
fn fractional_constant(input: &[u8]) -> IResult<&[u8], Fraction> {
    let (input, whole_part) = nom::combinator::opt(digit_sequence)(input)?;
    let (input, _) = nom::bytes::complete::tag(".")(input)?;

    // If there was not a whole part then the fractional part is mandatory
    let (input, fractional_part) = if whole_part.is_none() {
        nom::combinator::map(digit_sequence, |v| Some(v))(input)?
    } else {
        nom::combinator::opt(digit_sequence)(input)?
    };

    let whole_part = whole_part.unwrap_or_default();
    let fractional_part = fractional_part.unwrap_or_default();

    Ok((input, Fraction(whole_part, fractional_part)))
}

/// Float literal type
enum FloatType {
    Half,
    Float,
    Double,
}

/// Parse a float literal
fn float_type(input: &[u8]) -> IResult<&[u8], FloatType> {
    // Match on the first character
    let n = match input.first() {
        Some(b'h') | Some(b'H') => FloatType::Half,
        Some(b'f') | Some(b'F') => FloatType::Float,
        Some(b'l') | Some(b'L') => FloatType::Double,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Sign marker
enum Sign {
    Positive,
    Negative,
}

/// Parse a sign marker
fn sign(input: &[u8]) -> IResult<&[u8], Sign> {
    match input.first() {
        Some(b'+') => Ok((&input[1..], Sign::Positive)),
        Some(b'-') => Ok((&input[1..], Sign::Negative)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Exponent value
#[derive(PartialEq, Debug, Clone)]
struct Exponent(i64);

/// Parse an exponent in a float literal
fn float_exponent(input: &[u8]) -> IResult<&[u8], Exponent> {
    // Use streaming tag so we return incomplete on empty streams
    // Float parsing code currently relies on this to not error when there is not an exponent
    use nom::bytes::streaming::tag;
    let (input, _) = nom::branch::alt((tag("e"), tag("E")))(input)?;
    let (input, s_opt) = nom::combinator::opt(sign)(input)?;
    let (input, exponent) = digits(input)?;
    let exponent = match s_opt {
        Some(Sign::Negative) => -(exponent as i64),
        _ => exponent as i64,
    };
    Ok((input, Exponent(exponent)))
}

#[test]
fn test_exponent() {
    let p = float_exponent;
    assert_eq!(p(b"E0"), Ok((&b""[..], Exponent(0))));
    assert_eq!(p(b"E+8"), Ok((&b""[..], Exponent(8))));
    assert_eq!(p(b"E-45"), Ok((&b""[..], Exponent(-45))));

    assert_eq!(p(b"E0;"), Ok((&b";"[..], Exponent(0))));
    assert_eq!(p(b"E+8;"), Ok((&b";"[..], Exponent(8))));
    assert_eq!(p(b"E-45;"), Ok((&b";"[..], Exponent(-45))));

    assert_eq!(p(b""), Err(nom::Err::Incomplete(nom::Needed::new(1))));
    assert_eq!(
        p(b"."),
        Err(nom::Err::Error(nom::error::Error::new(
            &b"."[..],
            nom::error::ErrorKind::Tag
        )))
    );
}

/// Build a float literal token from each part of literal
fn calculate_float_from_parts(
    left: DigitSequence,
    right: DigitSequence,
    exponent: i64,
    float_type: Option<FloatType>,
) -> Token {
    let mut left_combined = 0f64;
    for digit in left {
        left_combined = left_combined * 10f64;
        left_combined += digit as f64;
    }
    let left_float = left_combined as f64;

    let mut right_combined = 0f64;
    let right_len = right.len();
    for digit in right {
        right_combined = right_combined * 10f64;
        right_combined += digit as f64;
    }
    let mut right_float = right_combined as f64;
    for _ in 0..right_len {
        right_float = right_float / 10f64;
    }

    let mantissa = left_float + right_float;
    let mut value64 = mantissa;
    if exponent > 0 {
        for _ in 0..exponent {
            value64 = value64 * 10f64;
        }
    } else {
        for _ in 0..(-exponent) {
            value64 = value64 / 10f64;
        }
    }

    match float_type.unwrap_or(FloatType::Float) {
        FloatType::Half => Token::LiteralHalf(value64 as f32),
        FloatType::Float => Token::LiteralFloat(value64 as f32),
        FloatType::Double => Token::LiteralDouble(value64),
    }
}

/// Parse a float literal
fn literal_float(input: &[u8]) -> IResult<&[u8], Token> {
    // First try to parse a fraction
    let (input, fraction) = nom::combinator::opt(fractional_constant)(input)?;

    // Then if that failed try to parse as a whole number
    let has_fraction = fraction.is_some();
    let (input, fraction) = match fraction {
        Some(f) => (input, f),
        None => {
            let (input, whole_number) = digit_sequence(input)?;
            (input, Fraction(whole_number, Vec::new()))
        }
    };

    let (input, exponent_opt) = nom::combinator::opt(float_exponent)(input)?;

    // If we did not have a fractional part then we require the exponent, else it is optional
    // This avoids integers parsing as valid floats
    if !has_fraction && exponent_opt.is_none() {
        return Err(nom::Err::Error(nom::error::Error::new(
            &b"."[..],
            nom::error::ErrorKind::Not,
        )));
    }

    let (input, float_type) = nom::combinator::opt(float_type)(input)?;

    let exponent = exponent_opt.unwrap_or(Exponent(0));
    let Fraction(left, right) = fraction;
    let Exponent(exp) = exponent;
    let token = calculate_float_from_parts(left, right, exp, float_type);

    Ok((input, token))
}

#[test]
fn test_literal_float() {
    let p = literal_float;
    assert_eq!(p(b"0.0f"), Ok((&b""[..], Token::LiteralFloat(0.0))));
    assert_eq!(p(b"2.7h"), Ok((&b""[..], Token::LiteralHalf(2.7))));
    assert_eq!(p(b"9.7L"), Ok((&b""[..], Token::LiteralDouble(9.7))));

    assert_eq!(p(b"0.f"), Ok((&b""[..], Token::LiteralFloat(0.0))));
    assert_eq!(p(b".0f"), Ok((&b""[..], Token::LiteralFloat(0.0))));

    // Float without suffix at end of file does not currently work
    assert_eq!(p(b"0.;"), Ok((&b";"[..], Token::LiteralFloat(0.0))));
    assert_eq!(p(b".0;"), Ok((&b";"[..], Token::LiteralFloat(0.0))));

    assert_eq!(p(b"7E-7"), Ok((&b""[..], Token::LiteralFloat(7e-7))));
    assert_eq!(p(b"1e+11"), Ok((&b""[..], Token::LiteralFloat(1e+11))));
    assert_eq!(
        p(b"4.863e+11"),
        Ok((&b""[..], Token::LiteralFloat(4.863e+11)))
    );

    assert!(p(b"0").is_err());
    assert!(p(b"0.").is_err());
    assert!(p(b".0").is_err());
    assert!(p(b".").is_err());
}

fn identifier_firstchar<'a>(input: &'a [u8]) -> IResult<&'a [u8], u8> {
    if input.len() == 0 {
        Err(nom::Err::Incomplete(Needed::new(1)))
    } else {
        let byte = input[0];
        let ch = byte as char;
        if (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch == '_') {
            Ok((&input[1..], byte))
        } else {
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

fn identifier_char<'a>(input: &'a [u8]) -> IResult<&'a [u8], u8> {
    if input.len() == 0 {
        Err(nom::Err::Incomplete(Needed::new(1)))
    } else {
        let byte = input[0];
        let ch = byte as char;
        if (ch >= 'A' && ch <= 'Z')
            || (ch >= 'a' && ch <= 'z')
            || (ch == '_')
            || (ch >= '0' && ch <= '9')
        {
            Ok((&input[1..], byte))
        } else {
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

fn identifier<'a>(input: &'a [u8]) -> IResult<&'a [u8], Identifier> {
    let mut chars = Vec::new();
    let first_result = identifier_firstchar(input);

    let mut stream = match first_result {
        Err(err) => return Err(err),
        Ok((output, ch)) => {
            chars.push(ch);
            output
        }
    };

    loop {
        stream = match identifier_char(stream) {
            Err(_) => break,
            Ok((output, ch)) => {
                chars.push(ch);
                output
            }
        }
    }

    // If we match a reserved word, fail instead. The token parser
    // will then match the reserved word next
    // This is done instead of just running the reserved word parsing
    // first to avoid having the deal with detecting the end of a name
    // for when parsing an identifier that has a reserved word as a
    // sub string at the start
    // Maybe just combine the identifier and reserved word parsing?
    if let Ok((slice, _)) = reserved_word(&chars[..]) {
        if slice.len() == 0 {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    }

    Ok((
        stream,
        Identifier(std::str::from_utf8(&chars[..]).unwrap().to_string()),
    ))
}

/// Parse trivial whitespace
fn whitespace_simple(input: &[u8]) -> IResult<&[u8], ()> {
    if input.len() == 0 {
        Err(nom::Err::Incomplete(Needed::new(1)))
    } else {
        match input[0] {
            b' ' | b'\n' | b'\r' | b'\t' => Ok((&input[1..], ())),
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Alt,
            ))),
        }
    }
}

/// Parse a line comment
fn line_comment(input: &[u8]) -> IResult<&[u8], ()> {
    if input.starts_with(b"//") {
        match input.iter().enumerate().position(|c| *c.1 == b'\n') {
            Some(len) => Ok((&input[len..], ())),
            None => Ok((&[], ())),
        }
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Eof,
        )))
    }
}

/// Parse a block comment
fn block_comment(input: &[u8]) -> IResult<&[u8], ()> {
    if input.starts_with(b"/*") {
        // Find the end of the block
        // We do not supported nested blocks
        let mut search = &input[2..];
        loop {
            if search.len() < 2 {
                break;
            }
            if search.starts_with(b"*/") {
                return Ok((&search[2..], ()));
            }
            search = &search[1..];
        }

        // Comment goes off the end of the file
        Err(nom::Err::Failure(nom::error::Error::new(
            input,
            ErrorKind::Eof,
        )))
    } else {
        // Not a block comment
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )))
    }
}

/// Parse any kind of whitespace
fn whitespace(input: &[u8]) -> IResult<&[u8], ()> {
    let mut search = input;
    loop {
        search = match nom::branch::alt((whitespace_simple, line_comment, block_comment))(search) {
            Ok((input, ())) => input,
            Err(nom::Err::Failure(err)) => return Err(nom::Err::Failure(err)),
            Err(_) => break,
        }
    }

    if input == search {
        // No whitespace found
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Alt,
        )))
    } else {
        // Whitespace found
        Ok((search, ()))
    }
}

/// Parse any kind of white space or no whitespace
fn skip_whitespace(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = nom::combinator::opt(whitespace)(input)?;
    Ok((input, ()))
}

#[test]
fn test_whitespace() {
    let complete = Ok((&[][..], ()));
    assert!(whitespace(b"").is_err());
    assert_eq!(whitespace(b" "), complete);
    assert_eq!(whitespace(b"//\n"), complete);
    assert_eq!(whitespace(b"// comment\n"), complete);
    assert_eq!(whitespace(b"/* comment */"), complete);
    assert_eq!(whitespace(b"/* line 1\n\t line 2\n\t line 3 */"), complete);
    assert_eq!(whitespace(b"/* line 1\n\t star *\n\t line 3 */"), complete);
    assert_eq!(whitespace(b"/* line 1\n\t slash /\n\t line 3 */"), complete);
}

macro_rules! declare_keyword {
    ( $parse_function:ident, $name:expr ) => {
        fn $parse_function(input: &[u8]) -> IResult<&[u8], &[u8]> {
            nom::bytes::complete::tag($name)(input)
        }
    };
}

// Reserved words
declare_keyword!(reserved_word_if, "if");
declare_keyword!(reserved_word_else, "else");
declare_keyword!(reserved_word_for, "for");
declare_keyword!(reserved_word_while, "while");
declare_keyword!(reserved_word_switch, "switch");
declare_keyword!(reserved_word_return, "return");
declare_keyword!(reserved_word_break, "break");
declare_keyword!(reserved_word_continue, "continue");
declare_keyword!(reserved_word_struct, "struct");
declare_keyword!(reserved_word_samplerstate, "SamplerState");
declare_keyword!(reserved_word_cbuffer, "cbuffer");
declare_keyword!(reserved_word_register, "register");
declare_keyword!(reserved_word_true, "true");
declare_keyword!(reserved_word_false, "false");
declare_keyword!(reserved_word_packoffset, "packoffset");
declare_keyword!(reserved_word_in, "in");
declare_keyword!(reserved_word_out, "out");
declare_keyword!(reserved_word_inout, "inout");
declare_keyword!(reserved_word_const, "const");
declare_keyword!(reserved_word_extern, "extern");
declare_keyword!(reserved_word_static, "static");
declare_keyword!(reserved_word_groupshared, "groupshared");
declare_keyword!(reserved_word_sizeof, "sizeof");

// Unused reserved words
declare_keyword!(reserved_word_auto, "auto");
declare_keyword!(reserved_word_case, "case");
declare_keyword!(reserved_word_catch, "catch");
declare_keyword!(reserved_word_char, "char");
declare_keyword!(reserved_word_class, "class");
declare_keyword!(reserved_word_const_cast, "const_cast");
declare_keyword!(reserved_word_default, "default");
declare_keyword!(reserved_word_delete, "delete");
declare_keyword!(reserved_word_dynamic_cast, "dynamic_cast");
declare_keyword!(reserved_word_enum, "enum");
declare_keyword!(reserved_word_explicit, "explicit");
declare_keyword!(reserved_word_friend, "friend");
declare_keyword!(reserved_word_goto, "goto");
declare_keyword!(reserved_word_long, "long");
declare_keyword!(reserved_word_mutable, "mutable");
declare_keyword!(reserved_word_new, "new");
declare_keyword!(reserved_word_operator, "operator");
declare_keyword!(reserved_word_private, "private");
declare_keyword!(reserved_word_protected, "protected");
declare_keyword!(reserved_word_public, "public");
declare_keyword!(reserved_word_reinterpret_cast, "reinterpret_cast");
declare_keyword!(reserved_word_short, "short");
declare_keyword!(reserved_word_signed, "signed");
declare_keyword!(reserved_word_static_cast, "static_cast");
declare_keyword!(reserved_word_template, "template");
declare_keyword!(reserved_word_this, "this");
declare_keyword!(reserved_word_throw, "throw");
declare_keyword!(reserved_word_try, "try");
declare_keyword!(reserved_word_typename, "typename");
declare_keyword!(reserved_word_union, "union");
declare_keyword!(reserved_word_unsigned, "unsigned");
declare_keyword!(reserved_word_using, "using");
declare_keyword!(reserved_word_virtual, "virtual");

fn reserved_word(input: &[u8]) -> IResult<&[u8], &[u8]> {
    // alt only supports 21 alternatives so recursively use alt
    nom::branch::alt((
        nom::branch::alt((
            reserved_word_if,
            reserved_word_else,
            reserved_word_for,
            reserved_word_while,
            reserved_word_switch,
            reserved_word_return,
            reserved_word_break,
            reserved_word_continue,
            reserved_word_struct,
            reserved_word_samplerstate,
            reserved_word_cbuffer,
            reserved_word_register,
            reserved_word_true,
            reserved_word_false,
            reserved_word_packoffset,
            reserved_word_inout,
            reserved_word_out,
            reserved_word_in,
            reserved_word_auto,
            reserved_word_case,
            reserved_word_catch,
        )),
        nom::branch::alt((
            reserved_word_char,
            reserved_word_class,
            reserved_word_const_cast,
            reserved_word_default,
            reserved_word_delete,
            reserved_word_dynamic_cast,
            reserved_word_enum,
            reserved_word_const,
            reserved_word_extern,
            reserved_word_static,
            reserved_word_groupshared,
            reserved_word_explicit,
            reserved_word_friend,
            reserved_word_goto,
            reserved_word_long,
            reserved_word_mutable,
            reserved_word_new,
            reserved_word_operator,
            reserved_word_private,
            reserved_word_protected,
            reserved_word_public,
        )),
        nom::branch::alt((
            reserved_word_reinterpret_cast,
            reserved_word_short,
            reserved_word_signed,
            reserved_word_sizeof,
            reserved_word_static_cast,
            reserved_word_template,
            reserved_word_this,
            reserved_word_throw,
            reserved_word_try,
            reserved_word_typename,
            reserved_word_union,
            reserved_word_unsigned,
            reserved_word_using,
            reserved_word_virtual,
        )),
    ))(input)
}

/// Register class for a resource
enum RegisterType {
    T,
    U,
    B,
    S,
}

/// Parse a register type
fn register_type(input: &[u8]) -> IResult<&[u8], RegisterType> {
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    nom::branch::alt((
        map(tag("t"), |_| RegisterType::T),
        map(tag("u"), |_| RegisterType::U),
        map(tag("b"), |_| RegisterType::B),
        map(tag("s"), |_| RegisterType::S),
    ))(input)
}

/// Parse a register slot attribute
fn register(input: &[u8]) -> IResult<&[u8], Token> {
    use nom::bytes::complete::tag;
    let (input, _) = reserved_word_register(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, slot_type) = register_type(input)?;
    let (input, num) = digits(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = tag(")")(input)?;

    let token = Token::Register(match slot_type {
        RegisterType::T => RegisterSlot::T(num as u32),
        RegisterType::U => RegisterSlot::U(num as u32),
        RegisterType::B => RegisterSlot::B(num as u32),
        RegisterType::S => RegisterSlot::S(num as u32),
    });

    Ok((input, token))
}

#[test]
fn test_register() {
    let p = register;
    assert_eq!(
        p(b"register(t0)"),
        Ok((&b""[..], Token::Register(RegisterSlot::T(0))))
    );
    assert_eq!(
        p(b"register(t1);"),
        Ok((&b";"[..], Token::Register(RegisterSlot::T(1))))
    );
    assert_eq!(
        p(b"register ( u1 ) ; "),
        Ok((&b" ; "[..], Token::Register(RegisterSlot::U(1))))
    );
}

/// Peek at what token is coming next unless there is whitespace
fn lookahead_token(input: &[u8]) -> IResult<&[u8], Option<Token>> {
    match token_no_whitespace_intermediate(input) {
        Ok((_, o)) => Ok((input, Some(o))),
        Err(_) => Ok((input, None)),
    }
}

/// Parse a < token
fn leftanglebracket(input: &[u8]) -> IResult<&[u8], Token> {
    match input.first() {
        Some(b'<') => {
            let input = &input[1..];
            let token = match lookahead_token(input)?.1 {
                Some(_) => Token::LeftAngleBracket(FollowedBy::Token),
                _ => Token::LeftAngleBracket(FollowedBy::Whitespace),
            };
            Ok((input, token))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

#[test]
fn test_leftanglebracket() {
    let p = leftanglebracket;
    assert_eq!(
        p(b"<"),
        Ok((&b""[..], Token::LeftAngleBracket(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"< "),
        Ok((&b" "[..], Token::LeftAngleBracket(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"<<"),
        Ok((&b"<"[..], Token::LeftAngleBracket(FollowedBy::Token)))
    );
    assert_eq!(
        p(b""),
        Err(nom::Err::Error(nom::error::Error::new(
            &b""[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b" "),
        Err(nom::Err::Error(nom::error::Error::new(
            &b" "[..],
            ErrorKind::Tag,
        )))
    );
}

/// Parse a > token
fn rightanglebracket(input: &[u8]) -> IResult<&[u8], Token> {
    match input.first() {
        Some(b'>') => {
            let input = &input[1..];
            let token = match lookahead_token(input)?.1 {
                Some(_) => Token::RightAngleBracket(FollowedBy::Token),
                _ => Token::RightAngleBracket(FollowedBy::Whitespace),
            };
            Ok((input, token))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

#[test]
fn test_rightanglebracket() {
    let p = rightanglebracket;
    assert_eq!(
        p(b">"),
        Ok((&b""[..], Token::RightAngleBracket(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"> "),
        Ok((&b" "[..], Token::RightAngleBracket(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b">>"),
        Ok((&b">"[..], Token::RightAngleBracket(FollowedBy::Token)))
    );
    assert_eq!(
        p(b""),
        Err(nom::Err::Error(nom::error::Error::new(
            &b""[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b" "),
        Err(nom::Err::Error(nom::error::Error::new(
            &b" "[..],
            ErrorKind::Tag,
        )))
    );
}

/// Parse a = or == token
fn symbol_equals(input: &[u8]) -> IResult<&[u8], Token> {
    match input {
        [b'=', b'=', b'=', ..] => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Not,
        ))),
        [b'=', b'=', ..] => Ok((&input[2..], Token::DoubleEquals)),
        [b'=', ..] => Ok((&input[1..], Token::Equals)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

#[test]
fn test_symbol_equals() {
    let p = symbol_equals;
    assert_eq!(p(b"="), Ok((&b""[..], Token::Equals)));
    assert_eq!(p(b"= "), Ok((&b" "[..], Token::Equals)));
    assert_eq!(p(b"=="), Ok((&b""[..], Token::DoubleEquals)));
    assert_eq!(p(b"== "), Ok((&b" "[..], Token::DoubleEquals)));
    assert_eq!(
        p(b""),
        Err(nom::Err::Error(nom::error::Error::new(
            &b""[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b" "),
        Err(nom::Err::Error(nom::error::Error::new(
            &b" "[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b"==="),
        Err(nom::Err::Error(nom::error::Error::new(
            &b"==="[..],
            ErrorKind::Not,
        )))
    );
}

/// Parse a ! or != token
fn symbol_exclamation(input: &[u8]) -> IResult<&[u8], Token> {
    match input {
        [b'!', b'=', b'=', ..] => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Not,
        ))),
        [b'!', b'=', ..] => Ok((&input[2..], Token::ExclamationEquals)),
        [b'!', ..] => Ok((&input[1..], Token::ExclamationPoint)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

#[test]
fn test_symbol_exclamation() {
    let p = symbol_exclamation;
    assert_eq!(p(b"!"), Ok((&b""[..], Token::ExclamationPoint)));
    assert_eq!(p(b"! "), Ok((&b" "[..], Token::ExclamationPoint)));
    assert_eq!(p(b"!="), Ok((&b""[..], Token::ExclamationEquals)));
    assert_eq!(p(b"!= "), Ok((&b" "[..], Token::ExclamationEquals)));
    assert_eq!(
        p(b""),
        Err(nom::Err::Error(nom::error::Error::new(
            &b""[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b" "),
        Err(nom::Err::Error(nom::error::Error::new(
            &b" "[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b"!=="),
        Err(nom::Err::Error(nom::error::Error::new(
            &b"!=="[..],
            ErrorKind::Not,
        )))
    );
}

/// Parse a & token
fn symbol_ampersand(input: &[u8]) -> IResult<&[u8], Token> {
    match input.first() {
        Some(b'&') => {
            let input = &input[1..];
            let token = match lookahead_token(input)?.1 {
                Some(_) => Token::Ampersand(FollowedBy::Token),
                _ => Token::Ampersand(FollowedBy::Whitespace),
            };
            Ok((input, token))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

#[test]
fn test_symbol_ampersand() {
    let p = symbol_ampersand;
    assert_eq!(
        p(b"&"),
        Ok((&b""[..], Token::Ampersand(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"& "),
        Ok((&b" "[..], Token::Ampersand(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"&&"),
        Ok((&b"&"[..], Token::Ampersand(FollowedBy::Token)))
    );
    assert_eq!(
        p(b""),
        Err(nom::Err::Error(nom::error::Error::new(
            &b""[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b" "),
        Err(nom::Err::Error(nom::error::Error::new(
            &b" "[..],
            ErrorKind::Tag,
        )))
    );
}

/// Parse a | token
fn symbol_verticalbar(input: &[u8]) -> IResult<&[u8], Token> {
    match input.first() {
        Some(b'|') => {
            let input = &input[1..];
            let token = match lookahead_token(input)?.1 {
                Some(_) => Token::VerticalBar(FollowedBy::Token),
                _ => Token::VerticalBar(FollowedBy::Whitespace),
            };
            Ok((input, token))
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

#[test]
fn test_symbol_verticalbar() {
    let p = symbol_verticalbar;
    assert_eq!(
        p(b"|"),
        Ok((&b""[..], Token::VerticalBar(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"| "),
        Ok((&b" "[..], Token::VerticalBar(FollowedBy::Whitespace)))
    );
    assert_eq!(
        p(b"||"),
        Ok((&b"|"[..], Token::VerticalBar(FollowedBy::Token)))
    );
    assert_eq!(
        p(b""),
        Err(nom::Err::Error(nom::error::Error::new(
            &b""[..],
            ErrorKind::Tag,
        )))
    );
    assert_eq!(
        p(b" "),
        Err(nom::Err::Error(nom::error::Error::new(
            &b" "[..],
            ErrorKind::Tag,
        )))
    );
}

/// Parse symbol into a token
fn token_no_whitespace_symbols(input: &[u8]) -> IResult<&[u8], Token> {
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    nom::branch::alt((
        map(tag(";"), |_| Token::Semicolon),
        map(tag(","), |_| Token::Comma),
        map(tag("+"), |_| Token::Plus),
        map(tag("-"), |_| Token::Minus),
        map(tag("/"), |_| Token::ForwardSlash),
        map(tag("%"), |_| Token::Percent),
        map(tag("*"), |_| Token::Asterix),
        symbol_verticalbar,
        symbol_ampersand,
        map(tag("^"), |_| Token::Hat),
        symbol_equals,
        map(tag("#"), |_| Token::Hash),
        map(tag("@"), |_| Token::At),
        symbol_exclamation,
        map(tag("~"), |_| Token::Tilde),
        map(tag("."), |_| Token::Period),
        map(tag(":"), |_| Token::Colon),
        map(tag("?"), |_| Token::QuestionMark),
    ))(input)
}

/// Parse keyword into a  token
fn token_no_whitespace_words(input: &[u8]) -> IResult<&[u8], Token> {
    use nom::combinator::map;
    nom::branch::alt((
        // Control flow
        nom::branch::alt((
            map(reserved_word_if, |_| Token::If),
            map(reserved_word_else, |_| Token::Else),
            map(reserved_word_for, |_| Token::For),
            map(reserved_word_while, |_| Token::While),
            map(reserved_word_switch, |_| Token::Switch),
            map(reserved_word_case, |_| Token::Case),
            map(reserved_word_default, |_| Token::Default),
            map(reserved_word_return, |_| Token::Return),
            map(reserved_word_break, |_| Token::Break),
            map(reserved_word_continue, |_| Token::Continue),
        )),
        // Types
        map(reserved_word_struct, |_| Token::Struct),
        map(reserved_word_samplerstate, |_| Token::SamplerState),
        map(reserved_word_cbuffer, |_| Token::ConstantBuffer),
        register,
        // Parameter Attributes
        map(reserved_word_inout, |_| Token::InOut),
        map(reserved_word_in, |_| Token::In),
        map(reserved_word_out, |_| Token::Out),
        // Type modifiers
        map(reserved_word_const, |_| Token::Const),
        // Variable storage classes
        map(reserved_word_extern, |_| Token::Extern),
        map(reserved_word_static, |_| Token::Static),
        map(reserved_word_groupshared, |_| Token::GroupShared),
        map(reserved_word_sizeof, |_| Token::SizeOf),
    ))(input)
}

/// Parse any single non-whitespace token - without a location
fn token_no_whitespace_intermediate(input: &[u8]) -> IResult<&[u8], Token> {
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    nom::branch::alt((
        // Literals and identifiers
        map(identifier, |id| Token::Id(id)),
        literal_float,
        literal_int,
        map(reserved_word_true, |_| Token::True),
        map(reserved_word_false, |_| Token::False),
        // Scope markers
        map(tag("{"), |_| Token::LeftBrace),
        map(tag("}"), |_| Token::RightBrace),
        map(tag("("), |_| Token::LeftParen),
        map(tag(")"), |_| Token::RightParen),
        map(tag("["), |_| Token::LeftSquareBracket),
        map(tag("]"), |_| Token::RightSquareBracket),
        leftanglebracket,
        rightanglebracket,
        // Keywords and symbols
        token_no_whitespace_symbols,
        token_no_whitespace_words,
    ))(input)
}

/// Parse any single non-whitespace token - with a location
fn token_no_whitespace(input: &[u8]) -> IResult<&[u8], IntermediateToken> {
    let (remaining, token) = token_no_whitespace_intermediate(input)?;
    let intermediate_token = IntermediateToken(token, IntermediateLocation(input.len() as u64));
    Ok((remaining, intermediate_token))
}

/// Parse a single token
fn token(input: &[u8]) -> IResult<&[u8], IntermediateToken> {
    let (input, _) = skip_whitespace(input)?;
    let (input, token) = token_no_whitespace(input)?;
    let (input, _) = skip_whitespace(input)?;

    Ok((input, token))
}

/// Parse all tokens in a stream
fn token_stream(input: &[u8]) -> IResult<&[u8], Vec<StreamToken>> {
    let total_length = input.len() as u64;
    match nom::multi::many0(nom::combinator::complete(token))(input) {
        Ok((rest, itokens)) => {
            let tokens = itokens
                .into_iter()
                .map(|itoken| StreamToken(itoken.0, StreamLocation(total_length - (itoken.1).0)))
                .collect::<Vec<_>>();
            Ok((rest, tokens))
        }
        Err(err) => Err(err),
    }
}

/// Run the lexer on input text to turn it into a token stream
pub fn lex(preprocessed: &PreprocessedText) -> Result<Tokens, LexError> {
    let code_bytes = preprocessed.as_bytes();
    let total_length = code_bytes.len() as u64;
    match token_stream(code_bytes) {
        Ok((rest, mut stream)) => {
            if rest.is_empty() {
                let stream = {
                    stream.push(StreamToken(Token::Eof, StreamLocation(total_length)));
                    stream
                };
                let mut lex_tokens = Vec::with_capacity(stream.len());
                for StreamToken(ref token, stream_location) in stream {
                    let loc = match preprocessed.get_file_location(stream_location) {
                        Ok(file_location) => file_location,
                        Err(()) => return Err(LexError::Unknown),
                    };
                    lex_tokens.push(LexToken(token.clone(), loc));
                }
                Ok(Tokens { stream: lex_tokens })
            } else {
                // Find the next point where we can find a valid token
                let mut after = rest;
                loop {
                    if after.is_empty() {
                        break;
                    }
                    after = &after[1..];

                    if let Ok((_, token)) = token_no_whitespace(after) {
                        if let IntermediateToken(Token::Id(_), _) = token {
                            // If we find an identifier then it would be a substring of another identifier which didn't lex
                        } else {
                            break;
                        }
                    }

                    if let Ok(_) = whitespace(after) {
                        break;
                    }
                }

                let failing_bytes = rest[..rest.len() - after.len()].to_vec();
                Err(LexError::FailedToParse(failing_bytes))
            }
        }
        Err(nom::Err::Incomplete(_)) => Err(LexError::UnexpectedEndOfStream),
        Err(_) => Err(LexError::Unknown),
    }
}

#[test]
fn test_token() {
    fn from_end(tok: Token, from: u64) -> IntermediateToken {
        IntermediateToken(tok, IntermediateLocation(from))
    }

    assert_eq!(token(&b""[..]), Err(nom::Err::Incomplete(Needed::new(1))));
    assert_eq!(
        token(&b";"[..]),
        Ok((&b""[..], from_end(Token::Semicolon, 1)))
    );
    assert_eq!(
        token(&b" ;"[..]),
        Ok((&b""[..], from_end(Token::Semicolon, 1)))
    );
    assert_eq!(
        token(&b"; "[..]),
        Ok((&b""[..], from_end(Token::Semicolon, 2)))
    );
    assert_eq!(
        token(&b" ; "[..]),
        Ok((&b""[..], from_end(Token::Semicolon, 2)))
    );
    assert_eq!(
        token(&b"name"[..]),
        Ok((
            &b""[..],
            from_end(Token::Id(Identifier("name".to_string())), 4)
        ))
    );

    assert_eq!(
        token(&b"12 "[..]),
        Ok((&b""[..], from_end(Token::LiteralInt(12), 3)))
    );
    assert_eq!(
        token(&b"12u"[..]),
        Ok((&b""[..], from_end(Token::LiteralUInt(12), 3)))
    );
    assert_eq!(
        token(&b"12l"[..]),
        Ok((&b""[..], from_end(Token::LiteralLong(12), 3)))
    );
    assert_eq!(
        token(&b"12L"[..]),
        Ok((&b""[..], from_end(Token::LiteralLong(12), 3)))
    );

    assert_eq!(
        token(&b"1.0f"[..]),
        Ok((&b""[..], from_end(Token::LiteralFloat(1.0f32), 4)))
    );
    assert_eq!(
        token(&b"2.0 "[..]),
        Ok((&b""[..], from_end(Token::LiteralFloat(2.0f32), 4)))
    );
    assert_eq!(
        token(&b"2.0L"[..]),
        Ok((&b""[..], from_end(Token::LiteralDouble(2.0f64), 4)))
    );
    assert_eq!(
        token(&b"0.5h"[..]),
        Ok((&b""[..], from_end(Token::LiteralHalf(0.5f32), 4)))
    );

    assert_eq!(
        token(&b"{"[..]),
        Ok((&b""[..], from_end(Token::LeftBrace, 1)))
    );
    assert_eq!(
        token(&b"}"[..]),
        Ok((&b""[..], from_end(Token::RightBrace, 1)))
    );
    assert_eq!(
        token(&b"("[..]),
        Ok((&b""[..], from_end(Token::LeftParen, 1)))
    );
    assert_eq!(
        token(&b")"[..]),
        Ok((&b""[..], from_end(Token::RightParen, 1)))
    );
    assert_eq!(
        token(&b"["[..]),
        Ok((&b""[..], from_end(Token::LeftSquareBracket, 1)))
    );
    assert_eq!(
        token(&b"]"[..]),
        Ok((&b""[..], from_end(Token::RightSquareBracket, 1)))
    );

    assert_eq!(
        token(&b"< "[..]),
        Ok((
            &b""[..],
            from_end(Token::LeftAngleBracket(FollowedBy::Whitespace), 2)
        ))
    );
    assert_eq!(
        token(&b"> "[..]),
        Ok((
            &b""[..],
            from_end(Token::RightAngleBracket(FollowedBy::Whitespace), 2)
        ))
    );
    assert_eq!(
        token(&b"<< "[..]),
        Ok((
            &b"< "[..],
            from_end(Token::LeftAngleBracket(FollowedBy::Token), 3)
        ))
    );
    assert_eq!(
        token(&b">> "[..]),
        Ok((
            &b"> "[..],
            from_end(Token::RightAngleBracket(FollowedBy::Token), 3)
        ))
    );
    assert_eq!(
        token(&b"<>"[..]),
        Ok((
            &b">"[..],
            from_end(Token::LeftAngleBracket(FollowedBy::Token), 2)
        ))
    );
    assert_eq!(
        token(&b"><"[..]),
        Ok((
            &b"<"[..],
            from_end(Token::RightAngleBracket(FollowedBy::Token), 2)
        ))
    );

    assert_eq!(
        token(&b";"[..]),
        Ok((&b""[..], from_end(Token::Semicolon, 1)))
    );
    assert_eq!(token(&b","[..]), Ok((&b""[..], from_end(Token::Comma, 1))));

    assert_eq!(token(&b"+ "[..]), Ok((&b""[..], from_end(Token::Plus, 2))));
    assert_eq!(token(&b"- "[..]), Ok((&b""[..], from_end(Token::Minus, 2))));
    assert_eq!(
        token(&b"/ "[..]),
        Ok((&b""[..], from_end(Token::ForwardSlash, 2)))
    );
    assert_eq!(
        token(&b"% "[..]),
        Ok((&b""[..], from_end(Token::Percent, 2)))
    );
    assert_eq!(
        token(&b"* "[..]),
        Ok((&b""[..], from_end(Token::Asterix, 2)))
    );
    assert_eq!(
        token(&b"| "[..]),
        Ok((
            &b""[..],
            from_end(Token::VerticalBar(FollowedBy::Whitespace), 2)
        ))
    );
    assert_eq!(
        token(&b"|| "[..]),
        Ok((
            &b"| "[..],
            from_end(Token::VerticalBar(FollowedBy::Token), 3)
        ))
    );
    assert_eq!(
        token(&b"& "[..]),
        Ok((
            &b""[..],
            from_end(Token::Ampersand(FollowedBy::Whitespace), 2)
        ))
    );
    assert_eq!(
        token(&b"&& "[..]),
        Ok((&b"& "[..], from_end(Token::Ampersand(FollowedBy::Token), 3)))
    );
    assert_eq!(token(&b"^ "[..]), Ok((&b""[..], from_end(Token::Hat, 2))));
    assert_eq!(
        token(&b"= "[..]),
        Ok((&b""[..], from_end(Token::Equals, 2)))
    );
    assert_eq!(token(&b"#"[..]), Ok((&b""[..], from_end(Token::Hash, 1))));
    assert_eq!(token(&b"@"[..]), Ok((&b""[..], from_end(Token::At, 1))));
    assert_eq!(
        token(&b"! "[..]),
        Ok((&b""[..], from_end(Token::ExclamationPoint, 2)))
    );
    assert_eq!(token(&b"~"[..]), Ok((&b""[..], from_end(Token::Tilde, 1))));
    assert_eq!(token(&b"."[..]), Ok((&b""[..], from_end(Token::Period, 1))));

    assert_eq!(token(&b"if"[..]), Ok((&b""[..], from_end(Token::If, 2))));
    assert_eq!(
        token(&b"else"[..]),
        Ok((&b""[..], from_end(Token::Else, 4)))
    );
    assert_eq!(token(&b"for"[..]), Ok((&b""[..], from_end(Token::For, 3))));
    assert_eq!(
        token(&b"while"[..]),
        Ok((&b""[..], from_end(Token::While, 5)))
    );
    assert_eq!(
        token(&b"switch"[..]),
        Ok((&b""[..], from_end(Token::Switch, 6)))
    );
    assert_eq!(
        token(&b"return"[..]),
        Ok((&b""[..], from_end(Token::Return, 6)))
    );
    assert_eq!(
        token(&b"break"[..]),
        Ok((&b""[..], from_end(Token::Break, 5)))
    );
    assert_eq!(
        token(&b"continue"[..]),
        Ok((&b""[..], from_end(Token::Continue, 8)))
    );

    assert_eq!(
        token(&b"struct"[..]),
        Ok((&b""[..], from_end(Token::Struct, 6)))
    );
    assert_eq!(
        token(&b"SamplerState"[..]),
        Ok((&b""[..], from_end(Token::SamplerState, 12)))
    );
    assert_eq!(
        token(&b"cbuffer"[..]),
        Ok((&b""[..], from_end(Token::ConstantBuffer, 7)))
    );
    assert_eq!(
        token(&b"register(t4)"[..]),
        Ok((&b""[..], from_end(Token::Register(RegisterSlot::T(4)), 12)))
    );
    assert_eq!(token(&b":"[..]), Ok((&b""[..], from_end(Token::Colon, 1))));
    assert_eq!(
        token(&b"?"[..]),
        Ok((&b""[..], from_end(Token::QuestionMark, 1)))
    );

    assert_eq!(token(&b"in"[..]), Ok((&b""[..], from_end(Token::In, 2))));
    assert_eq!(token(&b"out"[..]), Ok((&b""[..], from_end(Token::Out, 3))));
    assert_eq!(
        token(&b"inout"[..]),
        Ok((&b""[..], from_end(Token::InOut, 5)))
    );

    assert_eq!(
        token(&b"const"[..]),
        Ok((&b""[..], from_end(Token::Const, 5)))
    );

    assert_eq!(
        token(&b"extern"[..]),
        Ok((&b""[..], from_end(Token::Extern, 6)))
    );
    assert_eq!(
        token(&b"static"[..]),
        Ok((&b""[..], from_end(Token::Static, 6)))
    );
    assert_eq!(
        token(&b"groupshared"[..]),
        Ok((&b""[..], from_end(Token::GroupShared, 11)))
    );

    assert_eq!(
        token(&b"structName"[..]),
        Ok((
            &b""[..],
            from_end(Token::Id(Identifier("structName".to_string())), 10)
        ))
    );
}

#[test]
fn test_token_stream() {
    assert_eq!(token_stream(&b""[..]), Ok((&b""[..], vec![])));

    fn token_id(name: &'static str, loc: u64) -> StreamToken {
        StreamToken(Token::Id(Identifier(name.to_string())), StreamLocation(loc))
    }
    fn loc(tok: Token, loc: u64) -> StreamToken {
        StreamToken(tok, StreamLocation(loc))
    }

    assert_eq!(
        token_stream(&b" a "[..]),
        Ok((&b""[..], vec![token_id("a", 1),]))
    );

    assert_eq!(
        token_stream(&b"void func();"[..]),
        Ok((
            &b""[..],
            vec![
                token_id("void", 0),
                token_id("func", 5),
                loc(Token::LeftParen, 9),
                loc(Token::RightParen, 10),
                loc(Token::Semicolon, 11),
            ]
        ))
    );

    assert_eq!(
        token_stream(&b"-12 "[..]),
        Ok((
            &b""[..],
            vec![loc(Token::Minus, 0), loc(Token::LiteralInt(12), 1),]
        ))
    );
    assert_eq!(
        token_stream(&b"-12l"[..]),
        Ok((
            &b""[..],
            vec![loc(Token::Minus, 0), loc(Token::LiteralLong(12), 1),]
        ))
    );
    assert_eq!(
        token_stream(&b"-12L"[..]),
        Ok((
            &b""[..],
            vec![loc(Token::Minus, 0), loc(Token::LiteralLong(12), 1),]
        ))
    );

    assert_eq!(
        token_stream(&b"<<"[..]),
        Ok((
            &b""[..],
            vec![
                loc(Token::LeftAngleBracket(FollowedBy::Token), 0),
                loc(Token::LeftAngleBracket(FollowedBy::Whitespace), 1),
            ]
        ))
    );
    assert_eq!(
        token_stream(&b"<"[..]),
        Ok((
            &b""[..],
            vec![loc(Token::LeftAngleBracket(FollowedBy::Whitespace), 0),]
        ))
    );
    assert_eq!(
        token_stream(&b"< "[..]),
        Ok((
            &b""[..],
            vec![loc(Token::LeftAngleBracket(FollowedBy::Whitespace), 0),]
        ))
    );

    assert_eq!(
        token_stream(&b">>"[..]),
        Ok((
            &b""[..],
            vec![
                loc(Token::RightAngleBracket(FollowedBy::Token), 0),
                loc(Token::RightAngleBracket(FollowedBy::Whitespace), 1),
            ]
        ))
    );
    assert_eq!(
        token_stream(&b">"[..]),
        Ok((
            &b""[..],
            vec![loc(Token::RightAngleBracket(FollowedBy::Whitespace), 0),]
        ))
    );
    assert_eq!(
        token_stream(&b"> "[..]),
        Ok((
            &b""[..],
            vec![loc(Token::RightAngleBracket(FollowedBy::Whitespace), 0),]
        ))
    );
}
