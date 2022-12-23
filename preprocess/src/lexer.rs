use rssl_text::tokens::*;
use rssl_text::*;

/// Provides details on why a lex operation failed
#[derive(PartialEq, Clone)]
pub struct LexerError {
    pub reason: LexerErrorReason,
    pub location: SourceLocation,
}

/// The basic reason for a lex failure
#[derive(PartialEq, Clone)]
#[allow(clippy::derive_partial_eq_without_eq)]
pub enum LexerErrorReason {
    Unknown,
    FailedToParse(Vec<u8>),
    UnexpectedEndOfStream,
}

impl LexerError {
    /// Create a new lexer error
    pub fn new(reason: LexerErrorReason, location: SourceLocation) -> Self {
        LexerError { reason, location }
    }
}

impl std::fmt::Debug for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.reason {
            LexerErrorReason::Unknown => write!(f, "Unknown"),
            LexerErrorReason::FailedToParse(ref data) => match std::str::from_utf8(data) {
                Ok(friendly) => {
                    let substr = match friendly.find('\n') {
                        Some(index) => &friendly[..index],
                        None => friendly,
                    };
                    write!(f, "FailedToParse(\"{}\")", substr)
                }
                Err(_) => write!(f, "FailedToParse({:?})", data),
            },
            LexerErrorReason::UnexpectedEndOfStream => write!(f, "UnexpectedEndOfStream"),
        }
    }
}

impl std::fmt::Display for LexerErrorReason {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            LexerErrorReason::Unknown => write!(f, "unknown lexer error"),
            LexerErrorReason::FailedToParse(_) => write!(f, "unexpected character"),
            LexerErrorReason::UnexpectedEndOfStream => write!(f, "unexpected end of stream"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
struct IntermediateLocation(u32);

#[derive(PartialEq, Debug, Clone)]
struct IntermediateToken(Token, IntermediateLocation);

#[derive(PartialEq, Debug, Clone)]
struct StreamToken(pub Token, pub StreamLocation);

/// Internal error kind when a lexer fails to lex
#[derive(PartialEq, Debug, Clone)]
enum LexErrorKind {
    UnexpectedBytes,
    OtherTokenBytes,
    Eof,
    StringWrapsLine,
    StringWrapsFile,
    StringContainsInvalidCharacters,
}

/// Internal error data when a lexer fails to lex
#[derive(PartialEq, Debug, Clone)]
struct LexErrorContext<'b>(&'b [u8], LexErrorKind);

/// Internal error result type
type LexResult<'b, O> = Result<(&'b [u8], O), LexErrorContext<'b>>;

/// Make an error for when the wrong characters were encountered to parse a certain token
fn wrong_chars<T>(input: &[u8]) -> LexResult<T> {
    Err(LexErrorContext(input, LexErrorKind::UnexpectedBytes))
}

/// Make an error for when the characters are encountered which indicate we are another token
fn invalid_chars<T>(input: &[u8]) -> LexResult<T> {
    Err(LexErrorContext(input, LexErrorKind::OtherTokenBytes))
}

/// Make an error when the end of stream was encountered while trying to lex a certain token
fn end_of_stream<T>() -> LexResult<'static, T> {
    Err(LexErrorContext(&[], LexErrorKind::Eof))
}

/// Lex a token or return none
fn opt<'b, T>(
    lex_fn: impl Fn(&[u8]) -> LexResult<T>,
) -> impl Fn(&'b [u8]) -> LexResult<'b, Option<T>> {
    move |input: &'b [u8]| {
        match lex_fn(input) {
            // If we succeeded then return the element
            Ok((rest, element)) => Ok((rest, Some(element))),
            Err(_) => Ok((input, None)),
        }
    }
}

/// Lex a token and apply a function to the result
fn map<'b, T, G>(
    lex_fn: impl Fn(&[u8]) -> LexResult<T>,
    map_fn: impl Fn(T) -> G,
) -> impl Fn(&'b [u8]) -> LexResult<'b, G> {
    move |input: &'b [u8]| match lex_fn(input) {
        Ok((rest, element)) => Ok((rest, map_fn(element))),
        Err(err) => Err(err),
    }
}

type DynLexFn<'f, T> = &'f dyn Fn(&[u8]) -> LexResult<T>;

/// Lex a token from a set of lexers
fn choose<'b, T: std::fmt::Debug>(lex_fns: &[DynLexFn<T>], input: &'b [u8]) -> LexResult<'b, T> {
    for lex_fn in lex_fns {
        if let Ok(ok) = lex_fn(input) {
            return Ok(ok);
        }
    }
    wrong_chars(input)
}

/// Parse a single decimal digit
fn digit(input: &[u8]) -> LexResult<u64> {
    // Handle end of stream
    if input.is_empty() {
        return end_of_stream();
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
            return wrong_chars(input);
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse multiple decimal digits into a 64-bit value
fn digits(input: &[u8]) -> LexResult<u64> {
    let (mut input, mut value) = digit(input)?;
    while let Ok((next_input, d)) = digit(input) {
        input = next_input;
        value *= 10;
        value += d;
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
fn digit_hex(input: &[u8]) -> LexResult<u64> {
    // Handle end of stream
    if input.is_empty() {
        return end_of_stream();
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
            return wrong_chars(input);
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse multiple hexadecimal digits into a 64-bit value
fn digits_hex(input: &[u8]) -> LexResult<u64> {
    let (mut input, mut value) = digit_hex(input)?;
    while let Ok((next_input, d)) = digit_hex(input) {
        input = next_input;
        value *= 16;
        value += d;
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
fn digit_octal(input: &[u8]) -> LexResult<u64> {
    // Handle end of stream
    if input.is_empty() {
        return end_of_stream();
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
            return wrong_chars(input);
        }
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse multiple octal digits into a 64-bit value
fn digits_octal(input: &[u8]) -> LexResult<u64> {
    let (mut input, mut value) = digit_octal(input)?;
    while let Ok((next_input, d)) = digit_octal(input) {
        input = next_input;
        value *= 8;
        value += d;
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
fn int_type(input: &[u8]) -> LexResult<IntType> {
    // Match on the first character
    let n = match input.first() {
        Some(b'u') | Some(b'U') => IntType::UInt,
        Some(b'l') | Some(b'L') => IntType::Long,
        _ => return wrong_chars(input),
    };

    // Success
    Ok((&input[1..], n))
}

/// Parse a decimal literal
fn literal_decimal_int(input: &[u8]) -> LexResult<Token> {
    let (input, value) = digits(input)?;
    let (input, int_type_opt) = opt(int_type)(input)?;
    let token = match int_type_opt {
        None => Token::LiteralInt(value),
        Some(IntType::UInt) => Token::LiteralUInt(value),
        Some(IntType::Long) => Token::LiteralLong(value),
    };
    Ok((input, token))
}

/// Parse a hexadecimal literal
fn literal_hex_int(input: &[u8]) -> LexResult<Token> {
    let (input, value) = digits_hex(input)?;
    let (input, int_type_opt) = opt(int_type)(input)?;
    let token = match int_type_opt {
        None => Token::LiteralInt(value),
        Some(IntType::UInt) => Token::LiteralUInt(value),
        Some(IntType::Long) => Token::LiteralLong(value),
    };
    Ok((input, token))
}

/// Parse an octal literal
fn literal_octal_int(input: &[u8]) -> LexResult<Token> {
    let (input, value) = digits_octal(input)?;
    let (input, int_type_opt) = opt(int_type)(input)?;
    let token = match int_type_opt {
        None => Token::LiteralInt(value),
        Some(IntType::UInt) => Token::LiteralUInt(value),
        Some(IntType::Long) => Token::LiteralLong(value),
    };
    Ok((input, token))
}

/// Parse an integer literal
fn literal_int(input: &[u8]) -> LexResult<Token> {
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

/// Parse a literal string
fn literal_string(input: &[u8]) -> LexResult<Token> {
    if let Some((b'"', rest)) = input.split_first() {
        let end_res = rest.iter().position(|c| *c == b'"');
        match end_res {
            Some(pos) => {
                let (range, remaining) = input.split_at(pos + 2);
                // TODO: We do not currently support any special characters
                let range_no_quotes = &range[1..pos + 1];
                match std::str::from_utf8(range_no_quotes) {
                    Ok(string_utf8) => {
                        if string_utf8.contains('\n') {
                            Err(LexErrorContext(input, LexErrorKind::StringWrapsLine))
                        } else {
                            Ok((remaining, Token::LiteralString(string_utf8.to_string())))
                        }
                    }
                    Err(_) => Err(LexErrorContext(
                        input,
                        LexErrorKind::StringContainsInvalidCharacters,
                    )),
                }
            }
            None => Err(LexErrorContext(input, LexErrorKind::StringWrapsFile)),
        }
    } else {
        wrong_chars(input)
    }
}

#[test]
fn test_literal_string() {
    let p = literal_string;
    assert_eq!(
        p(b"\"\""),
        Ok((&b""[..], Token::LiteralString("".to_string())))
    );
    assert_eq!(
        p(b"\"abc\""),
        Ok((&b""[..], Token::LiteralString("abc".to_string())))
    );
    assert_eq!(
        p(b"\"a\nb\""),
        Err(LexErrorContext(b"\"a\nb\"", LexErrorKind::StringWrapsLine))
    );
    assert_eq!(
        p(b"\""),
        Err(LexErrorContext(b"\"", LexErrorKind::StringWrapsFile))
    );
    assert_eq!(
        p(b"\"\n"),
        Err(LexErrorContext(b"\"\n", LexErrorKind::StringWrapsFile))
    );
}

type DigitSequence = Vec<u64>;

/// Parse a sequence of digits into an array
fn digit_sequence(input: &[u8]) -> LexResult<DigitSequence> {
    let (mut input, first) = digit(input)?;
    let mut digits = Vec::from([first]);
    while let Ok((rest, next)) = digit(input) {
        input = rest;
        digits.push(next);
    }
    Ok((input, digits))
}

#[derive(PartialEq, Debug, Clone)]
struct Fraction(DigitSequence, DigitSequence);

/// Parse the main fractional parts of a float literal
fn fractional_constant(input: &[u8]) -> LexResult<Fraction> {
    let (input, whole_part) = opt(digit_sequence)(input)?;
    let (input, _) = specific_text(input, ".")?;

    // If there was not a whole part then the fractional part is mandatory
    let (input, fractional_part) = if whole_part.is_none() {
        map(digit_sequence, Some)(input)?
    } else {
        opt(digit_sequence)(input)?
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
fn float_type(input: &[u8]) -> LexResult<FloatType> {
    // Match on the first character
    let n = match input.first() {
        Some(b'h') | Some(b'H') => FloatType::Half,
        Some(b'f') | Some(b'F') => FloatType::Float,
        Some(b'l') | Some(b'L') => FloatType::Double,
        _ => return wrong_chars(input),
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
fn sign(input: &[u8]) -> LexResult<Sign> {
    match input.first() {
        Some(b'+') => Ok((&input[1..], Sign::Positive)),
        Some(b'-') => Ok((&input[1..], Sign::Negative)),
        _ => wrong_chars(input),
    }
}

/// Exponent value
#[derive(PartialEq, Debug, Clone)]
struct Exponent(i64);

/// Parse an exponent in a float literal
fn float_exponent(input: &[u8]) -> LexResult<Exponent> {
    let input = match input {
        [b'e', ..] | [b'E', ..] => &input[1..],
        _ => return wrong_chars(input),
    };
    let (input, s_opt) = opt(sign)(input)?;
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

    assert_eq!(p(b""), wrong_chars(b""));
    assert_eq!(p(b"."), wrong_chars(b"."));
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
        left_combined *= 10f64;
        left_combined += digit as f64;
    }
    let left_float = left_combined as f64;

    let mut right_combined = 0f64;
    let right_len = right.len();
    for digit in right {
        right_combined *= 10f64;
        right_combined += digit as f64;
    }
    let mut right_float = right_combined as f64;
    for _ in 0..right_len {
        right_float /= 10f64;
    }

    let mantissa = left_float + right_float;
    let mut value64 = mantissa;
    if exponent > 0 {
        for _ in 0..exponent {
            value64 *= 10f64;
        }
    } else {
        for _ in 0..(-exponent) {
            value64 /= 10f64;
        }
    }

    match float_type.unwrap_or(FloatType::Float) {
        FloatType::Half => Token::LiteralHalf(value64 as f32),
        FloatType::Float => Token::LiteralFloat(value64 as f32),
        FloatType::Double => Token::LiteralDouble(value64),
    }
}

/// Parse a float literal
fn literal_float(input: &[u8]) -> LexResult<Token> {
    // First try to parse a fraction
    let (input, fraction) = opt(fractional_constant)(input)?;

    // Then if that failed try to parse as a whole number
    let has_fraction = fraction.is_some();
    let (input, fraction) = match fraction {
        Some(f) => (input, f),
        None => {
            let (input, whole_number) = digit_sequence(input)?;
            (input, Fraction(whole_number, Vec::new()))
        }
    };

    let (input, exponent_opt) = opt(float_exponent)(input)?;

    // If we did not have a fractional part then we require the exponent, else it is optional
    // This avoids integers parsing as valid floats
    if !has_fraction && exponent_opt.is_none() {
        return invalid_chars(b".");
    }

    let (input, float_type) = opt(float_type)(input)?;

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

    assert_eq!(p(b"0.;"), Ok((&b";"[..], Token::LiteralFloat(0.0))));
    assert_eq!(p(b".0;"), Ok((&b";"[..], Token::LiteralFloat(0.0))));
    assert_eq!(p(b"0."), Ok((&b""[..], Token::LiteralFloat(0.0))));
    assert_eq!(p(b".0"), Ok((&b""[..], Token::LiteralFloat(0.0))));

    assert_eq!(p(b"7E-7"), Ok((&b""[..], Token::LiteralFloat(7e-7))));
    assert_eq!(p(b"1e+11"), Ok((&b""[..], Token::LiteralFloat(1e+11))));
    assert_eq!(
        p(b"4.863e+11"),
        Ok((&b""[..], Token::LiteralFloat(4.863e+11)))
    );

    assert!(p(b"0").is_err());
    assert!(p(b".").is_err());
}

/// Parse the first character of an identifier
fn identifier_firstchar(input: &[u8]) -> LexResult<u8> {
    if input.is_empty() {
        end_of_stream()
    } else {
        let byte = input[0];
        match byte as char {
            'A'..='Z' | 'a'..='z' | '_' => Ok((&input[1..], byte)),
            _ => wrong_chars(input),
        }
    }
}

/// Parse characters in an identifier after the first
fn identifier_char(input: &[u8]) -> LexResult<u8> {
    if input.is_empty() {
        end_of_stream()
    } else {
        let byte = input[0];
        match byte as char {
            'A'..='Z' | 'a'..='z' | '_' | '0'..='9' => Ok((&input[1..], byte)),
            _ => wrong_chars(input),
        }
    }
}

/// Parse an identifier or a keyword as an identifier
fn identifier(input: &[u8]) -> LexResult<Identifier> {
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

    Ok((
        stream,
        Identifier(std::str::from_utf8(&chars[..]).unwrap().to_string()),
    ))
}

/// Parse an identifier or keyword
fn any_word(input: &[u8]) -> LexResult<Token> {
    let (stream, id) = identifier(input)?;

    let tok = match id.0.as_str() {
        "if" => Token::If,
        "else" => Token::Else,
        "for" => Token::For,
        "while" => Token::While,
        "switch" => Token::Switch,
        "return" => Token::Return,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "discard" => Token::Discard,

        "struct" => Token::Struct,
        "enum" => Token::Enum,
        "typedef" => Token::Typedef,
        "cbuffer" => Token::ConstantBuffer,
        "register" => Token::Register,
        "packoffset" => Token::PackOffset,
        "namespace" => Token::Namespace,

        "true" => Token::True,
        "false" => Token::False,

        "in" => Token::In,
        "out" => Token::Out,
        "inout" => Token::InOut,

        "const" => Token::Const,
        "volatile" => Token::Volatile,
        "row_major" => Token::RowMajor,
        "column_major" => Token::ColumnMajor,
        "unorm" => Token::Unorm,
        "snorm" => Token::Snorm,

        "extern" => Token::Extern,
        "static" => Token::Static,
        "groupshared" => Token::GroupShared,
        "sizeof" => Token::SizeOf,
        "template" => Token::Template,
        "typename" => Token::Typename,

        // Unimplemented keywords
        "case" | "default" => Token::ReservedWord(id.0),

        // Reserved keywords for future use
        "auto" | "catch" | "char" | "class" | "const_cast" | "delete" | "dynamic_cast"
        | "explicit" | "friend" | "goto" | "long" | "mutable" | "new" | "operator" | "private"
        | "protected" | "public" | "reinterpret_cast" | "short" | "signed" | "static_cast"
        | "this" | "throw" | "try" | "union" | "unsigned" | "using" | "virtual" => {
            Token::ReservedWord(id.0)
        }

        _ => Token::Id(id),
    };
    Ok((stream, tok))
}

/// Parse a specific string of characters
fn specific_text<'a>(input: &'a [u8], text: &'static str) -> LexResult<'a, &'a [u8]> {
    let text_bytes = text.as_bytes();
    if input.starts_with(text_bytes) {
        let (k, r) = input.split_at(text_bytes.len());
        Ok((r, k))
    } else {
        wrong_chars(input)
    }
}

/// Parse trivial whitespace
fn whitespace_simple(input: &[u8]) -> LexResult<Token> {
    match input {
        [b' ', rest @ ..] | [b'\t', rest @ ..] => Ok((rest, Token::Whitespace)),
        _ => wrong_chars(input),
    }
}

/// Parse trivial whitespace
fn whitespace_endline(input: &[u8]) -> LexResult<Token> {
    match input {
        // File has an actual line ending but it is ignored and treated as normal whitespace
        [b'\\', b'\r', b'\n', rest @ ..] | [b'\\', b'\n', rest @ ..] => {
            Ok((rest, Token::PhysicalEndline))
        }
        // A normal line ending
        [b'\r', b'\n', rest @ ..] | [b'\n', rest @ ..] => Ok((rest, Token::Endline)),
        _ => wrong_chars(input),
    }
}

/// Parse a line comment
fn line_comment(input: &[u8]) -> LexResult<Token> {
    if input.starts_with(b"//") {
        let mut pos = 2;
        while pos < input.len() {
            let input_at_pos = &input[pos..];
            match whitespace_endline(&input[pos..]) {
                Ok((_, Token::Endline)) => return Ok((input_at_pos, Token::Comment)),
                Ok((rest, Token::PhysicalEndline)) => pos = input.len() - rest.len(),
                _ => pos += 1,
            }
        }
        Ok((&[], Token::Comment))
    } else {
        wrong_chars(input)
    }
}

/// Parse a block comment
fn block_comment(input: &[u8]) -> LexResult<Token> {
    if input.starts_with(b"/*") {
        // Find the end of the block
        // We do not supported nested blocks
        let mut search = &input[2..];
        loop {
            if search.len() < 2 {
                break;
            }
            if search.starts_with(b"*/") {
                return Ok((&search[2..], Token::Comment));
            }
            search = &search[1..];
        }

        // Comment goes off the end of the file
        end_of_stream()
    } else {
        // Not a block comment
        wrong_chars(input)
    }
}

#[test]
fn test_whitespace() {
    let end = |t: Token| Ok((&[][..], t));

    // Empty string is not any kind of space
    assert!(whitespace_simple(b"").is_err());
    assert!(line_comment(b"").is_err());
    assert!(block_comment(b"").is_err());
    assert_eq!(whitespace_simple(b" "), end(Token::Whitespace));
    assert_eq!(line_comment(b"//\n"), Ok((&b"\n"[..], Token::Comment)));
    assert_eq!(
        line_comment(b"// comment\n"),
        Ok((&b"\n"[..], Token::Comment))
    );
    assert_eq!(block_comment(b"/* comment */"), end(Token::Comment));
    assert_eq!(
        block_comment(b"/* line 1\n\t line 2\n\t line 3 */"),
        end(Token::Comment)
    );
    assert_eq!(
        block_comment(b"/* line 1\n\t star *\n\t line 3 */"),
        end(Token::Comment)
    );
    assert_eq!(
        block_comment(b"/* line 1\n\t slash /\n\t line 3 */"),
        end(Token::Comment)
    );
}

/// Peek at what token is coming next unless there is whitespace
fn lookahead_token(input: &[u8]) -> LexResult<Option<Token>> {
    match token_intermediate(input) {
        Ok((_, o)) => Ok((input, Some(o))),
        Err(_) => Ok((input, None)),
    }
}

/// Parse a < token
fn leftanglebracket(input: &[u8]) -> LexResult<Token> {
    match input.first() {
        Some(b'<') => {
            let input = &input[1..];
            let token = match lookahead_token(input)?.1 {
                Some(tok) if !tok.is_whitespace() => Token::LeftAngleBracket(FollowedBy::Token),
                _ => Token::LeftAngleBracket(FollowedBy::Whitespace),
            };
            Ok((input, token))
        }
        _ => wrong_chars(input),
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
    assert_eq!(p(b""), wrong_chars(b""));
    assert_eq!(p(b" "), wrong_chars(b" "));
}

/// Parse a > token
fn rightanglebracket(input: &[u8]) -> LexResult<Token> {
    match input.first() {
        Some(b'>') => {
            let input = &input[1..];
            let token = match lookahead_token(input)?.1 {
                Some(tok) if !tok.is_whitespace() => Token::RightAngleBracket(FollowedBy::Token),
                _ => Token::RightAngleBracket(FollowedBy::Whitespace),
            };
            Ok((input, token))
        }
        _ => wrong_chars(input),
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
    assert_eq!(p(b""), wrong_chars(b""));
    assert_eq!(p(b" "), wrong_chars(b" "));
}

/// Parse a single character symbol into a token
fn symbol_single(op_char: u8, op_token: Token) -> impl Fn(&[u8]) -> LexResult<Token> {
    move |input: &[u8]| match input {
        [c, ..] if *c == op_char => Ok((&input[1..], op_token.clone())),
        _ => wrong_chars(input),
    }
}

/// Parse a binary operation that can either be standalone or combined into an assignment operation
fn symbol_op_or_op_equals(
    op_char: u8,
    op_token: Token,
    op_equals_token: Token,
    op_op_token: Token,
) -> impl Fn(&[u8]) -> LexResult<Token> {
    move |input: &[u8]| match input {
        [c, b'=', ..] if *c == op_char && op_equals_token != Token::Eof => {
            Ok((&input[2..], op_equals_token.clone()))
        }
        [c1, c2, ..] if *c1 == op_char && *c2 == op_char && op_op_token != Token::Eof => {
            Ok((&input[2..], op_op_token.clone()))
        }
        [c, ..] if *c == op_char => Ok((&input[1..], op_token.clone())),
        _ => wrong_chars(input),
    }
}

/// Parse a = or == token
fn symbol_equals(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'=', Token::Equals, Token::EqualsEquals, Token::Eof)(input)
}

/// Parse a # or ## token
fn symbol_hash(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'#', Token::Hash, Token::Eof, Token::HashHash)(input)
}

/// Parse a : or :: token
fn symbol_colon(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b':', Token::Colon, Token::Eof, Token::ScopeResolution)(input)
}

#[test]
fn test_symbol_equals() {
    let p = symbol_equals;
    assert_eq!(p(b"="), Ok((&b""[..], Token::Equals)));
    assert_eq!(p(b"= "), Ok((&b" "[..], Token::Equals)));
    assert_eq!(p(b"=="), Ok((&b""[..], Token::EqualsEquals)));
    assert_eq!(p(b"== "), Ok((&b" "[..], Token::EqualsEquals)));
    assert_eq!(p(b""), wrong_chars(b""));
    assert_eq!(p(b" "), wrong_chars(b" "));
    assert_eq!(p(b"==="), Ok((&b"="[..], Token::EqualsEquals)));
}

/// Parse a + token
fn symbol_plus(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'+', Token::Plus, Token::PlusEquals, Token::PlusPlus)(input)
}

/// Parse a - token
fn symbol_minus(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'-', Token::Minus, Token::MinusEquals, Token::MinusMinus)(input)
}

/// Parse a / token
fn symbol_forward_slash(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(
        b'/',
        Token::ForwardSlash,
        Token::ForwardSlashEquals,
        Token::Eof,
    )(input)
}

/// Parse a % token
fn symbol_percent(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'%', Token::Percent, Token::PercentEquals, Token::Eof)(input)
}

/// Parse a * token
fn symbol_asterix(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'*', Token::Asterix, Token::AsterixEquals, Token::Eof)(input)
}

/// Parse a & token
fn symbol_ampersand(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(
        b'&',
        Token::Ampersand,
        Token::AmpersandEquals,
        Token::AmpersandAmpersand,
    )(input)
}

/// Parse a | token
fn symbol_verticalbar(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(
        b'|',
        Token::VerticalBar,
        Token::VerticalBarEquals,
        Token::VerticalBarVerticalBar,
    )(input)
}

/// Parse a ^ token
fn symbol_hat(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(b'^', Token::Hat, Token::HatEquals, Token::Eof)(input)
}

/// Parse a ! or != token
fn symbol_exclamation(input: &[u8]) -> LexResult<Token> {
    symbol_op_or_op_equals(
        b'!',
        Token::ExclamationPoint,
        Token::ExclamationPointEquals,
        Token::Eof,
    )(input)
}

#[test]
fn test_symbol_exclamation() {
    // Test covers implementation used by ! % * /
    let p = symbol_exclamation;
    assert_eq!(p(b"!"), Ok((&b""[..], Token::ExclamationPoint)));
    assert_eq!(p(b"! "), Ok((&b" "[..], Token::ExclamationPoint)));
    assert_eq!(p(b"!="), Ok((&b""[..], Token::ExclamationPointEquals)));
    assert_eq!(p(b"!= "), Ok((&b" "[..], Token::ExclamationPointEquals)));
    assert_eq!(p(b""), wrong_chars(b""));
    assert_eq!(p(b" "), wrong_chars(b" "));
    assert_eq!(p(b"!=="), Ok((&b"="[..], Token::ExclamationPointEquals)));
}

#[test]
fn test_symbol_ampersand() {
    // Test covers implementation used by + - && ||
    let p = symbol_ampersand;
    assert_eq!(p(b"&"), Ok((&b""[..], Token::Ampersand)));
    assert_eq!(p(b"& "), Ok((&b" "[..], Token::Ampersand)));
    assert_eq!(p(b"&="), Ok((&b""[..], Token::AmpersandEquals)));
    assert_eq!(p(b"&= "), Ok((&b" "[..], Token::AmpersandEquals)));
    assert_eq!(p(b"&&"), Ok((&b""[..], Token::AmpersandAmpersand)));
    assert_eq!(p(b"&& "), Ok((&b" "[..], Token::AmpersandAmpersand)));
    assert_eq!(p(b""), wrong_chars(b""));
    assert_eq!(p(b" "), wrong_chars(b" "));
}

/// Parse symbol into a token
fn token_no_whitespace_symbols(input: &[u8]) -> LexResult<Token> {
    choose(
        &[
            &symbol_single(b';', Token::Semicolon),
            &symbol_single(b',', Token::Comma),
            &symbol_plus,
            &symbol_minus,
            &symbol_forward_slash,
            &symbol_percent,
            &symbol_asterix,
            &symbol_ampersand,
            &symbol_verticalbar,
            &symbol_hat,
            &symbol_exclamation,
            &symbol_equals,
            &symbol_hash,
            &symbol_single(b'@', Token::At),
            &symbol_single(b'~', Token::Tilde),
            &symbol_single(b'.', Token::Period),
            &symbol_colon,
            &symbol_single(b'?', Token::QuestionMark),
        ],
        input,
    )
}

/// Parse a single token - without a location
fn token_intermediate(input: &[u8]) -> LexResult<Token> {
    choose(
        &[
            // Whitespace
            &whitespace_simple,
            &whitespace_endline,
            &line_comment,
            &block_comment,
            // Literals
            &literal_float,
            &literal_int,
            &literal_string,
            // Scope markers
            &symbol_single(b'{', Token::LeftBrace),
            &symbol_single(b'}', Token::RightBrace),
            &symbol_single(b'(', Token::LeftParen),
            &symbol_single(b')', Token::RightParen),
            &symbol_single(b'[', Token::LeftSquareBracket),
            &symbol_single(b']', Token::RightSquareBracket),
            &leftanglebracket,
            &rightanglebracket,
            // Symbols
            &token_no_whitespace_symbols,
            // Identifiers and keywords
            &any_word,
        ],
        input,
    )
}

/// Parse a single token
fn token(input: &[u8]) -> LexResult<IntermediateToken> {
    let (remaining, token) = token_intermediate(input)?;
    let intermediate_token = IntermediateToken(token, IntermediateLocation(input.len() as u32));
    Ok((remaining, intermediate_token))
}

/// Parse all tokens in a stream
fn token_stream(mut input: &[u8]) -> LexResult<Vec<StreamToken>> {
    let total_length = input.len() as u32;
    let mut tokens = Vec::new();

    while !input.is_empty() {
        match token(input) {
            Ok((rest, itoken)) => {
                input = rest;
                tokens.push(StreamToken(
                    itoken.0,
                    StreamLocation(total_length - (itoken.1).0),
                ))
            }
            Err(err) => return Err(err),
        }
    }
    Ok((input, tokens))
}

/// Run the lexer on input text to turn it into a token stream
pub fn lex(text: &str, source_offset: SourceLocation) -> Result<Vec<PreprocessToken>, LexerError> {
    lex_internal(text, source_offset, true)
}

/// Run the lexer on input text fragment to turn it into a token stream
#[cfg(test)]
pub fn lex_fragment(
    file_id: FileId,
    source_manager: &SourceManager,
) -> Result<Vec<PreprocessToken>, LexerError> {
    let contents = source_manager.get_contents(file_id);
    let offset = source_manager.get_source_location_from_file_offset(file_id, StreamLocation(0));
    lex_internal(contents, offset, false)
}

/// Run the lexer on input text to turn it into a token stream
fn lex_internal(
    text: &str,
    source_offset: SourceLocation,
    add_file_ending: bool,
) -> Result<Vec<PreprocessToken>, LexerError> {
    let code_bytes = text.as_bytes();
    let total_length = code_bytes.len() as u32;
    match token_stream(code_bytes) {
        Ok((rest, mut stream)) => {
            if rest.is_empty() {
                if add_file_ending {
                    // Insert a newline at the end of the file if it did not already end with a new empty line
                    // This is what C++ does before token generation normally
                    // This ensures when we return from the included file we are on a fresh line
                    if let Some(StreamToken(tok, _)) = stream.last() {
                        if *tok != Token::Endline {
                            stream.push(StreamToken(Token::Endline, StreamLocation(total_length)));
                        }
                    }
                }

                // Translate from locations in the current local stream into original source locations
                let mut lex_tokens = Vec::with_capacity(stream.len());
                for (i, StreamToken(ref token, stream_location)) in stream.iter().enumerate() {
                    let next_location = match stream.get(i + 1) {
                        Some(StreamToken(_, next_location)) => *next_location,
                        None => StreamLocation(code_bytes.len() as u32),
                    };
                    lex_tokens.push(PreprocessToken::new(
                        token.clone(),
                        source_offset,
                        stream_location.0,
                        next_location.0,
                    ));
                }

                Ok(lex_tokens)
            } else {
                // Find the next point where we can find a valid token
                let mut after = rest;
                loop {
                    if after.is_empty() {
                        break;
                    }
                    after = &after[1..];

                    if let Ok((_, token)) = token(after) {
                        if let IntermediateToken(Token::Id(_), _) = token {
                            // If we find an identifier then it would be a substring of another identifier which didn't lex
                        } else {
                            break;
                        }
                    }
                }

                let failing_bytes = rest[..rest.len() - after.len()].to_vec();
                let offset = StreamLocation((code_bytes.len() - rest.len()) as u32);
                Err(LexerError::new(
                    LexerErrorReason::FailedToParse(failing_bytes),
                    source_offset.offset(offset.0),
                ))
            }
        }
        Err(LexErrorContext(input, kind)) => {
            if kind == LexErrorKind::Eof {
                Err(LexerError::new(
                    LexerErrorReason::UnexpectedEndOfStream,
                    source_offset.offset(code_bytes.len() as u32),
                ))
            } else {
                Err(LexerError::new(
                    LexerErrorReason::Unknown,
                    source_offset.offset((code_bytes.len() - input.len()) as u32),
                ))
            }
        }
    }
}

#[test]
fn test_token() {
    macro_rules! assert_token {
        ($input:expr, $token:expr) => {
            assert_token!($input, $token, $input.as_bytes().len())
        };

        ($input:expr, $token:expr, $used:expr) => {
            let input_bytes = $input.as_bytes();
            let result = token(input_bytes);
            let rest = &input_bytes[$used..];
            assert_eq!(
                result,
                Ok((
                    rest,
                    IntermediateToken($token, IntermediateLocation(input_bytes.len() as u32))
                ))
            );
        };
    }

    assert!(token(&b""[..]).is_err());
    assert_token!(";", Token::Semicolon);
    assert_token!("; ", Token::Semicolon, 1);
    assert_token!("name", Token::Id(Identifier("name".to_string())));

    assert_token!("12 ", Token::LiteralInt(12), 2);
    assert_token!("12u", Token::LiteralUInt(12));
    assert_token!("12l", Token::LiteralLong(12));
    assert_token!("12L", Token::LiteralLong(12));

    assert_token!("1.0f", Token::LiteralFloat(1.0f32));
    assert_token!("2.0 ", Token::LiteralFloat(2.0f32), 3);
    assert_token!("2.0L", Token::LiteralDouble(2.0f64));
    assert_token!("0.5h", Token::LiteralHalf(0.5f32));

    assert_token!("\"\"", Token::LiteralString("".to_string()));

    assert_token!("true", Token::True);
    assert_token!("true ", Token::True, 4);
    assert_token!("truea", Token::Id(Identifier("truea".to_string())));

    assert_token!("false", Token::False);
    assert_token!("false ", Token::False, 5);
    assert_token!("falsea", Token::Id(Identifier("falsea".to_string())));

    assert_token!("{", Token::LeftBrace);
    assert_token!("}", Token::RightBrace);
    assert_token!("(", Token::LeftParen);
    assert_token!(")", Token::RightParen);
    assert_token!("[", Token::LeftSquareBracket);
    assert_token!("]", Token::RightSquareBracket);

    assert_token!("< ", Token::LeftAngleBracket(FollowedBy::Whitespace), 1);
    assert_token!("<", Token::LeftAngleBracket(FollowedBy::Whitespace));
    assert_token!("<< ", Token::LeftAngleBracket(FollowedBy::Token), 1);
    assert_token!("<<", Token::LeftAngleBracket(FollowedBy::Token), 1);
    assert_token!("> ", Token::RightAngleBracket(FollowedBy::Whitespace), 1);
    assert_token!(">", Token::RightAngleBracket(FollowedBy::Whitespace));
    assert_token!(">> ", Token::RightAngleBracket(FollowedBy::Token), 1);
    assert_token!(">>", Token::RightAngleBracket(FollowedBy::Token), 1);
    assert_token!("<>", Token::LeftAngleBracket(FollowedBy::Token), 1);
    assert_token!("><", Token::RightAngleBracket(FollowedBy::Token), 1);

    assert_token!(";", Token::Semicolon);
    assert_token!(",", Token::Comma);

    assert_token!("+", Token::Plus);
    assert_token!("+ ", Token::Plus, 1);
    assert_token!("+=", Token::PlusEquals);
    assert_token!("+= ", Token::PlusEquals, 2);
    assert_token!("++", Token::PlusPlus);
    assert_token!("++ ", Token::PlusPlus, 2);

    assert_token!("-", Token::Minus);
    assert_token!("- ", Token::Minus, 1);
    assert_token!("-=", Token::MinusEquals);
    assert_token!("-= ", Token::MinusEquals, 2);
    assert_token!("--", Token::MinusMinus);
    assert_token!("-- ", Token::MinusMinus, 2);

    assert_token!("/", Token::ForwardSlash);
    assert_token!("/ ", Token::ForwardSlash, 1);
    assert_token!("/=", Token::ForwardSlashEquals);
    assert_token!("/= ", Token::ForwardSlashEquals, 2);
    assert_token!("//", Token::Comment, 2);
    assert_token!("// ", Token::Comment, 3);

    assert_token!("%", Token::Percent);
    assert_token!("% ", Token::Percent, 1);
    assert_token!("%=", Token::PercentEquals);
    assert_token!("%= ", Token::PercentEquals, 2);
    assert_token!("%%", Token::Percent, 1);
    assert_token!("%% ", Token::Percent, 1);

    assert_token!("*", Token::Asterix);
    assert_token!("* ", Token::Asterix, 1);
    assert_token!("*=", Token::AsterixEquals);
    assert_token!("*= ", Token::AsterixEquals, 2);
    assert_token!("**", Token::Asterix, 1);
    assert_token!("** ", Token::Asterix, 1);

    assert_token!("|", Token::VerticalBar);
    assert_token!("| ", Token::VerticalBar, 1);
    assert_token!("|=", Token::VerticalBarEquals);
    assert_token!("|= ", Token::VerticalBarEquals, 2);
    assert_token!("||", Token::VerticalBarVerticalBar);
    assert_token!("|| ", Token::VerticalBarVerticalBar, 2);

    assert_token!("&", Token::Ampersand);
    assert_token!("& ", Token::Ampersand, 1);
    assert_token!("&=", Token::AmpersandEquals);
    assert_token!("&= ", Token::AmpersandEquals, 2);
    assert_token!("&&", Token::AmpersandAmpersand);
    assert_token!("&& ", Token::AmpersandAmpersand, 2);

    assert_token!("^", Token::Hat);
    assert_token!("^ ", Token::Hat, 1);
    assert_token!("^=", Token::HatEquals);
    assert_token!("^= ", Token::HatEquals, 2);
    assert_token!("^^", Token::Hat, 1);
    assert_token!("^^ ", Token::Hat, 1);

    assert_token!("=", Token::Equals);
    assert_token!("= ", Token::Equals, 1);
    assert_token!("==", Token::EqualsEquals);
    assert_token!("== ", Token::EqualsEquals, 2);
    assert_token!("===", Token::EqualsEquals, 2);
    assert_token!("=== ", Token::EqualsEquals, 2);

    assert_token!("#", Token::Hash);
    assert_token!("# ", Token::Hash, 1);
    assert_token!("##", Token::HashHash);
    assert_token!("## ", Token::HashHash, 2);

    assert_token!("@", Token::At);

    assert_token!("!", Token::ExclamationPoint);
    assert_token!("! ", Token::ExclamationPoint, 1);
    assert_token!("!=", Token::ExclamationPointEquals);
    assert_token!("!= ", Token::ExclamationPointEquals, 2);
    assert_token!("!==", Token::ExclamationPointEquals, 2);
    assert_token!("!== ", Token::ExclamationPointEquals, 2);

    assert_token!("~", Token::Tilde);
    assert_token!("~ ", Token::Tilde, 1);

    assert_token!(".", Token::Period);
    assert_token!(". ", Token::Period, 1);

    assert_token!("if", Token::If);
    assert_token!("if ", Token::If, 2);
    assert_token!("ifa", Token::Id(Identifier("ifa".to_string())));

    assert_token!("else", Token::Else);
    assert_token!("else ", Token::Else, 4);
    assert_token!("elsea", Token::Id(Identifier("elsea".to_string())));

    assert_token!("for", Token::For);
    assert_token!("for ", Token::For, 3);
    assert_token!("fora", Token::Id(Identifier("fora".to_string())));

    assert_token!("while", Token::While);
    assert_token!("while ", Token::While, 5);
    assert_token!("whilea", Token::Id(Identifier("whilea".to_string())));

    assert_token!("switch", Token::Switch);
    assert_token!("switch ", Token::Switch, 6);
    assert_token!("switcha", Token::Id(Identifier("switcha".to_string())));

    assert_token!("return", Token::Return);
    assert_token!("return ", Token::Return, 6);
    assert_token!("returna", Token::Id(Identifier("returna".to_string())));

    assert_token!("break", Token::Break);
    assert_token!("break ", Token::Break, 5);
    assert_token!("breaka", Token::Id(Identifier("breaka".to_string())));

    assert_token!("continue", Token::Continue);
    assert_token!("continue ", Token::Continue, 8);
    assert_token!("continuea", Token::Id(Identifier("continuea".to_string())));

    assert_token!("discard", Token::Discard);
    assert_token!("discard ", Token::Discard, 7);
    assert_token!("discarda", Token::Id(Identifier("discarda".to_string())));

    assert_token!("struct", Token::Struct);
    assert_token!("struct ", Token::Struct, 6);
    assert_token!("structa", Token::Id(Identifier("structa".to_string())));
    assert_token!(
        "structName",
        Token::Id(Identifier("structName".to_string()))
    );

    assert_token!("enum", Token::Enum);
    assert_token!("enum ", Token::Enum, 4);
    assert_token!("enuma", Token::Id(Identifier("enuma".to_string())));

    assert_token!("cbuffer", Token::ConstantBuffer);
    assert_token!("cbuffer ", Token::ConstantBuffer, 7);
    assert_token!("cbuffera", Token::Id(Identifier("cbuffera".to_string())));

    assert_token!("namespace", Token::Namespace);
    assert_token!("namespace ", Token::Namespace, 9);
    assert_token!(
        "namespacea",
        Token::Id(Identifier("namespacea".to_string()))
    );

    assert_token!("register", Token::Register);
    assert_token!("register ", Token::Register, 8);
    assert_token!("registera", Token::Id(Identifier("registera".to_string())));

    assert_token!("packoffset", Token::PackOffset);
    assert_token!("packoffset ", Token::PackOffset, 10);
    assert_token!(
        "packoffseta",
        Token::Id(Identifier("packoffseta".to_string()))
    );

    assert_token!(":", Token::Colon);
    assert_token!(": ", Token::Colon, 1);
    assert_token!(":=", Token::Colon, 1);
    assert_token!(":= ", Token::Colon, 1);
    assert_token!("::", Token::ScopeResolution);
    assert_token!(":: ", Token::ScopeResolution, 2);
    assert_token!("::=", Token::ScopeResolution, 2);
    assert_token!("::= ", Token::ScopeResolution, 2);

    assert_token!("?", Token::QuestionMark);
    assert_token!("? ", Token::QuestionMark, 1);
    assert_token!("?=", Token::QuestionMark, 1);
    assert_token!("??", Token::QuestionMark, 1);

    assert_token!("in", Token::In);
    assert_token!("in ", Token::In, 2);
    assert_token!("inou", Token::Id(Identifier("inou".to_string())));

    assert_token!("out", Token::Out);
    assert_token!("out ", Token::Out, 3);
    assert_token!("outa", Token::Id(Identifier("outa".to_string())));

    assert_token!("inout", Token::InOut);
    assert_token!("inout ", Token::InOut, 5);
    assert_token!("inouta", Token::Id(Identifier("inouta".to_string())));

    assert_token!("const", Token::Const);
    assert_token!("const ", Token::Const, 5);
    assert_token!("consta", Token::Id(Identifier("consta".to_string())));

    assert_token!("volatile", Token::Volatile);
    assert_token!("volatile ", Token::Volatile, 8);
    assert_token!("volatilea", Token::Id(Identifier("volatilea".to_string())));

    assert_token!("row_major", Token::RowMajor);
    assert_token!("row_major ", Token::RowMajor, 9);
    assert_token!("column_major", Token::ColumnMajor);
    assert_token!("column_major ", Token::ColumnMajor, 12);
    assert_token!(
        "row_column_major",
        Token::Id(Identifier("row_column_major".to_string()))
    );
    assert_token!(
        "column_row_major",
        Token::Id(Identifier("column_row_major".to_string()))
    );

    assert_token!("unorm", Token::Unorm);
    assert_token!("unorm ", Token::Unorm, 5);
    assert_token!("snorm", Token::Snorm);
    assert_token!("snorm ", Token::Snorm, 5);
    assert_token!(
        "unormsnorm",
        Token::Id(Identifier("unormsnorm".to_string()))
    );
    assert_token!(
        "snormunorm",
        Token::Id(Identifier("snormunorm".to_string()))
    );

    assert_token!("extern", Token::Extern);
    assert_token!("extern ", Token::Extern, 6);
    assert_token!("external", Token::Id(Identifier("external".to_string())));

    assert_token!("static", Token::Static);
    assert_token!("static ", Token::Static, 6);
    assert_token!(
        "staticconst",
        Token::Id(Identifier("staticconst".to_string()))
    );

    assert_token!("groupshared", Token::GroupShared);
    assert_token!("groupshared ", Token::GroupShared, 11);
    assert_token!(
        "groupsharedshared",
        Token::Id(Identifier("groupsharedshared".to_string()))
    );

    assert_token!("sizeof", Token::SizeOf);
    assert_token!("sizeof ", Token::SizeOf, 6);
    assert_token!("sizeofa", Token::Id(Identifier("sizeofa".to_string())));

    assert_token!("template", Token::Template);
    assert_token!("template ", Token::Template, 8);
    assert_token!("templatea", Token::Id(Identifier("templatea".to_string())));

    assert_token!("typename", Token::Typename);
    assert_token!("typename ", Token::Typename, 8);
    assert_token!("typenamea", Token::Id(Identifier("typenamea".to_string())));
}

#[test]
fn test_token_stream() {
    fn token_id(name: &'static str, loc: u32) -> StreamToken {
        StreamToken(Token::Id(Identifier(name.to_string())), StreamLocation(loc))
    }
    fn loc(tok: Token, loc: u32) -> StreamToken {
        StreamToken(tok, StreamLocation(loc))
    }

    assert_eq!(token_stream(&b""[..]), Ok((&b""[..], vec![])));
    assert_eq!(
        token_stream(&b"// Comment only source!\n"[..]),
        Ok((
            &b""[..],
            Vec::from([loc(Token::Comment, 0), loc(Token::Endline, 23)])
        ))
    );
    assert_eq!(
        token_stream(&b"// Comment only source!\nelse"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::Comment, 0),
                loc(Token::Endline, 23),
                loc(Token::Else, 24)
            ])
        ))
    );
    assert_eq!(
        token_stream(&b"// Comment only source!\r\nelse"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::Comment, 0),
                loc(Token::Endline, 23),
                loc(Token::Else, 25)
            ])
        ))
    );
    assert_eq!(
        token_stream(&b"a\nb"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::Id(Identifier("a".to_string())), 0),
                loc(Token::Endline, 1),
                loc(Token::Id(Identifier("b".to_string())), 2),
            ])
        ))
    );
    assert_eq!(
        token_stream(&b"a\\\nb"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::Id(Identifier("a".to_string())), 0),
                loc(Token::PhysicalEndline, 1),
                loc(Token::Id(Identifier("b".to_string())), 3),
            ])
        ))
    );
    assert_eq!(
        token_stream(&b"// Comment only source!\\\nelse"[..]),
        Ok((&b""[..], Vec::from([loc(Token::Comment, 0)])))
    );
    assert_eq!(
        token_stream(&b"// Comment only source!\\\r\nelse"[..]),
        Ok((&b""[..], Vec::from([loc(Token::Comment, 0)])))
    );

    assert_eq!(
        token_stream(&b" a "[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::Whitespace, 0),
                token_id("a", 1),
                loc(Token::Whitespace, 2),
            ])
        ))
    );

    assert_eq!(
        token_stream(&b"void func();"[..]),
        Ok((
            &b""[..],
            vec![
                token_id("void", 0),
                loc(Token::Whitespace, 4),
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
            vec![
                loc(Token::Minus, 0),
                loc(Token::LiteralInt(12), 1),
                loc(Token::Whitespace, 3),
            ]
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
            vec![
                loc(Token::LeftAngleBracket(FollowedBy::Whitespace), 0),
                loc(Token::Whitespace, 1),
            ]
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
            vec![
                loc(Token::RightAngleBracket(FollowedBy::Whitespace), 0),
                loc(Token::Whitespace, 1),
            ]
        ))
    );

    assert_eq!(
        token_stream(&b"+++++"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::PlusPlus, 0),
                loc(Token::PlusPlus, 2),
                loc(Token::Plus, 4),
            ])
        ))
    );

    assert_eq!(
        token_stream(&b"-----"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::MinusMinus, 0),
                loc(Token::MinusMinus, 2),
                loc(Token::Minus, 4),
            ])
        ))
    );

    assert_eq!(
        token_stream(&b"+++-+"[..]),
        Ok((
            &b""[..],
            Vec::from([
                loc(Token::PlusPlus, 0),
                loc(Token::Plus, 2),
                loc(Token::Minus, 3),
                loc(Token::Plus, 4),
            ])
        ))
    );
}
