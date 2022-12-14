use rssl_text::*;
use rssl_tok::*;

/// Provides details on why a lex operation failed
#[derive(PartialEq, Clone)]
pub struct LexerError {
    reason: LexerErrorReason,
    location: SourceLocation,
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

    /// Get formatter to print the error
    pub fn display<'a>(&'a self, source_manager: &'a SourceManager) -> LexerErrorPrinter<'a> {
        LexerErrorPrinter(self, source_manager)
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
            LexerErrorReason::Unknown => write!(f, "Unknown lexer error"),
            LexerErrorReason::FailedToParse(_) => write!(f, "Unexpected character"),
            LexerErrorReason::UnexpectedEndOfStream => write!(f, "Unexpected end of stream"),
        }
    }
}

/// Prints lexer errors
pub struct LexerErrorPrinter<'a>(&'a LexerError, &'a SourceManager);

impl<'a> std::fmt::Display for LexerErrorPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let LexerErrorPrinter(err, source_manager) = self;

        // Get file location info
        let file_location = source_manager.get_file_location(err.location);

        // Print basic failure reason
        writeln!(f, "{}: {}", file_location, err.reason)?;

        // Print source that caused the error
        source_manager.write_source_for_error(f, Some(err.location))
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
        "cbuffer" => Token::ConstantBuffer,
        "namespace" => Token::Namespace,

        "true" => Token::True,
        "false" => Token::False,

        "in" => Token::In,
        "out" => Token::Out,
        "inout" => Token::InOut,

        "const" => Token::Const,
        "extern" => Token::Extern,
        "static" => Token::Static,
        "groupshared" => Token::GroupShared,
        "sizeof" => Token::SizeOf,
        "template" => Token::Template,
        "typename" => Token::Typename,

        // Unimplemented keywords
        "packoffset" | "case" | "default" => Token::ReservedWord(id.0),

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

/// Parse a single specific word
fn specific_word<'a>(input: &'a [u8], name: &'static str) -> LexResult<'a, &'a [u8]> {
    let name_bytes = name.as_bytes();
    if input.starts_with(name_bytes) {
        let (k, r) = input.split_at(name_bytes.len());
        if r.is_empty() || identifier_char(r).is_err() {
            Ok((r, k))
        } else {
            wrong_chars(input)
        }
    } else {
        wrong_chars(input)
    }
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
fn whitespace_simple(input: &[u8]) -> LexResult<()> {
    if input.is_empty() {
        end_of_stream()
    } else {
        match input[0] {
            b' ' | b'\n' | b'\r' | b'\t' => Ok((&input[1..], ())),
            _ => wrong_chars(input),
        }
    }
}

/// Parse a line comment
fn line_comment(input: &[u8]) -> LexResult<()> {
    if input.starts_with(b"//") {
        match input.iter().enumerate().position(|c| *c.1 == b'\n') {
            Some(len) => Ok((&input[len..], ())),
            None => Ok((&[], ())),
        }
    } else {
        wrong_chars(input)
    }
}

/// Parse a block comment
fn block_comment(input: &[u8]) -> LexResult<()> {
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
        end_of_stream()
    } else {
        // Not a block comment
        wrong_chars(input)
    }
}

/// Parse any kind of whitespace
fn whitespace(input: &[u8]) -> LexResult<()> {
    let mut search = input;
    loop {
        search = match choose(&[&whitespace_simple, &line_comment, &block_comment], search) {
            Ok((input, ())) => input,
            Err(_) => break,
        }
    }

    if input == search {
        // No whitespace found
        wrong_chars(input)
    } else {
        // Whitespace found
        Ok((search, ()))
    }
}

/// Parse any kind of white space or no whitespace
fn skip_whitespace(input: &[u8]) -> LexResult<()> {
    let (input, _) = opt(whitespace)(input)?;
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

/// Register class for a resource
enum RegisterType {
    T,
    U,
    B,
    S,
}

/// Parse a register type
fn register_type(input: &[u8]) -> LexResult<RegisterType> {
    match input {
        [b't', rest @ ..] => Ok((rest, RegisterType::T)),
        [b'u', rest @ ..] => Ok((rest, RegisterType::U)),
        [b'b', rest @ ..] => Ok((rest, RegisterType::B)),
        [b's', rest @ ..] => Ok((rest, RegisterType::S)),
        _ => wrong_chars(input),
    }
}

/// Parse a register slot attribute
fn register(input: &[u8]) -> LexResult<Token> {
    let (input, _) = specific_word(input, "register")?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = specific_text(input, "(")?;
    let (input, _) = skip_whitespace(input)?;
    let (input, slot_type) = register_type(input)?;
    let (input, num) = digits(input)?;
    let (input, _) = skip_whitespace(input)?;
    let (input, _) = specific_text(input, ")")?;

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
fn lookahead_token(input: &[u8]) -> LexResult<Option<Token>> {
    match token_no_whitespace_intermediate(input) {
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
                Some(_) => Token::LeftAngleBracket(FollowedBy::Token),
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
                Some(_) => Token::RightAngleBracket(FollowedBy::Token),
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
        [c, b'=', b'=', ..] if *c == op_char && op_equals_token != Token::Eof => {
            invalid_chars(input)
        }
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
    assert_eq!(p(b"==="), invalid_chars(b"==="));
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
    assert_eq!(p(b"!=="), invalid_chars(b"!=="));
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
            &symbol_single(b'#', Token::Hash),
            &symbol_single(b'@', Token::At),
            &symbol_single(b'~', Token::Tilde),
            &symbol_single(b'.', Token::Period),
            &symbol_colon,
            &symbol_single(b'?', Token::QuestionMark),
        ],
        input,
    )
}

/// Parse any single non-whitespace token - without a location
fn token_no_whitespace_intermediate(input: &[u8]) -> LexResult<Token> {
    choose(
        &[
            // Literals
            &literal_float,
            &literal_int,
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
            // Special words
            &register,
            // Identifiers and keywords
            &any_word,
        ],
        input,
    )
}

/// Parse any single non-whitespace token - with a location
fn token_no_whitespace(input: &[u8]) -> LexResult<IntermediateToken> {
    let (remaining, token) = token_no_whitespace_intermediate(input)?;
    let intermediate_token = IntermediateToken(token, IntermediateLocation(input.len() as u32));
    Ok((remaining, intermediate_token))
}

/// Parse a single token
fn token(input: &[u8]) -> LexResult<IntermediateToken> {
    let (input, _) = skip_whitespace(input)?;
    let (input, token) = token_no_whitespace(input)?;
    let (input, _) = skip_whitespace(input)?;

    Ok((input, token))
}

/// Parse all tokens in a stream
fn token_stream(mut input: &[u8]) -> LexResult<Vec<StreamToken>> {
    let total_length = input.len() as u32;
    let mut tokens = Vec::new();

    // If the input only contains whitespace then early out
    // We only consume whitespace around tokens so with no tokens the whitespace will be left unparsed
    let (no_whitespace_input, _) = skip_whitespace(input)?;
    if no_whitespace_input.is_empty() {
        return Ok((&[], tokens));
    }

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
pub fn lex(preprocessed: &PreprocessedText) -> Result<Tokens, LexerError> {
    let code_bytes = preprocessed.as_bytes();
    let total_length = code_bytes.len() as u32;
    match token_stream(code_bytes) {
        Ok((rest, mut stream)) => {
            if rest.is_empty() {
                let stream = {
                    stream.push(StreamToken(Token::Eof, StreamLocation(total_length)));
                    stream
                };
                let mut lex_tokens = Vec::with_capacity(stream.len());
                let mut last_entry = 0;
                for StreamToken(ref token, stream_location) in stream {
                    let location_result =
                        preprocessed.get_source_location_sequential(stream_location, last_entry);
                    let source_location = location_result.0;
                    last_entry = location_result.1;
                    lex_tokens.push(LexToken(token.clone(), source_location));
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

                    if whitespace(after).is_ok() {
                        break;
                    }
                }

                let failing_bytes = rest[..rest.len() - after.len()].to_vec();
                let offset = StreamLocation((code_bytes.len() - rest.len()) as u32);
                Err(LexerError::new(
                    LexerErrorReason::FailedToParse(failing_bytes),
                    preprocessed.get_source_location(offset),
                ))
            }
        }
        Err(LexErrorContext(input, kind)) => {
            if kind == LexErrorKind::Eof {
                Err(LexerError::new(
                    LexerErrorReason::UnexpectedEndOfStream,
                    preprocessed.get_source_location(StreamLocation(code_bytes.len() as u32)),
                ))
            } else {
                let offset = StreamLocation((code_bytes.len() - input.len()) as u32);
                Err(LexerError::new(
                    LexerErrorReason::Unknown,
                    preprocessed.get_source_location(offset),
                ))
            }
        }
    }
}

/// Run the lexer on input text to turn it into tokens without location information or advanced error information
pub fn minilex(text: &str) -> Result<Vec<Token>, LexerError> {
    let code_bytes = text.as_bytes();
    match token_stream(code_bytes) {
        Ok((rest, stream)) => {
            if rest.is_empty() {
                let mut tokens = Vec::with_capacity(stream.len());
                for StreamToken(ref token, _) in stream {
                    tokens.push(token.clone());
                }
                Ok(tokens)
            } else {
                Err(LexerError::new(
                    LexerErrorReason::Unknown,
                    SourceLocation::UNKNOWN,
                ))
            }
        }
        Err(LexErrorContext(_, _)) => Err(LexerError::new(
            LexerErrorReason::Unknown,
            SourceLocation::UNKNOWN,
        )),
    }
}

#[test]
fn test_token() {
    fn from_end(tok: Token, from: u32) -> IntermediateToken {
        IntermediateToken(tok, IntermediateLocation(from))
    }

    assert!(token(&b""[..]).is_err());
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
        Ok((&b""[..], from_end(Token::VerticalBar, 2)))
    );
    assert_eq!(
        token(&b"|| "[..]),
        Ok((&b""[..], from_end(Token::VerticalBarVerticalBar, 3)))
    );
    assert_eq!(
        token(&b"& "[..]),
        Ok((&b""[..], from_end(Token::Ampersand, 2)))
    );
    assert_eq!(
        token(&b"&& "[..]),
        Ok((&b""[..], from_end(Token::AmpersandAmpersand, 3)))
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
        token(&b"discard"[..]),
        Ok((&b""[..], from_end(Token::Discard, 7)))
    );

    assert_eq!(
        token(&b"struct"[..]),
        Ok((&b""[..], from_end(Token::Struct, 6)))
    );
    assert_eq!(
        token(&b"enum"[..]),
        Ok((&b""[..], from_end(Token::Enum, 4)))
    );
    assert_eq!(
        token(&b"cbuffer"[..]),
        Ok((&b""[..], from_end(Token::ConstantBuffer, 7)))
    );
    assert_eq!(
        token(&b"namespace"[..]),
        Ok((&b""[..], from_end(Token::Namespace, 9)))
    );
    assert_eq!(
        token(&b"register(t4)"[..]),
        Ok((&b""[..], from_end(Token::Register(RegisterSlot::T(4)), 12)))
    );
    assert_eq!(token(&b":"[..]), Ok((&b""[..], from_end(Token::Colon, 1))));
    assert_eq!(
        token(&b"::"[..]),
        Ok((&b""[..], from_end(Token::ScopeResolution, 2)))
    );
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
    assert_eq!(
        token_stream(&b"// Comment only source!\n"[..]),
        Ok((&b""[..], vec![]))
    );

    fn token_id(name: &'static str, loc: u32) -> StreamToken {
        StreamToken(Token::Id(Identifier(name.to_string())), StreamLocation(loc))
    }
    fn loc(tok: Token, loc: u32) -> StreamToken {
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
