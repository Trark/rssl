use super::*;

pub trait TestTokenExt {
    fn loc(self, offset: u32) -> LexToken;
    fn noloc(self) -> LexToken;
}

impl TestTokenExt for Token {
    fn loc(self, offset: u32) -> LexToken {
        LexToken(self, SourceLocation::first().offset(offset))
    }

    fn noloc(self) -> LexToken {
        LexToken(self, SourceLocation::UNKNOWN)
    }
}

pub trait TestLocationExt
where
    Self: Sized,
{
    fn loc(self, offset: u32) -> Located<Self>;
    fn bloc(self, offset: u32) -> Box<Located<Self>>;
}

impl TestLocationExt for Expression {
    fn loc(self, offset: u32) -> Located<Self> {
        Located::new(self, SourceLocation::first().offset(offset))
    }

    fn bloc(self, offset: u32) -> Box<Located<Self>> {
        Box::new(Located::new(self, SourceLocation::first().offset(offset)))
    }
}

pub trait TestVariableExt {
    fn as_var(self, offset: u32) -> Located<Expression>;
    fn as_bvar(self, offset: u32) -> Box<Located<Expression>>;
}

impl TestVariableExt for &str {
    fn as_var(self, offset: u32) -> Located<Expression> {
        Expression::Variable(self.to_string()).loc(offset)
    }

    fn as_bvar(self, offset: u32) -> Box<Located<Expression>> {
        Expression::Variable(self.to_string()).bloc(offset)
    }
}

pub fn parse_result_from_str<T>(
) -> impl Fn(&'static str) -> Result<<T as Parse>::Output, ParseErrorReason>
where
    T: Parse + 'static,
{
    use rssl_lexer::lex;
    use rssl_preprocess::preprocess_fragment;
    move |string: &'static str| {
        let mut source_manager = SourceManager::new();
        let modified_string = string.to_string() + "\n";
        let preprocessed_text = preprocess_fragment(
            &modified_string,
            FileName("parser_test.hlsl".to_string()),
            &mut source_manager,
        )
        .expect("preprocess failed");
        let lex_result = lex(&preprocessed_text);
        match lex_result {
            Ok(tokens) => {
                let stream = &tokens.stream;
                match T::parse(stream, &SymbolTable::empty()) {
                    Ok((rem, exp)) => {
                        if rem.len() == 1 && rem[0].0 == Token::Eof {
                            Ok(exp)
                        } else {
                            Err(ParseErrorReason::FailedToParse)
                        }
                    }
                    Err(nom::Err::Incomplete(_)) => Err(ParseErrorReason::UnexpectedEndOfStream),
                    _ => Err(ParseErrorReason::FailedToParse),
                }
            }
            Err(error) => panic!("Failed to lex `{:?}`", error),
        }
    }
}

pub fn parse_from_str<T>() -> impl Fn(&'static str) -> <T as Parse>::Output
where
    T: Parse + 'static,
{
    use rssl_lexer::lex;
    use rssl_preprocess::preprocess_fragment;
    move |string: &'static str| {
        let mut source_manager = SourceManager::new();
        let modified_string = string.to_string() + "\n";
        let preprocessed_text = preprocess_fragment(
            &modified_string,
            FileName("parser_test.hlsl".to_string()),
            &mut source_manager,
        )
        .expect("preprocess failed");
        let lex_result = lex(&preprocessed_text);
        match lex_result {
            Ok(tokens) => {
                let stream = &tokens.stream;
                match T::parse(stream, &SymbolTable::empty()) {
                    Ok((rem, exp)) => {
                        if rem.len() == 1 && rem[0].0 == Token::Eof {
                            exp
                        } else {
                            panic!("Tokens remaining while parsing `{:?}`: {:?}", stream, rem)
                        }
                    }
                    Err(nom::Err::Incomplete(needed)) => {
                        panic!("Failed to parse `{:?}`: Needed {:?} more", stream, needed)
                    }
                    Err(nom::Err::Error(ParseErrorContext(rest, err))) => {
                        panic!("Failed to parse with `{:?}`: {:?}", err, rest)
                    }
                    Err(nom::Err::Failure(ParseErrorContext(rest, err))) => {
                        panic!("Failed to parse with `{:?}`: {:?}", err, rest)
                    }
                }
            }
            Err(error) => panic!("Failed to lex `{:?}`", error),
        }
    }
}

pub fn parse_from_str_with_symbols<T>(
) -> impl Fn(&'static str, &SymbolTable) -> <T as Parse>::Output
where
    T: Parse + 'static,
{
    use rssl_lexer::lex;
    use rssl_preprocess::preprocess_fragment;
    move |string: &'static str, symbols: &SymbolTable| {
        let mut source_manager = SourceManager::new();
        let modified_string = string.to_string() + "\n";
        let preprocessed_text = preprocess_fragment(
            &modified_string,
            FileName("parser_test.hlsl".to_string()),
            &mut source_manager,
        )
        .expect("preprocess failed");
        let lex_result = lex(&preprocessed_text);
        match lex_result {
            Ok(tokens) => {
                let stream = &tokens.stream;
                match T::parse(stream, symbols) {
                    Ok((rem, exp)) => {
                        if rem.len() == 1 && rem[0].0 == Token::Eof {
                            exp
                        } else {
                            panic!("Tokens remaining while parsing `{:?}`: {:?}", stream, rem)
                        }
                    }
                    Err(nom::Err::Incomplete(needed)) => {
                        panic!("Failed to parse `{:?}`: Needed {:?} more", stream, needed)
                    }
                    Err(nom::Err::Error(ParseErrorContext(rest, err))) => {
                        panic!("Failed to parse with `{:?}`: {:?}", err, rest)
                    }
                    Err(nom::Err::Failure(ParseErrorContext(rest, err))) => {
                        panic!("Failed to parse with `{:?}`: {:?}", err, rest)
                    }
                }
            }
            Err(error) => panic!("Failed to lex `{:?}`", error),
        }
    }
}
