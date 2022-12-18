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
    fn loc(self, offset: u32) -> Located<Self> {
        Located::new(self, SourceLocation::first().offset(offset))
    }

    fn bloc(self, offset: u32) -> Box<Located<Self>> {
        Box::new(Located::new(self, SourceLocation::first().offset(offset)))
    }
}

impl TestLocationExt for Expression {}
impl TestLocationExt for Type {}
impl TestLocationExt for String {}
impl TestLocationExt for &str {}

#[allow(clippy::wrong_self_convention)]
pub trait TestVariableExt {
    fn as_var(self, offset: u32) -> Located<Expression>;
    fn as_bvar(self, offset: u32) -> Box<Located<Expression>>;
    fn as_bvar2(self, inner_offset: u32, outer_offset: u32) -> Box<Located<Expression>>;
}

impl TestVariableExt for &str {
    fn as_var(self, offset: u32) -> Located<Expression> {
        Expression::Identifier(ScopedIdentifier {
            base: ScopedIdentifierBase::Relative,
            identifiers: Vec::from([self.to_string().loc(offset)]),
        })
        .loc(offset)
    }

    fn as_bvar(self, offset: u32) -> Box<Located<Expression>> {
        self.as_bvar2(offset, offset)
    }

    fn as_bvar2(self, inner_offset: u32, outer_offset: u32) -> Box<Located<Expression>> {
        Expression::Identifier(ScopedIdentifier {
            base: ScopedIdentifierBase::Relative,
            identifiers: Vec::from([self.to_string().loc(inner_offset)]),
        })
        .bloc(outer_offset)
    }
}

/// Turn a string into lex tokens for a test
#[track_caller]
fn lex_from_str(source: &str) -> (Vec<LexToken>, SourceManager) {
    // Create source manager to store the source into
    let mut source_manager = SourceManager::new();

    // Preprocess the text
    let tokens = match rssl_preprocess::preprocess_fragment(
        source,
        FileName("parser_test.rssl".to_string()),
        &mut source_manager,
    ) {
        Ok(tokens) => tokens,
        Err(err) => panic!("{}", err.display(&source_manager)),
    };

    let tokens = rssl_preprocess::prepare_tokens(&tokens);

    (tokens, source_manager)
}

/// Helper type to invoke parsing on fragments of text
pub struct ParserTester<F, T>(F, std::marker::PhantomData<T>);

impl<
        T: std::cmp::PartialEq + std::fmt::Debug,
        F: for<'t> Fn(&'t [LexToken]) -> ParseResult<'t, T>,
    > ParserTester<F, T>
{
    /// Create a new tester object from a parse function
    pub fn new(parse_fn: F) -> Self {
        ParserTester(parse_fn, std::marker::PhantomData)
    }

    /// Check that a source string parses into the given value
    #[track_caller]
    pub fn check(&self, input: &str, value: T) {
        let (tokens, source_manager) = lex_from_str(input);
        match (self.0)(&tokens) {
            Ok((rem, exp)) => {
                if rem.len() == 1 && rem[0].0 == Token::Eof {
                    assert_eq!(exp, value);
                } else {
                    panic!(
                        "{}",
                        ParseError::from_tokens_remaining(rem).display(&source_manager)
                    );
                }
            }
            Err(err) => panic!("{}", ParseError::from(err).display(&source_manager)),
        }
    }

    /// Check that a list of tokens parses into the given value
    #[track_caller]
    pub fn check_from_tokens(&self, input: &[LexToken], used_tokens: usize, value: T) {
        match (self.0)(input) {
            Ok((rem, exp)) if rem == &input[used_tokens..] => {
                assert_eq!(exp, value);
            }
            Ok((rem, _)) => panic!("{:?}", ParseError::from_tokens_remaining(rem)),

            Err(err) => panic!("{:?}", ParseError::from(err)),
        }
    }

    /// Check that parsing will fail for the given string
    #[track_caller]
    pub fn expect_fail(&self, input: &str, error_reason: ParseErrorReason, offset: u32) {
        let (tokens, _) = lex_from_str(input);
        match (self.0)(&tokens) {
            Ok((rem, exp)) => {
                if rem.len() == 1 && rem[0].0 == Token::Eof {
                    panic!("{:?}", exp);
                } else {
                    assert_eq!(
                        (ParseErrorReason::TokensUnconsumed, rem[0].1),
                        (error_reason, SourceLocation::first().offset(offset))
                    );
                }
            }
            Err(ParseErrorContext(_, ParseErrorReason::UnexpectedEndOfStream)) => {
                panic!("Unexpected end of stream")
            }
            Err(err) => {
                assert_eq!(
                    (err.1, err.0[0].1),
                    (error_reason, SourceLocation::first().offset(offset))
                );
            }
        }
    }
}
