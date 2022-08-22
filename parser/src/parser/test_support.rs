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

/// Turn a string into lex tokens for a test
#[track_caller]
fn lex_from_str(source: &str) -> (Vec<LexToken>, SourceManager) {
    // Create source manager to store the source into
    let mut source_manager = SourceManager::new();

    // Add a newline to the end of every test string as the lexer requires a clean ending
    let modified_string = source.to_string() + "\n";

    // Preprocess the text
    let preprocessed_text = rssl_preprocess::preprocess_fragment(
        &modified_string,
        FileName("parser_test.hlsl".to_string()),
        &mut source_manager,
    )
    .expect("preprocess failed");

    // Run the lexer on the input
    match rssl_lexer::lex(&preprocessed_text) {
        Ok(tokens) => (tokens.stream, source_manager),
        Err(err) => panic!("{}{:?}", err.display(&source_manager), err),
    }
}

/// Helper type to invoke parsing on fragments of text
pub struct ParserTester<F, T>(F, std::marker::PhantomData<T>);

impl<
        T: std::cmp::PartialEq + std::fmt::Debug,
        F: for<'t> Fn(&'t [LexToken], &SymbolTable) -> ParseResult<'t, T>,
    > ParserTester<F, T>
{
    /// Create a new tester object from a parse function
    pub fn new(parse_fn: F) -> Self {
        ParserTester(parse_fn, std::marker::PhantomData)
    }

    /// Check that a source string parses into the given value
    #[track_caller]
    pub fn check(&self, input: &str, value: T) {
        self.check_symbolic(input, &SymbolTable::empty(), value);
    }

    /// Check that a source string parses into the given value - in the scope of a given symbol table
    #[track_caller]
    pub fn check_symbolic(&self, input: &str, symbols: &SymbolTable, value: T) {
        let (tokens, source_manager) = lex_from_str(input);
        match (self.0)(&tokens, symbols) {
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

    /// Check that parsing will fail for the given string
    #[track_caller]
    pub fn expect_fail(&self, input: &str, error_reason: ParseErrorReason, offset: u32) {
        let (tokens, _) = lex_from_str(input);
        match (self.0)(&tokens, &SymbolTable::empty()) {
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
            Err(nom::Err::Error(err)) | Err(nom::Err::Failure(err)) => {
                assert_eq!(
                    (err.1, err.0[0].1),
                    (error_reason, SourceLocation::first().offset(offset))
                );
            }
            Err(nom::Err::Incomplete(_)) => panic!("Unexpected end of stream"),
        }
    }
}
