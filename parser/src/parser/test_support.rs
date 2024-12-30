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
impl TestLocationExt for TypeModifier {}
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
        F: for<'t> Fn(&'t [LexToken], &dyn SymbolResolver) -> ParseResult<'t, T>,
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
        match (self.0)(&tokens, &TestResolver) {
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
        match (self.0)(input, &TestResolver) {
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
        match (self.0)(&tokens, &TestResolver) {
            Ok((rem, exp)) => {
                if rem.len() == 1 && rem[0].0 == Token::Eof {
                    panic!("{exp:?}");
                } else {
                    assert_eq!(
                        (ParseErrorReason::TokensUnconsumed, rem[0].1),
                        (error_reason, SourceLocation::first().offset(offset))
                    );
                }
            }
            Err(ParseErrorContext(_, _, ParseErrorReason::UnexpectedEndOfStream)) => {
                panic!("Unexpected end of stream")
            }
            Err(err) => {
                assert_eq!(
                    (err.2, err.0[0].1),
                    (error_reason, SourceLocation::first().offset(offset))
                );
            }
        }
    }
}

/// Symbol resolver for use with tests
pub struct TestResolver;

impl SymbolResolver for TestResolver {
    fn is_type(&self, ty: &TypeLayout) -> bool {
        match ty.0.identifiers.as_slice() {
            [name] => {
                return matches!(
                    name.as_str(),
                    // Standard types
                    "void"
                        | "uint"
                        | "uint4"
                        | "int"
                        | "float"
                        | "float2"
                        | "float3"
                        | "float4"
                        | "float4x4"
                        | "half"
                        | "vector"
                        | "Buffer"
                        | "StructuredBuffer"
                        // Test common type argument names
                        | "T"
                        // Test common struct names
                        | "S"
                        | "MyStruct"
                        | "CustomType"
                        | "Parent"
                );
            }
            [ns, name] => {
                return matches!(
                    (ns.as_str(), name.as_str()),
                    // Test common struct names
                    ("My", "Type")
                );
            }
            _ => {}
        }

        false
    }

    fn is_function(&self, _: &TypeLayout) -> bool {
        false
    }
}

/// Parse all root level items
fn parse_for_test(source: &[LexToken]) -> Result<Module, ParseError> {
    let resolver = TestResolver;
    let mut parser = Parser::new(source.to_vec());
    let mut root_definitions = Vec::new();
    let mut namespace_depth = 0;
    loop {
        match parser.parse_item(&resolver) {
            Ok(ParserItem::Definition(item)) => {
                let mut defs = &mut root_definitions;
                for _ in 0..namespace_depth {
                    match defs.last_mut().unwrap() {
                        RootDefinition::Namespace(_, next_defs) => {
                            defs = next_defs;
                        }
                        _ => panic!(),
                    }
                }
                defs.push(item);
            }
            Ok(ParserItem::Template) => loop {
                if let Ok(None) = parser.parse_template_parameter(&resolver) {
                    break;
                }
            },
            Ok(ParserItem::Struct(name)) => {
                let mut def = parser.parse_struct(&resolver)?;
                // TODO:
                def.name = name;
                let mut defs = &mut root_definitions;
                for _ in 0..namespace_depth {
                    match defs.last_mut().unwrap() {
                        RootDefinition::Namespace(_, next_defs) => {
                            defs = next_defs;
                        }
                        _ => panic!(),
                    }
                }
                defs.push(RootDefinition::Struct(def));
            }
            Ok(ParserItem::NamespaceEnter(name)) => {
                let mut defs = &mut root_definitions;
                for _ in 0..namespace_depth {
                    match defs.last_mut().unwrap() {
                        RootDefinition::Namespace(_, next_defs) => {
                            defs = next_defs;
                        }
                        _ => panic!(),
                    }
                }
                defs.push(RootDefinition::Namespace(name, Vec::new()));
                namespace_depth += 1;
            }
            Ok(ParserItem::NamespaceExit) => {
                namespace_depth -= 1;
            }
            Ok(ParserItem::Empty) => {}
            Ok(ParserItem::EndOfFile) => return Ok(Module { root_definitions }),
            Err(err) => return Err(err),
        }
    }
}

/// Check that a source string parses into the given set of root definitions
#[track_caller]
pub fn check_roots(input: &str, value: &[RootDefinition]) {
    let (tokens, source_manager) = lex_from_str(input);
    match parse_for_test(&tokens) {
        Ok(module) => {
            assert_eq!(module.root_definitions, value);
        }
        Err(err) => panic!("{}", err.display(&source_manager)),
    }
}

/// Check that a source string parses into the given root definition
#[track_caller]
pub fn check_root(input: &str, value: RootDefinition) {
    check_roots(input, &[value]);
}
