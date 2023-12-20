use super::*;

/// Parse an initializer statement for a variable
pub fn parse_initializer(input: &[LexToken]) -> ParseResult<Option<Initializer>> {
    fn init_expr(input: &[LexToken]) -> ParseResult<Initializer> {
        let (input, expr) = parse_expression_no_seq(input)?;
        Ok((input, Initializer::Expression(expr)))
    }

    fn init_aggregate(input: &[LexToken]) -> ParseResult<Initializer> {
        let (input, _) = parse_token(Token::LeftBrace)(input)?;
        let (input, exprs) = parse_list(parse_token(Token::Comma), init_any)(input)?;
        let (input, _) = parse_optional(parse_token(Token::Comma))(input)?;
        let (input, _) = parse_token(Token::RightBrace)(input)?;
        Ok((input, Initializer::Aggregate(exprs)))
    }

    fn init_any(input: &[LexToken]) -> ParseResult<Initializer> {
        init_expr(input).select(init_aggregate(input))
    }

    if input.is_empty() {
        ParseErrorReason::end_of_stream()
    } else {
        match input[0].0 {
            Token::Equals => match init_any(&input[1..]) {
                Ok((input, init)) => Ok((input, Some(init))),
                Err(err) => Err(err),
            },
            _ => Ok((input, None)),
        }
    }
}

#[test]
fn test_initializer() {
    fn initializer(input: &[LexToken]) -> ParseResult<Option<Initializer>> {
        parse_initializer(input)
    }

    assert_eq!(initializer(&[]), ParseErrorReason::end_of_stream());

    // Semicolon to trigger parsing to end
    let semicolon = LexToken::with_no_loc(Token::Semicolon);
    let done_toks = &[semicolon.clone()][..];

    // No initializer tests
    assert_eq!(initializer(&[semicolon.clone()]), Ok((done_toks, None)));

    // Expression initialization tests
    // = [expr]
    let expr_lit = [
        LexToken::with_no_loc(Token::Equals),
        LexToken::with_no_loc(Token::LiteralInt(4)),
        semicolon.clone(),
    ];
    let hst_lit = Located::none(Expression::Literal(Literal::IntUntyped(4)));
    assert_eq!(
        initializer(&expr_lit),
        Ok((done_toks, Some(Initializer::Expression(hst_lit))))
    );

    // Aggregate initialization tests
    // = { [expr], [expr], [expr] }
    fn loc_lit(i: u64) -> Initializer {
        Initializer::Expression(Located::none(Expression::Literal(Literal::IntUntyped(i))))
    }

    // = { 4 };
    assert_eq!(
        initializer(&[
            LexToken::with_no_loc(Token::Equals),
            LexToken::with_no_loc(Token::LeftBrace),
            LexToken::with_no_loc(Token::LiteralInt(4)),
            LexToken::with_no_loc(Token::RightBrace),
            semicolon.clone(),
        ]),
        Ok((
            done_toks,
            Some(Initializer::Aggregate(Vec::from([loc_lit(4)])))
        ))
    );

    // = { 4, 2, 1 };
    assert_eq!(
        initializer(&[
            LexToken::with_no_loc(Token::Equals),
            LexToken::with_no_loc(Token::LeftBrace),
            LexToken::with_no_loc(Token::LiteralInt(4)),
            LexToken::with_no_loc(Token::Comma),
            LexToken::with_no_loc(Token::LiteralInt(2)),
            LexToken::with_no_loc(Token::Comma),
            LexToken::with_no_loc(Token::LiteralInt(1)),
            LexToken::with_no_loc(Token::RightBrace),
            semicolon.clone(),
        ]),
        Ok((
            done_toks,
            Some(Initializer::Aggregate(Vec::from([
                loc_lit(4),
                loc_lit(2),
                loc_lit(1)
            ])))
        ))
    );

    // = { 4, 2, 1, };
    assert_eq!(
        initializer(&[
            LexToken::with_no_loc(Token::Equals),
            LexToken::with_no_loc(Token::LeftBrace),
            LexToken::with_no_loc(Token::LiteralInt(4)),
            LexToken::with_no_loc(Token::Comma),
            LexToken::with_no_loc(Token::LiteralInt(2)),
            LexToken::with_no_loc(Token::Comma),
            LexToken::with_no_loc(Token::LiteralInt(1)),
            LexToken::with_no_loc(Token::Comma),
            LexToken::with_no_loc(Token::RightBrace),
            semicolon.clone(),
        ]),
        Ok((
            done_toks,
            Some(Initializer::Aggregate(Vec::from([
                loc_lit(4),
                loc_lit(2),
                loc_lit(1)
            ])))
        ))
    );

    // = {}
    let aggr_0 = [
        LexToken::with_no_loc(Token::Equals),
        LexToken::with_no_loc(Token::LeftBrace),
        LexToken::with_no_loc(Token::RightBrace),
        semicolon,
    ];
    assert_eq!(
        initializer(&aggr_0),
        Ok((done_toks, Some(Initializer::Aggregate(Vec::new()))))
    );
}

/// Parse a local variable definition
fn parse_vardef(input: &[LexToken]) -> ParseResult<VarDef> {
    let (input, typename) = parse_type(input)?;
    let (input, defs) = parse_init_declarators(input)?;
    let defs = VarDef {
        local_type: typename,
        defs,
    };
    Ok((input, defs))
}

#[test]
fn test_vardef() {
    use test_support::*;
    let vardef = ParserTester::new(parse_vardef);

    vardef.check(
        "uint x",
        VarDef::one("x".to_string().loc(5), Type::from("uint".loc(0))),
    );
}

/// Parse the init statement for a for loop
fn parse_init_statement(input: &[LexToken]) -> ParseResult<InitStatement> {
    let res_expr = match parse_expression(input) {
        Ok((input, vd)) => Ok((input, InitStatement::Expression(vd))),
        Err(err) => Err(err),
    };

    let res_vardef = match parse_vardef(input) {
        Ok((input, vd)) => Ok((input, InitStatement::Declaration(vd))),
        Err(err) => Err(err),
    };

    let res_empty = Ok((input, InitStatement::Empty));

    res_expr.select(res_vardef).select(res_empty)
}

#[test]
fn test_init_statement() {
    use test_support::*;
    let init_statement = ParserTester::new(parse_init_statement);

    init_statement.check("x", InitStatement::Expression("x".as_var(0)));
    init_statement.check(
        "uint x",
        InitStatement::Declaration(VarDef::one(
            "x".to_string().loc(5),
            Type::from("uint".loc(0)),
        )),
    );
    init_statement.check(
        "uint x = y",
        InitStatement::Declaration(VarDef::one_with_expr(
            "x".to_string().loc(5),
            Type::from("uint".loc(0)),
            "y".as_var(9),
        )),
    );
}

/// Parse an attribute with either [ ] or [[ ]]
pub fn parse_attribute(input: &[LexToken]) -> ParseResult<Attribute> {
    parse_attribute_base(input, true)
}

/// Parse an attribute with [[ ]]
pub fn parse_attribute_double_only(input: &[LexToken]) -> ParseResult<Attribute> {
    parse_attribute_base(input, false)
}

/// Parse an attribute
fn parse_attribute_base(input: &[LexToken], allow_single: bool) -> ParseResult<Attribute> {
    let original = input;

    let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;
    let (input, opt_second_bracket) = if allow_single {
        let (input, opt) = parse_optional(parse_token(Token::LeftSquareBracket))(input)?;
        (input, opt.is_some())
    } else {
        match parse_token(Token::LeftSquareBracket)(input) {
            Ok((input, _)) => (input, true),
            Err(_) => return ParseErrorReason::wrong_token(original),
        }
    };

    let (input, name) =
        parse_list_nonempty(parse_token(Token::ScopeResolution), parse_variable_name)(input)?;

    let (input, has_args) = match parse_token(Token::LeftParen)(input) {
        Ok((input, _)) => (input, true),
        Err(_) => (input, false),
    };

    let (input, arguments) = if has_args {
        let mut input = input;
        let mut args = Vec::new();
        if parse_token(Token::RightParen)(input).is_err() {
            loop {
                let (rest, arg) = parse_expression_no_seq(input)?;

                args.push(arg);
                input = rest;

                match parse_token(Token::Comma)(input) {
                    Ok((rest, _)) => {
                        input = rest;
                    }
                    Err(_) => break,
                }
            }
        }

        let (input, _) = parse_token(Token::RightParen)(input)?;
        (input, args)
    } else {
        (input, Vec::new())
    };

    let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
    let input = if opt_second_bracket {
        parse_token(Token::RightSquareBracket)(input)?.0
    } else {
        input
    };

    let attr = Attribute {
        name,
        arguments,
        two_square_brackets: opt_second_bracket,
    };

    Ok((input, attr))
}

#[test]
fn test_attribute() {
    assert_eq!(
        parse_attribute(&[
            LexToken::with_no_loc(Token::LeftSquareBracket),
            LexToken::with_no_loc(Token::Id(Identifier("fastopt".to_string()))),
            LexToken::with_no_loc(Token::RightSquareBracket),
        ]),
        Ok((
            &[][..],
            Attribute {
                name: Vec::from([Located::none("fastopt".to_string())]),
                arguments: Vec::new(),
                two_square_brackets: false,
            }
        ))
    );

    assert_eq!(
        parse_attribute(&[
            LexToken::with_no_loc(Token::LeftSquareBracket),
            LexToken::with_no_loc(Token::Id(Identifier("unroll".to_string()))),
            LexToken::with_no_loc(Token::RightSquareBracket),
        ]),
        Ok((
            &[][..],
            Attribute {
                name: Vec::from([Located::none("unroll".to_string())]),
                arguments: Vec::new(),
                two_square_brackets: false,
            }
        ))
    );

    assert_eq!(
        parse_attribute(&[
            LexToken::with_no_loc(Token::LeftSquareBracket),
            LexToken::with_no_loc(Token::Id(Identifier("unroll".to_string()))),
            LexToken::with_no_loc(Token::LeftParen),
            LexToken::with_no_loc(Token::LiteralInt(4)),
            LexToken::with_no_loc(Token::RightParen),
            LexToken::with_no_loc(Token::RightSquareBracket),
        ]),
        Ok((
            &[][..],
            Attribute {
                name: Vec::from([Located::none("unroll".to_string())]),
                arguments: Vec::from([Located::none(Expression::Literal(Literal::IntUntyped(4)))]),
                two_square_brackets: false,
            }
        ))
    );

    assert_eq!(
        parse_attribute(&[
            LexToken::with_no_loc(Token::LeftSquareBracket),
            LexToken::with_no_loc(Token::Id(Identifier("outputtopology".to_string()))),
            LexToken::with_no_loc(Token::LeftParen),
            LexToken::with_no_loc(Token::LiteralString("triangle".to_string())),
            LexToken::with_no_loc(Token::RightParen),
            LexToken::with_no_loc(Token::RightSquareBracket),
        ]),
        Ok((
            &[][..],
            Attribute {
                name: Vec::from([Located::none("outputtopology".to_string())]),
                arguments: Vec::from([Located::none(Expression::Literal(Literal::String(
                    "triangle".to_string()
                )))]),
                two_square_brackets: false,
            }
        ))
    );
}

/// Parse a single statement
fn parse_statement(input: &[LexToken]) -> ParseResult<Statement> {
    // Parse attributes before a statement
    let (input, attributes) = parse_multiple(parse_attribute)(input)?;

    // Locate the statement at the start of the main statement part
    // This may be empty now but can not be after parse_statement_kind
    let location_stream = input;

    let (input, kind) = parse_statement_kind(input)?;

    let statement = Statement {
        kind,
        location: location_stream[0].1,
        attributes,
    };

    Ok((input, statement))
}

/// Parse the main part of a statement
fn parse_statement_kind(input: &[LexToken]) -> ParseResult<StatementKind> {
    if input.is_empty() {
        return ParseErrorReason::end_of_stream();
    }
    let (head, tail) = (input[0].clone(), &input[1..]);
    match head {
        LexToken(Token::Semicolon, _) => Ok((tail, StatementKind::Empty)),
        LexToken(Token::If, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, cond) = parse_expression(input)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner_statement) = parse_statement(input)?;
            let inner_statement = Box::new(inner_statement);
            if input.is_empty() {
                return ParseErrorReason::end_of_stream();
            }
            let (head, tail) = (input[0].clone(), &input[1..]);
            match head {
                LexToken(Token::Else, _) => match parse_statement(tail) {
                    Err(err) => Err(err),
                    Ok((tail, else_part)) => {
                        let s = StatementKind::IfElse(cond, inner_statement, Box::new(else_part));
                        Ok((tail, s))
                    }
                },
                _ => Ok((input, StatementKind::If(cond, inner_statement))),
            }
        }
        LexToken(Token::For, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, init) = parse_init_statement(input)?;
            let (input, _) = parse_token(Token::Semicolon)(input)?;
            let (input, cond) = parse_optional(parse_expression)(input)?;
            let (input, _) = parse_token(Token::Semicolon)(input)?;
            let (input, inc) = parse_optional(parse_expression)(input)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner) = parse_statement(input)?;
            Ok((input, StatementKind::For(init, cond, inc, Box::new(inner))))
        }
        LexToken(Token::While, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, cond) = parse_expression(input)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner) = parse_statement(input)?;
            Ok((input, StatementKind::While(cond, Box::new(inner))))
        }
        LexToken(Token::Do, _) => {
            let (input, inner) = parse_statement(tail)?;
            let (input, _) = parse_token(Token::While)(input)?;
            let (input, _) = parse_token(Token::LeftParen)(input)?;
            let (input, cond) = parse_expression(input)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, _) = parse_token(Token::Semicolon)(input)?;
            Ok((input, StatementKind::DoWhile(Box::new(inner), cond)))
        }
        LexToken(Token::Switch, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, cond) = parse_expression(input)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner) = parse_statement(input)?;
            Ok((input, StatementKind::Switch(cond, Box::new(inner))))
        }
        LexToken(Token::Break, _) => {
            let (input, _) = parse_token(Token::Semicolon)(tail)?;
            Ok((input, StatementKind::Break))
        }
        LexToken(Token::Continue, _) => {
            let (input, _) = parse_token(Token::Semicolon)(tail)?;
            Ok((input, StatementKind::Continue))
        }
        LexToken(Token::Discard, _) => {
            let (input, _) = parse_token(Token::Semicolon)(tail)?;
            Ok((input, StatementKind::Discard))
        }
        LexToken(Token::Return, _) => match parse_expression(tail) {
            Ok((input, expression_statement)) => {
                let (input, _) = parse_token(Token::Semicolon)(input)?;
                Ok((input, StatementKind::Return(Some(expression_statement))))
            }
            Err(_) => {
                let (input, _) = parse_token(Token::Semicolon)(tail)?;
                Ok((input, StatementKind::Return(None)))
            }
        },
        LexToken(Token::Case, _) => {
            let (input, cond) = parse_expression(tail)?;
            let (input, _) = parse_token(Token::Colon)(input)?;
            let (input, next) = parse_statement(input)?;
            Ok((input, StatementKind::CaseLabel(cond, Box::new(next))))
        }
        LexToken(Token::Default, _) => {
            let (input, _) = parse_token(Token::Colon)(tail)?;
            let (input, next) = parse_statement(input)?;
            Ok((input, StatementKind::DefaultLabel(Box::new(next))))
        }
        LexToken(Token::LeftBrace, _) => {
            let (input, s) = statement_block(input)?;
            Ok((input, StatementKind::Block(s)))
        }
        _ => {
            // Try parsing a variable definition
            fn variable_def(input: &[LexToken]) -> ParseResult<VarDef> {
                let (input, var) = parse_vardef(input)?;
                let (input, _) = parse_token(Token::Semicolon)(input)?;
                Ok((input, var))
            }

            // Try parsing an expression statement
            fn expr_statement(input: &[LexToken]) -> ParseResult<Expression> {
                let (input, expression_statement) = parse_expression(input)?;
                let (input, _) = parse_token(Token::Semicolon)(input)?;
                Ok((input, expression_statement.node))
            }

            // Attempt to parse as a declaration first
            let declaration_result = variable_def(input);

            // Declarations defeat expressions - but we do not have type information yet
            // We need to parse both and let the type check pick the correct branch
            // The expression parsing can resolve ambiguity within an expression already
            // This is separate to that - we generate an ambiguous statement between the declaration and the (potentially ambiguous set of) expressions(s)

            // Try to parse as an expression
            // TODO: Skip expressions that are only valid if the declaration is valid to simplify the final tree
            let expression_result = expr_statement(input);

            match (declaration_result, expression_result) {
                (Ok((decl_rem, decl)), Ok((expr_rem, expr))) => {
                    assert_eq!(decl_rem.len(), expr_rem.len());
                    let statement = StatementKind::AmbiguousDeclarationOrExpression(decl, expr);
                    Ok((decl_rem, statement))
                }
                (Ok((input, decl)), Err(_)) => {
                    // Only valid as a declaration - return the declaration
                    Ok((input, StatementKind::Var(decl)))
                }
                (Err(_), Ok((input, expr))) => {
                    // Only valid as an expression - return the expression
                    Ok((input, StatementKind::Expression(expr)))
                }
                (Err(decl_err), Err(expr_err)) => {
                    // Both are errors - pick the error that is further into the stream
                    Err(decl_err).select(Err(expr_err))
                }
            }
        }
    }
}

/// Parse a block of statements
pub fn statement_block(input: &[LexToken]) -> ParseResult<Vec<Statement>> {
    let mut statements = Vec::new();
    let mut rest = match parse_token(Token::LeftBrace)(input) {
        Ok((rest, _)) => rest,
        Err(err) => return Err(err),
    };
    loop {
        let last_def = parse_statement(rest);
        match last_def {
            Ok((remaining, root)) => {
                statements.push(root);
                rest = remaining;
            }
            Err(last_def_err) => {
                return match parse_token(Token::RightBrace)(rest) {
                    Ok((rest, _)) => Ok((rest, statements)),
                    Err(_) => Err(last_def_err),
                };
            }
        }
    }
}

#[test]
fn test_empty_statement() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        ";",
        Statement {
            kind: StatementKind::Empty,
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );

    // This parser should only parse a single empty statement
    statement.expect_fail(";;", ParseErrorReason::TokensUnconsumed, 1);
}

#[test]
fn test_expression_statement() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "func();",
        Statement {
            kind: StatementKind::Expression(Expression::Call(
                "func".as_bvar(0),
                Vec::new(),
                Vec::new(),
            )),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        " func ( ) ; ",
        Statement {
            kind: StatementKind::Expression(Expression::Call(
                "func".as_bvar(1),
                Vec::new(),
                Vec::new(),
            )),
            location: SourceLocation::first().offset(1),
            attributes: Vec::new(),
        },
    );
}

#[test]
fn test_local_variables() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "uint x = y;",
        Statement {
            kind: StatementKind::Var(VarDef::one_with_expr(
                "x".to_string().loc(5),
                Type::from("uint".loc(0)),
                "y".as_var(9),
            )),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        "float x[3], y[2][4];",
        Statement {
            kind: StatementKind::Var(VarDef {
                local_type: Type::from("float".loc(0)),
                defs: Vec::from([
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new(Declarator::Identifier(
                                ScopedIdentifier::unqualified("x".to_string().loc(6)),
                                Vec::new(),
                            )),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(3)).bloc(8)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                    InitDeclarator {
                        declarator: Declarator::Array(ArrayDeclarator {
                            inner: Box::new(Declarator::Array(ArrayDeclarator {
                                inner: Box::new(Declarator::Identifier(
                                    ScopedIdentifier::unqualified("y".to_string().loc(12)),
                                    Vec::new(),
                                )),
                                array_size: Some(
                                    Expression::Literal(Literal::IntUntyped(2)).bloc(14),
                                ),
                                attributes: Vec::new(),
                            })),
                            array_size: Some(Expression::Literal(Literal::IntUntyped(4)).bloc(17)),
                            attributes: Vec::new(),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    },
                ]),
            }),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.expect_fail(
        "half g = func(4 * sizeof(const uint^7));",
        ParseErrorReason::WrongToken,
        35,
    );

    statement.check(
        "My::Type x = y;",
        Statement {
            kind: StatementKind::Var(VarDef::one_with_expr(
                "x".to_string().loc(9),
                Type::from_layout(TypeLayout(
                    ScopedIdentifier {
                        base: ScopedIdentifierBase::Relative,
                        identifiers: Vec::from([
                            "My".to_string().loc(0),
                            "Type".to_string().loc(4),
                        ]),
                    },
                    Default::default(),
                )),
                "y".as_var(13),
            )),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );

    statement.check(
        "uint * x;",
        Statement {
            kind: StatementKind::AmbiguousDeclarationOrExpression(
                VarDef {
                    local_type: Type::from("uint".loc(0)),
                    defs: Vec::from([InitDeclarator {
                        declarator: Declarator::Pointer(PointerDeclarator {
                            attributes: Vec::new(),
                            qualifiers: TypeModifierSet::default(),
                            inner: Box::new(Declarator::Identifier(
                                ScopedIdentifier::unqualified("x".to_string().loc(7)),
                                Vec::new(),
                            )),
                        }),
                        location_annotations: Vec::new(),
                        init: None,
                    }]),
                },
                Expression::BinaryOperation(BinOp::Multiply, "uint".as_bvar(0), "x".as_bvar(7)),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
}

#[test]
fn test_statement_blocks() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "{one();two();}",
        Statement {
            kind: StatementKind::Block(Vec::from([
                Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "one".as_bvar(1),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(1),
                    attributes: Vec::new(),
                },
                Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "two".as_bvar(7),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(7),
                    attributes: Vec::new(),
                },
            ])),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        " { one(); two(); } ",
        Statement {
            kind: StatementKind::Block(Vec::from([
                Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "one".as_bvar(3),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(3),
                    attributes: Vec::new(),
                },
                Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "two".as_bvar(10),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(10),
                    attributes: Vec::new(),
                },
            ])),
            location: SourceLocation::first().offset(1),
            attributes: Vec::new(),
        },
    );
}

#[test]
fn test_if() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "if(a)func();",
        Statement {
            kind: StatementKind::If(
                "a".as_var(3),
                Box::new(Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "func".as_bvar(5),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(5),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        "if (a) func(); ",
        Statement {
            kind: StatementKind::If(
                "a".as_var(4),
                Box::new(Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "func".as_bvar(7),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(7),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        "if (a)\n{\n\tone();\n\ttwo();\n}",
        Statement {
            kind: StatementKind::If(
                "a".as_var(4),
                Box::new(Statement {
                    kind: StatementKind::Block(Vec::from([
                        Statement {
                            kind: StatementKind::Expression(Expression::Call(
                                "one".as_bvar(10),
                                Vec::new(),
                                Vec::new(),
                            )),
                            location: SourceLocation::first().offset(10),
                            attributes: Vec::new(),
                        },
                        Statement {
                            kind: StatementKind::Expression(Expression::Call(
                                "two".as_bvar(18),
                                Vec::new(),
                                Vec::new(),
                            )),
                            location: SourceLocation::first().offset(18),
                            attributes: Vec::new(),
                        },
                    ])),
                    location: SourceLocation::first().offset(7),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );

    // If-else statement
    statement.check(
        "if (a) one(); else two();",
        Statement {
            kind: StatementKind::IfElse(
                "a".as_var(4),
                Box::new(Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "one".as_bvar(7),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(7),
                    attributes: Vec::new(),
                }),
                Box::new(Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "two".as_bvar(19),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(19),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
}

#[test]
fn test_while() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "while (a)\n{\n\tone();\n\ttwo();\n}",
        Statement {
            kind: StatementKind::While(
                "a".as_var(7),
                Box::new(Statement {
                    kind: StatementKind::Block(Vec::from([
                        Statement {
                            kind: StatementKind::Expression(Expression::Call(
                                "one".as_bvar(13),
                                Vec::new(),
                                Vec::new(),
                            )),
                            location: SourceLocation::first().offset(13),
                            attributes: Vec::new(),
                        },
                        Statement {
                            kind: StatementKind::Expression(Expression::Call(
                                "two".as_bvar(21),
                                Vec::new(),
                                Vec::new(),
                            )),
                            location: SourceLocation::first().offset(21),
                            attributes: Vec::new(),
                        },
                    ])),
                    location: SourceLocation::first().offset(10),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
}

#[test]
fn test_for() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "for(a;b;c)func();",
        Statement {
            kind: StatementKind::For(
                InitStatement::Expression("a".as_var(4)),
                Some("b".as_var(6)),
                Some("c".as_var(8)),
                Box::new(Statement {
                    kind: StatementKind::Expression(Expression::Call(
                        "func".as_bvar(10),
                        Vec::new(),
                        Vec::new(),
                    )),
                    location: SourceLocation::first().offset(10),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        "for (uint i = 0; i; i++) { func(); }",
        Statement {
            kind: StatementKind::For(
                InitStatement::Declaration(VarDef::one_with_expr(
                    "i".to_string().loc(10),
                    Type::from("uint".loc(5)),
                    Expression::Literal(Literal::IntUntyped(0)).loc(14),
                )),
                Some("i".as_var(17)),
                Some(
                    Expression::UnaryOperation(UnaryOp::PostfixIncrement, "i".as_bvar(20)).loc(20),
                ),
                Box::new(Statement {
                    kind: StatementKind::Block(Vec::from([Statement {
                        kind: StatementKind::Expression(Expression::Call(
                            "func".as_bvar(27),
                            Vec::new(),
                            Vec::new(),
                        )),
                        location: SourceLocation::first().offset(27),
                        attributes: Vec::new(),
                    }])),
                    location: SourceLocation::first().offset(25),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.check(
        "for (;;) ;",
        Statement {
            kind: StatementKind::For(
                InitStatement::Empty,
                None,
                None,
                Box::new(Statement {
                    kind: StatementKind::Empty,
                    location: SourceLocation::first().offset(9),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
    statement.expect_fail(
        "for(int a = 0|^; a < 10; a++) func(a);",
        ParseErrorReason::WrongToken,
        14,
    );
    statement.expect_fail(
        "for(int a = 0; a < 10 |^; a++) func(a);",
        ParseErrorReason::WrongToken,
        23,
    );
}

#[test]
fn test_do_while() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "do {} while (a);",
        Statement {
            kind: StatementKind::DoWhile(
                Box::new(Statement {
                    kind: StatementKind::Block(Vec::new()),
                    location: SourceLocation::first().offset(3),
                    attributes: Vec::new(),
                }),
                "a".as_var(13),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );

    statement.check(
        "do ; while (a);",
        Statement {
            kind: StatementKind::DoWhile(
                Box::new(Statement {
                    kind: StatementKind::Empty,
                    location: SourceLocation::first().offset(3),
                    attributes: Vec::new(),
                }),
                "a".as_var(12),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
}

#[test]
fn test_switch() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    statement.check(
        "switch (a) {}",
        Statement {
            kind: StatementKind::Switch(
                "a".as_var(8),
                Box::new(Statement {
                    kind: StatementKind::Block(Vec::new()),
                    location: SourceLocation::first().offset(11),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );

    statement.check(
        "switch (a) { case 0: return; default: break; }",
        Statement {
            kind: StatementKind::Switch(
                "a".as_var(8),
                Box::new(Statement {
                    kind: StatementKind::Block(Vec::from([
                        Statement {
                            kind: StatementKind::CaseLabel(
                                Expression::Literal(Literal::IntUntyped(0)).loc(18),
                                Box::new(Statement {
                                    kind: StatementKind::Return(None),
                                    location: SourceLocation::first().offset(21),
                                    attributes: Vec::new(),
                                }),
                            ),
                            location: SourceLocation::first().offset(13),
                            attributes: Vec::new(),
                        },
                        Statement {
                            kind: StatementKind::DefaultLabel(Box::new(Statement {
                                kind: StatementKind::Break,
                                location: SourceLocation::first().offset(38),
                                attributes: Vec::new(),
                            })),
                            location: SourceLocation::first().offset(29),
                            attributes: Vec::new(),
                        },
                    ])),
                    location: SourceLocation::first().offset(11),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );

    // Degenerate switch where inner statement is an empty statement instead of a block
    statement.check(
        "switch (a);",
        Statement {
            kind: StatementKind::Switch(
                "a".as_var(8),
                Box::new(Statement {
                    kind: StatementKind::Empty,
                    location: SourceLocation::first().offset(10),
                    attributes: Vec::new(),
                }),
            ),
            location: SourceLocation::first(),
            attributes: Vec::new(),
        },
    );
}
