use super::*;

/// Parse an initializer statement for a variable
pub fn parse_initializer<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Option<Initializer>> {
    fn init_expr<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Initializer> {
        let (input, expr) = parse_expression_no_seq(input, st)?;
        Ok((input, Initializer::Expression(expr)))
    }

    fn init_aggregate<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Initializer> {
        let (input, _) = parse_token(Token::LeftBrace)(input)?;
        let (input, exprs) =
            nom::multi::separated_list1(parse_token(Token::Comma), |input| init_any(input, st))(
                input,
            )?;
        let (input, _) = parse_token(Token::RightBrace)(input)?;
        Ok((input, Initializer::Aggregate(exprs)))
    }

    fn init_any<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Initializer> {
        if let Ok((input, expr)) = init_expr(input, st) {
            return Ok((input, expr));
        }

        if let Ok((input, expr)) = init_aggregate(input, st) {
            return Ok((input, expr));
        }

        Err(nom::Err::Error(ParseErrorContext(
            input,
            ParseErrorReason::ErrorKind(nom::error::ErrorKind::Alt),
        )))
    }

    if input.is_empty() {
        Err(nom::Err::Incomplete(nom::Needed::new(1)))
    } else {
        match input[0].0 {
            Token::Equals => match init_any(&input[1..], st) {
                Ok((input, init)) => Ok((input, Some(init))),
                Err(err) => Err(err),
            },
            _ => Ok((input, None)),
        }
    }
}

#[test]
fn test_initializer() {
    fn initializer<'t>(input: &'t [LexToken]) -> ParseResult<'t, Option<Initializer>> {
        let st = SymbolTable::empty();
        parse_initializer(input, &st)
    }

    assert_eq!(
        initializer(&[]),
        Err(nom::Err::Incomplete(nom::Needed::new(1)))
    );

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
    let hst_lit = Located::none(Expression::Literal(Literal::UntypedInt(4)));
    assert_eq!(
        initializer(&expr_lit),
        Ok((done_toks, Some(Initializer::Expression(hst_lit))))
    );

    // Aggregate initialization tests
    // = { [expr], [expr], [expr] }
    fn loc_lit(i: u64) -> Initializer {
        Initializer::Expression(Located::none(Expression::Literal(Literal::UntypedInt(i))))
    }

    let aggr_1 = [
        LexToken::with_no_loc(Token::Equals),
        LexToken::with_no_loc(Token::LeftBrace),
        LexToken::with_no_loc(Token::LiteralInt(4)),
        LexToken::with_no_loc(Token::RightBrace),
        semicolon.clone(),
    ];
    let aggr_1_lit = loc_lit(4);
    assert_eq!(
        initializer(&aggr_1),
        Ok((done_toks, Some(Initializer::Aggregate(vec![aggr_1_lit]))))
    );

    let aggr_3 = [
        LexToken::with_no_loc(Token::Equals),
        LexToken::with_no_loc(Token::LeftBrace),
        LexToken::with_no_loc(Token::LiteralInt(4)),
        LexToken::with_no_loc(Token::Comma),
        LexToken::with_no_loc(Token::LiteralInt(2)),
        LexToken::with_no_loc(Token::Comma),
        LexToken::with_no_loc(Token::LiteralInt(1)),
        LexToken::with_no_loc(Token::RightBrace),
        semicolon.clone(),
    ];
    let aggr_3_lits = vec![loc_lit(4), loc_lit(2), loc_lit(1)];
    assert_eq!(
        initializer(&aggr_3),
        Ok((done_toks, Some(Initializer::Aggregate(aggr_3_lits))))
    );

    // = {} should fail
    let aggr_0 = [
        LexToken::with_no_loc(Token::Equals),
        LexToken::with_no_loc(Token::LeftBrace),
        LexToken::with_no_loc(Token::RightBrace),
        semicolon,
    ];
    assert!(match initializer(&aggr_0) {
        Err(_) => true,
        _ => false,
    });
}

/// Parse the type for a local variable
fn parse_local_type<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, LocalType> {
    // Todo: input modifiers
    match parse_type(input, st) {
        Ok((rest, ty)) => Ok((rest, LocalType(ty, LocalStorage::default(), None))),
        Err(err) => Err(err),
    }
}

/// Parse a local variable definition
fn parse_vardef<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, VarDef> {
    let (input, typename) = parse_local_type(input, st)?;
    let (input, defs) = nom::multi::separated_list1(parse_token(Token::Comma), |input| {
        let (input, varname) = parse_variable_name(input)?;
        let (input, array_dim) = nom::combinator::opt(|input| parse_arraydim(input, st))(input)?;
        let (input, init) = parse_initializer(input, st)?;
        let v = LocalVariableName {
            name: varname.to_node(),
            bind: match array_dim {
                Some(ref expr) => VariableBind::Array(expr.clone()),
                None => VariableBind::Normal,
            },
            init,
        };
        Ok((input, v))
    })(input)?;
    let defs = VarDef {
        local_type: typename,
        defs,
    };
    Ok((input, defs))
}

/// Parse the init statement for a for loop
fn parse_init_statement<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, InitStatement> {
    let res = nom::branch::alt((
        nom::combinator::map(contextual(parse_vardef, st), |vd| {
            InitStatement::Declaration(vd)
        }),
        nom::combinator::map(contextual(parse_expression, st), |e| {
            InitStatement::Expression(e)
        }),
    ))(input);
    match res {
        Ok((rest, res)) => Ok((rest, res)),
        Err(nom::Err::Incomplete(needed)) => Err(nom::Err::Incomplete(needed)),
        Err(_) => Ok((input, InitStatement::Empty)),
    }
}

/// Parse an attribute that is attached to a statement
fn statement_attribute<'t>(input: &'t [LexToken], _: &SymbolTable) -> ParseResult<'t, ()> {
    let (input, _) = parse_token(Token::LeftSquareBracket)(input)?;
    let (input, _) = match_identifier(input)?;

    // Currently do not support arguments to attributes

    let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
    Ok((input, ()))
}

#[test]
fn test_statement_attribute() {
    let st = SymbolTable::empty();
    let fastopt = &[
        LexToken::with_no_loc(Token::LeftSquareBracket),
        LexToken::with_no_loc(Token::Id(Identifier("fastopt".to_string()))),
        LexToken::with_no_loc(Token::RightSquareBracket),
    ];
    assert_eq!(statement_attribute(fastopt, &st), Ok((&[][..], ())));
}

/// Parse a single statement
fn parse_statement<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Statement> {
    // Parse and ignore attributes before a statement
    let input = match nom::multi::many0(|input| statement_attribute(input, st))(input) {
        Ok((rest, _)) => rest,
        Err(err) => return Err(err),
    };
    if input.is_empty() {
        return Err(nom::Err::Incomplete(nom::Needed::new(1)));
    }
    let (head, tail) = (input[0].clone(), &input[1..]);
    match head {
        LexToken(Token::Semicolon, _) => Ok((tail, Statement::Empty)),
        LexToken(Token::If, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, cond) = parse_expression(input, st)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner_statement) = parse_statement(input, st)?;
            let inner_statement = Box::new(inner_statement);
            if input.is_empty() {
                return Err(nom::Err::Incomplete(nom::Needed::new(1)));
            }
            let (head, tail) = (input[0].clone(), &input[1..]);
            match head {
                LexToken(Token::Else, _) => match parse_statement(tail, st) {
                    Err(err) => Err(err),
                    Ok((tail, else_part)) => {
                        let s = Statement::IfElse(cond, inner_statement, Box::new(else_part));
                        Ok((tail, s))
                    }
                },
                _ => Ok((input, Statement::If(cond, inner_statement))),
            }
        }
        LexToken(Token::For, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, init) = parse_init_statement(input, st)?;
            let (input, _) = parse_token(Token::Semicolon)(input)?;
            let (input, cond) = parse_expression(input, st)?;
            let (input, _) = parse_token(Token::Semicolon)(input)?;
            let (input, inc) = parse_expression(input, st)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner) = parse_statement(input, st)?;
            Ok((input, Statement::For(init, cond, inc, Box::new(inner))))
        }
        LexToken(Token::While, _) => {
            let (input, _) = parse_token(Token::LeftParen)(tail)?;
            let (input, cond) = parse_expression(input, st)?;
            let (input, _) = parse_token(Token::RightParen)(input)?;
            let (input, inner) = parse_statement(input, st)?;
            Ok((input, Statement::While(cond, Box::new(inner))))
        }
        LexToken(Token::Break, _) => Ok((tail, Statement::Break)),
        LexToken(Token::Continue, _) => Ok((tail, Statement::Continue)),
        LexToken(Token::Return, _) => match parse_expression(tail, st) {
            Ok((input, expression_statement)) => {
                let (input, _) = parse_token(Token::Semicolon)(input)?;
                Ok((input, Statement::Return(Some(expression_statement))))
            }
            Err(_) => {
                let (input, _) = parse_token(Token::Semicolon)(tail)?;
                Ok((input, Statement::Return(None)))
            }
        },
        LexToken(Token::LeftBrace, _) => {
            let (input, s) = statement_block(input, st)?;
            Ok((input, Statement::Block(s)))
        }
        _ => {
            // Try parsing a variable definition
            fn variable_def<'t>(
                input: &'t [LexToken],
                st: &SymbolTable,
            ) -> ParseResult<'t, Statement> {
                let (input, var) = parse_vardef(input, st)?;
                let (input, _) = parse_token(Token::Semicolon)(input)?;
                Ok((input, Statement::Var(var)))
            }
            let err = match variable_def(input, st) {
                Ok((rest, statement)) => return Ok((rest, statement)),
                Err(nom::Err::Incomplete(needed)) => return Err(nom::Err::Incomplete(needed)),
                Err(e) => e,
            };
            // Try parsing an expression statement
            fn expr_statement<'t>(
                input: &'t [LexToken],
                st: &SymbolTable,
            ) -> ParseResult<'t, Statement> {
                let (input, expression_statement) = parse_expression(input, st)?;
                let (input, _) = parse_token(Token::Semicolon)(input)?;
                Ok((input, Statement::Expression(expression_statement)))
            }
            let err = match expr_statement(input, st) {
                Ok((rest, statement)) => return Ok((rest, statement)),
                Err(nom::Err::Incomplete(needed)) => return Err(nom::Err::Incomplete(needed)),
                Err(e) => get_most_relevant_error(err, e),
            };
            // Return the most likely error
            Err(err)
        }
    }
}

/// Parse a block of statements
pub fn statement_block<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Vec<Statement>> {
    let mut statements = Vec::new();
    let mut rest = match parse_token(Token::LeftBrace)(input) {
        Ok((rest, _)) => rest,
        Err(err) => return Err(err),
    };
    loop {
        let last_def = parse_statement(rest, st);
        if let Ok((remaining, root)) = last_def {
            statements.push(root);
            rest = remaining;
        } else {
            return match parse_token(Token::RightBrace)(rest) {
                Ok((rest, _)) => Ok((rest, statements)),
                Err(nom::Err::Incomplete(needed)) => Err(nom::Err::Incomplete(needed)),
                Err(_) => match last_def {
                    Ok(_) => unreachable!(),
                    Err(err) => Err(err),
                },
            };
        }
    }
}

#[test]
fn test_statement() {
    use test_support::*;
    let statement = ParserTester::new(parse_statement);

    // Empty statement
    statement.check(";", Statement::Empty);

    // Expression statements
    statement.check(
        "func();",
        Statement::Expression(Expression::Call("func".as_bvar(0), vec![], vec![]).loc(0)),
    );
    statement.check(
        " func ( ) ; ",
        Statement::Expression(Expression::Call("func".as_bvar(1), vec![], vec![]).loc(1)),
    );

    // For loop init statement
    let init_statement = ParserTester::new(parse_init_statement);
    let vardef = ParserTester::new(parse_vardef);

    init_statement.check("x", InitStatement::Expression("x".as_var(0)));
    vardef.check("uint x", VarDef::one("x", Type::uint().into()));
    init_statement.check(
        "uint x",
        InitStatement::Declaration(VarDef::one("x", Type::uint().into())),
    );
    init_statement.check(
        "uint x = y",
        InitStatement::Declaration(VarDef::one_with_expr(
            "x",
            Type::uint().into(),
            "y".as_var(9),
        )),
    );

    // Variable declarations
    statement.check(
        "uint x = y;",
        Statement::Var(VarDef::one_with_expr(
            "x",
            Type::uint().into(),
            "y".as_var(9),
        )),
    );
    statement.check(
        "float x[3];",
        Statement::Var(VarDef {
            local_type: Type::from_layout(TypeLayout::float()).into(),
            defs: vec![LocalVariableName {
                name: "x".to_string(),
                bind: VariableBind::Array(Some(Expression::Literal(Literal::UntypedInt(3)).loc(8))),
                init: None,
            }],
        }),
    );

    // Blocks
    statement.check(
        "{one();two();}",
        Statement::Block(vec![
            Statement::Expression(Expression::Call("one".as_bvar(1), vec![], vec![]).loc(1)),
            Statement::Expression(Expression::Call("two".as_bvar(7), vec![], vec![]).loc(7)),
        ]),
    );
    statement.check(
        " { one(); two(); } ",
        Statement::Block(vec![
            Statement::Expression(Expression::Call("one".as_bvar(3), vec![], vec![]).loc(3)),
            Statement::Expression(Expression::Call("two".as_bvar(10), vec![], vec![]).loc(10)),
        ]),
    );

    // If statement
    statement.check(
        "if(a)func();",
        Statement::If(
            "a".as_var(3),
            Box::new(Statement::Expression(
                Expression::Call("func".as_bvar(5), vec![], vec![]).loc(5),
            )),
        ),
    );
    statement.check(
        "if (a) func(); ",
        Statement::If(
            "a".as_var(4),
            Box::new(Statement::Expression(
                Expression::Call("func".as_bvar(7), vec![], vec![]).loc(7),
            )),
        ),
    );
    statement.check(
        "if (a)\n{\n\tone();\n\ttwo();\n}",
        Statement::If(
            "a".as_var(4),
            Box::new(Statement::Block(vec![
                Statement::Expression(Expression::Call("one".as_bvar(10), vec![], vec![]).loc(10)),
                Statement::Expression(Expression::Call("two".as_bvar(18), vec![], vec![]).loc(18)),
            ])),
        ),
    );

    // If-else statement
    statement.check(
        "if (a) one(); else two();",
        Statement::IfElse(
            "a".as_var(4),
            Box::new(Statement::Expression(
                Expression::Call("one".as_bvar(7), vec![], vec![]).loc(7),
            )),
            Box::new(Statement::Expression(
                Expression::Call("two".as_bvar(19), vec![], vec![]).loc(19),
            )),
        ),
    );

    // While loops
    statement.check(
        "while (a)\n{\n\tone();\n\ttwo();\n}",
        Statement::While(
            "a".as_var(7),
            Box::new(Statement::Block(vec![
                Statement::Expression(Expression::Call("one".as_bvar(13), vec![], vec![]).loc(13)),
                Statement::Expression(Expression::Call("two".as_bvar(21), vec![], vec![]).loc(21)),
            ])),
        ),
    );

    // For loops
    statement.check(
        "for(a;b;c)func();",
        Statement::For(
            InitStatement::Expression("a".as_var(4)),
            "b".as_var(6),
            "c".as_var(8),
            Box::new(Statement::Expression(
                Expression::Call("func".as_bvar(10), vec![], vec![]).loc(10),
            )),
        ),
    );
    statement.check(
        "for (uint i = 0; i; i++) { func(); }",
        Statement::For(
            InitStatement::Declaration(VarDef::one_with_expr(
                "i",
                Type::uint().into(),
                Expression::Literal(Literal::UntypedInt(0)).loc(14),
            )),
            "i".as_var(17),
            Expression::UnaryOperation(UnaryOp::PostfixIncrement, "i".as_bvar(20)).loc(20),
            Box::new(Statement::Block(vec![Statement::Expression(
                Expression::Call("func".as_bvar(27), vec![], vec![]).loc(27),
            )])),
        ),
    );
}
