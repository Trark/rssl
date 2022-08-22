use super::*;

/// Try to parse a literal
fn expr_literal(input: &[LexToken]) -> ParseResult<Located<Expression>> {
    match input.first() {
        Some(LexToken(tok, ref loc)) => {
            let literal = match *tok {
                Token::LiteralInt(v) => Literal::UntypedInt(v),
                Token::LiteralUInt(v) => Literal::UInt(v),
                Token::LiteralLong(v) => Literal::Long(v),
                Token::LiteralHalf(v) => Literal::Half(v),
                Token::LiteralFloat(v) => Literal::Float(v),
                Token::LiteralDouble(v) => Literal::Double(v),
                Token::True => Literal::Bool(true),
                Token::False => Literal::Bool(false),
                _ => {
                    return Err(nom::Err::Error(ParseErrorContext(
                        input,
                        ParseErrorReason::WrongToken,
                    )))
                }
            };
            Ok((
                &input[1..],
                Located::new(Expression::Literal(literal), *loc),
            ))
        }
        None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
    }
}

/// Try to parse an expression inside parenthesis
fn expr_in_paren<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    let (input, start) = parse_token(Token::LeftParen)(input)?;
    let (input, expr) = parse_expression(input, st)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;

    Ok((input, Located::new(expr.to_node(), start.to_loc())))
}

/// Try to parse one of the base components of an expression
fn expr_leaf<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    // Try to parse an expression nested in parenthesis
    let err = match expr_in_paren(input, st) {
        Ok(res) => return Ok(res),
        Err(err) => err,
    };

    // Try to parse a variable identifier
    let err = match parse_variable_name(input) {
        Ok((input, name)) => {
            return Ok((
                input,
                Located::new(Expression::Variable(name.node), name.location),
            ));
        }
        Err(e) => get_most_relevant_error(err, e),
    };

    // Try to parse a literal
    match expr_literal(input) {
        Ok(res) => Ok(res),
        Err(e) => Err(get_most_relevant_error(err, e)),
    }
}

/// Parse a list of template arguments
fn parse_template_args_opt<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Vec<Located<Type>>> {
    let (input, _) = match_left_angle_bracket(input)?;
    let (input, type_args) = nom::multi::separated_list0(
        parse_token(Token::Comma),
        locate(contextual(parse_type, st)),
    )(input)?;
    let (input, _) = match_right_angle_bracket(input)?;
    Ok((input, type_args))
}

/// Parse a list of template arguments or no arguments
fn parse_template_args<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Vec<Located<Type>>> {
    match parse_template_args_opt(input, st) {
        Ok(ok) => Ok(ok),
        Err(_) => Ok((input, Vec::new())),
    }
}

fn expr_p1<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    #[derive(Clone)]
    enum Precedence1Postfix {
        Increment,
        Decrement,
        Call(Vec<Located<Type>>, Vec<Located<Expression>>),
        ArraySubscript(Located<Expression>),
        Member(String),
    }

    fn expr_p1_increment(input: &[LexToken]) -> ParseResult<Located<Precedence1Postfix>> {
        let (input, start) = parse_token(Token::PlusPlus)(input)?;
        Ok((
            input,
            Located::new(Precedence1Postfix::Increment, start.to_loc()),
        ))
    }

    fn expr_p1_decrement(input: &[LexToken]) -> ParseResult<Located<Precedence1Postfix>> {
        let (input, start) = parse_token(Token::MinusMinus)(input)?;
        Ok((
            input,
            Located::new(Precedence1Postfix::Decrement, start.to_loc()),
        ))
    }

    fn expr_p1_member(input: &[LexToken]) -> ParseResult<Located<Precedence1Postfix>> {
        let (input, _) = parse_token(Token::Period)(input)?;
        let (input, member) = parse_variable_name(input)?;
        Ok((
            input,
            Located::new(
                Precedence1Postfix::Member(member.node.clone()),
                member.location,
            ),
        ))
    }

    fn expr_p1_call<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        let (input, template_args) = parse_template_args(input, st)?;

        let (input, start) = parse_token(Token::LeftParen)(input)?;

        let (input, args) = nom::multi::separated_list0(
            parse_token(Token::Comma),
            contextual(parse_expression_no_seq, st),
        )(input)?;

        let (input, _) = parse_token(Token::RightParen)(input)?;

        let call = Precedence1Postfix::Call(template_args, args);
        Ok((input, Located::new(call, start.to_loc())))
    }

    fn expr_p1_subscript<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        let (input, start) = parse_token(Token::LeftSquareBracket)(input)?;
        let (input, subscript) = parse_expression_no_seq(input, st)?;
        let (input, _) = parse_token(Token::RightSquareBracket)(input)?;
        Ok((
            input,
            Located::new(
                Precedence1Postfix::ArraySubscript(subscript),
                start.to_loc(),
            ),
        ))
    }

    fn expr_p1_right<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        nom::branch::alt((
            expr_p1_increment,
            expr_p1_decrement,
            |input| expr_p1_call(input, st),
            expr_p1_member,
            |input| expr_p1_subscript(input, st),
        ))(input)
    }

    fn cons<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
        let loc = if !input.is_empty() {
            input[0].1
        } else {
            return Err(nom::Err::Incomplete(nom::Needed::new(1)));
        };
        let (input, dtyl) = parse_data_layout(input)?;
        let (input, _) = parse_token(Token::LeftParen)(input)?;
        let (input, list) = nom::multi::separated_list0(
            parse_token(Token::Comma),
            contextual(parse_expression_no_seq, st),
        )(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        Ok((
            input,
            Located::new(Expression::NumericConstructor(dtyl, list), loc),
        ))
    }

    let input = match cons(input, st) {
        Ok((rest, rem)) => return Ok((rest, rem)),
        Err(nom::Err::Incomplete(needed)) => return Err(nom::Err::Incomplete(needed)),
        Err(_) => input,
    };

    let (input, left) = expr_leaf(input, st)?;
    let (input, rights) = nom::multi::many0(|input| expr_p1_right(input, st))(input)?;

    let expr = {
        let loc = left.location;
        let mut final_expression = left;
        for val in rights.iter() {
            final_expression = Located::new(
                match val.node.clone() {
                    Precedence1Postfix::Increment => Expression::UnaryOperation(
                        UnaryOp::PostfixIncrement,
                        Box::new(final_expression),
                    ),
                    Precedence1Postfix::Decrement => Expression::UnaryOperation(
                        UnaryOp::PostfixDecrement,
                        Box::new(final_expression),
                    ),
                    Precedence1Postfix::Call(template_args, args) => {
                        Expression::Call(Box::new(final_expression), template_args, args)
                    }
                    Precedence1Postfix::ArraySubscript(expr) => {
                        Expression::ArraySubscript(Box::new(final_expression), Box::new(expr))
                    }
                    Precedence1Postfix::Member(name) => {
                        Expression::Member(Box::new(final_expression), name)
                    }
                },
                loc,
            )
        }
        final_expression
    };

    Ok((input, expr))
}

fn unaryop_prefix(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
    fn unaryop_increment(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::PlusPlus)(input)?;
        Ok((
            input,
            Located::new(UnaryOp::PrefixIncrement, start.to_loc()),
        ))
    }

    fn unaryop_decrement(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::MinusMinus)(input)?;
        Ok((
            input,
            Located::new(UnaryOp::PrefixDecrement, start.to_loc()),
        ))
    }

    fn unaryop_add(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::Plus)(input)?;
        Ok((input, Located::new(UnaryOp::Plus, start.to_loc())))
    }

    fn unaryop_subtract(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::Minus)(input)?;
        Ok((input, Located::new(UnaryOp::Minus, start.to_loc())))
    }

    fn unaryop_logical_not(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::ExclamationPoint)(input)?;
        Ok((input, Located::new(UnaryOp::LogicalNot, start.to_loc())))
    }

    fn unaryop_bitwise_not(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::Tilde)(input)?;
        Ok((input, Located::new(UnaryOp::BitwiseNot, start.to_loc())))
    }

    nom::branch::alt((
        unaryop_increment,
        unaryop_decrement,
        unaryop_add,
        unaryop_subtract,
        unaryop_logical_not,
        unaryop_bitwise_not,
    ))(input)
}

fn expr_p2<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn expr_p2_unaryop<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, unary) = unaryop_prefix(input)?;
        let (input, expr) = expr_p2(input, st)?;
        Ok((
            input,
            Located::new(
                Expression::UnaryOperation(unary.node.clone(), Box::new(expr)),
                unary.location,
            ),
        ))
    }

    fn expr_p2_cast<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, start) = parse_token(Token::LeftParen)(input)?;
        let (input, cast) = locate(contextual(parse_type, st))(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        let (input, expr) = expr_p2(input, st)?;
        Ok((
            input,
            Located::new(Expression::Cast(cast, Box::new(expr)), start.to_loc()),
        ))
    }

    fn expr_p2_sizeof<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, start) = parse_token(Token::SizeOf)(input)?;
        let (input, _) = parse_token(Token::LeftParen)(input)?;
        let (input, ty) = locate(contextual(parse_type, st))(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        Ok((input, Located::new(Expression::SizeOf(ty), start.to_loc())))
    }

    nom::branch::alt((
        |input| expr_p2_unaryop(input, st),
        |input| expr_p2_cast(input, st),
        |input| expr_p2_sizeof(input, st),
        |input| expr_p1(input, st),
    ))(input)
}

/// Combine binary operations
fn combine_rights(
    left: Located<Expression>,
    rights: Vec<(BinOp, Located<Expression>)>,
) -> Located<Expression> {
    let loc = left.location;
    let mut final_expression = left;
    for val in rights.iter() {
        let (ref op, ref exp) = *val;
        final_expression = Located::new(
            Expression::BinaryOperation(
                op.clone(),
                Box::new(final_expression),
                Box::new(exp.clone()),
            ),
            loc,
        )
    }
    final_expression
}

/// Parse multiple binary operations
fn parse_binary_operations<'t>(
    operator_fn: impl Fn(&'t [LexToken]) -> ParseResult<'t, BinOp>,
    expression_fn: impl Fn(&'t [LexToken], &SymbolTable) -> ParseResult<'t, Located<Expression>>,
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    let (input, left) = expression_fn(input, st)?;

    // Parse as many operators / right expressions as we can
    let mut input = input;
    let mut rights = Vec::new();
    // First attempt to parse an operator
    while let Ok((rest, op)) = operator_fn(input) {
        // Then after an operator is successfully parsed
        // Unconditionally parse right side
        let (rest, right) = expression_fn(rest, st)?;

        rights.push((op, right));
        input = rest;
    }

    let expr = combine_rights(left, rights);
    Ok((input, expr))
}

fn expr_p3<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input.first() {
            Some(LexToken(tok, _)) => {
                let op = match *tok {
                    Token::Asterix => BinOp::Multiply,
                    Token::ForwardSlash => BinOp::Divide,
                    Token::Percent => BinOp::Modulus,
                    _ => {
                        return Err(nom::Err::Error(ParseErrorContext(
                            input,
                            ParseErrorReason::WrongToken,
                        )))
                    }
                };
                Ok((&input[1..], op))
            }
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    parse_binary_operations(parse_op, expr_p2, input, st)
}

fn expr_p4<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input.first() {
            Some(LexToken(tok, _)) => {
                let op = match *tok {
                    Token::Plus => BinOp::Add,
                    Token::Minus => BinOp::Subtract,
                    _ => {
                        return Err(nom::Err::Error(ParseErrorContext(
                            input,
                            ParseErrorReason::WrongToken,
                        )))
                    }
                };
                Ok((&input[1..], op))
            }
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    parse_binary_operations(parse_op, expr_p3, input, st)
}

fn expr_p5<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => {
                Ok((rest, BinOp::LeftShift))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::RightAngleBracket(_), _), rest @ ..] => {
                Ok((rest, BinOp::RightShift))
            }
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p4, input, st)
}

fn expr_p6<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..] => {
                Ok((rest, BinOp::LessEqual))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..] => {
                Ok((rest, BinOp::GreaterEqual))
            }
            [LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => Ok((rest, BinOp::LessThan)),
            [LexToken(Token::RightAngleBracket(_), _), rest @ ..] => Ok((rest, BinOp::GreaterThan)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p5, input, st)
}

fn expr_p7<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::EqualsEquals, _), rest @ ..] => Ok((rest, BinOp::Equality)),
            [LexToken(Token::ExclamationPointEquals, _), rest @ ..] => {
                Ok((rest, BinOp::Inequality))
            }
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p6, input, st)
}

fn expr_p8<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Ampersand, _), rest @ ..] => Ok((rest, BinOp::BitwiseAnd)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p7, input, st)
}

fn expr_p9<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Hat, _), rest @ ..] => Ok((rest, BinOp::BitwiseXor)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p8, input, st)
}

fn expr_p10<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::VerticalBar, _), rest @ ..] => Ok((rest, BinOp::BitwiseOr)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p9, input, st)
}

fn expr_p11<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::AmpersandAmpersand, _), rest @ ..] => Ok((rest, BinOp::BooleanAnd)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p10, input, st)
}

fn expr_p12<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::VerticalBarVerticalBar, _), rest @ ..] => Ok((rest, BinOp::BooleanOr)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p11, input, st)
}

fn expr_p13<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn ternary_right<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, (Located<Expression>, Located<Expression>)> {
        let (input, _) = parse_token(Token::QuestionMark)(input)?;
        let (input, left) = expr_p13(input, st)?;
        let (input, _) = parse_token(Token::Colon)(input)?;
        let (input, right) = expr_p13(input, st)?;
        Ok((input, (left, right)))
    }

    let (input, main) = expr_p12(input, st)?;
    match ternary_right(input, st) {
        Ok((input, (left, right))) => {
            let loc = main.location;
            let expr = Located::new(
                Expression::TernaryConditional(Box::new(main), Box::new(left), Box::new(right)),
                loc,
            );
            Ok((input, expr))
        }
        _ => Ok((input, main)),
    }
}

fn expr_p14<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Equals, _), rest @ ..] => Ok((rest, BinOp::Assignment)),
            [LexToken(Token::PlusEquals, _), rest @ ..] => Ok((rest, BinOp::SumAssignment)),
            [LexToken(Token::MinusEquals, _), rest @ ..] => Ok((rest, BinOp::DifferenceAssignment)),
            [LexToken(Token::AsterixEquals, _), rest @ ..] => Ok((rest, BinOp::ProductAssignment)),
            [LexToken(Token::ForwardSlashEquals, _), rest @ ..] => {
                Ok((rest, BinOp::QuotientAssignment))
            }
            [LexToken(Token::PercentEquals, _), rest @ ..] => {
                Ok((rest, BinOp::RemainderAssignment))
            }
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    fn binary_right<'t>(
        input: &'t [LexToken],
        st: &SymbolTable,
    ) -> ParseResult<'t, (BinOp, Located<Expression>)> {
        let (input, op) = parse_op(input)?;
        let (input, rhs) = expr_p14(input, st)?;
        Ok((input, (op, rhs)))
    }

    let (input, main) = expr_p13(input, st)?;
    match binary_right(input, st) {
        Ok((input, (op, right))) => {
            let loc = main.location;
            let expr = Located::new(
                Expression::BinaryOperation(op, Box::new(main), Box::new(right)),
                loc,
            );
            Ok((input, expr))
        }
        _ => Ok((input, main)),
    }
}

fn expr_p15<'t>(input: &'t [LexToken], st: &SymbolTable) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Comma, _), rest @ ..] => Ok((rest, BinOp::Sequence)),
            [] => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            _ => Err(nom::Err::Error(ParseErrorContext(
                input,
                ParseErrorReason::WrongToken,
            ))),
        }
    }

    parse_binary_operations(parse_op, expr_p14, input, st)
}

/// Parse an expression
pub fn parse_expression<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    expr_p15(input, st)
}

/// Parse an expression in a context where a comma has a different meaning at the top level
pub fn parse_expression_no_seq<'t>(
    input: &'t [LexToken],
    st: &SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    expr_p14(input, st)
}

#[test]
fn test_expression() {
    use test_support::*;

    fn expr(input: &[LexToken]) -> ParseResult<Located<Expression>> {
        let no_symbols = SymbolTable::empty();
        parse_expression(input, &no_symbols)
    }

    assert_eq!(
        expr(
            &[
                Token::Id(Identifier("a".to_string())).loc(0),
                Token::Asterix.loc(1),
                Token::Id(Identifier("b".to_string())).loc(2),
                Token::Eof.loc(3)
            ][..]
        ),
        Ok((
            &[Token::Eof.loc(3)][..],
            Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(0), "b".as_bvar(2)).loc(0)
        ))
    );

    let a_id = Identifier("a".to_string());
    let b_id = Identifier("b".to_string());
    let c_id = Identifier("c".to_string());
    let d_id = Identifier("d".to_string());
    let e_id = Identifier("e".to_string());
    let f_id = Identifier("f".to_string());
    let g_id = Identifier("g".to_string());

    // Test `a + b + c` is `((a + b) + c)`
    let add_chain_tokens = &[
        Token::Id(a_id.clone()).loc(0),
        Token::Plus.noloc(),
        Token::Id(b_id.clone()).loc(1),
        Token::Plus.noloc(),
        Token::Id(c_id.clone()).loc(2),
        Token::Semicolon.noloc(),
    ];
    let add_chain_expr = expr(add_chain_tokens);
    assert_eq!(
        add_chain_expr,
        Ok((
            &[Token::Semicolon.noloc()][..],
            Expression::BinaryOperation(
                BinOp::Add,
                Expression::BinaryOperation(
                    BinOp::Add,
                    Expression::Variable(a_id.0.clone()).bloc(0),
                    Expression::Variable(b_id.0.clone()).bloc(1),
                )
                .bloc(0),
                Expression::Variable(c_id.0.clone()).bloc(2),
            )
            .loc(0),
        ))
    );

    // Test `a, b, c` is `((a, b), c)`
    let comma_chain_tokens = &[
        Token::Id(a_id.clone()).loc(0),
        Token::Comma.noloc(),
        Token::Id(b_id.clone()).loc(1),
        Token::Comma.noloc(),
        Token::Id(c_id.clone()).loc(2),
        Token::Semicolon.noloc(),
    ];
    let comma_chain_expr = expr(comma_chain_tokens);
    assert_eq!(
        comma_chain_expr,
        Ok((
            &[Token::Semicolon.noloc()][..],
            Expression::BinaryOperation(
                BinOp::Sequence,
                Expression::BinaryOperation(
                    BinOp::Sequence,
                    Expression::Variable(a_id.0.clone()).bloc(0),
                    Expression::Variable(b_id.0.clone()).bloc(1),
                )
                .bloc(0),
                Expression::Variable(c_id.0.clone()).bloc(2),
            )
            .loc(0)
        ))
    );

    // Test `a ? b ? c : d : e ? f : g` is `a ? (b ? c : d) : (e ? f : g)`
    let nested_ternary_tokens = &[
        Token::Id(a_id.clone()).loc(0),
        Token::QuestionMark.noloc(),
        Token::Id(b_id.clone()).loc(1),
        Token::QuestionMark.noloc(),
        Token::Id(c_id.clone()).loc(2),
        Token::Colon.noloc(),
        Token::Id(d_id.clone()).loc(3),
        Token::Colon.noloc(),
        Token::Id(e_id.clone()).loc(4),
        Token::QuestionMark.noloc(),
        Token::Id(f_id.clone()).loc(5),
        Token::Colon.noloc(),
        Token::Id(g_id.clone()).loc(6),
        Token::Semicolon.noloc(),
    ];
    let comma_chain_expr = expr(nested_ternary_tokens);
    assert_eq!(
        comma_chain_expr,
        Ok((
            &[Token::Semicolon.noloc()][..],
            Expression::TernaryConditional(
                Expression::Variable(a_id.0).bloc(0),
                Expression::TernaryConditional(
                    Expression::Variable(b_id.0).bloc(1),
                    Expression::Variable(c_id.0).bloc(2),
                    Expression::Variable(d_id.0).bloc(3),
                )
                .bloc(1),
                Expression::TernaryConditional(
                    Expression::Variable(e_id.0).bloc(4),
                    Expression::Variable(f_id.0).bloc(5),
                    Expression::Variable(g_id.0).bloc(6),
                )
                .bloc(4),
            )
            .loc(0)
        ))
    );

    let expr = ParserTester::new(parse_expression);

    expr.check("a", "a".as_var(0));
    expr.check("4", Expression::Literal(Literal::UntypedInt(4)).loc(0));
    expr.check(
        "a+b",
        Expression::BinaryOperation(BinOp::Add, "a".as_bvar(0), "b".as_bvar(2)).loc(0),
    );
    expr.check(
        "a*b",
        Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(0), "b".as_bvar(2)).loc(0),
    );
    expr.check(
        "a + b",
        Expression::BinaryOperation(BinOp::Add, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );

    expr.check(
        "a-b+c",
        Expression::BinaryOperation(
            BinOp::Add,
            Expression::BinaryOperation(BinOp::Subtract, "a".as_bvar(0), "b".as_bvar(2)).bloc(0),
            "c".as_bvar(4),
        )
        .loc(0),
    );
    expr.check(
        "a-b*c",
        Expression::BinaryOperation(
            BinOp::Subtract,
            "a".as_bvar(0),
            Expression::BinaryOperation(BinOp::Multiply, "b".as_bvar(2), "c".as_bvar(4)).bloc(2),
        )
        .loc(0),
    );
    expr.check(
        "a*b-c",
        Expression::BinaryOperation(
            BinOp::Subtract,
            Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(0), "b".as_bvar(2)).bloc(0),
            "c".as_bvar(4),
        )
        .loc(0),
    );
    expr.check(
        "a-b*c",
        Expression::BinaryOperation(
            BinOp::Subtract,
            "a".as_bvar(0),
            Expression::BinaryOperation(BinOp::Multiply, "b".as_bvar(2), "c".as_bvar(4)).bloc(2),
        )
        .loc(0),
    );
    expr.check(
        "a*b-c",
        Expression::BinaryOperation(
            BinOp::Subtract,
            Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(0), "b".as_bvar(2)).bloc(0),
            "c".as_bvar(4),
        )
        .loc(0),
    );
    expr.check(
        "a*(b-c)",
        Expression::BinaryOperation(
            BinOp::Multiply,
            "a".as_bvar(0),
            Expression::BinaryOperation(BinOp::Subtract, "b".as_bvar(3), "c".as_bvar(5)).bloc(2),
        )
        .loc(0),
    );
    expr.check(
        "a*b/c",
        Expression::BinaryOperation(
            BinOp::Divide,
            Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(0), "b".as_bvar(2)).bloc(0),
            "c".as_bvar(4),
        )
        .loc(0),
    );
    expr.check(
        "(a*b)/c",
        Expression::BinaryOperation(
            BinOp::Divide,
            Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(1), "b".as_bvar(3)).bloc(0),
            "c".as_bvar(6),
        )
        .loc(0),
    );
    expr.check(
        "a*(b/c)",
        Expression::BinaryOperation(
            BinOp::Multiply,
            "a".as_bvar(0),
            Expression::BinaryOperation(BinOp::Divide, "b".as_bvar(3), "c".as_bvar(5)).bloc(2),
        )
        .loc(0),
    );

    expr.check(
        "(float3) x",
        Expression::Cast(Type::floatn(3).loc(1), "x".as_bvar(9)).loc(0),
    );

    let ambiguous_sum_or_cast = "(a) + (b)";
    expr.check(
        ambiguous_sum_or_cast,
        Expression::BinaryOperation(BinOp::Add, "a".as_bvar(0), "b".as_bvar(6)).loc(0),
    );
    let st_a_is_type = SymbolTable({
        let mut map = HashMap::new();
        map.insert("a".to_string(), SymbolType::Struct);
        map
    });
    expr.check_symbolic(
        ambiguous_sum_or_cast,
        &st_a_is_type,
        Expression::Cast(
            Type::custom("a").loc(1),
            Expression::UnaryOperation(UnaryOp::Plus, "b".as_bvar(6)).bloc(4),
        )
        .loc(0),
    );

    let numeric_cons = "float2(x, y)";
    let numeric_cons_out = {
        let x = "x".as_var(7);
        let y = "y".as_var(10);
        let ty = DataLayout::Vector(ScalarType::Float, 2);
        let cons = Expression::NumericConstructor(ty, vec![x, y]);
        cons.loc(0)
    };
    expr.check(numeric_cons, numeric_cons_out);

    let fake_cons = "(float2)(x, y)";
    let fake_cons_out = {
        let x = "x".as_bvar(9);
        let y = "y".as_bvar(12);
        let binop = Expression::BinaryOperation(BinOp::Sequence, x, y).bloc(8);
        let cons = Expression::Cast(Type::floatn(2).loc(1), binop);
        cons.loc(0)
    };
    expr.check(fake_cons, fake_cons_out);

    expr.check(
        "a++",
        Expression::UnaryOperation(UnaryOp::PostfixIncrement, "a".as_bvar(0)).loc(0),
    );
    expr.check(
        "a--",
        Expression::UnaryOperation(UnaryOp::PostfixDecrement, "a".as_bvar(0)).loc(0),
    );
    expr.check(
        "++a",
        Expression::UnaryOperation(UnaryOp::PrefixIncrement, "a".as_bvar(2)).loc(0),
    );
    expr.check(
        "--a",
        Expression::UnaryOperation(UnaryOp::PrefixDecrement, "a".as_bvar(2)).loc(0),
    );
    expr.check(
        "+a",
        Expression::UnaryOperation(UnaryOp::Plus, "a".as_bvar(1)).loc(0),
    );
    expr.check(
        "-a",
        Expression::UnaryOperation(UnaryOp::Minus, "a".as_bvar(1)).loc(0),
    );
    expr.check(
        "!a",
        Expression::UnaryOperation(UnaryOp::LogicalNot, "a".as_bvar(1)).loc(0),
    );
    expr.check(
        "~a",
        Expression::UnaryOperation(UnaryOp::BitwiseNot, "a".as_bvar(1)).loc(0),
    );

    expr.check(
        "a << b",
        Expression::BinaryOperation(BinOp::LeftShift, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a >> b",
        Expression::BinaryOperation(BinOp::RightShift, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a < b",
        Expression::BinaryOperation(BinOp::LessThan, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );
    expr.check(
        "a <= b",
        Expression::BinaryOperation(BinOp::LessEqual, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a > b",
        Expression::BinaryOperation(BinOp::GreaterThan, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );
    expr.check(
        "a >= b",
        Expression::BinaryOperation(BinOp::GreaterEqual, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a == b",
        Expression::BinaryOperation(BinOp::Equality, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a != b",
        Expression::BinaryOperation(BinOp::Inequality, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a & b",
        Expression::BinaryOperation(BinOp::BitwiseAnd, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );
    expr.check(
        "a | b",
        Expression::BinaryOperation(BinOp::BitwiseOr, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );
    expr.check(
        "a ^ b",
        Expression::BinaryOperation(BinOp::BitwiseXor, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );
    expr.check(
        "a && b",
        Expression::BinaryOperation(BinOp::BooleanAnd, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );
    expr.check(
        "a || b",
        Expression::BinaryOperation(BinOp::BooleanOr, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );

    expr.expect_fail("a < < b", ParseErrorReason::WrongToken, 4);
    expr.expect_fail("a > > b", ParseErrorReason::WrongToken, 4);
    expr.expect_fail("a < = b", ParseErrorReason::WrongToken, 4);
    expr.expect_fail("a > = b", ParseErrorReason::WrongToken, 4);
    // These pass with a sub expression - needs investigation
    expr.expect_fail("a = = b", ParseErrorReason::TokensUnconsumed, 2);
    expr.expect_fail("a ! = b", ParseErrorReason::TokensUnconsumed, 2);

    expr.check(
        "a[b]",
        Expression::ArraySubscript("a".as_bvar(0), "b".as_bvar(2)).loc(0),
    );
    expr.check(
        "d+a[b+c]",
        Expression::BinaryOperation(
            BinOp::Add,
            "d".as_bvar(0),
            Expression::ArraySubscript(
                "a".as_bvar(2),
                Expression::BinaryOperation(BinOp::Add, "b".as_bvar(4), "c".as_bvar(6)).bloc(4),
            )
            .bloc(2),
        )
        .loc(0),
    );
    expr.check(
        " d + a\t[ b\n+ c ]",
        Expression::BinaryOperation(
            BinOp::Add,
            "d".as_bvar(1),
            Expression::ArraySubscript(
                "a".as_bvar(5),
                Expression::BinaryOperation(BinOp::Add, "b".as_bvar(9), "c".as_bvar(13)).bloc(9),
            )
            .bloc(5),
        )
        .loc(1),
    );

    expr.check(
        " sizeof ( float4 ) ",
        Expression::SizeOf(Type::floatn(4).loc(10)).loc(1),
    );

    expr.check(
        "array.Load",
        Expression::Member("array".as_bvar(0), "Load".to_string()).loc(0),
    );
    expr.check(
        "array.Load()",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".to_string()).bloc(0),
            vec![],
            vec![],
        )
        .loc(0),
    );
    expr.check(
        " array . Load ( ) ",
        Expression::Call(
            Expression::Member("array".as_bvar(1), "Load".to_string()).bloc(1),
            vec![],
            vec![],
        )
        .loc(1),
    );
    expr.check(
        "array.Load(a)",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".to_string()).bloc(0),
            vec![],
            vec!["a".as_var(11)],
        )
        .loc(0),
    );
    expr.check(
        "array.Load(a,b)",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".to_string()).bloc(0),
            vec![],
            vec!["a".as_var(11), "b".as_var(13)],
        )
        .loc(0),
    );
    expr.check(
        "array.Load(a, b)",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".to_string()).bloc(0),
            vec![],
            vec!["a".as_var(11), "b".as_var(14)],
        )
        .loc(0),
    );

    expr.check(
        "array.Load<float4>(i * sizeof(float4))",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".to_string()).bloc(0),
            vec![Type::floatn(4).loc(11)],
            vec![Expression::BinaryOperation(
                BinOp::Multiply,
                "i".as_bvar(19),
                Expression::SizeOf(Type::floatn(4).loc(30)).bloc(23),
            )
            .loc(19)],
        )
        .loc(0),
    );

    expr.check(
        "(float) b",
        Expression::Cast(Type::float().loc(1), "b".as_bvar(8)).loc(0),
    );

    expr.check(
        "float2(b)",
        Expression::NumericConstructor(
            DataLayout::Vector(ScalarType::Float, 2),
            vec!["b".as_var(7)],
        )
        .loc(0),
    );

    expr.check(
        "a = b",
        Expression::BinaryOperation(BinOp::Assignment, "a".as_bvar(0), "b".as_bvar(4)).loc(0),
    );
    expr.check(
        "a = b = c",
        Expression::BinaryOperation(
            BinOp::Assignment,
            "a".as_bvar(0),
            Expression::BinaryOperation(BinOp::Assignment, "b".as_bvar(4), "c".as_bvar(8)).bloc(4),
        )
        .loc(0),
    );

    expr.check(
        "a += b",
        Expression::BinaryOperation(BinOp::SumAssignment, "a".as_bvar(0), "b".as_bvar(5)).loc(0),
    );

    expr.check(
        "a -= b",
        Expression::BinaryOperation(BinOp::DifferenceAssignment, "a".as_bvar(0), "b".as_bvar(5))
            .loc(0),
    );
    expr.check(
        "a *= b",
        Expression::BinaryOperation(BinOp::ProductAssignment, "a".as_bvar(0), "b".as_bvar(5))
            .loc(0),
    );
    expr.check(
        "a /= b",
        Expression::BinaryOperation(BinOp::QuotientAssignment, "a".as_bvar(0), "b".as_bvar(5))
            .loc(0),
    );
    expr.check(
        "a %= b",
        Expression::BinaryOperation(BinOp::RemainderAssignment, "a".as_bvar(0), "b".as_bvar(5))
            .loc(0),
    );

    expr.check(
        "a ? b : c",
        Expression::TernaryConditional("a".as_bvar(0), "b".as_bvar(4), "c".as_bvar(8)).loc(0),
    );
    expr.check(
        "a ? b ? c : d : e",
        Expression::TernaryConditional(
            "a".as_bvar(0),
            Expression::TernaryConditional("b".as_bvar(4), "c".as_bvar(8), "d".as_bvar(12)).bloc(4),
            "e".as_bvar(16),
        )
        .loc(0),
    );
    expr.check(
        "a ? b : c ? d : e",
        Expression::TernaryConditional(
            "a".as_bvar(0),
            "b".as_bvar(4),
            Expression::TernaryConditional("c".as_bvar(8), "d".as_bvar(12), "e".as_bvar(16))
                .bloc(8),
        )
        .loc(0),
    );
    expr.check(
        "a ? b ? c : d : e ? f : g",
        Expression::TernaryConditional(
            "a".as_bvar(0),
            Expression::TernaryConditional("b".as_bvar(4), "c".as_bvar(8), "d".as_bvar(12)).bloc(4),
            Expression::TernaryConditional("e".as_bvar(16), "f".as_bvar(20), "g".as_bvar(24))
                .bloc(16),
        )
        .loc(0),
    );
}
