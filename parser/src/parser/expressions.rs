use super::errors::get_result_significance;
use super::types::parse_type_with_symbols;
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
                _ => return ParseErrorReason::wrong_token(input),
            };
            Ok((
                &input[1..],
                Located::new(Expression::Literal(literal), *loc),
            ))
        }
        None => ParseErrorReason::end_of_stream(),
    }
}

/// Try to parse an expression inside parenthesis
fn expr_in_paren<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    let (input, start) = parse_token(Token::LeftParen)(input)?;
    let (input, expr) = parse_expression_internal(input, st, Terminator::Standard)?;
    let (input, _) = parse_token(Token::RightParen)(input)?;

    Ok((input, Located::new(expr.to_node(), start.to_loc())))
}

/// Parse an identifier that may be a variable name
pub fn parse_scoped_identifier(input: &[LexToken]) -> ParseResult<ScopedIdentifier> {
    let (base, input) = match parse_token(Token::ScopeResolution)(input) {
        Ok((input, _)) => (ScopedIdentifierBase::Absolute, input),
        _ => (ScopedIdentifierBase::Relative, input),
    };

    let (input, identifiers) =
        parse_list_nonempty(parse_token(Token::ScopeResolution), parse_variable_name)(input)?;

    Ok((input, ScopedIdentifier { base, identifiers }))
}

/// Parse an identifier that may be a variable name to an expression
fn parse_scoped_identifier_expr(input: &[LexToken]) -> ParseResult<Located<Expression>> {
    let (rest, id) = parse_scoped_identifier(input)?;

    assert_ne!(input.len(), rest.len());
    let loc = input[0].1;

    Ok((rest, Located::new(Expression::Identifier(id), loc)))
}

/// Try to parse one of the base components of an expression
fn expr_leaf<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    // Try to parse an expression nested in parenthesis
    let res = expr_in_paren(input, st);

    // Try to parse a variable identifier
    let res = res.select(parse_scoped_identifier_expr(input));

    // Try to parse a literal
    res.select(expr_literal(input))
}

/// Parse either an expression or a type
fn parse_expression_or_type(input: &[LexToken]) -> ParseResult<ExpressionOrType> {
    let type_result = parse_type(input);
    let expr_result = parse_expression_resolve_symbols(input, Terminator::TypeList);

    match (type_result, expr_result) {
        (Ok((rest_ty, ty)), Ok((rest_expr, expr))) => match rest_ty.len().cmp(&rest_expr.len()) {
            std::cmp::Ordering::Less => Ok((rest_ty, ExpressionOrType::Type(ty))),
            std::cmp::Ordering::Equal => Ok((rest_ty, ExpressionOrType::Either(expr, ty))),
            std::cmp::Ordering::Greater => Ok((rest_expr, ExpressionOrType::Expression(expr))),
        },
        (Ok((rest, ty)), Err(_)) => Ok((rest, ExpressionOrType::Type(ty))),
        (Err(_), Ok((rest, expr))) => Ok((rest, ExpressionOrType::Expression(expr))),
        (Err(err), Err(_)) => Err(err),
    }
}

/// Parse a list of template arguments
fn parse_template_args_req(input: &[LexToken]) -> ParseResult<Vec<ExpressionOrType>> {
    let (input, _) = match_left_angle_bracket(input)?;
    let (input, type_args) =
        parse_list(parse_token(Token::Comma), parse_expression_or_type)(input)?;
    let (input, _) = match_right_angle_bracket(input)?;
    Ok((input, type_args))
}

/// Parse a list of template arguments or no arguments
pub fn parse_template_args(input: &[LexToken]) -> ParseResult<Vec<ExpressionOrType>> {
    match parse_template_args_req(input) {
        Ok(ok) => Ok(ok),
        Err(_) => Ok((input, Vec::new())),
    }
}

fn expr_p1<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    #[derive(Clone)]
    enum Precedence1Postfix {
        Increment,
        Decrement,
        Call(Vec<ExpressionOrType>, Vec<Located<Expression>>),
        ArraySubscript(Located<Expression>),
        Member(ScopedIdentifier),
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
        let (input, member) = locate(parse_scoped_identifier)(input)?;
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
        st: &mut SymbolTable,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        let (input, template_args) = parse_template_args(input)?;

        let (input, start) = parse_token(Token::LeftParen)(input)?;

        let mut input = input;
        let mut args = Vec::new();
        if parse_token(Token::RightParen)(input).is_err() {
            loop {
                let (rest, arg) = parse_expression_internal(input, st, Terminator::Sequence)?;

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

        let call = Precedence1Postfix::Call(template_args, args);
        Ok((input, Located::new(call, start.to_loc())))
    }

    fn expr_p1_subscript<'t>(
        input: &'t [LexToken],
        st: &mut SymbolTable,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        let (input, start) = parse_token(Token::LeftSquareBracket)(input)?;
        let (input, subscript) = parse_expression_internal(input, st, Terminator::Sequence)?;
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
        st: &mut SymbolTable,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        expr_p1_increment(input)
            .select(expr_p1_decrement(input))
            .select(expr_p1_call(input, st))
            .select(expr_p1_member(input))
            .select(expr_p1_subscript(input, st))
    }

    fn right_side_ops<'t>(
        input: &'t [LexToken],
        st: &mut SymbolTable,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, left) = expr_leaf(input, st)?;

        // Parse as many operations as we can
        // If we partially parsed something then fail
        let mut input = input;
        let mut rights = Vec::new();
        loop {
            match expr_p1_right(input, st) {
                Ok((rest, right)) => {
                    input = rest;
                    rights.push(right);
                }
                Err(ParseErrorContext(rest, _)) if rest.len() == input.len() => {
                    break;
                }
                Err(err) => return Err(err),
            }
        }

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

    right_side_ops(input, st)
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

    unaryop_increment(input)
        .select(unaryop_decrement(input))
        .select(unaryop_add(input))
        .select(unaryop_subtract(input))
        .select(unaryop_logical_not(input))
        .select(unaryop_bitwise_not(input))
}

fn expr_p2<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn expr_p2_unaryop<'t>(
        input: &'t [LexToken],
        st: &mut SymbolTable,
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
        st: &mut SymbolTable,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, start) = parse_token(Token::LeftParen)(input)?;
        let (input, cast) = contextual(parse_type_with_symbols, st)(input)?;
        st.assumed_symbols.push(cast.layout.0.clone().unlocate());
        let (input, _) = parse_token(Token::RightParen)(input)?;
        let (input, expr) = expr_p2(input, st)?;
        Ok((
            input,
            Located::new(Expression::Cast(cast, Box::new(expr)), start.to_loc()),
        ))
    }

    fn expr_p2_sizeof<'t>(
        input: &'t [LexToken],
        st: &mut SymbolTable,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, start) = parse_token(Token::SizeOf)(input)?;
        let (input, _) = parse_token(Token::LeftParen)(input)?;
        let (input, ty) = contextual(parse_type_with_symbols, st)(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        Ok((input, Located::new(Expression::SizeOf(ty), start.to_loc())))
    }

    expr_p2_unaryop(input, st)
        .select(expr_p2_cast(input, st))
        .select(expr_p2_sizeof(input, st))
        .select(expr_p1(input, st))
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

/// Parse multiple binary operations where the binary operation can access the symbol table
fn parse_binary_operations_st<'t>(
    operator_fn: impl Fn(&'t [LexToken], &mut SymbolTable) -> ParseResult<'t, BinOp>,
    expression_fn: impl Fn(&'t [LexToken], &mut SymbolTable) -> ParseResult<'t, Located<Expression>>,
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    let (input, left) = expression_fn(input, st)?;

    // Parse as many operators / right expressions as we can
    let mut input = input;
    let mut rights = Vec::new();
    // First attempt to parse an operator
    while let Ok((rest, op)) = operator_fn(input, st) {
        // Then after an operator is successfully parsed
        // Unconditionally parse right side
        let (rest, right) = expression_fn(rest, st)?;

        rights.push((op, right));
        input = rest;
    }

    let expr = combine_rights(left, rights);
    Ok((input, expr))
}

/// Parse multiple binary operations
fn parse_binary_operations<'t>(
    operator_fn: impl Fn(&'t [LexToken]) -> ParseResult<'t, BinOp>,
    expression_fn: impl Fn(&'t [LexToken], &mut SymbolTable) -> ParseResult<'t, Located<Expression>>,
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    parse_binary_operations_st(|tokens, _| operator_fn(tokens), expression_fn, input, st)
}

fn expr_p3<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input.first() {
            Some(LexToken(tok, _)) => {
                let op = match *tok {
                    Token::Asterix => BinOp::Multiply,
                    Token::ForwardSlash => BinOp::Divide,
                    Token::Percent => BinOp::Modulus,
                    _ => return ParseErrorReason::wrong_token(input),
                };
                Ok((&input[1..], op))
            }
            None => ParseErrorReason::end_of_stream(),
        }
    }

    parse_binary_operations(parse_op, expr_p2, input, st)
}

fn expr_p4<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input.first() {
            Some(LexToken(tok, _)) => {
                let op = match *tok {
                    Token::Plus => BinOp::Add,
                    Token::Minus => BinOp::Subtract,
                    _ => return ParseErrorReason::wrong_token(input),
                };
                Ok((&input[1..], op))
            }
            None => ParseErrorReason::end_of_stream(),
        }
    }

    parse_binary_operations(parse_op, expr_p3, input, st)
}

fn expr_p5<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op<'t>(input: &'t [LexToken], st: &mut SymbolTable) -> ParseResult<'t, BinOp> {
        match input {
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => {
                Ok((rest, BinOp::LeftShift))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::RightAngleBracket(_), _), rest @ ..]
                if st.terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::RightShift))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations_st(parse_op, expr_p4, input, st)
}

fn expr_p6<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op<'t>(input: &'t [LexToken], st: &mut SymbolTable) -> ParseResult<'t, BinOp> {
        match input {
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..] => {
                Ok((rest, BinOp::LessEqual))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..]
                if st.terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::GreaterEqual))
            }
            [LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => Ok((rest, BinOp::LessThan)),
            [LexToken(Token::RightAngleBracket(_), _), rest @ ..]
                if st.terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::GreaterThan))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations_st(parse_op, expr_p5, input, st)
}

fn expr_p7<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::EqualsEquals, _), rest @ ..] => Ok((rest, BinOp::Equality)),
            [LexToken(Token::ExclamationPointEquals, _), rest @ ..] => {
                Ok((rest, BinOp::Inequality))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p6, input, st)
}

fn expr_p8<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Ampersand, _), rest @ ..] => Ok((rest, BinOp::BitwiseAnd)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p7, input, st)
}

fn expr_p9<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Hat, _), rest @ ..] => Ok((rest, BinOp::BitwiseXor)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p8, input, st)
}

fn expr_p10<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::VerticalBar, _), rest @ ..] => Ok((rest, BinOp::BitwiseOr)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p9, input, st)
}

fn expr_p11<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::AmpersandAmpersand, _), rest @ ..] => Ok((rest, BinOp::BooleanAnd)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p10, input, st)
}

fn expr_p12<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::VerticalBarVerticalBar, _), rest @ ..] => Ok((rest, BinOp::BooleanOr)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p11, input, st)
}

fn expr_p13<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn ternary_right<'t>(
        input: &'t [LexToken],
        st: &mut SymbolTable,
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

fn expr_p14<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
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
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    fn binary_right<'t>(
        input: &'t [LexToken],
        st: &mut SymbolTable,
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

fn expr_p15<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op<'t>(input: &'t [LexToken], st: &mut SymbolTable) -> ParseResult<'t, BinOp> {
        match input {
            [LexToken(Token::Comma, _), rest @ ..]
                if st.terminator != Terminator::Sequence
                    && st.terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::Sequence))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations_st(parse_op, expr_p14, input, st)
}

/// Variant of parse_expression to be used inside the symbol loop
fn parse_expression_internal<'t>(
    input: &'t [LexToken],
    st: &mut SymbolTable,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    let old_terminator = st.terminator;
    st.terminator = terminator;
    let result = expr_p15(input, st);
    st.terminator = old_terminator;
    result
}

struct SymbolQueue {
    added_symbols: Vec<ScopedIdentifier>,
    added_symbols_expanded: Vec<Vec<ScopedIdentifier>>,
    queue: Vec<Vec<ScopedIdentifier>>,
}

impl SymbolQueue {
    fn add_to_queue(&mut self, s: &ScopedIdentifier) {
        if !self.added_symbols.contains(s) {
            self.added_symbols.push(s.clone());

            // Low quality all combinations creation
            let mut new_expanded = self.added_symbols_expanded.clone();
            for l in &self.added_symbols_expanded {
                let mut with_new = l.clone();
                with_new.push(s.clone());
                new_expanded.push(with_new.clone());
                self.queue.push(with_new);
            }
            self.added_symbols_expanded = new_expanded;
        }
    }
}

/// Parse an expression with all possible combinations of type vs non-type symbols
pub fn parse_expression_resolve_symbols(
    input: &[LexToken],
    terminator: Terminator,
) -> ParseResult<Located<Expression>> {
    // Assume any symbol may be a type on the first pass
    let mut st = SymbolTable {
        reject_symbols: HashSet::new(),
        assumed_symbols: Vec::new(),
        terminator,
    };

    // Run the parser
    let first_result = expr_p15(input, &mut st);

    // Early out if there were no types to be ambiguous over
    if st.assumed_symbols.is_empty() {
        return first_result;
    }

    // For all the seen symbols try again but assuming they are not a type
    let mut queue = SymbolQueue {
        added_symbols: Vec::new(),
        added_symbols_expanded: Vec::from([Vec::new()]),
        queue: Vec::new(),
    };

    // Start by adding the symbols seen in the first run
    for symbol in &st.assumed_symbols {
        queue.add_to_queue(symbol);
    }

    let mut highest_significance = get_result_significance(&first_result);
    let mut results = Vec::from([(first_result, st.assumed_symbols, highest_significance)]);

    while let Some(symbols) = queue.queue.pop() {
        let mut st = SymbolTable {
            reject_symbols: HashSet::new(),
            assumed_symbols: Vec::new(),
            terminator,
        };
        for s in symbols {
            st.reject_symbols.insert(s);
        }

        let next_result = expr_p15(input, &mut st);

        // Add any symbols that only appeared in later runs
        for s in &st.assumed_symbols {
            queue.add_to_queue(s);
        }

        let significance = get_result_significance(&next_result);
        highest_significance = std::cmp::min(highest_significance, significance);
        results.push((next_result, st.assumed_symbols, significance));
    }

    // Remove parses that do not reach the end of the expression region
    let mut results = results
        .into_iter()
        .filter(|(_, _, s)| *s == highest_significance)
        .collect::<Vec<_>>();

    assert!(!results.is_empty());

    // If there were no successful chains then return the first as the error
    let all_fail = results.iter().all(|(r, _, _)| r.is_err());
    if all_fail {
        return results.pop().unwrap().0;
    }

    // Filter to just the successful results - and remove significance values
    let mut results = results
        .into_iter()
        .filter_map(|(r, t, _)| match r {
            Ok(r) => Some((r, t)),
            Err(_) => None,
        })
        .collect::<Vec<_>>();

    // Order results so shorter symbol lists come first
    results.sort_by(|(_, t1), (_, t2)| match t1.len().cmp(&t2.len()) {
        std::cmp::Ordering::Equal => t1.cmp(t2),
        other => other,
    });

    let mut reduced_results = Vec::<((&[LexToken], _), _)>::with_capacity(results.len());
    for result in results {
        // Check for redundancy with previous expression
        // If the expression is the same and has a more restrictive symbol requirement then discard it
        let mut same = false;
        for selected_result in &reduced_results {
            if selected_result.0 .1 == result.0 .1 {
                assert_eq!(selected_result.0 .0.len(), result.0 .0.len());
                let mut symbol_not_covered = false;
                for s in &selected_result.1 {
                    if !result.1.contains(s) {
                        symbol_not_covered = true;
                    }
                }
                if !symbol_not_covered {
                    same = true;
                }
            }
        }

        if !same {
            reduced_results.push((result.0, result.1));
        }
    }
    let mut results = reduced_results;

    assert!(!results.is_empty());
    if results.len() == 1 {
        Ok(results.pop().unwrap().0)
    } else {
        let mut output_expressions = Vec::new();
        let tokens = results[0].0 .0;
        for next_result in results.into_iter().rev() {
            assert_eq!(tokens, next_result.0 .0);
            output_expressions.push(ConstrainedExpression {
                expr: next_result.0 .1,
                expected_type_names: next_result.1,
            })
        }

        Ok((
            tokens,
            Located::none(Expression::AmbiguousParseBranch(output_expressions)),
        ))
    }
}

/// Parse an expression
pub fn parse_expression(input: &[LexToken]) -> ParseResult<Located<Expression>> {
    parse_expression_resolve_symbols(input, Terminator::Standard)
}

/// Parse an expression in a context where a comma has a different meaning at the top level
pub fn parse_expression_no_seq(input: &[LexToken]) -> ParseResult<Located<Expression>> {
    parse_expression_resolve_symbols(input, Terminator::Sequence)
}

#[test]
fn test_expression_leafs() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check("a", "a".as_var(0));
    expr.check("4", Expression::Literal(Literal::UntypedInt(4)).loc(0));
}

#[test]
fn test_binary_op_single() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check_from_tokens(
        &[
            Token::Id(Identifier("a".to_string())).loc(0),
            Token::Asterix.loc(1),
            Token::Id(Identifier("b".to_string())).loc(2),
            Token::Eof.loc(3),
        ],
        3,
        Expression::BinaryOperation(BinOp::Multiply, "a".as_bvar(0), "b".as_bvar(2)).loc(0),
    );

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
}

#[test]
fn test_binary_op_multi() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

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

    // Test `a + b + c` is `((a + b) + c)`
    expr.check_from_tokens(
        &[
            Token::Id(Identifier("a".to_string())).loc(0),
            Token::Plus.noloc(),
            Token::Id(Identifier("b".to_string())).loc(1),
            Token::Plus.noloc(),
            Token::Id(Identifier("c".to_string())).loc(2),
            Token::Semicolon.noloc(),
        ],
        5,
        Expression::BinaryOperation(
            BinOp::Add,
            Expression::BinaryOperation(BinOp::Add, "a".as_bvar(0), "b".as_bvar(1)).bloc(0),
            "c".as_bvar(2),
        )
        .loc(0),
    );
}

#[test]
fn test_invalid_op_symbols() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    //compile_error!("refactor this");

    expr.expect_fail("a < < b", ParseErrorReason::WrongToken, 4);
    expr.expect_fail("a > > b", ParseErrorReason::WrongToken, 4);
    expr.expect_fail("a < = b", ParseErrorReason::WrongToken, 4);
    expr.expect_fail("a > = b", ParseErrorReason::WrongToken, 4);
    // These pass with a sub expression - needs investigation
    expr.expect_fail("a = = b", ParseErrorReason::TokensUnconsumed, 2);
    expr.expect_fail("a ! = b", ParseErrorReason::TokensUnconsumed, 2);
}

#[test]
fn test_unary_op() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

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
}

#[test]
fn test_cast() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check(
        "(float) b",
        Expression::Cast(Type::from("float".loc(1)), "b".as_bvar(8)).loc(0),
    );

    expr.check(
        "(float3) x",
        Expression::Cast(Type::from("float3".loc(1)), "x".as_bvar(9)).loc(0),
    );

    expr.check(
        "float2(b)",
        Expression::Call(
            Expression::Identifier("float2".loc(0).into()).loc(0).into(),
            Vec::new(),
            vec!["b".as_var(7)],
        )
        .loc(0),
    );

    expr.check(
        "float2(x, y)",
        Expression::Call(
            Expression::Identifier("float2".loc(0).into()).loc(0).into(),
            Vec::new(),
            vec!["x".as_var(7), "y".as_var(10)],
        )
        .loc(0),
    );

    // Check that this structure that looks like a constructor is actually a cast on a sequenced expression if float2 is a type
    // Else it may be a function call if float2 happened to be a function call
    expr.check(
        "(float2)(x, y)",
        Located::none(Expression::AmbiguousParseBranch(Vec::from([
            ConstrainedExpression {
                expr: Expression::Cast(
                    Type::from("float2".loc(1)),
                    Expression::BinaryOperation(BinOp::Sequence, "x".as_bvar(9), "y".as_bvar(12))
                        .bloc(8),
                )
                .loc(0),
                expected_type_names: Vec::from([ScopedIdentifier::trivial("float2")]),
            },
            ConstrainedExpression {
                expr: Expression::Call(
                    Expression::Identifier("float2".loc(1).into()).loc(0).into(),
                    Vec::new(),
                    Vec::from(["x".as_var(9), "y".as_var(12)]),
                )
                .loc(0),
                expected_type_names: Vec::new(),
            },
        ]))),
    );
}

#[test]
fn test_ambiguous() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    let ambiguous_sum_or_cast = "(a) + (b)";
    expr.check(
        ambiguous_sum_or_cast,
        Located::none(Expression::AmbiguousParseBranch(Vec::from([
            ConstrainedExpression {
                expr: Expression::Cast(
                    Type::from("a".loc(1)),
                    Expression::UnaryOperation(UnaryOp::Plus, "b".as_bvar2(7, 6)).bloc(4),
                )
                .loc(0),
                expected_type_names: Vec::from([ScopedIdentifier::trivial("a")]),
            },
            ConstrainedExpression {
                expr: Expression::BinaryOperation(
                    BinOp::Add,
                    "a".as_bvar2(1, 0),
                    "b".as_bvar2(7, 6),
                )
                .loc(0),
                expected_type_names: Vec::from([]),
            },
        ]))),
    );

    let ambiguous_3 = "(a) + (b) + (c)";
    expr.check(
        ambiguous_3,
        Located::none(Expression::AmbiguousParseBranch(Vec::from([
            ConstrainedExpression {
                expr: Expression::Cast(
                    Type::from("a".loc(1)),
                    Expression::UnaryOperation(
                        UnaryOp::Plus,
                        Expression::Cast(
                            Type::from("b".loc(7)),
                            Expression::UnaryOperation(UnaryOp::Plus, "c".as_bvar2(13, 12))
                                .bloc(10),
                        )
                        .bloc(6),
                    )
                    .bloc(4),
                )
                .loc(0),
                expected_type_names: Vec::from([
                    ScopedIdentifier::trivial("a"),
                    ScopedIdentifier::trivial("b"),
                ]),
            },
            ConstrainedExpression {
                expr: Expression::BinaryOperation(
                    BinOp::Add,
                    "a".as_bvar2(1, 0),
                    Expression::Cast(
                        Type::from("b".loc(7)),
                        Expression::UnaryOperation(UnaryOp::Plus, "c".as_bvar2(13, 12)).bloc(10),
                    )
                    .bloc(6),
                )
                .loc(0),
                expected_type_names: Vec::from([ScopedIdentifier::trivial("b")]),
            },
            ConstrainedExpression {
                expr: Expression::BinaryOperation(
                    BinOp::Add,
                    Expression::Cast(
                        Type::from("a".loc(1)),
                        Expression::UnaryOperation(UnaryOp::Plus, "b".as_bvar2(7, 6)).bloc(4),
                    )
                    .bloc(0),
                    "c".as_bvar2(13, 12),
                )
                .loc(0),
                expected_type_names: Vec::from([ScopedIdentifier::trivial("a")]),
            },
            ConstrainedExpression {
                expr: Expression::BinaryOperation(
                    BinOp::Add,
                    Expression::BinaryOperation(BinOp::Add, "a".as_bvar2(1, 0), "b".as_bvar2(7, 6))
                        .bloc(0),
                    "c".as_bvar2(13, 12),
                )
                .loc(0),
                expected_type_names: Vec::from([]),
            },
        ]))),
    );
}

#[test]
fn test_array_index() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

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
}

#[test]
fn test_sizeof() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check(
        " sizeof ( float4 ) ",
        Expression::SizeOf(Type::from("float4".loc(10))).loc(1),
    );
}

#[test]
fn test_member_access() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    // Simple member access
    expr.check(
        "s.x",
        Expression::Member("s".as_bvar(0), "x".loc(2).into()).loc(0),
    );

    // Member access where the member name is a qualified name
    expr.check(
        "s.S::x",
        Expression::Member(
            "s".as_bvar(0),
            ScopedIdentifier {
                base: ScopedIdentifierBase::Relative,
                identifiers: Vec::from(["S".to_string().loc(2), "x".to_string().loc(5)]),
            },
        )
        .loc(0),
    );

    // Member access where the member name is a qualified name from the root namespace
    expr.check(
        "s.::S::x",
        Expression::Member(
            "s".as_bvar(0),
            ScopedIdentifier {
                base: ScopedIdentifierBase::Absolute,
                identifiers: Vec::from(["S".to_string().loc(4), "x".to_string().loc(7)]),
            },
        )
        .loc(0),
    );

    // Member access where the member name and object name are qualified names from the root namespace
    expr.check(
        "::M::s.::S::x",
        Expression::Member(
            Box::new(
                Expression::Identifier(ScopedIdentifier {
                    base: ScopedIdentifierBase::Absolute,
                    identifiers: Vec::from(["M".to_string().loc(2), "s".to_string().loc(5)]),
                })
                .loc(0),
            ),
            ScopedIdentifier {
                base: ScopedIdentifierBase::Absolute,
                identifiers: Vec::from(["S".to_string().loc(9), "x".to_string().loc(12)]),
            },
        )
        .loc(0),
    );
}

#[test]
fn test_method_call() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check(
        "array.Load",
        Expression::Member("array".as_bvar(0), "Load".loc(6).into()).loc(0),
    );
    expr.check(
        "array.Load()",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".loc(6).into()).bloc(0),
            vec![],
            vec![],
        )
        .loc(0),
    );
    expr.check(
        " array . Load ( ) ",
        Expression::Call(
            Expression::Member("array".as_bvar(1), "Load".loc(9).into()).bloc(1),
            vec![],
            vec![],
        )
        .loc(1),
    );
    expr.check(
        "array.Load(a)",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".loc(6).into()).bloc(0),
            vec![],
            vec!["a".as_var(11)],
        )
        .loc(0),
    );
    expr.check(
        "array.Load(a,b)",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".loc(6).into()).bloc(0),
            vec![],
            vec!["a".as_var(11), "b".as_var(13)],
        )
        .loc(0),
    );
    expr.check(
        "array.Load(a, b)",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".loc(6).into()).bloc(0),
            vec![],
            vec!["a".as_var(11), "b".as_var(14)],
        )
        .loc(0),
    );

    expr.check(
        "array.Load<float4>(i * sizeof(float4))",
        Expression::Call(
            Expression::Member("array".as_bvar(0), "Load".loc(6).into()).bloc(0),
            vec![ExpressionOrType::from("float4".loc(11))],
            vec![Expression::BinaryOperation(
                BinOp::Multiply,
                "i".as_bvar(19),
                Expression::SizeOf(Type::from("float4".loc(30))).bloc(23),
            )
            .loc(19)],
        )
        .loc(0),
    );
}

#[test]
fn test_assignment() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

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
}

#[test]
fn test_assignment_with_operators() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

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
}

#[test]
fn test_sequence_order() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    // Test `a, b, c` is `((a, b), c)`
    expr.check_from_tokens(
        &[
            Token::Id(Identifier("a".to_string())).loc(0),
            Token::Comma.noloc(),
            Token::Id(Identifier("b".to_string())).loc(1),
            Token::Comma.noloc(),
            Token::Id(Identifier("c".to_string())).loc(2),
            Token::Semicolon.noloc(),
        ],
        5,
        Expression::BinaryOperation(
            BinOp::Sequence,
            Expression::BinaryOperation(BinOp::Sequence, "a".as_bvar(0), "b".as_bvar(1)).bloc(0),
            "c".as_bvar(2),
        )
        .loc(0),
    );
}

#[test]
fn test_ternary() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    // Check basic form
    expr.check(
        "a ? b : c",
        Expression::TernaryConditional("a".as_bvar(0), "b".as_bvar(4), "c".as_bvar(8)).loc(0),
    );

    // Test this is `a ? (b ? c : d) : e`
    expr.check(
        "a ? b ? c : d : e",
        Expression::TernaryConditional(
            "a".as_bvar(0),
            Expression::TernaryConditional("b".as_bvar(4), "c".as_bvar(8), "d".as_bvar(12)).bloc(4),
            "e".as_bvar(16),
        )
        .loc(0),
    );

    // Test this is `a ? b : (c ? d : e)`
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

    // Test this is `a ? (b ? c : d) : (e ? f : g)`
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

    // Test `a ? b ? c : d : e ? f : g` from tokens is `a ? (b ? c : d) : (e ? f : g)`
    expr.check_from_tokens(
        &[
            Token::Id(Identifier("a".to_string())).loc(0),
            Token::QuestionMark.noloc(),
            Token::Id(Identifier("b".to_string())).loc(1),
            Token::QuestionMark.noloc(),
            Token::Id(Identifier("c".to_string())).loc(2),
            Token::Colon.noloc(),
            Token::Id(Identifier("d".to_string())).loc(3),
            Token::Colon.noloc(),
            Token::Id(Identifier("e".to_string())).loc(4),
            Token::QuestionMark.noloc(),
            Token::Id(Identifier("f".to_string())).loc(5),
            Token::Colon.noloc(),
            Token::Id(Identifier("g".to_string())).loc(6),
            Token::Semicolon.noloc(),
        ],
        13,
        Expression::TernaryConditional(
            "a".as_bvar(0),
            Expression::TernaryConditional("b".as_bvar(1), "c".as_bvar(2), "d".as_bvar(3)).bloc(1),
            Expression::TernaryConditional("e".as_bvar(4), "f".as_bvar(5), "g".as_bvar(6)).bloc(4),
        )
        .loc(0),
    );
}

#[test]
fn test_expression_fail_locations() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.expect_fail("func(4 * sizeof(uint|2))", ParseErrorReason::WrongToken, 20);
    expr.expect_fail(
        "func(7, 4 * sizeof(uint|2))",
        ParseErrorReason::WrongToken,
        23,
    );
    expr.expect_fail(
        "func(4 * sizeof(uint)))",
        ParseErrorReason::TokensUnconsumed,
        22,
    );
}
