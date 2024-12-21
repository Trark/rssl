use super::types::parse_type_id;
use super::*;

/// Try to parse a literal
fn expr_literal(input: &[LexToken]) -> ParseResult<Located<Expression>> {
    match input.first() {
        Some(LexToken(tok, ref loc)) => {
            let literal = match *tok {
                Token::LiteralInt(v) => Literal::IntUntyped(v),
                Token::LiteralIntUnsigned32(v) => Literal::IntUnsigned32(v),
                Token::LiteralIntUnsigned64(v) => Literal::IntUnsigned64(v),
                Token::LiteralIntSigned64(v) => Literal::IntSigned64(v),
                Token::LiteralFloat(v) => Literal::FloatUntyped(v),
                Token::LiteralFloat16(v) => Literal::Float16(v),
                Token::LiteralFloat32(v) => Literal::Float32(v),
                Token::LiteralFloat64(v) => Literal::Float64(v),
                Token::LiteralString(ref s) => Literal::String(s.clone()),
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
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Located<Expression>> {
    let (input, start) = parse_token(Token::LeftParen)(input)?;
    let (input, expr) = parse_expression_internal(input, resolver, Terminator::Standard)?;
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
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Located<Expression>> {
    // Try to parse an expression nested in parenthesis
    let res = expr_in_paren(input, resolver);

    // Try to parse a variable identifier
    let res = res.select(parse_scoped_identifier_expr(input));

    // Try to parse a literal
    res.select(expr_literal(input))
}

/// Parse either an expression or a type
fn parse_expression_or_type<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, ExpressionOrType> {
    if let Ok((rest, ty)) = parse_type_id(input, resolver) {
        return Ok((rest, ExpressionOrType::Type(ty)));
    }

    let (rest, expr) = parse_expression_internal(input, resolver, Terminator::TypeList)?;
    Ok((rest, ExpressionOrType::Expression(expr)))
}

/// Parse a list of template arguments
fn parse_template_args_req<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Vec<ExpressionOrType>> {
    let (input, _) = match_left_angle_bracket(input)?;
    let (input, type_args) = parse_list(
        parse_token(Token::Comma),
        contextual2(parse_expression_or_type, resolver),
    )(input)?;
    let (input, _) = match_right_angle_bracket(input)?;
    Ok((input, type_args))
}

/// Parse a list of template arguments or no arguments
pub fn parse_template_args<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Vec<ExpressionOrType>> {
    match parse_template_args_req(input, resolver) {
        Ok(ok) => Ok(ok),
        Err(_) => Ok((input, Vec::new())),
    }
}

fn expr_p1<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
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
        resolver: &dyn SymbolResolver,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        // If we fail before reaching the call parenthesis then other parse trees may be better fits
        let start_input = input;

        let (input, template_args) =
            parse_template_args(input, resolver).rebase_fail_point(start_input)?;

        let (input, start) = parse_token(Token::LeftParen)(input).rebase_fail_point(start_input)?;

        let mut input = input;
        let mut args = Vec::new();
        if parse_token(Token::RightParen)(input).is_err() {
            loop {
                let (rest, arg) = parse_expression_internal(input, resolver, Terminator::Sequence)?;

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
        resolver: &dyn SymbolResolver,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        let (input, start) = parse_token(Token::LeftSquareBracket)(input)?;
        let (input, subscript) = parse_expression_internal(input, resolver, Terminator::Sequence)?;
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
        resolver: &dyn SymbolResolver,
    ) -> ParseResult<'t, Located<Precedence1Postfix>> {
        expr_p1_increment(input)
            .select(expr_p1_decrement(input))
            .select(expr_p1_call(input, resolver))
            .select(expr_p1_member(input))
            .select(expr_p1_subscript(input, resolver))
    }

    fn right_side_ops<'t>(
        input: &'t [LexToken],
        resolver: &dyn SymbolResolver,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, left) = expr_leaf(input, resolver)?;

        // Parse as many operations as we can
        // If we partially parsed something then fail
        let mut input = input;
        let mut rights = Vec::new();
        loop {
            match expr_p1_right(input, resolver) {
                Ok((rest, right)) => {
                    input = rest;
                    rights.push(right);
                }
                Err(ParseErrorContext(rest, _, _)) if rest.len() == input.len() => {
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

    right_side_ops(input, resolver)
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

    fn unaryop_dereference(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::Asterix)(input)?;
        Ok((input, Located::new(UnaryOp::Dereference, start.to_loc())))
    }

    fn unaryop_address_of(input: &[LexToken]) -> ParseResult<Located<UnaryOp>> {
        let (input, start) = parse_token(Token::Ampersand)(input)?;
        Ok((input, Located::new(UnaryOp::AddressOf, start.to_loc())))
    }

    unaryop_increment(input)
        .select(unaryop_decrement(input))
        .select(unaryop_add(input))
        .select(unaryop_subtract(input))
        .select(unaryop_logical_not(input))
        .select(unaryop_bitwise_not(input))
        .select(unaryop_dereference(input))
        .select(unaryop_address_of(input))
}

fn expr_p2<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn expr_p2_unaryop<'t>(
        input: &'t [LexToken],

        resolver: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, unary) = unaryop_prefix(input)?;
        let (input, expr) = expr_p2(input, resolver, terminator)?;
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
        resolver: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, start) = parse_token(Token::LeftParen)(input)?;
        let (input, cast) = contextual2(parse_type_id, resolver)(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        let (input, expr) = expr_p2(input, resolver, terminator)?;
        Ok((
            input,
            Located::new(
                Expression::Cast(Box::new(cast), Box::new(expr)),
                start.to_loc(),
            ),
        ))
    }

    fn expr_p2_sizeof<'t>(
        input: &'t [LexToken],
        resolver: &dyn SymbolResolver,
    ) -> ParseResult<'t, Located<Expression>> {
        let (input, start) = parse_token(Token::SizeOf)(input)?;
        let (input, _) = parse_token(Token::LeftParen)(input)?;
        let (input, ty) = contextual2(parse_expression_or_type, resolver)(input)?;
        let (input, _) = parse_token(Token::RightParen)(input)?;
        Ok((
            input,
            Located::new(Expression::SizeOf(Box::new(ty)), start.to_loc()),
        ))
    }

    expr_p2_unaryop(input, resolver, terminator)
        .select(expr_p2_cast(input, resolver, terminator))
        .select(expr_p2_sizeof(input, resolver))
        .select(expr_p1(input, resolver))
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
    operator_fn: impl Fn(&'t [LexToken], &dyn SymbolResolver, Terminator) -> ParseResult<'t, BinOp>,
    expression_fn: impl Fn(
        &'t [LexToken],
        &dyn SymbolResolver,
        Terminator,
    ) -> ParseResult<'t, Located<Expression>>,
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    let (input, left) = expression_fn(input, resolver, terminator)?;

    // Parse as many operators / right expressions as we can
    let mut input = input;
    let mut rights = Vec::new();
    // First attempt to parse an operator
    while let Ok((rest, op)) = operator_fn(input, resolver, terminator) {
        // Then after an operator is successfully parsed
        // Unconditionally parse right side
        let (rest, right) = expression_fn(rest, resolver, terminator)?;

        rights.push((op, right));
        input = rest;
    }

    let expr = combine_rights(left, rights);
    Ok((input, expr))
}

/// Parse multiple binary operations
fn parse_binary_operations<'t>(
    operator_fn: impl Fn(&'t [LexToken]) -> ParseResult<'t, BinOp>,
    expression_fn: impl Fn(
        &'t [LexToken],
        &dyn SymbolResolver,
        Terminator,
    ) -> ParseResult<'t, Located<Expression>>,
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    parse_binary_operations_st(
        |tokens, _, _| operator_fn(tokens),
        expression_fn,
        input,
        resolver,
        terminator,
    )
}

fn expr_p3<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
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

    parse_binary_operations(parse_op, expr_p2, input, resolver, terminator)
}

fn expr_p4<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
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

    parse_binary_operations(parse_op, expr_p3, input, resolver, terminator)
}

fn expr_p5<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op<'t>(
        input: &'t [LexToken],
        _: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, BinOp> {
        match input {
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), ..] =>
            {
                // Reject tokens that will become <<=
                ParseErrorReason::wrong_token(input)
            }
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::LeftAngleBracket(_), _), rest @ ..]
                if rest.is_empty() || rest[0].0 != Token::Equals =>
            {
                Ok((rest, BinOp::LeftShift))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), ..] =>
            {
                // Reject tokens that will become >>=
                ParseErrorReason::wrong_token(input)
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::RightAngleBracket(_), _), rest @ ..]
                if terminator != Terminator::TypeList
                    && (rest.is_empty() || rest[0].0 != Token::Equals) =>
            {
                Ok((rest, BinOp::RightShift))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations_st(parse_op, expr_p4, input, resolver, terminator)
}

fn expr_p6<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op<'t>(
        input: &'t [LexToken],
        _: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, BinOp> {
        match input {
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..] => {
                Ok((rest, BinOp::LessEqual))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..]
                if terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::GreaterEqual))
            }
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), ..] =>
            {
                // Reject tokens that will become <<=
                ParseErrorReason::wrong_token(input)
            }
            [LexToken(Token::LeftAngleBracket(_), _), rest @ ..] => Ok((rest, BinOp::LessThan)),
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), ..] =>
            {
                // Reject tokens that will become >>=
                ParseErrorReason::wrong_token(input)
            }
            [LexToken(Token::RightAngleBracket(_), _), rest @ ..]
                if terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::GreaterThan))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations_st(parse_op, expr_p5, input, resolver, terminator)
}

fn expr_p7<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
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

    parse_binary_operations(parse_op, expr_p6, input, resolver, terminator)
}

fn expr_p8<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Ampersand, _), rest @ ..] => Ok((rest, BinOp::BitwiseAnd)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p7, input, resolver, terminator)
}

fn expr_p9<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::Hat, _), rest @ ..] => Ok((rest, BinOp::BitwiseXor)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p8, input, resolver, terminator)
}

fn expr_p10<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::VerticalBar, _), rest @ ..] => Ok((rest, BinOp::BitwiseOr)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p9, input, resolver, terminator)
}

fn expr_p11<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::AmpersandAmpersand, _), rest @ ..] => Ok((rest, BinOp::BooleanAnd)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p10, input, resolver, terminator)
}

fn expr_p12<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op(input: &[LexToken]) -> ParseResult<BinOp> {
        match input {
            [LexToken(Token::VerticalBarVerticalBar, _), rest @ ..] => Ok((rest, BinOp::BooleanOr)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations(parse_op, expr_p11, input, resolver, terminator)
}

fn expr_p13<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn ternary_right<'t>(
        input: &'t [LexToken],
        resolver: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, (Located<Expression>, Located<Expression>)> {
        let (input, _) = parse_token(Token::QuestionMark)(input)?;
        let (input, left) = expr_p13(input, resolver, terminator)?;
        let (input, _) = parse_token(Token::Colon)(input)?;
        let (input, right) = expr_p13(input, resolver, terminator)?;
        Ok((input, (left, right)))
    }

    let (input, main) = expr_p12(input, resolver, terminator)?;
    match ternary_right(input, resolver, terminator) {
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
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
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
            [LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::LeftAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..] => {
                Ok((rest, BinOp::LeftShiftAssignment))
            }
            [LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::RightAngleBracket(FollowedBy::Token), _), LexToken(Token::Equals, _), rest @ ..] => {
                Ok((rest, BinOp::RightShiftAssignment))
            }
            [LexToken(Token::AmpersandEquals, _), rest @ ..] => {
                Ok((rest, BinOp::BitwiseAndAssignment))
            }
            [LexToken(Token::VerticalBarEquals, _), rest @ ..] => {
                Ok((rest, BinOp::BitwiseOrAssignment))
            }
            [LexToken(Token::HatEquals, _), rest @ ..] => Ok((rest, BinOp::BitwiseXorAssignment)),
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    fn binary_right<'t>(
        input: &'t [LexToken],
        resolver: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, (BinOp, Located<Expression>)> {
        let (input, op) = parse_op(input)?;
        let (input, rhs) = expr_p14(input, resolver, terminator)?;
        Ok((input, (op, rhs)))
    }

    let (input, main) = expr_p13(input, resolver, terminator)?;
    match binary_right(input, resolver, terminator) {
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
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    fn parse_op<'t>(
        input: &'t [LexToken],
        _: &dyn SymbolResolver,
        terminator: Terminator,
    ) -> ParseResult<'t, BinOp> {
        match input {
            [LexToken(Token::Comma, _), rest @ ..]
                if terminator != Terminator::Sequence && terminator != Terminator::TypeList =>
            {
                Ok((rest, BinOp::Sequence))
            }
            [] => ParseErrorReason::end_of_stream(),
            _ => ParseErrorReason::wrong_token(input),
        }
    }

    parse_binary_operations_st(parse_op, expr_p14, input, resolver, terminator)
}

/// Parse an expression from the top priority
pub fn parse_expression_internal<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
    terminator: Terminator,
) -> ParseResult<'t, Located<Expression>> {
    expr_p15(input, resolver, terminator)
}

/// Parse an expression
pub fn parse_expression<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Located<Expression>> {
    parse_expression_internal(input, resolver, Terminator::Standard)
}

/// Parse an expression in a context where a comma has a different meaning at the top level
pub fn parse_expression_no_seq<'t>(
    input: &'t [LexToken],
    resolver: &dyn SymbolResolver,
) -> ParseResult<'t, Located<Expression>> {
    parse_expression_internal(input, resolver, Terminator::Sequence)
}

#[test]
fn test_expression_leafs() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check("a", "a".as_var(0));
    expr.check("4", Expression::Literal(Literal::IntUntyped(4)).loc(0));
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
fn test_condition_template_arg_ambiguity() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    // This is almost ambiguous as a template argument <b && c> for a
    expr.check(
        "a < b && c > d",
        Expression::BinaryOperation(
            BinOp::BooleanAnd,
            Expression::BinaryOperation(BinOp::LessThan, "a".as_bvar(0), "b".as_bvar(4)).bloc(0),
            Expression::BinaryOperation(BinOp::GreaterThan, "c".as_bvar(9), "d".as_bvar(13))
                .bloc(9),
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
    expr.check(
        "*a",
        Expression::UnaryOperation(UnaryOp::Dereference, "a".as_bvar(1)).loc(0),
    );
    expr.check(
        "&a",
        Expression::UnaryOperation(UnaryOp::AddressOf, "a".as_bvar(1)).loc(0),
    );
}

#[test]
fn test_cast() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check(
        "(float) b",
        Expression::Cast(Box::new(TypeId::from("float".loc(1))), "b".as_bvar(8)).loc(0),
    );

    expr.check(
        "(float3) x",
        Expression::Cast(Box::new(TypeId::from("float3".loc(1))), "x".as_bvar(9)).loc(0),
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
        Expression::Cast(
            Box::new(TypeId::from("float2".loc(1))),
            Expression::BinaryOperation(BinOp::Sequence, "x".as_bvar(9), "y".as_bvar(12)).bloc(8),
        )
        .loc(0),
    );

    // Use flaot2 as an example for not being a type
    expr.check(
        "(flaot2)(x, y)",
        Expression::Call(
            Expression::Identifier("flaot2".loc(1).into()).loc(0).into(),
            Vec::new(),
            Vec::from(["x".as_var(9), "y".as_var(12)]),
        )
        .loc(0),
    );
}

#[test]
fn test_ambiguous() {
    use test_support::*;
    let expr = ParserTester::new(parse_expression);

    expr.check(
        "(a) + (b)",
        Expression::BinaryOperation(BinOp::Add, "a".as_bvar2(1, 0), "b".as_bvar2(7, 6)).loc(0),
    );

    expr.check(
        "(float) + (b)",
        Expression::Cast(
            Box::new(TypeId::from("float".loc(1))),
            Box::new(Expression::UnaryOperation(UnaryOp::Plus, "b".as_bvar2(11, 10)).loc(8)),
        )
        .loc(0),
    );

    expr.check(
        "(a) + (b) + (c)",
        Expression::BinaryOperation(
            BinOp::Add,
            Expression::BinaryOperation(BinOp::Add, "a".as_bvar2(1, 0), "b".as_bvar2(7, 6)).bloc(0),
            "c".as_bvar2(13, 12),
        )
        .loc(0),
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
        Expression::SizeOf(Box::new(ExpressionOrType::Type(TypeId::from(
            "float4".loc(10),
        ))))
        .loc(1),
    );

    expr.check(
        " sizeof ( flaot4 ) ",
        Expression::SizeOf(Box::new(ExpressionOrType::Expression(
            Expression::Identifier("flaot4".loc(10).into()).loc(10),
        )))
        .loc(1),
    );

    expr.check(
        " sizeof ( float4* ) ",
        Expression::SizeOf(Box::new(ExpressionOrType::Type(TypeId {
            base: Type::from("float4".loc(10)),
            abstract_declarator: Declarator::Pointer(PointerDeclarator {
                attributes: Vec::new(),
                qualifiers: Default::default(),
                inner: Box::new(Declarator::Empty),
            }),
        })))
        .loc(1),
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
            vec![ExpressionOrType::Type(TypeId::from("float4".loc(11)))],
            vec![Expression::BinaryOperation(
                BinOp::Multiply,
                "i".as_bvar(19),
                Expression::SizeOf(Box::new(ExpressionOrType::Type(TypeId::from(
                    "float4".loc(30),
                ))))
                .bloc(23),
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

    expr.check(
        "a <<= b",
        Expression::BinaryOperation(BinOp::LeftShiftAssignment, "a".as_bvar(0), "b".as_bvar(6))
            .loc(0),
    );

    expr.check(
        "a >>= b",
        Expression::BinaryOperation(BinOp::RightShiftAssignment, "a".as_bvar(0), "b".as_bvar(6))
            .loc(0),
    );

    expr.check(
        "a &= b",
        Expression::BinaryOperation(BinOp::BitwiseAndAssignment, "a".as_bvar(0), "b".as_bvar(5))
            .loc(0),
    );

    expr.check(
        "a |= b",
        Expression::BinaryOperation(BinOp::BitwiseOrAssignment, "a".as_bvar(0), "b".as_bvar(5))
            .loc(0),
    );

    expr.check(
        "a ^= b",
        Expression::BinaryOperation(BinOp::BitwiseXorAssignment, "a".as_bvar(0), "b".as_bvar(5))
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

    expr.expect_fail(
        "func(4 * sizeof(const uint|2))",
        ParseErrorReason::WrongToken,
        26,
    );
    expr.expect_fail(
        "func(7, 4 * sizeof(const uint|2))",
        ParseErrorReason::WrongToken,
        29,
    );
    expr.expect_fail(
        "func(4 * sizeof(uint)))",
        ParseErrorReason::TokensUnconsumed,
        22,
    );
}
