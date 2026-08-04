use ast::expr::Expression;

use crate::{ParseResult, Parser, precedence::Precedence};

pub fn parse_bool_and(parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
    let token = parser.cur_token.clone();

    parser.next_token();

    let right = parser.parse_expression(Precedence::LogicalAnd)?;

    Ok(Expression::BoolAnd {
        left: Box::new(left),
        right: Box::new(right),
        token,
    })
}

pub fn parse_bool_or(parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
    let token = parser.cur_token.clone();

    parser.next_token();

    let right = parser.parse_expression(Precedence::LogicalOr)?;

    Ok(Expression::BoolOr {
        left: Box::new(left),
        right: Box::new(right),
        token,
    })
}
