use ast::expr::Expression;

use crate::{ParseResult, Parser, precedence::get_token_precedence};

pub fn parse_infix(parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
    let token = parser.cur_token.clone();

    let precedence = if token.value != "+".into() {
        get_token_precedence(token.token_type)
    } else {
        get_token_precedence(token.token_type) - 1
    };

    parser.next_token(); // 离开 中缀运算符

    let right = parser.parse_expression(precedence)?;

    Ok(Expression::Infix { token, left: Box::new(left), right: Box::new(right) })
}
