use ast::expr::Expression;
use token::token_type::TokenType;

use crate::{ParseResult, Parser};

pub fn parse_call(parser: &mut Parser, left: Expression) -> ParseResult<Expression> {
    let token = parser.cur_token.clone();

    let args = parser.parse_expression_list(TokenType::RParen)?;

    Ok(Expression::Call { token, func: Box::new(left), args })
}
