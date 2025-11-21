use ast::{expr::Expression, expressions::ident::Ident};
use token::token_type::TokenType;

use crate::{ParseResult, Parser};

pub fn parse_type_hint(parser: &mut Parser) -> ParseResult<Expression> {
    let token = parser.cur_token.clone();

    parser.expect_peek(TokenType::Colon)?;

    parser.next_token(); // 前进到冒号

    parser.expect_peek(TokenType::Ident)?;

    parser.next_token(); // 前进到标识符

    let ty_ident = parser.cur_token.clone();

    Ok(Expression::TypeHint(
        Ident {
            value: token.value.clone(),
            token,
        },
        Ident {
            value: ty_ident.value.clone(),
            token: ty_ident,
        },
    ))
}
