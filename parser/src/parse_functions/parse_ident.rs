use ast::{expr::Expression, expressions::ident::Ident};

use crate::{ParseResult, Parser};

pub fn parse_ident(parser: &mut Parser) -> ParseResult<Expression> {
    Ok(Expression::Ident(Ident {
        token: parser.cur_token.clone(),
        value: parser.cur_token.value.clone()
    }))
}
