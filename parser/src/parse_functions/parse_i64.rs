use std::{num::IntErrorKind, str::FromStr};

use ast::expr::{Expression, IntValue};

use crate::{ParseResult, Parser, error::{ParseIntErrorKind, ParserErrorKind}};

pub fn parse_i64(parser: &mut Parser) -> ParseResult<Expression> {
    Ok(Expression::Int {
        token: parser.cur_token.clone(),
        value: match i64::from_str(&parser.cur_token.value) {
            Ok(it) => IntValue::I64(it),
            Err(it) => return Err(parser.make_error(
                ParserErrorKind::ParseInt64Error(match it.kind() {
                    IntErrorKind::Empty => ParseIntErrorKind::Empty,
                    IntErrorKind::InvalidDigit => ParseIntErrorKind::InvalidDigit,
                    IntErrorKind::PosOverflow => ParseIntErrorKind::PosOverflow,
                    IntErrorKind::NegOverflow => ParseIntErrorKind::NegOverflow,
                    _ => unreachable!()
                }),
                None
            ))
        }
    })
}
