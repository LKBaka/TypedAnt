use std::fmt::Display;

use bigdecimal::BigDecimal;
use token::token::Token;

use crate::{expressions::ident::Ident, stmt::Statement};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Int {
        token: Token,
        value: BigDecimal,
    },
    Int64 {
        token: Token,
        value: i64,
    },
    Ident(Ident),
    Infix {
        token: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Function {
        token: Token,
        name: Option<Token>,
        params: Vec<Box<Expression>>,
        block: Box<Statement>,
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => write!(f, "{}", value),
            Self::Int64 { value, .. } => write!(f, "{}", value),
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Function {
                params,
                name,
                block,
                ..
            } => write!(
                f,
                "func {}({}) {{{}}}",
                name.as_ref()
                    .map_or_else(|| "".into(), |it| it.value.clone()),
                params
                    .iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                block.to_string()
            ),
            Self::Infix { token, left, right } => write!(f, "({}{}{})", left, token.value, right),
        }
    }
}
