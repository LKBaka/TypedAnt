use std::fmt::Display;

use token::token::Token;

use crate::{expr::Expression, expressions::ident::Ident};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    ExpressionStatement(Expression),
    Return {
        token: Token,
        expr: Expression,
    },
    Block {
        token: Token,
        statements: Vec<Statement>,
    },
    Let {
        token: Token,
        name: Ident,
        var_type: Option<Ident>,
        value: Expression,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpressionStatement(expr) => expr.fmt(f),
            Self::Return { expr, .. } => write!(f, "return {expr}"),
            Self::Let {
                name,
                var_type,
                value,
                ..
            } => match var_type {
                Some(ty) => write!(f, "let {name}: {ty} = {value}",),
                None => write!(f, "let {name} = {value}"),
            },
            Self::Block { statements, .. } => write!(
                f,
                "{}",
                statements
                    .iter()
                    .map(|it| "\t".to_owned() + &it.to_string())
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
        }
    }
}
