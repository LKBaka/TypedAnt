use std::fmt::Display;

use ast::expr::IntValue;
use bigdecimal::BigDecimal;
use token::token::Token;

use crate::{
    Ty,
    typed_ast::{GetType, typed_expressions::ident::Ident, typed_stmt::TypedStatement},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypedExpression {
    BigInt {
        token: Token,
        value: BigDecimal,
        ty: Ty,
    },
    Int {
        token: Token,
        value: IntValue,
        ty: Ty,
    },
    Ident(Ident, Ty),
    Infix {
        token: Token,
        left: Box<TypedExpression>,
        right: Box<TypedExpression>,
        ty: Ty,
    },
    Function {
        token: Token,
        name: Option<Token>,
        params: Vec<Box<TypedExpression>>,
        block: Box<TypedStatement>,
        ty: Ty,
    },
}

impl Display for TypedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BigInt { value, .. } => write!(f, "{}", value),
            Self::Int { value, .. } => write!(f, "{}", value),
            Self::Ident(ident, _) => write!(f, "{}", ident),
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
            Self::Infix {
                token, left, right, ..
            } => write!(f, "({}{}{})", left, token.value, right),
        }
    }
}

impl GetType for TypedExpression {
    fn get_type(&self) -> &Ty {
        match self {
            Self::BigInt { ty, .. } => ty,
            Self::Int { ty, .. } => ty,
            Self::Ident(_, ty) => ty,
            Self::Function { ty, .. } => ty,
            Self::Infix { ty, .. } => ty,
        }
    }
}
