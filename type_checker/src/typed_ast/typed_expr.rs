use std::fmt::Display;

use bigdecimal::BigDecimal;
use token::token::Token;

use crate::{
    Ty,
    typed_ast::{GetType, typed_expressions::ident::Ident, typed_stmt::TypedStatement},
};

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum IntValue {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    ISize(isize),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    USize(usize),
}

impl Display for IntValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntValue::I64(it) => write!(f, "{it}"),
            IntValue::I32(it) => write!(f, "{it}"),
            IntValue::I16(it) => write!(f, "{it}"),
            IntValue::I8(it) => write!(f, "{it}"),
            IntValue::ISize(it) => write!(f, "{it}"),
            IntValue::U32(it) => write!(f, "{it}"),
            IntValue::U64(it) => write!(f, "{it}"),
            IntValue::U16(it) => write!(f, "{it}"),
            IntValue::U8(it) => write!(f, "{it}"),
            IntValue::USize(it) => write!(f, "{it}"),
        }
    }
}

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
