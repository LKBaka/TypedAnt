use std::{fmt::Display, rc::Rc};

use bigdecimal::BigDecimal;
use token::token::Token;

use crate::{expressions::ident::Ident, stmt::Statement};

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
pub enum Expression {
    BigInt {
        token: Token,
        value: BigDecimal,
    },
    Int {
        token: Token,
        value: IntValue,
    },
    Ident(Ident),
    TypeHint(Ident, Ident),
    Infix {
        token: Token,
        op: Rc<str>,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Function {
        token: Token,
        name: Option<Token>,
        params: Vec<Box<Expression>>,
        block: Box<Statement>,
        ret_ty: Option<Ident>
    },
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BigInt { value, .. } => write!(f, "{}", value),
            Self::Int { value, .. } => write!(f, "{}", value),
            Self::TypeHint(ident, ty) => write!(f, "{ident}: {ty}"),
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Function {
                params,
                name,
                block,
                ret_ty,
                .. 
            } => write!(
                f,
                "func {}({}){}{{{}}}",
                name.as_ref()
                    .map_or_else(|| "".into(), |it| it.value.clone()),
                params
                    .iter()
                    .map(|it| it.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                ret_ty.as_ref()
                    .map_or_else(|| "".into(), |it| format!(" -> {it} ")),
                block.to_string()
            ),
            Self::Infix { op, left, right, .. } => write!(f, "({}{}{})", left, op, right),
        }
    }
}
