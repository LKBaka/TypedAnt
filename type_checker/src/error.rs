use std::rc::Rc;

use token::token::Token;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeCheckerErrorKind {
    VariableNotFound,
    TypeNotFound,
    TypeMismatch,
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeCheckerError {
    pub kind: TypeCheckerErrorKind,
    pub token: Option<Token>,
    pub message: Option<Rc<str>>
}