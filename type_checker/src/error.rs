use std::rc::Rc;

use token::token::Token;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeCheckerErrorKind {
    VariableNotFound
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeCheckerError {
    pub kind: TypeCheckerErrorKind,
    pub token: Token,
    pub message: Option<Rc<str>>
}