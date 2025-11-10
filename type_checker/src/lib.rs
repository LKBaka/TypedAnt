use ast::node::Node;

use crate::{error::TypeCheckerError, table::TypeTable, typed_ast::typed_node::TypedNode};

pub mod typed_ast;
pub mod table;
pub mod error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntTy {
    ISize,
    I64,
    I32,
    I16,
    I8,
    USize,
    U64,
    U32,
    U16,
    U8
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    BigInt,
    IntTy(IntTy)
}

type CheckResult<T> = Result<T, TypeCheckerError>;

pub struct TypeChecker {
    table: TypeTable,
}

impl TypeChecker {
    pub fn check_node(node: Node) -> CheckResult<TypedNode> {
        todo!()
    }
}