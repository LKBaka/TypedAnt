use ast::node::Node;

use crate::{error::TypeCheckerError, table::TypeTable};

pub mod table;
pub mod error;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    BigInt,
    Int64,
}

type CheckResult<T> = Result<T, TypeCheckerError>;

pub struct TypeChecker {
    table: TypeTable,
}

impl TypeChecker {
    pub fn check_node(program: Node) {
        todo!()
    }
}