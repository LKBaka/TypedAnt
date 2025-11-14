use crate::Ty;

pub mod typed_expressions;
pub mod typed_expr;
pub mod typed_stmt;
pub mod typed_node;

pub trait GetType {
    fn get_type(&self) -> Ty;
}