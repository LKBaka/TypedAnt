pub mod test;
use ast::{
    expr::{Expression, IntValue},
    node::Node,
    stmt::Statement,
};
use token::token::Token;

use crate::{
    error::{TypeCheckerError, TypeCheckerErrorKind},
    table::{TypeTable, str_to_ty},
    typed_ast::{
        GetType, typed_expr::TypedExpression, typed_expressions::ident::Ident,
        typed_node::TypedNode, typed_stmt::TypedStatement,
    },
};

pub mod error;
pub mod table;
pub mod typed_ast;

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
    U8,
}

impl From<IntValue> for IntTy {
    fn from(value: IntValue) -> Self {
        match value {
            IntValue::ISize(_) => Self::ISize,
            IntValue::I64(_) => Self::I64,
            IntValue::I32(_) => Self::I32,
            IntValue::I16(_) => Self::I16,
            IntValue::I8(_) => Self::I8,
            IntValue::USize(_) => Self::USize,
            IntValue::U64(_) => Self::U64,
            IntValue::U32(_) => Self::U32,
            IntValue::U16(_) => Self::U16,
            IntValue::U8(_) => Self::U8,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    BigInt,
    Function,
    IntTy(IntTy),
}

type CheckResult<T> = Result<T, TypeCheckerError>;

pub struct TypeChecker<'table> {
    table: &'table mut TypeTable,
}

impl TypeChecker<'_> {
    pub fn check_node(&mut self, node: Node) -> CheckResult<TypedNode> {
        match node {
            Node::Program { token, statements } => {
                let mut typed_statements = vec![];

                for stmt in statements {
                    typed_statements.push(self.check_statement(stmt)?);
                }

                Ok(TypedNode::Program {
                    token,
                    statements: typed_statements,
                })
            }
        }
    }

    pub fn check_expr(&mut self, expr: Expression) -> CheckResult<TypedExpression> {
        match expr {
            Expression::BigInt { token, value } => Ok(TypedExpression::BigInt {
                token,
                value,
                ty: Ty::BigInt,
            }),
            Expression::Int { token, value } => Ok(TypedExpression::Int {
                token,
                value,
                ty: Ty::IntTy(value.into()),
            }),

            Expression::Ident(it) => {
                let ident_name = &it.value;

                match self.table.get(&ident_name) {
                    Some(symbol) => Ok(TypedExpression::Ident(
                        Ident {
                            token: it.token,
                            value: it.value,
                        },
                        *symbol.ty.get_type(),
                    )),
                    None => Err(Self::push_err(
                        None,
                        TypeCheckerErrorKind::VariableNotFound,
                        it.token,
                    )),
                }
            }

            _ => todo!(),
        }
    }

    pub fn check_statement(&mut self, stmt: Statement) -> CheckResult<TypedStatement> {
        match stmt {
            Statement::ExpressionStatement(expr) => {
                Ok(TypedStatement::ExpressionStatement(self.check_expr(expr)?))
            }

            Statement::Let {
                token,
                name,
                var_type,
                value,
            } => {
                let ty_name = if let Some(ref ty) = var_type {
                    ty
                } else {
                    todo!()
                };

                let ty = match str_to_ty(&ty_name.value) {
                    Some(it) => it,
                    None => todo!(),
                };

                self.table.define_var(&name.value, ty);

                let typed_val = self.check_expr(value)?;

                Ok(TypedStatement::Let {
                    token: token.clone(),
                    name: Ident {
                        token,
                        value: name.value,
                    },
                    var_type: match var_type {
                        Some(it) => Some(Ident {
                            token: it.token,
                            value: it.value,
                        }),

                        None => None,
                    },
                    ty,
                    value: typed_val,
                })
            }

            _ => todo!(),
        }
    }

    pub fn push_err(
        message: Option<&str>,
        kind: TypeCheckerErrorKind,
        token: Token,
    ) -> TypeCheckerError {
        TypeCheckerError {
            kind,
            token,
            message: message.map_or_else(|| None, |it| Some(it.into())),
        }
    }
}
