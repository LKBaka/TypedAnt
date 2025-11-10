use std::fmt::Display;

use token::token::Token;

use crate::typed_ast::typed_stmt::TypedStatement;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypedNode {
    Program {
        token: Token,
        statements: Vec<TypedStatement>
    }
}

impl Display for TypedNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Program { statements, .. } => {
                write!(
                    f, "{}",
                    statements
                        .iter()
                        .map(|it| it.to_string())
                        .collect::<Vec<String>>()
                        .join("\n")
                )
            }
        }
    }
}