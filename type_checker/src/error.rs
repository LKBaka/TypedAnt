pub enum TypeCheckerErrorKind {
    VariableNotFound
}

pub struct TypeCheckerError {
    kind: TypeCheckerErrorKind
}