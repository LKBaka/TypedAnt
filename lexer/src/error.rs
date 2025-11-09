use std::{fmt::Display, rc::Rc};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Ord, Eq)]
pub enum LexerErrorKind {
    UnClosedString,
    InvalidUnicodeEscapeSequence,
}

impl Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::UnClosedString => "unclosed string",
            Self::InvalidUnicodeEscapeSequence => "invalid unicode escape sequence",
        };

        write!(f, "{s}")
    }
}

#[derive(Clone, Debug)]
pub struct LexerError {
    pub file: Rc<str>,
    pub line: usize,
    pub column: usize,
    pub kind: LexerErrorKind,
    pub message: Option<Rc<str>>,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(msg) = &self.message {
            write!(
                f,
                "{}. {}\n    (at file '{}', at line {}, at column {})",
                self.kind, msg, self.file, self.line, self.column
            )
        } else {
            write!(
                f,
                "{}.\n    (at file '{}', at line {}, at column {})",
                self.kind, self.file, self.line, self.column
            )
        }
    }
}
