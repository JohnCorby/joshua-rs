use crate::Rule;
use std::backtrace::Backtrace;
use std::char::ParseCharError;
use std::fmt::{Debug, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::option::NoneError;
use std::str::ParseBoolError;
use std::sync::TryLockError;
use thiserror::Error;

pub type MyResult<T> = Result<T, MyError>;

#[derive(Error)]
pub enum MyError {
    #[error("option returned none\n{0}")]
    NoneError(Backtrace),

    #[error("unexpected rule {0:?}\n{1}")]
    UnexpectedRule(Rule, Backtrace),

    #[error("expected rule {expected:?}, but got {actual:?}\n{backtrace}")]
    WrongRule {
        expected: Rule,
        actual: Rule,
        backtrace: Backtrace,
    },

    #[error("{0}\n{1}")]
    ParseFloatError(#[from] ParseFloatError, Backtrace),
    #[error("{0}\n{1}")]
    ParseIntError(#[from] ParseIntError, Backtrace),
    #[error("{0}\n{1}")]
    ParseBoolError(#[from] ParseBoolError, Backtrace),
    #[error("{0}\n{1}")]
    ParseCharError(#[from] ParseCharError, Backtrace),

    #[error("{0}\n{1}")]
    PestError(#[from] pest::error::Error<Rule>, Backtrace),

    #[error("{0}\n{1}")]
    Other(String, Backtrace),
}

impl Debug for MyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

pub fn unexpected_rule<T>(rule: Rule) -> MyResult<T> {
    Err(MyError::UnexpectedRule(rule, Backtrace::capture()))
}

impl From<NoneError> for MyError {
    fn from(_: NoneError) -> Self {
        Self::NoneError(Backtrace::capture())
    }
}

impl From<String> for MyError {
    fn from(string: String) -> Self {
        Self::Other(string, Backtrace::capture())
    }
}

impl<T> From<TryLockError<T>> for MyError {
    fn from(e: TryLockError<T>) -> Self {
        e.to_string().into()
    }
}
