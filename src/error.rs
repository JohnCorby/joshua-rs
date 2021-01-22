use crate::Rule;
use std::char::ParseCharError;
use std::fmt::Debug;
use std::num::{ParseFloatError, ParseIntError};
use std::option::NoneError;
use std::str::ParseBoolError;
use thiserror::Error;

pub type MyResult<T> = Result<T, MyError>;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("option returned none")]
    NoneError,

    #[error("rule {0:?} unreachable")]
    UnreachableRule(Rule),

    #[error("expected rule {expected:?}, but got {actual:?}")]
    UnexpectedRule { expected: Rule, actual: Rule },

    #[error(transparent)]
    ParseFloatError(#[from] ParseFloatError),
    #[error(transparent)]
    ParseIntError(#[from] ParseIntError),
    #[error(transparent)]
    ParseBoolError(#[from] ParseBoolError),
    #[error(transparent)]
    ParseCharError(#[from] ParseCharError),

    #[error(transparent)]
    PestError(#[from] pest::error::Error<crate::Rule>),

    #[error("error: {0}")]
    Other(String),
}

impl From<NoneError> for MyError {
    fn from(_: NoneError) -> Self {
        MyError::NoneError
    }
}

impl From<String> for MyError {
    fn from(string: String) -> Self {
        Self::Other(string)
    }
}
