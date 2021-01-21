use std::char::ParseCharError;
use std::fmt::Debug;
use std::num::{ParseFloatError, ParseIntError};
use std::option::NoneError;
use std::str::ParseBoolError;
use thiserror::Error;

pub type MyResult<T> = Result<T, MyError>;

#[derive(Error, Debug)]
pub enum MyError {
    #[error("option returend none")]
    NoneError,

    #[error("rule {0:?} unreachable")]
    UnreachableRule(crate::Rule),

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

    #[error(transparent)]
    AnyhowError(#[from] anyhow::Error),
}

impl From<NoneError> for MyError {
    fn from(_: NoneError) -> Self {
        MyError::NoneError
    }
}
