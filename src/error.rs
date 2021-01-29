use crate::parse::Rule;
use crate::pos::Pos;
use parking_lot::Mutex;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::char::ParseCharError;
use std::fmt::{Debug, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::ops::Deref;
use std::option::NoneError;
use std::str::ParseBoolError;

static CURRENT_BACKTRACE: Mutex<Option<Backtrace>> = Mutex::new(None);

pub type MyResult<T> = Result<T, MyError>;
pub struct MyError(String);

impl Debug for MyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match Pos::current().deref() {
            Some(pos) => writeln!(f, "{}", pos.to_string(&self.0))?,
            _ => writeln!(f, "{}", self.0)?,
        }
        if let Some(backtrace) = &*CURRENT_BACKTRACE.lock() {
            writeln!(f, "{}", backtrace)?;
        }
        Ok(())
    }
}

impl From<String> for MyError {
    fn from(s: String) -> Self {
        let backtrace = Backtrace::capture();
        if backtrace.status() == BacktraceStatus::Captured {
            *CURRENT_BACKTRACE.lock() = Some(backtrace);
        }
        Self(s)
    }
}
impl From<&str> for MyError {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

pub fn unexpected_rule<T>(rule: Rule) -> MyResult<T> {
    Err(format!("unexpected rule {:?}", rule).into())
}

impl From<NoneError> for MyError {
    fn from(_: NoneError) -> Self {
        "option returned none".to_string().into()
    }
}

impl From<ParseFloatError> for MyError {
    fn from(e: ParseFloatError) -> Self {
        e.to_string().into()
    }
}
impl From<ParseIntError> for MyError {
    fn from(e: ParseIntError) -> Self {
        e.to_string().into()
    }
}
impl From<ParseBoolError> for MyError {
    fn from(e: ParseBoolError) -> Self {
        e.to_string().into()
    }
}
impl From<ParseCharError> for MyError {
    fn from(e: ParseCharError) -> Self {
        e.to_string().into()
    }
}
impl From<std::fmt::Error> for MyError {
    fn from(e: std::fmt::Error) -> Self {
        e.to_string().into()
    }
}

impl From<pest::error::Error<Rule>> for MyError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        e.to_string().into()
    }
}

impl From<std::io::Error> for MyError {
    fn from(e: std::io::Error) -> Self {
        e.to_string().into()
    }
}
