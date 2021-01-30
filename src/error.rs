use crate::parse::Rule;
use crate::pos::Pos;
use parking_lot::Mutex;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::char::ParseCharError;
use std::fmt::{Debug, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::option::NoneError;
use std::str::ParseBoolError;

static CURRENT_BACKTRACE: Mutex<Option<Backtrace>> = Mutex::new(None);

pub type MyResult<T> = Result<T, MyError>;
pub struct MyError(String);

impl Debug for MyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", Pos::make_error(&self.0)).unwrap();
        if let Some(backtrace) = &*CURRENT_BACKTRACE.lock() {
            writeln!(f, "{}", backtrace).unwrap();
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

pub fn unexpected_rule(rule: Rule) -> ! {
    panic!("expected rule {:?}", rule)
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

impl From<pest::error::Error<Rule>> for MyError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        e.to_string().into()
    }
}
