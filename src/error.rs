use crate::parse::{Kind, Node};
use crate::span::Span;
use parking_lot::{Mutex, MutexGuard};
use std::backtrace::{Backtrace, BacktraceStatus};
use std::char::ParseCharError;
use std::fmt::{Debug, Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::option::NoneError;
use std::str::ParseBoolError;

static CURRENT_BACKTRACE: Mutex<Option<Backtrace>> = Mutex::new(None);
fn current_backtrace() -> MutexGuard<'static, Option<Backtrace>> {
    CURRENT_BACKTRACE
        .try_lock()
        .expect("CURRENT_BACKTRACE locked")
}

pub type MyResult<T> = Result<T, MyError>;
#[derive(Clone)]
pub struct MyError(String);

impl MyError {
    pub fn init() {
        std::panic::set_hook(Box::new(|info| {
            // get message
            let message = if let Some(message) = info.message() {
                message.to_string()
            } else if let Some(payload) = info.payload().downcast_ref::<&str>() {
                payload.to_string()
            } else {
                "cannot get panic message".to_string()
            };

            // make error
            let err = MyError::from(message);
            eprintln!("Internal Error: {:?}", err);
        }))
    }
}

impl Display for MyError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        writeln!(f, "{}", Span::make_error(&self.0)).unwrap();
        if let Some(backtrace) = &*current_backtrace() {
            writeln!(f, "{}", backtrace).unwrap();
        }
        Ok(())
    }
}
impl Debug for MyError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl From<String> for MyError {
    fn from(s: String) -> Self {
        let backtrace = Backtrace::capture();
        if backtrace.status() == BacktraceStatus::Captured {
            *current_backtrace() = Some(backtrace);
        }
        Self(s)
    }
}
impl From<&str> for MyError {
    fn from(s: &str) -> Self {
        s.to_string().into()
    }
}

pub fn err<T>(str: impl AsRef<str>) -> MyResult<T> {
    Err(str.as_ref().into())
}
#[allow(dead_code)]
pub fn warn(str: impl AsRef<str>) {
    let err = MyResult::<()>::Err(str.as_ref().into());
    eprintln!("Warning: {:?}", err);
}
pub fn warn_internal(str: impl AsRef<str>) {
    let err = MyResult::<()>::Err(str.as_ref().into());
    eprintln!("Internal Warning: {:?}", err);
}

pub fn unexpected_kind(node: Node) -> ! {
    panic!("unexpected node kind {:?} (node: {})", node.kind(), node)
}

impl From<NoneError> for MyError {
    fn from(_: NoneError) -> Self {
        "option returned none".into()
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

impl From<pest::error::Error<Kind>> for MyError {
    fn from(e: pest::error::Error<Kind>) -> Self {
        e.to_string().into()
    }
}
