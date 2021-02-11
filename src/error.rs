use crate::parse::Node;
use crate::span::Span;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{Debug, Display, Formatter};

pub type MyResult<T> = Result<T, MyError>;
// #[derive(Clone)]
pub struct MyError {
    message: String,
    span: Option<Span>,
    backtrace: Backtrace,
}

impl MyError {
    pub fn init() {
        std::panic::set_hook(Box::new(|info| {
            // get message
            let message = if let Some(message) = info.message() {
                message.to_string()
            } else if let Some(&payload) = info.payload().downcast_ref::<&str>() {
                payload.to_string()
            } else {
                "cannot get panic message".to_string()
            };

            // make error
            eprintln!("Internal Error: {}", message.into_err());
        }))
    }
}

impl Display for MyError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(span) = self.span {
            write!(f, "{}", span.make_error(&self.message))?;
        } else {
            write!(f, "{}", self.message)?;
        }
        if self.backtrace.status() == BacktraceStatus::Captured {
            writeln!(f)?;
            write!(f, "{}", self.backtrace)?;
        }
        Ok(())
    }
}
impl Debug for MyError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub trait IntoErr {
    fn into_err(self) -> MyError;
}
impl<T: ToString> IntoErr for T {
    fn into_err(self) -> MyError {
        MyError {
            message: self.to_string(),
            span: None,
            backtrace: Backtrace::capture(),
        }
    }
}

pub trait Context<T> {
    fn ctx(self, span: Span) -> MyResult<T>;
}
impl<T, E: Into<MyError>> Context<T> for Result<T, E> {
    fn ctx(self, span: Span) -> MyResult<T> {
        self.map_err(|e| {
            let mut e = e.into();
            if e.span.is_none() {
                e.span = Some(span);
            }
            e
        })
    }
}
impl<T> Context<T> for Option<T> {
    fn ctx(self, span: Span) -> MyResult<T> {
        self.ok_or_else(|| "option is None".into_err()).ctx(span)
    }
}

pub fn err<T>(str: impl AsRef<str>) -> MyResult<T> {
    Err(str.as_ref().into_err())
}
#[allow(dead_code)]
pub fn warn(str: impl AsRef<str>) {
    eprintln!("Warning: {}", (str.as_ref()));
}
pub fn warn_internal(str: impl AsRef<str>) {
    eprintln!("Internal Warning: {}", (str.as_ref()));
}

pub fn unexpected_kind(node: Node) -> ! {
    panic!("unexpected node kind {:?} (node: {})", node.kind(), node)
}
