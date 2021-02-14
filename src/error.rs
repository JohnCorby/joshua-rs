use crate::parse::Node;
use crate::span::Span;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{Debug, Display, Formatter};

pub type Res<T> = Result<T, Err>;
pub struct Err {
    message: String,
    span: Option<Span>,
    backtrace: Backtrace,
}

impl Err {
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
            eprintln!("Internal Error: {}", message.into_err(None));
        }))
    }
}

impl Display for Err {
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
impl Debug for Err {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub trait IntoErr {
    fn into_err(self, span: impl Into<Option<Span>>) -> Err;
}
impl<T: ToString> IntoErr for T {
    fn into_err(self, span: impl Into<Option<Span>>) -> Err {
        Err {
            message: self.to_string(),
            span: span.into(),
            backtrace: Backtrace::capture(),
        }
    }
}

pub fn err<T>(str: impl AsRef<str>, span: impl Into<Option<Span>>) -> Res<T> {
    Err(str.as_ref().into_err(span))
}
#[allow(dead_code)]
pub fn warn(str: impl AsRef<str>, span: impl Into<Option<Span>>) {
    eprintln!("Warning: {}", (str.as_ref().into_err(span)));
}
#[allow(dead_code)]
pub fn warn_internal(str: impl AsRef<str>, span: impl Into<Option<Span>>) {
    eprintln!("Internal Warning: {}", (str.as_ref().into_err(span)));
}

pub fn unexpected_kind(node: Node) -> ! {
    panic!("unexpected node kind {:?} (node: {})", node.kind(), node)
}
