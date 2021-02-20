use crate::parse::Node;
use crate::span::Span;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{Debug, Display, Formatter};
use std::panic::UnwindSafe;

pub type Res<'i, T> = Result<T, Err<'i>>;
pub struct Err<'i> {
    message: String,
    span: Option<Span<'i>>,
    backtrace: Backtrace,
}

impl Err<'_> {
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

    /// run in closure
    /// todo implement and use this
    #[allow(warnings)]
    pub fn run<F: FnOnce() -> R + UnwindSafe, R>(f: F) {
        std::panic::catch_unwind(Box::new(|| {}));
    }
}

impl Display for Err<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
impl Debug for Err<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub trait IntoErr {
    fn into_err<'i>(self, span: impl Into<Option<Span<'i>>>) -> Err<'i>;
}
impl<T: ToString> IntoErr for T {
    fn into_err<'i>(self, span: impl Into<Option<Span<'i>>>) -> Err<'i> {
        Err {
            message: self.to_string(),
            span: span.into(),
            backtrace: Backtrace::capture(),
        }
    }
}

pub fn err<'i, T>(str: impl AsRef<str>, span: impl Into<Option<Span<'i>>>) -> Res<'i, T> {
    Err(str.as_ref().into_err(span))
}
#[allow(dead_code)]
pub fn warn<'i>(str: impl AsRef<str>, span: impl Into<Option<Span<'i>>>) {
    eprintln!("Warning: {}", (str.as_ref().into_err(span)));
}
#[allow(dead_code)]
pub fn warn_internal<'i>(str: impl AsRef<str>, span: impl Into<Option<Span<'i>>>) {
    eprintln!("Internal Warning: {}", (str.as_ref().into_err(span)));
}

pub fn unexpected_kind(node: Node<'_>) -> ! {
    panic!("unexpected node kind {:?} (node: {})", node.kind(), node)
}
