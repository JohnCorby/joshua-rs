use crate::parse::Node;
use crate::span::Span;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{Debug, Display, Formatter};

pub type Res<'i, T = ()> = Result<T, Err<'i>>;
pub struct Err<'i> {
    message: String,
    /// should only be None when internal error
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
            writeln!(f, "Internal Backtrace:")?;
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
    fn into_err(self, span: Option<Span<'_>>) -> Err<'_>;
}
impl<T: ToString> IntoErr for T {
    fn into_err(self, span: Option<Span<'_>>) -> Err<'_> {
        Err {
            message: self.to_string(),
            span,
            backtrace: Backtrace::capture(),
        }
    }
}

pub fn err<T>(str: &str, span: Span<'i>) -> Res<'i, T> {
    Err(str.into_err(Some(span)))
}
#[allow(dead_code)]
pub fn warn(str: &str, span: Span<'_>) {
    eprintln!("Warning: {}", str.into_err(Some(span)));
}
#[allow(dead_code)]
pub fn warn_internal(str: &str) {
    eprintln!("Internal Warning: {}", str.into_err(None));
}

pub fn unexpected_kind(node: Node<'_>) -> ! {
    panic!("unexpected node kind {:?} (node: {})", node.kind(), node)
}
