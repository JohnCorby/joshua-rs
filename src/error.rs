use crate::parse::Node;
use crate::span::Span;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::fmt::{Debug, Display, Formatter};

pub type Res<T = ()> = Result<T, Err>;
pub struct Err {
    pub message: String,
    /// should only be None when internal error
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
            writeln!(f, "Internal Backtrace:")?;
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

#[ext(name = IntoErr)]
pub impl<T: ToString> T {
    fn into_err(self, span: Option<Span>) -> Err {
        Err {
            message: self.to_string(),
            span,
            backtrace: Backtrace::capture(),
        }
    }
}

pub fn err<T>(str: &str, span: Span) -> Res<T> {
    Err(str.into_err(Some(span)))
}
#[allow(dead_code)]
pub fn warn(str: &str, span: Span) {
    eprintln!("Warning: {}", str.into_err(Some(span)));
}
#[allow(dead_code)]
pub fn warn_internal(str: &str) {
    eprintln!("Internal Warning: {}", str.into_err(None));
}

pub fn unexpected_kind(node: Node) -> ! {
    panic!("unexpected node kind {:?} (node: {})", node.kind(), node)
}
