use crate::parse::Kind;
use pest::error::Error;
use pest::error::ErrorVariant::CustomError;
use std::fmt::{Debug, Display, Formatter};
use std::mem::transmute;

#[derive(Copy, Clone, Default)]
pub struct Span {
    i: &'static str,
    start: usize,
    end: usize,
}
impl From<pest::Span<'static>> for Span {
    fn from(span: pest::Span<'static>) -> Self {
        let str = span.as_str();
        let mut span: Span = unsafe { transmute(span) };
        span.end = str.find('\n').map_or(span.end, |i| span.start + i);
        span
    }
}
impl From<Span> for pest::Span<'static> {
    fn from(span: Span) -> Self {
        unsafe { transmute(span) }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&self.i[self.start..self.end], f)
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("str", &&self.i[self.start..self.end])
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

impl Span {
    pub fn make_error(self, message: &str) -> String {
        Error::<Kind>::new_from_span(
            CustomError {
                message: message.to_string(),
            },
            self.into(),
        )
        .to_string()
    }
}
