use crate::parse::Kind;
use pest::error::Error;
use pest::error::ErrorVariant::CustomError;
use std::fmt::{Debug, Display, Formatter};
use std::mem::transmute;

#[derive(Copy, Clone)]
pub struct Span<'i> {
    i: &'i str,
    start: usize,
    end: usize,
}
impl From<pest::Span<'_>> for Span<'_> {
    fn from(span: pest::Span<'_>) -> Self {
        let str = span.as_str();
        let mut span: Span<'_> = unsafe { transmute(span) };
        span.end = str.find('\n').map_or(span.end, |i| span.start + i);
        span
    }
}
impl From<Span<'_>> for pest::Span<'_> {
    fn from(span: Span<'_>) -> Self {
        unsafe { transmute(span) }
    }
}

impl Display for Span<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.i[self.start..self.end], f)
    }
}
impl Debug for Span<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("str", &&self.i[self.start..self.end])
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

impl Span<'_> {
    pub fn make_error(self, message: impl AsRef<str>) -> String {
        Error::<Kind>::new_from_span(
            CustomError {
                message: message.as_ref().to_string(),
            },
            self.into(),
        )
        .to_string()
    }
}
