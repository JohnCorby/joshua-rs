use crate::parse::Kind;
use crate::PROGRAM;
use parking_lot::{Mutex, MutexGuard};
use pest::error::Error;
use pest::error::ErrorVariant::CustomError;
use std::fmt::{Debug, Display, Formatter};

static CURRENT_SPAN: Mutex<Option<Span>> = Mutex::new(None);
fn current_span() -> MutexGuard<'static, Option<Span>> {
    CURRENT_SPAN.try_lock().expect("CURRENT_SPAN locked")
}

#[derive(Copy, Clone)]
pub struct Span(usize, usize);

impl From<pest::Span<'_>> for Span {
    fn from(s: pest::Span) -> Self {
        let start = s.start();
        let end = s.as_str().rfind('\n').map_or(s.end(), |i| start + i);
        Span(start, end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        Display::fmt(&PROGRAM.get().unwrap()[self.0..self.1], f)
    }
}
impl Debug for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("str", &self.to_string())
            .field("start", &self.0)
            .field("end", &self.1)
            .finish()
    }
}

impl Span {
    pub fn reset() {
        *current_span() = None
    }

    pub fn current() -> Option<Span> {
        match *current_span() {
            Some(span) => Some(span),
            None => None,
        }
    }
    pub fn track(self) -> Self {
        *current_span() = Some(self);
        self
    }

    pub fn make_error(message: impl AsRef<str>) -> String {
        let message = message.as_ref().to_string();
        let span = match Span::current() {
            Some(span) => span,
            None => return message,
        };
        let span = pest::Span::new(PROGRAM.get().unwrap(), span.0, span.1).unwrap();
        Error::<Kind>::new_from_span(CustomError { message }, span).to_string()
    }
}
