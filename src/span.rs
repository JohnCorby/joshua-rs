use crate::parse::Kind;
use crate::PROGRAM;
use pest::error::Error;
use pest::error::ErrorVariant::CustomError;
use std::fmt::{Debug, Display, Formatter};

#[derive(Copy, Clone)]
pub struct Span(usize, usize);

impl From<pest::Span<'_>> for Span {
    fn from(s: pest::Span) -> Self {
        let start = s.start();
        // let end = s.as_str().find('\n').map_or(s.end(), |i| start + i);
        let end = s.end();
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
    pub fn make_error(&self, message: impl AsRef<str>) -> String {
        Error::<Kind>::new_from_span(
            CustomError {
                message: message.as_ref().to_string(),
            },
            pest::Span::new(PROGRAM.get().unwrap(), self.0, self.1).unwrap(),
        )
        .to_string()
    }
}
