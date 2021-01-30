use crate::parse::{Pair, Rule};
use crate::PROGRAM;
use parking_lot::Mutex;
use pest::error::Error;
use pest::error::ErrorVariant::CustomError;
use pest::Span;

static CURRENT_POS: Mutex<Option<Pos>> = Mutex::new(None);

#[derive(Debug, Copy, Clone)]
pub struct Pos {
    start: usize,
    end: usize,
}

impl Pos {
    pub fn current() -> Option<Pos> {
        match &*CURRENT_POS.lock() {
            Some(pos) => Some(*pos),
            None => None,
        }
    }
    pub fn set_current(&self) {
        *CURRENT_POS.lock() = Some(*self);
    }

    fn as_span(&self) -> Span {
        Span::new(PROGRAM.get().unwrap(), self.start, self.end).unwrap()
    }
    pub fn make_error(message: impl AsRef<str>) -> String {
        let message = message.as_ref().to_string();
        let pos = match Pos::current() {
            Some(pos) => pos,
            None => return message,
        };
        Error::<Rule>::new_from_span(CustomError { message }, pos.as_span()).to_string()
    }
}

pub trait AsPos {
    fn as_pos(&self) -> Pos;
}
impl AsPos for Pair<'_> {
    fn as_pos(&self) -> Pos {
        let span = self.as_span();
        let start = span.start();
        let end = span.as_str().rfind('\n').map_or(span.end(), |i| start + i);
        Pos { start, end }
    }
}

pub trait HasPos {
    fn pos(&self) -> Pos;
}
