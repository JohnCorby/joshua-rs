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

    pub fn make_error(message: impl AsRef<str>) -> String {
        let message = message.as_ref().to_string();

        let pos = match Pos::current() {
            Some(pos) => pos,
            None => return message,
        };

        let input = PROGRAM.get().unwrap();
        let span = Span::new(input, pos.start, pos.end).unwrap();
        let error = Error::<Rule>::new_from_span(CustomError { message }, span);
        error.to_string()
    }
}

pub trait AsPos {
    fn as_pos(&self) -> Pos;
}
impl AsPos for Pair<'_> {
    fn as_pos(&self) -> Pos {
        let span = self.as_span();
        Pos {
            start: span.start(),
            end: span.end(),
        }
    }
}

pub trait HasPos {
    fn pos(&self) -> Pos;
}
