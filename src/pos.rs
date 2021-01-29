use crate::parse::{Pair, Rule};
use parking_lot::Mutex;
use pest::error::{Error, ErrorVariant};
use pest::Span;
use std::sync::Arc;

static CURRENT_POS: Mutex<Option<Pos>> = Mutex::new(None);

#[derive(Debug, Clone, PartialEq)]
pub struct Pos {
    input: Arc<String>,
    start: usize,
    end: usize,
}

impl Pos {
    pub fn unknown() -> Self {
        Self {
            input: "[unknown pos]".to_string().into(),
            start: 0,
            end: 0,
        }
    }

    pub fn reset() {
        *CURRENT_POS.lock() = None;
    }
    pub fn current() -> Option<Pos> {
        match &*CURRENT_POS.lock() {
            Some(pos) => Some(pos.clone()),
            None => None,
        }
    }
    pub fn set_current(&mut self) {
        // if self == &mut Default::default() {
        //     *self = Self::current().unwrap()
        // }
        *CURRENT_POS.lock() = Some(self.clone());
    }

    pub fn make_error(msg: impl AsRef<str>) -> String {
        let pos = match Pos::current() {
            Some(pos) => pos,
            None => return msg.as_ref().into(),
        };

        let span = match Span::new(&pos.input, pos.start, pos.end) {
            Some(span) => span,
            None => return msg.as_ref().into(),
        };

        let error = Error::new_from_span(
            ErrorVariant::<Rule>::CustomError {
                message: msg.as_ref().into(),
            },
            span,
        );
        error.to_string()
    }
}

pub trait AsPos {
    fn as_pos(&self) -> Pos;
}

impl AsPos for Pair<'_> {
    fn as_pos(&self) -> Pos {
        let (start_pos, end_pos) = self.as_span().split();

        let input = start_pos.line_of().to_string().into();
        let start = start_pos.line_col().1 - 1;
        let end = end_pos.line_col().1 - 1;
        Pos { input, start, end }
    }
}
