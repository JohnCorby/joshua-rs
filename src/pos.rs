use crate::parse::{Pair, Rule};
use parking_lot::{Mutex, MutexGuard};
use pest::Span;

static CURRENT_POS: Mutex<Option<Pos>> = Mutex::new(None);

pub struct Pos {
    input: String,
    start: usize,
    end: usize,
}

impl Pos {
    pub fn reset() {
        *CURRENT_POS.lock() = None;
    }

    pub fn current() -> MutexGuard<'static, Option<Pos>> {
        CURRENT_POS.lock()
    }

    pub fn update(pair: &Pair) {
        let (start_pos, end_pos) = pair.as_span().split();

        let input = start_pos.line_of().to_string();
        let start = start_pos.line_col().1 - 1;
        let end = end_pos.line_col().1 - 1;
        *CURRENT_POS.lock() = Some(Pos { input, start, end });
    }

    pub fn to_string(&self, err: impl AsRef<str>) -> String {
        let span = match Span::new(&self.input, self.start, self.end) {
            Some(span) => span,
            None => {
                return format!(
                    "error making span from {:?}[{}..{}]",
                    self.input, self.start, self.end
                )
            }
        };

        let error = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::<Rule>::CustomError {
                message: err.as_ref().into(),
            },
            span,
        );
        error.to_string()
    }
}
