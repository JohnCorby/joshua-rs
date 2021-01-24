use crate::parse::{Pair, Rule};
use parking_lot::Mutex;
use pest::Span;
use std::backtrace::{Backtrace, BacktraceStatus};
use std::char::ParseCharError;
use std::fmt::{Debug, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::option::NoneError;
use std::str::ParseBoolError;

static CURRENT_BACKTRACE: Mutex<Option<Backtrace>> = Mutex::new(None);
static CURRENT_POS: Mutex<Option<Pos>> = Mutex::new(None);

pub struct Pos {
    input: String,
    start: usize,
    end: usize,
}
impl Pos {
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

pub type MyResult<T> = Result<T, MyError>;
pub struct MyError(String);

impl Debug for MyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0)?;
        if let Some(pos) = &*CURRENT_POS.lock() {
            write!(f, "{}", pos.to_string(&self.0))?;
        }
        if let Some(backtrace) = &*CURRENT_BACKTRACE.lock() {
            write!(f, "{}", backtrace)?;
        }
        Ok(())
    }
}

impl From<String> for MyError {
    fn from(s: String) -> Self {
        let backtrace = Backtrace::capture();
        if backtrace.status() == BacktraceStatus::Captured {
            *CURRENT_BACKTRACE.lock() = Some(backtrace);
        }
        Self(s)
    }
}

pub fn unexpected_rule<T>(rule: Rule) -> MyResult<T> {
    Err(format!("unexpected rule {:?}", rule).into())
}

impl From<NoneError> for MyError {
    fn from(_: NoneError) -> Self {
        "option returned none".to_string().into()
    }
}

impl From<ParseFloatError> for MyError {
    fn from(e: ParseFloatError) -> Self {
        e.to_string().into()
    }
}
impl From<ParseIntError> for MyError {
    fn from(e: ParseIntError) -> Self {
        e.to_string().into()
    }
}
impl From<ParseBoolError> for MyError {
    fn from(e: ParseBoolError) -> Self {
        e.to_string().into()
    }
}
impl From<ParseCharError> for MyError {
    fn from(e: ParseCharError) -> Self {
        e.to_string().into()
    }
}

impl From<pest::error::Error<Rule>> for MyError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        e.to_string().into()
    }
}

impl From<std::io::Error> for MyError {
    fn from(e: std::io::Error) -> Self {
        e.to_string().into()
    }
}
