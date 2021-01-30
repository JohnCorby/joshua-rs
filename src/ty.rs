use crate::error::{MyError, MyResult};
use crate::gen::Gen;
use crate::parse::{parse, Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::util::PairExt;
use crate::visit::Visit;
use parking_lot::Mutex;
use std::str::FromStr;

static TYPES: Mutex<Vec<Type>> = Mutex::new(Vec::new());

#[derive(Debug, Clone)]
pub struct Type {
    pos: Pos,
    name: String,
}
impl Type {
    pub fn init() -> MyResult<()> {
        TYPES.lock().clear();

        let pos = Pos::current()?;
        Self::add("byte")?;
        Self::add("ubyte")?;
        Self::add("short")?;
        Self::add("ushort")?;
        Self::add("int")?;
        Self::add("uint")?;
        Self::add("long")?;
        Self::add("ulong")?;

        Self::add("float")?;
        Self::add("double")?;

        Self::add("bool")?;
        Self::add("char")?;
        Self::add("string")?;
        Self::add("void")?;

        Self::add("addr")?;
        pos.set_current();

        Ok(())
    }

    pub fn add(ty: impl AsRef<str>) -> MyResult<()> {
        let ty = ty.as_ref().parse()?;
        TYPES.lock().push(ty);
        Ok(())
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl FromStr for Type {
    type Err = MyError;
    fn from_str(s: &str) -> MyResult<Self> {
        parse(Rule::ty, s)?.visit()
    }
}

impl Visit for Type {
    fn visit_impl(pair: Pair) -> MyResult<Self> {
        let pos = pair.as_pos();
        let mut pairs = pair.into_inner_checked(Rule::ty)?;

        Ok(Self {
            pos,
            name: pairs.next()?.as_str().into(),
        })
    }
}

impl HasPos for Type {
    fn pos(&self) -> Pos {
        self.pos
    }
}
impl Gen for Type {
    fn gen_impl(self) -> MyResult<String> {
        if !TYPES.lock().contains(&self) {
            return Err(format!("cannot resolve type {:?}", self).into());
        }

        Ok(self.name)
    }
}
