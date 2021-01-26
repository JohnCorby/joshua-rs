use crate::error::{MyError, MyResult};
use crate::parse::{parse, Pair, Rule};
use crate::util::PairExt;
use crate::visit::Visit;
use parking_lot::Mutex;
use std::str::FromStr;

static TYPES: Mutex<Vec<Type>> = Mutex::new(Vec::new());

#[derive(Debug, Clone)]
pub struct Type {
    name: String,
}
impl Type {
    pub fn init() -> MyResult<()> {
        TYPES.lock().clear();

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

        Ok(())
    }

    pub fn add(ty: impl AsRef<str>) -> MyResult<()> {
        let ty = ty.as_ref().parse()?;
        TYPES.lock().push(ty);
        Ok(())
    }
}

impl FromStr for Type {
    type Err = MyError;
    fn from_str(s: &str) -> MyResult<Self> {
        parse(Rule::ty, s)?.visit()
    }
}

impl Visit for Type {
    fn visit(pair: Pair) -> MyResult<Self> {
        let mut pairs = pair.into_inner_checked(Rule::ty)?;

        Ok(Self {
            name: pairs.next()?.as_str().into(),
        })

        // // todo checking stage
        // match TYPES.lock().iter().find(|existing| existing.name == name) {
        //     Some(ty) => Ok(ty.clone()),
        //     None => Err(format!("cannot resolve type {:?}", name).into()),
        // }
    }
}
