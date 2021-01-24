use crate::error::{MyError, MyResult};
use crate::parse::{Pair, Rule};
use crate::util::PairExt;
use crate::visit::Visit;
use parking_lot::Mutex;
use std::str::FromStr;

static TYPES: Mutex<Vec<Type>> = Mutex::new(Vec::new());

#[derive(Debug, Clone)]
pub struct Type(String);

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
        Ok(Self(s.into()))
    }
}

impl Visit for Type {
    fn visit(pair: Pair) -> MyResult<Self> {
        // parsing stage
        let mut pairs = pair.into_inner_checked(Rule::ty)?;
        let ty = pairs.next()?.as_str().parse::<Type>()?;

        // checking stage
        match TYPES.lock().iter().find(|existing| existing.0 == ty.0) {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!("cannot resolve type {:?}", ty).into()),
        }
    }
}
