use crate::error::{MyError, MyResult};
use parking_lot::Mutex;
use std::str::FromStr;

static TYPES: Mutex<Vec<Type>> = Mutex::new(Vec::new());

#[derive(Debug, Clone)]
pub struct Type(String);

impl Type {
    pub fn init() -> MyResult<()> {
        TYPES.try_lock()?.clear();

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
        let ty = Self(ty.as_ref().into());

        TYPES.try_lock()?.push(ty);
        Ok(())
    }

    fn resolve(ty: impl AsRef<str>) -> MyResult<Self> {
        match TYPES
            .try_lock()?
            .iter()
            .find(|existing| existing.0 == ty.as_ref())
        {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!("cannot resolve type {}", ty.as_ref()).into()),
        }
    }
}

impl FromStr for Type {
    type Err = MyError;
    fn from_str(s: &str) -> MyResult<Self> {
        Self::resolve(s)
    }
}
