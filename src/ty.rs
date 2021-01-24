use crate::error::MyResult;
use crate::parse::Pair;
use crate::visit::Visit;
use parking_lot::Mutex;

static TYPES: Mutex<Vec<Type>> = Mutex::new(Vec::new());

#[derive(Debug, Clone)]
pub struct Type(String);

impl Type {
    pub fn init() -> MyResult<()> {
        TYPES.lock().clear();

        Self::add("byte");
        Self::add("ubyte");
        Self::add("short");
        Self::add("ushort");
        Self::add("int");
        Self::add("uint");
        Self::add("long");
        Self::add("ulong");

        Self::add("float");
        Self::add("double");

        Self::add("bool");
        Self::add("char");
        Self::add("string");
        Self::add("void");

        Self::add("addr");

        Ok(())
    }

    pub fn add(ty: impl AsRef<str>) {
        let ty = Self(ty.as_ref().into());

        TYPES.lock().push(ty);
    }

    fn resolve(ty: impl AsRef<str>) -> MyResult<Self> {
        match TYPES
            .lock()
            .iter()
            .find(|existing| existing.0 == ty.as_ref())
        {
            Some(ty) => Ok(ty.clone()),
            None => Err(format!("cannot resolve type {}", ty.as_ref()).into()),
        }
    }
}

impl Visit for Type {
    fn visit(pair: Pair) -> MyResult<Self> {
        Self::resolve(pair.as_str())
    }
}
