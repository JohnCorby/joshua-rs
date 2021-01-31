use crate::error::{unexpected_rule, MyResult};
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::util::PairExt;
use crate::visit::Visit;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

// static TYPES: Mutex<Vec<Type>> = Mutex::new(Vec::new());

#[derive(Debug, Clone)]
pub enum Type {
    Named { pos: Pos, name: String },
    Primitive { pos: Pos, ty: PrimitiveType },
    Literal { pos: Pos, ty: LiteralType },
}
impl Default for Type {
    fn default() -> Self {
        Self::Named {
            pos: Default::default(),
            name: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, strum::EnumString, strum::ToString)]
#[strum(serialize_all = "snake_case")]
pub enum PrimitiveType {
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Bool,
    Char,
    Void,
}
#[derive(Debug, Clone, PartialEq, Hash, strum::EnumString, strum::ToString)]
#[strum(serialize_all = "snake_case")]
pub enum LiteralType {
    Float,
    Int,
    Bool,
    Char,
    Str,
}

// impl Type {
//     pub fn add(ty: impl AsRef<str>) -> MyResult<()> {
//         let ty = ty.as_ref().parse()?;
//         TYPES.lock().push(ty);
//         Ok(())
//     }
// }

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Named { name: name1, .. }, Named { name: name2, .. }) => name1 == name2,
            (Primitive { ty: ty1, .. }, Primitive { ty: ty2, .. }) => ty1 == ty2,
            (Literal { ty: ty1, .. }, Literal { ty: ty2, .. }) => ty1 == ty2,
            (_, _) => false,
        }
    }
}
impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Named { name, .. } => name.hash(state),
            Type::Primitive { ty, .. } => ty.hash(state),
            Type::Literal { ty, .. } => ty.hash(state),
        }
    }
}
impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Named { name, .. } => format!("normal type {}", name),
            Type::Primitive { ty, .. } => format!("primitive type {}", ty.to_string()),
            Type::Literal { ty, .. } => format!("literal type {}", ty.to_string()),
        }
    }
}

impl Visit for Type {
    fn visit_impl(pair: Pair) -> Self {
        let pair = pair.into_inner_checked(Rule::ty).next().unwrap();
        let pos = pair.as_pos();
        match pair.as_rule() {
            Rule::primitive => {
                let ty: PrimitiveType = PrimitiveType::from_str("hello").unwrap();
                Self::Primitive { pos, ty }
            }
            Rule::ident => {
                // todo check not already exist?
                Self::Named {
                    pos,
                    name: pair.as_str().into(),
                }
            }

            rule => unexpected_rule(rule),
        }
    }
}

impl HasPos for Type {
    fn pos(&self) -> Pos {
        match self {
            Type::Named { pos, .. } => *pos,
            Type::Primitive { pos, .. } => *pos,
            Type::Literal { pos, .. } => *pos,
        }
    }
}
impl Gen for Type {
    fn gen_impl(self) -> MyResult<String> {
        // if !TYPES.lock().contains(&self) {
        //     return Err(format!("cannot resolve type {:?}", self).into());
        // }

        todo!()
    }
}
