use crate::error::{unexpected_rule, MyResult};
use crate::gen::Gen;
use crate::parse::{Pair, Rule};
use crate::pos::{AsPos, HasPos, Pos};
use crate::scope::Scope;
use crate::util::PairExt;
use crate::visit::Visit;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum Type {
    Primitive {
        pos: Pos,
        ty: PrimitiveType,
    },
    #[allow(dead_code)]
    Literal {
        pos: Pos,
        ty: LiteralType,
    },
    Named {
        pos: Pos,
        name: String,
    },
}
impl Default for Type {
    fn default() -> Self {
        Self::Named {
            pos: Default::default(),
            name: Default::default(),
        }
    }
}
impl Type {
    pub fn check(&self, expected: &Type) -> MyResult<()> {
        let actual = self;
        if expected == actual {
            Ok(())
        } else {
            Err(format!(
                "expected {}, but got {}",
                expected.to_string(),
                actual.to_string()
            )
            .into())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, strum::EnumString, strum::ToString)]
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
#[derive(Debug, Copy, Clone, PartialEq, strum::ToString)]
#[allow(dead_code)]
pub enum LiteralType {
    Float,
    Int,
    Str,
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Primitive { ty: ty1, .. }, Primitive { ty: ty2, .. }) => ty1 == ty2,
            (Literal { ty: ty1, .. }, Literal { ty: ty2, .. }) => ty1 == ty2,
            (Named { name: name1, .. }, Named { name: name2, .. }) => name1 == name2,

            // literal/primitive resolution
            (Literal { ty: lt, .. }, Primitive { ty: pt, .. })
            | (Primitive { ty: pt, .. }, Literal { ty: lt, .. }) => {
                use LiteralType::*;
                use PrimitiveType::*;
                match lt {
                    Float => matches!(pt, F32 | F64),
                    Int => matches!(pt, I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64),
                    Str => todo!("usage of string literals is not yet supported"),
                }
            }

            (_, _) => false,
        }
    }
}
impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Primitive { ty, .. } => format!("primitive type {}", ty.to_string()),
            Type::Literal { ty, .. } => format!("literal type {}", ty.to_string()),
            Type::Named { name, .. } => format!("named type {}", name),
        }
    }
}

impl Visit for Type {
    fn visit_impl(pair: Pair) -> Self {
        let pair = pair.into_inner_checked(Rule::ty).next().unwrap();
        let pos = pair.as_pos();
        match pair.as_rule() {
            Rule::primitive => Self::Primitive {
                pos,
                ty: PrimitiveType::from_str(pair.as_str()).unwrap(),
            },
            Rule::ident => Self::Named {
                pos,
                name: pair.as_str().into(),
            },

            rule => unexpected_rule(rule),
        }
    }
}

impl HasPos for Type {
    fn pos(&self) -> Pos {
        match self {
            Type::Primitive { pos, .. } => *pos,
            Type::Literal { pos, .. } => *pos,
            Type::Named { pos, .. } => *pos,
        }
    }
}
impl Gen for Type {
    fn gen_impl(self) -> MyResult<String> {
        Ok(match self {
            Type::Primitive { ty, .. } => {
                use PrimitiveType::*;
                match ty {
                    I8 => "signed char",
                    U8 => "unsigned char",
                    I16 => "signed short",
                    U16 => "unsigned short",
                    I32 => "signed int",
                    U32 => "unsigned int",
                    I64 => "signed long long",
                    U64 => "unsigned long long",
                    F32 => "float",
                    F64 => "double",
                    Bool => "unsigned char",
                    Char => "unsigned char",
                    Void => "void",
                }
                .into()
            }
            Type::Named { name, .. } => {
                Scope::current().get_type(&name)?;
                name
            }
            ty => panic!("tried to gen {}", ty.to_string()),
        })
    }
}

pub trait HasType {
    fn ty(&self) -> Type;
}
impl HasType for PrimitiveType {
    fn ty(&self) -> Type {
        Type::Primitive {
            pos: Default::default(),
            ty: *self,
        }
    }
}
impl HasType for LiteralType {
    fn ty(&self) -> Type {
        Type::Literal {
            pos: Default::default(),
            ty: *self,
        }
    }
}
