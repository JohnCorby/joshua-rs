use crate::error::Res;
use crate::pass::ast2::Type;
use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

pub mod ctx_str;
pub mod frozen_vec;

pub trait StrExt {
    /// gets verbose display name from stuff
    fn to_display(
        &self,
        what: &str,
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String;

    /// make a proper name out of stuff
    fn encode(&self, generic_replacements: &[&Type<'_>], arg_types: Option<&[&Type<'_>]>)
        -> String;

    fn mangle(&self, generic_replacements: &[&Type<'_>], arg_types: Option<&[&Type<'_>]>)
        -> String;
}
impl StrExt for str {
    fn to_display(
        &self,
        what: &str,
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String {
        let generic_replacements = if !generic_replacements.is_empty() {
            format!(
                "generic replacements ({})",
                generic_replacements
                    .iter()
                    .map(|it| it.to_string())
                    .vec()
                    .join(", ")
            )
        } else {
            String::new()
        };
        let arg_types = if let Some(arg_types) = arg_types {
            format!(
                "arg types ({})",
                arg_types.iter().map(|it| it.to_string()).vec().join(", ")
            )
        } else {
            String::new()
        };
        let mut ret = format!("{} `{}`", what, self);
        match (!generic_replacements.is_empty(), !arg_types.is_empty()) {
            (false, false) => {}
            (true, false) => write!(ret, " with {}", generic_replacements).unwrap(),
            (false, true) => write!(ret, " with {}", arg_types).unwrap(),
            (true, true) => {
                write!(ret, " with {} and {}", generic_replacements, arg_types).unwrap()
            }
        };
        ret
    }

    fn encode(
        &self,
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String {
        let generic_replacements = if !generic_replacements.is_empty() {
            format!(
                "<{}>",
                generic_replacements
                    .iter()
                    .map(|it| it.encoded_name())
                    .vec()
                    .join(", ")
            )
        } else {
            String::new()
        };
        let arg_types = if let Some(arg_types) = arg_types {
            format!(
                "({})",
                arg_types
                    .iter()
                    .map(|it| it.encoded_name())
                    .vec()
                    .join(", ")
            )
        } else {
            String::new()
        };
        format!("{}{}{}", self, generic_replacements, arg_types)
    }

    fn mangle(
        &self,
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String {
        // don't mangle func main (entry point)
        if matches!(arg_types, Some([])) && self == "main" {
            self.into()
        } else {
            let encoded = self.encode(generic_replacements, arg_types);
            format!("{}/*{}*/", mangling::mangle(encoded.as_bytes()), encoded)
        }
    }
}

pub trait IterExt: Iterator + Sized {
    /// shorthand for `self.collect::<Vec<_>>()`
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}
impl<T: Iterator> IterExt for T {}

pub trait IterResExt<'i, T>: Iterator<Item = Res<'i, T>> + Sized {
    /// shorthand for `self.collect::<Res<'i, Vec<_>>>()`
    fn res_vec(self) -> Res<'i, Vec<T>> {
        self.collect()
    }
}
impl<T, I: Iterator<Item = Res<'i, T>>> IterResExt<'i, T> for I {}

pub trait RcExt<T> {
    /// shorthand for `Rc::try_unwrap(self).unwrap()`
    fn into_inner(self) -> T;

    /// clone `T`, modify it, and then put a new `Rc` in `self`
    fn modify(&mut self, f: impl FnOnce(&mut T))
    where
        T: Clone;
}
impl<T> RcExt<T> for Rc<T> {
    fn into_inner(self) -> T {
        Rc::try_unwrap(self)
            .unwrap_or_else(|_| panic!("tried to unwrap Rc when there are still multiple refs"))
    }

    fn modify(&mut self, f: impl FnOnce(&mut T))
    where
        T: Clone,
    {
        let mut t = self.deref().deref().clone();
        f(&mut t);
        *self = t.into();
    }
}
