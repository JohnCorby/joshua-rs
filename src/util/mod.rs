use crate::error::Res;
use crate::pass::ast2::Type;
use std::ops::Deref;
use std::rc::Rc;

pub mod frozen_index_set;

pub trait StrExt {
    /// make a proper name by attaching formatted stuff
    fn encode(&self, generic_replacements: &[&Type<'_>], arg_types: Option<&[&Type<'_>]>)
        -> String;

    /// mangle string for c usage
    fn mangle(&self) -> String;
}
impl StrExt for str {
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
                    .map(|it| it.to_string())
                    .vec()
                    .join(", ")
            )
        } else {
            String::new()
        };
        let arg_types = if let Some(arg_types) = arg_types {
            format!(
                "({})",
                arg_types.iter().map(|it| it.to_string()).vec().join(", ")
            )
        } else {
            String::new()
        };
        format!("{}{}{}", self, generic_replacements, arg_types)
    }

    fn mangle(&self) -> String {
        format!("{}/*{}*/", mangling::mangle(self.as_bytes()), self)
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
