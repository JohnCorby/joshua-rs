use crate::pass::ty::Type;
use crate::util::ctx_str::CtxStr;
use std::fmt::Write;
use std::rc::Rc;

pub mod ctx_str;
pub mod frozen_vec;
pub mod late_init;

pub trait StrExt {
    /// gets verbose display name from stuff
    fn to_display(
        &self,
        what: &str,
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String;

    /// make a proper name out of stuff
    fn encode(
        &self,
        nesting_prefix: &[CtxStr<'_>],
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String;

    fn mangle(
        &self,
        nesting_prefix: &[CtxStr<'_>],
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String;
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
        nesting_prefix: &[CtxStr<'_>],
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
        let name = format!(
            "{}{}",
            nesting_prefix
                .iter()
                .map(|it| format!("{}::", it))
                .collect::<String>(),
            self
        );
        format!("{}{}{}", name, generic_replacements, arg_types)
    }

    fn mangle(
        &self,
        nesting_prefix: &[CtxStr<'_>],
        generic_replacements: &[&Type<'_>],
        arg_types: Option<&[&Type<'_>]>,
    ) -> String {
        // don't mangle func main (entry point)
        if arg_types.is_some() && arg_types.unwrap().is_empty() && self == "main" {
            self.into()
        } else {
            let encoded = self.encode(nesting_prefix, generic_replacements, arg_types);
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

pub trait RcExt<T> {
    /// shorthand for `Rc::try_unwrap(self).unwrap()`
    fn unwrap(self) -> T;

    // fn map<U>(&mut self, f: impl FnMut(&mut T))
    // where
    //     T: Clone;
}
impl<T> RcExt<T> for Rc<T> {
    fn unwrap(self) -> T {
        Rc::try_unwrap(self)
            .unwrap_or_else(|_| panic!("tried to unwrap Rc when there are still multiple refs"))
    }

    // fn map<U>(&mut self, f: impl FnMut(&mut T))
    // where
    //     T: Clone,
    // {
    //     let mut t = (**self).clone();
    //     f(&mut t);
    //     *self = Rc::new(t)
    // }
}
