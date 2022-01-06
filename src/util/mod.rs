use crate::error::Res;
use crate::pass::ast2::Type;
use std::ops::Deref;
use std::rc::Rc;

#[ext(name = StrExt)]
pub impl str {
    /// make a proper name by attaching formatted stuff
    ///
    /// used for codegen and display
    fn encode(
        &self,
        nesting_prefix: &str,
        receiver_ty: Option<&Type>,
        generic_replacements: &[Type],
        arg_types: Option<&[Type]>,
        include_nesting_prefixes: bool,
    ) -> String {
        let nesting_prefix = if include_nesting_prefixes {
            nesting_prefix
        } else {
            ""
        };
        let receiver_ty = receiver_ty.map_or(String::new(), |it| format!("{}::", it.encode(false)));
        let generic_replacements = if !generic_replacements.is_empty() {
            format!(
                "<{}>",
                generic_replacements
                    .iter()
                    .map(|it| it.encode(include_nesting_prefixes))
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
                    .map(|it| it.encode(include_nesting_prefixes))
                    .vec()
                    .join(", ")
            )
        } else {
            String::new()
        };
        format!(
            "{}{}{}{}{}",
            nesting_prefix, receiver_ty, self, generic_replacements, arg_types
        )
    }

    fn mangle(&self) -> String {
        format!("{}/*{}*/", mangling::mangle(self.as_bytes()), self)
    }
}

#[ext(name = IterExt, supertraits = Iterator)]
pub impl<T: Iterator> T {
    /// shorthand for `self.collect::<Vec<_>>()`
    fn vec(self) -> Vec<Self::Item> {
        self.collect()
    }
}

#[ext(name = IterResExt)]
pub impl<T, I: Iterator<Item = Res<T>>> I {
    /// shorthand for `self.collect::<Res<Vec<_>>>()`
    fn res_vec(self) -> Res<Vec<T>> {
        self.collect()
    }
}

#[ext(name = RcExt)]
pub impl<T> Rc<T> {
    /// get clone of inner value
    fn cloned(&self) -> T
    where
        T: Clone,
    {
        self.deref().clone()
    }

    /// shorthand for `Rc::try_unwrap(self).unwrap()`
    fn into_inner(self) -> T {
        Rc::try_unwrap(self)
            .unwrap_or_else(|_| panic!("tried to unwrap Rc when there are still multiple refs"))
    }

    /// clone `T`, modify it, and then put a new `Rc` in `self`
    fn modify(&mut self, f: impl FnOnce(&mut T))
    where
        T: Clone,
    {
        let mut t = self.cloned();
        f(&mut t);
        *self = t.into();
    }
}
