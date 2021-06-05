use crate::pass::ty::Type;
use std::fmt::Write;

pub mod frozen_vec;
pub mod interned_str;
pub mod late_init;

/// make a proper name out of stuff
pub fn code_name(
    name: &str,
    generic_replacements: &[&Type<'_>],
    arg_types: Option<&[&Type<'_>]>,
) -> String {
    let generic_replacements = if !generic_replacements.is_empty() {
        format!(
            "<{}>",
            generic_replacements
                .iter()
                .map(|it| it.code_name())
                .collect::<Vec<_>>()
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
                .map(|it| it.code_name())
                .collect::<Vec<_>>()
                .join(", ")
        )
    } else {
        String::new()
    };
    format!("{}{}{}", name, generic_replacements, arg_types)
}

/// gets verbose display name from stuff
pub fn to_string(
    what: &str,
    name: &str,
    generic_replacements: &[&Type<'_>],
    arg_types: Option<&[&Type<'_>]>,
) -> String {
    let generic_replacements = if !generic_replacements.is_empty() {
        format!(
            "generic replacements ({})",
            generic_replacements
                .iter()
                .map(|it| it.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    } else {
        String::new()
    };
    let arg_types = if let Some(arg_types) = arg_types {
        format!(
            "arg types ({})",
            arg_types
                .iter()
                .map(|it| it.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    } else {
        String::new()
    };

    let mut ret = format!("{} `{}`", what, name);
    match (generic_replacements.is_empty(), arg_types.is_empty()) {
        (false, false) => {}
        (true, false) => write!(ret, " with {}", generic_replacements).unwrap(),
        (false, true) => write!(ret, " with {}", arg_types).unwrap(),
        (true, true) => write!(ret, " with {} and {}", generic_replacements, arg_types).unwrap(),
    };
    ret
}

pub trait Mangle {
    fn mangle(&self) -> String;
    fn mangle_func(&self, arg_types: &[&Type<'_>], generic_replacements: &[&Type<'_>]) -> String;
}
impl Mangle for str {
    fn mangle(&self) -> String {
        format!("{}/*{}*/", mangling::mangle(self.as_bytes()), self)
    }
    fn mangle_func(&self, arg_types: &[&Type<'_>], generic_replacements: &[&Type<'_>]) -> String {
        // don't mangle func main (entry point)
        if self == "main" {
            self.to_string()
        } else {
            code_name(self, generic_replacements, Some(arg_types)).mangle()
        }
    }
}
