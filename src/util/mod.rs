use crate::pass::ty::Type;

pub mod frozen_vec;
pub mod interned_str;
pub mod late_init;

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
            if generic_replacements.is_empty() {
                format!(
                    "{}({})",
                    self,
                    arg_types
                        .iter()
                        .map(|ty| ty.func_name())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            } else {
                format!(
                    "{}<{}>({})",
                    self,
                    generic_replacements
                        .iter()
                        .map(|it| it.func_name())
                        .collect::<Vec<_>>()
                        .join(", "),
                    arg_types
                        .iter()
                        .map(|it| it.func_name())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            .mangle()
        }
    }
}
