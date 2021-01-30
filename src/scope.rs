use crate::error::MyResult;
use crate::ty::Type;
use parking_lot::Mutex;
use std::ops::Try;

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());

#[derive(Debug, Clone, Default)]
pub struct Scope {
    items: Vec<ScopeItem>,
}

#[derive(Debug, Clone)]
pub enum ScopeItem {
    Func {
        ty: Type,
        name: String,
        arg_types: Vec<Type>,
    },
    Var {
        ty: Type,
        name: String,
    },
    Struct {
        name: String,
    },
}

impl Scope {
    pub fn push() {
        SCOPES.lock().push(Scope::default());
    }

    pub fn pop() {
        SCOPES
            .lock()
            .pop()
            .expect("tried to pop an empty scope stack");
    }

    pub fn add_item(item: ScopeItem) {
        SCOPES
            .lock()
            .last_mut()
            .expect("tried to get current scope from empty scope stack")
            .items
            .push(item);
    }

    fn find(mut predicate: impl FnMut(&ScopeItem) -> bool) -> Option<ScopeItem> {
        for scope in SCOPES.lock().iter().rev() {
            if let Some(item) = scope.items.iter().find(|item| predicate(item)) {
                return Some(item.clone());
            }
        }
        None
    }

    pub fn get_var(name: impl AsRef<str>) -> MyResult<ScopeItem> {
        Self::find(|item| {
            if let ScopeItem::Var { name: n, .. } = item {
                return n == name.as_ref();
            }
            false
        })
        .into_result()
        .map_err(|_| format!("unable to find var `{}`", name.as_ref()).into())
    }
    #[allow(dead_code)]
    pub fn get_func(name: impl AsRef<str>, arg_types: impl AsRef<[Type]>) -> MyResult<ScopeItem> {
        Self::find(|item| {
            if let ScopeItem::Func {
                name: n,
                arg_types: at,
                ..
            } = item
            {
                return n == name.as_ref() && at == arg_types.as_ref();
            }
            false
        })
        .into_result()
        .map_err(|_| {
            format!(
                "unable to find func `{}` with matching arg types",
                name.as_ref()
            )
            .into()
        })
    }
}
