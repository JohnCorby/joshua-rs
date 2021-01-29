use crate::error::{MyError, MyResult};
use parking_lot::Mutex;
use std::ops::Try;

static SCOPES: Mutex<Vec<Scope>> = Mutex::new(Vec::new());

#[derive(Debug, Clone, Default)]
pub struct Scope {
    items: Vec<ScopeItem>,
}

#[derive(Debug, Clone)]
pub enum ScopeItem {
    Func,
    Var,
    Struct,
}

impl Scope {
    pub fn push() {
        SCOPES.lock().push(Scope::default());
    }

    pub fn pop() -> MyResult<()> {
        SCOPES
            .lock()
            .pop()
            .into_result()
            .map(|_| ())
            .map_err(|_| "tried to pop an empty scope stack".into())
    }

    pub fn add_item(item: ScopeItem) -> MyResult<()> {
        let mut scopes = SCOPES.lock();
        let scope = scopes
            .last_mut()
            .into_result()
            .map_err(|_| MyError::from("tried to get current scope from empty scope stack"))?;
        scope.items.push(item);
        Ok(())
    }
}
