use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use crate::Value;

use super::{ThreadValueSnapshot, restore_thread_value, snapshot_value_for_thread};

#[derive(Clone, Debug)]
pub(crate) struct Binding {
    pub(crate) mutable: bool,
    storage: BindingStorage,
}

pub(crate) type BindingRef = Rc<RefCell<Binding>>;

#[derive(Clone, Debug)]
enum BindingStorage {
    Local(Value),
    Shared(Arc<Mutex<ThreadValueSnapshot>>),
}

impl Binding {
    pub(crate) fn with_value(mutable: bool, value: Value) -> Self {
        Self {
            mutable,
            storage: BindingStorage::Local(value),
        }
    }

    pub(crate) fn with_shared_snapshot(
        mutable: bool,
        value: Arc<Mutex<ThreadValueSnapshot>>,
    ) -> Self {
        Self {
            mutable,
            storage: BindingStorage::Shared(value),
        }
    }

    fn placeholder(mutable: bool) -> Self {
        Self::with_value(mutable, Value::Unit)
    }

    pub(crate) fn current_value(&self) -> Value {
        match &self.storage {
            BindingStorage::Local(value) => value.clone(),
            BindingStorage::Shared(cell) => {
                restore_thread_value(cell.lock().expect("shared binding lock").clone())
            }
        }
    }

    pub(crate) fn set_value(&mut self, value: Value) {
        match &mut self.storage {
            BindingStorage::Local(slot) => *slot = value,
            BindingStorage::Shared(cell) => {
                *cell.lock().expect("shared binding lock") = snapshot_value_for_thread(&value);
            }
        }
    }

    pub(crate) fn shared_snapshot_cell(&mut self) -> Arc<Mutex<ThreadValueSnapshot>> {
        match &mut self.storage {
            BindingStorage::Shared(cell) => cell.clone(),
            BindingStorage::Local(value) => {
                let cell = Arc::new(Mutex::new(snapshot_value_for_thread(value)));
                self.storage = BindingStorage::Shared(cell.clone());
                cell
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct Environment {
    pub(crate) scopes: Vec<HashMap<String, BindingRef>>,
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub(crate) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn declare_with_value(&mut self, name: String, mutable: bool, value: Value) {
        self.declare_binding(
            name,
            Rc::new(RefCell::new(Binding::with_value(mutable, value))),
        );
    }

    pub(crate) fn declare_placeholder(&mut self, name: String, mutable: bool) -> BindingRef {
        let binding = Rc::new(RefCell::new(Binding::placeholder(mutable)));
        self.declare_binding(name, binding.clone());
        binding
    }

    fn declare_binding(&mut self, name: String, binding: BindingRef) {
        self.scopes
            .last_mut()
            .expect("at least one scope always exists")
            .insert(name, binding);
    }

    pub(crate) fn get_binding(&self, name: &str) -> Option<BindingRef> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }

    pub(crate) fn lookup_value(&self, name: &str) -> Option<Value> {
        self.get_binding(name)
            .map(|binding| binding.borrow().current_value())
    }

    pub(crate) fn assign(&mut self, name: &str, value: Value) -> Result<Value, AssignmentFailure> {
        let Some(binding) = self.get_binding(name) else {
            return Err(AssignmentFailure::Undefined);
        };
        let mut binding = binding.borrow_mut();
        if !binding.mutable {
            return Err(AssignmentFailure::Immutable);
        }
        binding.set_value(value.clone());
        Ok(value)
    }

    pub(crate) fn root_exports(&self) -> HashMap<String, Value> {
        self.scopes
            .first()
            .into_iter()
            .flat_map(|scope| scope.iter())
            .map(|(name, binding)| (name.clone(), binding.borrow().current_value()))
            .collect()
    }
}

pub(crate) enum AssignmentFailure {
    Undefined,
    Immutable,
}
