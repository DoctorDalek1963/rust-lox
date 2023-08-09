//! This module provides [`Environment`].

use crate::{interpreter::RuntimeError, object::LoxObject, span::Span};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// The environment of defined values in the current interpreter session.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    /// The enclosing environment.
    pub enclosing: Option<Rc<RefCell<Environment>>>,

    /// A map of variable names to their values.
    values: HashMap<String, LoxObject>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::enclosing(None)
    }
}

impl Environment {
    /// Create a new environment enclosing the given environment.
    pub fn enclosing(enclosing: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            enclosing,
            values: HashMap::new(),
        }
    }

    /// Define a new variable with the given value.
    pub fn define(&mut self, name: String, value: LoxObject) {
        self.values.insert(name, value);
    }

    /// Re-assign an already existing variable. Returns a [`RuntimeError`] if the name is undefined.
    pub fn assign(&mut self, name: &str, value: LoxObject, span: Span) -> Result<(), RuntimeError> {
        if let Some(current) = self.values.get_mut(name) {
            *current = value;
            Ok(())
        } else {
            if let Some(env) = &mut self.enclosing {
                env.borrow_mut().assign(name, value, span)
            } else {
                Err(RuntimeError {
                    message: format!("Undefined variable name '{name}'"),
                    span,
                })
            }
        }
    }

    /// Get the value of the given variable, returning a [`RuntimeError`] if the name is undefined.
    pub fn get(&self, name: &str, span: Span) -> Result<LoxObject, RuntimeError> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else {
            if let Some(env) = &self.enclosing {
                env.borrow().get(name, span).clone()
            } else {
                Err(RuntimeError {
                    span,
                    message: format!("Undefined variable name '{name}'"),
                })
            }
        }
    }
}
