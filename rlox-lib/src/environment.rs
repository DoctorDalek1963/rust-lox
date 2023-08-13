//! This module provides [`Environment`].

use crate::{
    interpreter::RuntimeError,
    object::LoxObject,
    span::{Span, WithSpan},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// The environment of defined values in the current interpreter session.
#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    /// The enclosing environment.
    pub enclosing: Option<Rc<RefCell<Environment>>>,

    /// A map of variable names to their values.
    pub values: HashMap<String, LoxObject>,
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

    /// Re-assign an already existing name. Returns a [`RuntimeError`] if the name is undefined.
    pub fn assign(&mut self, name: &str, value: LoxObject, span: Span) -> Result<(), RuntimeError> {
        if let Some(current) = self.values.get_mut(name) {
            *current = value;
            Ok(())
        } else if let Some(env) = &mut self.enclosing {
            env.borrow_mut().assign(name, value, span)
        } else {
            Err(RuntimeError {
                message: format!("Undefined variable name '{name}'"),
                span,
            })
        }
    }

    pub fn assign_at_depth(
        env: &Rc<RefCell<Environment>>,
        depth: usize,
        name: &WithSpan<String>,
        value: LoxObject,
    ) -> Result<(), RuntimeError> {
        match Environment::ancestor(env, depth) {
            Some(env) => match env.borrow_mut().values.get_mut(&name.value) {
                Some(current) => {
                    *current = value;
                    Ok(())
                }
                None => Err(RuntimeError {
                    message: format!(
                        concat!(
                            "INTERNAL ERROR: Name '{}' does not exist ",
                            "at expected environment depth"
                        ),
                        name.value
                    ),
                    span: name.span,
                }),
            },
            None => Err(RuntimeError {
                message: format!(
                    "INTERNAL ERROR: Resolved environment depth for name '{}' is too great",
                    name.value
                ),
                span: name.span,
            }),
        }
    }

    /// Get the value of the given name, returning a [`RuntimeError`] if the name is undefined.
    pub fn get(&self, name: &WithSpan<String>) -> Result<LoxObject, RuntimeError> {
        if let Some(value) = self.values.get(&name.value) {
            Ok(value.clone())
        } else if let Some(env) = &self.enclosing {
            env.borrow().get(name).clone()
        } else {
            Err(RuntimeError {
                span: name.span,
                message: format!("Undefined variable name '{}'", name.value),
            })
        }
    }

    /// Get the value of the given name in the environment at the given depth above this one.
    pub fn get_at_depth(
        env: &Rc<RefCell<Environment>>,
        depth: usize,
        name: &WithSpan<String>,
    ) -> Result<LoxObject, RuntimeError> {
        match Environment::ancestor(env, depth) {
            Some(env) => match env.borrow().values.get(&name.value) {
                Some(value) => Ok(value.clone()),
                None => Err(RuntimeError {
                    message: format!(
                        concat!(
                            "INTERNAL ERROR: Name '{}' does not exist ",
                            "at expected environment depth"
                        ),
                        name.value
                    ),
                    span: name.span,
                }),
            },
            None => Err(RuntimeError {
                message: format!(
                    "INTERNAL ERROR: Resolved environment depth for name '{}' is too great",
                    name.value
                ),
                span: name.span,
            }),
        }
    }

    /// Get the ancestor of this environment at the given distance.
    fn ancestor(
        env: &Rc<RefCell<Environment>>,
        distance: usize,
    ) -> Option<Rc<RefCell<Environment>>> {
        if distance == 0 {
            Some(Rc::clone(env))
        } else {
            let enclosing = match &env.borrow().enclosing {
                Some(x) => Rc::clone(x),
                None => return None,
            };
            Self::ancestor(&enclosing, distance - 1)
        }
    }
}
