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
    /// The environment being enclosed by this one.
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

    /// Mutate this environment to be a new empty one, enclosing the old one.
    pub fn wrap_with_new_env(this_env: &mut Rc<RefCell<Self>>) {
        *this_env = Rc::new(RefCell::new(Self::enclosing(Some(core::mem::take(
            this_env,
        )))));
    }

    /// Pop this environment and replace it with the environment that it was previously enclosing,
    /// panicking if so such environment exists.
    pub fn pop_env(this_env: &mut Rc<RefCell<Self>>) {
        let other_env = if let Some(other_env) = &this_env.borrow().enclosing {
            Rc::clone(other_env)
        } else {
            panic!("Environment::pop_env called with an environment that enclosed nothing");
        };

        *this_env = other_env;
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

    /// Re-assign an already existing name in the environment at the given depth above this one.
    pub fn assign_at_depth(
        env: &Rc<RefCell<Environment>>,
        depth: usize,
        name: &WithSpan<String>,
        value: LoxObject,
    ) {
        let env = Environment::ancestor(env, depth).unwrap_or_else(|| {
            panic!(
                "Resolved environment depth ({depth}) for name '{}' is too great (span = {:?})",
                name.value, name.span
            )
        });
        *env.borrow_mut()
            .values
            .get_mut(&name.value)
            .unwrap_or_else(|| {
                panic!(
                "Name '{}' does not exist at expected environment depth ({depth}) (span = {:?})",
                name.value, name.span
            )
            }) = value;
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
    ) -> LoxObject {
        Environment::ancestor(env, depth)
            .unwrap_or_else(|| {
                panic!(
                    "Resolved environment depth ({depth}) for name '{}' is too great (span = {:?})",
                    name.value, name.span
                )
            })
            .borrow()
            .values
            .get(&name.value)
            .unwrap_or_else(|| {
                panic!(
                "Name '{}' does not exist at expected environment depth ({depth}) (span = {:?})",
                name.value, name.span
            )
            })
            .clone()
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
