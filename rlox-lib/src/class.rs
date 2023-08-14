//! This module provides [`LoxClass`] and [`LoxInstance`].

use crate::{
    callable::LoxCallable,
    interpreter::RuntimeError,
    object::{LoxObject, SpanObject},
    span::{Span, WithSpan},
    Interpreter,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// A class itself, used to create instances.
#[derive(Clone, Debug, Eq, Hash)]
pub struct LoxClass {
    /// The name of the class, including the span where it was defined.
    name: WithSpan<String>,
}

impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl LoxClass {
    /// Create a new Lox class.
    pub fn new(name: WithSpan<String>) -> Self {
        Self { name }
    }
}

impl LoxCallable for Rc<LoxClass> {
    fn name(&self) -> &str {
        &self.name.value
    }

    fn arity(&self) -> u8 {
        // TODO: Read init method
        0
    }

    fn call(
        &self,
        _interpreter: &mut dyn Interpreter,
        _callee_span: Span,
        _arguments: &[SpanObject],
        _close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        Ok(LoxObject::LoxInstance(Rc::new(RefCell::new(
            LoxInstance::new(Rc::clone(self)),
        ))))
    }
}

/// An instance of a class, created from its constructor.
#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance {
    /// The class that created this instance.
    class: Rc<LoxClass>,

    /// The fields on this instance.
    fields: HashMap<String, LoxObject>,
}

impl LoxInstance {
    /// Create a new instance.
    pub fn new(class: Rc<LoxClass>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    /// Get the name of the class that created this instance.
    pub fn class_name(&self) -> &str {
        self.class.name()
    }

    /// Get the given property on this instance.
    pub fn get(&self, ident: &WithSpan<String>) -> Result<LoxObject, RuntimeError> {
        self.fields
            .get(&ident.value)
            .cloned()
            .ok_or_else(|| RuntimeError {
                message: format!("No property '{}' on instance", ident.value),
                span: ident.span,
            })
    }

    /// Set the value of the field.
    pub fn set(&mut self, ident: String, value: LoxObject) {
        self.fields.insert(ident, value);
    }
}
