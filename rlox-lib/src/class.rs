//! This module provides [`LoxClass`] and [`LoxInstance`].

use std::rc::Rc;

use crate::{
    callable::LoxCallable,
    interpreter::RuntimeError,
    object::{LoxObject, SpanObject},
    span::{Span, WithSpan},
    Interpreter,
};

/// A class itself, used to create instances.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LoxClass {
    /// The name of the class, including the span where it was defined.
    name: WithSpan<String>,
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
        Ok(LoxObject::LoxInstance(LoxInstance::new(Rc::clone(self))))
    }
}

/// An instance of a class, created from its constructor.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LoxInstance {
    /// The class that created this instance.
    class: Rc<LoxClass>,
}

impl LoxInstance {
    /// Create a new instance.
    pub fn new(class: Rc<LoxClass>) -> Self {
        Self { class }
    }

    /// Get the name of the class that created this instance.
    pub fn class_name(&self) -> &str {
        self.class.name()
    }
}
