//! This module provides [`LoxFunction`].

use crate::{
    ast::SpanStmt,
    callable::LoxCallable,
    environment::Environment,
    interpreter::{ErrorOrReturn, Interpreter, RuntimeError},
    object::{LoxObject, SpanObject},
    span::{Span, WithSpan},
};
use std::{cell::RefCell, rc::Rc};

/// A function that was defined by user Lox code.
#[derive(Clone, Debug, PartialEq)]
pub struct LoxFunction {
    /// The name of the function, including the span where it was defined.
    name: WithSpan<String>,

    /// The parameters that this function takes.
    parameters: Box<[WithSpan<String>]>,

    /// The body of the function.
    body: Box<[SpanStmt]>,

    /// The environment that the function was defined in.
    environment: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    /// Create a new Lox function.
    pub fn new(
        name: WithSpan<String>,
        parameters: impl Into<Box<[WithSpan<String>]>>,
        body: impl Into<Box<[SpanStmt]>>,
        environment: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            name,
            parameters: parameters.into(),
            body: body.into(),
            environment,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn name(&self) -> &str {
        &self.name
    }

    fn arity(&self) -> u8 {
        self.parameters
            .len()
            .try_into()
            .expect("Functions can never be defined with >= 255 params")
    }

    fn call(
        &self,
        interpreter: &mut dyn Interpreter,
        _callee_span: Span,
        arguments: &[SpanObject],
        _close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        let mut environment = Environment::enclosing(Some(Rc::clone(&self.environment)));

        for i in 0..self.parameters.len() {
            environment.define(self.parameters[i].value.clone(), arguments[i].value.clone());
        }

        match interpreter.execute_block(&self.body, Some(Rc::new(RefCell::new(environment)))) {
            Err(ErrorOrReturn::Return(value)) => Ok(value.value),
            Ok(()) => Ok(LoxObject::Nil),
            Err(ErrorOrReturn::Error(e)) => Err(e),
        }
    }
}
