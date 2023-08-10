//! This module provides the [`Interpreter`] trait and a few extra things.

use crate::{ast::SpanStmt, environment::Environment, object::SpanObject, span::Span};
use std::{cell::RefCell, fmt, rc::Rc};
use thiserror::Error;

/// An error encountered by the interpreter at runtime.
#[derive(Clone, Debug, PartialEq, Error)]
pub struct RuntimeError {
    /// The error message.
    pub message: String,

    /// The span where the error occurred.
    pub span: Span,
}

/// A runtime error has occured or we need to return from a function call.
pub enum ErrorOrReturn {
    /// A [`RuntimeError`] has occured.
    Error(RuntimeError),

    /// Return from the current function.
    Return(SpanObject),
}

impl From<RuntimeError> for ErrorOrReturn {
    fn from(value: RuntimeError) -> Self {
        Self::Error(value)
    }
}

/// A result wrapping a [`RuntimeError`].
pub type Result<T, E = ErrorOrReturn> = ::std::result::Result<T, E>;

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RuntimeError({:?})", self.message)
    }
}

pub trait Interpreter {
    /// Create a new interpreter.
    fn new() -> Self
    where
        Self: Sized;

    /// Get an `Rc` to the interpreter's current environment.
    fn get_current_env(&self) -> Rc<RefCell<Environment>>;

    /// Interpret the given AST, reporting (not returning) a runtime error if one occurs.
    fn interpret(&mut self, stmts: &[SpanStmt]);

    /// Execute the given block.
    ///
    /// If the environment argument is Some, then use that environment. Otherwise, create a
    /// new one for this block. Either way, restore the parent environment at the end.
    fn execute_block(
        &mut self,
        stmts: &[SpanStmt],
        environment: Option<Rc<RefCell<Environment>>>,
    ) -> Result<()>;
}
