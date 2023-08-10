//! This module provides the [`LoxCallable`] trait.

pub mod lox_function;
pub mod native;

use crate::{
    interpreter::{Interpreter, RuntimeError},
    object::{LoxObject, SpanObject},
    span::Span,
};
use std::fmt;

/// A trait to encompass the ability to call an object in Lox.
pub trait LoxCallable: fmt::Debug {
    /// The name of this function.
    fn name(&self) -> &str;

    /// The number of arguments that this callable takes.
    fn arity(&self) -> u8;

    /// Call the callable and return a value.
    ///
    /// Implementors should assume the arguments list will have the length returned by
    /// [`arity`](LoxCallable::arity), since this is checked by the interpreter before calling this
    /// function.
    fn call(
        &self,
        interpreter: &mut dyn Interpreter,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError>;

    /// Return the error for when the argument list has a length that disagrees with the arity.
    fn bad_arity_error(
        &self,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> RuntimeError {
        assert_ne!(
            arguments.len(),
            self.arity() as usize,
            concat!(
                "This method should only be called when the argument ",
                "number and arity are definitely different"
            )
        );

        RuntimeError {
            message: format!(
                "Expected {} arguments in function call but got {}",
                self.arity(),
                arguments.len()
            ),
            span: arguments
                .iter()
                .map(|obj| obj.span)
                .fold(callee_span, |acc, span| acc.union(&span))
                .union(&close_paren),
        }
    }

    fn bad_type_error(&self, argument: &SpanObject, expected_type: &str) -> RuntimeError {
        assert_ne!(
            argument.value.type_name(),
            expected_type,
            "This method should only be called when the type of the argument is definitely wrong"
        );

        RuntimeError {
            message: format!(
                "Expected value of type {} but got value of type {}",
                expected_type,
                argument.value.type_name()
            ),
            span: argument.span,
        }
    }
}
