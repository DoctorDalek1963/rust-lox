//! This module provides some native functions callable from Lox.

use super::LoxCallable;
use crate::{
    interpreter::{RuntimeError, TwInterpreter},
    object::{LoxObject, SpanObject},
    span::Span,
};
use std::{
    thread,
    time::{self, Duration},
};

/// Return the current Unix time in seconds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Clock;

impl LoxCallable for Clock {
    fn name(&self) -> &str {
        "clock"
    }

    fn arity(&self) -> u8 {
        0
    }

    fn call(
        &self,
        _: &mut TwInterpreter,
        callee_span: Span,
        _: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        Ok(LoxObject::Number(
            time::SystemTime::now()
                .duration_since(time::UNIX_EPOCH.into())
                .map_err(|error| RuntimeError {
                    message: format!("NATIVE FUNCTION INTERNAL ERROR: {error:?}"),
                    span: callee_span.union(&close_paren),
                })?
                .as_nanos() as f64
                / 1_000_000_000.0,
        ))
    }
}

/// Sleep for the given number of nanoseconds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SleepNs;

impl LoxCallable for SleepNs {
    fn name(&self) -> &str {
        "sleep_ns"
    }

    fn arity(&self) -> u8 {
        1
    }

    fn call(
        &self,
        _: &mut TwInterpreter,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        let &[nanos] = &arguments else {
            return Err(self.bad_arity_error(callee_span, arguments, close_paren));
        };

        let LoxObject::Number(nanos) = nanos.value else {
            return Err(self.bad_type_error(nanos, "number"));
        };

        thread::sleep(Duration::from_nanos(nanos.round() as u64));
        Ok(LoxObject::Nil)
    }
}
