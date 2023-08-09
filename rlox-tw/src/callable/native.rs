//! This module provides some native functions callable from Lox.

use super::LoxCallable;
use crate::{
    environment::Environment,
    interpreter::{RuntimeError, TwInterpreter},
    object::{LoxObject, SpanObject},
    span::Span,
};
use std::{
    thread,
    time::{self, Duration},
};

/// Return a boolean representing the truthiness of the given value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bool;

impl LoxCallable for Bool {
    fn name(&self) -> &str {
        "bool"
    }

    fn arity(&self) -> u8 {
        1
    }

    fn call(
        &self,
        _interpreter: &mut TwInterpreter,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        let &[value] = &arguments else {
            return Err(self.bad_arity_error(callee_span, arguments, close_paren));
        };

        Ok(LoxObject::Boolean(value.is_truthy()))
    }
}

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
        _interpreter: &mut TwInterpreter,
        callee_span: Span,
        _arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        Ok(LoxObject::Number(
            time::SystemTime::now()
                .duration_since(time::UNIX_EPOCH)
                .map_err(|error| RuntimeError {
                    message: format!("NATIVE FUNCTION INTERNAL ERROR: {error:?}"),
                    span: callee_span.union(&close_paren),
                })?
                .as_nanos() as f64
                / 1_000_000_000.0,
        ))
    }
}

/// Print every variable and function in the current environment and all enclosing environments.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Env;

impl LoxCallable for Env {
    fn name(&self) -> &str {
        "env"
    }

    fn arity(&self) -> u8 {
        0
    }

    fn call(
        &self,
        interpreter: &mut TwInterpreter,
        _callee_span: Span,
        _arguments: &[SpanObject],
        _close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        use std::cmp::Ordering;

        /// A representation of a [`LoxObject`] to be printing in the call to [`env`](Env::call).
        #[derive(Clone, Debug, PartialEq, Eq)]
        struct Value {
            name: String,
            type_name: String,
            repr: String,
        }

        impl PartialOrd for Value {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for Value {
            fn cmp(&self, other: &Self) -> Ordering {
                if self.type_name.starts_with("<") && !other.type_name.starts_with("<") {
                    Ordering::Less
                } else if !self.type_name.starts_with("<") && other.type_name.starts_with("<") {
                    Ordering::Greater
                } else {
                    self.name.cmp(&other.name)
                }
            }
        }

        /// An entry in a list of values and appropriate scoping markers.
        #[derive(Clone, Debug, PartialEq)]
        enum ListEntry {
            Value(Value),
            StartScope,
        }

        /// Collect the values in this environment and the enclosing one if it exists.
        fn collect_values(env: &Environment) -> Vec<ListEntry> {
            let mut current_env: Vec<Value> = env
                .values
                .iter()
                .map(|(name, obj)| Value {
                    name: name.clone(),
                    type_name: obj.type_name(),
                    repr: obj.repr(),
                })
                .collect();

            current_env.sort();

            let list: Vec<ListEntry> = current_env.into_iter().map(ListEntry::Value).collect();

            if let Some(env) = &env.enclosing {
                let mut parent_env = collect_values(&env.borrow());
                parent_env.push(ListEntry::StartScope);
                parent_env.extend(list);
                parent_env
            } else {
                list
            }
        }

        /// Print the values in the list, scoping accordingly.
        fn print_values(values: &[ListEntry]) -> String {
            let mut string = String::new();
            let mut scope_counter = 0u32;

            for value in values {
                for _ in 0..scope_counter {
                    string.push_str("  ");
                }

                match value {
                    ListEntry::Value(Value {
                        name,
                        type_name,
                        repr,
                    }) => string.push_str(&format!("{name}: {type_name} = {repr};")),
                    ListEntry::StartScope => {
                        string.push_str("{");
                        scope_counter = scope_counter + 1;
                    }
                }
                string.push('\n');
            }

            while scope_counter > 0 {
                scope_counter = scope_counter - 1;
                for _ in 0..scope_counter {
                    string.push_str("  ");
                }

                string.push_str("}");
                string.push('\n');
            }

            string.pop();
            string
        }

        println!(
            "{}",
            print_values(&collect_values(&interpreter.get_current_env().borrow()))
        );

        Ok(LoxObject::Nil)
    }
}

/// Raise a base number to an exponent.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Pow;

impl LoxCallable for Pow {
    fn name(&self) -> &str {
        "pow"
    }

    fn arity(&self) -> u8 {
        2
    }

    fn call(
        &self,
        _interpreter: &mut TwInterpreter,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        let &[base, exponent] = &arguments else {
            return Err(self.bad_arity_error(callee_span, arguments, close_paren));
        };

        let LoxObject::Number(base) = base.value else {
            return Err(self.bad_type_error(base, "number"));
        };
        let LoxObject::Number(exponent) = exponent.value else {
            return Err(self.bad_type_error(exponent, "number"));
        };

        Ok(LoxObject::Number(base.powf(exponent)))
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
        _interpreter: &mut TwInterpreter,
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

/// Convert the given value to a string.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Str;

impl LoxCallable for Str {
    fn name(&self) -> &str {
        "str"
    }

    fn arity(&self) -> u8 {
        1
    }

    fn call(
        &self,
        _interpreter: &mut TwInterpreter,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        let &[value] = &arguments else {
            return Err(self.bad_arity_error(callee_span, arguments, close_paren));
        };

        Ok(LoxObject::String(value.print()))
    }
}
