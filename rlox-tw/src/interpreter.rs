//! This module provides [`TwInterpreter`].

use crate::{
    ast::{BinaryOperator, Expr, UnaryOperator},
    lox::print_error_message,
    object::LoxObject,
};
use std::fmt;
use thiserror::Error;

/// An error encountered by the interpreter at runtime.
#[derive(Clone, Debug, PartialEq, Error)]
struct RuntimeError {
    /// The error message.
    message: String,
}

/// A result wrapping a [`RuntimeError`].
type Result<T, E = RuntimeError> = ::std::result::Result<T, E>;

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RuntimeError({:?})", self.message)
    }
}

/// A tree-walk Lox interpreter.
pub struct TwInterpreter {}

impl TwInterpreter {
    /// Interpret the given AST.
    pub fn interpret(expr: &Expr) -> Option<LoxObject> {
        let mut interpreter = Self {};

        match interpreter.evaluate(expr) {
            Ok(obj) => Some(obj),
            Err(e) => {
                print_error_message(None, &e.message);
                None
            }
        }
    }

    /// Evaluate the given expression.
    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject> {
        Ok(match expr {
            Expr::Nil => LoxObject::Nil,
            Expr::Boolean(b) => LoxObject::Boolean(*b),
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
                self.evaluate_binary_expression(*operator, left, right)?
            }
            Expr::Grouping(expr) => self.evaluate(expr)?,
            Expr::String(string) => LoxObject::String(string.clone()),
            Expr::Number(number) => LoxObject::Number(*number),
            Expr::Unary(operator, expr) => {
                let value = self.evaluate(expr)?;
                self.evaluate_unary_expression(*operator, value)?
            }
        })
    }

    fn evaluate_binary_expression(
        &mut self,
        operator: BinaryOperator,
        left: LoxObject,
        right: LoxObject,
    ) -> Result<LoxObject> {
        use BinaryOperator::*;
        use LoxObject::*;

        let unsupported = || {
            Err(RuntimeError {
                message: format!(
                    "Unsupported operation '{}' between types '{}' and '{}'",
                    operator.to_string(),
                    left.type_name(),
                    right.type_name()
                ),
            })
        };

        match (&left, &right) {
            (Number(a), Number(b)) => Ok(match operator {
                Slash => Number(a / b),
                Star => Number(a * b),
                Plus => Number(a + b),
                Minus => Number(a - b),
                Greater => Boolean(a > b),
                GreaterEqual => Boolean(a >= b),
                Less => Boolean(a < b),
                LessEqual => Boolean(a <= b),
                BangEqual => Boolean(a != b),
                EqualEqual => Boolean(a == b),
            }),
            (String(a), String(b)) => match operator {
                Plus => Ok(String(a.clone() + b)),
                EqualEqual => Ok(Boolean(a == b)),
                BangEqual => Ok(Boolean(a != b)),
                _ => unsupported(),
            },
            (Nil, Nil) => match operator {
                EqualEqual => Ok(Boolean(true)),
                BangEqual => Ok(Boolean(false)),
                _ => unsupported(),
            },
            (Boolean(a), Boolean(b)) => match operator {
                EqualEqual => Ok(Boolean(a == b)),
                BangEqual => Ok(Boolean(a != b)),
                _ => unsupported(),
            },
            _ => unsupported(),
        }
    }

    fn evaluate_unary_expression(
        &mut self,
        operator: UnaryOperator,
        value: LoxObject,
    ) -> Result<LoxObject> {
        use LoxObject::*;
        use UnaryOperator::*;

        let unsupported = || {
            Err(RuntimeError {
                message: format!(
                    "Unsupported operation '{}' on type '{}'",
                    operator.to_string(),
                    value.type_name(),
                ),
            })
        };

        match (operator, &value) {
            (Bang, val) => Ok(Boolean(!self.is_truthy(val))),
            (Minus, Number(n)) => Ok(Number(-*n)),
            _ => unsupported(),
        }
    }

    /// Check if the given object is truthy.
    fn is_truthy(&self, object: &LoxObject) -> bool {
        use LoxObject::*;

        match object {
            Nil | Boolean(false) => false,
            _ => true,
        }
    }
}
