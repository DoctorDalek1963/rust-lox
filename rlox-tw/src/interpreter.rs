//! This module provides [`TwInterpreter`].

use crate::{
    ast::{BinaryOperator, Expr, SpanExpr, SpanStmt, Stmt, UnaryOperator},
    object::{LoxObject, SpanObject},
    span::{Span, WithSpan},
};
use std::{collections::HashMap, fmt};
use thiserror::Error;

/// An error encountered by the interpreter at runtime.
#[derive(Clone, Debug, PartialEq, Error)]
struct RuntimeError {
    /// The error message.
    message: String,

    /// The span where the error occurred.
    span: Span,
}

/// A result wrapping a [`RuntimeError`].
type Result<T, E = RuntimeError> = ::std::result::Result<T, E>;

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RuntimeError({:?})", self.message)
    }
}

/// The environment of defined values in the current interpreter session.
#[derive(Clone, Debug, PartialEq)]
struct Environment {
    /// A map of variable names to their values.
    values: HashMap<String, LoxObject>,
}

impl Environment {
    /// Create a new, empty environment.
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    /// Assign a value to a variable name. Also declares a variable if it's new.
    fn assign(&mut self, name: String, value: LoxObject) {
        self.values.insert(name, value);
    }

    /// Get the value of the given variable, returning a [`RuntimeError`] if the name is undefined.
    fn get(&self, name: &str, span: Span) -> Result<&LoxObject> {
        self.values.get(name).ok_or_else(|| RuntimeError {
            span,
            message: format!("Undefined variable name '{name}'"),
        })
    }
}

/// A tree-walk Lox interpreter.
#[derive(Clone, Debug, PartialEq)]
pub struct TwInterpreter {
    global_environment: Environment,
}

impl TwInterpreter {
    /// Create a new tree-walk interpreter.
    pub fn new() -> Self {
        Self {
            global_environment: Environment::new(),
        }
    }

    /// Interpret the given AST, reporting a runtime error if one occurs.
    pub fn interpret(&mut self, stmts: &[SpanStmt]) {
        if let Err(e) = self.execute_statements(stmts) {
            crate::lox::report_runtime_error(e.span, &e.message);
        }
    }

    /// Execute the given statements.
    fn execute_statements(&mut self, stmts: &[SpanStmt]) -> Result<()> {
        for stmt in stmts {
            self.execute_statement(stmt)?;
        }
        Ok(())
    }

    /// Execute the given statement.
    fn execute_statement(&mut self, stmt: &SpanStmt) -> Result<()> {
        match &stmt.value {
            Stmt::Expression(expr) => {
                self.evaluate_expression(expr)?;
            }
            Stmt::Print(expr) => println!("{}", self.evaluate_expression(expr)?.print()),
            Stmt::VarDecl(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.evaluate_expression(expr)?.value,
                    None => LoxObject::Nil,
                };
                self.global_environment.assign(name.value.clone(), value);
            }
        }

        Ok(())
    }

    /// Evaluate the given expression.
    fn evaluate_expression(&mut self, expr: &SpanExpr) -> Result<SpanObject> {
        let WithSpan {
            mut span,
            value: expr,
        } = expr;

        let value = match expr {
            Expr::Nil => LoxObject::Nil,
            Expr::Boolean(b) => LoxObject::Boolean(*b),
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;
                let WithSpan {
                    span: new_span,
                    value,
                } = self.evaluate_binary_expression(*operator, left, right)?;
                span = new_span;
                value
            }
            Expr::Grouping(expr) => {
                let WithSpan {
                    span: new_span,
                    value,
                } = self.evaluate_expression(expr)?;
                span = new_span;
                value
            }
            Expr::String(string) => LoxObject::String(string.clone()),
            Expr::Number(number) => LoxObject::Number(*number),
            Expr::Unary(operator, expr) => {
                let value = self.evaluate_expression(expr)?;
                let WithSpan {
                    span: new_span,
                    value,
                } = self.evaluate_unary_expression(*operator, value)?;
                span = new_span;
                value
            }
            Expr::Variable(name) => self.global_environment.get(name, span)?.clone(),
        };

        Ok(WithSpan { span, value })
    }

    /// Evaluate a binary expression.
    fn evaluate_binary_expression(
        &mut self,
        operator: WithSpan<BinaryOperator>,
        left: SpanObject,
        right: SpanObject,
    ) -> Result<SpanObject> {
        use BinaryOperator::*;
        use LoxObject::*;

        let WithSpan {
            span: left_span,
            value: left,
        } = left;
        let WithSpan {
            span: right_span,
            value: right,
        } = right;
        let WithSpan {
            span: op_span,
            value: operator,
        } = operator;
        let span = left_span.union(&right_span).union(&op_span);

        let unsupported = || {
            Err(RuntimeError {
                message: format!(
                    "Unsupported operation '{}' between types '{}' and '{}'",
                    operator.to_string(),
                    left.type_name(),
                    right.type_name()
                ),
                span,
            })
        };

        let value = match (&left, &right) {
            (Number(a), Number(b)) => match operator {
                Slash => {
                    if *b == 0.0 {
                        return Err(RuntimeError {
                            message: "Division by 0".to_string(),
                            span,
                        });
                    } else {
                        Number(a / b)
                    }
                }
                Star => Number(a * b),
                Plus => Number(a + b),
                Minus => Number(a - b),
                Greater => Boolean(a > b),
                GreaterEqual => Boolean(a >= b),
                Less => Boolean(a < b),
                LessEqual => Boolean(a <= b),
                BangEqual => Boolean(a != b),
                EqualEqual => Boolean(a == b),
            },
            (String(a), String(b)) => match operator {
                Plus => String(a.clone() + b),
                EqualEqual => Boolean(a == b),
                BangEqual => Boolean(a != b),
                _ => unsupported()?,
            },
            (Nil, Nil) => match operator {
                EqualEqual => Boolean(true),
                BangEqual => Boolean(false),
                _ => unsupported()?,
            },
            (Boolean(a), Boolean(b)) => match operator {
                EqualEqual => Boolean(a == b),
                BangEqual => Boolean(a != b),
                _ => unsupported()?,
            },
            _ => unsupported()?,
        };

        Ok(WithSpan { span, value })
    }

    /// Evaluate a unary expression.
    fn evaluate_unary_expression(
        &mut self,
        operator: WithSpan<UnaryOperator>,
        object: SpanObject,
    ) -> Result<SpanObject> {
        use LoxObject::*;
        use UnaryOperator::*;

        let WithSpan { span, value } = object;
        let WithSpan {
            span: op_span,
            value: operator,
        } = operator;
        let span = span.union(&op_span);

        let unsupported = || {
            Err(RuntimeError {
                message: format!(
                    "Unsupported operation '{}' on type '{}'",
                    operator.to_string(),
                    value.type_name(),
                ),
                span,
            })
        };

        let value = match (operator, &value) {
            (Bang, val) => Boolean(!val.is_truthy()),
            (Minus, Number(n)) => Number(-*n),
            _ => unsupported()?,
        };

        Ok(WithSpan { span, value })
    }
}
