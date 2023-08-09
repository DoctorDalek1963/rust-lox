//! This module provides [`TwInterpreter`].

use crate::{
    ast::{BinaryOperator, Expr, LogicalOperator, SpanExpr, SpanStmt, Stmt, UnaryOperator},
    callable::{lox_function::LoxFunction, native, LoxCallable},
    environment::Environment,
    object::{LoxObject, SpanObject},
    span::{Span, WithSpan},
};
use std::{cell::RefCell, fmt, mem, rc::Rc};
use thiserror::Error;

/// An error encountered by the interpreter at runtime.
#[derive(Clone, Debug, PartialEq, Error)]
pub struct RuntimeError {
    /// The error message.
    pub message: String,

    /// The span where the error occurred.
    pub span: Span,
}

/// A result wrapping a [`RuntimeError`].
type Result<T, E = RuntimeError> = ::std::result::Result<T, E>;

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RuntimeError({:?})", self.message)
    }
}

/// A tree-walk Lox interpreter.
#[derive(Clone, Debug, PartialEq)]
pub struct TwInterpreter {
    /// The global environment.
    global_env: Rc<RefCell<Environment>>,

    /// The environment of defined values in the current interpreter session.
    current_env: Rc<RefCell<Environment>>,
}

impl TwInterpreter {
    /// Create a new tree-walk interpreter.
    pub fn new() -> Self {
        use native::*;

        let environment = Rc::new(RefCell::new(Environment::default()));

        macro_rules! define_native_functions {
            ( $($name:literal => $function:expr),* $(,)? ) => {
                $(
                    environment.borrow_mut().define(
                        $name.to_string(),
                        LoxObject::NativeFunction(Rc::new($function))
                    );
                )*
            };
        }

        define_native_functions!("clock" => Clock, "sleep_ns" => SleepNs);

        Self {
            global_env: Rc::clone(&environment),
            current_env: environment,
        }
    }

    /// Get an `Rc` to the interpreter's global environment.
    pub fn get_global_env(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(&self.global_env)
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
            Stmt::VarDecl(name, initializer) => self.execute_var_decl(name, initializer)?,
            Stmt::FunDecl(name, parameters, body) => self.execute_fun_decl(name, parameters, body),
            Stmt::Expression(expr) => {
                self.evaluate_expression(expr)?;
            }
            Stmt::If(condition, then_branch, else_branch) => {
                self.execute_if_statement(condition, then_branch, else_branch)?
            }
            Stmt::Print(expr) => println!("{}", self.evaluate_expression(expr)?.print()),
            Stmt::While(condition, body) => self.execute_while_loop(condition, body)?,
            Stmt::Block(stmts) => self.execute_block(stmts, None)?,
        }

        Ok(())
    }

    /// Execute a variable declaration in the current environment.
    fn execute_var_decl(
        &mut self,
        name: &WithSpan<String>,
        initializer: &Option<SpanExpr>,
    ) -> Result<()> {
        let value = match initializer {
            Some(expr) => self.evaluate_expression(expr)?.value,
            None => LoxObject::Nil,
        };
        self.current_env
            .borrow_mut()
            .define(name.value.clone(), value);
        Ok(())
    }

    /// Execute a function declaration in the current environment.
    fn execute_fun_decl(
        &mut self,
        name: &WithSpan<String>,
        parameters: &Vec<WithSpan<String>>,
        body: &Vec<SpanStmt>,
    ) {
        let function = LoxFunction::new(name.clone(), parameters.clone(), body.clone());
        self.current_env.borrow_mut().define(
            name.value.clone(),
            LoxObject::LoxFunction(Rc::new(function)),
        );
    }

    /// Execute an `if` statement.
    fn execute_if_statement(
        &mut self,
        condition: &SpanExpr,
        then_branch: &Box<SpanStmt>,
        else_branch: &Option<Box<SpanStmt>>,
    ) -> Result<()> {
        if self.evaluate_expression(condition)?.is_truthy() {
            self.execute_statement(&then_branch)?;
        } else if let Some(else_branch) = else_branch {
            self.execute_statement(&else_branch)?;
        }

        Ok(())
    }

    /// Execute a while loop.
    fn execute_while_loop(&mut self, condition: &SpanExpr, body: &Box<SpanStmt>) -> Result<()> {
        while self.evaluate_expression(condition)?.is_truthy() {
            self.execute_statement(body)?;
        }

        Ok(())
    }

    /// Execute the given block.
    ///
    /// If the environment argument is Some, then we use that environment. Otherwise, we create a
    /// new one for this block. Either way, we restore the parent environment at the end.
    pub fn execute_block(
        &mut self,
        stmts: &[SpanStmt],
        environment: Option<Rc<RefCell<Environment>>>,
    ) -> Result<()> {
        if let Some(environment) = environment {
            self.current_env = environment;
        } else {
            let original_env = mem::take(&mut self.current_env);
            self.current_env = Rc::new(RefCell::new(Environment::enclosing(Some(original_env))));
        }

        let result = self.execute_statements(stmts);
        self.current_env = {
            let x = self.current_env.borrow();
            let y = x.enclosing.as_ref().unwrap();
            Rc::clone(&y)
        };
        result
    }

    /// Evaluate the given expression.
    fn evaluate_expression(&mut self, expr: &SpanExpr) -> Result<SpanObject> {
        let WithSpan { span, value: expr } = expr;
        let span = *span;

        Ok(match expr {
            Expr::Nil => WithSpan {
                span,
                value: LoxObject::Nil,
            },
            Expr::Boolean(b) => WithSpan {
                span,
                value: LoxObject::Boolean(*b),
            },
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;
                self.evaluate_binary_expression(*operator, left, right)?
            }
            Expr::Call(callee, arguments, close_paren) => {
                self.evaluate_function_call(callee, arguments, close_paren)?
            }
            Expr::Grouping(expr) => {
                let value = self.evaluate_expression(expr)?.value;
                WithSpan { span, value }
            }
            Expr::String(string) => WithSpan {
                span,
                value: LoxObject::String(string.clone()),
            },
            Expr::Number(number) => WithSpan {
                span,
                value: LoxObject::Number(*number),
            },
            Expr::Logical(left, operator, right) => {
                self.evaluate_logical_expression(left, operator, right)?
            }
            Expr::Unary(operator, expr) => {
                let value = self.evaluate_expression(expr)?;
                self.evaluate_unary_expression(*operator, value)?
            }
            Expr::Variable(name) => WithSpan {
                span,
                value: self.current_env.borrow().get(name, span)?.clone(),
            },
            Expr::Assign(name, expr) => {
                let value = self.evaluate_expression(expr)?;
                self.current_env
                    .borrow_mut()
                    .assign(name, value.value.clone(), span)?;
                value
            }
        })
    }

    /// Evaluate a function call with the given callee and arguments.
    fn evaluate_function_call(
        &mut self,
        callee: &SpanExpr,
        arguments: &[SpanExpr],
        close_paren: &Span,
    ) -> Result<SpanObject> {
        let callee = self.evaluate_expression(callee)?;
        let callee_span = callee.span;

        let arguments: Vec<SpanObject> = arguments
            .iter()
            .map(|expr| self.evaluate_expression(expr))
            .collect::<Result<Vec<SpanObject>>>()?;

        let func = self.try_get_function(callee, close_paren)?;

        if arguments.len() != func.arity() as usize {
            return Err(func.bad_arity_error(callee_span, &arguments, *close_paren));
        }

        let value = func.call(self, callee_span, &arguments, *close_paren)?;

        Ok(WithSpan {
            span: callee_span.union(close_paren),
            value,
        })
    }

    /// Try to resolve a function from an object.
    fn try_get_function(
        &self,
        callee: SpanObject,
        close_paren: &Span,
    ) -> Result<Rc<dyn LoxCallable>> {
        match &callee.value {
            LoxObject::NativeFunction(func) => Ok(Rc::clone(func)),
            LoxObject::LoxFunction(func) => Ok(Rc::clone(func) as Rc<dyn LoxCallable>),
            _ => Err(RuntimeError {
                message: format!(
                    "Can only call objects of type function or class, not {}",
                    callee.value.type_name()
                ),
                span: callee.span.union(close_paren),
            }),
        }
    }

    /// Evaluate a logical expression by short-circuiting.
    fn evaluate_logical_expression(
        &mut self,
        left: &SpanExpr,
        operator: &WithSpan<LogicalOperator>,
        right: &SpanExpr,
    ) -> Result<SpanObject> {
        let span = left.span.union(&right.span);
        let left = self.evaluate_expression(left)?;

        match operator.value {
            LogicalOperator::Or if left.value.is_truthy() => Ok(WithSpan { span, ..left }),
            LogicalOperator::And if !left.value.is_truthy() => Ok(WithSpan { span, ..left }),
            _ => {
                let right = self.evaluate_expression(right)?;
                Ok(WithSpan { span, ..right })
            }
        }
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
