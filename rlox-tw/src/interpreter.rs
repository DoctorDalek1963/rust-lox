//! This module provides [`TwInterpreter`].

use crate::resolver::Resolver;
use rlox_lib::{
    ast::{
        BinaryOperator, Expr, FunctionOrMethod, LogicalOperator, SpanExpr, SpanStmt, Stmt,
        UnaryOperator,
    },
    callable::{self, lox_function::LoxFunction, LoxCallable},
    class::{LoxClass, LoxInstance},
    environment::Environment,
    interpreter::{ErrorOrReturn, Interpreter, Result, RuntimeError},
    object::{LoxObject, SpanObject},
    pretty_printers::ParenPrinter,
    span::{Span, WithSpan},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use tracing::{debug, instrument, trace};

/// A tree-walk Lox interpreter.
#[derive(Clone, Debug, PartialEq)]
pub struct TwInterpreter {
    /// The global environment.
    global_env: Rc<RefCell<Environment>>,

    /// The environment of defined values in the current interpreter session.
    current_env: Rc<RefCell<Environment>>,

    /// A map from a name to its local environment depth. When resolving that name, go up that many
    /// environments in the chain.
    locals: HashMap<WithSpan<String>, usize>,
}

impl Interpreter for TwInterpreter {
    fn new() -> Self {
        use callable::native::*;

        let environment = Rc::new(RefCell::new(Environment::default()));

        macro_rules! define_native_functions {
            ( $($function:expr),* $(,)? ) => {
                $(
                    environment.borrow_mut().define(
                        $function.name().to_string(),
                        LoxObject::NativeFunction(Rc::new($function))
                    );
                )*
            };
        }

        define_native_functions!(Bool, Clock, Env, Pow, SleepNs, Str);

        Self {
            global_env: Rc::clone(&environment),
            current_env: environment,
            locals: HashMap::new(),
        }
    }

    fn get_current_env(&self) -> Rc<RefCell<Environment>> {
        Rc::clone(&self.current_env)
    }

    fn interpret(&mut self, stmts: &[SpanStmt]) {
        match Resolver::get_locals_map(stmts) {
            Ok(map) => self.locals.extend(map),
            Err(e) => {
                rlox_lib::lox::report_non_runtime_error(e.span, &e.message);
                return;
            }
        };

        if let Err(ErrorOrReturn::Error(e)) = self.execute_statements(stmts) {
            rlox_lib::lox::report_runtime_error(e.span, &e.message);
        }
    }

    fn execute_block(
        &mut self,
        stmts: &[SpanStmt],
        environment: Option<Rc<RefCell<Environment>>>,
    ) -> Result<()> {
        if let Some(environment) = environment {
            self.current_env = environment;
        } else {
            Environment::wrap_with_new_env(&mut self.current_env);
        }

        let result = self.execute_statements(stmts);
        Environment::pop_env(&mut self.current_env);
        result
    }
}

macro_rules! trace_evaluating {
    ($expression:expr) => {
        ::tracing::trace!(
            "Evaluating `{}`",
            ::rlox_lib::pretty_printers::ParenPrinter::print_expr($expression)
        )
    };
}

macro_rules! trace_evaluated {
    ($expression:expr, $result:expr) => {
        ::tracing::trace!(
            "Evaluated `{}` to {}",
            ::rlox_lib::pretty_printers::ParenPrinter::print_expr($expression),
            $result.repr()
        )
    };
}

impl TwInterpreter {
    /// Execute the given statements.
    fn execute_statements(&mut self, stmts: &[SpanStmt]) -> Result<()> {
        for stmt in stmts {
            self.execute_statement(stmt)?;
        }
        Ok(())
    }

    /// Execute the given statement.
    fn execute_statement(&mut self, stmt: &SpanStmt) -> Result<()> {
        debug!("Executing statement `{}`", ParenPrinter::print_stmt(stmt));

        match &stmt.value {
            Stmt::ClassDecl(name, superclass_name, methods) => {
                self.execute_class_decl(name, superclass_name.as_ref(), methods)?
            }
            Stmt::VarDecl(name, initializer) => self.execute_var_decl(name, initializer)?,
            Stmt::FunDecl((name, parameters, _, body)) => {
                self.execute_fun_decl(name, parameters, body)
            }
            Stmt::Expression(expr) => {
                self.evaluate_expression(expr)?;
            }
            Stmt::If(condition, then_branch, else_branch) => {
                self.execute_if_statement(condition, then_branch, else_branch)?
            }
            Stmt::Print(expr) => self.execute_print(expr)?,
            Stmt::Return(keyword_span, expr) => self.execute_return(keyword_span, expr)?,
            Stmt::While(condition, body) => self.execute_while_loop(condition, body)?,
            Stmt::Block(stmts) => self.execute_block(stmts, None)?,
        }

        Ok(())
    }

    /// Execute a class declaration in the current environment.
    #[instrument(skip_all)]
    fn execute_class_decl(
        &mut self,
        name: &WithSpan<String>,
        superclass_name: Option<&WithSpan<String>>,
        methods: &[WithSpan<FunctionOrMethod>],
    ) -> Result<()> {
        let superclass: Option<Rc<LoxClass>> = if let Some(superclass_name) = superclass_name {
            let WithSpan { span, value } = superclass_name.clone();
            let superclass = self.evaluate_expression(&WithSpan {
                span,
                value: Expr::Variable(value),
            })?;

            match superclass.value {
                LoxObject::LoxClass(class) => {
                    trace!(
                        "Declaring class {} (subclass of {}) with {} methods",
                        name.value,
                        class.name(),
                        methods.len()
                    );

                    Some(class)
                }
                _ => {
                    return Err(ErrorOrReturn::Error(RuntimeError {
                        message: "Superclass must be a valid class".to_string(),
                        span,
                    }));
                }
            }
        } else {
            trace!(
                "Declaring class {} with {} methods",
                name.value,
                methods.len()
            );

            None
        };

        self.current_env
            .borrow_mut()
            .define(name.value.clone(), LoxObject::Nil);

        if let Some(superclass) = &superclass {
            Environment::wrap_with_new_env(&mut self.current_env);
            self.current_env.borrow_mut().define(
                String::from("super"),
                LoxObject::LoxClass(Rc::clone(superclass)),
            );
        }

        let methods_map: HashMap<String, Rc<LoxFunction>> = methods
            .iter()
            .map(
                |WithSpan {
                     span: _,
                     value: (method_name, params, _close_paren_span, body),
                 }| {
                    trace!(
                        "Declaring method {}.{} with {} parameters",
                        name.value,
                        method_name.value,
                        params.len()
                    );

                    (
                        method_name.value.clone(),
                        Rc::new(LoxFunction::new(
                            method_name.clone(),
                            params.clone(),
                            body.clone(),
                            Rc::clone(&self.current_env),
                            method_name.value == "init",
                        )),
                    )
                },
            )
            .collect();

        let superclass_is_some = superclass.is_some();

        let class = LoxObject::LoxClass(Rc::new(LoxClass::new(
            name.clone(),
            superclass,
            methods_map,
        )));

        if superclass_is_some {
            Environment::pop_env(&mut self.current_env);
        }

        self.current_env
            .borrow_mut()
            .assign(&name.value, class, name.span)?;
        Ok(())
    }

    /// Execute a variable declaration in the current environment.
    #[instrument(skip_all)]
    fn execute_var_decl(
        &mut self,
        name: &WithSpan<String>,
        initializer: &Option<SpanExpr>,
    ) -> Result<()> {
        trace!("Declaring variable {}", name.value);

        let value = match initializer {
            Some(expr) => self.evaluate_expression(expr)?.value,
            None => LoxObject::Nil,
        };

        trace!("Defining variable {} = {}", name.value, value.repr());

        self.current_env
            .borrow_mut()
            .define(name.value.clone(), value);
        Ok(())
    }

    /// Execute a free function declaration in the current environment.
    #[instrument(skip_all)]
    fn execute_fun_decl(
        &mut self,
        name: &WithSpan<String>,
        parameters: &[WithSpan<String>],
        body: &[SpanStmt],
    ) {
        trace!(
            "Declaring free function {} with {} parameters",
            name.value,
            parameters.len()
        );

        let function = LoxFunction::new(
            name.clone(),
            parameters.to_owned(),
            body.to_owned(),
            self.get_current_env(),
            false,
        );
        self.current_env.borrow_mut().define(
            name.value.clone(),
            LoxObject::LoxFunction(Rc::new(function)),
        );
    }

    /// Execute an `if` statement.
    #[instrument(skip_all)]
    fn execute_if_statement(
        &mut self,
        condition: &SpanExpr,
        then_branch: &SpanStmt,
        else_branch: &Option<Box<SpanStmt>>,
    ) -> Result<()> {
        if self.evaluate_expression(condition)?.is_truthy() {
            self.execute_statement(then_branch)?;
        } else if let Some(else_branch) = else_branch {
            self.execute_statement(else_branch)?;
        }

        Ok(())
    }

    /// Execute a print statement.
    #[instrument(skip_all)]
    fn execute_print(&mut self, expr: &SpanExpr) -> Result<()> {
        println!("{}", self.evaluate_expression(expr)?.print());
        Ok(())
    }

    /// Execute a return statement.
    #[instrument(skip_all)]
    fn execute_return(&mut self, keyword_span: &Span, expr: &Option<SpanExpr>) -> Result<()> {
        let value = if let Some(expr) = expr {
            self.evaluate_expression(expr)?
        } else {
            WithSpan {
                span: *keyword_span,
                value: LoxObject::Nil,
            }
        };

        trace!("Returning {} from function", value.value.repr());

        Err(ErrorOrReturn::Return(value))
    }

    /// Execute a while loop.
    #[instrument(skip_all)]
    fn execute_while_loop(&mut self, condition: &SpanExpr, body: &SpanStmt) -> Result<()> {
        while self.evaluate_expression(condition)?.is_truthy() {
            self.execute_statement(body)?;
        }

        Ok(())
    }

    /// Evaluate the given expression.
    #[instrument(skip_all)]
    fn evaluate_expression(&mut self, expr: &SpanExpr) -> Result<SpanObject> {
        trace_evaluating!(expr);

        let WithSpan {
            span,
            value: expr_value,
        } = expr;
        let span = *span;

        let result = match expr_value {
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
                self.evaluate_binary_expression(*operator, left, right, expr)?
            }
            Expr::Call(callee, arguments, close_paren) => {
                self.evaluate_function_call(callee, arguments, close_paren, expr)?
            }
            Expr::Get(object_expr, ident) => self.execute_get_expr(object_expr, ident, span)?,
            Expr::Set(object_expr, ident, r_value_expr) => WithSpan {
                span,
                value: self.execute_set_expr(object_expr, ident, r_value_expr)?,
            },
            Expr::Super(super_keyword_span, method_name) => WithSpan {
                span,
                value: LoxObject::LoxFunction(self.evaluate_super_access(
                    *super_keyword_span,
                    span,
                    &method_name.value,
                )?),
            },
            Expr::This => WithSpan {
                span,
                value: self.look_up_name(&WithSpan {
                    span,
                    value: String::from("this"),
                })?,
            },
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
                self.evaluate_logical_expression(left, operator, right, expr)?
            }
            Expr::Unary(operator, r_expr) => {
                let value = self.evaluate_expression(r_expr)?;
                self.evaluate_unary_expression(*operator, value, expr)?
            }
            Expr::Variable(name) => WithSpan {
                span,
                value: self.look_up_name(&WithSpan {
                    span,
                    value: name.clone(),
                })?,
            },
            Expr::Assign(name, expr) => self.execute_assign_expr(name, expr, span)?,
        };

        trace_evaluated!(expr, result);
        Ok(result)
    }

    /// Execute a get expression to get a property on an instance.
    #[instrument(skip_all)]
    fn execute_get_expr(
        &mut self,
        object_expr: &SpanExpr,
        ident: &WithSpan<String>,
        span: Span,
    ) -> Result<SpanObject> {
        let object = self.evaluate_expression(object_expr)?;
        trace!(
            "Trying to get property `{}` on {}",
            ident.value,
            object.value.repr()
        );

        if let LoxObject::LoxInstance(instance) = object.value {
            Ok(WithSpan {
                span,
                value: LoxInstance::get(&instance, ident)?,
            })
        } else {
            Err(RuntimeError {
                message: "Only class instances have properties".to_string(),
                span,
            }
            .into())
        }
    }

    /// Execute a set expression to set a property on an instance.
    #[instrument(skip_all)]
    fn execute_set_expr(
        &mut self,
        object_expr: &SpanExpr,
        ident: &WithSpan<String>,
        r_value_expr: &SpanExpr,
    ) -> Result<LoxObject> {
        let object = self.evaluate_expression(object_expr)?;
        trace!(
            "Trying to set property `{}` on {}",
            ident.value,
            object.value.repr()
        );

        if let WithSpan {
            span: _,
            value: LoxObject::LoxInstance(ref instance),
        } = object
        {
            let r_value = self.evaluate_expression(r_value_expr)?;
            trace!(
                "Setting property `{}` on {} to {}",
                ident.value,
                object.value.repr(),
                r_value.value.repr()
            );

            instance
                .borrow_mut()
                .set(ident.value.clone(), r_value.value.clone());
            Ok(r_value.value)
        } else {
            Err(RuntimeError {
                message: format!(
                    "Only class instances have fields, not objects of type {}",
                    object.value.type_name()
                ),
                span: object.span.union(&ident.span),
            }
            .into())
        }
    }

    /// Evaluate an access of a method on `super`.
    #[instrument(skip_all)]
    fn evaluate_super_access(
        &mut self,
        super_keyword_span: Span,
        full_span: Span,
        method_name: &str,
    ) -> Result<Rc<LoxFunction>> {
        trace!("Trying to get method {method_name} on `super`");
        let super_keyword = WithSpan {
            span: super_keyword_span,
            value: String::from("super"),
        };

        let distance = *self
            .locals
            .get(&super_keyword)
            .expect("`super` should be defined");

        let LoxObject::LoxClass(superclass) =
            Environment::get_at_depth(&self.current_env, distance, &super_keyword)
        else {
            panic!("Finding `super` yielded an object which was not a class");
        };

        let LoxObject::LoxInstance(object) = Environment::get_at_depth(
            &self.current_env,
            distance - 1,
            &WithSpan {
                span: full_span,
                value: String::from("this"),
            },
        ) else {
            panic!("Finding `this` yielded an object which was not an instance");
        };

        match superclass.find_method(method_name) {
            None => Err(ErrorOrReturn::Error(RuntimeError {
                message: format!("Undefined property `{method_name}` on superclass"),
                span: full_span,
            })),
            Some(method) => Ok(method.bind_this(LoxObject::LoxInstance(object))),
        }
    }

    /// Execute an assignment expression.
    #[instrument(skip_all)]
    fn execute_assign_expr(
        &mut self,
        name: &WithSpan<String>,
        expr: &SpanExpr,
        span: Span,
    ) -> Result<SpanObject> {
        let value = self.evaluate_expression(expr)?;

        trace!("Assigning variable {} = {}", name.value, value.repr());

        match self.locals.get(name) {
            Some(depth) => {
                Environment::assign_at_depth(&self.current_env, *depth, name, value.value.clone())
            }
            None => self
                .global_env
                .borrow_mut()
                .assign(name, value.value.clone(), span)?,
        };

        Ok(value)
    }

    /// Look up the name to resolve it in the [`locals`](Self.locals) map or in the global scope.
    #[instrument(skip_all)]
    fn look_up_name(&self, name: &WithSpan<String>) -> Result<LoxObject> {
        trace!("Looking up name `{}`", name.value);

        Ok(match self.locals.get(name) {
            Some(depth) => Environment::get_at_depth(&self.current_env, *depth, name),
            None => self.global_env.borrow().get(name)?,
        })
    }

    /// Evaluate a function call with the given callee and arguments.
    #[instrument(skip_all)]
    fn evaluate_function_call(
        &mut self,
        callee: &SpanExpr,
        arguments: &[SpanExpr],
        close_paren: &Span,
        expr: &SpanExpr,
    ) -> Result<SpanObject> {
        trace_evaluating!(expr);

        let callee = self.evaluate_expression(callee)?;
        let callee_span = callee.span;

        let arguments: Vec<SpanObject> = arguments
            .iter()
            .map(|expr| self.evaluate_expression(expr))
            .collect::<Result<Vec<SpanObject>>>()?;

        let func = self.try_get_function(callee, close_paren)?;

        if arguments.len() != func.arity() as usize {
            return Err(func
                .bad_arity_error(callee_span, &arguments, *close_paren)
                .into());
        }

        let value = func.call(self, callee_span, &arguments, *close_paren)?;

        trace_evaluated!(expr, value);

        Ok(WithSpan {
            span: callee_span.union(close_paren),
            value,
        })
    }

    /// Try to resolve a function from an object.
    #[instrument(skip_all)]
    fn try_get_function(
        &self,
        callee: SpanObject,
        close_paren: &Span,
    ) -> Result<Rc<dyn LoxCallable>> {
        trace!("Trying to resolve `{}` as a function", callee.value.repr());

        match &callee.value {
            LoxObject::NativeFunction(func) => Ok(Rc::clone(func)),
            LoxObject::LoxFunction(func) => Ok(Rc::clone(func) as Rc<dyn LoxCallable>),
            LoxObject::LoxClass(class) => Ok(Rc::new(Rc::clone(class)) as Rc<dyn LoxCallable>),
            _ => Err(RuntimeError {
                message: format!(
                    "Can only call objects of type function or class, not {}",
                    callee.value.type_name()
                ),
                span: callee.span.union(close_paren),
            }
            .into()),
        }
    }

    /// Evaluate a logical expression by short-circuiting.
    #[instrument(skip_all)]
    fn evaluate_logical_expression(
        &mut self,
        left: &SpanExpr,
        operator: &WithSpan<LogicalOperator>,
        right: &SpanExpr,
        expr: &SpanExpr,
    ) -> Result<SpanObject> {
        trace_evaluating!(expr);

        let span = left.span.union(&right.span);
        let left = self.evaluate_expression(left)?;

        let result = match operator.value {
            LogicalOperator::Or if left.value.is_truthy() => WithSpan { span, ..left },
            LogicalOperator::And if !left.value.is_truthy() => WithSpan { span, ..left },
            _ => {
                let right = self.evaluate_expression(right)?;
                WithSpan { span, ..right }
            }
        };

        trace_evaluated!(expr, result);
        Ok(result)
    }

    /// Evaluate a binary expression.
    #[instrument(skip_all)]
    fn evaluate_binary_expression(
        &mut self,
        operator: WithSpan<BinaryOperator>,
        left: SpanObject,
        right: SpanObject,
        expr: &SpanExpr,
    ) -> Result<SpanObject> {
        use BinaryOperator::*;
        use LoxObject::*;

        trace_evaluating!(expr);

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
                            span,
                            message: "Division by 0".to_string(),
                        }
                        .into());
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
            (NativeFunction(a), NativeFunction(b)) => match operator {
                EqualEqual => Boolean(Rc::ptr_eq(a, b)),
                BangEqual => Boolean(!Rc::ptr_eq(a, b)),
                _ => unsupported()?,
            },
            (LoxFunction(a), LoxFunction(b)) => match operator {
                EqualEqual => Boolean(a == b),
                BangEqual => Boolean(a != b),
                _ => unsupported()?,
            },
            (LoxInstance(a), LoxInstance(b)) => match operator {
                EqualEqual => Boolean(*a.borrow() == *b.borrow()),
                BangEqual => Boolean(*a.borrow() != *b.borrow()),
                _ => unsupported()?,
            },
            // Guaranteed to be of different types
            _ => match operator {
                EqualEqual => Boolean(left == right),
                BangEqual => Boolean(left != right),
                _ => unsupported()?,
            },
        };

        trace_evaluated!(expr, value);
        Ok(WithSpan { span, value })
    }

    /// Evaluate a unary expression.
    #[instrument(skip_all)]
    fn evaluate_unary_expression(
        &mut self,
        operator: WithSpan<UnaryOperator>,
        object: SpanObject,
        expr: &SpanExpr,
    ) -> Result<SpanObject> {
        use LoxObject::*;
        use UnaryOperator::*;

        trace_evaluating!(expr);

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

        trace_evaluated!(expr, value);
        Ok(WithSpan { span, value })
    }
}
