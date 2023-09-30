//! This module provides the [`Resolver`].

use rlox_lib::{
    ast::{Expr, SpanExpr, SpanStmt, Stmt},
    span::{Span, WithSpan},
};
use std::{cmp::Ordering, collections::HashMap, fmt};
use thiserror::Error;

/// An error that occurred whilst resolving.
#[derive(Clone, Debug, PartialEq, Error)]
pub struct ResolveError {
    /// The error message.
    pub message: String,

    /// The span where the error occurred.
    pub span: Span,
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ResolveError({:?})", self.message)
    }
}

/// A result wrapping a [`ResolveError`].
type Result<T = (), E = ResolveError> = ::std::result::Result<T, E>;

/// An enum to determine if the [`Resolver`] is currently in a function. Used to detect badly
/// placed return statements.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum FunctionType {
    /// Not in a function.
    None,

    /// In a free function.
    Function,

    /// In a method on a class.
    Method,
}

/// An enum to distinguish different things that a name could refer to. Used for warning reporting.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum ScopeValueType {
    Class,
    Function,
    Parameter,
    Variable,
}

/// A value for the [`scopes`](Resolver.scopes) map.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct ScopeValue {
    /// Where was the name declared?
    declaration: Span,

    /// What is the type of the name?
    value_type: ScopeValueType,

    /// Has the name been defined?
    defined: bool,

    /// Has the name been used?
    used: bool,
}

impl ScopeValue {
    /// Create a new value for a name that's just been declared.
    fn new(declaration: Span, value_type: ScopeValueType) -> Self {
        Self {
            declaration,
            value_type,
            defined: false,
            used: false,
        }
    }
}

/// A type to handle resolving and binding names before runtime.
#[derive(Clone, Debug)]
pub struct Resolver {
    /// A stack of local scopes.
    ///
    /// The global scope is not included and global variables are never
    /// [declared](Self::declare_name) or [defined](Self::define_name).
    ///
    /// The hashmap maps variable names to whether they've been defined and/or used. When a
    /// variable gets declared, its name is added here, and when it is defined or used, the
    /// appropriate value is set to true.
    scopes: Vec<HashMap<String, ScopeValue>>,

    /// A map from a name to its local environment depth. When resolving that name, go up that many
    /// environments in the chain.
    locals: HashMap<WithSpan<String>, usize>,

    /// The type of function that we're currently inside.
    current_function: FunctionType,
}

impl Resolver {
    /// Resolve the given code and get the map of local names to their depths.
    pub fn get_locals_map(
        stmts: &[SpanStmt],
    ) -> Result<HashMap<WithSpan<String>, usize>, ResolveError> {
        let mut resolver = Self::new();
        match resolver.resolve_stmts(stmts) {
            Ok(()) => Ok(resolver.locals),
            Err(error) => Err(error),
        }
    }

    /// Create a new Resolver.
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            locals: HashMap::new(),
            current_function: FunctionType::None,
        }
    }

    /// Resolve a list of statements.
    fn resolve_stmts(&mut self, stmts: &[SpanStmt]) -> Result {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    /// Resolve a single statement.
    fn resolve_stmt(&mut self, stmt: &SpanStmt) -> Result {
        match &stmt.value {
            Stmt::Block(body) => {
                self.begin_scope();
                self.resolve_stmts(body)?;
                self.end_scope();
            }
            Stmt::ClassDecl(name, methods) => {
                self.declare_name(name.clone(), stmt.span, ScopeValueType::Class)?;
                self.define_name(&name.value);

                self.begin_scope();
                if let Some(scope) = self.scopes.last_mut() {
                    let ret = scope.insert(
                        String::from("this"),
                        ScopeValue {
                            declaration: name.span,
                            value_type: ScopeValueType::Variable,
                            defined: true,
                            used: true,
                        },
                    );
                    assert!(
                        ret.is_none(),
                        "`this` should not exist in the new scope when declaring a class"
                    );
                }

                for method in methods {
                    let WithSpan {
                        span: _,
                        value: (_method_name, params, _close_paren_span, body),
                    } = method;
                    let declaration = FunctionType::Method;
                    self.resolve_function(params, body, declaration)?;
                }

                self.end_scope();
            }
            Stmt::VarDecl(name, initializer) => {
                self.declare_name(name.clone(), stmt.span, ScopeValueType::Variable)?;
                if let Some(initializer) = initializer {
                    self.resolve_expr(initializer)?;
                }
                self.define_name(&name);
            }
            Stmt::FunDecl((name, params, right_paren, body)) => {
                self.declare_name(
                    name.clone(),
                    Span::between(&stmt.span, &right_paren),
                    ScopeValueType::Function,
                )?;
                self.define_name(&name.value);
                self.resolve_function(params, body, FunctionType::Function)?;
            }
            Stmt::Expression(expr) => self.resolve_expr(expr)?,
            Stmt::If(condition, then_branch, else_branch) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::Print(expr) => self.resolve_expr(expr)?,
            Stmt::Return(_, expr) => {
                if self.current_function == FunctionType::None {
                    return Err(ResolveError {
                        message: "Cannot return outside of a function".to_string(),
                        span: stmt.span,
                    });
                }

                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
            }
            Stmt::While(condition, body) => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
            }
        }

        Ok(())
    }

    /// Resolve a single expression.
    fn resolve_expr(&mut self, expr: &SpanExpr) -> Result {
        match &expr.value {
            Expr::Variable(name) => {
                if self.scopes.last().is_some_and(|scope| {
                    scope.get(name).is_some_and(|value| value.defined == false)
                }) {
                    return Err(ResolveError {
                        message: "Cannot read local variable in its own initializer".to_string(),
                        span: expr.span,
                    });
                }

                self.resolve_local(WithSpan {
                    span: expr.span,
                    value: name.clone(),
                });
            }
            Expr::Assign(name, value) => {
                self.resolve_expr(value)?;
                self.resolve_local(name.clone());
            }
            Expr::Binary(left, _, right) | Expr::Logical(left, _, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call(callee, arguments, _) => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Get(expr, _) => self.resolve_expr(expr)?,
            Expr::Set(object, _, value) => {
                self.resolve_expr(value)?;
                self.resolve_expr(object)?;
            }
            Expr::This => self.resolve_local(WithSpan {
                span: expr.span,
                value: String::from("this"),
            }),
            Expr::Grouping(expr) | Expr::Unary(_, expr) => self.resolve_expr(expr)?,
            Expr::Nil | Expr::Boolean(_) | Expr::String(_) | Expr::Number(_) => (),
        }

        Ok(())
    }

    /// Begin a new local scope.
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// End a local scope.
    fn end_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            report_warnings(scope);
        }
    }

    /// Declare the given name to exist in the current scope, but not yet be defined.
    fn declare_name(
        &mut self,
        name: WithSpan<String>,
        declaration: Span,
        value_type: ScopeValueType,
    ) -> Result {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.value) {
                return Err(ResolveError {
                    message: format!(
                        "Already declared variable '{}' in this local scope",
                        &name.value
                    ),
                    span: name.span,
                });
            }
            scope.insert(name.value, ScopeValue::new(declaration, value_type));
        }
        Ok(())
    }

    /// Define the given name in the current scope, setting its value to true.
    fn define_name(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            let x = scope.get_mut(name).expect(
                "We should only call define_name() after calling declare_name() with the same name",
            );
            x.defined = true;
        }
    }

    /// Resolve a name in a local scope by traversing up the scope tree to find the definition of
    /// the name, and add it [`self.locals`](Self.locals).
    fn resolve_local(&mut self, name: WithSpan<String>) {
        let num = self.scopes.len().saturating_sub(1);

        for (idx, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(scope_value) = scope.get_mut(&name.value) {
                scope_value.used = true;
                self.locals.insert(name, num.saturating_sub(idx));
                return;
            }
        }
    }

    /// Resolve a function declaration.
    fn resolve_function(
        &mut self,
        params: &[WithSpan<String>],
        body: &[SpanStmt],
        function_type: FunctionType,
    ) -> Result {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();

        for param in params {
            self.declare_name(param.clone(), param.span, ScopeValueType::Parameter)?;
            self.define_name(&param.value);
        }
        self.resolve_stmts(body)?;

        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }
}

/// Report warnings for unused names.
fn report_warnings(scope: HashMap<String, ScopeValue>) {
    let mut names: Vec<_> = scope
        .into_iter()
        .filter_map(
            |(
                name,
                ScopeValue {
                    declaration,
                    value_type,
                    defined: _,
                    used,
                },
            )| { (!used).then_some((declaration, value_type, name)) },
        )
        .collect();

    names.sort_by(
        |(_, l_type, l_name), (_, r_type, r_name)| match l_type.cmp(r_type) {
            Ordering::Equal => l_name.cmp(&r_name),
            other => other,
        },
    );

    for (span, value_type, name) in names {
        rlox_lib::lox::report_warning(
            span,
            &format!(
                "{} '{name}' is never used",
                match value_type {
                    ScopeValueType::Class => "Class",
                    ScopeValueType::Function => "Local function",
                    ScopeValueType::Parameter => "Parameter",
                    ScopeValueType::Variable => "Local variable",
                }
            ),
        );
    }
}
