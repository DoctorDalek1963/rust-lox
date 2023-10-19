//! This module handles the AST.

use crate::span::{Span, WithSpan};
use std::fmt;

/// A binary operator - includes arithmetic and comparison.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum BinaryOperator {
    Slash,
    Star,
    Plus,
    Minus,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BangEqual,
    EqualEqual,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Slash => "/",
                Self::Star => "*",
                Self::Plus => "+",
                Self::Minus => "-",
                Self::Greater => ">",
                Self::GreaterEqual => ">=",
                Self::Less => "<",
                Self::LessEqual => "<=",
                Self::BangEqual => "!=",
                Self::EqualEqual => "==",
            }
        )
    }
}

/// A binary logic operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum LogicalOperator {
    And,
    Or,
}

impl fmt::Display for LogicalOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::And => "and",
                Self::Or => "or",
            }
        )
    }
}

/// A unary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bang => "!",
                Self::Minus => "-",
            }
        )
    }
}

/// An [`Expr`] wrapped in [`WithSpan`].
pub type SpanExpr = WithSpan<Expr>;

/// A list of all the possible expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    /// Nothing.
    Nil,

    /// A boolean.
    Boolean(bool),

    /// (left_expr, operator, right_expr).
    Binary(Box<SpanExpr>, WithSpan<BinaryOperator>, Box<SpanExpr>),

    /// (callee, arguments, close_paren_span).
    Call(Box<SpanExpr>, Vec<SpanExpr>, Span),

    /// (object, identifier).
    Get(Box<SpanExpr>, WithSpan<String>),

    /// (object, identifier, new_value).
    Set(Box<SpanExpr>, WithSpan<String>, Box<SpanExpr>),

    /// `this`.
    This,

    /// Parens around expression.
    Grouping(Box<SpanExpr>),

    /// A string.
    String(String),

    /// A number.
    Number(f64),

    /// (left_expr, operator, right_expr).
    Logical(Box<SpanExpr>, WithSpan<LogicalOperator>, Box<SpanExpr>),

    /// (operator, expr).
    Unary(WithSpan<UnaryOperator>, Box<SpanExpr>),

    /// The name of a variable.
    Variable(String),

    /// (identifier, new_value).
    Assign(WithSpan<String>, Box<SpanExpr>),
}

/// A [`Stmt`] wrapped in [`WithSpan`].
pub type SpanStmt = WithSpan<Stmt>;

/// A function or method declaration. (identifier, parameters, right_paren_span, body).
pub type FunctionOrMethod = (WithSpan<String>, Vec<WithSpan<String>>, Span, Vec<SpanStmt>);

/// A list of all the possible statements.
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    /// (class_name, superclass_name, methods).
    ClassDecl(
        WithSpan<String>,
        Option<WithSpan<String>>,
        Vec<WithSpan<FunctionOrMethod>>,
    ),

    /// (identifier, initializer).
    VarDecl(WithSpan<String>, Option<SpanExpr>),

    /// (identifier, parameters, right_paren_span, body).
    FunDecl(FunctionOrMethod),

    /// See [`Expr`].
    Expression(SpanExpr),

    /// (condition, then_block, else_block)
    If(SpanExpr, Box<SpanStmt>, Option<Box<SpanStmt>>),

    /// (expr).
    Print(SpanExpr),

    /// (keyword_span, return_value).
    Return(Span, Option<SpanExpr>),

    /// (condition, body).
    While(SpanExpr, Box<SpanStmt>),

    /// (body).
    Block(Vec<SpanStmt>),
}
