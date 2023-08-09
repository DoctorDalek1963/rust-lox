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
#[allow(clippy::missing_docs_in_private_items)]
pub enum Expr {
    Nil,
    Boolean(bool),
    Binary(Box<SpanExpr>, WithSpan<BinaryOperator>, Box<SpanExpr>),
    Call(Box<SpanExpr>, Vec<SpanExpr>, Span),
    Grouping(Box<SpanExpr>),
    String(String),
    Number(f64),
    Logical(Box<SpanExpr>, WithSpan<LogicalOperator>, Box<SpanExpr>),
    Unary(WithSpan<UnaryOperator>, Box<SpanExpr>),
    Variable(String),
    Assign(String, Box<SpanExpr>),
}

/// An [`Stmt`] wrapped in [`WithSpan`].
pub type SpanStmt = WithSpan<Stmt>;

/// A list of all the possible statements.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum Stmt {
    VarDecl(WithSpan<String>, Option<SpanExpr>),
    FunDecl(WithSpan<String>, Vec<WithSpan<String>>, Vec<SpanStmt>),
    Expression(SpanExpr),
    If(SpanExpr, Box<SpanStmt>, Option<Box<SpanStmt>>),
    Print(SpanExpr),
    While(SpanExpr, Box<SpanStmt>),
    Block(Vec<SpanStmt>),
}
