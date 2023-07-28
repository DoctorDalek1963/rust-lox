//! This module handles the AST.

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

/// A list of all the possible expressions.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Nil,
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
    String(String),
    Number(f64),
    Unary(UnaryOperator, Box<Expr>),
}
