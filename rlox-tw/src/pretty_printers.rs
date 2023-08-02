//! This module provides pretty-printers for the AST.

use crate::ast::{Expr, SpanExpr};

/// Pretty-print the AST with clarifying parentheses.
pub struct ParenPrinter;

impl ParenPrinter {
    /// Print a version of the given expression with extra parentheses.
    pub fn print(expr: &SpanExpr) -> String {
        match &expr.value {
            Expr::Nil => "nil".to_string(),
            Expr::Boolean(boolean) => boolean.to_string(),
            Expr::Binary(left, operator, right) => {
                format!(
                    "({} {} {})",
                    Self::print(left),
                    operator.value,
                    Self::print(right)
                )
            }
            Expr::Grouping(expr) => format!("({})", Self::print(expr)),
            Expr::String(string) => format!("{string:?}"),
            Expr::Number(number) => number.to_string(),
            Expr::Unary(operator, expr) => format!("({}{})", operator.value, Self::print(expr)),
        }
    }
}

/// Pretty-print the AST in RPN.
pub struct RpnPrinter;

impl RpnPrinter {
    /// Print the RPN version of the given expression.
    pub fn print(expr: &SpanExpr) -> String {
        match &expr.value {
            Expr::Nil => "nil".to_string(),
            Expr::Boolean(boolean) => boolean.to_string(),
            Expr::Binary(left, operator, right) => {
                format!(
                    "{} {} {}",
                    Self::print(left),
                    Self::print(right),
                    operator.value
                )
            }
            Expr::Grouping(expr) => format!("({})", Self::print(expr)),
            Expr::String(string) => format!("{string:?}"),
            Expr::Number(number) => number.to_string(),
            Expr::Unary(operator, expr) => format!("{} {}", operator.value, Self::print(expr)),
        }
    }
}
