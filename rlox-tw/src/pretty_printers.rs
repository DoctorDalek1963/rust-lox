//! This module provides pretty-printers for the AST.

use crate::ast::Expr;

/// Pretty-print the AST with clarifying parentheses.
pub struct ParenPrinter;

impl ParenPrinter {
    /// Print a version of the given expression with extra parentheses.
    pub fn print(expr: &Expr) -> String {
        match expr {
            Expr::Nil => "nil".to_string(),
            Expr::Boolean(boolean) => boolean.to_string(),
            Expr::Binary(left, operator, right) => {
                format!("({} {operator} {})", Self::print(left), Self::print(right))
            }
            Expr::Grouping(expr) => format!("({})", Self::print(expr)),
            Expr::String(string) => format!("{string:?}"),
            Expr::Number(number) => number.to_string(),
            Expr::Unary(operator, expr) => format!("({operator}{})", Self::print(expr)),
        }
    }
}

/// Pretty-print the AST in RPN.
pub struct RpnPrinter;

impl RpnPrinter {
    /// Print the RPN version of the given expression.
    pub fn print(expr: &Expr) -> String {
        match expr {
            Expr::Nil => "nil".to_string(),
            Expr::Boolean(boolean) => boolean.to_string(),
            Expr::Binary(left, operator, right) => {
                format!("{} {} {operator}", Self::print(left), Self::print(right))
            }
            Expr::Grouping(expr) => format!("({})", Self::print(expr)),
            Expr::String(string) => format!("{string:?}"),
            Expr::Number(number) => number.to_string(),
            Expr::Unary(operator, expr) => format!("{} {operator}", Self::print(expr)),
        }
    }
}
