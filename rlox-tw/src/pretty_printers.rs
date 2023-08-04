//! This module provides pretty-printers for the AST.

use crate::ast::{Expr, SpanExpr, SpanStmt, Stmt};

/// Pretty-print the AST with clarifying parentheses.
pub struct ParenPrinter;

impl ParenPrinter {
    /// Print the statements with extra parentheses.
    pub fn print_stmts(stmts: &[SpanStmt]) -> String {
        stmts
            .iter()
            .map(|stmt| match &stmt.value {
                Stmt::Expression(expr) => format!("{};", Self::print_expr(expr)),
                Stmt::Print(expr) => format!("print {};", Self::print_expr(expr)),
                Stmt::VarDecl(name, initializer) => format!(
                    "var {}{};",
                    name.value,
                    if let Some(expr) = initializer {
                        format!(" = {}", Self::print_expr(&expr))
                    } else {
                        String::new()
                    }
                ),
            })
            .intersperse("\n".to_string())
            .collect()
    }

    /// Print a version of the given expression with extra parentheses.
    pub fn print_expr(expr: &SpanExpr) -> String {
        match &expr.value {
            Expr::Nil => "nil".to_string(),
            Expr::Boolean(boolean) => boolean.to_string(),
            Expr::Binary(left, operator, right) => {
                format!(
                    "({} {} {})",
                    Self::print_expr(left),
                    operator.value,
                    Self::print_expr(right)
                )
            }
            Expr::Grouping(expr) => format!("({})", Self::print_expr(expr)),
            Expr::String(string) => format!("{string:?}"),
            Expr::Number(number) => number.to_string(),
            Expr::Unary(operator, expr) => {
                format!("({}{})", operator.value, Self::print_expr(expr))
            }
            Expr::Variable(name) => name.to_string(),
            Expr::Assign(name, expr) => format!("{name} = {}", Self::print_expr(expr)),
        }
    }
}

/// Pretty-print the AST in RPN.
pub struct RpnPrinter;

impl RpnPrinter {
    /// Print the statements in RPN.
    pub fn print_stmts(stmts: &[SpanStmt]) -> String {
        stmts
            .iter()
            .map(|stmt| match &stmt.value {
                Stmt::Expression(expr) => format!("{} ;", Self::print_expr(expr)),
                Stmt::Print(expr) => format!("{} print ;", Self::print_expr(expr)),
                Stmt::VarDecl(name, initializer) => format!(
                    "{} var{} ;",
                    name.value,
                    if let Some(expr) = initializer {
                        format!(" {} =", Self::print_expr(&expr))
                    } else {
                        String::new()
                    }
                ),
            })
            .intersperse("\n".to_string())
            .collect()
    }

    /// Print the RPN version of the given expression.
    pub fn print_expr(expr: &SpanExpr) -> String {
        match &expr.value {
            Expr::Nil => "nil".to_string(),
            Expr::Boolean(boolean) => boolean.to_string(),
            Expr::Binary(left, operator, right) => {
                format!(
                    "{} {} {}",
                    Self::print_expr(left),
                    Self::print_expr(right),
                    operator.value
                )
            }
            Expr::Grouping(expr) => format!("({})", Self::print_expr(expr)),
            Expr::String(string) => format!("{string:?}"),
            Expr::Number(number) => number.to_string(),
            Expr::Unary(operator, expr) => format!("{} {}", Self::print_expr(expr), operator.value),
            Expr::Variable(name) => name.to_string(),
            Expr::Assign(name, expr) => format!("{name} {} =", Self::print_expr(expr)),
        }
    }
}
