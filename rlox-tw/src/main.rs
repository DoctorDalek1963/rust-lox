//! This crate contains a tree-walk interpreter for Lox, as described in
//! <https://craftinginterpreters.com/a-tree-walk-interpreter.html>.

pub(crate) mod ast;
pub(crate) mod lox;
pub(crate) mod pretty_printers;
pub(crate) mod scanner;
pub(crate) mod tokens;

use crate::pretty_printers::{LispPrinter, RpnPrinter};
use ast::{BinaryOperator, Expr, UnaryOperator};
use color_eyre::Result;
use std::env::args;
use tracing_subscriber::{filter::LevelFilter, fmt::Layer, prelude::*, EnvFilter};

/// Run the interpreter, taking a source file as the first CLI argument, or running the REPL if no
/// file was given.
fn main() -> Result<()> {
    color_eyre::install()?;

    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(
            Layer::new().with_filter(
                EnvFilter::builder()
                    .with_default_directive(LevelFilter::WARN.into())
                    .from_env_lossy(),
            ),
        ),
    )?;

    let mut interpreter = lox::LoxInterpreter;

    match args().nth(1) {
        Some(path) => interpreter.run_file(path)?,
        None => interpreter.run_prompt()?,
    }

    let expr = Expr::Binary(
        Box::new(Expr::Unary(
            UnaryOperator::Minus,
            Box::new(Expr::Binary(
                Box::new(Expr::Number(120.)),
                BinaryOperator::Plus,
                Box::new(Expr::String("3".into())),
            )),
        )),
        BinaryOperator::Star,
        Box::new(Expr::Grouping(Box::new(Expr::Nil))),
    );

    println!("{}", LispPrinter::print(&expr));
    println!("{}", RpnPrinter::print(&expr));

    Ok(())
}
