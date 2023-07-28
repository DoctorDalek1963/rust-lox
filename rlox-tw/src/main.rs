//! This crate contains a tree-walk interpreter for Lox, as described in
//! <https://craftinginterpreters.com/a-tree-walk-interpreter.html>.

pub(crate) mod lox;
pub(crate) mod scanner;
pub(crate) mod tokens;

use color_eyre::Result;
use std::env::args;
use tracing_subscriber::{filter::LevelFilter, fmt::Layer, prelude::*, EnvFilter};

/// Run the interpreter, taking a source file as the first CLI argument.
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

    Ok(())
}
