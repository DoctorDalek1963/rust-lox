//! This crate contains a tree-walk interpreter for Lox, as described in
//! <https://craftinginterpreters.com/a-tree-walk-interpreter.html>.

#![feature(box_patterns)]
#![feature(file_create_new)]
#![feature(fs_try_exists)]
#![feature(iter_intersperse)]

pub mod ast;
pub mod callable;
pub mod environment;
pub mod interpreter;
pub mod lox;
pub mod object;
pub mod parser;
pub mod pretty_printers;
pub mod scanner;
pub mod span;
pub mod tokens;

use color_eyre::Result;
use std::env::args;
use tracing_subscriber::{filter::LevelFilter, fmt::Layer, prelude::*, EnvFilter};

pub use self::interpreter::Interpreter;

/// Run the interpreter, taking a source file as the first CLI argument, or running the REPL if no
/// file was given.
pub fn run_interpreter<T: Interpreter>() -> Result<()> {
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

    let mut interpreter = lox::LoxInterpreter::<T>::new();

    match args().nth(1) {
        Some(path) => interpreter.run_file(path)?,
        None => interpreter.run_prompt()?,
    }

    Ok(())
}
