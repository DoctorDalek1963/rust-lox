//! This module acts as a top-level entrypoint to evaluating Lox code.

use crate::{
    parser::Parser,
    pretty_printers::{ParenPrinter, RpnPrinter},
    scanner::Scanner,
    tokens::{Token, TokenType},
};
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
    fs, io,
    path::Path,
    sync::atomic::{AtomicBool, Ordering},
};
use thiserror::Error;
use tracing::{debug, instrument};

/// Have we encountered at least one error yet?
static HAD_ERROR: AtomicBool = AtomicBool::new(false);

/// The Lox interpreter.
#[derive(Clone, Copy, Debug)]
pub struct LoxInterpreter;

/// An error that can be returned from [`LoxInterpreter::run_prompt`].
#[derive(Debug, Error)]
pub enum PromptError {
    /// An error from `rustyline`.
    #[error("rustyline error: `{0:?}`")]
    Readline(#[from] ReadlineError),

    /// A standard I/O error.
    #[error("I/O error: `{0:?}`")]
    Io(#[from] io::Error),
}

impl LoxInterpreter {
    /// Read the file and run the contents.
    pub fn run_file(&mut self, path: impl AsRef<Path>) -> io::Result<()> {
        self.run_code(&fs::read_to_string(path)?);

        if HAD_ERROR.load(Ordering::Relaxed) {
            eprintln!("TODO: Report error properly and return Err()");
        }

        Ok(())
    }

    /// Read code from an interactive prompt and run it.
    pub fn run_prompt(&mut self) -> Result<(), PromptError> {
        let mut prompt = DefaultEditor::new()?;

        loop {
            match prompt.readline("> ") {
                Ok(mut line) => {
                    prompt.add_history_entry(&line)?;
                    line.push('\n');
                    self.run_code(&line);

                    if HAD_ERROR.load(Ordering::Relaxed) {
                        eprintln!("ERROR: Bad input, please try again");
                    }
                }
                Err(ReadlineError::Eof | ReadlineError::Interrupted) => return Ok(()),
                Err(ReadlineError::Io(e)) => return Err(e)?,
                Err(error) => panic!("Unknown error: `{error:?}`"),
            }
            HAD_ERROR.store(false, Ordering::Relaxed);
        }
    }

    /// Run the given Lox code.
    #[instrument(skip_all)]
    fn run_code(&mut self, code: &str) {
        debug!(?code);

        let tokens = Scanner::scan_tokens(code);
        let expr = match Parser::parse(tokens) {
            Some(expr) => expr,
            None => return,
        };

        println!("{}", ParenPrinter::print(&expr));
        println!("{}", RpnPrinter::print(&expr));
    }
}

/// Report the error with the given details.
pub fn report_error(line: usize, col_start: usize, col_end: usize, message: &str) {
    eprintln!("[line {line}, cols {col_start}:{col_end}] ERROR: {message}");
    HAD_ERROR.store(true, Ordering::Relaxed);
}

/// Report an error at the given token with the given message.
pub fn report_token_error(token: &Token<'_>, message: &str) {
    let string = if token.token_type == TokenType::Eof {
        format!("at end {message}")
    } else {
        format!("at '{}' {message}", token.lexeme)
    };

    report_error(
        token.line,
        token.col_start,
        token.col_start + token.length,
        &string,
    )
}
