//! This module acts as a top-level entrypoint to evaluating Lox code.

use crate::{
    interpreter::TwInterpreter,
    parser::Parser,
    pretty_printers::{ParenPrinter, RpnPrinter},
    scanner::Scanner,
    span::{LineOffsets, Span},
    tokens::{Token, TokenType},
};
use lazy_static::lazy_static;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
    fs, io,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        RwLock,
    },
};
use thiserror::Error;
use tracing::{debug, instrument};

/// Have we encountered at least one error before runtime?
static HAD_NON_RUNTIME_ERROR: AtomicBool = AtomicBool::new(false);

/// Have we encountered at least one error at runtime?
static HAD_RUNTIME_ERROR: AtomicBool = AtomicBool::new(false);

lazy_static! {
    /// The LineOffsets of the code being worked with.
    pub(crate) static ref LINE_OFFSETS: RwLock<LineOffsets> = RwLock::new(LineOffsets::new(""));
}

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

        if HAD_NON_RUNTIME_ERROR.load(Ordering::Relaxed) {
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

                    if HAD_NON_RUNTIME_ERROR.load(Ordering::Relaxed) {
                        print_error_message(None, "Bad input, please try again");
                    }
                }
                Err(ReadlineError::Eof | ReadlineError::Interrupted) => return Ok(()),
                Err(ReadlineError::Io(e)) => return Err(e)?,
                Err(error) => panic!("Unknown error: `{error:?}`"),
            }
            HAD_NON_RUNTIME_ERROR.store(false, Ordering::Relaxed);
            HAD_RUNTIME_ERROR.store(false, Ordering::Relaxed);
        }
    }

    /// Run the given Lox code.
    #[instrument(skip_all)]
    fn run_code(&mut self, code: &str) {
        debug!(?code);

        let tokens = Scanner::scan_tokens(code);

        debug!(?tokens);
        let expr = match Parser::parse(tokens) {
            Some(expr) => expr,
            None => return,
        };

        debug!(?expr);
        debug!(parens = ParenPrinter::print(&expr));
        debug!(rpn = RpnPrinter::print(&expr));

        if let Some(output) = TwInterpreter::interpret(&expr) {
            println!("{}", output.value);
        }
    }
}

/// Report an error at the given token with the given message.
pub fn report_token_error(token: &Token<'_>, message: &str) {
    let string = if token.token_type == TokenType::Eof {
        format!("at end: {message}")
    } else {
        format!("at '{}': {message}", token.lexeme)
    };

    report_error(token.span, &string);
    HAD_NON_RUNTIME_ERROR.store(true, Ordering::Relaxed);
}

/// Report an error during the scanning of source code.
pub fn report_scanning_error(span: Span, message: &str) {
    report_error(span, message);
    HAD_NON_RUNTIME_ERROR.store(true, Ordering::Relaxed);
}

pub fn report_runtime_error(span: Span, message: &str) {
    report_error(span, message);
    HAD_RUNTIME_ERROR.store(true, Ordering::Relaxed);
}

/// Report the error with the given details.
fn report_error(span: Span, message: &str) {
    let (start_line, start_nl) = LINE_OFFSETS
        .read()
        .unwrap()
        .line_and_newline_offset(span.start);
    let (end_line, end_nl) = LINE_OFFSETS
        .read()
        .unwrap()
        .line_and_newline_offset(span.end);
    let start_col = span.start - start_nl + 1;
    let end_col = span.end - end_nl + 1;

    let prefix = if start_line == end_line {
        format!("[line {start_line}, cols {start_col} to {end_col}]")
    } else {
        format!("[{start_line}:{start_col} to {end_line}:{end_col}]")
    };

    print_error_message(Some(&prefix), message);
}

/// Print the given error message.
fn print_error_message(prefix: Option<&str>, message: &str) {
    use crossterm::{
        execute,
        style::{Attribute, Color, Print, ResetColor, SetAttribute, SetForegroundColor},
    };
    use std::io::stderr;

    if let Some(prefix) = prefix {
        execute!(
            stderr(),
            Print(format!("{prefix} ")),
            SetForegroundColor(Color::Red),
            SetAttribute(Attribute::Bold),
            Print("ERROR"),
            ResetColor,
            SetAttribute(Attribute::Reset),
            Print(format!(": {message}\n"))
        )
    } else {
        execute!(
            stderr(),
            SetForegroundColor(Color::Red),
            SetAttribute(Attribute::Bold),
            Print("ERROR"),
            ResetColor,
            SetAttribute(Attribute::Reset),
            Print(format!(": {message}\n"))
        )
    }
    .unwrap();
}
