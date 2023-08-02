//! This module acts as a top-level entrypoint to evaluating Lox code.

use crate::{
    interpreter::TwInterpreter,
    parser::Parser,
    pretty_printers::{ParenPrinter, RpnPrinter},
    scanner::Scanner,
    span::{LineOffsets, Span},
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
                        print_error_message(None, "Bad input, please try again");
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

        let mut scanner = Scanner::new(code);
        let tokens = scanner.scan_tokens();

        debug!(?tokens);
        let mut parser = Parser::new(tokens.clone(), scanner.line_offsets);
        let expr = match parser.parse() {
            Some(expr) => expr,
            None => return,
        };

        debug!(?expr);
        debug!(parens = ParenPrinter::print(&expr));
        debug!(rpn = RpnPrinter::print(&expr));

        if let Some(output) = TwInterpreter::interpret(&expr, &parser.line_offsets) {
            println!("{}", output.value);
        }
    }
}

/// Report the error with the given details.
pub fn report_error(span: Span, line_offsets: &LineOffsets, message: &str) {
    let (start_line, start_nl) = line_offsets.line_and_newline_offset(span.start);
    let (end_line, end_nl) = line_offsets.line_and_newline_offset(span.end);
    let start_col = span.start - start_nl + 1;
    let end_col = span.end - end_nl + 1;

    let prefix = if start_line == end_line {
        format!("[line {start_line}, cols {start_col} to {end_col}]")
    } else {
        format!("[{start_line}:{start_col} to {end_line}:{end_col}]")
    };

    print_error_message(Some(&prefix), message);
    HAD_ERROR.store(true, Ordering::Relaxed);
}

/// Report an error at the given token with the given message.
pub fn report_token_error(token: &Token<'_>, line_offsets: &LineOffsets, message: &str) {
    let string = if token.token_type == TokenType::Eof {
        format!("at end: {message}")
    } else {
        format!("at '{}': {message}", token.lexeme)
    };

    report_error(token.span, line_offsets, &string)
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
