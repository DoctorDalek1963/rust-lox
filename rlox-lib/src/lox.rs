//! This module acts as a top-level entrypoint to evaluating Lox code.

use crate::{
    parser::Parser,
    scanner::Scanner,
    span::{LineOffsets, Span},
    tokens::{Token, TokenType},
    Interpreter,
};
use lazy_static::lazy_static;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
    cmp, fs, io,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        RwLock,
    },
};
use thiserror::Error;
use tracing::{debug, instrument, trace};

/// Have we encountered at least one error before runtime?
static HAD_NON_RUNTIME_ERROR: AtomicBool = AtomicBool::new(false);

/// Have we encountered at least one error at runtime?
static HAD_RUNTIME_ERROR: AtomicBool = AtomicBool::new(false);

lazy_static! {
    /// The LineOffsets of the code being worked with.
    static ref LINE_OFFSETS: RwLock<LineOffsets> = RwLock::new(LineOffsets::new(""));

    /// The source code that we're working with.
    static ref SOURCE_CODE: RwLock<String> = RwLock::new(String::new());
}

/// The Lox interpreter.
#[derive(Clone, Debug)]
pub struct LoxInterpreter<T: Interpreter> {
    /// The core interpreter implementation to use.
    interpreter: T,
}

/// An error that can be returned from [`LoxInterpreter::run_file`].
#[derive(Debug, Error)]
pub enum RunFileError {
    /// An error from running the user's Lox code.
    #[error("An error occured while running the Lox code")]
    LoxError,

    /// A standard I/O error.
    #[error("I/O error: `{0:?}`")]
    Io(#[from] io::Error),
}

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

impl<T: Interpreter> LoxInterpreter<T> {
    /// Create a new interpreter.
    pub fn new() -> Self {
        Self {
            interpreter: T::new(),
        }
    }

    /// Read the file and run the contents.
    pub fn run_file(&mut self, path: impl AsRef<Path>) -> Result<(), RunFileError> {
        let code = fs::read_to_string(path)?;

        *SOURCE_CODE.write().unwrap() = code.clone();
        *LINE_OFFSETS.write().unwrap() = LineOffsets::new(&code);

        self.run_code(&code);

        if HAD_NON_RUNTIME_ERROR.load(Ordering::Relaxed)
            || HAD_RUNTIME_ERROR.load(Ordering::Relaxed)
        {
            Err(RunFileError::LoxError)
        } else {
            Ok(())
        }
    }

    /// Read code from an interactive prompt and run it.
    pub fn run_prompt(&mut self) -> Result<(), PromptError> {
        let mut prompt = DefaultEditor::new()?;

        let history_file =
            home::home_dir().map(|home| home.join(".config").join("rlox").join(".history"));
        if let Some(history_file) = &history_file {
            if let Ok(false) = fs::exists(history_file) {
                fs::create_dir_all(history_file.parent().unwrap())?;
                fs::File::create_new(history_file)?;
            }
            prompt.load_history(&history_file)?;
        }

        loop {
            match prompt.readline("> ") {
                Ok(mut line) => {
                    prompt.add_history_entry(&line)?;
                    if let Some(history_file) = &history_file {
                        prompt.save_history(history_file)?;
                    }
                    line.push('\n');

                    let old_code_width = SOURCE_CODE.read().unwrap().len();
                    SOURCE_CODE.write().unwrap().push_str(&line);
                    *LINE_OFFSETS.write().unwrap() = LineOffsets::new(&SOURCE_CODE.read().unwrap());

                    let line = format!("{:old_code_width$}{line}", "");

                    self.run_code(&line);
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
    fn run_code(&mut self, code: &str) {
        debug!("Running code: ```lox\n{}```", code.trim_start());

        let tokens = Scanner::scan_tokens(code);
        trace!(?tokens);

        let stmts = Parser::parse(tokens);
        trace!(?stmts);

        self.interpreter.interpret(&stmts);
    }
}

/// The level of severity in an error/warning message.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SeverityLevel {
    /// A fatal error.
    Error,

    /// A non-fatal warning.
    Warning,
}

/// Report an error at the given token with the given message.
pub fn report_token_error(token: &Token<'_>, message: &str) {
    let string = if token.token_type == TokenType::Eof {
        format!("at end: {message}")
    } else {
        format!("at '{}': {message}", token.lexeme)
    };

    print_error_message(Some(token.span), &string, SeverityLevel::Error);
    HAD_NON_RUNTIME_ERROR.store(true, Ordering::Relaxed);
}

/// Report an error before runtime.
pub fn report_non_runtime_error(span: Span, message: &str) {
    print_error_message(Some(span), message, SeverityLevel::Error);
    HAD_NON_RUNTIME_ERROR.store(true, Ordering::Relaxed);
}

/// Report an error at runtime.
pub fn report_runtime_error(span: Span, message: &str) {
    print_error_message(Some(span), message, SeverityLevel::Error);
    HAD_RUNTIME_ERROR.store(true, Ordering::Relaxed);
}

/// Report a non-fatal warning.
pub fn report_warning(span: Span, message: &str) {
    print_error_message(Some(span), message, SeverityLevel::Warning);
}

/// Print the given error message.
#[instrument(skip_all)]
fn print_error_message(span: Option<Span>, message: &str, level: SeverityLevel) {
    use crossterm::{
        execute,
        style::{Attribute, Color, Print, ResetColor, SetAttribute, SetForegroundColor},
    };
    use std::io::stderr;

    let (highlight_color, severity_name) = match level {
        SeverityLevel::Error => (Color::Red, "ERROR"),
        SeverityLevel::Warning => (Color::Yellow, "WARNING"),
    };

    let message = if let Some(span) = span {
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
        let line_number_width =
            cmp::max(start_line.to_string().len(), end_line.to_string().len()) + 1;

        trace!(?span);
        trace!(?start_line, ?start_nl, ?start_col);
        trace!(?end_line, ?end_nl, ?end_col);

        let mut message = format!(": {message}\n");
        message.push_str(&format!(
            "{:width$}{}{}-->{}{} {start_line}:{start_col}\n",
            "",
            SetForegroundColor(Color::Blue),
            Attribute::Bold,
            ResetColor,
            Attribute::Reset,
            width = line_number_width - 1,
        ));
        message.push_str(&format!(
            "{}{}{:line_number_width$}|{}{}\n",
            SetForegroundColor(Color::Blue),
            Attribute::Bold,
            "",
            ResetColor,
            Attribute::Reset,
        ));

        if start_line == end_line {
            message.push_str(&format!(
                "{}{}{start_line}{:width$}|{}{} ",
                SetForegroundColor(Color::Blue),
                Attribute::Bold,
                "",
                ResetColor,
                Attribute::Reset,
                width = line_number_width - start_line.to_string().len(),
            ));
            message.push_str(
                SOURCE_CODE
                    .read()
                    .unwrap()
                    .lines()
                    .nth(start_line.saturating_sub(1))
                    .unwrap_or(""),
            );
            message.push('\n');
            message.push_str(&format!(
                "{}{}{:line_number_width$}|{}{} ",
                SetForegroundColor(Color::Blue),
                Attribute::Bold,
                "",
                ResetColor,
                Attribute::Reset,
            ));

            if start_col == end_col {
                message.push_str(&format!(
                    "{}{}{:space_width$}^{}{}",
                    SetForegroundColor(highlight_color),
                    Attribute::Bold,
                    "",
                    ResetColor,
                    Attribute::Reset,
                    space_width = start_col.saturating_sub(1),
                ));
            } else {
                message.push_str(&format!(
                    "{}{}{:space_width$}^{:-<dash_width$}^{}{}",
                    SetForegroundColor(highlight_color),
                    Attribute::Bold,
                    "",
                    "",
                    ResetColor,
                    Attribute::Reset,
                    space_width = start_col.saturating_sub(1),
                    dash_width = end_col.saturating_sub(start_col).saturating_sub(1),
                ));
            }
        } else {
            let source_code_text = SOURCE_CODE.read().unwrap();

            for line in start_line..=end_line {
                let line_text = source_code_text
                    .lines()
                    .nth(line.saturating_sub(1))
                    .unwrap_or("");

                message.push_str(&format!(
                    "{}{}{line}{:width$}|{}{} ",
                    SetForegroundColor(Color::Blue),
                    Attribute::Bold,
                    "",
                    ResetColor,
                    Attribute::Reset,
                    width = line_number_width - line.to_string().len(),
                ));
                message.push_str(line_text);
                message.push('\n');
                message.push_str(&format!(
                    "{}{}{:line_number_width$}|{}{} ",
                    SetForegroundColor(Color::Blue),
                    Attribute::Bold,
                    "",
                    ResetColor,
                    Attribute::Reset,
                ));

                if line == start_line {
                    message.push_str(&format!(
                        "{}{}{:space_width$}^{:-<dash_width$}{}{}",
                        SetForegroundColor(highlight_color),
                        Attribute::Bold,
                        "",
                        "",
                        ResetColor,
                        Attribute::Reset,
                        space_width = start_col.saturating_sub(1),
                        dash_width = line_text.len().saturating_sub(start_col),
                    ));
                } else if line == end_line {
                    message.push_str(&format!(
                        "{}{}{:-<dash_width$}^{}{}",
                        SetForegroundColor(highlight_color),
                        Attribute::Bold,
                        "",
                        ResetColor,
                        Attribute::Reset,
                        dash_width = line_text.len().saturating_sub(end_col),
                    ));
                } else {
                    message.push_str(&format!(
                        "{}{}{:-<dash_width$}{}{}",
                        SetForegroundColor(highlight_color),
                        Attribute::Bold,
                        "",
                        ResetColor,
                        Attribute::Reset,
                        dash_width = line_text.len(),
                    ));
                }

                message.push('\n');
            }
        }

        message.push_str("\n\n");
        message
    } else {
        format!(": {message}\n")
    };

    execute!(
        stderr(),
        SetForegroundColor(highlight_color),
        SetAttribute(Attribute::Bold),
        Print(severity_name),
        ResetColor,
        SetAttribute(Attribute::Reset),
        Print(message)
    )
    .expect("Should be able to print error messages with crossterm");
}
