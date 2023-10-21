//! This crate acts as a test harness to test the interpreter with some Lox code.

use clap::Parser;
use crossterm::{
    execute,
    style::{Attribute, Color, Print, ResetColor, SetAttribute, SetForegroundColor},
};
use similar::{ChangeTag, TextDiff};
use std::{
    env, fs,
    io::{self, stdout},
    path::{Path, PathBuf},
    process::{self, Command},
    str::{self, Utf8Error},
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc, Mutex,
    },
};
use strip_ansi::strip_ansi;
use thiserror::Error;
use threadpool::ThreadPool;
use walkdir::WalkDir;

static TOTAL_TESTS: AtomicU32 = AtomicU32::new(0);
static TOTAL_FAILURES: AtomicU32 = AtomicU32::new(0);

#[derive(Debug, Error)]
enum TestError {
    #[error("I/O error: `{0}`")]
    Io(#[from] io::Error),

    #[error("Utf8 decoding error: `{0}`")]
    DecodingError(#[from] Utf8Error),

    #[error("Failed stdout in test:\nExpected:\n{0}\nGot:\n{1}")]
    FailedStdout(String, String),

    #[error("Failed stderr in test:\nExpected:\n{0}\nGot:\n{1}")]
    FailedStderr(String, String),
}

/// Run the test in the given file with the given interpreter.
fn run_test(file: &Path, interpreter_path: &Path) -> Result<(), TestError> {
    let expected_stdout = fs::read_to_string(file.with_extension("stdout"))
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|_| String::new());
    let expected_stderr = fs::read_to_string(file.with_extension("stderr"))
        .map(|s| s.trim().to_string())
        .unwrap_or_else(|_| String::new());

    let output = Command::new(interpreter_path).arg(file).output()?;
    let stdout = strip_ansi(str::from_utf8(&output.stdout)?)
        .trim()
        .to_string();
    let stderr = strip_ansi(str::from_utf8(&output.stderr)?)
        .trim()
        .to_string();

    if expected_stdout != stdout {
        Err(TestError::FailedStdout(expected_stdout, stdout))
    } else if expected_stderr != stderr {
        Err(TestError::FailedStderr(expected_stderr, stderr))
    } else {
        Ok(())
    }
}

fn print_diff(fd: &str, expected: &str, got: &str) -> String {
    let mut output = String::new();

    output.push_str(&format!(
        "{}Expected {}:{}\n",
        Attribute::Bold,
        fd,
        Attribute::Reset
    ));
    output.push_str(expected);
    output.push_str("\n\n");
    output.push_str(&format!(
        "{}Got {}:{}\n",
        Attribute::Bold,
        fd,
        Attribute::Reset
    ));

    let diff = TextDiff::from_lines(expected, got);
    for change in diff.iter_all_changes() {
        let sign = match change.tag() {
            ChangeTag::Delete => format!("{}-", SetForegroundColor(Color::Red)),
            ChangeTag::Insert => format!("{}+", SetForegroundColor(Color::Green)),
            ChangeTag::Equal => format!("{} ", Attribute::Dim),
        };
        output.push_str(&format!(
            "{}{}{}{}",
            sign,
            change,
            Attribute::Reset,
            ResetColor
        ));
    }

    output
}

fn display_test_result(filename: &Path, interpreter_path: &Path, print_lock: Arc<Mutex<()>>) {
    let result = run_test(filename, interpreter_path);

    let filename = filename
        .to_string_lossy()
        .split("lox-tests/")
        .last()
        .unwrap()
        .to_owned();
    let interpreter = interpreter_path.file_name().unwrap().to_str().unwrap();

    TOTAL_TESTS.fetch_add(1, Ordering::Relaxed);

    match result {
        Ok(()) => {
            let lock = print_lock.lock().unwrap();
            execute!(
                stdout(),
                Print(format!("{interpreter} {filename} ")),
                SetForegroundColor(Color::Green),
                SetAttribute(Attribute::Bold),
                Print("PASSED\n"),
                ResetColor,
                SetAttribute(Attribute::Reset),
            )
            .unwrap();
            drop(lock);
        }
        Err(error) => {
            let message = match error {
                TestError::Io(e) => format!("IO error: {e:?}"),
                TestError::DecodingError(e) => format!("Utf8 decoding error: {e:?}"),
                TestError::FailedStdout(expected, got) => print_diff("stdout", &expected, &got),
                TestError::FailedStderr(expected, got) => print_diff("stderr", &expected, &got),
            };

            let lock = print_lock.lock().unwrap();
            execute!(
                stdout(),
                Print(format!("{interpreter} {filename} ")),
                SetForegroundColor(Color::Red),
                SetAttribute(Attribute::Bold),
                Print("FAILED"),
                ResetColor,
                SetAttribute(Attribute::Reset),
                Print(":\n"),
                Print(message),
                Print("\n"),
            )
            .unwrap();
            drop(lock);
            TOTAL_FAILURES.fetch_add(1, Ordering::Relaxed);
        }
    }
}

/// The args of the program.
#[derive(Parser, Debug)]
struct Args {
    /// The directory of Lox files to test.
    test_dir: PathBuf,

    /// The interpreters to test with.
    #[arg(short, long)]
    interpreter: Vec<PathBuf>,
}

fn main() -> Result<(), io::Error> {
    let args = Args::parse();

    let files = WalkDir::new(env::current_dir()?.join(args.test_dir))
        .into_iter()
        .filter_map(Result::ok)
        .filter_map(|entry| entry.file_type().is_file().then_some(entry.into_path()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "lox"))
        .collect::<Vec<_>>();

    let pool = ThreadPool::new(num_cpus::get());
    let print_lock = Arc::new(Mutex::new(()));

    for interpreter in &args.interpreter {
        for file in &files {
            let file = file.clone();
            let interpreter = interpreter.clone();
            let print_lock = Arc::clone(&print_lock);
            pool.execute(move || display_test_result(&file, &interpreter, print_lock));
        }
    }

    pool.join();

    println!(
        "\n{} failures in {} tests",
        TOTAL_FAILURES.load(Ordering::Relaxed),
        TOTAL_TESTS.load(Ordering::Relaxed)
    );
    if TOTAL_FAILURES.load(Ordering::Relaxed) > 0 {
        process::exit(255);
    }

    Ok(())
}
