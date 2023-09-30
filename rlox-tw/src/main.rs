use color_eyre::Result;

/// Run the interpreter.
fn main() -> Result<()> {
    rlox_lib::run_interpreter::<rlox_tw::TwInterpreter>()
}
