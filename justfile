test:
	cargo build --release
	cargo test --release
	cargo run --release --bin test-harness -- lox-tests/tests/ -i {{justfile_directory()}}/target/release/rlox-tw
