_default:
	@just --list

# run the tree-walk interpreter
run-tw args='':
	cargo run --bin rlox-tw -- {{args}}

# run the tree-walk interpreter in release mode
run-tw-release args='':
	cargo run --bin rlox-tw --release -- {{args}}

# run the tests
test:
	cargo build --release
	cargo test --release
	cargo run --release --bin test-harness -- lox-tests/tests/ -i {{justfile_directory()}}/target/release/rlox-tw

# run clippy
clippy:
	cargo clippy --all-targets --workspace -- --deny warnings
