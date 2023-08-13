//! This crate provides an interpreter that interprets the AST directly by tree-walking it.

pub mod interpreter;
pub(crate) mod resolver;

pub use interpreter::TwInterpreter;
