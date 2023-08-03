//! This module provides [`LoxObject`].

use crate::span::WithSpan;

/// A [`LoxObject`] wrapped in [`WithSpan`].
pub type SpanObject = WithSpan<LoxObject>;

/// Possible objects in Lox.
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum LoxObject {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl LoxObject {
    /// Get the name of the type of this object.
    pub fn type_name(&self) -> String {
        use LoxObject::*;

        match self {
            Nil => "nil".to_string(),
            Boolean(_) => "boolean".to_string(),
            String(_) => "string".to_string(),
            Number(_) => "number".to_string(),
        }
    }

    /// Return the representation of the type to display when printing.
    pub fn print(&self) -> String {
        use LoxObject::*;

        match self {
            Nil => "nil".to_string(),
            Boolean(b) => b.to_string(),
            String(s) => s.to_string(),
            Number(n) => n.to_string(),
        }
    }

    /// Is this object truthy?
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil | Self::Boolean(false) => false,
            _ => true,
        }
    }
}
