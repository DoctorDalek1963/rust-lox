//! This module provides [`LoxObject`].

use std::fmt;

/// Possible objects in Lox.
#[derive(Clone, Debug, PartialEq)]
pub enum LoxObject {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl fmt::Display for LoxObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LoxObject::*;

        let string = match self {
            Nil => "nil".to_string(),
            Boolean(b) => b.to_string(),
            String(s) => s.clone(),
            Number(n) => n.to_string(),
        };

        write!(f, "{}", string)
    }
}

impl LoxObject {
    pub fn type_name(&self) -> String {
        use LoxObject::*;

        match self {
            Nil => "nil".to_string(),
            Boolean(_) => "boolean".to_string(),
            String(_) => "string".to_string(),
            Number(_) => "number".to_string(),
        }
    }
}
