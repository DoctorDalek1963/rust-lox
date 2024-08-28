//! This module provides [`LoxObject`].

use crate::{
    callable::{lox_function::LoxFunction, LoxCallable},
    class::{LoxClass, LoxInstance},
    span::WithSpan,
};
use std::{cell::RefCell, rc::Rc};

/// A [`LoxObject`] wrapped in [`WithSpan`].
pub type SpanObject = WithSpan<LoxObject>;

/// Possible objects in Lox.
#[derive(Clone, Debug)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum LoxObject {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
    NativeFunction(Rc<dyn LoxCallable>),
    LoxFunction(Rc<LoxFunction>),
    LoxClass(Rc<LoxClass>),
    LoxInstance(Rc<RefCell<LoxInstance>>),
}

impl PartialEq for LoxObject {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::NativeFunction(a), Self::NativeFunction(b)) => a.name() == b.name(),
            (Self::LoxFunction(a), Self::LoxFunction(b)) => a.name() == b.name(),
            (Self::LoxClass(a), Self::LoxClass(b)) => a == b,
            (Self::LoxInstance(a), Self::LoxInstance(b)) => *a.borrow() == *b.borrow(),
            _ => false,
        }
    }
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
            NativeFunction(_) => "<native fn>".to_string(),
            LoxFunction(_) => "<fn>".to_string(),
            LoxClass(_) => "<class>".to_string(),
            LoxInstance(instance) => format!("<{} instance>", instance.borrow().class_name()),
        }
    }

    /// Return the representation of the object to display when printing.
    pub fn print(&self) -> String {
        use LoxObject::*;

        match self {
            Nil => "nil".to_string(),
            Boolean(b) => b.to_string(),
            String(s) => s.to_string(),
            Number(n) => n.to_string(),
            NativeFunction(func) => format!("<native fn {}>", func.name()),
            LoxFunction(func) => format!("<fn {}>", func.name()),
            LoxClass(class) => format!("<class {}>", class.name()),
            LoxInstance(instance) => format!("<{} instance>", instance.borrow().class_name()),
        }
    }

    /// Return the representation of the object to display in the REPL.
    pub fn repr(&self) -> String {
        use LoxObject::*;

        match self {
            String(s) => format!("{s:?}"),
            _ => self.print(),
        }
    }

    /// Is this object truthy?
    pub fn is_truthy(&self) -> bool {
        #[allow(clippy::match_like_matches_macro, reason = "This is much clearer")]
        match self {
            Self::Nil | Self::Boolean(false) => false,
            _ => true,
        }
    }
}
