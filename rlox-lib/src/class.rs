//! This module provides [`LoxClass`] and [`LoxInstance`].

use crate::{
    callable::{lox_function::LoxFunction, LoxCallable},
    interpreter::RuntimeError,
    object::{LoxObject, SpanObject},
    span::{Span, WithSpan},
    Interpreter,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// A class itself, used to create instances.
#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    /// The name of the class, including the span where it was defined.
    name: WithSpan<String>,

    /// The superclass of this class.
    superclass: Option<Rc<LoxClass>>,

    /// The methods of this class.
    methods: HashMap<String, Rc<LoxFunction>>,
}

impl LoxClass {
    /// Create a new Lox class.
    pub fn new(
        name: WithSpan<String>,
        superclass: Option<Rc<LoxClass>>,
        methods: HashMap<String, Rc<LoxFunction>>,
    ) -> Self {
        Self {
            name,
            superclass,
            methods,
        }
    }

    /// Try to find the method with the given name on this class.
    fn find_method(&self, name: &str) -> Option<&Rc<LoxFunction>> {
        self.methods.get(name).or_else(|| {
            self.superclass
                .as_ref()
                .map(|class| class.find_method(name))
                .flatten()
        })
    }
}

impl LoxCallable for Rc<LoxClass> {
    fn name(&self) -> &str {
        &self.name.value
    }

    fn arity(&self) -> u8 {
        self.find_method("init")
            .map_or(0, |init_method| init_method.arity())
    }

    fn call(
        &self,
        interpreter: &mut dyn Interpreter,
        callee_span: Span,
        arguments: &[SpanObject],
        close_paren: Span,
    ) -> Result<LoxObject, RuntimeError> {
        let instance =
            LoxObject::LoxInstance(Rc::new(RefCell::new(LoxInstance::new(Rc::clone(self)))));

        if let Some(init_method) = self.find_method("init") {
            init_method.bind_this(instance.clone()).call(
                interpreter,
                callee_span,
                arguments,
                close_paren,
            )?;
        }

        Ok(instance)
    }
}

/// An instance of a class, created from its constructor.
#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance {
    /// The class that created this instance.
    class: Rc<LoxClass>,

    /// The fields on this instance.
    fields: HashMap<String, LoxObject>,
}

impl LoxInstance {
    /// Create a new instance.
    pub fn new(class: Rc<LoxClass>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    /// Get the name of the class that created this instance.
    pub fn class_name(&self) -> &str {
        self.class.name()
    }

    /// Get the given property on this instance, either a field or a method.
    pub fn get(
        self_instance: &Rc<RefCell<Self>>,
        ident: &WithSpan<String>,
    ) -> Result<LoxObject, RuntimeError> {
        match self_instance.borrow().fields.get(&ident.value).cloned() {
            Some(field) => Ok(field),
            None => match self_instance.borrow().class.find_method(&ident.value) {
                Some(method) => Ok(LoxObject::LoxFunction(
                    method.bind_this(LoxObject::LoxInstance(Rc::clone(self_instance))),
                )),
                None => Err(RuntimeError {
                    message: format!("No property '{}' on instance", ident.value),
                    span: ident.span,
                }),
            },
        }
    }

    /// Set the value of the field.
    pub fn set(&mut self, ident: String, value: LoxObject) {
        self.fields.insert(ident, value);
    }
}
