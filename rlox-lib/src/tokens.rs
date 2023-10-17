//! This module handles tokens.

use crate::span::Span;
use std::fmt;

/// A single token.
#[derive(Clone, Copy, PartialEq)]
pub struct Token<'s> {
    /// The type of the token.
    pub token_type: TokenType,

    /// The part of the source code where the token came from.
    pub lexeme: &'s str,

    /// The optional literal part of the token (string or numeric literal).
    pub literal: Option<TokenLiteral<'s>>,

    /// The span of the token.
    pub span: Span,
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("Token");
        debug_struct
            .field("token_type", &self.token_type)
            .field("lexeme", &self.lexeme)
            .field("literal", &self.literal);

        if cfg!(debug_assertions) {
            debug_struct.field("span", &self.span);
        }

        debug_struct.finish()
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token({:?}, {:?}, {:?})",
            self.token_type, self.lexeme, self.literal
        )
    }
}

/// A string or numeric literal token.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenLiteral<'s> {
    /// A string literal.
    String(&'s str),

    /// A numeric literal.
    Number(f64),
}

/// A list of all possible Lox tokens.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[allow(clippy::missing_docs_in_private_items)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}
