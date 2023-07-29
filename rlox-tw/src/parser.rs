//! This module provides the [`Parser`].

use crate::{
    ast::{BinaryOperator, Expr, UnaryOperator},
    lox::report_token_error,
    tokens::{Token, TokenLiteral, TokenType},
};
use std::fmt;
use thiserror::Error;

/// An error that occured during parsing.
#[derive(Clone, Copy, Debug, PartialEq, Error)]
struct ParseError<'s> {
    /// The token that caused the error.
    token: Token<'s>,

    /// The message to display to the user.
    message: &'static str,
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A result wrapping a [`ParseError`].
type ParseResult<'s, T, E = ParseError<'s>> = ::std::result::Result<T, E>;

/// A recursive descent parser for Lox.
///
/// It parses this grammar:
/// ```text
/// expression → equality ;
/// equality   → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term       → factor ( ( "-" | "+" ) factor )* ;
/// factor     → unary ( ( "/" | "*" ) unary )* ;
/// unary      → ( "!" | "-" ) unary
///              | primary ;
/// primary    → NUMBER | STRING | "true" | "false" | "nil"
///              | "(" expression ")" ;
/// ```
pub struct Parser<'s> {
    /// The token list that we're parsing.
    tokens: Vec<Token<'s>>,

    /// The index of the token currently being considered.
    current: usize,
}

impl<'s> Parser<'s> {
    /// Parse the given list of tokens.
    pub fn parse(tokens: Vec<Token<'s>>) -> Option<Expr> {
        let mut parser = Self { tokens, current: 0 };
        parser.parse_expression().ok()
    }

    /// Get the current token.
    #[inline]
    fn peek(&self) -> Option<&Token<'s>> {
        self.tokens.get(self.current)
    }

    /// Get the previous token.
    #[inline]
    fn previous(&self) -> Option<&Token<'s>> {
        self.tokens.get(self.current.saturating_sub(1))
    }

    /// Are we at the end of the tokn list?
    #[inline]
    fn is_at_end(&self) -> bool {
        self.check(TokenType::Eof)
    }

    /// Advance the internal pointer and get the next token.
    fn advance(&mut self) -> Token<'s> {
        if !self.is_at_end() {
            self.current += 1;
        }
        *self.previous().unwrap()
    }

    /// Check if the next token is of the given type.
    #[inline]
    fn check(&self, token_type: TokenType) -> bool {
        self.peek().is_some_and(|t| t.token_type == token_type)
    }

    /// Check if the next token is of one of the given types and advance if it is.
    fn match_tokens(&mut self, token_types: impl IntoIterator<Item = TokenType>) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    /// Expect the next token to be of the given type and return it if so, or return an error with
    /// the given message if the check fails.
    fn consume(
        &mut self,
        token_type: TokenType,
        message: &'static str,
    ) -> ParseResult<'s, Token<'s>> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = *self.peek().unwrap();
            crate::lox::report_token_error(&token, message);
            Err(ParseError { token, message })
        }
    }

    /// Synchronize the parser to an assumed correct state after an error.
    fn synchronize(&mut self) {
        use TokenType::*;

        self.advance();

        while !self.is_at_end() {
            if self.previous().is_some_and(|t| t.token_type == Semicolon) {
                return;
            }

            match self.peek().map(|t| t.token_type) {
                Some(Class | Fun | Var | For | If | While | Print | Return) => return,
                _ => {}
            }

            self.advance();
        }
    }

    /// expression → equality ;
    fn parse_expression(&mut self) -> ParseResult<'s, Expr> {
        self.parse_equality()
    }

    /// equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn parse_equality(&mut self) -> ParseResult<'s, Expr> {
        use TokenType::*;

        let mut expr = self.parse_comparison()?;

        while self.match_tokens([BangEqual, EqualEqual]) {
            let operator = match self.previous() {
                Some(Token {
                    token_type: BangEqual,
                    ..
                }) => BinaryOperator::BangEqual,
                Some(Token {
                    token_type: EqualEqual,
                    ..
                }) => BinaryOperator::EqualEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    /// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn parse_comparison(&mut self) -> ParseResult<'s, Expr> {
        use TokenType::*;

        let mut expr = self.parse_term()?;

        while self.match_tokens([Greater, GreaterEqual, Less, LessEqual]) {
            let operator = match self.previous() {
                Some(Token {
                    token_type: Greater,
                    ..
                }) => BinaryOperator::Greater,
                Some(Token {
                    token_type: GreaterEqual,
                    ..
                }) => BinaryOperator::GreaterEqual,
                Some(Token {
                    token_type: Less, ..
                }) => BinaryOperator::Less,
                Some(Token {
                    token_type: LessEqual,
                    ..
                }) => BinaryOperator::LessEqual,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    /// term → factor ( ( "-" | "+" ) factor )* ;
    fn parse_term(&mut self) -> ParseResult<'s, Expr> {
        use TokenType::*;

        let mut expr = self.parse_factor()?;

        while self.match_tokens([Minus, Plus]) {
            let operator = match self.previous() {
                Some(Token {
                    token_type: Minus, ..
                }) => BinaryOperator::Minus,
                Some(Token {
                    token_type: Plus, ..
                }) => BinaryOperator::Plus,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    /// factor → unary ( ( "/" | "*" ) unary )* ;
    fn parse_factor(&mut self) -> ParseResult<'s, Expr> {
        use TokenType::*;

        let mut expr = self.parse_unary()?;

        while self.match_tokens([Slash, Star]) {
            let operator = match self.previous() {
                Some(Token {
                    token_type: Slash, ..
                }) => BinaryOperator::Slash,
                Some(Token {
                    token_type: Star, ..
                }) => BinaryOperator::Star,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;

            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    /// unary → ( "!" | "-" ) unary | primary ;
    fn parse_unary(&mut self) -> ParseResult<'s, Expr> {
        use TokenType::*;

        if self.match_tokens([Bang, Minus]) {
            let operator = match self.previous() {
                Some(Token {
                    token_type: Bang, ..
                }) => UnaryOperator::Bang,
                Some(Token {
                    token_type: Minus, ..
                }) => UnaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;

            Ok(Expr::Unary(operator, Box::new(right)))
        } else {
            self.parse_primary()
        }
    }

    /// primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn parse_primary(&mut self) -> ParseResult<'s, Expr> {
        use TokenType::*;

        if self.match_tokens([True, False, Nil, Number, String, LeftParen]) {
            Ok(match self.previous() {
                Some(Token {
                    token_type: True, ..
                }) => Expr::Boolean(true),
                Some(Token {
                    token_type: False, ..
                }) => Expr::Boolean(false),
                Some(Token {
                    token_type: Nil, ..
                }) => Expr::Nil,
                Some(Token {
                    token_type: Number,
                    literal: Some(TokenLiteral::Number(num)),
                    ..
                }) => Expr::Number(*num),
                Some(Token {
                    token_type: String,
                    literal: Some(TokenLiteral::String(string)),
                    ..
                }) => Expr::String(string.to_string()),
                Some(Token {
                    token_type: LeftParen,
                    ..
                }) => {
                    let expr = self.parse_expression()?;
                    self.consume(RightParen, "Expect ')' at end of grouped expression")?;
                    Expr::Grouping(Box::new(expr))
                }
                _ => unreachable!(),
            })
        } else {
            let token = *self.peek().unwrap_or(self.previous().unwrap());
            let message = "Expected primary token";
            report_token_error(&token, message);
            Err(ParseError { token, message })
        }
    }
}
