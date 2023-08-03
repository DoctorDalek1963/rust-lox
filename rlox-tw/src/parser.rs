//! This module provides the [`Parser`].

use crate::{
    ast::{BinaryOperator, Expr, SpanExpr, UnaryOperator},
    lox::{report_non_runtime_error, report_token_error},
    span::{Span, WithSpan},
    tokens::{Token, TokenLiteral, TokenType},
};
use std::fmt;
use thiserror::Error;

/// An error that occured during parsing.
#[derive(Clone, Debug, PartialEq, Error)]
struct ParseError<'s> {
    /// The token that caused the error.
    token: Token<'s>,

    /// The span of related tokens before this error.
    previous_span: Option<Span>,

    /// The message to display to the user.
    message: String,
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
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
    pub fn parse(tokens: Vec<Token<'s>>) -> Option<SpanExpr> {
        let mut parser = Self { tokens, current: 0 };
        match parser.parse_expression() {
            Ok(span_expr) => Some(span_expr),
            Err(error) => {
                match error {
                    ParseError {
                        token,
                        previous_span: Some(span),
                        message,
                    } => report_non_runtime_error(span.union(&token.span), &message),
                    ParseError {
                        token,
                        previous_span: None,
                        message,
                    } => report_token_error(&token, &message),
                };
                None
            }
        }
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
        previous_span: Option<Span>,
        message: String,
    ) -> ParseResult<'s, Token<'s>> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let token = *self.peek().unwrap();
            Err(ParseError {
                token,
                previous_span,
                message,
            })
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
    fn parse_expression(&mut self) -> ParseResult<'s, SpanExpr> {
        self.parse_equality()
    }

    /// equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn parse_equality(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        let mut expr = self.parse_comparison()?;

        while self.match_tokens([BangEqual, EqualEqual]) {
            let (value, span) = match self.previous() {
                Some(Token {
                    token_type: BangEqual,
                    span,
                    ..
                }) => (BinaryOperator::BangEqual, *span),
                Some(Token {
                    token_type: EqualEqual,
                    span,
                    ..
                }) => (BinaryOperator::EqualEqual, *span),
                _ => unreachable!(),
            };
            let operator = WithSpan { span, value };

            let right = self.parse_comparison()?;

            let span = expr.span.union(&operator.span).union(&right.span);
            let value = Expr::Binary(Box::new(expr), operator, Box::new(right));
            expr = WithSpan { span, value };
        }

        Ok(expr)
    }

    /// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn parse_comparison(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        let mut expr = self.parse_term()?;

        while self.match_tokens([Greater, GreaterEqual, Less, LessEqual]) {
            let (value, span) = match self.previous() {
                Some(Token {
                    token_type: Greater,
                    span,
                    ..
                }) => (BinaryOperator::Greater, *span),
                Some(Token {
                    token_type: GreaterEqual,
                    span,
                    ..
                }) => (BinaryOperator::GreaterEqual, *span),
                Some(Token {
                    token_type: Less,
                    span,
                    ..
                }) => (BinaryOperator::Less, *span),
                Some(Token {
                    token_type: LessEqual,
                    span,
                    ..
                }) => (BinaryOperator::LessEqual, *span),
                _ => unreachable!(),
            };
            let operator = WithSpan { span, value };

            let right = self.parse_term()?;

            let span = expr.span.union(&operator.span).union(&right.span);
            let value = Expr::Binary(Box::new(expr), operator, Box::new(right));
            expr = WithSpan { span, value };
        }

        Ok(expr)
    }

    /// term → factor ( ( "-" | "+" ) factor )* ;
    fn parse_term(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        let mut expr = self.parse_factor()?;

        while self.match_tokens([Minus, Plus]) {
            let (value, span) = match self.previous() {
                Some(Token {
                    token_type: Minus,
                    span,
                    ..
                }) => (BinaryOperator::Minus, *span),
                Some(Token {
                    token_type: Plus,
                    span,
                    ..
                }) => (BinaryOperator::Plus, *span),
                _ => unreachable!(),
            };
            let operator = WithSpan { span, value };

            let right = self.parse_factor()?;

            let span = expr.span.union(&operator.span).union(&right.span);
            let value = Expr::Binary(Box::new(expr), operator, Box::new(right));
            expr = WithSpan { span, value };
        }

        Ok(expr)
    }

    /// factor → unary ( ( "/" | "*" ) unary )* ;
    fn parse_factor(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        let mut expr = self.parse_unary()?;

        while self.match_tokens([Slash, Star]) {
            let (value, span) = match self.previous() {
                Some(Token {
                    token_type: Slash,
                    span,
                    ..
                }) => (BinaryOperator::Slash, *span),
                Some(Token {
                    token_type: Star,
                    span,
                    ..
                }) => (BinaryOperator::Star, *span),
                _ => unreachable!(),
            };
            let operator = WithSpan { span, value };

            let right = self.parse_unary()?;

            let span = expr.span.union(&operator.span).union(&right.span);
            let value = Expr::Binary(Box::new(expr), operator, Box::new(right));
            expr = WithSpan { span, value };
        }

        Ok(expr)
    }

    /// unary → ( "!" | "-" ) unary | primary ;
    fn parse_unary(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        if self.match_tokens([Bang, Minus]) {
            let (value, span) = match self.previous() {
                Some(Token {
                    token_type: Bang,
                    span,
                    ..
                }) => (UnaryOperator::Bang, *span),
                Some(Token {
                    token_type: Minus,
                    span,
                    ..
                }) => (UnaryOperator::Minus, *span),
                _ => unreachable!(),
            };
            let operator = WithSpan { span, value };

            let right = self.parse_unary()?;

            let span = operator.span.union(&right.span);
            let value = Expr::Unary(operator, Box::new(right));
            Ok(WithSpan { span, value })
        } else {
            self.parse_primary()
        }
    }

    /// primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn parse_primary(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        if self.match_tokens([True, False, Nil, Number, String, LeftParen]) {
            let previous = self.previous();

            let span = match previous {
                Some(Token { span, .. }) => *span,
                _ => unreachable!(),
            };
            let value = match previous {
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
                    self.consume(
                        RightParen,
                        Some(span.union(&expr.span)),
                        "Expected ')' at end of grouped expression".to_string(),
                    )?;
                    Expr::Grouping(Box::new(expr))
                }
                _ => unreachable!(),
            };

            Ok(WithSpan { span, value })
        } else {
            let token = *self.peek().unwrap_or_else(|| self.previous().unwrap());
            let message = format!("Expected primary token, got {:?}", token.token_type);
            Err(ParseError {
                token,
                previous_span: None,
                message,
            })
        }
    }
}
