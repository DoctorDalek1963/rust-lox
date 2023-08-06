//! This module lets the [`Parser`] parse expressions.

use super::{ParseError, ParseResult, Parser};
use crate::{
    ast::{BinaryOperator, Expr, SpanExpr, UnaryOperator},
    span::WithSpan,
    tokens::{Token, TokenLiteral, TokenType},
};

impl<'s> Parser<'s> {
    /// expression → assignment ;
    pub(super) fn parse_expression(&mut self) -> ParseResult<'s, SpanExpr> {
        self.parse_assignment()
    }

    /// assignment → IDENTIFIER "=" assignment | equality ;
    fn parse_assignment(&mut self) -> ParseResult<'s, SpanExpr> {
        let expr = self.parse_equality()?;

        if self.match_tokens([TokenType::Equal]) {
            let equals = self.previous().unwrap().clone();
            let r_value = self.parse_assignment()?;

            if let WithSpan {
                span: _,
                value: Expr::Variable(name),
            } = expr
            {
                return Ok(WithSpan {
                    span: expr.span.union(&r_value.span),
                    value: Expr::Assign(name, Box::new(r_value)),
                });
            } else {
                ParseError {
                    token: equals,
                    previous_span: Some(expr.span),
                    message: "Invalid assignment target".to_string(),
                }
                .report();
            }
        }

        Ok(expr)
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

        if self.match_tokens([True, False, Nil, Number, String, Identifier, LeftParen]) {
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
                    token_type: Identifier,
                    lexeme,
                    ..
                }) => Expr::Variable(lexeme.to_string()),
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
