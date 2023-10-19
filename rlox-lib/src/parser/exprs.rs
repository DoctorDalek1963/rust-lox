//! This module lets the [`Parser`] parse expressions.

use super::{ParseError, ParseResult, Parser};
use crate::{
    ast::{BinaryOperator, Expr, LogicalOperator, SpanExpr, UnaryOperator},
    lox::report_non_runtime_error,
    span::WithSpan,
    tokens::{Token, TokenLiteral, TokenType},
};

impl<'s> Parser<'s> {
    /// expression → assignment ;
    pub(super) fn parse_expression(&mut self) -> ParseResult<'s, SpanExpr> {
        self.parse_assignment()
    }

    /// assignment → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
    fn parse_assignment(&mut self) -> ParseResult<'s, SpanExpr> {
        let expr = self.parse_logic_or()?;

        if self.match_tokens([TokenType::Equal]) {
            let equals = *self.previous().unwrap();
            let r_value = self.parse_assignment()?;

            if let WithSpan {
                span: var_name_span,
                value: Expr::Variable(name),
            } = expr
            {
                return Ok(WithSpan {
                    span: var_name_span.union(&r_value.span),
                    value: Expr::Assign(
                        WithSpan {
                            span: var_name_span,
                            value: name,
                        },
                        Box::new(r_value),
                    ),
                });
            } else if let WithSpan {
                span: get_expr_span,
                value: Expr::Get(l_value, ident),
            } = expr
            {
                return Ok(WithSpan {
                    span: get_expr_span.union(&r_value.span),
                    value: Expr::Set(l_value, ident, Box::new(r_value)),
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

    /// logic_or → logic_and ( "or" logic_and )* ;
    fn parse_logic_or(&mut self) -> ParseResult<'s, SpanExpr> {
        let mut expr = self.parse_logic_and()?;

        while self.match_tokens([TokenType::Or]) {
            let operator = WithSpan {
                span: self.previous().unwrap().span,
                value: LogicalOperator::Or,
            };

            let right = self.parse_logic_and()?;

            let span = expr.span.union(&operator.span).union(&right.span);
            let value = Expr::Logical(Box::new(expr), operator, Box::new(right));
            expr = WithSpan { span, value };
        }

        Ok(expr)
    }

    /// logic_and → equality ( "and" equality )* ;
    fn parse_logic_and(&mut self) -> ParseResult<'s, SpanExpr> {
        let mut expr = self.parse_equality()?;

        while self.match_tokens([TokenType::And]) {
            let operator = WithSpan {
                span: self.previous().unwrap().span,
                value: LogicalOperator::And,
            };

            let right = self.parse_equality()?;

            let span = expr.span.union(&operator.span).union(&right.span);
            let value = Expr::Logical(Box::new(expr), operator, Box::new(right));
            expr = WithSpan { span, value };
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

    /// unary → ( "!" | "-" ) unary | call ;
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
            self.parse_call()
        }
    }

    /// call → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    fn parse_call(&mut self) -> ParseResult<'s, SpanExpr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_tokens([TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_tokens([TokenType::Dot]) {
                let Token { lexeme, span, .. } = self.consume(
                    TokenType::Identifier,
                    Some(expr.span.union(&self.previous().unwrap().span)),
                    "Expected property name after '.'".to_string(),
                )?;
                let name = WithSpan {
                    span,
                    value: lexeme.to_string(),
                };
                expr = WithSpan {
                    span: expr.span.union(&span),
                    value: Expr::Get(Box::new(expr), name),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Finish parsing a function call by parsing the argument list and closing paren.
    /// `arguments → expression ( "," expression )* ;`
    fn finish_call(&mut self, callee: SpanExpr) -> ParseResult<'s, SpanExpr> {
        let mut arguments: Vec<SpanExpr> = Vec::new();
        let mut reported_max_args_error = false;

        /// Get the combined span of the callee and all arguments that have been parsed so far.
        macro_rules! get_callee_and_args_span {
            () => {
                arguments
                    .iter()
                    .map(|expr| expr.span)
                    .fold(callee.span, |acc, current| acc.union(&current))
            };
        }

        if !self.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 && !reported_max_args_error {
                    report_non_runtime_error(
                        get_callee_and_args_span!(),
                        "Cannot have more than 255 arguments in function call",
                    );
                    reported_max_args_error = true;
                }

                arguments.push(self.parse_expression()?);

                if !self.match_tokens([TokenType::Comma]) {
                    break;
                }
            }
        }

        let close_paren = self.consume(
            TokenType::RightParen,
            Some(get_callee_and_args_span!()),
            "Expected ')' after arguments in function call".to_string(),
        )?;

        Ok(WithSpan {
            span: callee.span.union(&close_paren.span),
            value: Expr::Call(Box::new(callee), arguments, close_paren.span),
        })
    }

    /// primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn parse_primary(&mut self) -> ParseResult<'s, SpanExpr> {
        use TokenType::*;

        if self.match_tokens([
            True, False, Nil, This, Number, String, Identifier, LeftParen,
        ]) {
            let previous = self.previous().unwrap();
            let mut span = previous.span;

            let value = match previous {
                Token {
                    token_type: True, ..
                } => Expr::Boolean(true),
                Token {
                    token_type: False, ..
                } => Expr::Boolean(false),
                Token {
                    token_type: Nil, ..
                } => Expr::Nil,
                Token {
                    token_type: This, ..
                } => Expr::This,
                Token {
                    token_type: Number,
                    literal: Some(TokenLiteral::Number(num)),
                    ..
                } => Expr::Number(*num),
                Token {
                    token_type: String,
                    literal: Some(TokenLiteral::String(string)),
                    ..
                } => Expr::String(string.to_string()),
                Token {
                    token_type: Identifier,
                    lexeme,
                    ..
                } => Expr::Variable(lexeme.to_string()),
                Token {
                    token_type: LeftParen,
                    ..
                } => {
                    let expr = self.parse_expression()?;
                    let right_paren_span = self
                        .consume(
                            RightParen,
                            Some(span.union(&expr.span)),
                            "Expected ')' at end of grouped expression".to_string(),
                        )?
                        .span;
                    span.mut_union(&right_paren_span);
                    Expr::Grouping(Box::new(expr))
                }
                _ => unreachable!(
                    "match_tokens() will only return a token with a TokenType that we expected"
                ),
            };

            Ok(WithSpan { span, value })
        } else {
            let token = *self.peek().unwrap_or_else(|| self.previous().unwrap());
            let message = format!("Expected expression, got {:?}", token.token_type);
            Err(ParseError {
                token,
                previous_span: None,
                message,
            })
        }
    }
}
