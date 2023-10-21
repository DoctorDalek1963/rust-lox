//! This module provides the [`Parser`].

mod exprs;
mod stmts;

use crate::{
    ast::SpanStmt,
    lox::{report_non_runtime_error, report_token_error},
    span::Span,
    tokens::{Token, TokenType},
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

impl ParseError<'_> {
    /// Report the parsing error to the user.
    fn report(&self) {
        match self {
            Self {
                token,
                previous_span: Some(span),
                message,
            } => report_non_runtime_error(span.union(&token.span), message),
            Self {
                token,
                previous_span: None,
                message,
            } => report_token_error(token, message),
        }
    }
}

/// A result wrapping a [`ParseError`].
type ParseResult<'s, T, E = ParseError<'s>> = ::std::result::Result<T, E>;

/// A recursive descent parser for Lox.
///
/// It parses this grammar:
/// ```text
/// program     → declaration* EOF ;
///
/// declaration → funDecl | varDecl | statement ;
///
/// funDecl     → "fun" function ;
/// function    → IDENTIFIER "(" parameters? ")" block ;
/// parameters  → IDENTIFIER ( "," IDENTIFIER )* ;
/// varDecl     → "var" IDENTIFIER ( "=" expression )? ";" ;
///
/// statement   → exprStmt | ifStmt | printStmt | returnStmt | whileStmt | forStmt | block ;
/// exprStmt    → expression ";" ;
/// ifStmt      → "if" "(" expression ")" statement ( "else" statement )? ;
/// printStmt   → "print" expression ";" ;
/// returnStmt  → "return" expression? ";" ;
/// whileStmt   → "while" "(" expression ")" statement ;
/// forStmt     → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
/// block       → "{" declaration* "}" ;
///
/// expression  → assignment ;
/// assignment  → ( call "." )? IDENTIFIER "=" assignment | logic_or ;
/// logic_or    → logic_and ( "or" logic_and )* ;
/// logic_and   → equality ( "and" equality )* ;
/// equality    → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term        → factor ( ( "-" | "+" ) factor )* ;
/// factor      → unary ( ( "/" | "*" ) unary )* ;
/// unary       → ( "!" | "-" ) unary | call ;
/// call        → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
/// arguments   → expression ( "," expression )* ;
/// primary     → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
/// ```
pub struct Parser<'s> {
    /// The token list that we're parsing.
    tokens: Vec<Token<'s>>,

    /// The index of the token currently being considered.
    current: usize,

    /// The statements that have been parsed by the parser.
    statements: Vec<SpanStmt>,
}

impl<'s> Parser<'s> {
    /// Parse the given list of tokens.
    pub fn parse(tokens: Vec<Token<'s>>) -> Vec<SpanStmt> {
        let mut parser = Self {
            tokens,
            current: 0,
            statements: vec![],
        };

        parser.parse_program();
        parser.statements
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

    /// program → declaration* EOF ;
    fn parse_program(&mut self) {
        while !self.is_at_end() {
            if let Some(stmt) = self.parse_declaration() {
                self.statements.push(stmt);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{BinaryOperator, Expr, Stmt, UnaryOperator},
        scanner::Scanner,
        span::WithSpan,
    };

    #[test]
    fn parse() {
        let ast: Vec<_> = Parser::parse(Scanner::scan_tokens("print (5 - (3.2 / 1)) + -1;"))
            .into_iter()
            .map(|stmt| stmt.value)
            .collect();

        #[allow(illegal_floating_point_literal_pattern)]
        let &[Stmt::Print(WithSpan {
            span: _,
            value:
                Expr::Binary(
                    box WithSpan {
                        span: _,
                        value:
                            Expr::Grouping(box WithSpan {
                                span: _,
                                value:
                                    Expr::Binary(
                                        box WithSpan {
                                            span: _,
                                            value: Expr::Number(5.0),
                                        },
                                        WithSpan {
                                            span: _,
                                            value: BinaryOperator::Minus,
                                        },
                                        box WithSpan {
                                            span: _,
                                            value:
                                                Expr::Grouping(box WithSpan {
                                                    span: _,
                                                    value:
                                                        Expr::Binary(
                                                            box WithSpan {
                                                                span: _,
                                                                value: Expr::Number(3.2),
                                                            },
                                                            WithSpan {
                                                                span: _,
                                                                value: BinaryOperator::Slash,
                                                            },
                                                            box WithSpan {
                                                                span: _,
                                                                value: Expr::Number(1.0),
                                                            },
                                                        ),
                                                }),
                                        },
                                    ),
                            }),
                    },
                    WithSpan {
                        span: _,
                        value: BinaryOperator::Plus,
                    },
                    box WithSpan {
                        span: _,
                        value:
                            Expr::Unary(
                                WithSpan {
                                    span: _,
                                    value: UnaryOperator::Minus,
                                },
                                box WithSpan {
                                    span: _,
                                    value: Expr::Number(1.0),
                                },
                            ),
                    },
                ),
        })] = &ast[..]
        else {
            panic!("Parsed statements did not match expected pattern. Parsed statements: {ast:#?}")
        };
    }
}
