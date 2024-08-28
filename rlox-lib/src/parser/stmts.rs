//! This module lets the [`Parser`] parse statements.

use super::{ParseResult, Parser};
use crate::{
    ast::{Expr, FunctionOrMethod, SpanStmt, Stmt},
    lox::report_non_runtime_error,
    span::{Span, WithSpan},
    tokens::{Token, TokenType},
};
use std::fmt;

/// The type of function declaration.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FunDeclKind {
    /// A free function.
    Function,

    /// A method in a class.
    Method,
}

impl fmt::Display for FunDeclKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Function => "function",
                Self::Method => "method",
            }
        )
    }
}

impl<'s> Parser<'s> {
    /// declaration → classDecl | funDecl | varDecl | statement ;
    pub(super) fn parse_declaration(&mut self) -> Option<SpanStmt> {
        let result = if self.match_tokens([TokenType::Class]) {
            self.parse_class_decl()
        } else if self.match_tokens([TokenType::Fun]) {
            self.parse_function(FunDeclKind::Function)
                .map(|WithSpan { span, value }| WithSpan {
                    span,
                    value: Stmt::FunDecl(value),
                })
        } else if self.match_tokens([TokenType::Var]) {
            self.parse_var_decl()
        } else {
            self.parse_statement()
        };

        match result {
            Ok(stmt) => Some(stmt),
            Err(error) => {
                error.report();
                self.synchronize();
                None
            }
        }
    }

    /// function → IDENTIFIER "(" parameters? ")" block ;
    /// parameters → IDENTIFIER ( "," IDENTIFIER )* ;
    fn parse_function(&mut self, kind: FunDeclKind) -> ParseResult<'s, WithSpan<FunctionOrMethod>> {
        let previous_span = if kind == FunDeclKind::Function {
            self.previous().map(|token| token.span)
        } else {
            None
        };

        let name = {
            let Token { lexeme, span, .. } = self.consume(
                TokenType::Identifier,
                previous_span,
                format!("Expected {kind} name"),
            )?;
            WithSpan {
                span,
                value: lexeme.to_string(),
            }
        };

        let mut span = name.span;
        if let Some(prev) = previous_span {
            span.mut_union(&prev);
        }

        span.mut_union(
            &self
                .consume(
                    TokenType::LeftParen,
                    Some(span),
                    format!("Expected '(' after {kind} name"),
                )?
                .span,
        );

        let mut parameters: Vec<WithSpan<String>> = Vec::new();
        let mut reported_max_params_error = false;

        macro_rules! get_declaration_and_params_span {
            () => {
                parameters
                    .iter()
                    .map(|x| x.span)
                    .fold(span, |acc, span| acc.union(&span))
            };
        }

        if !self.check(TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 && !reported_max_params_error {
                    report_non_runtime_error(
                        get_declaration_and_params_span!(),
                        &format!("Cannot have more than 255 parameters in a {kind} declaration"),
                    );
                    reported_max_params_error = true;
                }

                let Token {
                    lexeme,
                    span: token_span,
                    ..
                } = self.consume(
                    TokenType::Identifier,
                    Some(get_declaration_and_params_span!()),
                    "Expected parameter name".to_string(),
                )?;

                parameters.push(WithSpan {
                    span: token_span,
                    value: lexeme.to_string(),
                });

                if !self.match_tokens([TokenType::Comma]) {
                    break;
                }
            }
        }

        let right_paren = self
            .consume(
                TokenType::RightParen,
                Some(span),
                format!("Expected ')' after {kind} parameters"),
            )?
            .span;

        span.mut_union(
            &self
                .consume(
                    TokenType::LeftBrace,
                    Some(span),
                    format!("Expected '{{' before {kind} body"),
                )?
                .span,
        );

        let stmts = self.parse_block()?;
        span.mut_union(&stmts.span);

        Ok(WithSpan {
            span,
            value: (name, parameters, right_paren, stmts.value),
        })
    }

    /// classDecl → "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
    fn parse_class_decl(&mut self) -> ParseResult<'s, SpanStmt> {
        let class_keyword_span = self.previous().unwrap().span;
        let name = {
            let Token { lexeme, span, .. } = self.consume(
                TokenType::Identifier,
                Some(class_keyword_span),
                "Expected identifier after 'class' keyword".to_string(),
            )?;
            WithSpan {
                span,
                value: lexeme.to_string(),
            }
        };

        let superclass_name = if self.match_tokens([TokenType::Less]) {
            let Token { lexeme, span, .. } = self.consume(
                TokenType::Identifier,
                self.previous().map(|t| t.span),
                "Expected superclass name after '<'".to_string(),
            )?;
            Some(WithSpan {
                span,
                value: lexeme.to_string(),
            })
        } else {
            None
        };

        let left_brace_span = self
            .consume(
                TokenType::LeftBrace,
                Some(class_keyword_span.union(&name.span)),
                "Expected '{' before class body".to_string(),
            )?
            .span;

        let mut methods = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.parse_function(FunDeclKind::Method)?);
        }

        let right_brace_span = self
            .consume(
                TokenType::RightBrace,
                Some(
                    class_keyword_span.union(if let Some(method) = methods.last() {
                        &method.span
                    } else {
                        &left_brace_span
                    }),
                ),
                "Expected '}' after class body".to_string(),
            )?
            .span;

        Ok(WithSpan {
            span: class_keyword_span.union(&right_brace_span),
            value: Stmt::ClassDecl(name, superclass_name, methods),
        })
    }

    /// varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn parse_var_decl(&mut self) -> ParseResult<'s, SpanStmt> {
        let var_keyword_span = self.previous().unwrap().span;
        let name = self.consume(
            TokenType::Identifier,
            Some(var_keyword_span),
            "Expected variable name after 'var' keyword".to_string(),
        )?;

        let expr = if self.match_tokens([TokenType::Equal]) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let mut span = match expr {
            Some(WithSpan { span, value: _ }) => var_keyword_span.union(&span),
            _ => var_keyword_span,
        };

        span.mut_union(
            &self
                .consume(
                    TokenType::Semicolon,
                    Some(span),
                    "Expected ';' after variable declaration".to_string(),
                )?
                .span,
        );

        let value = Stmt::VarDecl(
            WithSpan {
                span: name.span,
                value: name.lexeme.to_string(),
            },
            expr,
        );
        Ok(WithSpan { span, value })
    }

    /// statement → exprStmt | ifStmt | printStmt | returnStmt | whileStmt | forStmt | block ;
    fn parse_statement(&mut self) -> ParseResult<'s, SpanStmt> {
        if self.match_tokens([TokenType::If]) {
            self.parse_if_statement()
        } else if self.match_tokens([TokenType::Print]) {
            self.parse_print_statement()
        } else if self.match_tokens([TokenType::Return]) {
            self.parse_return_statement()
        } else if self.match_tokens([TokenType::While]) {
            self.parse_while_loop()
        } else if self.match_tokens([TokenType::For]) {
            self.parse_for_loop()
        } else if self.match_tokens([TokenType::LeftBrace]) {
            self.parse_block().map(|WithSpan { span, value }| WithSpan {
                span,
                value: Stmt::Block(value),
            })
        } else {
            self.parse_expr_statement()
        }
    }

    /// exprStmt → expression ";" ;
    fn parse_expr_statement(&mut self) -> ParseResult<'s, SpanStmt> {
        let value = self.parse_expression()?;
        let semicolon_span = self
            .consume(
                TokenType::Semicolon,
                Some(value.span),
                "Expected ';' after expression".to_string(),
            )?
            .span;

        let span = value.span.union(&semicolon_span);
        let value = Stmt::Expression(value);
        Ok(WithSpan { span, value })
    }

    /// ifStmt → "if" "(" expression ")" statement ( "else" statement )? ;
    fn parse_if_statement(&mut self) -> ParseResult<'s, SpanStmt> {
        let mut full_span = self.previous().unwrap().span;

        let left_paren_span = self
            .consume(
                TokenType::LeftParen,
                Some(full_span),
                "Expected '(' after 'if'".to_string(),
            )?
            .span;
        full_span.mut_union(&left_paren_span);

        let condition = self.parse_expression()?;
        full_span.mut_union(&condition.span);

        let right_paren_span = self
            .consume(
                TokenType::RightParen,
                Some(full_span),
                "Expected ')' after if condition".to_string(),
            )?
            .span;
        full_span.mut_union(&right_paren_span);

        let then_branch = self.parse_statement()?;
        full_span.mut_union(&then_branch.span);

        let else_branch = if self.match_tokens([TokenType::Else]) {
            let stmt = self.parse_statement()?;
            full_span.mut_union(&stmt.span);
            Some(stmt)
        } else {
            None
        };

        Ok(WithSpan {
            span: full_span,
            value: Stmt::If(condition, Box::new(then_branch), else_branch.map(Box::new)),
        })
    }

    /// printStmt → "print" expression ";" ;
    fn parse_print_statement(&mut self) -> ParseResult<'s, SpanStmt> {
        let print_keyword_span = self.previous().unwrap().span;
        let value = self.parse_expression()?;
        let semicolon_span = self
            .consume(
                TokenType::Semicolon,
                Some(print_keyword_span),
                "Expected ';' after value".to_string(),
            )?
            .span;

        let span = print_keyword_span.union(&semicolon_span);
        let value = Stmt::Print(value);
        Ok(WithSpan { span, value })
    }

    /// returnStmt → "return" expression? ";" ;
    fn parse_return_statement(&mut self) -> ParseResult<'s, SpanStmt> {
        let keyword_span = self.previous().unwrap().span;

        let expr = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let semicolon_span = self
            .consume(
                TokenType::Semicolon,
                Some(if let Some(expr) = &expr {
                    expr.span.union(&keyword_span)
                } else {
                    keyword_span
                }),
                "Expected ';' after return value".to_string(),
            )?
            .span;

        Ok(WithSpan {
            span: keyword_span.union(&semicolon_span),
            value: Stmt::Return(keyword_span, expr),
        })
    }

    /// whileStmt → "while" "(" expression ")" statement ;
    fn parse_while_loop(&mut self) -> ParseResult<'s, SpanStmt> {
        let mut span = self.previous().unwrap().span;

        let left_paren_span = self
            .consume(
                TokenType::LeftParen,
                Some(span),
                "Expected '(' after 'while'".to_string(),
            )?
            .span;
        span.mut_union(&left_paren_span);

        let condition = self.parse_expression()?;
        span.mut_union(&condition.span);

        let right_paren_span = self
            .consume(
                TokenType::RightParen,
                Some(span),
                "Expected ')' after while condition".to_string(),
            )?
            .span;
        span.mut_union(&right_paren_span);

        let stmt = self.parse_statement()?;
        span.mut_union(&stmt.span);

        Ok(WithSpan {
            span,
            value: Stmt::While(condition, Box::new(stmt)),
        })
    }

    /// forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
    ///
    /// This method desugars the for loop to a [while loop](Stmt::While) automatically.
    fn parse_for_loop(&mut self) -> ParseResult<'s, SpanStmt> {
        let mut span = self.previous().unwrap().span;

        self.consume(
            TokenType::LeftParen,
            Some(span),
            "Expected '(' after 'for'".to_string(),
        )?;

        let initializer: Option<SpanStmt> = if self.match_tokens([TokenType::Semicolon]) {
            None
        } else if self.match_tokens([TokenType::Var]) {
            Some(self.parse_var_decl()?)
        } else {
            Some(self.parse_expr_statement()?)
        };
        if let Some(init) = &initializer {
            span.mut_union(&init.span);
        }

        let (condition, cond_span) = if !self.check(TokenType::Semicolon) {
            let expr = self.parse_expression()?;
            let expr_span = expr.span;
            span.mut_union(&expr.span);
            (Some(expr), expr_span)
        } else {
            let end = self.previous().unwrap().span.end;
            (None, Span { start: end, end })
        };
        self.consume(
            TokenType::Semicolon,
            Some(span),
            "Expected ';' after for loop condition".to_string(),
        )?;

        let increment = if !self.check(TokenType::RightParen) {
            let expr = self.parse_expression()?;
            span.mut_union(&expr.span);
            Some(expr)
        } else {
            None
        };
        self.consume(
            TokenType::RightParen,
            Some(span),
            "Expected ')' after for loop clauses".to_string(),
        )?;

        let mut body = self.parse_statement()?;
        span.mut_union(&body.span);

        if let Some(increment) = increment {
            let increment: SpanStmt = WithSpan {
                span: increment.span,
                value: Stmt::Expression(increment),
            };
            body = WithSpan {
                span: body.span.union(&increment.span),
                value: Stmt::Block(vec![body, increment]),
            };
        }

        let condition = condition.unwrap_or_else(|| WithSpan {
            span: cond_span,
            value: Expr::Boolean(true),
        });
        body = WithSpan {
            span: body.span.union(&condition.span),
            value: Stmt::While(condition, Box::new(body)),
        };

        if let Some(initializer) = initializer {
            body = WithSpan {
                span: initializer.span.union(&body.span),
                value: Stmt::Block(vec![initializer, body]),
            };
        }

        body.span = span;

        Ok(body)
    }

    /// block → "{" declaration* "}" ;
    ///
    /// This function does not return a [`Stmt::Block`] but just the span of the block including
    /// braces, and the statements inside the block. It is easily wrapped into a [`Stmt::Block`] by
    /// [`parse_statement`](Self::parse_statement).
    fn parse_block(&mut self) -> ParseResult<'s, WithSpan<Vec<SpanStmt>>> {
        let mut stmts = Vec::new();
        let mut span = self.previous().unwrap().span;

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if let Some(stmt) = self.parse_declaration() {
                span.mut_union(&stmt.span);
                stmts.push(stmt);
            }
        }

        let right_brace_span = self
            .consume(
                TokenType::RightBrace,
                Some(span),
                "Expected '}' after block".to_string(),
            )?
            .span;
        span.mut_union(&right_brace_span);

        Ok(WithSpan { span, value: stmts })
    }
}
