//! This module lets the [`Parser`] parse statements.

use super::{ParseResult, Parser};
use crate::{
    ast::{SpanStmt, Stmt},
    span::WithSpan,
    tokens::TokenType,
};

impl<'s> Parser<'s> {
    /// declaration → varDecl | statement ;
    pub(super) fn parse_declaration(&mut self) -> Option<SpanStmt> {
        let result = if self.match_tokens([TokenType::Var]) {
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

        let span = match expr {
            Some(WithSpan { span, value: _ }) => var_keyword_span.union(&span),
            _ => var_keyword_span,
        };

        self.consume(
            TokenType::Semicolon,
            Some(span),
            "Expected ';' after variable declaration".to_string(),
        )?;

        let value = Stmt::VarDecl(
            WithSpan {
                span: name.span,
                value: name.lexeme.to_string(),
            },
            expr,
        );
        Ok(WithSpan { span, value })
    }

    /// statement → exprStmt | ifStmt | printStmt | whileStmt | block ;
    fn parse_statement(&mut self) -> ParseResult<'s, SpanStmt> {
        if self.match_tokens([TokenType::If]) {
            self.parse_if_statement()
        } else if self.match_tokens([TokenType::Print]) {
            self.parse_print_statement()
        } else if self.match_tokens([TokenType::While]) {
            self.parse_while_loop()
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
        full_span = full_span.union(&left_paren_span);

        let condition = self.parse_expression()?;
        full_span = full_span.union(&condition.span);

        let right_paren_span = self
            .consume(
                TokenType::RightParen,
                Some(full_span),
                "Expected ')' after if condition".to_string(),
            )?
            .span;
        full_span = full_span.union(&right_paren_span);

        let then_branch = self.parse_statement()?;
        full_span = full_span.union(&then_branch.span);

        let else_branch = if self.match_tokens([TokenType::Else]) {
            let stmt = self.parse_statement()?;
            full_span = full_span.union(&stmt.span);
            Some(stmt)
        } else {
            None
        };

        Ok(WithSpan {
            span: full_span,
            value: Stmt::If(
                condition,
                Box::new(then_branch),
                else_branch.map(|stmt| Box::new(stmt)),
            ),
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
        span = span.union(&left_paren_span);

        let condition = self.parse_expression()?;
        span = span.union(&condition.span);

        let right_paren_span = self
            .consume(
                TokenType::RightParen,
                Some(span),
                "Expected ')' after while condition".to_string(),
            )?
            .span;
        span = span.union(&right_paren_span);

        let stmt = self.parse_statement()?;
        span = span.union(&stmt.span);

        Ok(WithSpan {
            span,
            value: Stmt::While(condition, Box::new(stmt)),
        })
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
                span = span.union(&stmt.span);
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
        span = span.union(&right_brace_span);

        Ok(WithSpan { span, value: stmts })
    }
}
