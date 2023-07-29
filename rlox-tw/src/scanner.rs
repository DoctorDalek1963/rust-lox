//! This module handles scanning source code to produce tokens.

use crate::{
    lox::report_error,
    tokens::{Token, TokenLiteral, TokenType},
};

/// A scanner to get tokens from source code.
pub struct Scanner<'s> {
    /// The source code.
    source: &'s str,

    /// The tokens that we've already scanned out.
    tokens: Vec<Token<'s>>,

    /// An index to the start of the token currently being scanned.
    start: usize,

    /// An index to the character currently being considered.
    current: usize,

    /// The line that we're currently on.
    line: usize,

    /// The index of the most recently seen newline character.
    last_newline: usize,
}

impl<'s> Scanner<'s> {
    /// Scan all the tokens from the given source code.
    pub fn scan_tokens(source: &'s str) -> Vec<Token<'s>> {
        let mut scanner = Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            last_newline: 0,
        };

        while !scanner.is_at_end() {
            scanner.start = scanner.current;
            scanner.scan_token();
        }

        scanner.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "",
            literal: None,
            line: scanner.line,
            col_start: 1,
            length: 0,
        });

        scanner.tokens
    }

    /// Are we at the end of the source code?
    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Scan a single token.
    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen, None),
            ')' => self.add_token(TokenType::RightParen, None),
            '{' => self.add_token(TokenType::LeftBrace, None),
            '}' => self.add_token(TokenType::RightBrace, None),
            ',' => self.add_token(TokenType::Comma, None),
            '.' => self.add_token(TokenType::Dot, None),
            '-' => self.add_token(TokenType::Minus, None),
            '+' => self.add_token(TokenType::Plus, None),
            ';' => self.add_token(TokenType::Semicolon, None),
            '*' => self.add_token(TokenType::Star, None),

            '/' => {
                if self.match_char('/') {
                    while self.current_char() != Some('\n') && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash, None);
                }
            }
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type, None)
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type, None)
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type, None)
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type, None)
            }

            '\n' => {
                self.line += 1;
                self.last_newline = self.current;
            }
            ' ' | '\t' | '\r' => {}

            '"' => self.scan_string(),

            '0'..='9' => self.scan_number(),

            c if c.is_ascii_alphabetic() => self.scan_identifier_or_keyword(),

            _ => report_error(
                self.line,
                self.current - self.last_newline,
                self.current - self.last_newline,
                &format!("Unrecognied character: {c:?}"),
            ),
        }
    }

    /// Return the char pointed to by `self.current`.
    #[inline]
    fn current_char(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    /// Return the char after the one pointed to by `self.current`.
    #[inline]
    fn next_char(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    /// Advance the internal pointer.
    fn advance(&mut self) -> char {
        let c = self.current_char().unwrap_or_else(|| {
            panic!(
                "source: {:?}, current: {}, tokens: {:?}",
                self.source, self.current, self.tokens
            )
        });
        self.current += 1;
        return c;
    }

    /// Add a token with the given token type and literal to the internal token vec.
    fn add_token(&mut self, token_type: TokenType, literal: Option<TokenLiteral<'s>>) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token {
            token_type,
            lexeme,
            literal,
            line: self.line,
            col_start: self.start - self.last_newline,
            length: self.current - self.start,
        });
    }

    /// Conditionally [`advance`] if the next char is the expected one.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || (self.current_char() != Some(expected)) {
            false
        } else {
            self.current += 1;
            true
        }
    }

    /// Scan a string literal.
    fn scan_string(&mut self) {
        while self.current_char() != Some('"') && !self.is_at_end() {
            if self.current_char() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            report_error(
                // Unterminated string literals end in a newline, so we have to ignore that one
                self.line - 1,
                self.start - self.last_newline + 1,
                self.current - self.last_newline - 1,
                "Unterminated string literal",
            );
            return;
        }

        // The closing "
        self.advance();

        self.add_token(
            TokenType::String,
            Some(TokenLiteral::String(
                // Trim the surrounding quotes
                &self.source[(self.start + 1)..(self.current - 1)],
            )),
        )
    }

    /// Scan a numeric literal
    fn scan_number(&mut self) {
        while self.current_char().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }

        if self.current_char() == Some('.') && self.next_char().is_some_and(|c| c.is_ascii_digit())
        {
            self.advance();
            while self.current_char().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }

        self.add_token(
            TokenType::Number,
            Some(TokenLiteral::Number(
                self.source[self.start..self.current].parse().unwrap(),
            )),
        );
    }

    /// Scan a single identifier or keyword.
    fn scan_identifier_or_keyword(&mut self) {
        fn is_ident_char(c: Option<char>) -> bool {
            match c {
                None => false,
                Some(c) => c.is_ascii_alphanumeric() || c == '_',
            }
        }

        while is_ident_char(self.current_char()) {
            self.advance();
        }

        let token_type = match &self.source[self.start..self.current] {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };

        self.add_token(token_type, None);
    }
}
