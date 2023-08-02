//! This module handles scanning source code to produce tokens.

use crate::{
    lox::LINE_OFFSETS,
    span::{LineOffsets, Span},
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
}

impl<'s> Scanner<'s> {
    /// Scan all the tokens from the given source code.
    pub fn scan_tokens(source: &'s str) -> Vec<Token<'s>> {
        *LINE_OFFSETS.write().unwrap() = LineOffsets::new(source);

        let mut scanner = Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
        };

        while !scanner.is_at_end() {
            scanner.start = scanner.current;
            scanner.scan_token();
        }

        scanner.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "",
            literal: None,
            span: Span {
                start: scanner.current,
                end: scanner.current,
            },
        });

        scanner.tokens
    }

    /// Are we at the end of the source code?
    #[inline]
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Get the span from the start of this lexeme to the character most recently consumed.
    #[inline]
    fn current_span(&self) -> Span {
        Span {
            start: self.start,
            end: self.current - 1,
        }
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
                self.add_token(token_type, None);
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type, None);
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type, None);
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type, None);
            }

            ' ' | '\t' | '\r' | '\n' => {}

            '"' => self.scan_string(),

            '0'..='9' => self.scan_number(),

            c if c.is_ascii_alphabetic() => self.scan_identifier_or_keyword(),

            _ => self.report_error(&format!("Unrecognised character: {c:?}")),
        }
    }

    /// Report the given error message with the current span.
    fn report_error(&self, message: &str) {
        crate::lox::report_scanning_error(self.current_span(), message);
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
        c
    }

    /// Add a token with the given token type and literal to the internal token vec.
    fn add_token(&mut self, token_type: TokenType, literal: Option<TokenLiteral<'s>>) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token {
            token_type,
            lexeme,
            literal,
            span: self.current_span(),
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
            self.advance();
        }

        if self.is_at_end() {
            self.report_error("Unterminated string literal");
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
        );
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
        /// Check if the given character is valid to be used in an identifier.
        fn is_ident_char(c: Option<char>) -> bool {
            c.map(|c| c.is_ascii_alphanumeric() || c == '_')
                .unwrap_or(false)
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
