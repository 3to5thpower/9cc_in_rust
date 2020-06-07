use crate::lex::{Loc, Token, TokenKind};
use std::fmt;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("{loc}: Invalid char '{c}'")]
    InvalidChar { c: char, loc: Loc },
    #[error("End of File")]
    Eof(Loc),
}
impl LexError {
    pub fn show_diagnostic(&self, input: &str) {
        use LexError::*;
        let loc = match self {
            Eof(loc) | InvalidChar { loc, .. } => loc.clone(),
        };
        print_annot(input, loc);
    }
}

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("End of File")]
    Eof,
    #[error("{}: '{}' is not expected", .0.loc, .0.value)]
    UnexpectedToken(Token),
    #[error("{}: '{}' is not a start of expression", .0.loc, .0.value)]
    NotExpression(Token),
    #[error("{}: '{}' is not an operator", .0.loc, .0.value)]
    NotOperator(Token),
    #[error("{}: '{}' is not closed", .0.loc, .0.value)]
    UnClosedOpenParen(Token),
    #[error("{}: '{}' is not defined variable", .0.loc, .0.value)]
    NotDefinedExp(Token),
    #[error("{}: ';' needs before tokens '{}'", .0.loc, .0.value)]
    NotSemicolon(Token),
    #[error("{}: expression after '{}' is rebundant", .0.loc, .0.value)]
    RebundantExpression(Token),
    #[error("{}: cannot assign value in '{}'", .0.loc, .0.value)]
    NotAddressExp(Token),
    #[error("{}: type '{}' is not defined type", .0.loc, .0.value)]
    NotType(Token),
}
impl ParseError {
    pub fn show_diagnostic(&self, input: &str) {
        use self::ParseError as P;
        let loc: Loc = match self {
            P::UnexpectedToken(Token { loc, .. })
            | P::NotAddressExp(Token { loc, .. })
            | P::NotExpression(Token { loc, .. })
            | P::NotSemicolon(Token { loc, .. })
            | P::NotOperator(Token { loc, .. })
            | P::NotDefinedExp(Token { loc, .. })
            | P::NotType(Token { loc, .. })
            | P::UnClosedOpenParen(Token { loc, .. }) => loc.clone(),
            P::RebundantExpression(Token { loc, .. }) => Loc(loc.0, input.len() + 1),
            P::Eof => Loc(input.len(), input.len() + 1),
        };
        print_annot(input, loc);
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("parse error")]
    Parser(ParseError),
    #[error("lex error")]
    Lexer(LexError),
}

fn print_annot(input: &str, loc: Loc) {
    eprintln!("{}", input);
    eprintln!("{}{}", " ".repeat(loc.0), "^".repeat(loc.len()));
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenKind::*;
        match self {
            Num(n) => n.fmt(f),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            Lparen => write!(f, "("),
            Rparen => write!(f, ")"),
            DoubleEqual => write!(f, "=="),
            NotEqual => write!(f, "!="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Equal => write!(f, "="),
            Semicolon => write!(f, ";"),
            Ident(s) => s.fmt(f),
            Return => write!(f, "return"),
            BlockOpen => write!(f, "{{"),
            BlockClose => write!(f, "}}"),
            Comma => write!(f, ","),
            Ampersand => write!(f, "&"),
        }
    }
}
impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}
