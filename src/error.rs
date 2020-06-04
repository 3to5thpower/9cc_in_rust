use crate::lex::{LexError, LexErrorKind, Loc, Token, TokenKind};
use std::error::Error as StdError;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    RebundantExpression(Token),
    UnClosedOpenParen(Token),
    Eof,
    NotSemicolon(Token),
    NotAddressExp(Token),
    NotDefinedExp(Token),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    Lexer(LexError),
    Parser(ParseError),
}
impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error::Lexer(e)
    }
}
impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parser(e)
    }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parser error")
    }
}
impl StdError for LexError {}
impl StdError for ParseError {}
impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        use self::Error::*;
        match self {
            Lexer(lex) => Some(lex),
            Parser(parse) => Some(parse),
        }
    }
}
fn print_annot(input: &str, loc: Loc) {
    eprintln!("{}", input);
    eprintln!("{}{}", " ".repeat(loc.0), "^".repeat(loc.len()));
}
impl Error {
    pub fn show_diagnostic(&self, input: &str) {
        use self::Error::*;
        use self::ParseError as P;
        let (e, loc): (&dyn StdError, Loc) = match self {
            Lexer(e) => (e, e.loc.clone()),
            Parser(e) => {
                let loc = match e {
                    P::UnexpectedToken(Token { loc, .. })
                    | P::NotAddressExp(Token { loc, .. })
                    | P::NotExpression(Token { loc, .. })
                    | P::NotSemicolon(Token { loc, .. })
                    | P::NotOperator(Token { loc, .. })
                    | P::NotDefinedExp(Token { loc, .. })
                    | P::UnClosedOpenParen(Token { loc, .. }) => loc.clone(),
                    P::RebundantExpression(Token { loc, .. }) => Loc(loc.0, input.len() + 1),
                    P::Eof => Loc(input.len(), input.len() + 1),
                };
                (e, loc)
            }
        };
        eprintln!("{}", e);
        print_annot(input, loc);
    }
}
pub fn show_trace<E: StdError>(e: E) {
    eprintln!("{}", e);
    let mut source = e.source();
    while let Some(e) = source {
        eprintln!("caused by {}", e);
        source = e.source()
    }
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
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LexErrorKind::*;
        let loc = &self.loc;
        match &self.value {
            InvalidChar(c) => write!(f, "{}: Invalid char '{}'", loc, c),
            Eof => write!(f, "End of file"),
        }
    }
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParseError::*;
        match self {
            UnexpectedToken(tok) => write!(f, "{}: {} is not expected", tok.loc, tok.value),
            NotExpression(tok) => write!(
                f,
                "{}: '{}' is not a start of expression",
                tok.loc, tok.value
            ),
            NotOperator(tok) => write!(f, "{}: '{}' is not an operator", tok.loc, tok.value),
            UnClosedOpenParen(tok) => write!(f, "{}: '{}' is not closed", tok.loc, tok.value),
            NotSemicolon(tok) => write!(f, "{}: not ';' before tokens '{}'", tok.loc, tok.value),
            NotAddressExp(tok) => write!(f, "{}: cannot assign value in '{}'", tok.loc, tok.value),
            NotDefinedExp(tok) => write!(f, "{}: '{}' is not defined variable", tok.loc, tok.value),
            RebundantExpression(tok) => write!(
                f,
                "{}: expression after '{}' is rebundant",
                tok.loc, tok.value
            ),
            Eof => write!(f, "End of file"),
        }
    }
}
