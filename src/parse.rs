use std::error::Error as StdError;
use std::fmt;
use std::iter::Peekable;

use crate::lex::{Annot, LexError, LexErrorKind, Loc, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(u64),
    Variable(usize), // usizeはBPからのオフセット
    UniOp { op: UniOp, e: Box<Ast> },
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
    Stmt(Box<Ast>),
    Assign { l: Box<Ast>, r: Box<Ast> },
}
pub type Ast = Annot<AstKind>;
impl Ast {
    fn num(n: u64, loc: Loc) -> Self {
        Self::new(AstKind::Num(n), loc)
    }
    fn variable(offset: usize, loc: Loc) -> Self {
        Self::new(AstKind::Variable(offset), loc)
    }
    fn binop(op: BinOp, l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::BinOp {
                op,
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
    fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstKind::UniOp { op, e: Box::new(e) }, loc)
    }
    fn stmt(expr: Ast, loc: Loc) -> Self {
        Self::new(AstKind::Stmt(Box::new(expr)), loc)
    }
    fn assign(l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::Assign {
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
    Minus,
}
type UniOp = Annot<UniOpKind>;
impl UniOp {
    fn plus(loc: Loc) -> Self {
        Self::new(UniOpKind::Plus, loc)
    }
    fn minus(loc: Loc) -> Self {
        Self::new(UniOpKind::Minus, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mult,
    Div,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}
type BinOp = Annot<BinOpKind>;
impl BinOp {
    fn add(loc: Loc) -> Self {
        Self::new(BinOpKind::Add, loc)
    }
    fn sub(loc: Loc) -> Self {
        Self::new(BinOpKind::Sub, loc)
    }
    fn mult(loc: Loc) -> Self {
        Self::new(BinOpKind::Mult, loc)
    }
    fn div(loc: Loc) -> Self {
        Self::new(BinOpKind::Div, loc)
    }
    fn less(loc: Loc) -> Self {
        Self::new(BinOpKind::Less, loc)
    }
    fn less_equal(loc: Loc) -> Self {
        Self::new(BinOpKind::LessEqual, loc)
    }
    fn greater(loc: Loc) -> Self {
        Self::new(BinOpKind::Greater, loc)
    }
    fn greater_equal(loc: Loc) -> Self {
        Self::new(BinOpKind::GreaterEqual, loc)
    }
    fn equal(loc: Loc) -> Self {
        Self::new(BinOpKind::Equal, loc)
    }
    fn not_equal(loc: Loc) -> Self {
        Self::new(BinOpKind::NotEqual, loc)
    }
}

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
}

pub fn parse(input: &str) -> Result<Vec<Ast>, Error> {
    let tokens = match crate::lex::lex(input) {
        Ok(v) => v,
        Err(e) => return Err(Error::Lexer(e)),
    };
    //println!("{:?}", tokens);
    let res = match parse_body(tokens) {
        Ok(v) => v,
        Err(e) => return Err(Error::Parser(e)),
    };
    Ok(res)
}

fn parse_body(tokens: Vec<Token>) -> Result<Vec<Ast>, ParseError> {
    // peekはイテレータが現在指している値をただ返す
    let mut tokens = tokens.into_iter().peekable();
    let mut res = vec![];
    let mut vars = vec![];
    parse_program(&mut tokens, &mut res, &mut vars)?;
    Ok(res)
}

fn parse_program<Tokens>(
    tokens: &mut Peekable<Tokens>,
    v: &mut Vec<Ast>,
    vars: &mut Vec<(String, usize)>,
) -> Result<(), ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek() {
        Some(_) => {
            let stmt = match parse_stmt(tokens, vars) {
                Ok(stmt) => stmt,
                //Err(ParseError::Eof) => return Ok(()),
                Err(e) => return Err(e),
            };
            v.push(stmt);
            parse_program(tokens, v, vars)?;
            Ok(())
        }
        None => Ok(()),
    }
}

fn parse_stmt<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let exp = parse_expr(tokens, vars)?;
    match tokens.next() {
        None => Err(ParseError::Eof),
        Some(tok) => match tok.value {
            TokenKind::Semicolon => {
                let loc = exp.loc.merge(&tok.loc);
                Ok(Ast::stmt(exp, loc))
            }
            _ => Err(ParseError::NotSemicolon(tok.clone())),
        },
    }
}

fn parse_expr<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    parse_assign(tokens, vars)
}

fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
    subexpr_parser: fn(&mut Peekable<Tokens>, &mut Vec<(String, usize)>) -> Result<Ast, ParseError>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp, ParseError>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut e = subexpr_parser(tokens, vars)?;
    loop {
        match tokens.peek() {
            Some(_) => {
                let op = match op_parser(tokens) {
                    Ok(op) => op,
                    Err(_) => break,
                };
                let r = subexpr_parser(tokens, vars)?;
                let loc = e.loc.merge(&r.loc);
                e = Ast::binop(op, e, r, loc)
            }
            _ => break,
        }
    }
    Ok(e)
}

fn parse_assign<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let equal_tok = tokens.peek().ok_or(ParseError::Eof)?.clone();
    let equality = parse_equality(tokens, vars)?;
    match tokens.peek() {
        None
        | Some(Token {
            value: TokenKind::Semicolon,
            ..
        })
        | Some(Token {
            value: TokenKind::Rparen,
            ..
        }) => Ok(Ast::stmt(equality.clone(), equality.loc.clone())),
        Some(tok) => match tok.value {
            TokenKind::Equal => {
                if let AstKind::Variable(_) = equality.value {
                    tokens.next();
                } else {
                    return Err(ParseError::NotAddressExp(equal_tok.clone()));
                }
                let rvalue = parse_assign(tokens, vars)?;
                let loc = equality.loc.merge(&rvalue.loc);
                Ok(Ast::assign(equality, rvalue, loc))
            }
            _ => Err(ParseError::NotExpression(tok.clone())),
        },
    }
}

fn parse_equality<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr5_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::DoubleEqual => Ok(BinOp::equal(tok.loc.clone())),
                TokenKind::NotEqual => Ok(BinOp::not_equal(tok.loc.clone())),
                _ => Err(ParseError::NotOperator(tok.clone())),
            })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_relational, parse_expr5_op)
}

fn parse_relational<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr4_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::Less => Ok(BinOp::less(tok.loc.clone())),
                TokenKind::LessEqual => Ok(BinOp::less_equal(tok.loc.clone())),
                TokenKind::Greater => Ok(BinOp::greater(tok.loc.clone())),
                TokenKind::GreaterEqual => Ok(BinOp::greater_equal(tok.loc.clone())),
                _ => Err(ParseError::NotOperator(tok.clone())),
            })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr3, parse_expr4_op)
}

fn parse_expr3<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr3_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::Plus => Ok(BinOp::add(tok.loc.clone())),
                TokenKind::Minus => Ok(BinOp::sub(tok.loc.clone())),
                _ => Err(ParseError::NotOperator(tok.clone())),
            })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr2, parse_expr3_op)
}

fn parse_expr2<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr2_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp, ParseError>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens
            .peek()
            .ok_or(ParseError::Eof)
            .and_then(|tok| match tok.value {
                TokenKind::Asterisk => Ok(BinOp::mult(tok.loc.clone())),
                TokenKind::Slash => Ok(BinOp::div(tok.loc.clone())),
                _ => Err(ParseError::NotOperator(tok.clone())),
            })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr1, parse_expr2_op)
}

fn parse_expr1<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|tok| tok.value.clone()) {
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
            let op = match tokens.next() {
                Some(Token {
                    value: TokenKind::Plus,
                    loc,
                }) => UniOp::plus(loc),
                Some(Token {
                    value: TokenKind::Minus,
                    loc,
                }) => UniOp::minus(loc),
                _ => unreachable!(),
            };
            let e = parse_atom(tokens, vars)?;
            let loc = e.loc.merge(&op.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_atom(tokens, vars),
    }
}

fn parse_atom<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value.clone() {
            TokenKind::Num(n) => Ok(Ast::num(n, tok.loc)),
            TokenKind::Ident(s) => {
                let max = vars.iter().find(|&(name, _)| s == *name);
                if max.is_some() {
                    let offset = max.unwrap().1;
                    Ok(Ast::variable(offset, tok.loc))
                } else {
                    let offset = match vars.iter().max_by_key(|(_, offset)| offset) {
                        None => 8,
                        Some((_, offset)) => offset + 8,
                    };
                    vars.push((s, offset));
                    Ok(Ast::variable(offset, tok.loc))
                }
            }
            TokenKind::Lparen => {
                let e = parse_expr(tokens, vars)?;
                match tokens.next() {
                    Some(Token {
                        value: TokenKind::Rparen,
                        ..
                    }) => Ok(e),
                    Some(t) => Err(ParseError::RebundantExpression(t.clone())),
                    _ => Err(ParseError::UnClosedOpenParen(tok)),
                }
            }
            _ => Err(ParseError::NotExpression(tok)),
        })
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
            RebundantExpression(tok) => write!(
                f,
                "{}: expression after '{}' is rebundant",
                tok.loc, tok.value
            ),
            Eof => write!(f, "End of file"),
        }
    }
}