use std::error::Error as StdError;
use std::fmt;
use std::iter::Peekable;

use crate::lex::{Annot, LexError, LexErrorKind, Loc, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    Num(u64),
    Variable(usize), // usizeはBPからのオフセット
    UniOp {
        op: UniOp,
        e: Box<Ast>,
    },
    BinOp {
        op: BinOp,
        l: Box<Ast>,
        r: Box<Ast>,
    },
    Stmt(Box<Ast>),
    Assign {
        l: Box<Ast>,
        r: Box<Ast>,
    },
    Return(Box<Ast>),
    If {
        cond: Box<Ast>,
        expr: Box<Ast>,
        els: Option<Box<Ast>>,
    },
    While {
        cond: Box<Ast>,
        stmt: Box<Ast>,
    },
    Block(Vec<Ast>),
    For {
        declare: Option<Box<Ast>>,
        cond: Option<Box<Ast>>,
        update: Option<Box<Ast>>,
        stmt: Box<Ast>,
    },
    Fun {
        name: String,
        args: Vec<Ast>,
    },
    FunDeclare {
        name: String,
        args: Vec<Ast>,
        body: Vec<Ast>,
    },
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
    fn make_return(expr: Ast, loc: Loc) -> Self {
        Self::new(AstKind::Return(Box::new(expr)), loc)
    }
    fn make_if(cond: Ast, stmt: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::If {
                cond: Box::new(cond),
                expr: Box::new(stmt),
                els: None,
            },
            loc,
        )
    }
    fn make_if_else(cond: Ast, stmt: Ast, els: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::If {
                cond: Box::new(cond),
                expr: Box::new(stmt),
                els: Some(Box::new(els)),
            },
            loc,
        )
    }
    fn make_block(v: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::Block(v), loc)
    }
    fn make_while(cond: Ast, stmt: Ast, loc: Loc) -> Self {
        Self::new(
            AstKind::While {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    fn make_for(
        declare: Option<Ast>,
        cond: Option<Ast>,
        update: Option<Ast>,
        stmt: Ast,
        loc: Loc,
    ) -> Self {
        Self::new(
            AstKind::For {
                declare: declare.map(|ast| Box::new(ast)),
                cond: cond.map(|ast| Box::new(ast)),
                update: update.map(|ast| Box::new(ast)),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    fn make_function(name: String, args: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::Fun { name, args }, loc)
    }
    fn dec_fun(name: String, args: Vec<Ast>, body: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstKind::FunDeclare { name, args, body }, loc)
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

macro_rules! expect_token {
    ($tokens: expr, $token: pat) => {
        match $tokens.next() {
            None => return Err(ParseError::Eof),
            Some(tok) => match tok.value.clone() {
                $token => (),
                _ => return Err(ParseError::UnexpectedToken(tok.clone())),
            },
        }
    };
}
macro_rules! expect_semicolon {
    ($tokens: expr) => {
        match $tokens.next() {
            None => return Err(ParseError::Eof),
            Some(tok) => match tok.value.clone() {
                TokenKind::Semicolon => (),
                _ => return Err(ParseError::NotSemicolon(tok.clone())),
            },
        }
    };
}
macro_rules! expect_paren_close {
    ($tokens: expr) => {
        match $tokens.next() {
            None => return Err(ParseError::Eof),
            Some(tok) => match tok.value.clone() {
                TokenKind::Rparen => (),
                _ => return Err(ParseError::UnClosedOpenParen(tok.clone())),
            },
        }
    };
}

macro_rules! parse_vectors {
    ($tokens: expr, $vars: expr, $end_token: pat, $sep: pat, $parser: expr) => {{
        let mut v = vec![];
        loop {
            match $tokens.peek() {
                None => return Err(ParseError::Eof),
                Some(token) => match token.value.clone() {
                    $end_token => {
                        $tokens.next().unwrap();
                        break;
                    }
                    $sep => {
                        $tokens.next();
                        continue;
                    }
                    _ => {
                        let stmt = $parser($tokens, $vars)?;
                        v.push(stmt);
                    }
                },
            }
        }
        v
    }};
}

pub fn parse(input: &str) -> Result<Vec<Ast>, Error> {
    use crate::lex::lex;
    let tokens = lex(input).map_err(|e| Error::Lexer(e))?;
    //println!("{:?}", tokens);

    fn parse_body(tokens: Vec<Token>) -> Result<Vec<Ast>, ParseError> {
        let mut tokens = tokens.into_iter().peekable();
        Ok(parse_program(&mut tokens)?)
    }

    Ok(parse_body(tokens).map_err(|e| Error::Parser(e))?)
}

fn parse_program<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Ast>, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut funs = vec![];
    loop {
        match tokens.peek() {
            None => return Ok(funs),
            Some(_) => funs.push(parse_fun(tokens)?),
        }
    }
}

fn parse_fun<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let (funname, mut loc) = match tokens.next() {
        None => return Err(ParseError::Eof),
        Some(tok) => match tok.value.clone() {
            TokenKind::Ident(name) => (name.to_owned(), tok.loc.clone()),
            _ => return Err(ParseError::UnexpectedToken(tok)),
        },
    };
    expect_token!(tokens, TokenKind::Lparen);
    expect_paren_close!(tokens);
    expect_token!(tokens, TokenKind::BlockOpen);
    let mut stmts = vec![];
    let mut vars = vec![];
    loop {
        match tokens.peek() {
            None => return Err(ParseError::Eof),
            Some(tok) => match tok.value.clone() {
                TokenKind::BlockClose => break,
                _ => {
                    loc = loc.merge(&tok.loc);
                    let ast = parse_stmt(tokens, &mut vars)?;
                    stmts.push(ast);
                }
            },
        }
    }
    tokens.next();
    Ok(Ast::dec_fun(funname, vec![], stmts, loc))
}

fn parse_stmt<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().ok_or(ParseError::Eof)?.value.clone() {
        TokenKind::Return => {
            let tok_ret = tokens.next().unwrap();
            let exp = parse_expr(tokens, vars)?;
            match tokens.next() {
                None => Err(ParseError::Eof),
                Some(Token {
                    value: TokenKind::Semicolon,
                    loc,
                }) => {
                    let loc = tok_ret.loc.merge(&loc);
                    Ok(Ast::make_return(exp, loc))
                }
                Some(tok) => Err(ParseError::NotSemicolon(tok)),
            }
        }
        TokenKind::Ident(s) if s == "if".to_owned() => {
            let tok = tokens.next().unwrap();

            expect_token!(tokens, TokenKind::Lparen);
            let cond = parse_expr(tokens, vars)?;
            expect_paren_close!(tokens);

            let stmt = parse_stmt(tokens, vars)?;

            match tokens.peek() {
                None => {
                    let loc = tok.loc.merge(&stmt.loc);
                    Ok(Ast::make_if(cond, stmt, loc))
                }
                Some(token) => match token.value.clone() {
                    TokenKind::Ident(s) if s == "else".to_owned() => {
                        tokens.next().unwrap();
                        let els = parse_stmt(tokens, vars)?;
                        let loc = tok.loc.merge(&els.loc);
                        Ok(Ast::make_if_else(cond, stmt, els, loc))
                    }
                    _ => {
                        let loc = tok.loc.merge(&stmt.loc);
                        Ok(Ast::make_if(cond, stmt, loc))
                    }
                },
            }
        }
        TokenKind::Ident(s) if s == "while".to_owned() => {
            let tok = tokens.next().unwrap();
            expect_token!(tokens, TokenKind::Lparen);
            let cond = parse_expr(tokens, vars)?;
            expect_paren_close!(tokens);

            let stmt = parse_stmt(tokens, vars)?;
            let loc = tok.loc.merge(&stmt.loc);
            Ok(Ast::make_while(cond, stmt, loc))
        }
        TokenKind::Ident(s) if s == "for".to_owned() => {
            let tok = tokens.next().unwrap();

            expect_token!(tokens, TokenKind::Lparen);

            let declare = match tokens.peek() {
                None => return Err(ParseError::Eof),
                Some(token) => match token.value.clone() {
                    TokenKind::Semicolon => {
                        tokens.next();
                        None
                    }
                    _ => {
                        let declare = parse_expr(tokens, vars)?;
                        expect_semicolon!(tokens);
                        Some(declare)
                    }
                },
            };

            let cond = match tokens.peek() {
                None => return Err(ParseError::Eof),
                Some(token) => match token.value.clone() {
                    TokenKind::Semicolon => {
                        tokens.next();
                        None
                    }
                    _ => {
                        let cond = parse_expr(tokens, vars)?;
                        expect_semicolon!(tokens);
                        Some(cond)
                    }
                },
            };

            let update = match tokens.peek() {
                None => return Err(ParseError::Eof),
                Some(token) => match token.value.clone() {
                    TokenKind::Rparen => None,
                    _ => Some(parse_expr(tokens, vars)?),
                },
            };

            expect_paren_close!(tokens);

            let stmt = parse_stmt(tokens, vars)?;
            let loc = tok.loc.merge(&stmt.loc);
            Ok(Ast::make_for(declare, cond, update, stmt, loc))
        }
        TokenKind::BlockOpen => {
            let tok = tokens.next().unwrap();
            let mut loc = tok.loc;
            let v = parse_vectors!(
                tokens,
                vars,
                TokenKind::BlockClose,
                TokenKind::Semicolon,
                parse_stmt
            );
            loc = loc.merge(&v[v.len() - 1].loc);
            Ok(Ast::make_block(v, loc))
        }
        _ => {
            let exp = parse_expr(tokens, vars)?;
            expect_semicolon!(tokens);
            Ok(exp)
        }
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
        })
        | Some(Token {
            value: TokenKind::BlockClose,
            ..
        })
        | Some(Token {
            value: TokenKind::Comma,
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
            TokenKind::Ident(s) => match tokens.peek() {
                Some(tok) if tok.value == TokenKind::Lparen => {
                    let mut loc = tok.loc.clone();
                    tokens.next();
                    let args = parse_vectors!(
                        tokens,
                        vars,
                        TokenKind::Rparen,
                        TokenKind::Comma,
                        parse_expr
                    );
                    if args.len() != 0 {
                        loc = loc.merge(&args[args.len() - 1].loc);
                    }
                    Ok(Ast::make_function(s, args, loc))
                }
                _ => {
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
            },
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
            Return => write!(f, "return"),
            BlockOpen => write!(f, "{{"),
            BlockClose => write!(f, "}}"),
            Comma => write!(f, ","),
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
