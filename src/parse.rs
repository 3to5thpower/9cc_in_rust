use std::error::Error as StdError;
use std::fmt;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Loc(usize, usize);
impl Loc {
    fn merge(&self, other: &Loc) -> Self {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
    fn len(&self) -> usize {
        self.1 - self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub value: T,
    loc: Loc,
}
impl<T> Annot<T> {
    fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Num(u64),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Rparen,
    Lparen,
    DoubleEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Ident(String),
    Equal,
    Semicolon,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexErrorKind {
    InvalidChar(char),
    Eof,
}

pub type Token = Annot<TokenKind>;
impl Token {
    fn number(n: u64, loc: Loc) -> Self {
        Self::new(TokenKind::Num(n), loc)
    }
    fn plus(loc: Loc) -> Self {
        Self::new(TokenKind::Plus, loc)
    }
    fn minus(loc: Loc) -> Self {
        Self::new(TokenKind::Minus, loc)
    }
    fn asterisk(loc: Loc) -> Self {
        Self::new(TokenKind::Asterisk, loc)
    }
    fn slash(loc: Loc) -> Self {
        Self::new(TokenKind::Slash, loc)
    }
    fn lparen(loc: Loc) -> Self {
        Self::new(TokenKind::Lparen, loc)
    }
    fn rparen(loc: Loc) -> Self {
        Self::new(TokenKind::Rparen, loc)
    }
    fn double_equal(loc: Loc) -> Self {
        Self::new(TokenKind::DoubleEqual, loc)
    }
    fn not_equal(loc: Loc) -> Self {
        Self::new(TokenKind::NotEqual, loc)
    }
    fn less(loc: Loc) -> Self {
        Self::new(TokenKind::Less, loc)
    }
    fn less_equal(loc: Loc) -> Self {
        Self::new(TokenKind::LessEqual, loc)
    }
    fn greater(loc: Loc) -> Self {
        Self::new(TokenKind::Greater, loc)
    }
    fn greater_equal(loc: Loc) -> Self {
        Self::new(TokenKind::GreaterEqual, loc)
    }
    fn ident(name: &str, loc: Loc) -> Self {
        Self::new(TokenKind::Ident(name.to_owned()), loc)
    }
    fn equal(loc: Loc) -> Self {
        Self::new(TokenKind::Equal, loc)
    }
    fn semicolon(loc: Loc) -> Self {
        Self::new(TokenKind::Semicolon, loc)
    }
}

pub type LexError = Annot<LexErrorKind>;
impl LexError {
    fn invalid_char(c: char, loc: Loc) -> Self {
        Self::new(LexErrorKind::InvalidChar(c), loc)
    }
    fn eof(loc: Loc) -> Self {
        Self::new(LexErrorKind::Eof, loc)
    }
}

fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut res = vec![];
    let input = input.as_bytes();
    let mut pos = 0;

    macro_rules! lex_a_token {
        ($lexer: expr) => {{
            let (tok, p) = $lexer?;
            res.push(tok);
            pos = p;
            continue;
        }};
    }

    while pos < input.len() {
        match input[pos] {
            b'<' => lex_a_token!(lex_less(input, pos)),
            b'>' => lex_a_token!(lex_greater(input, pos)),
            b'!' => lex_a_token!(lex_not_equal(input, pos)),
            b'=' => lex_a_token!(lex_equal(input, pos)),
            b'+' => lex_a_token!(
                consume_byte(&input, pos, b'+').map(|p| (Token::plus(Loc(pos, p.1)), p.1))
            ),
            b'-' => lex_a_token!(
                consume_byte(&input, pos, b'-').map(|p| (Token::minus(Loc(pos, p.1)), p.1))
            ),
            b'*' => lex_a_token!(
                consume_byte(&input, pos, b'*').map(|p| (Token::asterisk(Loc(pos, p.1)), p.1))
            ),
            b'/' => lex_a_token!(
                consume_byte(&input, pos, b'/').map(|p| (Token::slash(Loc(pos, p.1)), p.1))
            ),
            b'(' => lex_a_token!(
                consume_byte(&input, pos, b'(').map(|p| (Token::lparen(Loc(pos, p.1)), p.1))
            ),
            b')' => lex_a_token!(
                consume_byte(&input, pos, b')').map(|p| (Token::rparen(Loc(pos, p.1)), p.1))
            ),
            b';' => lex_a_token!(
                consume_byte(&input, pos, b';').map(|p| (Token::semicolon(Loc(pos, p.1)), p.1))
            ),
            b'0'..=b'9' => lex_a_token!(lex_number(input, pos)),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => lex_a_token!(lex_ident(input, pos)),
            b' ' | b'\n' | b'\t' => pos += 1,
            b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
        }
    }
    Ok(res)
}

fn consume_byte(input: &[u8], pos: usize, b: u8) -> Result<(u8, usize), LexError> {
    if input.len() <= pos {
        return Err(LexError::eof(Loc(pos, pos)));
    }
    if input[pos] != b {
        return Err(LexError::invalid_char(
            input[pos] as char,
            Loc(pos, pos + 1),
        ));
    }
    Ok((b, pos + 1))
}
fn recognize_many(input: &[u8], mut pos: usize, mut f: impl FnMut(u8) -> bool) -> usize {
    while pos < input.len() && f(input[pos]) {
        pos += 1;
    }
    pos
}

fn lex_number(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;
    let start = pos;
    let end = recognize_many(input, start, |b| b"1234567890".contains(&b));
    let n = from_utf8(&input[start..end]).unwrap().parse().unwrap();
    Ok((Token::number(n, Loc(start, end)), end))
}
fn lex_ident(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    use std::str::from_utf8;
    let start = pos;
    let end = recognize_many(input, start, |b| (b as char).is_alphanumeric() || b == b'_');
    let s = from_utf8(&input[start..end]).unwrap();
    Ok((Token::ident(s, Loc(start, end)), end))
}

fn lex_less(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    let (_, pos2) = consume_byte(input, pos, b'<')?;
    if pos2 < input.len() && input[pos2] == b'=' {
        return Ok((Token::less_equal(Loc(pos, pos2 + 1)), pos2 + 1));
    }
    Ok((Token::less(Loc(pos, pos2)), pos2))
}
fn lex_greater(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    let (_, pos2) = consume_byte(input, pos, b'>')?;
    if pos2 < input.len() && input[pos2] == b'=' {
        return Ok((Token::greater_equal(Loc(pos, pos2 + 1)), pos2 + 1));
    }
    Ok((Token::greater(Loc(pos, pos2)), pos2))
}
fn lex_not_equal(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    let (_, pos2) = consume_byte(input, pos, b'!')?;
    let (_, end) = consume_byte(input, pos2, b'=')?;
    Ok((Token::not_equal(Loc(pos, end)), end))
}
fn lex_equal(input: &[u8], pos: usize) -> Result<(Token, usize), LexError> {
    let (_, pos2) = consume_byte(input, pos, b'=')?;
    if pos2 < input.len() && input[pos2] == b'=' {
        return Ok((Token::double_equal(Loc(pos, pos2 + 1)), pos2 + 1));
    }
    Ok((Token::equal(Loc(pos, pos2)), pos2))
}

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
}

pub fn parse(input: &str) -> Result<Vec<Ast>, Error> {
    let tokens = match lex(input) {
        Ok(v) => v,
        Err(e) => return Err(Error::Lexer(e)),
    };
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
    parse_program(&mut tokens, &mut res)?;
    Ok(res)
}

fn parse_program<Tokens>(tokens: &mut Peekable<Tokens>, v: &mut Vec<Ast>) -> Result<(), ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek() {
        Some(tok) => {
            let stmt = parse_stmt(tokens)?;
            v.push(stmt);
            parse_program(tokens, v)?;
            Ok(())
        }
        None => Ok(()),
    }
}

fn parse_stmt<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let exp = parse_expr(tokens)?;
    match tokens.peek() {
        None => Err(ParseError::Eof),
        Some(tok) => {
            let e = parse_expr(tokens)?;
            match tokens.peek() {
                Some(tok) => match tok.value.clone() {
                    TokenKind::Semicolon => {
                        let loc = e.loc.merge(&tok.loc);
                        Ok(Ast::stmt(e, loc))
                    }
                    _ => Err(ParseError::NotSemicolon(tok.clone())),
                },
                _ => Err(ParseError::NotSemicolon(tok.clone())),
            }
        }
    }
}

fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    parse_assign(tokens)
}

fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    subexpr_parser: fn(&mut Peekable<Tokens>) -> Result<Ast, ParseError>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp, ParseError>,
) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let mut e = subexpr_parser(tokens)?;
    loop {
        match tokens.peek() {
            Some(_) => {
                let op = match op_parser(tokens) {
                    Ok(op) => op,
                    Err(_) => break,
                };
                let r = subexpr_parser(tokens)?;
                let loc = e.loc.merge(&r.loc);
                e = Ast::binop(op, e, r, loc)
            }
            _ => break,
        }
    }
    Ok(e)
}

fn parse_assign<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    let equality = parse_equality(tokens)?;
    match tokens.peek() {
        None => Ok(Ast::stmt(equality, equality.loc)),
        Some(tok) => match tokens.next().unwrap().value {
            TokenKind::Equal => {
                let rvalue = parse_assign(tokens)?;
                let loc = equality.loc.merge(&rvalue.loc);
                Ok(Ast::assign(equality, rvalue, loc))
            }
            _ => Err(ParseError::NotExpression(tok.clone())),
        },
    }
}

fn parse_equality<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
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
    parse_left_binop(tokens, parse_relational, parse_expr5_op)
}

fn parse_relational<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
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
    parse_left_binop(tokens, parse_expr3, parse_expr4_op)
}

fn parse_expr3<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
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
    parse_left_binop(tokens, parse_expr2, parse_expr3_op)
}

fn parse_expr2<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
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
    parse_left_binop(tokens, parse_expr1, parse_expr2_op)
}

fn parse_expr1<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
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
            let e = parse_atom(tokens)?;
            let loc = e.loc.merge(&op.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_atom(tokens),
    }
}

fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|tok| match tok.value {
            TokenKind::Num(n) => Ok(Ast::num(n, tok.loc)),
            TokenKind::Ident(s) => match s.len() {
                1 => {
                    let n = (s.as_bytes()[0] as u8 - 'a' as u8 + 1) as usize * 8;
                    Ok(Ast::variable(n, tok.loc))
                }
                _ => Err(ParseError::NotExpression(tok)),
            },
            TokenKind::Lparen => {
                let e = parse_expr(tokens)?;
                match tokens.next() {
                    Some(Token {
                        value: TokenKind::Rparen,
                        ..
                    }) => Ok(e),
                    Some(t) => Err(ParseError::RebundantExpression(t)),
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
                    | P::NotExpression(Token { loc, .. })
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
            RebundantExpression(tok) => write!(
                f,
                "{}: expression after '{}' is rebundant",
                tok.loc, tok.value
            ),
            Eof => write!(f, "End of file"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_lexer() {
        let input = "1 + 2 * 3 - -10";
        assert_eq!(
            lex(input),
            Ok(vec![
                Token::number(1, Loc(0, 1)),
                Token::plus(Loc(2, 3)),
                Token::number(2, Loc(4, 5)),
                Token::asterisk(Loc(6, 7)),
                Token::number(3, Loc(8, 9)),
                Token::minus(Loc(10, 11)),
                Token::minus(Loc(12, 13)),
                Token::number(10, Loc(13, 15)),
            ])
        );
    }

    #[test]
    fn test_parser() {
        let ast = parse(vec![
            Token::number(1, Loc(0, 1)),
            Token::plus(Loc(2, 3)),
            Token::number(2, Loc(4, 5)),
            Token::asterisk(Loc(6, 7)),
            Token::number(3, Loc(8, 9)),
            Token::minus(Loc(10, 11)),
            Token::minus(Loc(12, 13)),
            Token::number(10, Loc(13, 15)),
        ]);

        assert_eq!(
            ast,
            Ok(vec![Ast::binop(
                BinOp::sub(Loc(10, 11)),
                Ast::binop(
                    BinOp::add(Loc(2, 3)),
                    Ast::num(1, Loc(0, 1)),
                    Ast::binop(
                        BinOp::new(BinOpKind::Mult, Loc(6, 7)),
                        Ast::num(2, Loc(4, 5)),
                        Ast::num(3, Loc(8, 9)),
                        Loc(4, 9)
                    ),
                    Loc(0, 9),
                ),
                Ast::uniop(
                    UniOp::minus(Loc(12, 13)),
                    Ast::num(10, Loc(13, 15)),
                    Loc(12, 15)
                ),
                Loc(0, 15)
            )])
        );
    }

    #[test]
    fn lex_parse() {
        let input = "1 + 2 * 3 - -10";
        assert_eq!(
            parse(lex(input).unwrap()),
            Ok(vec![Ast::binop(
                BinOp::sub(Loc(10, 11)),
                Ast::binop(
                    BinOp::add(Loc(2, 3)),
                    Ast::num(1, Loc(0, 1)),
                    Ast::binop(
                        BinOp::new(BinOpKind::Mult, Loc(6, 7)),
                        Ast::num(2, Loc(4, 5)),
                        Ast::num(3, Loc(8, 9)),
                        Loc(4, 9)
                    ),
                    Loc(0, 9),
                ),
                Ast::uniop(
                    UniOp::minus(Loc(12, 13)),
                    Ast::num(10, Loc(13, 15)),
                    Loc(12, 15)
                ),
                Loc(0, 15)
            )])
        )
    }

    #[test]
    fn lex_parse_lessthan() {
        let input = "1+2*3>=-10";
        assert_eq!(
            parse(lex(input).unwrap()),
            Ok(vec![Ast::binop(
                BinOp::greater_equal(Loc(5, 7)),
                Ast::binop(
                    BinOp::add(Loc(1, 2)),
                    Ast::num(1, Loc(0, 1)),
                    Ast::binop(
                        BinOp::mult(Loc(3, 4)),
                        Ast::num(2, Loc(2, 3)),
                        Ast::num(3, Loc(4, 5)),
                        Loc(2, 5),
                    ),
                    Loc(0, 5),
                ),
                Ast::uniop(
                    UniOp::minus(Loc(7, 8)),
                    Ast::num(10, Loc(8, 10)),
                    Loc(7, 10),
                ),
                Loc(0, 10),
            )])
        )
    }
}
