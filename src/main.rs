use std::env;
use std::iter::Peekable;

fn main() -> Result<(), String> {
    /*
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(format!("引数の個数が正しくありません"));
    }
      let tokens = match lex(&args[1]) {
          Ok(v) => v,
          Err(e) => {
              return Err(format!("invalid char {:?}", e));
          }
      };

      if let TokenKind::Op(o) = &tokens[0].value {
          return Err(format!("invalid operator {:?}", o));
      }

      println!(".intel_syntax noprefix");
      println!(".global main");
      println!("main:");

      if let TokenKind::Num(n) = &tokens[0].value {
          println!("  mov rax, {}", n);
      }
      let mut itr = tokens[1..].iter();
    println!("  ret");
    */
    use std::io::{stdin, BufRead, BufReader};
    let stdin = stdin();
    let stdin = stdin.lock();
    let stdin = BufReader::new(stdin);
    let mut lines = stdin.lines();

    fn prompt(s: &str) -> std::io::Result<()> {
        use std::io::{stdout, Write};
        let stdout = stdout();
        let mut stdout = stdout.lock();
        stdout.write(s.as_bytes())?;
        stdout.flush()
    }

    loop {
        prompt("> ").unwrap();
        if let Some(Ok(line)) = lines.next() {
            let ast = match line.parse::<Ast>() {
                Ok(ast) => ast,
                Err(e) => unimplemented!(),
            };
            println!("{:?}", ast);
        } else {
            break;
        }
    }
    Ok(())
}

fn error_at(input: &str, etoken: LexError, s: &str) {
    let len = etoken.loc.len();
    println!("{}", &input);
    for _ in 0..len {
        print!("{}", " ");
        print!("^ ");
        println!("{}", s);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Error {
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

impl std::str::FromStr for Ast {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = lex(s)?;
        let ast = parse(tokens)?;
        Ok(ast)
    }
}

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
struct Annot<T> {
    value: T,
    loc: Loc,
}
impl<T> Annot<T> {
    fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TokenKind {
    Num(u64),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Rparen,
    Lparen,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LexErrorKind {
    InvalidChar(char),
    Eof,
}

type Token = Annot<TokenKind>;
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
}

type LexError = Annot<LexErrorKind>;
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

    //入力
    let input = input.as_bytes();
    let mut pos = 0;

    while pos < input.len() {
        match input[pos] {
            b'+' => {
                res.push(Token::plus(Loc(pos, pos + 1)));
                pos += 1;
            }
            b'-' => {
                res.push(Token::minus(Loc(pos, pos + 1)));
                pos += 1;
            }
            b'*' => {
                res.push(Token::asterisk(Loc(pos, pos + 1)));
                pos += 1;
            }
            b'/' => {
                res.push(Token::slash(Loc(pos, pos + 1)));
                pos += 1;
            }
            b'(' => {
                res.push(Token::lparen(Loc(pos, pos + 1)));
                pos += 1;
            }
            b')' => {
                res.push(Token::rparen(Loc(pos, pos + 1)));
                pos += 1;
            }
            b'0'..=b'9' => {
                let mut end = pos;
                while end < input.len() && b'0' <= input[end] && input[end] <= b'9' {
                    end += 1;
                }
                let num: u64 = std::str::from_utf8(&input[pos..end])
                    .unwrap()
                    .parse()
                    .unwrap();
                res.push(Token::number(num, Loc(pos, end)));
                pos = end;
            }
            b' ' | b'\n' | b'\t' => {
                pos += 1;
            }
            b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
        }
    }
    Ok(res)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AstKind {
    Num(u64),
    UniOp { op: UniOp, e: Box<Ast> },
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
}
type Ast = Annot<AstKind>;
impl Ast {
    fn num(n: u64, loc: Loc) -> Self {
        Self::new(AstKind::Num(n), loc)
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UniOpKind {
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
enum BinOpKind {
    Add,
    Sub,
    Mult,
    Div,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ParseError {
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    RebundantExpression(Token),
    UnClosedOpenParen(Token),
    Eof,
}

fn parse(tokens: Vec<Token>) -> Result<Ast, ParseError> {
    // peekはイテレータが現在指している値をただ返す
    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_expr(&mut tokens)?;
    match tokens.next() {
        Some(tok) => Err(ParseError::RebundantExpression(tok)),
        None => Ok(ret),
    }
}

fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast, ParseError>
where
    Tokens: Iterator<Item = Token>,
{
    parse_expr3(tokens)
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
            TokenKind::Num(n) => Ok(Ast::new(AstKind::Num(n), tok.loc)),
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

#[cfg(test)]
mod test {
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
            Ok(Ast::binop(
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
            ))
        );
    }

    #[test]
    fn lex_parse() {
        let input = "1 + 2 * 3 - -10";
        assert_eq!(
            parse(lex(input).unwrap()),
            Ok(Ast::binop(
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
            ))
        )
    }
}
