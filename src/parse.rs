use crate::ast::{Ast, AstKind, BinOp, UniOp, TYPES};
use crate::error::ParseError::*;
use crate::lex::{Token, TokenKind};
use std::iter::Peekable;

use anyhow::{anyhow, bail, Result};

macro_rules! expect_token {
    ($tokens: expr, $token: pat) => {{
        let tok = $tokens.next().ok_or(Eof)?;
        match tok.value.clone() {
            $token => (),
            _ => bail!(UnexpectedToken(tok.clone())),
        }
    }};
}
macro_rules! expect_semicolon {
    ($tokens: expr) => {{
        let tok = $tokens.next().ok_or(Eof)?;
        match tok.value.clone() {
            TokenKind::Semicolon => (),
            _ => bail!(NotSemicolon(tok.clone())),
        }
    }};
}
macro_rules! expect_paren_close {
    ($tokens: expr) => {{
        let tok = $tokens.next().ok_or(Eof)?;
        match tok.value {
            TokenKind::Rparen => (),
            _ => bail!(UnClosedOpenParen(tok)),
        }
    }};
}

macro_rules! parse_vectors {
    ($tokens: expr, $vars: expr, $end_token: pat, $sep: pat, $parser: expr) => {{
        let mut v = vec![];
        loop {
            let token = $tokens.peek().ok_or(Eof)?;
            match token.value {
                $end_token => break,
                $sep => {
                    $tokens.next();
                    continue;
                }
                _ => v.push($parser($tokens, $vars)?),
            }
        }
        $tokens.next();
        v
    }};
}
macro_rules! parse_or {
    ($tokens: expr, $vars: expr, $sep: pat, $parser: expr) => {{
        let tok = $tokens.peek().ok_or(Eof)?;
        match tok.value {
            $sep => None,
            _ => Some($parser($tokens, $vars)?),
        }
    }};
}

pub fn parse(input: &str) -> Result<Vec<Ast>> {
    use crate::lex::lex;
    let tokens = lex(input).map_err(|e| anyhow!(e))?;

    fn parse_body(tokens: Vec<Token>) -> Result<Vec<Ast>> {
        let mut tokens = tokens.into_iter().peekable();
        Ok(parse_program(&mut tokens)?)
    }

    Ok(parse_body(tokens).map_err(|e| anyhow!(e))?)
}

fn parse_program<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Vec<Ast>>
where
    Tokens: Iterator<Item = Token>,
{
    let mut funs = vec![];
    loop {
        if tokens.peek().is_none() {
            return Ok(funs);
        }
        funs.push(parse_fun(tokens)?);
    }
}

fn parse_fun<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;
    let tok = tokens.next().unwrap();
    if !matches!(tok.value.clone(), Ident(s) if s == "int".to_owned()) {
        bail!(UnexpectedToken(tok));
    }
    let (funname, mut loc) = match tokens.next().ok_or(Eof)?.value.clone() {
        Ident(name) => (name.to_owned(), tok.loc.clone()),
        _ => bail!(UnexpectedToken(tok)),
    };
    let mut vars = vec![];
    expect_token!(tokens, Lparen);
    let args = parse_vectors!(tokens, &mut vars, Rparen, Comma, parse_ident);
    expect_token!(tokens, BlockOpen);
    let mut stmts = vec![];
    loop {
        let tok = tokens.peek().ok_or(Eof)?;
        if let TokenKind::BlockClose = tok.value {
            break;
        }
        loc = loc.merge(&tok.loc);
        stmts.push(parse_stmt(tokens, &mut vars)?);
    }
    tokens.next();
    Ok(Ast::dec_fun(funname, args, stmts, loc))
}

fn parse_stmt<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<(String, usize)>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;
    match tokens.peek().ok_or(Eof)?.value.clone() {
        Return => {
            let tok = tokens.next().unwrap();
            let exp = parse_expr(tokens, vars)?;
            let loc = tok.loc.merge(&exp.loc);
            expect_semicolon!(tokens);
            Ok(Ast::make_return(exp, loc))
        }
        Ident(s) if s == "if".to_owned() => {
            let tok = tokens.next().unwrap();
            expect_token!(tokens, Lparen);
            let cond = parse_expr(tokens, vars)?;
            expect_paren_close!(tokens);
            let stmt = parse_stmt(tokens, vars)?;

            if tokens.peek().is_none() {
                let loc = tok.loc.merge(&stmt.loc);
                return Ok(Ast::make_if(cond, stmt, loc));
            }
            let tok = tokens.next().unwrap();
            if let Ident(s) = tok.value {
                if s == "else" {
                    let els = parse_stmt(tokens, vars)?;
                    let loc = tok.loc.merge(&els.loc);
                    return Ok(Ast::make_if_else(cond, stmt, els, loc));
                }
            }
            let loc = tok.loc.merge(&stmt.loc);
            Ok(Ast::make_if(cond, stmt, loc))
        }
        Ident(s) if s == "while".to_owned() => {
            let tok = tokens.next().unwrap();
            expect_token!(tokens, Lparen);
            let cond = parse_expr(tokens, vars)?;
            expect_paren_close!(tokens);
            let stmt = parse_stmt(tokens, vars)?;
            let loc = tok.loc.merge(&stmt.loc);
            Ok(Ast::make_while(cond, stmt, loc))
        }
        Ident(s) if s == "for".to_owned() => {
            let tok = tokens.next().unwrap();
            let mut loc = tok.loc;
            expect_token!(tokens, Lparen);
            let declare = parse_or!(tokens, vars, Semicolon, parse_expr);
            expect_semicolon!(tokens);
            let cond = parse_or!(tokens, vars, Semicolon, parse_expr);
            expect_semicolon!(tokens);
            let update = parse_or!(tokens, vars, Rparen, parse_expr);
            expect_paren_close!(tokens);
            let stmt = parse_stmt(tokens, vars)?;
            loc = loc.merge(&stmt.loc);
            Ok(Ast::make_for(declare, cond, update, stmt, loc))
        }
        Ident(s) if TYPES.binary_search(&(&s[..])).is_ok() => {
            let tok = tokens.next().unwrap();
            let loc = tok.loc;
            let id_tok = tokens.next().ok_or(anyhow!(Eof))?;
            let name = match id_tok.value {
                Ident(name) => Ok(name),
                _ => Err(UnexpectedToken(id_tok.clone())),
            }?;
            loc.merge(&id_tok.loc);
            let offset = vars
                .iter()
                .max_by_key(|(_, offset)| offset)
                .map_or(8, |(_, offset)| offset + 8);
            vars.push((name, offset));
            let var = Ast::variable(offset, loc.clone());

            let res = match tokens.peek().ok_or(Eof)?.value {
                Semicolon | Rparen | BlockClose | Comma => {
                    Ok(Ast::assign(var, Ast::num(0, loc.clone()), loc))
                }
                Equal => {
                    tokens.next();
                    let e = parse_expr(tokens, vars)?;
                    Ok(Ast::assign(var, e, loc))
                }
                _ => bail!(NotExpression(tokens.peek().unwrap().clone())),
            };
            expect_semicolon!(tokens);
            res
        }
        BlockOpen => {
            let tok = tokens.next().unwrap();
            let mut loc = tok.loc;
            let v = parse_vectors!(tokens, vars, BlockClose, Semicolon, parse_stmt);
            if v.len() != 0 {
                loc = loc.merge(&v[v.len() - 1].loc);
            }
            Ok(Ast::make_block(v, loc))
        }
        _ => {
            let exp = parse_expr(tokens, vars)?;
            expect_semicolon!(tokens);
            Ok(exp)
        }
    }
}

fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<(String, usize)>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    parse_assign(tokens, vars)
}

fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
    subexpr_parser: fn(&mut Peekable<Tokens>, &mut Vec<(String, usize)>) -> Result<Ast>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    let mut e = subexpr_parser(tokens, vars)?;
    loop {
        if let None = tokens.peek() {
            break;
        }
        let op = match op_parser(tokens) {
            Ok(op) => op,
            Err(_) => break,
        };
        let r = subexpr_parser(tokens, vars)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc)
    }
    Ok(e)
}

fn parse_assign<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    let equal_tok = tokens.peek().ok_or(Eof)?.clone();
    let loc = equal_tok.loc.clone();
    let equality = parse_equality(tokens, vars)?;
    let tok = tokens.peek().ok_or(Eof)?;
    use TokenKind::*;
    match tok.value {
        Semicolon | Rparen | BlockClose | Comma => Ok(Ast::stmt(equality, loc)),
        Equal => {
            if let AstKind::Variable(_) = equality.value {
                tokens.next();
                let rvalue = parse_assign(tokens, vars)?;
                let loc = equality.loc.merge(&rvalue.loc);
                Ok(Ast::assign(equality, rvalue, loc))
            } else {
                bail!(NotAddressExp(equal_tok.clone()))
            }
        }
        _ => bail!(NotExpression(tok.clone())),
    }
}

fn parse_equality<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr5_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            TokenKind::DoubleEqual => Ok(BinOp::equal(tok.loc.clone())),
            TokenKind::NotEqual => Ok(BinOp::not_equal(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_relational, parse_expr5_op)
}

fn parse_relational<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr4_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            TokenKind::Less => Ok(BinOp::less(tok.loc.clone())),
            TokenKind::LessEqual => Ok(BinOp::less_equal(tok.loc.clone())),
            TokenKind::Greater => Ok(BinOp::greater(tok.loc.clone())),
            TokenKind::GreaterEqual => Ok(BinOp::greater_equal(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr3, parse_expr4_op)
}

fn parse_expr3<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr3_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            TokenKind::Plus => Ok(BinOp::add(tok.loc.clone())),
            TokenKind::Minus => Ok(BinOp::sub(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr2, parse_expr3_op)
}

fn parse_expr2<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr2_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            TokenKind::Asterisk => Ok(BinOp::mult(tok.loc.clone())),
            TokenKind::Slash => Ok(BinOp::div(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr1, parse_expr2_op)
}

fn parse_expr1<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;
    match tokens.peek().map(|tok| tok.value.clone()) {
        Some(Plus) | Some(Minus) | Some(Asterisk) | Some(Ampersand) => {
            let (op, e) = match tokens.next() {
                Some(Token { value: Plus, loc }) => (UniOp::plus(loc), parse_atom(tokens, vars)?),
                Some(Token { value: Minus, loc }) => (UniOp::minus(loc), parse_atom(tokens, vars)?),
                Some(Token {
                    value: Asterisk,
                    loc,
                }) => (UniOp::dereference(loc), parse_expr1(tokens, vars)?),
                Some(Token {
                    value: Ampersand,
                    loc,
                }) => (UniOp::reference(loc), parse_expr1(tokens, vars)?),
                _ => unreachable!(),
            };
            let loc = e.loc.merge(&op.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_atom(tokens, vars),
    }
}

fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<(String, usize)>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;
    let tok = tokens.next().ok_or(Eof)?;
    match tok.value {
        Num(n) => Ok(Ast::num(n, tok.loc)),
        Ident(s) => {
            if matches!(tokens.peek(), Some(tok) if tok.value == Lparen) {
                let mut loc = tok.loc.clone();
                tokens.next();
                let args = parse_vectors!(tokens, vars, Rparen, Comma, parse_expr);
                if args.len() != 0 {
                    loc = loc.merge(&args[args.len() - 1].loc);
                }
                Ok(Ast::make_function(s, args, loc))
            } else {
                let max = vars.iter().find(|&(name, _)| s == *name);
                let t = Token::new(Ident(s), tok.loc.clone());
                match max {
                    Some(_) => Ok(Ast::variable(max.unwrap().1, tok.loc)),
                    _ => bail!(NotDefinedExp(t)),
                }
            }
        }
        Lparen => {
            let e = parse_expr(tokens, vars)?;
            expect_paren_close!(tokens);
            Ok(e)
        }
        _ => bail!(NotExpression(tok)),
    }
}

fn parse_ident<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<(String, usize)>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    use TokenKind::*;
    if matches!(tokens.peek().ok_or(Eof)?.value.clone(),
        Ident(s) if TYPES.binary_search(&(&s[..])).is_ok())
    {
        let tok = tokens.next().unwrap();
        let loc = tok.loc;
        let id_tok = tokens.next().ok_or(Eof)?;
        let name = match id_tok.value {
            Ident(name) => Ok(name),
            _ => Err(UnexpectedToken(id_tok.clone())),
        }?;
        loc.merge(&id_tok.loc);
        let offset = vars
            .iter()
            .max_by_key(|(_, offset)| offset)
            .map_or(8, |(_, offset)| offset + 8);
        vars.push((name, offset));
        Ok(Ast::variable(offset, loc.clone()))
    } else {
        bail!(UnexpectedToken(tokens.peek().unwrap().clone()))
    }
}
