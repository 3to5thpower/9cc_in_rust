use crate::ast::{Ast, AstKind, BinOp, Types, UniOp, UniOpKind};
use crate::error::ParseError::*;
use crate::lex::{Token, TokenKind::*};
use anyhow::{anyhow, bail, Result};
use std::iter::Peekable;

type VarInfo = (String, usize, Types);

macro_rules! expect_token {
    ($tokens: expr, $token: pat, $err: ident) => {{
        let tok = $tokens.next().ok_or(Eof)?;
        if !matches!(tok.value.clone(), $token) {
            bail!($err(tok.clone()))
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

    Ok(parse_body(tokens)?)
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
    let loc = tokens.peek().ok_or(Eof)?.loc.clone();
    let _ty = parse_type(tokens)?;
    let (funname, mut loc) = match tokens.next().ok_or(Eof)?.value.clone() {
        Ident(name) => (name, loc),
        _ => bail!(UnexpectedToken(tokens.peek().unwrap().clone())),
    };
    let mut vars = vec![];
    expect_token!(tokens, Lparen, UnexpectedToken);
    let args = parse_vectors!(tokens, &mut vars, Rparen, Comma, parse_ident);
    expect_token!(tokens, BlockOpen, UnexpectedToken);
    let mut stmts = vec![];
    loop {
        let tok = tokens.peek().ok_or(Eof)?;
        if matches!(tok.value, BlockClose) {
            tokens.next();
            break;
        }
        loc = loc.merge(&tok.loc);
        stmts.push(parse_stmt(tokens, &mut vars)?);
    }
    Ok(Ast::dec_fun(funname, args, stmts, loc))
}

fn parse_stmt<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().ok_or(Eof)?.value.clone() {
        Return => {
            let mut loc = tokens.next().unwrap().loc;
            let exp = parse_expr(tokens, vars)?;
            loc = loc.merge(&exp.loc);
            expect_token!(tokens, Semicolon, NotSemicolon);
            Ok(Ast::make_return(exp, loc))
        }
        Ident(s) if s == "if" => {
            let tok = tokens.next().unwrap();
            expect_token!(tokens, Lparen, UnexpectedToken);
            let cond = parse_expr(tokens, vars)?;
            expect_token!(tokens, Rparen, UnClosedOpenParen);
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
        Ident(s) if s == "while" => {
            let tok = tokens.next().unwrap();
            expect_token!(tokens, Lparen, UnexpectedToken);
            let cond = parse_expr(tokens, vars)?;
            expect_token!(tokens, Rparen, UnClosedOpenParen);
            let stmt = parse_stmt(tokens, vars)?;
            let loc = tok.loc.merge(&stmt.loc);
            Ok(Ast::make_while(cond, stmt, loc))
        }
        Ident(s) if s == "for" => {
            let tok = tokens.next().unwrap();
            let mut loc = tok.loc;
            expect_token!(tokens, Lparen, UnexpectedToken);
            let declare = parse_or!(tokens, vars, Semicolon, parse_expr);
            expect_token!(tokens, Semicolon, NotSemicolon);
            let cond = parse_or!(tokens, vars, Semicolon, parse_expr);
            expect_token!(tokens, Semicolon, NotSemicolon);
            let update = parse_or!(tokens, vars, Rparen, parse_expr);
            expect_token!(tokens, Rparen, UnClosedOpenParen);
            let stmt = parse_stmt(tokens, vars)?;
            loc = loc.merge(&stmt.loc);
            Ok(Ast::make_for(declare, cond, update, stmt, loc))
        }
        Ident(s) if s == "int" => {
            let loc = tokens.peek().unwrap().loc.clone();
            let var = parse_ident(tokens, vars)?;
            let tok = tokens.peek().ok_or(Eof)?.value.clone();
            if matches!(tok, Semicolon | Rparen | BlockClose | Comma) {
                tokens.next();
                return Ok(Ast::assign(var, Ast::num(0, loc.clone()), loc));
            } else if !matches!(tok, Equal) {
                bail!(NotExpression(tokens.peek().unwrap().clone()))
            }
            tokens.next();
            let e = parse_expr(tokens, vars)?;
            expect_token!(tokens, Semicolon, NotSemicolon);
            Ok(Ast::assign(var, e, loc))
        }
        BlockOpen => {
            let tok = tokens.next().unwrap();
            let mut vars = vars.clone();
            let mut loc = tok.loc;
            let v = parse_vectors!(tokens, &mut vars, BlockClose, Semicolon, parse_stmt);
            if v.len() != 0 {
                loc = loc.merge(&v[v.len() - 1].loc);
            }
            Ok(Ast::make_block(v, loc))
        }
        _ => {
            let exp = parse_expr(tokens, vars)?;
            expect_token!(tokens, Semicolon, NotSemicolon);
            Ok(exp)
        }
    }
}

fn parse_expr<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    parse_assign(tokens, vars)
}

fn parse_left_binop<Tokens>(
    tokens: &mut Peekable<Tokens>,
    vars: &mut Vec<VarInfo>,
    subexpr_parser: fn(&mut Peekable<Tokens>, &mut Vec<VarInfo>) -> Result<Ast>,
    op_parser: fn(&mut Peekable<Tokens>) -> Result<BinOp>,
) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    let mut e = subexpr_parser(tokens, vars)?;
    loop {
        let op = match op_parser(tokens) {
            Ok(op) => op,
            Err(_) => return Ok(e),
        };
        let r = subexpr_parser(tokens, vars)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc)
    }
}

fn parse_assign<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    let equal_tok = tokens.peek().ok_or(Eof)?.clone();
    let loc = equal_tok.loc.clone();
    let equality = parse_equality(tokens, vars)?;
    let tok = tokens.peek().ok_or(Eof)?;
    if matches!(tok.value, Semicolon | Rparen | BlockClose | Comma) {
        return Ok(Ast::stmt(equality, loc));
    } else if tok.value == Equal {
        match equality.value {
            AstKind::Variable(_, _)
            | AstKind::UniOp {
                op:
                    UniOp {
                        value: UniOpKind::Dereference,
                        ..
                    },
                ..
            } => {
                tokens.next();
                let rvalue = parse_assign(tokens, vars)?;
                let loc = equality.loc.merge(&rvalue.loc);
                return Ok(Ast::assign(equality, rvalue, loc));
            }
            _ => bail!(NotAddressExp(equal_tok.clone())),
        }
    }
    Err(anyhow!(NotExpression(tok.clone())))
}

fn parse_equality<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr5_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            DoubleEqual => Ok(BinOp::equal(tok.loc.clone())),
            NotEqual => Ok(BinOp::not_equal(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_relational, parse_expr5_op)
}

fn parse_relational<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr4_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            Less => Ok(BinOp::less(tok.loc.clone())),
            LessEqual => Ok(BinOp::less_equal(tok.loc.clone())),
            Greater => Ok(BinOp::greater(tok.loc.clone())),
            GreaterEqual => Ok(BinOp::greater_equal(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr3, parse_expr4_op)
}

fn parse_expr3<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr3_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            Plus => Ok(BinOp::add(tok.loc.clone())),
            Minus => Ok(BinOp::sub(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr2, parse_expr3_op)
}

fn parse_expr2<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    fn parse_expr2_op<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<BinOp>
    where
        Tokens: Iterator<Item = Token>,
    {
        let op = tokens.peek().ok_or(Eof).and_then(|tok| match tok.value {
            Asterisk => Ok(BinOp::mult(tok.loc.clone())),
            Slash => Ok(BinOp::div(tok.loc.clone())),
            _ => Err(NotOperator(tok.clone())),
        })?;
        tokens.next();
        Ok(op)
    }
    parse_left_binop(tokens, vars, parse_expr1, parse_expr2_op)
}

fn parse_expr1<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    match tokens.peek().map(|tok| tok.value.clone()) {
        Some(Plus) | Some(Minus) | Some(Asterisk) | Some(Ampersand) => {
            let tok = tokens.next().unwrap().clone();
            let (op, e) = match tok.value {
                Plus => (UniOp::plus(tok.loc), parse_atom(tokens, vars)?),
                Minus => (UniOp::minus(tok.loc), parse_atom(tokens, vars)?),
                Asterisk => (UniOp::dereference(tok.loc), parse_expr1(tokens, vars)?),
                Ampersand => (UniOp::reference(tok.loc), parse_expr1(tokens, vars)?),
                _ => unreachable!(),
            };
            let loc = e.loc.merge(&op.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_atom(tokens, vars),
    }
}

fn parse_atom<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    let tok = tokens.next().ok_or(Eof)?.clone();
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
                let var = vars.iter().find(|&(name, _, _)| s == *name);
                let t = Token::new(Ident(s), tok.loc.clone());
                match var {
                    Some(_) => Ok(Ast::variable(
                        var.unwrap().1.clone(),
                        var.unwrap().2.clone(),
                        tok.loc,
                    )),
                    _ => bail!(NotDefinedExp(t)),
                }
            }
        }
        Lparen => {
            let e = parse_expr(tokens, vars)?;
            expect_token!(tokens, Rparen, UnClosedOpenParen);
            Ok(e)
        }
        _ => bail!(NotExpression(tok.clone())),
    }
}

fn parse_type<Tokens>(tokens: &mut Peekable<Tokens>) -> Result<Types>
where
    Tokens: Iterator<Item = Token>,
{
    let typename = match tokens.peek().ok_or(Eof)?.value.clone() {
        Ident(s) => s,
        _ => bail!(UnexpectedToken(tokens.peek().unwrap().clone())),
    };
    let mut res = match typename.as_str() {
        "int" => Types::Int,
        _ => bail!(NotType(tokens.peek().unwrap().clone())),
    };
    tokens.next();
    while tokens.peek().ok_or(Eof)?.value == Asterisk {
        tokens.next();
        res = Types::Ptr(Box::new(res));
    }
    Ok(res)
}

fn parse_ident<Tokens>(tokens: &mut Peekable<Tokens>, vars: &mut Vec<VarInfo>) -> Result<Ast>
where
    Tokens: Iterator<Item = Token>,
{
    let loc = tokens.peek().ok_or(Eof)?.loc.clone();
    let ty = parse_type(tokens)?;
    let id_tok = tokens.next().ok_or(Eof)?;
    let size = match ty {
        Types::Int => 8,
        Types::Ptr(_) => 8,
    };
    let name = match id_tok.value {
        Ident(name) => Ok(name),
        _ => Err(UnexpectedToken(id_tok.clone())),
    }?;
    loc.merge(&id_tok.loc);
    let offset = vars
        .iter()
        .max_by_key(|(_, offset, _)| offset)
        .map_or(size, |(_, offset, _)| offset + size);
    vars.push((name, offset, ty.clone()));
    Ok(Ast::variable(offset, ty, loc.clone()))
}
