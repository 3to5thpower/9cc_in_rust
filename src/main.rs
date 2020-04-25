use std::env;

fn main() -> Result<(), String> {
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

    if let TokenKind::Op(o) = &tokens[0] {
        return Err(format!("invalid operator {:?}", o));
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    if let TokenKind::Num(n) = &tokens[0] {
        println!("  mov rax, {}", n);
    }
    let mut itr = tokens[1..].iter();
    loop {
        use Operater::*;
        use TokenKind::*;
        match itr.next() {
            None => break,
            Some(&Op(Plus)) => {
                let n = match itr.next() {
                    Some(&Num(n)) => n,
                    Some(op) => return Err(format!("invalid operator {:?}", *op)),
                    None => return Err(format!("no integer to add")),
                };
                println!("  add rax, {}", n);
            }
            Some(&Op(Minus)) => {
                let n = match itr.next() {
                    Some(&Num(n)) => n,
                    Some(op) => return Err(format!("invalid operator {:?}", *op)),
                    None => return Err(format!("no integer to subtract")),
                };
                println!("  sub rax, {}", n);
            }
            Some(&Num(n)) => {
                return Err(format!("invalid integer {}", n));
            }
        }
    }
    println!("  ret");
    Ok(())
}

#[derive(Debug)]
enum TokenKind {
    Num(i32),
    Op(Operater),
}

#[derive(Debug)]
enum Operater {
    Plus,
    Minus,
}

#[derive(Debug)]
enum LexErrorKind {
    InvalidChar(char),
    _Eof,
}

fn lex(input: &str) -> Result<Vec<TokenKind>, LexErrorKind> {
    let mut res = vec![];

    //入力
    let input = input.as_bytes();
    let mut pos = 0;

    while pos < input.len() {
        match input[pos] {
            b'+' => {
                res.push(TokenKind::Op(Operater::Plus));
                pos += 1;
            }
            b'-' => {
                res.push(TokenKind::Op(Operater::Minus));
                pos += 1;
            }
            b'0'..=b'9' => {
                let mut end = pos;
                while end < input.len() && b'0' <= input[end] && input[end] <= b'9' {
                    end += 1;
                }
                let num: i32 = std::str::from_utf8(&input[pos..end])
                    .unwrap()
                    .parse()
                    .unwrap();
                res.push(TokenKind::Num(num));
                pos = end;
            }
            b' ' | b'\n' | b'\t' => {
                pos += 1;
            }
            b => return Err(LexErrorKind::InvalidChar(b as char)),
        }
    }
    Ok(res)
}
