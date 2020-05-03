#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);
impl Loc {
    pub fn merge(&self, other: &Loc) -> Self {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
    pub fn len(&self) -> usize {
        self.1 - self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub value: T,
    pub loc: Loc,
}
impl<T> Annot<T> {
    pub fn new(value: T, loc: Loc) -> Self {
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
    Return,
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
    fn make_return(loc: Loc) -> Self {
        Self::new(TokenKind::Return, loc)
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

pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
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
    if s[..s.len()] == *"return" {
        return Ok((Token::make_return(Loc(start, end)), end));
    }
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
