use thiserror::{Error};

#[derive(Debug, Clone)]
pub struct Token {
    pub line: u64,
    pub column: u64,
    pub length: u64,
    pub index: u64,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Ident,
    Integer,

    ColonColon,
    Dot,


}

pub struct Tokens<'a> {
    inner: Vec<Token>,
    source: &'a str,
}

#[derive(Error, Debug)]
pub enum TokenizeError {
    #[error("unexpected character: {0:?}")]
    UnexpectedCharacter(char),
}

struct Tokenizer<'a> {
    source: &'a str,
    index: usize,
    line: u64,
    column: u64,
}

impl<'a> Tokenizer<'a> {
    fn peek(&self) -> Option<char> {
        self.source[self.index..].chars().next()
    }

    fn advance(&self) {
        if let Some(c) = self.peek() {
            self.index += c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
    }
}

pub fn lex(source: &str) -> Result<Tokens, TokenizeError> {
    let mut out = Tokens {
        inner: vec![],
        source,
    };

    let mut tok = Tokenizer {
        source,
        index: 0,
        line: 1,
        column: 1,
    };

    take_whitespace(&mut tok);
    while let Some(c) = tok.peek() {
        if c.is_ascii_alphabetic() {
            let token = take_while(&mut tok, |c| c.is_ascii_alphabetic() || c == '_')
                .expect("we checked that the current char is alphabetic");
            out.inner.push(token);
            continue;
        }

        if c.is_ascii_digit() {
            let token = take_while(&mut tok, |c| c.is_ascii_alphanumeric() || c == '_')
                .expect("we checked that the current char is numeric");
            out.inner.push(token);
            continue;
        }


    }

    Ok(out)
}

fn take_while(tok: &mut Tokenizer, f: impl Fn(char) -> bool) -> Option<Token> {
    let index = tok.index;
    let column = tok.column;
    let line = tok.line;

    while let Some(c) = tok.peek() {
        if f(c) {
            tok.advance();
        }
    }

    if tok.index == index {
        None
    } else {
        Some(Token {
            line,
            column,
            length: tok.column - column,
            index: u64::try_from(index).unwrap(),
        })
    }
}

fn take_whitespace(tok: &mut Tokenizer) {
    while let Some(c) = tok.peek() {
        if c.is_ascii_whitespace() {
            tok.advance();
        }
    }
}
