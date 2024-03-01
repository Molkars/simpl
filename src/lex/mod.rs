use thiserror::Error;

#[derive(Clone, Copy)]
pub struct Span {
    pub index: usize,
    pub column: usize,
    pub line: usize,
    pub length: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Punct(Punct),
    Ident(String),
    Integer(u64),
    Decimal(f64),
    String(String),
    Group {
        inner: Vec<Token>,
        kind: GroupKind,
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Punct {
    ColonColon,
    Dot,
    Comma,
    EqualGt,
    Equal,
    At,
    Bang,
}

#[derive(Debug, Clone, Copy)]
pub enum GroupKind {
    Brace,
    Bracket,
    Paren,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("unterminated group: {kind:?}")]
    UnterminatedGroup {
        span: Span,
        kind: GroupKind,
    },
    #[error("unexpected group terminator: {kind:?}")]
    UnexpectedGroupTerminator {
        span: Span,
        kind: GroupKind,
    },
    #[error("unterminated string")]
    UnterminatedString {
        span: Span,
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, Error> {
    let mut tok = Tokenizer {
        source,
        index: 0,
        line: 1,
        column: 1
    };

    let mut out = Vec::new();

    while let Some(tok) = tokenize_one(&mut tok) {
        out.push(tok?);
    }

    Ok(out)
}

fn tokenize_one(tok: &mut Tokenizer) -> Option<Result<Token, Error>> {
    tok.take_while(|c| c.is_ascii_whitespace());

    let Some(c) = tok.peek() else {
        return None;
    };

    if c.is_ascii_alphabetic() || c == '_' {
        let span = tok.take_while(|c| c.is_alphabetic() || c == '_').expect("we already checked this is true");
        let content = tok.content(span);
        return Some(Ok(Token {
            span,
            kind: TokenKind::Ident(content.to_string()),
        }));
    }

    if c.is_ascii_digit() {
        let span = tok.take_while(|c| c.is_ascii_alphanumeric() || c == '_').expect("we already checked this is true");

        if tok.peek() == Some('.') && tok.peekn(1).is_some_and(|c| c.is_ascii_digit()) {
            tok.adv('.');
            let span2 = tok.take_while(|c| c.is_ascii_alphanumeric() || c == '_').expect("we already checked this is true");
            let span = Span {
                index: span.index,
                column: span.column,
                line: span.line,
                length: span2.index + span2.length - span.index,
            };

            let content = tok.content(span);
            let value: f64 = content.parse().expect("should not fail");
            return Some(Ok(Token {
                span,
                kind: TokenKind::Decimal(value),
            }))
        }

        let content = tok.content(span);
        let value: u64 = content.parse().expect("should not fail");
        return Some(Ok(Token {
            span,
            kind: TokenKind::Integer(value),
        }));
    }

    if c == '"' {
        let mut span = Span {
            index: tok.index,
            line: tok.line,
            column: tok.column,
            length: 0,
        };
        let mut content = String::new();

        tok.adv(c);
        loop {
            let Some(c) = tok.peek() else {
                span.length = tok.index - span.index;
                return Some(Err(Error::UnterminatedString {
                    span,
                }));
            };
            tok.adv(c);

            if c == '"' {
                break;
            }

            if c != '\\' {
                content.push(c);
                continue;
            }

            todo!("string escape sequences");
        }

        span.length = tok.index - span.index;
        return Some(Ok(Token {
            span,
            kind: TokenKind::String(content),
        }));
    }

    if matches!(c, '(' | '{' | '[') {
        let mut span = Span {
            index: tok.index,
            line: tok.line,
            column: tok.column,
            length: 0,
        };
        tok.adv(c);

        let (group_kind, terminator) = match c {
            '(' => (GroupKind::Paren, ')'),
            '{' => (GroupKind::Brace, '}'),
            '[' => (GroupKind::Bracket, ']'),
            _ => unreachable!(),
        };

        let mut inner = Vec::new();
        loop {
            tok.take_while(|c| c.is_ascii_whitespace());

            if !tok.peek().is_some_and(|c| c != terminator) {
                break;
            };

            let Some(token_result) = tokenize_one(tok) else {
                span.length = tok.index - span.index;
                return Some(Err(Error::UnterminatedGroup {
                    span,
                    kind: group_kind,
                }));
            };

            match token_result {
                Ok(token) => inner.push(token),
                Err(err) => return Some(Err(err)),
            };
        }

        if tok.peek() != Some(terminator) {
            span.length = tok.index - span.index;
            return Some(Err(Error::UnterminatedGroup {
                span,
                kind: group_kind,
            }));
        }

        tok.adv(terminator);
        span.length = tok.index - span.index;

        return Some(Ok(Token {
            span,
            kind: TokenKind::Group {
                inner,
                kind: group_kind,
            }
        }));
    }

    if matches!(c, ')' | '}' | ']') {
        let span = tok.adv(c);
        return Some(Err(Error::UnexpectedGroupTerminator {
            span,
            kind: match c {
                ')' => GroupKind::Paren,
                '}' => GroupKind::Brace,
                ']' => GroupKind::Bracket,
                _ => unreachable!(),
            },
        }));
    }

    if let Some(span) = tok.take_str("::") {
        return Some(Ok(token_punct(span, Punct::ColonColon)));
    }

    if let Some(span) = tok.take_str(".") {
        return Some(Ok(token_punct(span, Punct::Dot)));
    }

    if let Some(span) = tok.take_str(",") {
        return Some(Ok(token_punct(span, Punct::Comma)));
    }

    if let Some(span) = tok.take_str("=>") {
        return Some(Ok(token_punct(span, Punct::EqualGt)));
    }

    if let Some(span) = tok.take_str("=") {
        return Some(Ok(token_punct(span, Punct::Equal)));
    }

    if let Some(span) = tok.take_str("@") {
        return Some(Ok(token_punct(span, Punct::At)));
    }

    if let Some(span) = tok.take_str("!") {
        return Some(Ok(token_punct(span, Punct::Bang)));
    }

    None
}

#[inline(always)]
fn token_punct(span: Span, punct: Punct) -> Token {
    Token {
        span,
        kind: TokenKind::Punct(punct),
    }
}

struct Tokenizer<'a> {
    source: &'a str,
    index: usize,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    fn adv(&mut self, c: char) -> Span {
        let out = Span {
            index: self.index,
            column: self.column,
            line: self.line,
            length: 1
        };
        self.index += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        out
    }

    fn peek(&self) -> Option<char> {
        self.source[self.index..].chars().next()
    }

    fn peekn(&self, n: usize) -> Option<char> {
        self.source[self.index..].chars().skip(n).next()
    }

    fn take_while(&mut self, f: impl Fn(char) -> bool) -> Option<Span> {
        let index = self.index;
        let line = self.line;
        let column = self.column;
        while let Some(c) = self.peek() {
            if f(c) {
                self.adv(c);
            } else {
                break;
            }
        }
        (self.index != index)
            .then(|| Span { index, line, column, length: self.index - index })
    }

    fn content(&self, span: Span) -> &str {
        &self.source[span.index..span.index + span.length]
    }

    fn take_str(&mut self, s: &str) -> Option<Span> {
        if s.is_empty() || self.index + s.len() >= self.source.len() {
            return None;
        }

        let index = self.index;
        let line = self.line;
        let column = self.column;

        for (a, b) in self.source[self.index..].chars().zip(s.chars()) {
            if a != b {
                self.index = index;
                self.line = line;
                self.column = column;
                return None;
            }
            self.adv(a);
        }

        Some(Span {
            index,
            line,
            column,
            length: self.index - index,
        })
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(i={}:l={}:c={}:w={})", self.index, self.line, self.column, self.length)
    }
}
