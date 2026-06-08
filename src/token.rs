// type Starship {
//   id: ID!
//   name: String!
//   length(unit: LengthUnit = METER): Float
// }

pub trait TokenChar {
    fn is_name(&self) -> bool;
    fn is_white(&self) -> bool;
}

impl TokenChar for char {
    fn is_name(&self) -> bool {
        *self == '_' || self.is_alphanumeric()
    }
    fn is_white(&self) -> bool {
        self.is_whitespace()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SYMBOL {
    Aruba,
    OpCurl,
    ClCurl,
    Colon,
    Exclam,
    Equals,
    Pipe,
    Comma,
    OpParen,
    ClParen,
    OpSquar,
    ClSquar,
    DQuote,
    TDQuote,
    Ampers,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Symbol(SYMBOL),
    White,
    Name,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Chunk {
    pub index: usize,
    pub len: usize,
    pub token: Token,
}

impl<'a> Chunk {
    pub fn new(index: usize, len: usize, token: Token) -> Chunk {
        Chunk { index, len, token }
    }
    pub fn is_symbol(&self, symbol: SYMBOL) -> bool {
        match &self.token {
            Token::Symbol(s) => *s == symbol,
            _ => false,
        }
    }
    pub fn extend(&self, other: &Chunk) -> Chunk {
        Chunk {
            index: self.index,
            len: other.index - self.index + other.len,
            token: self.token.clone(),
        }
    }
}

impl Chunk {
    fn new_symbol(symbol: SYMBOL, index: usize, len: usize) -> Chunk {
        Chunk::new(index, len, Token::Symbol(symbol))
    }
    fn new_white(index: usize, len: usize) -> Chunk {
        Chunk::new(index, len, Token::White)
    }
    fn new_name(index: usize, len: usize) -> Chunk {
        Chunk::new(index, len, Token::Name)
    }
    fn new_unknown(index: usize, len: usize) -> Chunk {
        Chunk::new(index, len, Token::Unknown)
    }
    pub fn apply<'a>(&self, source: &'a str) -> &'a str {
        &source[self.index..self.index + self.len]
    }
}

fn as_symbol(off: &str, index: usize) -> Option<Chunk> {
    // All recognized symbols are single-byte ASCII, so operate on the raw bytes
    // to safely handle leading multi-byte UTF-8 without panicking on `&off[0..1]`.
    let first = *off.as_bytes().first()?;
    let (symbol, len) = match first {
        b'@' => (SYMBOL::Aruba, 1),
        b'{' => (SYMBOL::OpCurl, 1),
        b'}' => (SYMBOL::ClCurl, 1),
        b':' => (SYMBOL::Colon, 1),
        b'!' => (SYMBOL::Exclam, 1),
        b'=' => (SYMBOL::Equals, 1),
        b'|' => (SYMBOL::Pipe, 1),
        b',' => (SYMBOL::Comma, 1),
        b'(' => (SYMBOL::OpParen, 1),
        b')' => (SYMBOL::ClParen, 1),
        b'[' => (SYMBOL::OpSquar, 1),
        b']' => (SYMBOL::ClSquar, 1),
        b'&' => (SYMBOL::Ampers, 1),
        b'"' => {
            if off.as_bytes().get(0..3) == Some(b"\"\"\"") {
                (SYMBOL::TDQuote, 3)
            } else {
                (SYMBOL::DQuote, 1)
            }
        }
        _ => return None,
    };
    Some(Chunk::new_symbol(symbol, index, len))
}

fn as_white(off: &str, index: usize) -> Option<Chunk> {
    // Sum UTF-8 byte lengths rather than char counts: `len` is later used as a
    // byte offset into `off`, so a multi-byte char in a comment would otherwise
    // misalign the slice.
    let mut len: usize = off
        .chars()
        .take_while(TokenChar::is_white)
        .map(char::len_utf8)
        .sum();
    let mut it = off[len..].chars();
    if it.next() == Some('#') {
        // the rest of the line is a comment, which we treat as whitespace
        let mut c_len: usize = 1; // for '#'
        for c in it {
            if c == '\n' {
                break;
            }
            c_len += c.len_utf8();
        }
        len += c_len;
        // tokenize whitespace after the comment
        if let Some(c) = as_white(&off[len..], index + len) {
            len += c.len;
        }
    }
    if len == 0 {
        None
    } else {
        Some(Chunk::new_white(index, len))
    }
}

fn as_name(off: &str, index: usize) -> Option<Chunk> {
    let len: usize = off
        .chars()
        .take_while(TokenChar::is_name)
        .map(char::len_utf8)
        .sum();
    if len == 0 {
        None
    } else {
        Some(Chunk::new_name(index, len))
    }
}

pub struct TokenIter<'a> {
    offset: &'a str,
    index: usize,
    peek: Option<Chunk>,
}

impl<'a> TokenIter<'a> {
    pub fn peek(&mut self) -> Option<&Chunk> {
        if self.peek.is_none() {
            self.peek = self.next();
        }
        self.peek.as_ref()
    }
    pub fn skip_white(&mut self) {
        loop {
            let do_break = {
                match self.peek() {
                    None => break,
                    Some(chunk) => chunk.token != Token::White,
                }
            };
            if do_break {
                break;
            }
            // the peek is whitespace.
            self.peek = None;
        }
    }
    pub fn consume(&mut self) -> Option<Chunk> {
        if self.peek.is_some() {
            self.peek.take()
        } else {
            self.next()
        }
    }
    pub fn peek_is_name<'b>(&mut self, source: &'b str, name: &str) -> bool {
        match self.peek() {
            None => false,
            Some(chunk) => {
                let text = chunk.apply(source);
                name == text
            }
        }
    }
    pub fn peek_is_symbol(&mut self, symbol: SYMBOL) -> bool {
        match self.peek() {
            None => false,
            Some(chunk) => chunk.is_symbol(symbol),
        }
    }
    pub fn peek_is_white_with_lf<'b>(&mut self, source: &'b str) -> bool {
        match self.peek() {
            None => false,
            Some(chunk) => {
                if chunk.token == Token::White {
                    let text = chunk.apply(source);
                    text.chars().any(|c| c == '\n')
                } else {
                    false
                }
            }
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Chunk;
    fn next(&mut self) -> Option<Chunk> {
        if self.offset.is_empty() {
            return None;
        }
        let chunk = if let Some(whit) = as_white(self.offset, self.index) {
            whit
        } else if let Some(symb) = as_symbol(self.offset, self.index) {
            symb
        } else if let Some(name) = as_name(self.offset, self.index) {
            name
        } else {
            // Unknown: advance by the full UTF-8 width of the leading char so we
            // never split a multi-byte sequence and panic on the next `&str` slice.
            let len = self.offset.chars().next().map(char::len_utf8).unwrap_or(1);
            Chunk::new_unknown(self.index, len)
        };
        self.offset = &self.offset[chunk.len..];
        self.index += chunk.len;
        Some(chunk)
    }
}

pub fn tokenize(source: &str) -> TokenIter {
    TokenIter {
        offset: source,
        index: 0,
        peek: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_simple() {
        let x: Vec<_> = tokenize("  abc(42)\n   ").collect();
        let c = vec![
            Chunk::new_white(0, 2),
            Chunk::new_name(2, 3),
            Chunk::new_symbol(SYMBOL::OpParen, 5, 1),
            Chunk::new_name(6, 2),
            Chunk::new_symbol(SYMBOL::ClParen, 8, 1),
            Chunk::new_white(9, 4),
        ];
        assert_eq!(x, c);
    }

    #[test]
    fn tokenize_struct() {
        let x: Vec<_> = tokenize(
            "\ntype Starship {\
             \n  id: ID!\
             \n  name: String!\
             \n  length(unit: LengthUnit = METER): Float\
             \n}\n",
        )
        .collect();
        assert_eq!(x.len(), 36);
    }
}
