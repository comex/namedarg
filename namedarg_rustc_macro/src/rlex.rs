// TODO shebang
use std::str;
#[allow(non_snake_case)]
fn Pattern_White_Space(c: u32) -> bool {
    match c {
        0x9 ... 0xd | 0x20 | 0x85 |
        0x200e | 0x200f | 0x2028 | 0x2029 => true,
        _ => false
    }
}

pub fn idx_before_ending_whitespace_char(s: &[u8]) -> Option<usize> {
    let len = s.len();
    if len == 0 { return None; }
    match s[len - 1] {
        0x9 ... 0xd | 0x20 => Some(len - 1),
        0x85 if s[..len].ends_with(b"\xc2") => Some(len - 2),
        0x8e | 0x8f | 0xa8 | 0xa9
            if s[..len - 1].ends_with(b"\xe2\x80")
            => Some(len - 3),
        _ => None,
    }
}


#[derive(Clone, Copy)]
struct Reader<'a> {
    data: &'a [u8],
    cur: u8,
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Reader {
            data: data,
            cur: if let Some(&c) = data.get(0) { c } else { b'\0' },
            pos: 1,
        }
    }
    #[inline]
    fn advance(&mut self) {
        if let Some(&c) = self.data.get(self.pos) {
            self.cur = c;
            self.pos += 1;
        } else {
            self.cur = 0;
        }
    }
    #[inline]
    fn next(&mut self) -> u8 {
        let ret = self.cur;
        self.advance();
        ret
    }
    fn advance_n(&mut self, n: usize) {
        for _ in 0..n { self.advance(); }
    }
    fn next_utf8(&mut self) -> u32 {
        let c = self.next();
        let num_bytes = (!c).leading_zeros();
        match num_bytes {
            2 | 3 | 4 => (),
            _ => return c as u32,
        }
        let mut chr = (c & (0x7f >> num_bytes)) as u32;
        for _ in 0..num_bytes {
            let c = self.cur;
            if c & 0xc0 != 0x80 { break; }
            self.advance();
            chr = (chr << 6) | (c & 0x3f) as u32;
        }
        chr
    }
    fn rewind(&mut self) {
        self.pos -= 1;
        assert!(self.pos >= 1);
        self.cur = self.data[self.pos - 1];
    }
    #[inline]
    fn at_eof(&self) -> bool {
        self.pos >= self.data.len()
    }
    fn pos_of_cur(&self) -> usize {
        self.pos - 1
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum DelimToken {
    Paren,
    Bracket,
    Brace,
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BinOp {
    And,
    Or,
    Star,
    Plus,
    Shl,
    Shr,
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Ident {
    pub name: Name,
}
#[derive(Copy, Clone)]
pub struct Keyword {
    pub ident: Ident,
}
impl Keyword {
    pub fn ident(self) -> Ident { self.ident }
    pub fn name(self) -> Name { self.ident.name }
}
macro_rules! define_idents {
    {$(($name:ident : $($str:tt)*)),*,} => {
        #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        pub enum Name {
            $($name),*,
            Other
        }
        #[allow(non_upper_case_globals)]
        pub mod keywords {
            use super::{Ident, Name, Keyword};
            $(
                pub const $name: Keyword = Keyword { ident: Ident { name: Name::$name } };
            )*
            pub const Other: Keyword = Keyword { ident: Ident { name: Name::Other } };
        }
        impl Name {
            pub fn as_str(&self) -> &'static str {
                match *self {
                    $(
                        Name::$name => unsafe {
                            str::from_utf8_unchecked($($str)*)
                        }
                    ),*,
                    Name::Other => unreachable!(),
                }
            }
        }
        impl Ident {
            pub fn from_bytes(bytes: &[u8]) -> Self {
                match bytes {
                    $($($str)* => Ident { name: Name::$name }),*,
                    _ => Ident { name: Name::Other },
                }
            }
        }
    }
}
define_idents! {
    (As: b"as"),
    (Box: b"box"),
    (Default: b"default"),
    (Extern: b"extern"),
    (Fn: b"fn"),
    (For: b"for"),
    (Impl: b"impl"),
    (Mut: b"mut"),
    (Ref: b"ref"),
    (SelfType: b"Self"),
    (SelfValue: b"self"),
    (Super: b"super"),
    (Trait: b"trait"),
    (Unsafe: b"unsafe"),
    (Where: b"where"),
}
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Token {
    OpenDelim(DelimToken),
    CloseDelim(DelimToken),
    Ident(Ident),
    Eof,
    DotDot,
    DotDotDot,
    Pound,
    Not,
    Lt,
    Gt,
    Comma,
    Colon,
    ModSep,
    Semi,
    Question,
    Dollar,
    Underscore,
    Eq,
    RArrow,
    Literal(()),
    Lifetime(()),
    BinOp(BinOp),
    #[allow(dead_code)]
    Dummy,
    White,
    Other,
}
pub mod token {
    pub use super::Token::*;
    pub use super::BinOp::*;
}

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    read: Reader<'a>,
    lineno: usize,
    line_start_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        let data = Lexer::skip_shebang(data);
        Lexer {
            read: Reader::new(data),
            lineno: 1,
            line_start_pos: 0,
        }
    }
    fn bump_lineno(&mut self) {
        self.lineno += 1;
        self.line_start_pos = self.pos();
    }
    fn scan_quoted_char(&mut self) -> bool {
        match self.read.cur {
            b'\\' => {
                self.read.advance();
                match self.read.next() {
                    b'\r' => { return false; },
                    b'\n' => { self.bump_lineno(); return false; },
                    b'x' => { self.read.advance_n(2); },
                    b'u' => {
                        if self.read.next() != b'{' { return true; }
                        while self.read.next() != b'}' &&
                              !self.read.at_eof() {}
                    },
                    _ => (),
                }
            },
            b'\n' => { self.bump_lineno(); self.read.advance(); },
            b'\x00' ... b'\x7f' => { self.read.advance(); },
            _ => { self.read.next_utf8(); },
        }
        true
    }
    fn skip_shebang(data: &[u8]) -> &[u8] {
        if data.len() > 2 &&
           data[0] == b'#' && data[1] == b'!' && data[2] != b'[' {
            if let Some(pos) = data.iter().position(|&c| c == b'\n') {
                return &data[pos+1..];
            }
        }
        data
    }
    fn scan_singlequote(&mut self) -> Token {
        while !self.scan_quoted_char() {
            if self.read.at_eof() { return Token::Eof; }
        }
        if self.read.cur == b'\'' {
            self.read.advance();
            Token::Literal(())
        } else {
            Token::Lifetime(())
        }
    }
    fn scan_doublequote(&mut self) -> Token {
        while self.read.cur != b'"' {
            if self.read.at_eof() { return Token::Eof; }
            self.scan_quoted_char();
        }
        self.read.advance();
        Token::Literal(())
    }
    fn scan_slashstar_comment(&mut self) {
        self.read.advance();
        let mut level = 0u32;
        loop {
            match self.read.next() {
                b'/' if self.read.cur == b'*' => {
                    self.read.advance();
                    level += 1;
                },
                b'*' if self.read.cur == b'/' => {
                    self.read.advance();
                    level -= 1;
                    if level == 0 { return; }
                },
                b'\0' if self.read.at_eof() => { return; },
                _ => (),
            }
        }
    }
    fn skip_to_nl(&mut self) {
        while self.read.next() != b'\n' && !self.read.at_eof() {}
        self.bump_lineno();
    }
    fn scan_ident(&mut self, start: usize) -> Token {
        loop {
            match self.read.cur {
                b'a' ... b'z' | b'A' ... b'Z' | b'_' | b'0' ... b'9' => self.read.advance(),

                b'\x00' ... b'\x7f' => { break },
                _ => {
                    let c = self.read.next_utf8();
                    if Pattern_White_Space(c) { break; }
                    // again, assume unknown unicode chars are XID_Continue
                },
            }
        }
        let end = self.read.pos_of_cur();
        if start + 1 == end && self.read.data[start] == b'_' {
            Token::Underscore
        } else {
            Token::Ident(Ident::from_bytes(&self.read.data[start..end]))
        }
    }
    fn next_unicode(&mut self) -> Token {
        self.read.rewind();
        let pos = self.read.pos_of_cur();
        let c = self.read.next_utf8();
        if Pattern_White_Space(c) {
            return Token::White;
        }
        // assume it's an ident
        return self.scan_ident(pos);
    }
    #[inline]
    pub fn next(&mut self) -> Token {
        //println!(">> cur={:?} pos={}, line={}", self.read.cur, self.pos(), self.line());
        match self.read.next() {
            b'/' => match self.read.cur {
                b'/' => { self.skip_to_nl(); Token::White },
                b'*' => { self.scan_slashstar_comment(); Token::White },
                _ => { Token::Other /* div */ },
            },
            b'\0' => Token::Eof,
            b'.' => {
                if self.read.cur != b'.' { return Token::Other; }
                self.read.advance();
                if self.read.cur != b'.' { return Token::DotDot; }
                self.read.advance();
                Token::DotDotDot
            },
            b'#' if self.read.pos == 1 => {
                self.skip_to_nl();
                Token::White
            },
            b'#' => Token::Pound,
            b'!' => Token::Not,
            b'<' if self.read.cur == b'<' => {
                self.read.advance();
                Token::BinOp(BinOp::Shl)
            },
            b'>' if self.read.cur == b'>' => {
                self.read.advance();
                Token::BinOp(BinOp::Shr)
            },
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b',' => Token::Comma,
            b':' => match self.read.cur {
                b':' => { self.read.advance(); Token::ModSep },
                _ => { Token::Colon },
            },
            b';' => Token::Semi,
            b'?' => Token::Question,
            b'$' => Token::Dollar,
            b'=' => Token::Eq,
            b'-' => match self.read.cur {
                b'>' => { self.read.advance(); Token::RArrow },
                _ => { Token::Other },
            },
            b'[' =>  Token::OpenDelim(DelimToken::Bracket),
            b']' => Token::CloseDelim(DelimToken::Bracket),
            b'(' =>  Token::OpenDelim(DelimToken::Paren),
            b')' => Token::CloseDelim(DelimToken::Paren),
            b'{' =>  Token::OpenDelim(DelimToken::Brace),
            b'}' => Token::CloseDelim(DelimToken::Brace),
            b'\'' => self.scan_singlequote(),
            b'"' => self.scan_doublequote(),
            b'|' => Token::BinOp(BinOp::Or),
            b'&' => Token::BinOp(BinOp::And),
            b'*' => Token::BinOp(BinOp::Star),
            b'+' => Token::BinOp(BinOp::Plus),
            b' ' | b'\t' | b'\r' => {
                loop {
                    match self.read.cur {
                        b' ' | b'\t' | b'\r' => self.read.advance(),
                        _ => break
                    }
                }
                Token::White
            },
            b'\n' => { self.bump_lineno(); Token::White },
            b'a' ... b'z' | b'A' ... b'Z' | b'_' => {
                let pos = self.read.pos_of_cur() - 1;
                self.scan_ident(pos)
            },
            b'0' ... b'9' => {
                loop {
                    match self.read.cur {
                        b'0' ... b'9' | b'a' ... b'z' | b'A' ... b'Z' =>
                            self.read.advance(),
                        _ => break
                    }
                }
                Token::Literal(())
            },
            b'\x00' ... b'\x7f' => Token::Other,
            _ => return self.next_unicode(),
        }
    }
    #[inline]
    pub fn pos(&self) -> usize {
        self.read.pos_of_cur()
    }
    #[inline]
    pub fn line(&self) -> usize {
        self.lineno
    }
    #[inline]
    pub fn col(&self) -> usize {
        1 + self.pos() - self.line_start_pos
    }
}
