use std::str;
fn Pattern_White_Space(c: u32) -> bool {
    match c {
        0x9 ... 0xd | 0x20 | 0x85 |
        0x200e | 0x200f | 0x2028 | 0x2029 => true,
        _ => false
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
        let mut num_bytes = (!c).leading_zeros();
        match num_bytes {
            2 | 3 | 4 => (),
            _ => return c as u32,
        }
        let mut chr = (c & (0x7f >> num_bytes)) as u32;
        for i in 0..num_bytes {
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

pub enum DelimToken {
    Paren,
    Bracket,
    Brace,
}
pub enum BinOp {
    And,
    Or,
    Star,
    Plus,
    Shr,
}
#[derive(Copy, Clone)]
pub struct Ident {
    pub name: Name,
}
#[derive(Copy, Clone)]
pub struct Keyword {
    pub ident: Ident,
}
impl Keyword {
    fn ident(self) -> Ident { self.ident }
    fn name(self) -> Name { self.ident.name }
}
macro_rules! define_idents {
    {$(($name:ident : $($str:tt)*)),*,} => {
        #[derive(Copy, Clone)]
        pub enum Name {
            $($name),*,
            Other
        }
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
                    $($($str)* => keywords::$name),*,
                    _ => keywords::Other,
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
    Dummy,
    Other,
}
pub mod token {
    pub use Token::*;
    pub use BinOp::*;
}

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    read: Reader<'a>,
    lineno: usize,
    line_start_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
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
    fn scan_quoted_char(&mut self) {
        match self.read.cur {
            b'\\' => loop {
                match self.read.next() {
                    b'\r' => { continue; },
                    b'\n' => { self.bump_lineno(); continue; },
                    b'x' => { self.read.advance_n(2); },
                    b'u' => {
                        if self.read.next() != b'{' { return; }
                        while self.read.next() != b'}' {}
                    },
                    _ => (),
                }
                break;
            },
            b'\x00' ... b'\x7f' => { self.read.advance() },
            _ => { self.read.next_utf8(); },
        }
    }
    fn scan_singlequote(&mut self) -> Token {
        self.scan_quoted_char();
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
        Token::Literal(())
    }
    fn scan_slashstar_comment(&mut self) {
        panic!()
    }
    fn skip_to_nl(&mut self) {
        while self.read.next() != b'\n' && !self.read.at_eof() {}
        self.bump_lineno();
    }
    fn scan_ident(&mut self) -> Token {
        let start = self.read.pos_of_cur();
        loop {
            match self.read.cur {
                b'a' ... b'z' | b'A' ... b'Z' | b'_' | b'0' ... b'9' => self.read.advance(),

                b'\x00' ... b'\x7f' => { break },
                b'\x80' ... b'\xff' => {
                    let c = self.read.next_utf8();
                    if Pattern_White_Space(c) { break; }
                    // again, assume unknown unicode chars are XID_Continue
                },
            }
        }
        let end = self.read.pos_of_cur();
        Token::Ident(Ident::from_bytes(&self.read.data[start..end]))
    }
    fn next_unicode(&mut self) -> Option<Token> {
        self.read.rewind();
        let c = self.read.next_utf8();
        if Pattern_White_Space(c) {
            if c == (b'\n' as u32) { self.bump_lineno(); }
            return None;
        }
        // assume it's an ident
        return Some(self.scan_ident());
    }
    pub fn next(&mut self) -> Token {
        loop {
            let r: Token = match self.read.next() {
                b'/' => match self.read.cur {
                    b'/' => { self.skip_to_nl(); continue },
                    b'*' => { self.scan_slashstar_comment(); continue },
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
                    continue
                },
                b'#' => Token::Pound,
                b'!' => Token::Not,
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
                b'_' => Token::Underscore,
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
                b' ' | b'\t' | b'\r' => continue,
                b'\n' => { self.bump_lineno(); continue },
                b'a' ... b'z' | b'A' ... b'Z' | b'_' => self.scan_ident(),
                b'\x00' ... b'\x7f' => Token::Other,
                _ => {
                    if let Some(tok) = self.next_unicode() { tok } else { continue }
                },
            };
            return r;
        }
    }
    pub fn pos(&self) -> usize {
        self.read.pos_of_cur()
    }
    pub fn line(&self) -> usize {
        self.lineno
    }
    pub fn col(&self) -> usize {
        1 + self.pos() - self.line_start_pos
    }
    pub fn set_pos(&mut self, pos: usize) {
        self.lineno = 1000000;
        self.read.pos = pos;
        self.read.advance();
    }
}
