fn Pattern_White_Space(c: char) -> bool {
    match c {
        '\x09' ... '\x0d' | ' ' | '\u{85}' |
        '\u{200e}' | '\u{200f}' | '\u{2028}' | '\u{2029}' => true,
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
            cur: self.data.get(0).unwrap_or(0),
            pos: 1,
        }
    }
    #[inline]
    fn advance(&mut self) {
        if let Some(c) = self.data.get(self.pos) {
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
        // in case of invalid chars
        if num_bytes > 4 { num_bytes = 0; }
        self.advance_n(num_bytes);
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
    Or,
}
pub struct Ident {
    name: Name,
}
macro_rules! define_idents {
    {$(($name:ident : $($str:tt)*)),*,} => {
        pub enum Name {
            $($name),*,
            Other
        }
        pub mod keywords {
            $(
                pub const $name: Ident = Ident { name: Name::$name };
            )*
            pub const Other: Ident = Ident { name: Name::Other };
        }
        impl Ident {
            pub fn from_bytes(bytes: &[u8]) -> Self {
                match bytes {
                    $($($str)* => Some(keywords::$name)),*,
                    _ => keywords::Other,
                }
            }
            pub fn as_str(&self) -> Option<&str> {
                match bytes {
                    $(keywords::$name => Some($($str)*)),*,
                    keywords::Other => None,
                }
            }
        }
    }
}
define_idents! {
    (As: b"as"),
    (Box: b"box"),
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
    Literal(()),
    Lifetime(()),
    BinOp(BinOp),
    Dummy,
    Other,
}
pub mod token {
    pub use Token::*;
}

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    read: Reader<'a>,
    lineno: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Lexer {
            read: Reader::new(data),
            lineno: 1,
        }
    }
    fn bump_lineno(&mut self) {
        self.lineno += 1;
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
            Literal(())
        } else {
            Lifetime(())
        }
    }
    fn scan_doublequote(&mut self) -> Token {
        while self.read.cur != b'"' {
            if self.read.at_eof() { return Token::Eof; }
            self.read_quoted_char();
        }
        Literal(())
    }
    fn scan_slashstar_comment(&mut self) {
        // XXX
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
            if c == b'\n' { self.bump_lineno(); }
            return None;
        }
        // assume it's an ident
        return self.read_ident();
    }
    pub fn next(&mut self) -> Token {
        loop {
            return match self.read.next() {
                b'/' => match self.read.cur {
                    b'/' => { self.skip_to_nl(); continue },
                    b'*' => { self.read_slashstar_comment(); continue },
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
                b'#' if self.read.pos == 1 => self.skip_to_nl(),
                b'#' => Token::Pound,
                b'!' => Token::Not,
                b'<' => Token::Lt,
                b'>' => Token::Gt,
                b',' => Token::Comma,
                b':' => match self.read.cur {
                    ':' => { self.read.advance(); Token::ModSep },
                    _ => { Token::Colon },
                },
                b';' => Token::Semi,
                b'?' => Token::Question,
                b'$' => Token::Dollar,
                b'_' => Token::Underscore,
                b'[' =>  Token::OpenDelim(DelimToken::Bracket),
                b']' => Token::CloseDelim(DelimToken::Bracket),
                b'(' =>  Token::OpenDelim(DelimToken::Paren),
                b')' => Token::CloseDelim(DelimToken::Paren),
                b'{' =>  Token::OpenDelim(DelimToken::Brace),
                b'}' => Token::CloseDelim(DelimToken::Brace),
                b'\'' => self.read_singlequote(),
                b'"' => self.read_doublequote(),
                b'|' => Token::BinOp(BinOp::Or),
                b' ' | b'\t' | b'\r' => continue,
                b'\n' => { self.bump_lineno(); continue },
                b'a' ... b'z' | b'A' ... b'Z' | b'_' => self.read_ident(),
                b'\x00' ... b'\x7f' => Token::Other,
                _ => {
                    if let Some(tok) = self.next_unicode(c) { tok } else { continue }
                },
            };
        }

    }
    pub fn pos(&self) -> usize {
        self.read.pos_of_cur()
    }
    pub fn lineno(&self) -> usize {
        self.lineno
    }
    pub fn set_pos(&mut self, pos: usize) {
        self.lineno = 1000000;
        self.read.pos = pos;
        self.read.advance();
    }
}
