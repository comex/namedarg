use super::{GetMode, SpanToken, Storage};
use super::rlex::{Lexer, token, Token, DelimToken, Ident};
use std::cell::UnsafeCell;
use std::str;

pub struct Splice {
    pos: usize,
    len: usize,
    new: Vec<u8>,
}

#[derive(Copy, Clone, Debug)]
pub struct Mark {
    pos: usize,
}
#[derive(Copy, Clone, Debug)]
pub struct InIdent {
    start: usize,
    end: usize,
}
impl InIdent {
    pub fn mark(&self) -> Mark {
        Mark { pos: self.start }
    }
}

#[derive(Copy, Clone)]
pub struct Span {
    pub pos: usize,
    pub line: usize,
    pub col: usize,
}
pub fn dummy_span() -> Span {
    Span { pos: !0, line: 0, col: 0 }
}
pub struct TTWriter<'x, 'a: 'x> {
    tr: &'x mut TTReader<'a>,
    pos: usize,
    pub out: Vec<u8>,
}
impl<'x, 'a: 'x> TTWriter<'x, 'a> {
    pub fn write(&mut self, tok: Token) {
        let s = match tok {
            token::Pound => "#",
            token::Underscore => "_",
            token::Comma => ",",
            token::Colon => ":",
            token::ModSep => "::",
            token::Lt => "<",
            token::Gt => ">",
             token::OpenDelim(DelimToken::Bracket) => "[",
            token::CloseDelim(DelimToken::Bracket) => "]",
             token::OpenDelim(DelimToken::Paren) => "(",
            token::CloseDelim(DelimToken::Paren) => ")",
             token::OpenDelim(DelimToken::Brace) => "{",
            token::CloseDelim(DelimToken::Brace) => "}",
            token::Ident(ident) => ident.name.as_str(),
            _ => panic!("missing case {:?}", tok),
        };
        self.out.push(b' ');
        self.out.extend_from_slice(s.as_bytes());
        self.out.push(b' ');
    }
    pub fn write_ident_str(&mut self, ident_str: &str) {
        self.out.extend_from_slice(ident_str.as_bytes());
    }
    pub fn copy_from_mark_range(&mut self, start: Mark, end: Mark, _: GetMode) {
        self.out.extend_from_slice(&self.tr.data[start.pos..end.pos]);
    }
    pub fn finish(mut self) {
        self.tr.splices.push(Splice {
            pos: self.pos,
            len: 0,
            new: self.out,
        });
    }

}
pub struct TTReader<'a> {
    data: &'a [u8],
    lexer: Lexer<'a>,
    pub storage: &'a UnsafeCell<Storage>,
    splices: Vec<Splice>,
}
impl<'a> TTReader<'a> {
    pub fn new(data: &'a [u8], storage: &'a UnsafeCell<Storage>) -> Self {
        TTReader {
            data: data,
            lexer: Lexer::new(data),
            storage: storage,
            splices: Vec::new(),
        }
    }
    pub fn next(&mut self) -> Option<SpanToken<'a>> {
        loop {
            let span = Span {
                pos: self.lexer.pos(),
                line: self.lexer.line(),
                col: self.lexer.col(),
            };
            match self.lexer.next() {
                token::Eof => return None,
                token::White => continue,
                tok @ _ => {
                    unsafe {
                        let ptr = self.storage.get();
                        (*ptr).span = span;
                        (*ptr).token = tok;
                        return Some(SpanToken { span: &(*ptr).span, token: &(*ptr).token });
                    }
                },
            }
        }

    }
    pub fn next_no_enter(&mut self) -> Option<SpanToken<'a>> {
        self.next()
    }
    pub fn mark_last(&self) -> Mark {
        unsafe {
            Mark { pos: (*self.storage.get()).span.pos }
        }
    }
    pub fn mark_next(&self) -> Mark {
        Mark { pos: self.lexer.pos() }
    }
    pub fn last_ii(&self, _: &Ident) -> InIdent {
        InIdent {
            start: self.mark_last().pos,
            end: self.lexer.pos(),
        }
    }
    pub fn rewind_to(&mut self, _: Mark, span: Span) {
        self.lexer.rewind_to(span.pos, span.line, span.col);
    }
    pub fn delete_mark_range(&mut self, start: Mark, end: Mark) {
        if start.pos == end.pos { return; }
        self.splices.push(Splice {
            pos: start.pos,
            len: end.pos - start.pos,
            new: Vec::new(),
        });
    }
    pub fn mutate_ident(&mut self, ii: InIdent, new: String) {
        self.splices.push(Splice {
            pos: ii.start,
            len: ii.end - ii.start,
            new: new.into_bytes(),
        });
    }
    pub fn get_ident_str(&self, ii: InIdent) -> &'a str {
        str::from_utf8(&self.data[ii.start..ii.end]).unwrap()
    }
    pub fn writer<'x>(&'x mut self) -> TTWriter<'x, 'a> {
        let pos = self.lexer.pos();
        TTWriter {
            tr: self,
            pos: pos,
            out: Vec::new(),
        }
    }
    pub fn output(&mut self) -> Option<Vec<u8>> {
        if self.splices.is_empty() {
            None
        } else {
            self.splices.sort_by_key(|splice| splice.pos);
            let mut pos = 0;
            let mut out = Vec::with_capacity(self.data.len() +
                                             self.splices.iter()
                                                .map(|s| s.new.len())
                                                .sum::<usize>());
            for splice in &self.splices {
                assert!(pos <= splice.pos);
                out.extend_from_slice(&self.data[pos..splice.pos]);
                //out.push(b'<');
                out.extend_from_slice(&splice.new);
                //out.push(b'>');
                pos = splice.pos + splice.len;
            }
            out.extend_from_slice(&self.data[pos..]);
            Some(out)
        }
    }
}
