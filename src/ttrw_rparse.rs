use {GetMode, SpanToken, Storage, rparse};
use rparse::{Lexer, token, Token, DelimToken, Ident};
use std::cell::UnsafeCell;

pub type OutIdent = String;
pub fn out_ident_from_ident(ident: &Ident) -> OutIdent { ident.name.as_str().to_owned() }

pub struct Splice {
    pos: usize,
    len: usize,
    new: Vec<u8>,
}

#[derive(Copy, Clone)]
pub struct Mark {
    pos: usize,
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
    out: Vec<u8>,
}
impl<'x, 'a: 'x> TTWriter<'x, 'a> {
    pub fn write(&mut self, tok: Token) {
        let s = match tok {
            token::Pound => "#",
            token::Underscore => "_",
            token::Comma => ",",
            token::Colon => ":",
             token::OpenDelim(DelimToken::Bracket) => "[",
            token::CloseDelim(DelimToken::Bracket) => "]",
             token::OpenDelim(DelimToken::Paren) => "(",
            token::CloseDelim(DelimToken::Paren) => ")",
             token::OpenDelim(DelimToken::Brace) => "{",
            token::CloseDelim(DelimToken::Brace) => "}",
            token::Ident(ident) => ident.name.as_str(),
            _ => panic!("missing case"),
        };
        self.out.extend_from_slice(s.as_bytes());
    }
    pub fn write_ident(&mut self, ident_str: &str) {
        self.out.extend_from_slice(ident_str.as_bytes());
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
    storage: &'a UnsafeCell<Storage>,
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
        let span = Span {
            pos: self.lexer.pos(),
            line: self.lexer.line(),
            col: self.lexer.col(),
        };
        match self.lexer.next() {
            token::Eof => None,
            tok @ _ => {
                unsafe {
                    let ptr = self.storage.get();
                    (*ptr).span = span;
                    (*ptr).token = tok;
                    Some(SpanToken { span: &(*ptr).span, token: &(*ptr).token })
                }
            }
        }

    }
    pub fn mark_last(&self) -> Mark {
        unsafe {
            Mark { pos: (*self.storage.get()).span.pos }
        }
    }
    pub fn mark_next(&self) -> Mark {
        Mark { pos: self.lexer.pos() }
    }
    pub fn delete_mark_range(&mut self, start: Mark, end: Mark) {
        self.splices.push(Splice {
            pos: start.pos,
            len: end.pos - start.pos,
            new: Vec::new(),
        });
    }
    pub fn copy_from_mark_range(&mut self, start: Mark, end: Mark, _: GetMode) {
        self.splices.push(Splice {
            pos: self.lexer.pos(),
            len: 0,
            new: self.data[start.pos..end.pos].to_owned(),
        });
    }
    pub fn mutate_ident<F>(&mut self, mark: Mark, f: F) where F: FnOnce(Ident) -> OutIdent {
        let mut lexer_copy = self.lexer;
        lexer_copy.set_pos(mark.pos);
        let ident = match lexer_copy.next() {
            token::Ident(ident) => ident,
            _ => unreachable!(),
        };
        let new = f(ident);
        self.splices.push(Splice {
            pos: mark.pos,
            len: lexer_copy.pos() - mark.pos,
            new: new.into_bytes(),
        });
    }
    pub fn writer<'x>(&'x mut self) -> TTWriter<'x, 'a> {
        TTWriter {
            tr: self,
            pos: self.lexer.pos(),
            out: Vec::new(),
        }
    }
}
