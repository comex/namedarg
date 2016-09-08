use ::{GetMode, SpanToken, StorageCell, rparse}
use ::rparse::{Lexer, token};
use std::cell::UnsafeCell;
pub type OutIdent = String;

pub struct Splice {
    pos: usize,
    len: usize,
    new: String,
}

pub struct Mark {
    pos: usize,
}
pub struct Span {
    lineno: usize,
    pos: usize,
}
pub fn dummy_span() -> Span {
    Span { lineno: 0, pos: 0 }
}
pub struct TTWriter<'x, 'a: 'x> {
    tr: &'x mut TTReader<'a>,
    pos: usize,
    out: String,
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
            token::Ident(ident) => ident.as_str().unwrap(),
            _ => panic!("missing case"),
        }

    }
    pub fn write_ident(&mut self, ident_str: &str) {
        self.out.push_str(ident_str);
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
    pub fn new(data: &'a [u8], span_storage: &'a UnsafeCell<Span>)) -> Self {
        TTReader {
            data: data,
            lexer: Lexer::new(data),
            storage: storage,
            splices: Vec::new(),
        }
    }
    pub fn next(&mut self) -> Option<SpanToken<'a>> {
        let span = Span { lineno: self.lexer.lineno(), pos: self.lexer.pos() };
        match self.lexer.next() {
            token::Eof => None,
            tok @ _ => {
                unsafe {
                    let ptr = self.storage.get();
                    (*ptr).span = span;
                    (*ptr).token = tok;
                    SpanToken { span: &(*ptr).span, token: &(*ptr).token }
                };
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
            new: String::new(),
        });
    }
    pub fn copy_from_mark_range(&mut self, start: Mark, end: Mark, _: GetMode) {
        self.splices.push(Splice {
            pos: self.lexer.pos(),
            len: 0,
            new: self.data[start.cur_offset..end.cur_offset].to_owned(),
        });
    }
    pub fn mutate_ident<F>(&mut self, mark: Mark, f: F) where F: FnOnce(Ident) -> OutIdent {
        let mut lexer_copy = self.lexer;
        lexer_copy.set_pos(mark.pos);
        let ident = match lexer_copy.next() {
            token::Ident(ident) => ident,
            _ => unreachable!(),
        }
        let new = f(ident);
        self.splices.push(Splice {
            pos: mark.pos,
            len: lexer_copy.pos() - mark.pos,
            new: new,
        });
    }
    pub fn writer<'x>(&'x mut self) -> TTWriter<'x, 'a> {
        TTWriter {
            tr: self,
            pos: self.lexer.pos(),
            out: String::new(),
        }
    }
}
