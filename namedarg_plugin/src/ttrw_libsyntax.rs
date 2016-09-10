use super::{GetMode, SpanToken, Storage};
use super::syntax::parse::token;
use super::syntax::ast::Ident;
use super::syntax::parse::token::Token;
use super::syntax::tokenstream::{TokenTree, Delimited};
use super::syntax_pos;
use super::syntax_pos::Span;
use std::mem::replace;
use std::rc::Rc;
use std::cell::UnsafeCell;
use std::mem::transmute;

pub fn dummy_span() -> Span {
    syntax_pos::COMMAND_LINE_SP
}

pub struct TTWriter<'x, 'a: 'x> {
    tr: &'x mut TTReader<'a>,
    output_stack: Vec<Vec<TokenTree>>,
    output: Vec<TokenTree>,
    span: Span,
}

impl<'x, 'a: 'x> TTWriter<'x, 'a> {
    pub fn write(&mut self, tok: Token) {
        //println!("*write* {:?}", tok);
        match tok {
            token::OpenDelim(_) => {
                self.output_stack.push(replace(&mut self.output, Vec::new()));
            },
            token::CloseDelim(delim) => {
                let mut output = self.output_stack.pop().unwrap();
                output.push(TokenTree::Delimited(self.span, Rc::new(Delimited {
                    delim: delim,
                    open_span: self.span,
                    tts: replace(&mut self.output, Vec::new()),
                    close_span: self.span,
                })));
                self.output = output;
            },
            _ => {
                self.output.push(TokenTree::Token(self.span, tok));
            },
        }
    }
    pub fn write_ident_str(&mut self, ident_str: &str) {
        self.write(Token::Ident(Ident::with_empty_ctxt(token::intern(ident_str))))
    }
    pub fn copy_from_mark_range(&mut self, start: Mark, end: Mark, gm: GetMode) {
        //println!("CFMR: enter={:?} start={:?} end={:?}", enter, start, end);
        //println!("CFMR: have {:?}",self.output);
        let to_add: Vec<TokenTree> = self.replacing_output(|tr| tr.get(start, end, gm).to_owned());
        let storage = UnsafeCell::new(Storage::new());
        let mut tr2 = TTReader::new(&to_add, &storage);
        while let Some(st) = tr2.next() {
            self.write(st.token.clone());
        }
        //println!("CFMR out");
    }
    pub fn replacing_output<O, F: FnOnce(&mut TTReader<'a>) -> O>(&mut self, f: F) -> O {
        let ptr = if self.output_stack.is_empty() { &mut self.output } else { self.output_stack.first_mut().unwrap() };
        self.tr.output = Some(replace(ptr, Vec::new()));
        let o = f(self.tr);
        *ptr = replace(&mut self.tr.output, None).unwrap();
        o
    }
    pub fn last_normal_token(&mut self) -> Option<&mut Token> {
        match self.output.last_mut() {
            Some(&mut TokenTree::Token(_, ref mut token)) => Some(token),
            _ => None,
        }
    }
    pub fn finish(mut self) {
        assert!(self.output_stack.is_empty());
        self.tr.cur_offset = self.output.len();
        self.tr.output = Some(self.output);
    }
}

#[cfg(debug_assertions)]
type MarkCurStackDepth = usize;
#[cfg(debug_assertions)] #[inline(always)]
fn make_cur_stack_depth(x: usize) -> MarkCurStackDepth { x }
#[cfg(not(debug_assertions))]
type MarkCurStackDepth = ();
#[cfg(not(debug_assertions))] #[inline(always)]
fn make_cur_stack_depth(_: usize) -> MarkCurStackDepth { () }

#[derive(Copy, Clone)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub struct Mark {
    cur_stack_depth: MarkCurStackDepth,
    cur_offset: usize,
}
/*
impl Mark {
    fn dummy() -> Self {
        Mark { cur_len: !0usize / 2, cur_stack_depth: make_cur_stack_depth(!0usize / 2) }
    }
}
*/
#[derive(Copy, Clone)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub struct InIdent {
    mark: Mark,
    ident: Ident,
}
impl InIdent {
    #[inline]
    pub fn mark(&self) -> Mark {
        self.mark
    }
}

pub struct TTReader<'a> {
    stack: Vec<(&'a [TokenTree], &'a [TokenTree], Option<Vec<TokenTree>>)>,
    whole: &'a [TokenTree],
    cur: &'a [TokenTree],
    cur_offset: usize,
    pub output: Option<Vec<TokenTree>>,
    pub storage: &'a UnsafeCell<Storage>,
}

impl<'a> TTReader<'a> {
    pub fn new(initial: &'a [TokenTree], storage: &'a UnsafeCell<Storage>) -> Self {
        TTReader {
            stack: Vec::new(),
            whole: initial,
            cur: initial,
            cur_offset: 0,
            output: None,
            storage: storage,
        }
    }
    pub fn output_as_slice(&self) -> &[TokenTree] {
        if let Some(ref output) = self.output {
            &output[..]
        } else {
            self.whole
        }
    }
    pub fn next(&mut self) -> Option<SpanToken<'a>> {
        let otok: Option<&'a TokenTree> = self.cur.get(0);
        if let Some(tok) = otok {
            let tok: &'a TokenTree = tok;
            match tok {
                &TokenTree::Token(ref span, ref token) => {
                    let span: &'a Span = span;
                    let token: &'a Token = token;
                    self.cur = &self.cur[1..];
                    self.cur_offset += 1;
                    if let Some(ref mut output) = self.output {
                        output.push(tok.clone());
                    }
                    Some(SpanToken { span: span, token: token })
                },
                &TokenTree::Delimited(ref span, ref delimed) => {
                    let span: &'a Span = span;
                    let delimed: &'a Delimited = delimed;
                    let tts = &delimed.tts[..];
                    let p = (
                        replace(&mut self.whole, tts),
                        replace(&mut self.cur, tts),
                        replace(&mut self.output, None),
                    );
                    self.stack.push(p);
                    self.cur_offset = 0;
                    unsafe {
                        let ptr = self.storage.get();
                        (*ptr).token = token::OpenDelim(delimed.delim);
                        Some(SpanToken { span: span, token: &(*ptr).token })
                    }
                },
                _ => panic!("unexpected tt type"),
            }
        } else {
            if let Some((pwhole, pcur, poutput)) = self.stack.pop() {
                if let TokenTree::Delimited(ref span, ref delimed) = pcur[0] {
                    let old_whole = replace(&mut self.whole, pwhole);
                    self.cur = &pcur[1..];
                    let old_output = replace(&mut self.output, poutput);
                    if let Some(ref mut output) = self.output {
                        let tts = old_output.unwrap_or_else(|| old_whole.to_owned());
                        output.push(TokenTree::Delimited(span.clone(), Rc::new(Delimited {
                            tts: tts,
                            ..**delimed
                        })));
                        self.cur_offset = output.len();
                    } else if let Some(tts) = old_output {
                        let mut new_output = self.whole[..self.offset_in_whole()-1].to_owned();
                        new_output.push(TokenTree::Delimited(span.clone(), Rc::new(Delimited {
                            tts: tts,
                            ..**delimed
                        })));
                        self.cur_offset = new_output.len();
                        self.output = Some(new_output);
                    } else {
                        self.cur_offset = self.whole.len() - self.cur.len();
                    }
                    unsafe {
                        let ptr = self.storage.get();
                        (*ptr).token = token::CloseDelim(delimed.delim);
                        Some(SpanToken { span: span, token: &(*ptr).token })
                    }
                } else {
                    panic!("bad stack");
                }
            } else {
                None
            }
        }
    }
    pub fn mark_last(&self) -> Mark {
        self.check_offset();
        if self.cur_offset == 0 {
            let &(pwhole, pcur, ref poutput) = self.stack.last().unwrap();
            let pos = if let &Some(ref poutput) = poutput {
                poutput.len()
            } else {
                pwhole.len() - pcur.len()
            };
            Mark {
                cur_offset: pos,
                cur_stack_depth: make_cur_stack_depth(self.stack.len() - 1),
            }
        } else {
            Mark {
                cur_offset: self.cur_offset - 1,
                cur_stack_depth: make_cur_stack_depth(self.stack.len()),
            }
        }
    }
    pub fn mark_next(&self) -> Mark {
        self.check_offset();
        Mark {
            cur_offset: self.cur_offset,
            cur_stack_depth: make_cur_stack_depth(self.stack.len()),
        }
    }
    pub fn last_ii(&self, ident: &Ident) -> InIdent {
        InIdent {
            mark: self.mark_last(),
            ident: *ident,
        }
    }
    #[cfg(debug_assertions)]
    fn check_offset(&self) {
        assert_eq!(self.cur_offset, if let Some(ref o) = self.output { o.len() } else { self.offset_in_whole() });
    }
    #[cfg(not(debug_assertions))]
    fn check_offset(&self) {}
    fn offset_in_whole(&self) -> usize {
        self.whole.len() - self.cur.len()
    }
    pub fn delete_mark_range(&mut self, start: Mark, end: Mark) {
        self.check_mark(start);
        self.check_mark(end);
        let count = end.cur_offset - start.cur_offset;
        if let Some(ref mut output) = self.output {
            for _ in 0..count {
                output.remove(start.cur_offset);
            }
            self.cur_offset = output.len();
        } else {
            //println!("dfm: new");
            let mut vec = self.whole[..start.cur_offset].to_owned();
            vec.extend_from_slice(&self.whole[end.cur_offset..self.cur_offset]);
            self.cur_offset = vec.len();
            self.output = Some(vec);
        }
    }
    pub fn mutate_ident(&mut self, ii: InIdent, new: String) {
        let name = self.mutate_mark(ii.mark);
        if let TokenTree::Token(_, token::Ident(ref mut ident)) = *name {
            *ident = Ident::with_empty_ctxt(token::intern(&new));
        } else {
            unreachable!()
        }
    }
    pub fn get_ident_str(&self, ii: InIdent) -> &'a str {
        let ident = &ii.ident;
        let s: &str = &*ident.name.as_str();
        unsafe { transmute(s) }
    }
    fn mutate_mark(&mut self, mark: Mark) -> &mut TokenTree {
        self.check_mark(mark);
        let offset = mark.cur_offset;
        if self.output.is_none() {
            self.output = Some(self.whole[..self.cur_offset].to_owned());
        }
        &mut self.output.as_mut().unwrap()[offset]
    }
    pub fn writer<'x>(&'x mut self) -> TTWriter<'x, 'a> {
        let output = if let Some(output) = replace(&mut self.output, None) {
            output
        } else {
            self.whole[..self.cur_offset].to_owned()
        };
        TTWriter {
            tr: self,
            output_stack: Vec::new(),
            output: output,
            span: syntax_pos::COMMAND_LINE_SP,
        }
    }
    fn check_mark(&self, _mark: Mark) {
        if_debug_assertions! {
            assert_eq!(_mark.cur_stack_depth, self.stack.len());
        }
    }
    fn get(&self, start: Mark, end: Mark, gm: GetMode) -> &[TokenTree] {
        if_debug_assertions! {
            assert_eq!(start.cur_stack_depth, end.cur_stack_depth);
        }
        let _cur_stack_depth = self.stack.len();
        let base = if let Some(ref out) = self.output { &out[..] } else { self.whole };
        let base = match gm {
            GetMode::InnerDepth(mark) => {
                if_debug_assertions! {
                    assert_eq!(start.cur_stack_depth, _cur_stack_depth + 1);
                    assert_eq!(mark.cur_stack_depth, _cur_stack_depth);
                }
                if let &TokenTree::Delimited(_, ref delimed) = &base[mark.cur_offset] {
                    &delimed.tts[..]
                } else { unreachable!() }
            },
            GetMode::OuterDepth => {
                if_debug_assertions! {
                    assert_eq!(start.cur_stack_depth, _cur_stack_depth - 1);
                }
                self.stack.last().unwrap().0
            },
            GetMode::SameDepth => {
                if_debug_assertions! {
                    assert_eq!(start.cur_stack_depth, _cur_stack_depth);
                }
                base
            },
        };
        &base[start.cur_offset..end.cur_offset]
    }
}

