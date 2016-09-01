#![feature(plugin_registrar, rustc_private, slice_patterns)]

#[macro_use]
extern crate syntax;
extern crate syntax_pos;
extern crate rustc_plugin;
extern crate rustc_errors as errors;
use syntax::tokenstream::{TokenTree, Delimited};
use syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use syntax_pos::Span;
use syntax::ast;
use syntax::ast::Ident;
use syntax::util::small_vector::SmallVector;
use syntax::print::pprust;
use rustc_plugin::registry::Registry;
//use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::parse::token::{Token, DelimToken};
use syntax::parse::token::keywords;
use std::rc::Rc;
//use std::borrow::Cow;
use std::mem::replace;
use std::cell::UnsafeCell;

fn passthrough_items(cx: &mut ExtCtxt, args: &[TokenTree])
    -> Box<MacResult + 'static> {
    let mut parser = cx.new_parser_from_tts(args);
    let mut items = SmallVector::zero();
    while let Some(item) = panictry!(parser.parse_item()) {
        items.push(item);
    }
    if parser.token != token::Eof {
        let token_str = parser.this_token_to_string();
        let msg = format!("extra tokens starting with `{}`", token_str);
        parser.diagnostic().span_err(parser.span, &msg);
    }
    MacEager::items(items)
}

fn mutate_name<'a, I>(fn_name_ident: &ast::Ident, arg_names: I) -> ast::Ident
    where I: Iterator<Item=Option<&'a ast::Ident>> {
    let mut name: String = (*fn_name_ident.name.as_str()).to_owned();
    name.push('{');
    //name.push_str("_$lbl");
    for arg_name in arg_names {
        //name.push('_');
        if let Some(arg_name) = arg_name {
            name.push_str(&*arg_name.name.as_str());
        }
        name.push(':');
    }
    name.push('}');
    ast::Ident { name: token::intern(&name), ctxt: fn_name_ident.ctxt }
}


struct SpanToken<'a> {
    span: &'a Span,
    token: &'a Token,
}

struct TTReader<'a> {
    stack: Vec<(&'a [TokenTree], &'a [TokenTree], Option<Vec<TokenTree>>)>,
    whole: &'a [TokenTree],
    cur: &'a [TokenTree],
    cur_offset: usize,
    output: Option<Vec<TokenTree>>,
    token_storage: &'a UnsafeCell<Token>,
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
#[derive(Debug)]
struct Mark {
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

impl<'a> TTReader<'a> {
    fn new(initial: &'a [TokenTree], token_storage: &'a UnsafeCell<Token>) -> Self {
        TTReader {
            stack: Vec::new(),
            whole: initial,
            cur: initial,
            cur_offset: 0,
            output: None,
            token_storage: token_storage,
        }
    }
    fn output_as_slice(&self) -> &[TokenTree] {
        if let Some(ref output) = self.output {
            &output[..]
        } else {
            self.whole
        }
    }
    fn next<'b, 'x>(&'x mut self) -> Option<SpanToken<'a>> {
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
                        let ptr = self.token_storage.get();
                        *ptr = token::OpenDelim(delimed.delim);
                        Some(SpanToken { span: span, token: &*ptr })
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
                        let ptr = self.token_storage.get();
                        *ptr = token::CloseDelim(delimed.delim);
                        Some(SpanToken { span: span, token: &*ptr })
                    }
                } else {
                    panic!("bad stack");
                }
            } else {
                None
            }
        }
    }
    fn mark_last(&self) -> Mark {
        Mark {
            cur_offset: self.cur_offset - 1,
            cur_stack_depth: make_cur_stack_depth(self.stack.len())
        }
    }
    fn force_pop(&mut self) {
        let (pwhole, pcur, poutput) = self.stack.pop().expect("force_pop");
        self.whole = pwhole;
        self.cur = pcur;
        self.output = poutput;
        if let Some(ref output) = self.output {
            self.cur_offset = output.len();
        } else {
            self.cur_offset = self.whole.len() - self.cur.len();
        }
    }
    fn offset_in_whole(&self) -> usize {
        self.whole.len() - self.cur.len()
    }
    fn delete_from_mark(&mut self, mark: Mark, count: usize) {
        println!("dfm({})", count);
        self.check_mark(mark);
        let offset = mark.cur_offset;
        let cur_offset = self.cur_offset;
        if let Some(ref mut output) = self.output {
            println!("dfm: old ({:?})", output);
            println!("offset = {} len = {}", offset, output.len());
            for _ in 0..count {
                output.remove(offset);
            }
            self.cur_offset = output.len();
        } else {
            println!("dfm: new");
            let mut vec = self.whole[..offset].to_owned();
            vec.extend_from_slice(&self.whole[offset+count..cur_offset]);
            self.cur_offset = vec.len();
            self.output = Some(vec);
        }
        println!("dfm: now we look like {}", pprust::tts_to_string(self.output.as_ref().unwrap()));
        println!("...cur is {}", pprust::tts_to_string(self.cur));
    }
    fn mutate_mark(&mut self, mark: Mark) -> &mut TokenTree {
        self.check_mark(mark);
        let offset = mark.cur_offset;
        let cur_offset = self.cur_offset;
        if self.output.is_none() {
            self.output = Some(self.whole[..cur_offset].to_owned());
        }
        &mut self.output.as_mut().unwrap()[offset]
    }
    #[cfg(debug_assertions)]
    fn check_mark(&self, mark: Mark) {
        assert_eq!(mark.cur_stack_depth, self.stack.len());
    }
    #[cfg(not(debug_assertions))]
    fn check_mark(&self, _: Mark) {}
}

struct Context<'x, 'y: 'x> {
    cx: &'x mut ExtCtxt<'y>
}

fn do_transform<'x, 'y, 'a: 'x>(tr: &mut TTReader<'a>, ctx: &mut Context<'x, 'y>) {
    #[derive(Debug)]
    enum State {
        Null,
        GotIdent(Mark),
        GotIdentColonColon(Mark),
        GotFn,
        GotFnName { name: Mark, generic_start: Option<Mark> },
        CallArgStart(CallStuff),
        DeclArgStart(DeclStuff),
        SeekingMatchingAngleBracket { angle_depth: usize },
        Dummy,
    }
    struct StackEntry {
        state: State,
        comma_return: Option<State>,
        delim_depth: usize,
    }
    #[derive(Debug)]
    struct CallStuff {
        name: Mark,
        args: Vec<Option<Ident>>,
    }
    #[derive(Debug)]
    struct DeclStuff {
        name: Mark,
        generic_start_end: Option<(Mark, Mark)>,
        args: Vec<Option<Ident>>,
    }
    struct Stuff<'x, 'y: 'x, 'z: 'y, 'a: 'x> {
        ctx: &'x mut Context<'y, 'z>,
        state: State,
        comma_return: Option<State>,
        delim_depth: usize,
        tr: &'x mut TTReader<'a>,
        stack: Vec<StackEntry>,
    }
    let mut s: Stuff = Stuff {
        state: State::Null,
        comma_return: None,
        delim_depth: 0,
        tr: tr,
        stack: Vec::new(),
        ctx: ctx,
    };
    let xsp = &mut s.state as *mut State;
    let mut st: SpanToken<'a>;
    macro_rules! st_or_return {
        () => ( if let Some(st) = s.tr.next() { st } else { return } )
    }
    macro_rules! continue_next {
        ($state:expr) => {{
            let new: State = $state;
            unsafe { *xsp = new; }
            st = st_or_return!();
            continue;
        }}
    }
    macro_rules! continue_same {
        ($state:expr) => {{
            let new: State = $state;
            unsafe { *xsp = new; }
            continue;
        }}
    }
    #[inline(always)]
    fn push<'x, 'y, 'z, 'a>(s: &mut Stuff<'x, 'y, 'z, 'a>, state: State) {
        s.stack.push(StackEntry { state: state, comma_return: replace(&mut s.comma_return, None), delim_depth: s.delim_depth });
        s.comma_return = None;
        s.delim_depth = 0;
    }
    #[inline(always)]
    fn pop<'x, 'y, 'z, 'a>(s: &mut Stuff<'x, 'y, 'z, 'a>) {
        if let Some(entry) = s.stack.pop() {
            s.delim_depth = entry.delim_depth;
            s.state = entry.state;
            s.comma_return = entry.comma_return;
        } else {
            println!("popped out of last state");
            s.delim_depth = 0;
            s.state = State::Null;
            s.comma_return = None;
        }
    }
    st = st_or_return!();
    loop {
        s.ctx.cx.span_warn(*st.span, "hi");
        println!("state={:?} cr={:?}", s.state, s.comma_return);
        println!("depth={} dd={} tok={:?}", s.tr.stack.len(), s.delim_depth, st.token);
        match replace(&mut s.state, State::Dummy) {
            State::Null => {
                match st.token {
                    &token::Ident(ref ident) => {
                        // XXX trait, attr
                        if ident.name == keywords::Fn.name() {
                            continue_next!(State::GotFn);
                        } else {
                            continue_next!(State::GotIdent(s.tr.mark_last()));
                        }
                    },
                    &token::OpenDelim(_) => {
                        s.delim_depth += 1;
                    },
                    &token::CloseDelim(_) => {
                        if s.delim_depth == 0 {
                            if let Some(state) = s.comma_return {
                                s.comma_return = None;
                                continue_same!(state);
                            } else {
                                pop(&mut s);
                            }
                        } else {
                            s.delim_depth -= 1;
                        }
                    },
                    &token::Comma => {
                        if s.delim_depth == 0 {
                            if let Some(state) = s.comma_return {
                                s.comma_return = None;
                                continue_next!(state);
                            }
                        }
                    },
                    _ => (),
                }
                continue_next!(State::Null);
            },
            State::GotIdent(name) => {
                match st.token {
                    &token::ModSep => {
                        continue_next!(State::GotIdentColonColon(name));
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        push(&mut s, State::Null);
                        continue_next!(State::CallArgStart(CallStuff {
                            name: name,
                            args: Vec::new(),
                        }));
                    },
                    _ => {
                        continue_same!(State::Null);
                    },
                }
            },
            State::GotIdentColonColon(name) => {
                match st.token {
                    &token::Lt => {
                        push(&mut s, State::GotIdent(name));
                        continue_next!(State::SeekingMatchingAngleBracket { angle_depth: 1 });
                    },
                    _ => continue_same!(State::Null),
                }
            },
            State::GotFn => {
                match st.token {
                    &token::Ident(_) => {
                        continue_next!(State::GotFnName { name: s.tr.mark_last(), generic_start: None });
                    },
                    _ => continue_same!(State::Null),
                }
            },
            State::GotFnName { name, generic_start } => {
                match st.token {
                    &token::Lt => {
                        let state = State::GotFnName { name: name, generic_start: Some(s.tr.mark_last()) };
                        push(&mut s, state);
                        continue_next!(State::SeekingMatchingAngleBracket { angle_depth: 1 });
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        push(&mut s, State::Null);
                        continue_next!(State::DeclArgStart(DeclStuff {
                            name: name,
                            generic_start_end: if let Some(start) = generic_start {
                                Some((start, s.tr.mark_last()))
                            } else { None },
                            args: Vec::new(),
                        }));
                    },
                    _ => continue_same!(State::Null),
                }
            },
            State::CallArgStart(mut call) => {
                match st.token {
                    &token::Ident(ref ident) => {
                        let mark = s.tr.mark_last();
                        let st2 = st_or_return!();
                        if let &token::Colon = st2.token {
                            call.args.push(Some(*ident));
                            s.tr.delete_from_mark(mark, 2);
                            s.comma_return = Some(State::CallArgStart(call));
                            continue_next!(State::Null);
                        } else {
                            call.args.push(None);
                            s.comma_return = Some(State::CallArgStart(call));
                            continue_same!(State::GotIdent(s.tr.mark_last()));
                        }
                    },
                    &token::CloseDelim(_) => {
                        //println!("call closedelim: {:?}", call.args);
                        if call.args.iter().any(|arg| arg.is_some()) {
                            let name = s.tr.mutate_mark(call.name);
                            match *name {
                                TokenTree::Token(_, token::Ident(ref mut ident)) => {
                                    let new_name = mutate_name(ident, call.args.iter().map(|arg| arg.as_ref()));
                                    *ident = new_name;
                                },
                                _ => unreachable!(),
                            }
                        }
                        pop(&mut s);
                    },
                    _ => {
                        call.args.push(None);
                        s.comma_return = Some(State::CallArgStart(call));
                        continue_same!(State::Null);
                    }
                }
            },
            State::DeclArgStart(mut decl) => {
                match st.token {
                    &Token::Underscore | &token::Ident(_) => {
                        let to_delete = s.tr.mark_last();
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Ident(ident2) => {
                                s.tr.delete_from_mark(to_delete, 1);
                                match st.token {
                                    &token::Ident(ident1) => {
                                        decl.args.push(Some(ident1));
                                    },
                                    &Token::Underscore => {
                                        decl.args.push(Some(ident2));
                                        let st3 = st_or_return!();
                                        if let &Token::Colon = st3.token {} else {
                                            s.ctx.cx.span_err(*st3.span, "_ should be followed by an ident pattern");

                                        }
                                    },
                                    _ => unreachable!(),
                                };
                            },
                            _ => {
                                decl.args.push(None);
                            },
                        }
                        s.comma_return = Some(State::DeclArgStart(decl));
                        continue_same!(State::Null);
                    },
                    &token::CloseDelim(_) => {
                        println!("decl closedelim: {:?}", decl.args);
                        if decl.args.iter().any(|arg| arg.is_some()) {
                            let name = s.tr.mutate_mark(decl.name);
                            match *name {
                                TokenTree::Token(_, token::Ident(ref mut ident)) => {
                                    *ident = mutate_name(ident, decl.args.iter().map(|arg| arg.as_ref()));
                                },
                                _ => unreachable!(),
                            }
                        }
                        pop(&mut s);
                    },
                    _ => {
                        decl.args.push(None);
                        s.comma_return = Some(State::DeclArgStart(decl));
                        continue_same!(State::Null);
                    },
                }
            },
            State::SeekingMatchingAngleBracket { mut angle_depth } => {
                match st.token {
                    &token::Lt => {
                        angle_depth += 1;
                    },
                    &token::Gt => {
                        if angle_depth == 0 {
                            s.ctx.cx.span_err(*st.span, "'>' without '<'. possible parser bug");
                        } else {
                            angle_depth -= 1;
                        }
                        if angle_depth == 0 {
                            if s.delim_depth > 0 {
                                s.ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                                while s.delim_depth > 0 {
                                    s.tr.force_pop();
                                    s.delim_depth -= 1;
                                }
                            }
                            pop(&mut s);
                        }
                    },
                    &token::Comma => {
                        if angle_depth == 0 {
                            if let Some(state) = s.comma_return {
                                s.state = state;
                                s.comma_return = None;
                            }
                        }
                    },
                    &token::Semi => {
                        // only valid inside an array decl, puts us into expression context
                        push(&mut s, State::SeekingMatchingAngleBracket { angle_depth: angle_depth });
                        continue_next!(State::Null);
                    },
                    &token::OpenDelim(_) => {
                        s.delim_depth += 1;
                    },
                    &token::CloseDelim(_) => {
                        if s.delim_depth != 0 {
                            s.delim_depth -= 1;
                        } else {
                            if angle_depth != 0 {
                                // similar
                                s.ctx.cx.span_err(*st.span, "unexpected thingy [todo]. possible parser bug");
                            }
                            pop(&mut s);
                        }
                    },
                    _ => ()
                }
                continue_next!(State::SeekingMatchingAngleBracket { angle_depth: angle_depth });
            },
            State::Dummy => unreachable!(),
        }
        st = st_or_return!();
    }
}
/*
fn transform_this_fn(args: &[TokenTree], cx: &mut ExtCtxt) -> (Option<Vec<TokenTree>>, usize) {
    let first = &args[0];
    let ident = if let &TokenTree::Token(_, token::Ident(ref ident)) = first {
        ident
    } else { return (None, 0) };
    if ident.name != keywords::Fn.name() { return (None, 0); }
    // XXX generics
    let (fn_name_span, fn_name_ident, delimed_span, delimed) =
        if let (Some(&TokenTree::Token(fn_name_span, token::Ident(ref fn_name_ident))),
                Some(&TokenTree::Delimited(delimed_span, ref delimed))) =
               (args.get(1), args.get(2)) {
            (fn_name_span, fn_name_ident, delimed_span, &**delimed)
        } else { return (None, 1) };
    if delimed.delim != token::DelimToken::Paren { return (None, 3) };
    let tts = &delimed.tts[..];
    let mut should_transform = false;
    {
        let mut i = 0;
        'should_transform: loop {
            match (tts.get(i), tts.get(i+1)) {
                (Some(&TokenTree::Token(_, ref second)),
                 Some(&TokenTree::Token(_, token::Ident(_)))) => {
                    match *second {
                        token::Ident(_) | token::Underscore => {
                            should_transform = true;
                            break 'should_transform;
                        },
                        _ => ()
                    }
                },
                (Some(&TokenTree::Token(_, token::Pound)),
                 Some(&TokenTree::Delimited(..))) => {
                    i += 2;
                    continue;
                }
                _ => ()
            }
            loop {
                match tts.get(i) {
                    Some(&TokenTree::Token(_, token::Comma)) => { i += 1; break; },
                    None => { break 'should_transform; },
                    _ => { i += 1; },
                }
            }
        }
    }
    if !should_transform { return (None, 3 + generics.len()) };
    #[derive(Debug)]
    struct Arg<'a> {
        comma: Option<&'a TokenTree>,
        attrs: Vec<TokenTree>,
        is_default: bool,
        name: Option<&'a ast::Ident>,
        pattern: Option<&'a [TokenTree]>,
        temp_name: Option<ast::Ident>,
        ty: &'a [TokenTree],
    }
    let mut parsed_out: Vec<Arg> = Vec::new();
    let mut remaining = tts;
    let mut comma: Option<&TokenTree> = None;
    let mut default_count: usize = 0;
    while remaining.len() != 0 {
        let mut this_arg: &[TokenTree] = remaining;
        let mut next: &[TokenTree] = &[];
        let mut next_comma: Option<&TokenTree> = None;
        let mut colon_idx: Option<usize> = None;
        let mut attrs: Vec<TokenTree> = Vec::new();
        let mut is_default: bool = false;
        while let Some(&TokenTree::Token(_, token::Pound)) = this_arg.get(0) {
            if let Some(&TokenTree::Delimited(delim_span, ref attr_delimed)) = this_arg.get(1) {
                if attr_delimed.delim != token::DelimToken::Bracket { break }
                // ok, this looks like an attr
                let mut out: Vec<TokenTree> = Vec::new();
                let comma_split = attr_delimed.tts.split(|a| {
                    match a { &TokenTree::Token(_, token::Comma) => true, _ => false }
                });
                for attr in comma_split {
                    if let &[TokenTree::Token(_, token::Ident(ref ident))] = attr {
                        if ident.name.as_str() == "default" {
                            is_default = true;
                            default_count += 1;
                            continue;
                        }
                    }
                    out.extend_from_slice(attr);
                }
                if out.len() != 0 {
                    attrs.push(this_arg[0].clone());
                    attrs.push(TokenTree::Delimited(delim_span, Rc::new(Delimited {
                        tts: out,
                        ..**attr_delimed
                    })));
                }
                this_arg = &this_arg[2..];
            } else { break }
        }
        for (i, tt) in this_arg.iter().enumerate() {
            match tt {
                &TokenTree::Token(_, token::Comma) => {
                    next_comma = Some(&this_arg[i]);
                    next = &this_arg[i+1..];
                    this_arg = &this_arg[..i];
                    break;
                },
                &TokenTree::Token(_, token::Colon) => {
                    if colon_idx.is_none() { colon_idx = Some(i); }
                },
                _ => (),
            }
        }

        let name: Option<&ast::Ident> = match this_arg.get(1) {
            Some(&TokenTree::Token(_, token::Ident(ref ident2))) =>
                match this_arg.get(0) {
                    Some(&TokenTree::Token(_, token::Ident(ref ident))) => {
                        this_arg = &this_arg[1..];
                        Some(ident)
                    },
                    Some(&TokenTree::Token(underscore_span, token::Underscore)) => {
                        if colon_idx != Some(2) {
                            let msg = "underscore not followed by an identifier binding";
                            cx.span_err(underscore_span, msg);
                        }
                        this_arg = &this_arg[1..];
                        Some(ident2)
                    },
                    _ => None,
                },
            _ => None
        };
        let (pattern, ty): (Option<&[TokenTree]>, &[TokenTree]) =
            match colon_idx {
                Some(colon_idx) => {
                    let (a, b) = this_arg.split_at(colon_idx);
                    (Some(a), b)
                },
                _ => (None, this_arg)
            };
        parsed_out.push(Arg { attrs: attrs, is_default: is_default, comma: comma, name: name, pattern: pattern, ty: ty });
        remaining = next;
        comma = next_comma;
    }
    let mut replacement: Vec<TokenTree> = Vec::new();
    let mutated_full = mutate_name(fn_name_ident, parsed_out.iter().map(|arg| arg.name));
    if default_count != 0 {
        let mut i = parsed_out.len();
        let fake_span: Span = ..;
        let none: ast::Ident = Ident::with_empty_ctxt(token::Intern("None"));
        while default_count != 0 {
            loop {
                i -= 1;
                if parsed_out[i].is_default { break; }
            }
            i -= 1;
            default_count -= 1;
            let mutated = mutate_name(fn_name_ident, parsed_out[..i].iter().map(|arg| arg.name));

            replacement.push(TokenTree::Token(fake_span, token::Ident(keywords::Fn.ident()))); // fn
            replacement.push(TokenTree::Token(fake_span, token::Ident(mutated))); // name
            replacement.extend_from_slice(generics); // <...>
            let mut arg_tts: Vec<TokenTree> = Vec::new();
            for (j, arg) in parsed_out[..i].iter_mut().enumerate() {
                arg_tts.extend_from_slice(&arg.attrs[..]);
                if j != 0 { arg_tts.push(arg.comma.unwrap().clone()); }
                if arg.temp_name.is_none() {
                    arg.temp_name = Some(if let Some(ref ident) = arg.name {
                            ident.clone()
                        } else {
                            Ident::with_empty_ctxt(token::intern(&format!("arg{}", j)))
                        });
                }
                replacement.push(TokenTree::Token(fake_span, token::Ident(arg.temp_name.unwrap())));
                replacement.push(TokenTree::Token(fake_span, token::Colon));
                arg_tts.extend_from_slice(arg.ty);
            }
            replacement.push(TokenTree::Delimited(fake_span, Rc::new(Delimited { // (...)
                delim: token::DelimToken::Paren,
                open_span: fake_span,
                tts: arg_tts,
                close_span: fake_span,
            })));
            // XX return type

            // body
            let mut body_tts: Vec<TokenTree> = Vec::new();
            // XX <Self as TraitName>::
            body_tts.push(TokenTree::Token(fake_span, token::Ident(mutated_full.clone())));
            let mut recall_tts: Vec<TokenTree> = Vec::new();
            for (j, arg) in parsed_out.iter().enumerate() {
                let the_ident = if j < i {
                    arg.temp_name.as_ref().unwrap().clone()
                } else {
                    none_ident.clone();
                }
                if j != 0 {
                    body_tts.push(TokenTree::Token(fake_span, token::Comma));
                }
                body_tts.push(TokenTree::Token(fake_span, token::Ident(the_ident));
            }
            body_tts.push(TokenTree::Delimited(fake_span, Rc::new(Delimited {
                delim: token::DelimToken::Paren,
                open_span: fake_span,
                tts: recall_tts,
                close_span: fake_span,
            })));

            replacement.push(TokenTree::Delimited(fake_span, Rc::new(Delimited {
                delim: token::DelimToken::Brace,
                open_span: fake_span,
                tts: body_tts,
                close_span: fake_span,
            })));

        }
    }
    let mut replacement_tts: Vec<TokenTree> = Vec::new();
    for (i, arg) in parsed_out.iter().enumerate() {
        replacement_tts.extend_from_slice(&arg.attrs[..]);
        if i != 0 { replacement_tts.push(arg.comma.unwrap().clone()); }
        if let Some(pat) = arg.pattern { replacement_tts.extend_from_slice(pat); }
        replacement_tts.extend_from_slice(arg.ty);
    }
    replacement.push(args[0].clone()); // fn
    replacement.push(TokenTree::Token(fn_name_span, token::Ident(mutated_full))); // name
    replacement.extend_from_slice(generics);
    replacement.push(TokenTree::Delimited(delimed_span, Rc::new(Delimited {
        tts: replacement_tts,
        ..*delimed
    })));

    (Some(replacement), 3 + generics.len())
}

fn transform_this_invocation(args: &[TokenTree]) -> (Option<Vec<TokenTree>>, usize) {
    let (fn_name_span, fn_name_ident, delimed_span, delimed) =
        if let (Some(&TokenTree::Token(fn_name_span, token::Ident(ref fn_name_ident))),
                Some(&TokenTree::Delimited(delimed_span, ref delimed))) =
               (args.get(0), args.get(1)) {
            (fn_name_span, fn_name_ident, delimed_span, &**delimed)
        } else { return (None, 1) };
    if delimed.delim != token::DelimToken::Paren { return (None, 1) };
    let tts = &delimed.tts[..];
    struct Stuff<'a> {
        replacement_tts: Vec<TokenTree>,
        arg_names: Vec<Option<&'a ast::Ident>>,
    }
    let mut num_args = 0;
    let mut stuff: Stuff;
    let mut i = 0;
    loop {
        let tt: &TokenTree = if let Some(tt) = tts.get(i) { tt } else {
            return (None, 2)
        };
        if let &TokenTree::Token(_, token::Ident(ref ident)) = tt {
            if let Some(&TokenTree::Token(_, token::Colon)) = tts.get(i+1) {
                stuff = Stuff {
                    replacement_tts: tts[..i].to_owned(),
                    arg_names: (0..num_args).map(|_| None).collect(),
                };
                stuff.arg_names.push(Some(ident));
                i += 2;
                break;
            }
        }
        num_args += 1;
        loop {
            let next = if let Some(tt) = tts.get(i) { tt } else {
                return (None, 2)
            };
            i += 1;
            if let &TokenTree::Token(_, token::Comma) = next { break }
        }
    }
    'outer: loop {
        loop {
            let next = if let Some(tt) = tts.get(i) { tt } else { break 'outer };
            stuff.replacement_tts.push(next.clone());
            i += 1;
            if let &TokenTree::Token(_, token::Comma) = next { break }
        }
        let tt = if let Some(tt) = tts.get(i) { tt } else { break };
        let mut arg_name = None;
        if let &TokenTree::Token(_, token::Ident(ref ident)) = tt {
            if let Some(&TokenTree::Token(_, token::Colon)) = tts.get(i+1) {
                arg_name = Some(ident);
                i += 2;
            }
        }
        stuff.arg_names.push(arg_name);
    }
    let mutated = mutate_name(fn_name_ident, stuff.arg_names.into_iter());
    let replacement = vec![
        TokenTree::Token(fn_name_span, token::Ident(mutated)),
        TokenTree::Delimited(delimed_span, Rc::new(Delimited {
            tts: stuff.replacement_tts,
            ..*delimed
        }))
    ];
    (Some(replacement), 2)
}

fn transform_this(args: &[TokenTree], cx: &mut xtCtxt) -> (Option<Vec<TokenTree>>, usize) {
    let (x, count) = transform_this_fn(args, cx);
    if count != 0 { return (x, count); }
    transform_this_invocation(args)
}

fn transform_tts_direct<'a>(args: &'a [TokenTree], cx: &mut ExtCtxt) -> Cow<'a, [TokenTree]> {
    let mut remaining_args = args;
    let mut vec: Vec<TokenTree>;
    loop {
        let len = remaining_args.len();
        if len == 0 { return Cow::Borrowed(args); }
        let (x, count) = transform_this(remaining_args, cx);
        remaining_args = &remaining_args[count..];
        if let Some(to_add) = x {
            vec = args[..(args.len() - len)].to_vec();
            vec.extend_from_slice(&to_add[..]);
            break;
        }
    }
    loop {
        if remaining_args.len() == 0 { return Cow::Owned(vec); }
        let (x, count) = transform_this(remaining_args, cx);
        if let Some(to_add) = x {
            vec.extend_from_slice(&to_add[..]);
        } else {
            vec.extend_from_slice(&remaining_args[..count]);
        }
        remaining_args = &remaining_args[count..];
    }
}

fn transform_tts<'a>(args: &'a [TokenTree], cx: &mut ExtCtxt) -> Cow<'a, [TokenTree]> {
    let mut tts: Cow<[TokenTree]> = transform_tts_direct(args, cx);
    //println!("==> {:?}", tts);
    let mut replacement: Option<(usize, Vec<TokenTree>)> = None;
    let mut mut_start_i: Option<usize> = None;
    if let Cow::Borrowed(btts) = tts {
        for (i, tt) in btts.iter().enumerate() {
            if let &TokenTree::Delimited(_, ref delimed) = tt {
                let new_inner = transform_tts(&delimed.tts[..], cx);
                if let Cow::Owned(xreplacement) = new_inner {
                    replacement = Some((i, xreplacement));
                    mut_start_i = Some(i + 1);
                    break;
                }
            }
        }
    } else {
        mut_start_i = Some(0);
    }
    if let Some((i, replacement)) = replacement {
        if let TokenTree::Delimited(_, ref mut delimed) = tts.to_mut()[i] {
            *delimed = Rc::new(Delimited {
                tts: replacement,
                ..**delimed
            });
        } else { panic!(); }
    }
    if let Some(i) = mut_start_i {
        for tt in tts.to_mut()[i..].iter_mut() {
            if let &mut TokenTree::Delimited(_, ref mut delimed) = tt {
                let replacement = {
                    let new_inner = transform_tts(&delimed.tts[..], cx);
                    if let Cow::Owned(replacement) = new_inner { Some(replacement) } else { None }
                };
                if let Some(replacement) = replacement {
                    *delimed = Rc::new(Delimited {
                        tts: replacement,
                        ..**delimed
                    });
                }
            }
        }
    }
    tts
}
*/

fn expand_namedarg<'a, 'b>(cx: &'a mut ExtCtxt, _sp: Span, args: &'b [TokenTree])
        -> Box<MacResult + 'static> {
    let token_storage = UnsafeCell::new(token::DotDot);
    let mut tr = TTReader::new(args, &token_storage);
    let mut ctx = Context { cx: cx };
    do_transform(&mut tr, &mut ctx);
    let output = tr.output_as_slice();
    println!("==> {}", pprust::tts_to_string(output));
    passthrough_items(ctx.cx, output)
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("namedarg", expand_namedarg);
}

