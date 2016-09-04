#![feature(plugin_registrar, rustc_private, slice_patterns, relaxed_adts)]
/*
Known mishandled cases:
macro_rules! a {
    ($x:type) => { foo(x: y as $x < z) }
    // correct parse depends on whether $x is a type or ident
}
*/

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
//use syntax::print::pprust;
use rustc_plugin::registry::Registry;
use syntax::parse::token;
use syntax::parse::token::{Token, DelimToken};
use syntax::parse::token::keywords;
use std::rc::Rc;
use std::mem::replace;
use std::cell::UnsafeCell;

#[cfg(not(println_spam))]
macro_rules! if_println_spam { {$($stuff:tt)*} => {} }
#[cfg(println_spam)]
macro_rules! if_println_spam { {$($stuff:tt)*} => {$($stuff)*} }

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
    let mut any = false;
    for arg_name in arg_names {
        //name.push('_');
        if let Some(arg_name) = arg_name {
            name.push_str(&*arg_name.name.as_str());
        }
        name.push(':');
        any = true;
    }
    if !any {
        // if all arguments defaulted, use an untransformed ident
        return *fn_name_ident;
    }
    name.push('}');
    ast::Ident { name: token::intern(&name), ctxt: fn_name_ident.ctxt }
}

struct TTWriter<'x, 'a: 'x> {
    tr: &'x mut TTReader<'a>,
    output_stack: Vec<Vec<TokenTree>>,
    output: Vec<TokenTree>,
    span: Span,
}

impl<'x, 'a: 'x> TTWriter<'x, 'a> {
    fn write(&mut self, tok: Token) {
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
    fn get(&self, start: Mark, end: Mark) -> &[TokenTree] {
        self.tr.check_mark(start);
        self.tr.check_mark(end);
        let output0 = self.output_stack.get(0).unwrap_or(&self.output);
        &output0[start.cur_offset..end.cur_offset]
    }
    fn copy_from_mark_range(&mut self, enter: Option<Mark>, start: Mark, end: Mark) {
        //println!("CFMR: enter={:?} start={:?} end={:?}", enter, start, end);
        //println!("CFMR: have {:?}",self.output);
        let to_add: Vec<TokenTree> = {
            let x: &[TokenTree] = if let Some(enter) = enter {
                self.tr.check_mark_plus1(start);
                self.tr.check_mark_plus1(end);
                let mut enter_plus = enter;
                enter_plus.cur_offset += 1;
                let inner = &self.get(enter, enter_plus)[0];
                if let &TokenTree::Delimited(_, ref delimed) = inner {
                    &delimed.tts[(start.cur_offset)..(end.cur_offset)]
                } else { unreachable!() }
            } else {
                self.get(start, end)
            };
            x.to_owned()
        };
        let token_storage = UnsafeCell::new(token::DotDot);
        let mut tr2 = TTReader::new(&to_add, &token_storage);
        while let Some(st) = tr2.next() {
            self.write(st.token.clone());
        }
        //println!("CFMR out");
    }
    fn finish(mut self) {
        assert!(self.output_stack.is_empty());
        self.tr.cur_offset = self.output.len();
        self.tr.output = Some(self.output);
    }
}

struct SpanToken<'a> {
    span: &'a Span,
    token: &'a Token,
}

pub struct TTReader<'a> {
    stack: Vec<(&'a [TokenTree], &'a [TokenTree], Option<Vec<TokenTree>>)>,
    whole: &'a [TokenTree],
    cur: &'a [TokenTree],
    cur_offset: usize,
    pub output: Option<Vec<TokenTree>>,
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
#[cfg_attr(feature = "derive_debug", derive(Debug))]
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
    pub fn new(initial: &'a [TokenTree], token_storage: &'a UnsafeCell<Token>) -> Self {
        TTReader {
            stack: Vec::new(),
            whole: initial,
            cur: initial,
            cur_offset: 0,
            output: None,
            token_storage: token_storage,
        }
    }
    pub fn output_as_slice(&self) -> &[TokenTree] {
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
    fn mark_next(&self) -> Mark {
        Mark {
            cur_offset: self.cur_offset,
            cur_stack_depth: make_cur_stack_depth(self.stack.len()),
        }
    }
    fn offset_in_whole(&self) -> usize {
        self.whole.len() - self.cur.len()
    }
    fn delete_from_mark(&mut self, mark: Mark, count: usize) {
        //println!("dfm({})", count);
        self.check_mark(mark);
        let offset = mark.cur_offset;
        let cur_offset = self.cur_offset;
        if let Some(ref mut output) = self.output {
            //println!("dfm: old ({:?})", output);
            //println!("offset = {} len = {}", offset, output.len());
            for _ in 0..count {
                output.remove(offset);
            }
            self.cur_offset = output.len();
        } else {
            //println!("dfm: new");
            let mut vec = self.whole[..offset].to_owned();
            vec.extend_from_slice(&self.whole[offset+count..cur_offset]);
            self.cur_offset = vec.len();
            self.output = Some(vec);
        }
        //println!("dfm: now we look like {}", pprust::tts_to_string(self.output.as_ref().unwrap()));
        //println!("...cur is {}", pprust::tts_to_string(self.cur));
    }
    fn mutate_mark(&mut self, mark: Mark) -> &mut TokenTree {
        self.check_mark(mark);
        let offset = mark.cur_offset;
        if self.output.is_none() {
            self.output = Some(self.whole[..self.cur_offset].to_owned());
        }
        &mut self.output.as_mut().unwrap()[offset]
    }
    fn writer<'x>(&'x mut self, span: Span) -> TTWriter<'x, 'a> {
        let output = if let Some(output) = replace(&mut self.output, None) {
            output
        } else {
            self.whole[..self.cur_offset].to_owned()
        };
        TTWriter { tr: self, output_stack: Vec::new(), output: output, span: span }
    }
    #[cfg(debug_assertions)]
    fn check_mark(&self, mark: Mark) {
        assert_eq!(mark.cur_stack_depth, self.stack.len());
    }
    #[cfg(not(debug_assertions))]
    fn check_mark(&self, _: Mark) {}
    #[cfg(debug_assertions)]
    fn check_mark_plus1(&self, mark: Mark) {
        assert_eq!(mark.cur_stack_depth, self.stack.len() + 1);
    }
    #[cfg(not(debug_assertions))]
    fn check_mark_plus1(&self, _: Mark) {}
}

pub trait ExtCtxtish {
    fn span_err(&self, sp: Span, msg: &str);
    fn span_warn(&self, sp: Span, msg: &str);
}

impl<'a> ExtCtxtish for ExtCtxt<'a> {
    fn span_err(&self, sp: Span, msg: &str) { ExtCtxt::span_err(self, sp, msg) }
    fn span_warn(&self, sp: Span, msg: &str) { ExtCtxt::span_warn(self, sp, msg) }
}
impl ExtCtxtish for errors::Handler {
    fn span_err(&self, sp: Span, msg: &str) { errors::Handler::span_err(self, sp, msg) }
    fn span_warn(&self, sp: Span, msg: &str) { errors::Handler::span_warn(self, sp, msg) }
}

pub struct Context<'x, EC: ExtCtxtish + 'x> {
    pub cx: &'x EC
}
#[cfg_attr(feature = "derive_debug", derive(Debug))]
enum State {
    Null { expecting_operator: bool }, // pops on , or closing delim, not skipping
    GotIdent { ident: Mark },
    GotIdentColonColon { ident: Mark },
    GotFn,
    GotFnName { name: Mark, generic_start: Option<Mark> },
    CallArgStart(CallStuff), // skips
    DeclArgStart { decl: DeclStuff, pending_default: bool }, // skips
    DeclEnd { decl: DeclStuff, etc: Box<(
        /*old_name*/      ast::Ident,
        /*new_full_name*/ ast::Ident,
        /*args_end*/      Mark,
    )> },
    SeekingSemiOrOpenBrace,
    DefinitelyType { angle_depth: usize, eager_exit: bool, start_of_type: bool }, // pops on closing >, skipping it
    LambdaEnd,
    ExcessCloses,
    Dummy,
}
struct StackEntry {
    state: State,
    delim_depth: usize,
    in_func_parens: bool,
}
#[cfg_attr(feature = "derive_debug", derive(Debug))]
struct CallStuff {
    name: Mark,
    args: Vec<Option<Ident>>,
}
#[cfg_attr(feature = "derive_debug", derive(Debug))]
struct DeclStuff {
    name: Mark,
    args: Vec<DeclArg>,
    generic_start: Option<Mark>,
    args_start: Mark,
    decl_end: Option<Mark>,
}
#[derive(Clone, Copy)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
struct DeclArg {
    name: Option<Ident>,
    ty_start: Option<Mark>,
    ty_end: Option<Mark>,
    is_default: bool,
}

fn gen_default_stub<'a>(tr: &mut TTReader<'a>, args: &[DeclArg], num_include: usize, generic_start: Option<Mark>, args_start: Mark, args_end: Mark, decl_end: Mark, old_name: &ast::Ident, new_full_name: &ast::Ident) {
    let none_ident = Ident::with_empty_ctxt(token::intern("None"));
    let allow_ident = Ident::with_empty_ctxt(token::intern("allow"));
    let dead_code_ident = Ident::with_empty_ctxt(token::intern("dead_code"));
    let mut tw = tr.writer(syntax_pos::COMMAND_LINE_SP);
    // #[allow(dead_code)]
    tw.write(token::Pound);
    tw.write(token::OpenDelim(DelimToken::Bracket));
    tw.write(token::Ident(allow_ident));
    tw.write(token::OpenDelim(DelimToken::Paren));
    tw.write(token::Ident(dead_code_ident));
    tw.write(token::CloseDelim(DelimToken::Paren));
    tw.write(token::CloseDelim(DelimToken::Bracket));

    tw.write(token::Ident(keywords::Fn.ident()));
    let partial_name = mutate_name(old_name, {
        let mut i = 0;
        args.iter().filter_map(move |arg| {
            if arg.is_default {
                i += 1;
                if i > num_include {
                    return None;
                }
            }
            Some(arg.name.as_ref())
        })
    });
    tw.write(token::Ident(partial_name));
    if let Some(generic_start) = generic_start {
        tw.copy_from_mark_range(None, generic_start, args_start);
    }
    tw.write(token::OpenDelim(DelimToken::Paren));
    let mut i = 0;
    let mut arg_names: Vec<Ident> = Vec::with_capacity(args.len());
    for arg in args {
        if arg.is_default {
            i += 1;
            if i > num_include {
                arg_names.push(none_ident);
                continue;
            }
        }
        let arg_name = Ident::with_empty_ctxt(token::intern(&format!("x{}", arg_names.len())));
        tw.write(Token::Ident(arg_name));
        arg_names.push(arg_name);
        if let (Some(start), Some(end)) = (arg.ty_start, arg.ty_end) {
            tw.copy_from_mark_range(Some(args_start), start, end);
        } else {
            // huh?
            tw.write(token::Colon);
            tw.write(token::Underscore);
            tw.write(token::Comma);
        }
    }
    tw.write(token::CloseDelim(DelimToken::Paren));
    tw.copy_from_mark_range(None, args_end, decl_end);
    tw.write(token::OpenDelim(DelimToken::Brace));
    tw.write(token::Ident(*new_full_name));
    tw.write(token::OpenDelim(DelimToken::Paren));
    for name in arg_names {
        tw.write(token::Ident(name));
        tw.write(token::Comma);
    }
    tw.write(token::CloseDelim(DelimToken::Paren));
    tw.write(token::CloseDelim(DelimToken::Brace));
    tw.finish();
}

pub fn do_transform<'x, 'a: 'x, EC: ExtCtxtish + 'x>(tr: &mut TTReader<'a>, ctx: &mut Context<'x, EC>) {
    let default_name = token::intern("default");
    let mut state: State = State::Null { expecting_operator: false };
    let mut delim_depth: usize = 0;
    let mut in_func_parens: bool = false;
    let mut stack: Vec<StackEntry> = vec![StackEntry { state: State::ExcessCloses, delim_depth: 0, in_func_parens: false }];
    let xsp = &mut state as *mut State;
    let mut st: SpanToken<'a>;
    macro_rules! st_or_return {
        () => ( if let Some(st) = tr.next() { st } else { return } )
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
    macro_rules! push { ($state:expr) => { {
        let _state = $state;
        stack.push(StackEntry { state: _state, delim_depth: delim_depth, in_func_parens: in_func_parens });
        delim_depth = 0;
        in_func_parens = false;
        let _ = in_func_parens; // avoid warning
    } } }
    macro_rules! pop { () => { {
        let entry = stack.pop().unwrap();
        delim_depth = entry.delim_depth;
        in_func_parens = entry.in_func_parens;
        entry.state
    } } }
    st = st_or_return!();
    loop {
        if_println_spam! {
            println!("stack={} state={:?}", stack.len(), state);
            println!("parserdepth={} delim_depth={} tok={:?}", tr.stack.len(), delim_depth, st.token);
            ctx.cx.span_warn(*st.span, "hi");
        }
        match replace(&mut state, State::Dummy) {
            State::Null { mut expecting_operator } => {
                match st.token {
                    &token::Ident(ref ident) => {
                        // XXX trait, attr
                        let name = ident.name;
                        if name.0 >= keywords::Default.name().0 ||
                           name == keywords::SelfValue.name() ||
                           name == keywords::SelfType.name() ||
                           name == keywords::Super.name() {
                            continue_next!(State::GotIdent { ident: tr.mark_last() });
                        }
                        // this is a keyword
                        if name == keywords::Fn.name() {
                            continue_next!(State::GotFn);
                        }
                        if name == keywords::As.name() && in_func_parens && delim_depth == 0 {
                            push!(State::Null { expecting_operator: true });
                            continue_next!(State::DefinitelyType { angle_depth: 0, eager_exit: false, start_of_type: true });
                        }
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::OpenDelim(_) => {
                        delim_depth += 1;
                        expecting_operator = false;
                    },
                    &token::CloseDelim(_) => {
                        if delim_depth == 0 {
                            continue_same!(pop!());
                        } else {
                            delim_depth -= 1;
                        }
                        expecting_operator = true;
                    },
                    &token::Comma => {
                        if delim_depth == 0 {
                            continue_same!(pop!());
                        }
                        expecting_operator = false;
                    },
                    &token::Colon => {
                        if delim_depth == 0 && in_func_parens {
                            match stack.last_mut() {
                                Some(&mut StackEntry { state: State::DeclArgStart { ref mut decl, .. }, .. }) => {
                                    decl.args.last_mut().unwrap().ty_start = Some(tr.mark_last());
                                },
                                _ => ()
                            }
                            push!(State::Null { expecting_operator: true });
                            continue_next!(State::DefinitelyType { angle_depth: 0, eager_exit: false, start_of_type: true });
                        }
                    },
                    &token::Lt => {
                        // I had a clever scheme to not have to know whether we were expecting an
                        // operator, but lambdas ruined it.
                        if expecting_operator {
                            expecting_operator = false;
                        } else {
                            if delim_depth == 0 && in_func_parens {
                                push!(State::Null { expecting_operator: true });
                                continue_next!(State::DefinitelyType { angle_depth: 1, eager_exit: true, start_of_type: true });
                            }
                        }
                    },
                    &token::BinOp(token::Or) if !expecting_operator => {
                        if delim_depth == 0 {
                            if let Some(&StackEntry { state: State::LambdaEnd, .. }) = stack.last() {
                                // end of lambda
                                continue_next!(pop!());
                            }
                        }
                        // start of lambda
                        push!(State::LambdaEnd);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::Eq | &token::Le | &token::EqEq | &token::Ne | &token::Ge |
                    &token::Gt | &token::AndAnd | &token::OrOr | &token::Not | &token::Tilde |
                    &token::BinOp(_) | &token::BinOpEq(_) | &token::At | &token::Dot | &token::DotDot |
                    &token::DotDotDot | &token::Semi | &token::ModSep |
                    &token::LArrow | &token::RArrow | &token::FatArrow | &token::Dollar => {
                        expecting_operator = false;
                    },
                    &token::Question | &token::Literal(..) | &token::Underscore => {
                        expecting_operator = true;
                    },
                    &token::Pound => {
                        // this should always be followed by [ so doesn't matter
                        expecting_operator = false;
                    },
                    &token::Lifetime(..) => {
                        // this is probably actually a type
                    }
                    &token::DocComment(..) | &token::Whitespace | &token::Comment | &token::Shebang(..) |
                    &token::Eof => {
                        // don't really affect the AST
                    },
                    &token::Interpolated(..) | &token::MatchNt(..) | &token::SubstNt(..) |
                    &token::SpecialVarNt(..) => {
                        panic!("shouldn't get tokens like {:?}", st.token);
                    },
                }
                continue_next!(State::Null { expecting_operator: expecting_operator });
            },
            State::GotIdent { ident } => {
                match st.token {
                    &token::ModSep => {
                        continue_next!(State::GotIdentColonColon { ident: ident });
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        push!(State::Null { expecting_operator: true });
                        in_func_parens = true;
                        continue_next!(State::CallArgStart(CallStuff {
                            name: ident,
                            args: Vec::new(),
                        }));
                    },
                    _ => {
                        continue_same!(State::Null { expecting_operator: true });
                    },
                }
            },
            State::GotIdentColonColon { ident } => {
                match st.token {
                    &token::Lt => {
                        push!(State::GotIdent { ident: ident });
                        continue_next!(State::DefinitelyType { angle_depth: 1, eager_exit: true, start_of_type: true });
                    },
                    _ => continue_same!(State::Null { expecting_operator: false }),
                }
            },
            State::GotFn => {
                match st.token {
                    &token::Ident(_) => {
                        continue_next!(State::GotFnName { name: tr.mark_last(), generic_start: None });
                    },
                    &token::Dollar => {
                        // hack for macro definitions - 'fn $foo(a: ...)'
                        continue_next!(State::GotFn);
                    },
                    // this probably shouldn't happen outside of a type
                    _ => continue_same!(State::Null { expecting_operator: true }),
                }
            },
            State::GotFnName { name, generic_start } => {
                match st.token {
                    &token::Lt => {
                        let state = State::GotFnName { name: name, generic_start: Some(tr.mark_last()) };
                        push!(state);
                        continue_next!(State::DefinitelyType { angle_depth: 1, eager_exit: true, start_of_type: true });
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        push!(State::Null { expecting_operator: false });
                        in_func_parens = true;
                        continue_next!(State::DeclArgStart {
                            decl: DeclStuff {
                                name: name,
                                args: Vec::new(),
                                generic_start: generic_start,
                                args_start: tr.mark_last(),
                                decl_end: None,
                            },
                            pending_default: false,
                        });
                    },
                    // this definitely shouldn't happen
                    _ => continue_same!(State::Null { expecting_operator: true }),
                }
            },
            State::CallArgStart(mut call) => {
                match st.token {
                    &token::Ident(ref ident) => {
                        let mark = tr.mark_last();
                        let st2 = st_or_return!();
                        if let &token::Colon = st2.token {
                            call.args.push(Some(*ident));
                            tr.delete_from_mark(mark, 2);
                            push!(State::CallArgStart(call));
                            continue_next!(State::Null { expecting_operator: false });
                        } else {
                            call.args.push(None);
                            push!(State::CallArgStart(call));
                            st = st2;
                            continue_same!(State::GotIdent { ident: tr.mark_last() });
                        }
                    },
                    &token::CloseDelim(_) => {
                        //println!("call closedelim: {:?}", call.args);
                        if call.args.iter().any(|arg| arg.is_some()) {
                            let name = tr.mutate_mark(call.name);
                            match *name {
                                TokenTree::Token(_, token::Ident(ref mut ident)) => {
                                    let new_name = mutate_name(ident, call.args.iter().map(|arg| arg.as_ref()));
                                    *ident = new_name;
                                },
                                _ => unreachable!(),
                            }
                        }
                        continue_next!(pop!());
                    },
                    &token::Comma => {
                        continue_next!(State::CallArgStart(call));
                    },
                    _ => {
                        call.args.push(None);
                        push!(State::CallArgStart(call));
                        continue_same!(State::Null { expecting_operator: false });
                    }
                }
            },
            State::DeclArgStart { mut decl, pending_default } => {
                if let Some(prev_arg) = decl.args.last_mut() {
                    prev_arg.ty_end = Some(tr.mark_last());
                }
                match st.token {
                    &token::Ident(ident) if ident.name == keywords::Ref.name() ||
                                            ident.name == keywords::Mut.name() ||
                                            ident.name == keywords::Box.name() => (),
                    &Token::Underscore | &token::Ident(_) => {
                        let to_delete = tr.mark_last();
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Ident(ident2) => {
                                tr.delete_from_mark(to_delete, 1);
                                match st.token {
                                    &token::Ident(ident1) => {
                                        decl.args.push(DeclArg { name: Some(ident1), ty_start: None, ty_end: None, is_default: pending_default });
                                    },
                                    &Token::Underscore => {
                                        decl.args.push(DeclArg { name: Some(ident2), ty_start: None, ty_end: None, is_default: pending_default });
                                        let st3 = st_or_return!();
                                        if let &Token::Colon = st3.token {} else {
                                            ctx.cx.span_err(*st3.span, "_ should be followed by an ident pattern");

                                        }
                                    },
                                    _ => unreachable!(),
                                };
                            },
                            _ => {
                                // this could just be a type
                                decl.args.push(DeclArg { name: None, ty_start: Some(tr.mark_last()), ty_end: None, is_default: pending_default });
                                st = st2;
                            },
                        }
                        push!(State::DeclArgStart { decl: decl, pending_default: pending_default });
                        continue_same!(State::Null { expecting_operator: false });
                    },
                    &token::CloseDelim(_) => {
                        //println!("decl closedelim: {:?}", decl);
                        if decl.args.iter().any(|arg| arg.name.is_some() || arg.is_default) {
                            let args_end = tr.mark_next();
                            let new_full_name: ast::Ident;
                            let old_name: ast::Ident;
                            {
                                let name = tr.mutate_mark(decl.name);
                                if let TokenTree::Token(_, token::Ident(ref mut ident)) = *name {
                                    old_name = *ident;
                                    new_full_name = mutate_name(ident, decl.args.iter().map(|arg| arg.name.as_ref()));
                                    *ident = new_full_name;
                                } else {
                                    panic!("?? {:?}", name);
                                }
                            }
                            // generate stubs for default arguments
                            if decl.args.iter().any(|arg| arg.is_default) {
                                push!(State::DeclEnd { decl: decl, etc: Box::new((
                                    old_name,
                                    new_full_name,
                                    args_end,
                                )) });
                                continue_next!(State::SeekingSemiOrOpenBrace);
                            }
                        }
                        continue_next!(pop!());
                    },
                    &token::Pound => { // #
                        let attr_start = tr.mark_last();
                        let st2 = st_or_return!();
                        if let &token::OpenDelim(DelimToken::Bracket) = st2.token { // #[
                            let st3 = st_or_return!();
                            if let &token::Ident(ident) = st3.token {
                                if ident.name == default_name { // #[default
                                    let st4 = st_or_return!();
                                    if let &token::CloseDelim(DelimToken::Bracket) = st2.token { // #[default]
                                        // this is only 2 TokenTrees
                                        tr.delete_from_mark(attr_start, 2);
                                        if pending_default {
                                            ctx.cx.span_err(*st2.span, "duplicate #[default]");
                                        }
                                        continue_next!(State::DeclArgStart { decl: decl, pending_default: true });

                                    } else { st = st4; }
                                } else { st = st3; }
                            } else { st = st3; }
                        } else { st = st2; }
                    },
                    &token::Comma => continue_next!(State::DeclArgStart { decl: decl, pending_default: false }),
                    _ => (),
                }
                decl.args.push(DeclArg { name: None, ty_start: Some(tr.mark_last()), ty_end: None, is_default: pending_default });
                push!(State::DeclArgStart { decl: decl, pending_default: false });
                continue_same!(State::Null { expecting_operator: false });
            },
            State::DeclEnd { decl, etc } => {
                // only get here if we need defaults
                match st.token {
                    &token::Semi | &Token::CloseDelim(DelimToken::Brace) => {
                        let decl_end: Mark = decl.decl_end.unwrap_or_else(|| tr.mark_last());
                        let default_count = decl.args.iter().filter(|arg| arg.is_default).count();
                        let (old_name, new_full_name, args_end) = *etc;
                        for num_include in 0..default_count {
                            gen_default_stub(tr, &decl.args, num_include, decl.generic_start, decl.args_start, args_end, decl_end, &old_name, &new_full_name);
                        }
                        continue_next!(pop!());
                    },
                    &Token::OpenDelim(DelimToken::Brace) => {
                        push!(State::DeclEnd { decl: decl, etc: etc });
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    _ => panic!("unexpected token at decl end"),
                }
            }
            State::DefinitelyType { mut angle_depth, eager_exit, start_of_type } => {
                let mut new_start_of_type = false;
                match st.token {
                    &token::Lt => {
                        angle_depth += 1;
                        new_start_of_type = true;
                    },
                    &token::Gt if angle_depth > 0 => {
                        angle_depth -= 1;
                        if angle_depth == 0 && eager_exit {
                            if delim_depth != 0 {
                                ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                            }
                            continue_next!(pop!());
                        }
                    },
                    &token::BinOp(token::Shr) if angle_depth > 0 => {
                        if angle_depth == 1 {
                            // yuck
                            unsafe {
                                let ptr = tr.token_storage.get();
                                *ptr = token::Gt;
                                st.token = &*ptr;
                            }
                            continue_same!(pop!());
                        } else {
                            angle_depth -= 2;
                        }
                        if angle_depth == 0 && eager_exit {
                            if delim_depth != 0 {
                                ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                            }
                            continue_next!(pop!());
                        }
                    },
                    &token::OpenDelim(DelimToken::Bracket) | &token::OpenDelim(DelimToken::Paren) => {
                        delim_depth += 1;
                        new_start_of_type = true;
                    },
                    &token::CloseDelim(_) => {
                        if delim_depth != 0 {
                            delim_depth -= 1;
                        } else {
                            if angle_depth != 0 {
                                // similar
                                ctx.cx.span_err(*st.span, "unexpected close delimiter in type. possible parser bug");
                            }
                            continue_same!(pop!());
                        }
                    },
                    // tokens allowed in a type
                    &token::Ident(ident) => {
                        if ident.name == keywords::Extern.name() {
                            // allow extern "C"
                            let st2 = st_or_return!();
                            match st2.token {
                                &token::Literal(..) => (),
                                _ => {
                                    st = st2;
                                    continue_same!(State::DefinitelyType { angle_depth: angle_depth, eager_exit: eager_exit, start_of_type: false });
                                },
                            }
                        }
                    },
                    &token::ModSep | &token::Underscore | &token::Not | 
                    &token::Question | &token::Dollar => (),
                    &token::Lifetime(_) | &token::RArrow => { new_start_of_type = true; },
                    &token::Pound => {
                        let st2 = st_or_return!();
                        if let &token::OpenDelim(DelimToken::Bracket) = st2.token {
                            // attribute
                            delim_depth += 1;
                            push!(State::DefinitelyType { angle_depth: angle_depth, eager_exit: eager_exit, start_of_type: true });
                            continue_next!(State::Null { expecting_operator: false });
                        } else {
                            ctx.cx.span_err(*st.span, "this shouldn't be in a type. possible parser bug");
                            st = st2;
                            continue;
                        }
                    },
                    // tokens allowed inside other things
                    &token::Semi if delim_depth != 0 => {
                        // inside an array decl, puts us into expression context
                        push!(State::DefinitelyType { angle_depth: angle_depth, eager_exit: eager_exit, start_of_type: false });
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::Comma | &token::Colon | &token::Eq | &token::BinOp(token::Plus) if
                        delim_depth != 0 || angle_depth != 0 => {
                        new_start_of_type = true;
                    },
                    // tokens allowed at start
                    &token::BinOp(token::And) | &token::BinOp(token::Star) if start_of_type => (),
                    // anything else ends the type
                    _ => {
                        if angle_depth == 0 && delim_depth == 0 {
                            continue_same!(pop!());
                        } else {
                            match st.token {
                                &token::BinOp(token::Star) | &token::BinOp(token::Plus) => (),
                                _ => {
                                    ctx.cx.span_err(*st.span, "this shouldn't be in a type. possible parser bug");
                                    continue_same!(State::Null { expecting_operator: false });
                                },
                            }
                        }
                    },
                }
                continue_next!(State::DefinitelyType { angle_depth: angle_depth, eager_exit: eager_exit, start_of_type: new_start_of_type });
            },
            State::SeekingSemiOrOpenBrace => {
                match st.token {
                    &token::Semi | &token::OpenDelim(DelimToken::Brace) => {
                        if delim_depth == 0 {
                            continue_same!(pop!());
                        }
                    },
                    &token::OpenDelim(_) => {
                        delim_depth += 1;
                        push!(State::SeekingSemiOrOpenBrace);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::CloseDelim(_) => {
                        if delim_depth == 1 {
                            delim_depth -= 1;
                        } else {
                            ctx.cx.span_err(*st.span, "unexpected close delim. possible parser bug");
                            continue_next!(pop!());
                        }
                    },
                    _ => ()
                }
                continue_next!(State::SeekingSemiOrOpenBrace);
            },
            state @ State::LambdaEnd => {
                // these are used as tokens
                match st.token {
                    &token::Comma => {
                        push!(state);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    _ => {
                        continue_same!(State::Null { expecting_operator: true });
                    },
                }
            },
            State::ExcessCloses => {
                ctx.cx.span_err(*st.span, "excess close delimeters. possible parser bug");
                push!(State::ExcessCloses);
                continue_next!(State::Null { expecting_operator: false });
            },
            State::Dummy => unreachable!(),
        }
        st = st_or_return!();
    }
}

fn expand_namedarg<'a, 'b>(cx: &'a mut ExtCtxt, _sp: Span, args: &'b [TokenTree])
        -> Box<MacResult + 'static> {
    let token_storage = UnsafeCell::new(token::DotDot);
    let mut tr = TTReader::new(args, &token_storage);
    {
        let mut ctx = Context { cx: cx };
        do_transform(&mut tr, &mut ctx);
    }
    let output = tr.output_as_slice();
    //println!("==> {}", pprust::tts_to_string(output));
    passthrough_items(cx, output)
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("namedarg", expand_namedarg);
}

