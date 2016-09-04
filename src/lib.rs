#![feature(plugin_registrar, rustc_private, slice_patterns, relaxed_adts)]

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
    Null { expecting_operator: bool, in_func_parens: bool }, // pops on , or closing delim, not skipping
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
    ControlBlock,
    StructLiteral,
    ExcessCloses,
    Dummy,
}
struct StackEntry {
    state: State,
    delim_depth: usize,
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
struct Stuff<'x, 'y: 'x, 'a: 'x, EC: ExtCtxtish + 'y> {
    ctx: &'x mut Context<'y, EC>,
    state: State,
    delim_depth: usize,
    tr: &'x mut TTReader<'a>,
    stack: Vec<StackEntry>,
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
    let mut s = Stuff {
        state: State::Null { expecting_operator: false },
        delim_depth: 0,
        tr: tr,
        stack: vec![StackEntry { state: State::ExcessCloses, delim_depth: 0 }],
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
    macro_rules! continue_next_pop { () => { continue_next!(pop(&mut s)) } }
    macro_rules! continue_same_pop { () => { continue_same!(pop(&mut s)) } }
    #[inline(always)]
    fn push<'x, 'y, 'a, EC: ExtCtxtish + 'y>(s: &mut Stuff<'x, 'y, 'a, EC>, state: State) {
        s.stack.push(StackEntry { state: state, delim_depth: s.delim_depth });
        s.delim_depth = 0;
    }
    #[inline(always)]
    fn pop<'x, 'y, 'a, EC: ExtCtxtish + 'y>(s: &mut Stuff<'x, 'y, 'a, EC>) -> State {
        if let Some(entry) = s.stack.pop() {
            s.delim_depth = entry.delim_depth;
            entry.state
        } else {
            panic!("popped out of last state");
        }
    }
    st = st_or_return!();
    loop {
        #[cfg(feature = "println_spam")]
        fn println_spam<'x, 'y, 'a, EC: ExtCtxtish + 'y>(s: &mut Stuff<'x, 'y, 'a, EC>, st: &SpanToken<'x>) {
            println!("stack={} state={:?}", s.stack.len(), s.state);
            println!("parserdepth={} delim_depth={} tok={:?}", s.tr.stack.len(), s.delim_depth, st.token);
            s.ctx.cx.span_warn(*st.span, "hi");
        }
        #[cfg(not(feature = "println_spam"))]
        fn println_spam<T, U>(_: T, _: U) {}
        println_spam(&mut s, &st);
        match replace(&mut s.state, State::Dummy) {
            State::Null { mut expecting_operator } => {
                match st.token {
                    &token::Ident(ref ident) => {
                        // XXX trait, attr
                        let name = ident.name;
                        if name.0 >= keywords::Default.name().0 ||
                           name == keywords::SelfValue.name() ||
                           name == keywords::SelfType.name() ||
                           name == keywords::Super.name() {
                            continue_next!(State::GotIdent { ident: s.tr.mark_last() });
                        }
                        // this is a keyword
                        if name == keywords::Fn.name() {
                            continue_next!(State::GotFn);
                        }
                        if name == keywords::As.name() ||
                           name == keywords::Impl.name() ||
                           name == keywords::Trait.name() ||
                           name == keywords::Struct.name() ||
                           name == keywords::Type.name() ||
                           name == keywords::Where.name() {
                            // this isn't perfect - some types will still be treated as expressions,
                            // e.g. in 'type A = B', B
                            // but this is ok as long as they're not in calls
                            push(&mut s, State::Null { expecting_operator: true });
                            continue_next!(State::DefinitelyType { angle_depth: 0, eager_exit: false, start_of_type: true });
                        }
                        if name == keywords::If.name() ||
                           name == keywords::For.name() ||
                           name == keywords::Loop.name() ||
                           name == keywords::Match.name() ||
                           name == keywords::While.name() {
                           push(&mut s, State::ControlBlock);
                           continue_next!(State::Null { expecting_operator: false });
                        }
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::OpenDelim(DelimToken::Brace) if s.delim_depth == 0 => {
                        while let Some(&StackEntry { state: State::ControlBlock, .. }) = s.stack.last() {
                            pop(&mut s); // stay in this state
                        }
                        s.delim_depth += 1;
                        expecting_operator = false;
                    },
                    &token::OpenDelim(_) => {
                        s.delim_depth += 1;
                        expecting_operator = false;
                    },
                    &token::CloseDelim(_) => {
                        if s.delim_depth == 0 {
                            continue_same_pop!();
                        } else {
                            s.delim_depth -= 1;
                        }
                        expecting_operator = true;
                    },
                    &token::Comma => {
                        if s.delim_depth == 0 {
                            continue_same_pop!();
                        }
                        expecting_operator = false;
                    },
                    &token::Colon => {
                        if s.delim_depth == 0  {
                            match s.stack.last_mut() {
                                Some(&mut StackEntry { state: State::DeclArgStart { ref mut decl, .. }, .. }) => {
                                    decl.args.last_mut().unwrap().ty_start = Some(s.tr.mark_last());
                                },
                                _ => ()
                            }
                        }
                        push(&mut s, State::Null { expecting_operator: true });
                        continue_next!(State::DefinitelyType { angle_depth: 0, eager_exit: false, start_of_type: true });
                    },
                    &token::RArrow => {
                        push(&mut s, State::Null { expecting_operator: true });
                        continue_next!(State::DefinitelyType { angle_depth: 0, eager_exit: false, start_of_type: true });
                    },
                    &token::Lt => {
                        // I had a clever scheme to not have to know whether we were expecting an
                        // operator, but lambdas ruined it.
                        if expecting_operator {
                            expecting_operator = false;
                        } else {
                            push(&mut s, State::Null { expecting_operator: true });
                            continue_next!(State::DefinitelyType { angle_depth: 1, eager_exit: true, start_of_type: true });
                        }
                    },
                    &token::BinOp(token::Or) if !expecting_operator => {
                        if s.delim_depth == 0 {
                            if let Some(&StackEntry { state: State::LambdaEnd, .. }) = s.stack.last() {
                                // end of lambda
                                continue_next_pop!();
                            }
                        }
                        // start of lambda
                        push(&mut s, State::LambdaEnd);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::Eq | &token::Le | &token::EqEq | &token::Ne | &token::Ge |
                    &token::Gt | &token::AndAnd | &token::OrOr | &token::Not | &token::Tilde |
                    &token::BinOp(_) | &token::BinOpEq(_) | &token::At | &token::Dot | &token::DotDot |
                    &token::DotDotDot | &token::Semi | &token::ModSep |
                    &token::LArrow | &token::FatArrow | &token::Dollar => {
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
                        push(&mut s, State::Null { expecting_operator: true });
                        continue_next!(State::CallArgStart(CallStuff {
                            name: ident,
                            args: Vec::new(),
                        }));
                    },
                    &token::OpenDelim(DelimToken::Brace) => {
                        if s.delim_depth == 0 {
                            if let Some(&StackEntry { state: State::ControlBlock, .. }) = s.stack.last() {
                                continue_same!(State::Null { expecting_operator: true });
                            }
                        }
                        // otherwise this could be a struct literal
                        s.delim_depth += 1;
                        continue_next!(State::StructLiteral);
                    },
                    _ => {
                        continue_same!(State::Null { expecting_operator: true });
                    },
                }
            },
            State::GotIdentColonColon { ident } => {
                match st.token {
                    &token::Lt => {
                        push(&mut s, State::GotIdent { ident: ident });
                        continue_next!(State::DefinitelyType { angle_depth: 1, eager_exit: true, start_of_type: true });
                    },
                    _ => continue_same!(State::Null { expecting_operator: false }),
                }
            },
            State::GotFn => {
                match st.token {
                    &token::Ident(_) => {
                        continue_next!(State::GotFnName { name: s.tr.mark_last(), generic_start: None });
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
                        let state = State::GotFnName { name: name, generic_start: Some(s.tr.mark_last()) };
                        push(&mut s, state);
                        continue_next!(State::DefinitelyType { angle_depth: 1, eager_exit: true, start_of_type: true });
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        push(&mut s, State::Null { expecting_operator: false });
                        continue_next!(State::DeclArgStart {
                            decl: DeclStuff {
                                name: name,
                                args: Vec::new(),
                                generic_start: generic_start,
                                args_start: s.tr.mark_last(),
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
                        let mark = s.tr.mark_last();
                        let st2 = st_or_return!();
                        if let &token::Colon = st2.token {
                            call.args.push(Some(*ident));
                            s.tr.delete_from_mark(mark, 2);
                            push(&mut s, State::CallArgStart(call));
                            continue_next!(State::Null { expecting_operator: false });
                        } else {
                            call.args.push(None);
                            push(&mut s, State::CallArgStart(call));
                            st = st2;
                            continue_same!(State::GotIdent { ident: s.tr.mark_last() });
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
                        continue_next_pop!();
                    },
                    &token::Comma => {
                        continue_next!(State::CallArgStart(call));
                    },
                    _ => {
                        call.args.push(None);
                        push(&mut s, State::CallArgStart(call));
                        continue_same!(State::Null { expecting_operator: false });
                    }
                }
            },
            State::DeclArgStart { mut decl, pending_default } => {
                if let Some(prev_arg) = decl.args.last_mut() {
                    prev_arg.ty_end = Some(s.tr.mark_last());
                }
                match st.token {
                    &token::Ident(ident) if ident.name == keywords::Ref.name() ||
                                            ident.name == keywords::Mut.name() => (),
                    &Token::Underscore | &token::Ident(_) => {
                        let to_delete = s.tr.mark_last();
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Ident(ident2) => {
                                s.tr.delete_from_mark(to_delete, 1);
                                match st.token {
                                    &token::Ident(ident1) => {
                                        decl.args.push(DeclArg { name: Some(ident1), ty_start: None, ty_end: None, is_default: pending_default });
                                    },
                                    &Token::Underscore => {
                                        decl.args.push(DeclArg { name: Some(ident2), ty_start: None, ty_end: None, is_default: pending_default });
                                        let st3 = st_or_return!();
                                        if let &Token::Colon = st3.token {} else {
                                            s.ctx.cx.span_err(*st3.span, "_ should be followed by an ident pattern");

                                        }
                                    },
                                    _ => unreachable!(),
                                };
                            },
                            _ => {
                                // this could just be a type
                                decl.args.push(DeclArg { name: None, ty_start: Some(s.tr.mark_last()), ty_end: None, is_default: pending_default })
                            },
                        }
                        push(&mut s, State::DeclArgStart { decl: decl, pending_default: pending_default });
                        continue_same!(State::Null { expecting_operator: false });
                    },
                    &token::CloseDelim(_) => {
                        //println!("decl closedelim: {:?}", decl);
                        if decl.args.iter().any(|arg| arg.name.is_some() || arg.is_default) {
                            let args_end = s.tr.mark_next();
                            let new_full_name: ast::Ident;
                            let old_name: ast::Ident;
                            {
                                let name = s.tr.mutate_mark(decl.name);
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
                                push(&mut s, State::DeclEnd { decl: decl, etc: Box::new((
                                    old_name,
                                    new_full_name,
                                    args_end,
                                )) });
                                continue_next!(State::SeekingSemiOrOpenBrace);
                            }
                        }
                        continue_next_pop!();
                    },
                    &token::Pound => { // #
                        let attr_start = s.tr.mark_last();
                        let st2 = st_or_return!();
                        if let &token::OpenDelim(DelimToken::Bracket) = st2.token { // #[
                            let st3 = st_or_return!();
                            if let &token::Ident(ident) = st3.token {
                                if ident.name == default_name { // #[default
                                    let st4 = st_or_return!();
                                    if let &token::CloseDelim(DelimToken::Bracket) = st2.token { // #[default]
                                        // this is only 2 TokenTrees
                                        s.tr.delete_from_mark(attr_start, 2);
                                        if pending_default {
                                            s.ctx.cx.span_err(*st2.span, "duplicate #[default]");
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
                decl.args.push(DeclArg { name: None, ty_start: Some(s.tr.mark_last()), ty_end: None, is_default: pending_default });
                push(&mut s, State::DeclArgStart { decl: decl, pending_default: false });
                continue_same!(State::Null { expecting_operator: false });
            },
            State::DeclEnd { decl, etc } => {
                // only get here if we need defaults
                match st.token {
                    &token::Semi | &Token::CloseDelim(DelimToken::Brace) => {
                        let decl_end: Mark = decl.decl_end.unwrap_or_else(|| s.tr.mark_last());
                        let default_count = decl.args.iter().filter(|arg| arg.is_default).count();
                        let (old_name, new_full_name, args_end) = *etc;
                        for num_include in 0..default_count {
                            gen_default_stub(s.tr, &decl.args, num_include, decl.generic_start, decl.args_start, args_end, decl_end, &old_name, &new_full_name);
                        }
                        continue_next_pop!();
                    },
                    &Token::OpenDelim(DelimToken::Brace) => {
                        push(&mut s, State::DeclEnd { decl: decl, etc: etc });
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
                            if s.delim_depth != 0 {
                                s.ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                            }
                            continue_next_pop!();
                        }
                    },
                    &token::BinOp(token::Shr) if angle_depth > 0 => {
                        if angle_depth == 1 {
                            // yuck
                            unsafe {
                                let ptr = s.tr.token_storage.get();
                                *ptr = token::Gt;
                                st.token = &*ptr;
                            }
                            continue_same_pop!();
                        } else {
                            angle_depth -= 2;
                        }
                        if angle_depth == 0 && eager_exit {
                            if s.delim_depth != 0 {
                                s.ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                            }
                            continue_next_pop!();
                        }
                    },
                    &token::OpenDelim(DelimToken::Bracket) | &token::OpenDelim(DelimToken::Paren) => {
                        s.delim_depth += 1;
                        new_start_of_type = true;
                    },
                    &token::CloseDelim(_) => {
                        if s.delim_depth != 0 {
                            s.delim_depth -= 1;
                        } else {
                            if angle_depth != 0 {
                                // similar
                                s.ctx.cx.span_err(*st.span, "unexpected close delimiter in type. possible parser bug");
                            }
                            continue_same_pop!();
                        }
                    },
                    // tokens allowed in a type
                    &token::Ident(_) | &token::ModSep | &token::Underscore |
                    &token::Question | &token::Dollar => (),
                    &token::Lifetime(_) | &token::RArrow => { new_start_of_type = true; },
                    &token::Pound => {
                        let st2 = st_or_return!();
                        if let &token::OpenDelim(DelimToken::Bracket) = st2.token {
                            // attribute
                            s.delim_depth += 1;
                            push(&mut s, State::DefinitelyType { angle_depth: angle_depth, eager_exit: eager_exit, start_of_type: true });
                            continue_next!(State::Null { expecting_operator: false });
                        } else {
                            s.ctx.cx.span_err(*st.span, "this shouldn't be in a type. possible parser bug");
                            st = st2;
                            continue;
                        }
                    },
                    // tokens allowed inside other things
                    &token::Semi if s.delim_depth != 0 => {
                        // inside an array decl, puts us into expression context
                        push(&mut s, State::DefinitelyType { angle_depth: angle_depth, eager_exit: eager_exit, start_of_type: false });
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::Comma | &token::Colon | &token::Eq | &token::BinOp(token::Plus) if
                        s.delim_depth != 0 || angle_depth != 0 => {
                        new_start_of_type = true;
                    },
                    // tokens allowed at start
                    &token::BinOp(token::And) | &token::BinOp(token::Star) if start_of_type => (),
                    // anything else ends the type
                    _ => {
                        if angle_depth == 0 && s.delim_depth == 0 {
                            continue_same_pop!();
                        } else {
                            match st.token {
                                &token::BinOp(token::Star) | &token::BinOp(token::Plus) => (),
                                _ => {
                                    s.ctx.cx.span_err(*st.span, "this shouldn't be in a type. possible parser bug");
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
                        if s.delim_depth == 0 {
                            continue_same_pop!();
                        }
                    },
                    &token::OpenDelim(_) => {
                        s.delim_depth += 1;
                        push(&mut s, State::SeekingSemiOrOpenBrace);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::CloseDelim(_) => {
                        if s.delim_depth == 1 {
                            s.delim_depth -= 1;
                        } else {
                            s.ctx.cx.span_err(*st.span, "unexpected close delim. possible parser bug");
                            continue_next_pop!();
                        }
                    },
                    _ => ()
                }
                continue_next!(State::SeekingSemiOrOpenBrace);
            },
            state @ State::LambdaEnd | state @ State::ControlBlock => {
                // these are used as tokens
                match st.token {
                    &token::Comma => {
                        push(&mut s, state);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    _ => {
                        continue_same!(State::Null { expecting_operator: true });
                    },
                }
            },
            State::StructLiteral => {
                match st.token {
                    &token::Comma => {
                        continue_next!(State::StructLiteral);
                    },
                    &token::Ident(_) => {
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Colon => {
                                push(&mut s, State::StructLiteral);
                                continue_next!(State::Null { expecting_operator: false });
                            },
                            _ => {
                                // this could be a pattern, where 'a: a' can be elided to a
                                st = st2;
                                continue_same!(State::StructLiteral);
                            },
                        }
                    },
                    &token::DotDot => {
                        push(&mut s, State::StructLiteral);
                        continue_next!(State::Null { expecting_operator: false });
                    },
                    &token::CloseDelim(DelimToken::Brace) => {
                        continue_next!(State::Null { expecting_operator: true });
                    },
                    _ => {
                        // I don't know what this is.
                        // But this might not really be a struct literal.  It could be something like
                        // 'macro_rules! X { }' or 'mod X { }'.
                        // We could try to detect these... <work>
                        continue_same!(State::Null { expecting_operator: false });
                    },
                }
                unreachable!();
            },
            State::ExcessCloses => {
                s.ctx.cx.span_err(*st.span, "excess close delimeters. possible parser bug");
                push(&mut s, State::ExcessCloses);
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

