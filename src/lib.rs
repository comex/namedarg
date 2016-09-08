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
pub use syntax::ast::Ident;
use syntax::util::small_vector::SmallVector;
#[allow(unused_imports)]
use syntax::print::pprust;
use rustc_plugin::registry::Registry;
use syntax::parse::token;
use syntax::parse::token::{Token, DelimToken};
use syntax::parse::token::keywords;
use std::rc::Rc;
use std::mem::{replace, size_of, transmute};
use std::cell::UnsafeCell;
use std::slice;

#[cfg(not(feature = "println_spam"))]
macro_rules! if_println_spam { {$($stuff:tt)*} => {} }
#[cfg(feature = "println_spam")]
macro_rules! if_println_spam { {$($stuff:tt)*} => {$($stuff)*} }

#[cfg(not(debug_assertions))]
macro_rules! if_debug_assertions { {$($stuff:tt)*} => {} }
#[cfg(debug_assertions)]
macro_rules! if_debug_assertions { {$($stuff:tt)*} => {$($stuff)*} }

#[cfg(not(use_rparse))]
macro_rules! if_rparse { {$($stuff:tt)*} => {} }
#[cfg(use_rparse)]
macro_rules! if_rparse { {$($stuff:tt)*} => {$($stuff)*} }

#[cfg(not(use_rparse))]
macro_rules! if_not_rparse { {$($stuff:tt)*} => {$($stuff)*} }
#[cfg(use_rparse)]
macro_rules! if_not_rparse { {$($stuff:tt)*} => {} }

if_not_rparse! {
    macro_rules! other_unexpected { () => {
        &token::Interpolated(..) | &token::MatchNt(..) | &token::SubstNt(..) |
        &token::SpecialVarNt(..)
    } }
    macro_rules! other_expected { () => {
        &token::Eq | &token::Le | &token::EqEq | &token::Ne | &token::Ge |
        &token::Gt | &token::AndAnd | &token::OrOr | &token::Not | &token::Tilde |
        &token::BinOp(_) | &token::BinOpEq(_) | &token::At | &token::Dot | &token::DotDot |
        &token::DotDotDot | &token::ModSep | &token::Dollar |
        &token::LArrow | &token::RArrow | &token::FatArrow
    } }
    macro_rules! other_ignore { () => {
        &token::DocComment(..) | &token::Whitespace | &token::Comment | &token::Shebang(..) |
        &token::Eof
    } }
}
if_rparse! {
    macro_rules! other_unexpected { () => {
        &token::Dummy
    } }
    macro_rules! other_expected { () => {
        &token::Gt | &token::Not |
        &token::BinOp(_) | &token::BinOpEq(_) | &token::DotDot |
        &token::DotDotDot | &token::ModSep | &token::Dollar |
        &token::Other
    } }
    macro_rules! other_ignore { () => {
        &token::Eof
    } }
}

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

fn mutate_name<'a, I>(fn_name_ident: &ast::Ident, arg_names: I, ctx: &Context) -> ast::Ident
    where I: Iterator<Item=Option<&'a ast::Ident>> {
    let mut name: String = (*fn_name_ident.name.as_str()).to_owned();
    if ctx.use_valid_idents {
        name.push_str("__lbl");
    } else {
        name.push('{');
    }
    let mut any = false;
    for arg_name in arg_names {
        if ctx.use_valid_idents {
            if let Some(arg_name) = arg_name {
                name.push_str("__");
                name.push_str(&*arg_name.name.as_str());
            } else {
                name.push_str("_X");
            }
        } else {
            if let Some(arg_name) = arg_name {
                name.push_str(&*arg_name.name.as_str());
            }
            name.push(':');
        }
        any = true;
    }
    if !any {
        // if all arguments defaulted, use an untransformed ident
        return *fn_name_ident;
    }
    if !ctx.use_valid_idents {
        name.push('}');
    }
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
    fn copy_from_mark_range(&mut self, start: Mark, end: Mark, gm: GetMode) {
        //println!("CFMR: enter={:?} start={:?} end={:?}", enter, start, end);
        //println!("CFMR: have {:?}",self.output);
        let to_add: Vec<TokenTree> = self.replacing_output(|tr| tr.get(start, end, gm).to_owned());
        let token_storage = UnsafeCell::new(token::DotDot);
        let mut tr2 = TTReader::new(&to_add, &token_storage);
        while let Some(st) = tr2.next() {
            self.write(st.token.clone());
        }
        //println!("CFMR out");
    }
    fn replacing_output<O, F: FnOnce(&mut TTReader<'a>) -> O>(&mut self, f: F) -> O {
        let ptr = if self.output_stack.is_empty() { &mut self.output } else { self.output_stack.first_mut().unwrap() };
        self.tr.output = Some(replace(ptr, Vec::new()));
        let o = f(self.tr);
        *ptr = replace(&mut self.tr.output, None).unwrap();
        o
    }
    fn last_normal_token(&mut self) -> Option<&mut Token> {
        match self.output.last_mut() {
            Some(&mut TokenTree::Token(_, ref mut token)) => Some(token),
            _ => None,
        }
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
enum GetMode {
    InnerDepth(Mark),
    OuterDepth,
    SameDepth,
}

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
    fn mark_next(&self) -> Mark {
        self.check_offset();
        Mark {
            cur_offset: self.cur_offset,
            cur_stack_depth: make_cur_stack_depth(self.stack.len()),
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

pub struct Context<'x> {
    pub cx: &'x ExtCtxtish,
    pub use_valid_idents: bool,
}


macro_rules! derive_variant_data_inner {
    ($e:ident ; $enum_name:ident ; $variant_name:ident , $($rest:tt)* ) => {
        #[derive(Clone, Copy)]
        #[cfg_attr(feature = "derive_debug", derive(Debug))]
        pub struct $variant_name;
        impl $variant_name {
            #[inline(always)]
            #[allow(dead_code)]
            pub fn variant(&self) -> StateVariant { StateVariant::$variant_name }
            #[inline(always)]
            #[allow(dead_code)]
            pub fn to_state(&self) -> State {
                State::$variant_name
            }
        }
        derive_variant_data_inner!($e ; $enum_name ; $($rest)*);
    };
    ($e:ident ; $enum_name:ident ; $variant_name:ident { $($name:ident : $ty:ty),* } , $($rest:tt)* ) => {
        #[derive(Clone, Copy)]
        #[cfg_attr(feature = "derive_debug", derive(Debug))]
        pub struct $variant_name { $(pub $name: $ty),* }
        impl $variant_name {
            #[inline(always)]
            #[allow(dead_code)]
            pub fn variant(&self) -> StateVariant { StateVariant::$variant_name }
            #[inline(always)]
            #[allow(dead_code)]
            pub fn to_state(&self) -> State {
                State::$variant_name { $($name: self.$name),* }
            }
        }
        derive_variant_data_inner!($e ; $enum_name ; $($rest)*);
    };
    ($e:ident ; $enum_name:ident ;) => ()
}
macro_rules! derive_variant_data {
    { pub enum $enum_name:ident { $($args:tt)* } } => {
        #[allow(non_snake_case)]
        mod VariantData {
            use super::{State, Mark, StateVariant, XAndCommon, DeclStuff, DeclArg, Ident, VariantData, StackPtr, DTMode};
            derive_variant_data_inner!(e ; $enum_name ; $($args)* );
        }
        #[cfg_attr(feature = "derive_debug", derive(Debug))]
        #[derive(Clone, Copy)]
        pub enum $enum_name { $($args)* }
    }
}

derive_variant_data! {
pub enum State {
    Dummy,
    Null { expecting_operator: bool, after_semi_or_brace: bool },
    GotIdent { ident: Mark },
    GotIdentColonColon { ident: Mark },
    GotFn,
    GotFnName { name: Mark, generic_start: Option<Mark> },
    CallArgStart,
    DeclArgStart { pending_default: bool },
    DeclEnd { first_arg: StackPtr, decl: &'static DeclStuff, davs: &'static [XAndCommon<DeclArg>], old_name: Ident, new_full_name: Ident, args_end: Mark, decl_end: Option<Mark> },
    SeekingSemiOrOpenBrace,
    DefinitelyType { angle_depth: u32, mode: DTMode, start_of_type: bool }, // pops on closing >, skipping it
    LambdaEnd,
    TraitDeclOpen { trait_start: Mark, trait_end: Option<Mark> },
    ImplDeclAfterGeneric,
    ImplDeclAfterTrait { trait_start: Mark, trait_end: Option<Mark> },
    InTraitOrImpl { trait_start_end: Option<(Mark, Mark)>, prev: Option<&'static VariantData::InTraitOrImpl> },
    ExcessCloses,
    Pop,
}
}
#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub enum DTMode {
    EagerAngle, InExpr, InItem
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum StateVariant {
    Dummy,
    Null,
    GotIdent,
    GotIdentColonColon,
    GotFn,
    GotFnName,
    CallStart, // name: Mark
    CallArg, // Option<Ident>
    CallArgStart,
    DeclStart, // DeclStuff
    DeclArg, // DeclArg
    DeclArgStart,
    DeclEnd,
    SeekingSemiOrOpenBrace,
    DefinitelyType,
    LambdaEnd,
    TraitDeclOpen,
    ImplDeclAfterGeneric,
    ImplDeclAfterTrait,
    InTraitOrImpl,
    ExcessCloses,
    // dummy
    Pop,
}


struct Stack {
    v: Vec<u64>,
    top: *mut u64
}
impl Stack {
    fn new() -> Self {
        // allocate some huge buffer
        let mut v: Vec<u64> = Vec::with_capacity(500000);
        let top = unsafe { v.as_mut_ptr().offset(v.capacity() as isize) };
        Stack { v: v, top: top }
    }
    fn top(&self) -> StackPtr { StackPtr(self.top) }
    fn bottom(&mut self) -> StackPtr { StackPtr(self.v.as_mut_ptr()) }
    #[allow(dead_code)]
    fn len(&mut self, sp: StackPtr) -> usize {
        ((sp.0 as usize) - (self.bottom().0 as usize)) / size_of::<u64>()
    }
}
#[derive(Clone, Copy)]
#[cfg_attr(feature = "derive_debug", derive(Debug))]
pub struct StackPtr(*mut u64);
impl StackPtr {
    #[inline(always)]
    fn ptr(self) -> *mut u64 { self.0 }
    #[inline(always)]
    fn pop_ref<T: Copy>(&mut self) -> &'static mut T {
        unsafe {
            let count = (size_of::<T>() + 7) / 8;
            if_println_spam! {
                println!("popping {} u64s", count);
            }
            self.0 = self.0.offset(-(count as isize));
            &mut *(self.0 as *mut T)
        }
    }
    #[inline(always)]
    fn last_ref<T: Copy>(self) -> &'static T {
        unsafe {
            let count = (size_of::<T>() + 7) / 8;
            let p = self.0.offset(-(count as isize));
            &*(p as *const T)
        }
    }
    #[inline(always)]
    fn last_ref_mut<T: Copy>(self) -> &'static mut T {
        unsafe {
            let count = (size_of::<T>() + 7) / 8;
            let p = self.0.offset(-(count as isize));
            &mut *(p as *mut T)
        }
    }
    #[inline(always)]
    fn push<T: Copy>(&mut self, top: StackPtr, t: T) -> &'static mut T {
        unsafe {
            if (top.0 as usize) - (self.0 as usize) < size_of::<T>() {
                panic!("stack overflow");
            }
            let count = (size_of::<T>() + 7) / 8;
            if_println_spam! {
                println!("pushing {} u64s", count);
            }
            let ptr = &mut *(self.0 as *mut T);
            *ptr = t;
            self.0 = self.0.offset(count as isize);
            ptr
        }
    }
}

#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[derive(Clone, Copy)]
pub struct DeclStuff {
    name: Mark,
    generic_start: Option<Mark>,
    args_start: Mark,
}
#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[derive(Clone, Copy)]
pub struct DeclArg {
    name: Option<Ident>,
    ty_start: Option<Mark>,
    ty_end: Option<Mark>,
    is_default: bool,
}

#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[derive(Clone, Copy)]
#[repr(C)]
pub struct Common {
    _align: [u64; 0],
    delim_depth: u32,
    in_func_parens: bool,
    variant: u8,
}

impl Common {
    #[inline(always)]
    pub fn variant(&self) -> StateVariant {
        unsafe { transmute(self.variant) }
    }
}

#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[derive(Clone, Copy)]
#[repr(C)]
pub struct XAndCommon<X: Copy> {
    x: X,
    c: Common,
}

impl<X: Copy> XAndCommon<X> {
    #[inline(always)]
    fn new(x: X, common: Common) -> Self {
        XAndCommon { x: x, c: common }
    }
}

fn gen_default_stub<'a>(tr: &mut TTReader<'a>, args: &[XAndCommon<DeclArg>], num_include: usize, generic_start: Option<Mark>, args_start: Mark, args_end: Mark, decl_end: Mark, old_name: &ast::Ident, new_full_name: &ast::Ident, cur_in_trait_or_impl: Option<&'static VariantData::InTraitOrImpl>, ctx: &Context) {
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
        args.iter().filter_map(move |&XAndCommon { x: ref arg, .. }| {
            if arg.is_default {
                i += 1;
                if i > num_include {
                    return None;
                }
            }
            Some(arg.name.as_ref())
        })
    }, ctx);
    tw.write(token::Ident(partial_name));
    if let Some(generic_start) = generic_start {
        tw.copy_from_mark_range(generic_start, args_start, GetMode::SameDepth);
    }
    tw.write(token::OpenDelim(DelimToken::Paren));
    let mut i = 0;
    let mut arg_names: Vec<Ident> = Vec::with_capacity(args.len());
    for &XAndCommon { x: ref arg, .. } in args {
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
        tw.write(token::Colon);
        if let (Some(start), Some(end)) = (arg.ty_start, arg.ty_end) {
            tw.copy_from_mark_range(start, end, GetMode::InnerDepth(args_start));
            if let Some(&mut token::Ident(ref mut ident)) = tw.last_normal_token() {
                if ident.name == keywords::SelfValue.name() {
                    *ident = keywords::SelfType.ident();
                }
            }
        } else {
            // huh?
            tw.write(token::Underscore);
        }
        tw.write(token::Comma);
    }
    tw.write(token::CloseDelim(DelimToken::Paren));
    tw.copy_from_mark_range(args_end, decl_end, GetMode::SameDepth);
    tw.write(token::OpenDelim(DelimToken::Brace));

    if let Some(&VariantData::InTraitOrImpl { trait_start_end, .. }) = cur_in_trait_or_impl {
        // Self:: or <Self as Foo>::
        // TODO self args
        if let Some((start, end)) = trait_start_end {
            tw.write(token::Lt);
            tw.write(token::Ident(keywords::SelfType.ident()));
            tw.write(token::Ident(keywords::As.ident()));
            tw.copy_from_mark_range(start, end, GetMode::OuterDepth);
            tw.write(token::Gt);
        } else {
            tw.write(token::Ident(keywords::SelfType.ident()));
        }
        tw.write(token::ModSep);
    }

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

pub fn do_transform<'x, 'a: 'x>(tr: &mut TTReader<'a>, ctx: &mut Context<'x>) {
    let default_name = token::intern("default");
    let mut state: State = State::Null { expecting_operator: false, after_semi_or_brace: true };
    let mut delim_depth: u32 = 0;
    let mut in_func_parens: bool = false;
    let mut stack: Stack = Stack::new();
    let mut sp: StackPtr = stack.bottom();
    let mut st: SpanToken<'a>;
    let mut cur_in_trait_or_impl: Option<&'static VariantData::InTraitOrImpl> = None;
    macro_rules! st_or_return {
        () => ( if let Some(st) = tr.next() { st } else { return } )
    }
    macro_rules! pushx_manual { ($variant:expr, $vdata:expr) => { {
        let _variant: StateVariant = $variant;
        if_println_spam! {
            println!("pushing variant {:?} dd={} ifp={}", _variant, delim_depth, in_func_parens);
        }
        let r = sp.push(stack.top(), XAndCommon::new($vdata, Common { _align: [], delim_depth: delim_depth, in_func_parens: in_func_parens, variant: _variant as u8 }));
        delim_depth = 0;
        in_func_parens = false;
        let _ = in_func_parens; // avoid warning
        r
    } } }
    macro_rules! pushx { ($vdata:expr) => { {
        let _vdata = $vdata;
        pushx_manual!(_vdata.variant(), _vdata)
    } } }
    macro_rules! popx { ($vdata_ty:ty) => { {
        let xac = sp.pop_ref::<XAndCommon<$vdata_ty>>();
        debug_assert_eq!(xac.c.variant, xac.c.variant() as u8);
        if_println_spam! {
            println!("popping {:?}", xac);
        }
        delim_depth = xac.c.delim_depth;
        in_func_parens = xac.c.in_func_parens;
        xac.x.to_state()
    } } }
    pushx!(VariantData::ExcessCloses);
    st = st_or_return!();
    'outer_loop: loop {
        macro_rules! continue_next {
            ($state:expr) => {{
                let new: State = $state;
                st = st_or_return!();
                state = new;
                continue 'outer_loop;
            }}
        }
        macro_rules! continue_same {
            ($state:expr) => {{
                let new: State = $state;
                state = new;
                continue 'outer_loop;
            }}
        }
        if_println_spam! {
            println!("stack={} state={:?}", stack.len(sp), state);
            println!("parserdepth={} delim_depth={} ifp={} tok={:?}", tr.stack.len(), delim_depth, in_func_parens, st.token);
            ctx.cx.span_warn(*st.span, "hi");
        }
        match replace(&mut state, State::Dummy) {
            State::Null { mut expecting_operator, mut after_semi_or_brace } => loop {
                let was_after_semi_or_brace = after_semi_or_brace;
                after_semi_or_brace = false;
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
                        if name == keywords::Trait.name() {
                            pushx!(VariantData::TraitDeclOpen { trait_start: tr.mark_next(), trait_end: None });
                            continue_next!(State::DefinitelyType { angle_depth: 0, mode: DTMode::InItem, start_of_type: true });
                        }
                        if name == keywords::Impl.name() && was_after_semi_or_brace {
                            st = st_or_return!();
                            if let &token::Lt = st.token {
                                pushx!(VariantData::ImplDeclAfterGeneric);
                                continue_next!(State::DefinitelyType { angle_depth: 1, mode: DTMode::EagerAngle, start_of_type: true });
                            } else {
                                pushx!(VariantData::ImplDeclAfterTrait { trait_start: tr.mark_last(), trait_end: None });
                                continue_same!(State::DefinitelyType { angle_depth: 0, mode: DTMode::InItem, start_of_type: true });
                            }
                        }
                        if name == keywords::As.name() && in_func_parens && delim_depth == 0 {
                            pushx!(VariantData::Null { expecting_operator: true, after_semi_or_brace: false });
                            continue_next!(State::DefinitelyType { angle_depth: 0, mode: DTMode::InExpr, start_of_type: true });
                        }
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                    },
                    &token::OpenDelim(delim) => {
                        delim_depth += 1;
                        expecting_operator = false;
                        after_semi_or_brace = delim == DelimToken::Brace;
                    },
                    &token::CloseDelim(delim) => {
                        if delim_depth == 0 {
                            continue_same!(State::Pop);
                        }
                        delim_depth -= 1;
                        expecting_operator = true;
                        after_semi_or_brace = delim == DelimToken::Brace;
                    },
                    &token::Comma => {
                        if delim_depth == 0 {
                            continue_same!(State::Pop);
                        }
                        expecting_operator = false;
                    },
                    &token::Colon => {
                        if delim_depth == 0 && in_func_parens {
                            let mut test = sp;
                            if test.pop_ref::<Common>().variant() == StateVariant::DeclArgStart {
                                test.pop_ref::<VariantData::DeclArgStart>();
                                let data = test.pop_ref::<XAndCommon<DeclArg>>();
                                data.x.ty_start = Some(tr.mark_next());
                            }
                            pushx!(VariantData::Null { expecting_operator: true, after_semi_or_brace: false });
                            continue_next!(State::DefinitelyType { angle_depth: 0, mode: DTMode::InExpr, start_of_type: true });
                        }
                    },
                    &token::Lt => {
                        // I had a clever scheme to not have to know whether we were expecting an
                        // operator, but lambdas ruined it.
                        if expecting_operator {
                            expecting_operator = false;
                        } else {
                            if delim_depth == 0 && in_func_parens {
                                pushx!(VariantData::Null { expecting_operator: true, after_semi_or_brace: false });
                                continue_next!(State::DefinitelyType { angle_depth: 1, mode: DTMode::EagerAngle, start_of_type: true });
                            }
                        }
                    },
                    &token::BinOp(token::Or) if !expecting_operator => {
                        if delim_depth == 0 {
                            if sp.last_ref::<Common>().variant() == StateVariant::LambdaEnd {
                                // end of lambda
                                continue_next!(popx!(VariantData::LambdaEnd));
                            }
                        }
                        // start of lambda
                        pushx!(VariantData::LambdaEnd);
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                    },
                    other_expected!() => {
                        expecting_operator = false;
                    },
                    &token::Semi => {
                        after_semi_or_brace = true;
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
                    other_ignore!() => {
                        // don't really affect the AST
                    },
                    other_unexpected!() => {
                        panic!("shouldn't get tokens like {:?}", st.token);
                    },
                }
                st = st_or_return!();
            },
            State::GotIdent { ident } => {
                match st.token {
                    &token::ModSep => {
                        continue_next!(State::GotIdentColonColon { ident: ident });
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        delim_depth += 1;
                        pushx_manual!(StateVariant::CallStart, ident);
                        continue_next!(State::CallArgStart);
                    },
                    _ => {
                        continue_same!(State::Null { expecting_operator: true, after_semi_or_brace: false });
                    },
                }
            },
            State::GotIdentColonColon { ident } => {
                match st.token {
                    &token::Lt => {
                        pushx!(VariantData::GotIdent { ident: ident });
                        continue_next!(State::DefinitelyType { angle_depth: 1, mode: DTMode::EagerAngle, start_of_type: true });
                    },
                    _ => continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false }),
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
                    _ => continue_same!(State::Null { expecting_operator: true, after_semi_or_brace: false }),
                }
            },
            State::GotFnName { name, generic_start } => {
                match st.token {
                    &token::Lt => {
                        pushx!(VariantData::GotFnName { name: name, generic_start: Some(tr.mark_last()) });
                        continue_next!(State::DefinitelyType { angle_depth: 1, mode: DTMode::EagerAngle, start_of_type: true });
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        //delim_depth += 1;
                        pushx_manual!(StateVariant::DeclStart, DeclStuff {
                            name: name,
                            generic_start: generic_start,
                            args_start: tr.mark_last(),
                        });
                        continue_next!(State::DeclArgStart { pending_default: false });
                    },
                    // this definitely shouldn't happen
                    _ => continue_same!(State::Null { expecting_operator: true, after_semi_or_brace: false }),
                }
            },
            State::CallArgStart => {
                match st.token {
                    &token::Ident(ref ident) => {
                        let mark = tr.mark_last();
                        let st2 = st_or_return!();
                        if let &token::Colon = st2.token {
                            tr.delete_from_mark(mark, 2);
                            pushx_manual!(StateVariant::CallArg, Some(*ident));
                            pushx!(VariantData::CallArgStart);
                            in_func_parens = true;
                            continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                        } else {
                            pushx_manual!(StateVariant::CallArg, None::<Ident>);
                            pushx!(VariantData::CallArgStart);
                            in_func_parens = true;
                            st = st2;
                            continue_same!(State::GotIdent { ident: tr.mark_last() });
                        }
                    },
                    &token::CloseDelim(_) => {
                        // end of call
                        let mut any = false;
                        let orig_name: Mark;
                        let mut first_arg = sp;
                        let mut num_args = 0;
                        loop {
                            let common = sp.pop_ref::<Common>();
                            delim_depth = common.delim_depth;
                            match common.variant() {
                                StateVariant::CallStart => {
                                    orig_name = *sp.pop_ref::<Mark>();
                                    break;
                                },
                                StateVariant::CallArg => {
                                    let ident = *sp.pop_ref::<Option<Ident>>();
                                    if ident.is_some() { any = true; }
                                    num_args += 1;
                                    first_arg = sp;
                                },
                                _ => {
                                    unreachable!();
                                },
                            }
                        }
                        if any {
                            let name = tr.mutate_mark(orig_name);
                            if let TokenTree::Token(_, token::Ident(ref mut ident)) = *name {
                                let cavs: &'static [XAndCommon<Option<Ident>>] = unsafe { slice::from_raw_parts(transmute(first_arg.ptr()), num_args) };
                                let arg_names = cavs.iter().map(|cav| cav.x.as_ref());
                                let new_name = mutate_name(ident, arg_names, ctx);
                                *ident = new_name;
                            } else { unreachable!() }
                        }
                        in_func_parens = true;
                        continue_same!(State::Null { expecting_operator: true, after_semi_or_brace: false });
                    },
                    &token::Comma => {
                        continue_next!(State::CallArgStart);
                    },
                    _ => {
                        pushx_manual!(StateVariant::CallArg, None::<Ident>);
                        pushx!(VariantData::CallArgStart);
                        in_func_parens = true;
                        continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                    }
                }
            },
            State::DeclArgStart { pending_default } => {
                in_func_parens = true;
                if sp.last_ref::<Common>().variant() == StateVariant::DeclArg {
                    let prev_arg = sp.last_ref_mut::<XAndCommon<DeclArg>>();
                    if prev_arg.x.ty_end.is_none() {
                        prev_arg.x.ty_end = Some(tr.mark_last());
                    }
                }
                match st.token {
                    &token::Ident(ident) if ident.name == keywords::Ref.name() ||
                                            ident.name == keywords::Mut.name() ||
                                            ident.name == keywords::Box.name() => (),
                    &Token::Underscore | &token::Ident(_) => {
                        // TODO other patterns
                        let to_delete = tr.mark_last();
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Ident(ident2) => {
                                tr.delete_from_mark(to_delete, 1);
                                match st.token {
                                    &token::Ident(ident1) => {
                                        pushx_manual!(StateVariant::DeclArg, DeclArg { name: Some(ident1), ty_start: None, ty_end: None, is_default: pending_default });
                                        pushx!(VariantData::DeclArgStart { pending_default: false });
                                    },
                                    &Token::Underscore => {
                                        pushx_manual!(StateVariant::DeclArg, DeclArg { name: Some(ident2), ty_start: None, ty_end: None, is_default: pending_default });
                                        pushx!(VariantData::DeclArgStart { pending_default: false });
                                        let st3 = st_or_return!();
                                        if let &Token::Colon = st3.token {} else {
                                            ctx.cx.span_err(*st3.span, "_ should be followed by an ident pattern");

                                        }
                                    },
                                    _ => unreachable!(),
                                };
                            },
                            &token::BinOp(token::And) | &token::BinOp(token::Star) | &token::Underscore => {
                                // reference/pointer/dontcare patterns
                                match st.token {
                                    &token::Ident(ident1) => {
                                        tr.delete_from_mark(to_delete, 1);
                                        pushx_manual!(StateVariant::DeclArg, DeclArg { name: Some(ident1), ty_start: None, ty_end: None, is_default: pending_default });
                                        pushx!(VariantData::DeclArgStart { pending_default: false });
                                    },
                                    &Token::Underscore => {
                                        ctx.cx.span_err(*st.span, "_ should be followed by an ident pattern");
                                    },
                                    _ => unreachable!(),
                                }
                            },
                            _ => {
                                // this could just be a type
                                pushx_manual!(StateVariant::DeclArg, DeclArg { name: None, ty_start: Some(to_delete), ty_end: None, is_default: pending_default });
                                pushx!(VariantData::DeclArgStart { pending_default: false });
                                st = st2;
                            },
                        }
                        in_func_parens = true;
                        continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                    },
                    &token::CloseDelim(_) => {
                        let args_end_mark = tr.mark_next();
                        let save = sp;
                        let mut first_arg = sp;
                        let mut any = false;
                        let mut any_default = false;
                        let mut num_args = 0;
                        let decl: &'static DeclStuff;
                        loop {
                            let common = sp.pop_ref::<Common>();
                            delim_depth = common.delim_depth;
                            in_func_parens = common.in_func_parens;
                            match common.variant() {
                                StateVariant::DeclStart => {
                                    decl = sp.pop_ref::<DeclStuff>();
                                    break;
                                },
                                StateVariant::DeclArg => {
                                    let arg = sp.pop_ref::<DeclArg>();
                                    if arg.name.is_some() || arg.is_default { any = true; }
                                    if arg.is_default { any_default = true; }
                                    num_args += 1;
                                    first_arg = sp;
                                },
                                _ => unreachable!(),
                            }
                        }
                        if any {
                            let new_full_name: ast::Ident;
                            let old_name: ast::Ident;
                            let davs: &'static [XAndCommon<DeclArg>] = unsafe { slice::from_raw_parts(transmute(first_arg.ptr()), num_args) };
                            {
                                let name = tr.mutate_mark(decl.name);
                                if let TokenTree::Token(_, token::Ident(ref mut ident)) = *name {
                                    old_name = *ident;
                                    new_full_name = mutate_name(ident, davs.iter().map(|dav| dav.x.name.as_ref()), ctx);
                                    *ident = new_full_name;
                                } else {
                                    unreachable!();
                                }
                            }
                            // generate stubs for default arguments
                            if any_default {
                                sp = save;
                                pushx!(VariantData::DeclEnd { first_arg: first_arg, decl: decl, davs: davs, old_name: old_name, new_full_name: new_full_name, args_end: args_end_mark, decl_end: None });
                                continue_next!(State::SeekingSemiOrOpenBrace);
                            }
                        }
                        continue_next!(State::Null { expecting_operator: true, after_semi_or_brace: false });
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
                                        continue_next!(State::DeclArgStart { pending_default: true });

                                    } else { st = st4; }
                                } else { st = st3; }
                            } else { st = st3; }
                        } else { st = st2; }
                    },
                    &token::Comma => continue_next!(State::DeclArgStart { pending_default: false }),
                    _ => (),
                }
                pushx_manual!(StateVariant::DeclArg, DeclArg { name: None, ty_start: Some(tr.mark_last()), ty_end: None, is_default: pending_default });
                pushx!(VariantData::DeclArgStart { pending_default: false });
                in_func_parens = true;
                continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false });
            },
            State::DeclEnd { first_arg, decl, davs, old_name, new_full_name, args_end, decl_end } => {
                // only get here if we need defaults
                match st.token {
                    &token::Semi | &Token::CloseDelim(DelimToken::Brace) => {
                        let decl_end: Mark = decl_end.unwrap_or_else(|| tr.mark_last());
                        let default_count = davs.iter().filter(|dav| dav.x.is_default).count();
                        for num_include in 0..default_count {
                            gen_default_stub(tr, &davs, num_include, decl.generic_start, decl.args_start, args_end, decl_end, &old_name, &new_full_name, cur_in_trait_or_impl, ctx);
                        }
                        sp = first_arg;
                        {
                            let common = sp.pop_ref::<Common>();
                            delim_depth = common.delim_depth;
                            in_func_parens = common.in_func_parens;
                            assert_eq!(common.variant(), StateVariant::DeclStart);
                            sp.pop_ref::<DeclStuff>();
                        }
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                    &Token::OpenDelim(DelimToken::Brace) => {
                        // repush?
                        let decl_end = tr.mark_last();
                        pushx!(VariantData::DeclEnd { first_arg: first_arg, decl: decl, davs: davs, old_name: old_name, new_full_name: new_full_name, args_end: args_end, decl_end: Some(decl_end) });
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                    _ => panic!("unexpected token at DeclEnd XXX"),
                }
            }
            State::DefinitelyType { mut angle_depth, mode, start_of_type } => {
                let mut new_start_of_type = false;
                match st.token {
                    &token::Lt => {
                        angle_depth += 1;
                        new_start_of_type = true;
                    },
                    &token::Gt if angle_depth > 0 => {
                        angle_depth -= 1;
                        if angle_depth == 0 && mode == DTMode::EagerAngle {
                            if delim_depth != 0 {
                                ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                            }
                            continue_next!(State::Pop);
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
                            continue_same!(State::Pop);
                        } else {
                            angle_depth -= 2;
                        }
                        if angle_depth == 0 && mode == DTMode::EagerAngle {
                            if delim_depth != 0 {
                                ctx.cx.span_err(*st.span, "unexpected '>' misnested w.r.t. ()[]{}. possible parser bug");
                            }
                            continue_next!(State::Pop);
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
                            continue_same!(State::Pop);
                        }
                    },
                    // tokens allowed in a type
                    &token::ModSep | &token::Underscore | &token::Not |
                    &token::Question | &token::Dollar | &token::DotDotDot => (),
                    &token::Lifetime(_) | &token::RArrow => { new_start_of_type = true; },
                    &token::Ident(ident) if ident.name == keywords::As.name() ||
                                            ident.name == keywords::Unsafe.name() => {
                        new_start_of_type = true;
                    },
                    &token::Ident(ident) if ident.name == keywords::For.name() && start_of_type => {
                        st = st_or_return!();
                        if let &token::Lt = st.token {
                            pushx!(VariantData::DefinitelyType { angle_depth: angle_depth, mode: mode, start_of_type: true });
                            continue_next!(State::DefinitelyType { angle_depth: 1, mode: DTMode::EagerAngle, start_of_type: true });
                        } else {
                            ctx.cx.span_err(*st.span, "'for' in type not followed by '<'. possible parser bug");
                            continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                        }
                    },
                    &token::Ident(ident) if ident.name == keywords::Extern.name() => {
                        // allow extern "C"
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Literal(..) => (),
                            _ => {
                                st = st2;
                                continue_same!(State::DefinitelyType { angle_depth: angle_depth, mode: mode, start_of_type: true });
                            },
                        }
                    },
                    &token::Ident(ident) if !((ident.name == keywords::For.name() && !start_of_type) ||
                                              ident.name == keywords::Where.name()) => (),
                    &token::Pound => {
                        let st2 = st_or_return!();
                        if let &token::OpenDelim(DelimToken::Bracket) = st2.token {
                            // attribute
                            delim_depth += 1;
                            pushx!(VariantData::DefinitelyType { angle_depth: angle_depth, mode: mode, start_of_type: true });
                            continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                        } else {
                            ctx.cx.span_err(*st.span, "this shouldn't be in a type. possible parser bug");
                            st = st2;
                            continue 'outer_loop;
                        }
                    },
                    // tokens allowed inside other things
                    &token::Semi if delim_depth != 0 => {
                        // inside an array decl, puts us into expression context
                        pushx!(VariantData::DefinitelyType { angle_depth: angle_depth, mode: mode, start_of_type: false });
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                    &token::Comma | &token::Colon | &token::Eq if
                        delim_depth != 0 || angle_depth != 0 => {
                        new_start_of_type = true;
                    },
                    &token::BinOp(token::Plus) if
                        delim_depth != 0 || angle_depth != 0 || mode == DTMode::InItem => {
                        new_start_of_type = true;
                    },
                    // tokens allowed at start
                    &token::BinOp(token::And) | &token::BinOp(token::Star) if start_of_type => {
                        new_start_of_type = true;
                    },
                    // anything else ends the type
                    _ => {
                        if angle_depth == 0 && delim_depth == 0 {
                            continue_same!(State::Pop);
                        } else {
                            match st.token {
                                &token::BinOp(token::Star) | &token::BinOp(token::Plus) => (),
                                _ => {
                                    ctx.cx.span_err(*st.span, "this shouldn't be in a type. possible parser bug");
                                    continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                                },
                            }
                        }
                    },
                }
                continue_next!(State::DefinitelyType { angle_depth: angle_depth, mode: mode, start_of_type: new_start_of_type });
            },
            State::Pop => {
                let variant = sp.last_ref::<Common>().variant();
                match variant {
                    StateVariant::GotIdent => continue_same!(popx!(VariantData::GotIdent)),
                    StateVariant::GotFnName => continue_same!(popx!(VariantData::GotFnName)),
                    StateVariant::Null => continue_same!(popx!(VariantData::Null)),
                    StateVariant::LambdaEnd => continue_same!(popx!(VariantData::LambdaEnd)),
                    StateVariant::CallArgStart => continue_same!(popx!(VariantData::CallArgStart)),
                    StateVariant::DeclArgStart => continue_same!(popx!(VariantData::DeclArgStart)),
                    StateVariant::DeclEnd => continue_same!(popx!(VariantData::DeclEnd)),
                    StateVariant::DefinitelyType => continue_same!(popx!(VariantData::DefinitelyType)),
                    StateVariant::SeekingSemiOrOpenBrace => continue_same!(popx!(VariantData::SeekingSemiOrOpenBrace)),
                    StateVariant::TraitDeclOpen => continue_same!(popx!(VariantData::TraitDeclOpen)),
                    StateVariant::ImplDeclAfterGeneric => continue_same!(popx!(VariantData::ImplDeclAfterGeneric)),
                    StateVariant::ImplDeclAfterTrait => continue_same!(popx!(VariantData::ImplDeclAfterTrait)),
                    StateVariant::InTraitOrImpl => continue_same!(popx!(VariantData::InTraitOrImpl)),
                    StateVariant::ExcessCloses => continue_same!(popx!(VariantData::ExcessCloses)),
                    _ => unreachable!("variant was {:?}?", variant),

                }

            },
            State::SeekingSemiOrOpenBrace => loop {
                match st.token {
                    &token::Semi | &token::OpenDelim(DelimToken::Brace) => {
                        if delim_depth == 0 {
                            continue_same!(State::Pop);
                        }
                    },
                    &token::OpenDelim(_) => {
                        delim_depth += 1;
                        pushx!(VariantData::SeekingSemiOrOpenBrace);
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                    },
                    &token::CloseDelim(_) => {
                        if delim_depth != 0 {
                            delim_depth -= 1;
                        } else {
                            ctx.cx.span_err(*st.span, "unexpected close delim. possible parser bug");
                            continue_same!(State::Pop);
                        }
                    },
                    _ => ()
                }
                st = st_or_return!();
            },
            State::LambdaEnd => {
                match st.token {
                    &token::Comma => {
                        pushx!(VariantData::LambdaEnd);
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                    },
                    _ => {
                        continue_same!(State::Null { expecting_operator: true, after_semi_or_brace: false });
                    },
                }
            },
            State::TraitDeclOpen { trait_start, trait_end } => {
                let trait_end = trait_end.unwrap_or(tr.mark_last());
                match st.token {
                    &token::Colon => {
                        pushx!(VariantData::TraitDeclOpen { trait_start: trait_start, trait_end: Some(trait_end) });
                        continue_next!(State::SeekingSemiOrOpenBrace);
                    },
                    &token::Ident(ident) if ident.name == keywords::Where.name() => {
                        pushx!(VariantData::TraitDeclOpen { trait_start: trait_start, trait_end: Some(trait_end) });
                        continue_next!(State::SeekingSemiOrOpenBrace);
                    },
                    &token::OpenDelim(DelimToken::Brace) => {
                        let new = pushx!(VariantData::InTraitOrImpl { trait_start_end: Some((trait_start, trait_end)), prev: cur_in_trait_or_impl });
                        cur_in_trait_or_impl = Some(&new.x);
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                    _ => {
                        ctx.cx.span_err(*st.span, "unexpected thingy in trait decl. possible parser bug");
                        continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                }

            },
            State::ImplDeclAfterGeneric => {
                pushx!(VariantData::ImplDeclAfterTrait { trait_start: tr.mark_last(), trait_end: None });
                continue_same!(State::DefinitelyType { angle_depth: 0, mode: DTMode::InItem, start_of_type: true });
            },
            State::ImplDeclAfterTrait { trait_start, trait_end } => {
                match st.token {
                    &token::Ident(ident) if ident.name == keywords::For.name() ||
                                            ident.name == keywords::Where.name() => {
                        pushx!(VariantData::ImplDeclAfterTrait { trait_start: trait_start, trait_end: Some(tr.mark_last()) });
                        continue_next!(State::SeekingSemiOrOpenBrace);
                    },
                    &token::OpenDelim(DelimToken::Brace) => {
                        let trait_start_end = if let Some(trait_end) = trait_end {
                            Some((trait_start, trait_end))
                        } else { None };
                        let new = pushx!(VariantData::InTraitOrImpl { trait_start_end: trait_start_end, prev: cur_in_trait_or_impl });
                        cur_in_trait_or_impl = Some(&new.x);
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                    _ => {
                        ctx.cx.span_err(*st.span, "unexpected thingy in impl decl. possible parser bug");
                        continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                }
            },
            State::InTraitOrImpl { trait_start_end, prev } => {
                match st.token {
                    &token::CloseDelim(_) => {
                        cur_in_trait_or_impl = prev;
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                    _ => {
                        let new = pushx!(VariantData::InTraitOrImpl { trait_start_end: trait_start_end, prev: prev });
                        cur_in_trait_or_impl = Some(&new.x);
                        continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
                    },
                }
            },
            State::ExcessCloses => {
                ctx.cx.span_err(*st.span, "excess close delimeters. possible parser bug");
                pushx!(VariantData::ExcessCloses);
                continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: true });
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
        let mut ctx = Context { cx: cx, use_valid_idents: false };
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

