/*
Known mishandled cases:
macro_rules! a {
    ($x:type) => { foo(x: y as $x < z) }
    // correct parse depends on whether $x is a type or ident
}
*/

use std::mem::{replace, size_of, transmute};
use std::slice;
use std::ptr;
use std::str;

#[cfg(not(feature = "println_spam"))]
macro_rules! if_println_spam { {$($stuff:tt)*} => {} }
#[cfg(feature = "println_spam")]
macro_rules! if_println_spam { {$($stuff:tt)*} => {$($stuff)*} }

#[cfg(not(debug_assertions))]
macro_rules! if_debug_assertions { {$($stuff:tt)*} => {} }
#[cfg(debug_assertions)]
macro_rules! if_debug_assertions { {$($stuff:tt)*} => {$($stuff)*} }

#[cfg(feature = "force_macros11_mode")]
macro_rules! if_rlex { {$($stuff:tt)*} => {$($stuff)*} }
#[cfg(not(feature = "force_macros11_mode"))]
macro_rules! if_rlex { {$($stuff:tt)*} => {} }

#[cfg(not(feature = "force_macros11_mode"))]
macro_rules! if_not_rlex { {$($stuff:tt)*} => {$($stuff)*} }
#[cfg(feature = "force_macros11_mode")]
macro_rules! if_not_rlex { {$($stuff:tt)*} => {} }

pub enum GetMode {
    InnerDepth(Mark),
    OuterDepth,
    SameDepth,
}

#[allow(dead_code)]
pub struct Storage { span: Span, token: Token, }

impl Storage {
    pub fn new() -> Self {
        Storage {
            span: dummy_span(),
            token: token::DotDot,
        }
    }
}

pub struct SpanToken<'a> {
    pub span: &'a Span,
    pub token: &'a Token,
}

enum Judge { Expected, Ignore, Unexpected, }

if_not_rlex! {
    extern crate rustc_errors as errors;
    extern crate syntax;
    extern crate syntax_pos;
    use self::syntax::ext::base::ExtCtxt;
    #[allow(unused_imports)]
    use self::syntax::print::pprust;
    use self::syntax::parse::token;
    use self::syntax::parse::token::{Token, DelimToken};
    use self::syntax::parse::token::keywords;
    use self::syntax_pos::Span;
    pub use self::syntax::ast::Ident;

    mod ttrw_libsyntax;
    pub use self::ttrw_libsyntax::{TTWriter, TTReader, Mark, InIdent, dummy_span};

    fn judge_other_token(tok: &Token) -> Judge {
        match tok {
            &token::Interpolated(..) | &token::MatchNt(..) | &token::SubstNt(..) |
            &token::SpecialVarNt(..) =>
                Judge::Unexpected,
            &token::Eq | &token::Le | &token::EqEq | &token::Ne | &token::Ge |
            &token::Gt | &token::AndAnd | &token::OrOr | &token::Not | &token::Tilde |
            &token::BinOp(_) | &token::BinOpEq(_) | &token::At | &token::Dot | &token::DotDot |
            &token::DotDotDot | &token::ModSep | &token::Dollar |
            &token::LArrow | &token::RArrow | &token::FatArrow =>
                Judge::Expected,
            &token::DocComment(..) | &token::Whitespace | &token::Comment | &token::Shebang(..) |
            &token::Eof =>
                Judge::Ignore,

            &token::Ident(_) | &token::OpenDelim(_) | &token::CloseDelim(_) |
            &token::Comma | &token::Colon | &token::Lt |
            &token::Semi | &token::Question | &token::Literal(..) | &token::Underscore |
            &token::Pound | &token::Lifetime(..) => unreachable!("jot"),
        }
    }
}
if_rlex! {
    mod rlex;
    pub use self::rlex::{Ident, BinOp, DelimToken, Token, token, keywords};

    mod ttrw_rlex;
    pub use self::ttrw_rlex::{TTWriter, TTReader, Mark, InIdent, Span, dummy_span};

    fn judge_other_token(tok: &Token) -> Judge {
        match tok {
            &token::Dummy | &token::White => Judge::Unexpected,
            &token::Gt | &token::Not |
            &token::BinOp(_) | &token::DotDot | &token::DotDotDot |
            &token::ModSep | &token::Dollar | &token::Eq | &token::RArrow |
            &token::Other =>
                Judge::Expected,
            &token::Eof => Judge::Ignore,

            &token::Ident(_) | &token::OpenDelim(_) | &token::CloseDelim(_) |
            &token::Comma | &token::Colon | &token::Lt |
            &token::Semi | &token::Question | &token::Literal(..) | &token::Underscore |
            &token::Pound | &token::Lifetime(..) => unreachable!("jot"),
        }
    }
}

fn mutate_name(fn_name: &str, arg_names: &mut Iterator<Item=Option<&str>>, ctx: &Context) -> String {
    let mut name: String = fn_name.to_owned();
    let olen = name.len();
    if ctx.use_valid_idents {
        name.push_str("_labeledargs");
    } else {
        name.push('{');
    }
    let mut any = false;
    while let Some(arg_name) = arg_names.next() {
        if ctx.use_valid_idents {
            if let Some(arg_name) = arg_name {
                name.push_str("_");
                name.push_str(arg_name);
            } else {
                name.push_str("_0");
            }
        } else {
            if let Some(arg_name) = arg_name {
                name.push_str(arg_name);
            }
            name.push(':');
        }
        any = true;
    }
    if !any {
        // if all arguments defaulted, use an untransformed ident
        name.truncate(olen);
        return name;
    }
    if !ctx.use_valid_idents {
        name.push('}');
    }
    name
}

pub trait ExtCtxtish {
    fn span_err(&self, sp: Span, msg: &str);
    fn span_warn(&self, sp: Span, msg: &str);
}

if_rlex! {
    use std::cell::Cell;
    pub struct DummyExtCtxt {
        err_count: Cell<usize>
    }
    impl ExtCtxtish for DummyExtCtxt {
        fn span_err(&self, sp: Span, msg: &str) {
            println!("{}:{}: error: {}", sp.line, sp.col, msg);
            self.err_count.set(self.err_count.get() + 1);
        }
        fn span_warn(&self, sp: Span, msg: &str) {
            println!("{}:{}: warning: {}", sp.line, sp.col, msg);
        }
    }
    impl DummyExtCtxt {
        pub fn new() -> Self {
            DummyExtCtxt { err_count: Cell::new(0) }
        }
        pub fn err_count(&self) -> usize {
            self.err_count.get()
        }
    }
}
if_not_rlex! {
    impl<'a> ExtCtxtish for ExtCtxt<'a> {
        fn span_err(&self, sp: Span, msg: &str) { ExtCtxt::span_err(self, sp, msg) }
        fn span_warn(&self, sp: Span, msg: &str) { ExtCtxt::span_warn(self, sp, msg) }
    }
    impl ExtCtxtish for errors::Handler {
        fn span_err(&self, sp: Span, msg: &str) { errors::Handler::span_err(self, sp, msg) }
        fn span_warn(&self, sp: Span, msg: &str) { errors::Handler::span_warn(self, sp, msg) }
    }
}

pub struct Context<'x> {
    pub cx: &'x ExtCtxtish,
    pub use_valid_idents: bool,
}


macro_rules! derive_variant_data_inner2 {
    ($enum_name:ident ;
     $variant_name:ident ;
     ($($derives:tt)*) ;
     [$($struct_fields:tt)*] ;
     $_self:tt ;
     [$($_self_fields:tt)*]) => {
        #[derive($($derives)*)]
        #[cfg_attr(feature = "derive_debug", derive(Debug))]
        pub struct $variant_name $($struct_fields)*
        impl $variant_name {
            #[inline(always)]
            #[allow(dead_code)]
            pub fn variant(&self) -> StateVariant { StateVariant::$variant_name }
            #[inline(always)]
            #[allow(dead_code)]
            pub fn to_state($_self) -> State {
                State::$variant_name $($_self_fields)*
            }
        }
    }
}

macro_rules! derive_variant_data_inner {
    ($enum_name:ident ; $variant_name:ident , $($rest:tt)* ) => {
        derive_variant_data_inner2!($enum_name ;
                                    $variant_name ;
                                    (Copy, Clone) ;
                                    [;] ;
                                    self ;
                                    []);
        derive_variant_data_inner!($enum_name ; $($rest)*);
    };
    ($enum_name:ident ; $variant_name:ident { $($name:ident : $ty:ty),* } , $($rest:tt)* ) => {
        derive_variant_data_inner2!($enum_name ;
                                    $variant_name ;
                                    (Copy, Clone) ;
                                    [{ $(pub $name : $ty),* }] ;
                                    self ;
                                    [{ $($name: self.$name),* }]);
        derive_variant_data_inner!($enum_name ; $($rest)*);
    };
    ($enum_name:ident ;) => ()
}
macro_rules! derive_variant_data {
    { pub enum $enum_name:ident { $($args:tt)* } } => {
        #[allow(non_snake_case)]
        mod VariantData {
            use super::{State, Mark, InIdent, StateVariant, XAndCommon, DeclStuff, DeclArg, VariantData, StackPtr, DTMode};
            derive_variant_data_inner!($enum_name ; $($args)*);
        }
        #[cfg_attr(feature = "derive_debug", derive(Debug))]
        #[derive(Copy, Clone)]
        pub enum $enum_name { $($args)* }
    }
}

derive_variant_data! {
pub enum State {
    Dummy,
    Null { expecting_operator: bool, after_semi_or_brace: bool },
    GotIdent { ident: InIdent },
    GotIdentColonColon { ident: InIdent },
    GotFn,
    GotFnName { name: InIdent, generic_start: Option<Mark> },
    CallArgStart,
    DeclArgStart { pending_default: bool },
    DeclEnd { first_arg: StackPtr, decl: &'static DeclStuff, davs: &'static [XAndCommon<DeclArg>], args_end: Mark, decl_end: Option<Mark> },
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
    CallStart, // name: InIdent
    CallArg, // Option<Mark>
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
    fn pop_val<T>(&mut self) -> T {
        unsafe {
            let count = (size_of::<T>() + 7) / 8;
            if_println_spam! {
                println!("popping {} u64s", count);
            }
            self.0 = self.0.offset(-(count as isize));
            ptr::read(self.0 as *mut T)
        }
    }
    #[inline(always)]
    fn last_ref<T>(self) -> &'static T {
        unsafe {
            let count = (size_of::<T>() + 7) / 8;
            let p = self.0.offset(-(count as isize));
            &*(p as *const T)
        }
    }
    #[inline(always)]
    fn last_ref_mut<T>(self) -> &'static mut T {
        unsafe {
            let count = (size_of::<T>() + 7) / 8;
            let p = self.0.offset(-(count as isize));
            &mut *(p as *mut T)
        }
    }
    #[inline(always)]
    fn push<T>(&mut self, top: StackPtr, t: T) -> &'static mut T {
        unsafe {
            if (top.0 as usize) - (self.0 as usize) < size_of::<T>() {
                panic!("stack overflow");
            }
            let count = (size_of::<T>() + 7) / 8;
            if_println_spam! {
                println!("pushing {} u64s", count);
            }
            let ptr = &mut *(self.0 as *mut T);
            ptr::write(ptr, t);
            self.0 = self.0.offset(count as isize);
            ptr
        }
    }
}

#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[derive(Clone, Copy)]
pub struct DeclStuff {
    name: InIdent,
    generic_start: Option<Mark>,
    args_start: Mark,
}
#[cfg_attr(feature = "derive_debug", derive(Debug))]
#[derive(Clone, Copy, Default)]
pub struct DeclArg {
    name: Option<InIdent>,
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
pub struct XAndCommon<X> {
    x: X,
    c: Common,
}

impl<X> XAndCommon<X> {
    #[inline(always)]
    fn new(x: X, common: Common) -> Self {
        XAndCommon { x: x, c: common }
    }
}

fn gen_default_stub<'a>(tr: &mut TTReader<'a>, args: &[XAndCommon<DeclArg>], num_include: usize, generic_start: Option<Mark>, args_start: Mark, args_end: Mark, decl_end: Mark, old_name: &str, new_full_name: &str, cur_in_trait_or_impl: Option<&'static VariantData::InTraitOrImpl>, ctx: &Context) {
    let mut i = 0;
    let partial_name = mutate_name(old_name, {
        &mut args.iter().filter_map(|&XAndCommon { x: ref arg, .. }| {
            if arg.is_default {
                i += 1;
                if i > num_include {
                    return None;
                }
            }
            Some(arg.name.map(|mark| tr.get_ident_str(mark)))
        })
    }, ctx);
    let mut tw: TTWriter = tr.writer();
    // #[allow(dead_code)]
    tw.write(token::Pound);
    tw.write(token::OpenDelim(DelimToken::Bracket));
    tw.write_ident_str("allow");
    tw.write(token::OpenDelim(DelimToken::Paren));
    tw.write_ident_str("dead_code");
    tw.write(token::CloseDelim(DelimToken::Paren));
    tw.write(token::CloseDelim(DelimToken::Bracket));

    tw.write(token::Ident(keywords::Fn.ident()));
    tw.write_ident_str(&partial_name);
    if let Some(generic_start) = generic_start {
        tw.copy_from_mark_range(generic_start, args_start, GetMode::SameDepth);
    }
    tw.write(token::OpenDelim(DelimToken::Paren));
    let mut i = 0;
    let mut arg_names: Vec<String> = Vec::with_capacity(args.len());
    for &XAndCommon { x: ref arg, .. } in args {
        if arg.is_default {
            i += 1;
            if i > num_include {
                arg_names.push("None".to_owned());
                continue;
            }
        }
        let arg_name = format!("x{}", arg_names.len());
        tw.write_ident_str(&arg_name);
        arg_names.push(arg_name);
        tw.write(token::Colon);
        if let (Some(start), Some(end)) = (arg.ty_start, arg.ty_end) {
            tw.copy_from_mark_range(start, end, GetMode::InnerDepth(args_start));
            // we already wrote 'x0: ' so change self to Self
            if_rlex! { {
                let mut slice: &mut [u8] = &mut tw.out[..];
                while let Some(idx) = rlex::idx_before_ending_whitespace_char(&slice[..]) {
                    slice = &mut {slice}[..idx];
                }
                let len = slice.len();
                if slice.ends_with(b"self") &&
                   (len == 4 ||
                    rlex::idx_before_ending_whitespace_char(&slice[..len-4]).is_some()) {
                    slice[len - 4] = b'S';
                }
            } }
            if_not_rlex! { {
                match tw.last_normal_token() {
                    Some(&mut token::Ident(ref mut ident))
                        if ident.name == keywords::SelfValue.name() => {
                        *ident = keywords::SelfType.ident();
                    },
                    _ => (),
                }
            } }
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

    tw.write_ident_str(new_full_name);
    tw.write(token::OpenDelim(DelimToken::Paren));
    for name in arg_names {
        tw.write_ident_str(&name);
        tw.write(token::Comma);
    }
    tw.write(token::CloseDelim(DelimToken::Paren));
    tw.write(token::CloseDelim(DelimToken::Brace));
    tw.finish();
}

pub fn do_transform<'x, 'a: 'x>(tr: &mut TTReader<'a>, ctx: &mut Context<'x>) {
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
        let xac = sp.pop_val::<XAndCommon<$vdata_ty>>();
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
            println!("delim_depth={} ifp={} tok={:?}", delim_depth, in_func_parens, st.token);
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
                        if_rlex! {
                            let big = name == keywords::Other.name();
                        }
                        if_not_rlex! {
                            let big = name.0 >= keywords::Default.name().0;
                        }
                        if big ||
                           name == keywords::SelfValue.name() ||
                           name == keywords::SelfType.name() ||
                           name == keywords::Super.name() {
                            continue_next!(State::GotIdent { ident: tr.last_ii(ident) });
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
                                let data = test.last_ref_mut::<XAndCommon<DeclArg>>();
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
                    _ => match judge_other_token(st.token) {
                        Judge::Expected => expecting_operator = false,
                        Judge::Ignore => (),
                        Judge::Unexpected => {
                            panic!("shouldn't get tokens like {:?}", st.token);
                        }
                    },
                }
                st = st_or_return!();
                if_println_spam! {
                    continue_same!(State::Null { expecting_operator: expecting_operator, after_semi_or_brace: after_semi_or_brace });
                }
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
                    &token::Ident(ref ident) => {
                        continue_next!(State::GotFnName { name: tr.last_ii(ident), generic_start: None });
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
                        let ii = tr.last_ii(ident);
                        let st2 = st_or_return!();
                        if let &token::Colon = st2.token {
                            let to = tr.mark_next();
                            tr.delete_mark_range(ii.mark(), to);
                            pushx_manual!(StateVariant::CallArg, Some::<InIdent>(ii));
                            pushx!(VariantData::CallArgStart);
                            in_func_parens = true;
                            continue_next!(State::Null { expecting_operator: false, after_semi_or_brace: false });
                        } else {
                            pushx_manual!(StateVariant::CallArg, None::<InIdent>);
                            pushx!(VariantData::CallArgStart);
                            in_func_parens = true;
                            st = st2;
                            continue_same!(State::GotIdent { ident: ii });
                        }
                    },
                    &token::CloseDelim(_) => {
                        // end of call
                        let mut any = false;
                        let orig_name: InIdent;
                        let mut first_arg = sp;
                        let mut num_args = 0;
                        loop {
                            let common = sp.pop_ref::<Common>();
                            delim_depth = common.delim_depth;
                            match common.variant() {
                                StateVariant::CallStart => {
                                    orig_name = *sp.pop_ref::<InIdent>();
                                    break;
                                },
                                StateVariant::CallArg => {
                                    let ident = sp.pop_val::<Option<InIdent>>();
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
                            let cavs: &'static [XAndCommon<Option<InIdent>>] = unsafe { slice::from_raw_parts(transmute(first_arg.ptr()), num_args) };
                            let new_name = {
                                let mut arg_names = cavs.iter().map(|cav| cav.x.map(|ii| tr.get_ident_str(ii)));
                                mutate_name(tr.get_ident_str(orig_name), &mut arg_names, ctx)
                            };
                            tr.mutate_ident(orig_name, new_name);
                        }
                        in_func_parens = true;
                        continue_same!(State::Null { expecting_operator: true, after_semi_or_brace: false });
                    },
                    &token::Comma => {
                        continue_next!(State::CallArgStart);
                    },
                    _ => {
                        pushx_manual!(StateVariant::CallArg, None::<InIdent>);
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
                        let ident1_ii: Option<InIdent> = match st.token {
                            &token::Ident(ref ident) => Some(tr.last_ii(ident)),
                            &token::Underscore => None,
                            _ => unreachable!(),
                        };
                        let to_delete = tr.mark_last();
                        let st2 = st_or_return!();
                        match st2.token {
                            &token::Ident(ref ident2) => {
                                let to = tr.mark_last();
                                tr.delete_mark_range(to_delete, to);
                                if let Some(ident1_ii) = ident1_ii {
                                    pushx_manual!(StateVariant::DeclArg, DeclArg {
                                        name: Some(ident1_ii), is_default: pending_default, ..DeclArg::default()
                                    });
                                    pushx!(VariantData::DeclArgStart { pending_default: false });
                                } else {
                                    pushx_manual!(StateVariant::DeclArg, DeclArg {
                                        name: Some(tr.last_ii(ident2)), is_default: pending_default, ..DeclArg::default()
                                    });
                                    pushx!(VariantData::DeclArgStart { pending_default: false });
                                    let st3 = st_or_return!();
                                    if let &Token::Colon = st3.token {} else {
                                        ctx.cx.span_err(*st3.span, "_ should be followed by an ident pattern");

                                    }
                                }
                            },
                            &token::BinOp(token::And) | &token::BinOp(token::Star) | &token::Underscore => {
                                // reference/pointer/dontcare patterns
                                if let Some(ident1_ii) = ident1_ii {
                                    let to = tr.mark_last();
                                    tr.delete_mark_range(to_delete, to);
                                    pushx_manual!(StateVariant::DeclArg, DeclArg {
                                        name: Some(ident1_ii), is_default: pending_default, ..DeclArg::default()
                                    });
                                    pushx!(VariantData::DeclArgStart { pending_default: false });
                                } else {
                                    ctx.cx.span_err(*st.span, "_ should be followed by an ident pattern");
                                }
                            },
                            _ => {
                                // this could just be a type
                                pushx_manual!(StateVariant::DeclArg, DeclArg {
                                    ty_start: Some(to_delete), is_default: pending_default, ..DeclArg::default()
                                });
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
                                    let arg = sp.pop_val::<DeclArg>();
                                    if arg.name.is_some() || arg.is_default { any = true; }
                                    if arg.is_default { any_default = true; }
                                    num_args += 1;
                                    first_arg = sp;
                                },
                                _ => unreachable!(),
                            }
                        }
                        if any {
                            let davs: &'static [XAndCommon<DeclArg>] = unsafe { slice::from_raw_parts(transmute(first_arg.ptr()), num_args) };
                            // generate stubs for default arguments
                            if any_default {
                                sp = save;
                                pushx!(VariantData::DeclEnd { first_arg: first_arg, decl: decl, davs: davs, args_end: args_end_mark, decl_end: None });
                                continue_next!(State::SeekingSemiOrOpenBrace);
                            } else {
                                let new_name = {
                                    let mut arg_names = davs.iter().map(|dav| dav.x.name.map(|mark| tr.get_ident_str(mark)));
                                    mutate_name(tr.get_ident_str(decl.name), &mut arg_names, ctx)
                                };
                                tr.mutate_ident(decl.name, new_name);
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
                                if ident.name == keywords::Default.name() { // #[default
                                    let st4 = st_or_return!();
                                    if let &token::CloseDelim(DelimToken::Bracket) = st2.token { // #[default]
                                        let to = tr.mark_next();
                                        tr.delete_mark_range(attr_start, to);
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
                pushx_manual!(StateVariant::DeclArg, DeclArg {
                    ty_start: Some(tr.mark_last()), is_default: pending_default, ..DeclArg::default()
                });
                pushx!(VariantData::DeclArgStart { pending_default: false });
                in_func_parens = true;
                continue_same!(State::Null { expecting_operator: false, after_semi_or_brace: false });
            },
            State::DeclEnd { first_arg, decl, davs, args_end, decl_end } => {
                // only get here if we need defaults
                match st.token {
                    &token::Semi | &Token::CloseDelim(DelimToken::Brace) => {
                        let old_name: &'a str = tr.get_ident_str(decl.name);
                        let new_full_name: String = {
                            let mut arg_names = davs.iter().map(|dav| dav.x.name.map(|mark| tr.get_ident_str(mark)));
                            mutate_name(old_name, &mut arg_names, ctx)
                        };
                        let decl_end: Mark = decl_end.unwrap_or_else(|| tr.mark_last());
                        let default_count = davs.iter().filter(|dav| dav.x.is_default).count();
                        for num_include in 0..default_count {
                            gen_default_stub(tr, &davs, num_include, decl.generic_start, decl.args_start, args_end, decl_end, old_name, &new_full_name, cur_in_trait_or_impl, ctx);
                        }
                        tr.mutate_ident(decl.name, new_full_name);
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
                        pushx!(VariantData::DeclEnd { first_arg: first_arg, decl: decl, davs: davs, args_end: args_end, decl_end: Some(decl_end) });
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
                                let ptr = tr.storage.get();
                                (*ptr).token = token::Gt;
                                st.token = &(*ptr).token;
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

