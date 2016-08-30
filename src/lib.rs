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
use syntax::ast::{Token, Ident};
use syntax::util::small_vector::SmallVector;
use rustc_plugin::registry::Registry;
//use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::parse::token::keywords;
use std::rc::Rc;
use std::borrow::Cow;

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
    writer: Option<Vec<TokenTree>>,
}

#![derive(Copy, Clone)]
struct Mark {
    cur_len: usize,
}
impl Mark {
    fn dummy() -> Self { Mark { cur_len: !0usize / 2 } }
}

impl<'a> TTReader<'a> {
    fn new(initial: &'a [TokenTree]) -> Self {
        TTReader {
            stack: Vec::new(),
            cur: initial,
        }
    }
    fn next<'b: 'a>(&mut self, token_storage: &'b mut Token) -> Option<SpanToken<'b>> {
        if let Some(tok) = self.read.get(0) {
            match tok {
                &TokenTree::Token(ref span, ref token) => {
                    self.cur = &self.cur[1..];
                    SpanToken { span: span, token: token }
                },
                &TokenTree::Delimited(ref span, ref delimed) => {
                    self.stack.push((self.whole, self.cur, self.writer));
                    self.whole = &delimed.tts[..];
                    self.cur = self.whole;
                    *token_storage = token::OpenDelim(delimed.delim);
                    SpanToken { span: span, token: token_storage }
                },
                _ => panic!("unexpected tt type"),
            }
        } else {
            if let Some((pwhole, pcur, pwriter)) = self.stack.pop() {
                if let TokenTree::Delimited(ref span, ref delimed) = pcur[0] {
                    self.whole = pwhole;
                    self.cur = &pcur[1..];
                    self.writer = pwriter;
                    *token_storage = token::CloseDelim(delimed.delim);
                    SpanToken { span: span, token: token_storage }
                } else {
                    panic!("bad stack");
                }
            } else {
                None
            }
        }
    }
    fn mark_last(&self) -> Mark {
        Mark { cur_len: self.cur.len() + 1 }
    }
    fn force_pop(&mut self) {
        let (pwhole, pcur, pwriter) = self.stack.pop().unwrap();
        self.whole = pwhole;
        self.cur = pcur;
        self.writer = pwriter;
    }
    fn force_repush(&mut self) {
        self.rewind(1);
        if let TokenTree::Delimited(_, ref delimed) = pcur[0] {
            self.stack.push((self.whole, self.cur, self.writer));
            self.whole = &delimed.tts[..];
            self.cur = self.whole;
        } else {
            panic!();
        }
    }
    // assumes current stack depth is equal
    fn reset_to(&mut self, mark: Mark) {
        self.set_pos(self.whole.len() - mark.cur_len);
    }
    fn set_pos(&mut self, pos: usize) {

    }
    fn delete_from_mark(&mut self, mark: Mark) {
        self.fault_up_to(self.whole.len() - mark.len());
    }
}

fn skip_past_closing_angle_bracket(tr: &mut TTReader) -> bool /*ok*/ {
    let mut level: usize = 1;
    let mut delim_depth: usize = 0;
    let mark = tr.mark_last();
    while let Some(st) = tr.next() {
        match st.token {
            &token::Lt => {
                level += 1;
            },
            &token::Gt => {
                level -= 1;
                if level == 0 {
                    if delim_depth > 0 {
                        while delim_depth > 0 {
                            tr.force_pop();
                            delim_depth -= 1;
                        }
                        tr.reset_to(mark);
                        return false;
                    }
                    return true;
                }
            },
            &token::OpenDelim(_) => {
                if st.is_stack {
                    delim_depth += 1;
                }
            },
            &token::CloseDelim(_) => {
                if st.is_stack {
                    if delim_depth == 0 {
                        tr.force_repush();
                        tr.reset_to(mark);
                        return false;
                    }
                    delim_depth -= 1;
                }
            },
            _ => ()
        }
    }
    false
}

struct Context<'a> {
    cx: &'a mut ExtCtxt
}

fn do_transform<'a>(args: &'a [TokenTree], ctx: &mut Context) {
    enum State<'a> {
        Null,
        GotIdent,
        GotIdentColon,
        GotFn,
        GotFnName,
        CallArgStart(CallStuff<'a>),
        DeclArgStart,
        MidType { angle_depth: usize, delim_depth: usize },
    }
    struct CallStuff<'a> {
        name: Mark,
        generic_start_end: Option<(Mark, Mark)>,
        delim_depth: usize,
        args: Vec<Option<Ident>>,
    }
    struct Stuff<'a> {
        state: State,
        tr: TTReader<'a>,
        stack: Vec<State<'a>>,
        generic_start: Mark,
        generic_end: Mark,
        name: Option<(&Span, &Ident)>,
        delim_depth: usize,
        decl_have_generic: bool,
    }
    let mut stuff: Stuff<'a> = {
        state: State::Null,
        stack: Vec::new(),
        generic_start: Mark::dummy(),
        generic_end: Mark::dummy(),
        name: None,
        delim_depth: 0,
        decl_have_generic: false,
    };
    macro_rules! st_or_break {
        () => ( if let Some(st) = s.tr.next(&s.token_storage) { st } else { break } )
    }
    #[inline(always)]
    fn do_null(st: SpanToken<'a>, s: &mut Stuff) {
        if let &token::Ident(ref ident) = st.token {
            // XXX trait, attr
            if ident.name == keywords::Fn.name() {
                s.state = State::GotFn;
            } else {
                s.name = s.tr.mark_last();
                s.state = State::GotIdent;
            }
        } else if s.delim_depth != 0 {
            match st.token {
                &token::OpenDelim(_) => {
                    s.delim_depth += 1;
                },
                &token::CloseDelim(_) => {
                    s.delim_depth -= 1;
                    if s.delim_depth == 0 {
                        pop_call_stack(s);
                    }
                },
                &token::Comma => {
                    if s.delim_depth == 1 {
                        s.state = State::CallArgStart;
                    }
                },
            }
        }
    }
    #[inline(always)]
    fn do_ident<'a>(st: SpanToken<'a>, s: &mut Stuff<'a>) {
        match st.token {
            &token::ModSep => {
                s.state = State::GotIdentColon;
            },
            &token::OpenDelim(DelimToken::Paren) => {
                s.state = State::CallArgStart;
            },
            _ => do_null(st, s),
        }
    },
    #[inline(always)]
    fn do_generic<'a>(s: &mut Stuff<'a>) -> bool {
        // this really should be a type
        s.generic_start = s.tr.mark();
        let ok = skip_past_closing_angle_bracket(s.tr);
        if !ok {
            // confusing...
            s.state = State::Null;
            return false;
        }
        s.generic_end = s.tr.mark();
        let open_paren_st = st_or_break!();
        if let &token::OpenDelim(DelimToken::Paren) = open_paren_st.token {
            true
        } else {
            // taking a pointer to the function? should be uncommon
            s.tr.copy_from(generic_end);
            s.state = State::Null;
            false
        }
    }
    loop {
        match state {
            State::Null => do_null(st_or_break!(), &mut s),
            State::GotIdent => do_ident(st_or_break!(), &mut s),
            State::GotIdentColon => {
                let st = st_or_break!();
                match st.token {
                    &token::Lt => {
                        if do_generic(&mut s) {
                            s.call_stack.push(CallStuff {
                                name: name,
                                generic_start_end: (generic_start, generic_end),
                            });
                            s.generic_start = Mark::dummy();
                            s.generic_end = Mark::dummy();
                            s.state = State::CallArgStart;
                        }
                    },
                    _ => do_null(st, &mut s),
                }
            },
            State::GotFn => {
                let st = st_or_break!();
                match st.token {
                    &token::Ident(ref ident) => {
                        s.name = s.tr.mark_last_rw();
                        s.state = State::GotFnName;
                    },
                    _ => do_null(st, &mut state),
                }
            },
            State::GotFnName => {
                let st = st_or_break!();
                match st.token {
                    &token::Lt => {
                        if do_generic(&mut s) {
                            s.decl_have_generic = true;
                            s.state = State::DeclArgStart;
                        }
                    },
                    &token::OpenDelim(DelimToken::Paren) => {
                        s.decl_have_generic = false;
                        s.state = State::DeclArgStart;
                    },
                    _ => do_null(st, &mut s),
                }
            },
            State::CallArgStart => {
                let st = st_or_break!();
                if let &token::Ident(ref ident) = st.token {
                    let mark = s.tr.mark_last_rw();
                    let st2 = st_or_break!();
                    if let &token::Colon = st2.token {
                        let last = s.call_stack.last_mut().unwrap();
                        last.args.push(Some(ident));
                        s.tr.delete_from_mark(mark);
                        state = State::Null;
                    } else {
                        {
                            let last = s.call_stack.last_mut().unwrap();
                            last.args.push(None);
                        }
                        s.name = (st.span, indent);
                        do_ident(st2, &mut s);
                    },
                } else {
                    {
                        let last = s.call_stack.last_mut().unwrap();
                        last.args.push(None);
                    }
                    do_null(st, &mut s);
                }
            },
            State::MidType { ref mut angle_depth, ref mut delim_depth } => {
                let st = st_or_break!();
                match st.token {
                if let &token::Ident(ref ident) = st.token {
                    &token::Lt => {
                        *angle_depth += 1;
                    },
                    &token::Gt => {
                        *angle_depth -= 1;
                        if *angle_depth == 0 {
                            

                        }
                    },
                    &token::Semi => {
                        // only valid inside an array decl, puts us into expression context
                        s.stack.push(State::MidType { angle_depth: angle_depth, delim_depth: delim_depth });
                        s.state = State::Null;
                    },

            
            }
        }

    }
}

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

fn transform_this(args: &[TokenTree], cx: &mut ExtCtxt) -> (Option<Vec<TokenTree>>, usize) {
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

fn expand_namedarg(cx: &mut ExtCtxt, _sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    let res = transform_tts(args, cx);
    passthrough_items(cx, &res[..])
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("namedarg", expand_namedarg);
}

