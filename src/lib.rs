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
        parser.diagnostic().struct_span_err(parser.span, &msg).emit();
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

fn transform_this_fn(args: &[TokenTree]) -> (Option<Vec<TokenTree>>, usize) {
    let first = &args[0];
    let ident = if let &TokenTree::Token(_, token::Ident(ref ident)) = first {
        ident
    } else { return (None, 0) };
    if ident.name != keywords::Fn.name() { return (None, 0); }
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
    if !should_transform { return (None, 3) };
    #[derive(Debug)]
    struct Arg<'a> {
        comma: Option<&'a TokenTree>,
        name: Option<&'a ast::Ident>,
        pattern: Option<&'a [TokenTree]>,
        ty: &'a [TokenTree],
        default: Option<&'a [TokenTree]>,
    }
    let mut parsed_out: Vec<Arg> = Vec::new();
    let mut remaining = tts;
    let mut comma: Option<&TokenTree> = None;
    while remaining.len() != 0 {
        let mut this_arg: &[TokenTree] = remaining;
        let mut next: &[TokenTree] = &[];
        let mut next_comma: Option<&TokenTree> = None;
        let mut colon_idx: Option<usize> = None;
        let mut eq_idx: Option<usize> = None;
        for (i, tt) in remaining.iter().enumerate() {
            match tt {
                &TokenTree::Token(_, token::Comma) => {
                    this_arg = &remaining[..i];
                    next_comma = Some(&remaining[i]);
                    next = &remaining[i+1..];
                    break;
                },
                &TokenTree::Token(_, token::Colon) => {
                    if colon_idx.is_none() { colon_idx = Some(i); }
                },
                &TokenTree::Token(_, token::Eq) => {
                    if colon_idx.is_some() && eq_idx.is_none() { eq_idx = Some(i); }
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
                    Some(&TokenTree::Token(_, token::Underscore)) => {
                        this_arg = &this_arg[1..];
                        Some(ident2)
                    },
                    _ => None,
                },
            _ => None
        };
        let (pattern, ty, default): (Option<&[TokenTree]>, &[TokenTree], Option<&[TokenTree]>) =
        match (colon_idx, eq_idx) {
            (Some(colon_idx), Some(eq_idx)) => {
                (Some(&this_arg[..colon_idx]),
                 &this_arg[colon_idx..eq_idx],
                 Some(&this_arg[eq_idx+1..]))
            },
            (Some(colon_idx), None) => {
                let (a, b) = this_arg.split_at(colon_idx);
                (Some(a), b, None)
            },
            _ => (None, this_arg, None)
        };
        parsed_out.push(Arg { comma: comma, name: name, pattern: pattern, ty: ty, default: default });
        remaining = next;
        comma = next_comma;
    }
    let mutated = mutate_name(fn_name_ident, parsed_out.iter().map(|arg| arg.name));
    let mut replacement_tts: Vec<TokenTree> = Vec::new();
    for (i, arg) in parsed_out.iter().enumerate() {
        if i != 0 { replacement_tts.push(arg.comma.unwrap().clone()); }
        if let Some(pat) = arg.pattern { replacement_tts.extend_from_slice(pat); }
        replacement_tts.extend_from_slice(arg.ty);
    }
    let replacement = vec![
        args[0].clone(), // fn
        TokenTree::Token(fn_name_span, token::Ident(mutated)),
        TokenTree::Delimited(delimed_span, Rc::new(Delimited {
            tts: replacement_tts,
            ..*delimed
        }))
    ];

    (Some(replacement), 3)
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
    let mut next = if let Some(tt) = tts.get(0) { tt } else {
        return (None, 2)
    };
    loop {
        let tt: &TokenTree = next;
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
            next = if let Some(tt) = tts.get(i) { tt } else {
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

fn transform_this(args: &[TokenTree]) -> (Option<Vec<TokenTree>>, usize) {
    let (x, count) = transform_this_fn(args);
    if count != 0 { return (x, count); }
    transform_this_invocation(args)
}

fn transform_tts_direct(args: &[TokenTree]) -> Cow<[TokenTree]> {
    let mut remaining_args = args;
    let mut vec: Vec<TokenTree>;
    loop {
        let len = remaining_args.len();
        if len == 0 { return Cow::Borrowed(args); }
        let (x, count) = transform_this(remaining_args);
        remaining_args = &remaining_args[count..];
        if let Some(to_add) = x {
            vec = args[..(args.len() - len)].to_vec();
            vec.extend_from_slice(&to_add[..]);
            break;
        }
    }
    loop {
        if remaining_args.len() == 0 { return Cow::Owned(vec); }
        let (x, count) = transform_this(remaining_args);
        if let Some(to_add) = x {
            vec.extend_from_slice(&to_add[..]);
        }
        vec.extend_from_slice(&remaining_args[..count]);
        remaining_args = &remaining_args[count..];
    }
}

fn transform_tts(args: &[TokenTree]) -> Cow<[TokenTree]> {
    let mut tts: Cow<[TokenTree]> = transform_tts_direct(args);
    //println!("==> {:?}", tts);
    let mut replacement: Option<(usize, Vec<TokenTree>)> = None;
    let mut mut_start_i: Option<usize> = None;
    if let Cow::Borrowed(btts) = tts {
        for (i, tt) in btts.iter().enumerate() {
            if let &TokenTree::Delimited(_, ref delimed) = tt {
                let new_inner = transform_tts(&delimed.tts[..]);
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
                    let new_inner = transform_tts(&delimed.tts[..]);
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
    let res = transform_tts(args);
    passthrough_items(cx, &res[..])
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("namedarg", expand_namedarg);
}

