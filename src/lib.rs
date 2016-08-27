#![feature(plugin_registrar, rustc_private, slice_patterns)]

#[macro_use]
extern crate syntax;
extern crate syntax_pos;
extern crate rustc_plugin;
extern crate rustc_errors as errors;
use syntax::tokenstream::TokenTree;
use syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use syntax_pos::Span;
use syntax::ast;
use syntax::util::small_vector::SmallVector;
use rustc_plugin::registry::Registry;
//use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::parse::token::keywords;

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

fn transform_this_fn(args: &[TokenTree]) -> Option<(Vec<TokenTree>, &[TokenTree])> {
    let first = &args[0];
    let ident = if let &TokenTree::Token(_, token::Ident(ref ident)) = first {
        ident
    } else { return None };
    if ident.name != keywords::Fn.name() { return None; }
    let delimed = if let (Some(&TokenTree::Token(_, token::Ident(ref ident))),
                          Some(&TokenTree::Delimited(_, ref delimed)))
                          = (args.get(1), args.get(2)) { &**delimed } else { return None };
    if delimed.delim != token::DelimToken::Paren { return None };
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
                    Some(&TokenTree::Token(_, token::Comma)) => {
                        i += 1;
                        break;
                    },
                    None => { break 'should_transform; },
                    _ => { i += 1; },
                }
            }
        }
    }
    if !should_transform { return None };
    #[derive(Debug)]
    struct Arg<'a> {
        name: Option<&'a ast::Ident>,
        pattern: Option<&'a [TokenTree]>,
        ty_etc: &'a [TokenTree],
    }
    let mut parsed_out: Vec<Arg> = Vec::new();
    let mut remaining = args;
    while remaining.len() != 0 {
        let mut this_arg = remaining;
        let mut next: &[TokenTree] = &[];
        let mut colon_idx = None;
        for (i, tt) in remaining.iter().enumerate() {
            match tt {
                &TokenTree::Token(_, token::Comma) => {
                    this_arg = &remaining[..i];
                    next = &remaining[i+1..];
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
                match this_arg.get(1) {
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
        let (pattern, ty_etc): (Option<&[TokenTree]>, &[TokenTree]) = match colon_idx {
            Some(idx) => {
                let (a, b) = this_arg.split_at(idx);
                (Some(a), b)
            },
            None => (None, this_arg)
        };
        parsed_out.push(Arg { name: name, pattern: pattern, ty_etc: ty_etc });
        remaining = next;
    }
    panic!("got {:?}", parsed_out);
    None
}

fn transform_this(args: &[TokenTree]) -> Option<(Vec<TokenTree>, &[TokenTree])> {
    if let Some(x) = transform_this_fn(args) {
        Some(x)
    } else {
        None
    }
}

fn transform_tts(args: &[TokenTree]) -> Option<Vec<TokenTree>> {
    let mut remaining_args = args;
    let mut vec: Vec<TokenTree>;
    loop {
        let len = remaining_args.len();
        if len == 0 { return None }
        if let Some((to_add, next)) = transform_this(remaining_args) {
            vec = args[..(args.len() - len)].to_vec();
            vec.extend_from_slice(&to_add[..]);
            remaining_args = next;
            break;
        } else {
            remaining_args = &remaining_args[1..];
        }
    }
    loop {
        if remaining_args.len() == 0 { return Some(vec) }
        if let Some((to_add, next)) = transform_this(remaining_args) {
            vec.extend_from_slice(&to_add[..]);
            remaining_args = next;
        } else {
            vec.push(remaining_args[0].clone());
            remaining_args = &remaining_args[1..];
        }
    }
}

fn expand_namedarg(cx: &mut ExtCtxt, _sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    let res = transform_tts(args);
    passthrough_items(cx, if let Some(ref tts) = res { tts } else { args })
}


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("namedarg", expand_namedarg);
}

