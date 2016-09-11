extern crate rustc_plugin;
#[macro_use]
extern crate syntax;
extern crate syntax_pos;
extern crate rustc_errors as errors;
use self::rustc_plugin::registry::Registry;
use self::syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use self::syntax_pos::Span;
use self::syntax::parse::token;
use self::syntax::tokenstream::TokenTree;
use self::syntax::util::small_vector::SmallVector;

#[path = "../../common/main.rs"]
mod main;
use main::*;

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

use std::cell::UnsafeCell;
fn expand_namedarg_plugin<'a, 'b>(cx: &'a mut ExtCtxt, _sp: Span, args: &'b [TokenTree])
        -> Box<MacResult + 'static> {
    let storage = UnsafeCell::new(Storage::new());
    let mut tr = TTReader::new(args, &storage);
    {
        let mut ctx = Context { cx: cx, use_valid_idents: false };
        do_transform(&mut tr, &mut ctx);
    }
    let output = tr.output_as_slice();
    //println!("==> {}", pprust::tts_to_string(output));
    passthrough_items(cx, output)
}

/*
fn expand_namedarg_unscopeme<'a, 'b>(cx: &'a mut ExtCtxt, _sp: Span, args: &'b [TokenTree])
        -> Box<MacResult + 'static> {
    cx.syntax_env.insert(
    MacEager::items(SmallVector::zero())
}
*/


#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("_namedarg_plugin", expand_namedarg_plugin);
    //reg.register_macro("_namedarg_unscopeme", expand_namedarg_unscopeme);
}
