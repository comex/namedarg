extern crate rustc_plugin;
#[macro_use]
extern crate syntax;
extern crate syntax_pos;
extern crate syntax_ext;
extern crate rustc_errors as errors;
use self::syntax::ext::base::{ExtCtxt, MacResult, MacEager};
use self::syntax_pos::Span;
use self::syntax::parse::token;
use self::syntax::tokenstream::TokenTree;
use self::syntax::util::small_vector::SmallVector;

use std::cell::UnsafeCell;

use namedarg::*;

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

use self::syntax::ast::{MetaItem, ItemKind, ExprKind};
use self::syntax::ext::base::{MultiItemModifier, LoadedMacro, Annotatable};
use rustc_macro::TokenStream;
use rustc_macro::__internal::Registry;

fn get_tts(an: &Annotatable) -> &[TokenTree] {
    if let Annotatable::Item(ref item) = *an {
        if let ItemKind::Enum(ref edef, _) = item.node {
            assert_eq!(edef.variants.len(), 1);
            let expr = edef.variants[0].node.disr_expr.as_ref().unwrap();
            if let ExprKind::Mac(ref mac) = expr.node {
                let tts = &mac.node.tts[..];
                assert!(tts.len() > 1);
                return &tts[..tts.len() - 1];
            }
        }
    }
    panic!("huh")
}

struct MyMIM;
impl MultiItemModifier for MyMIM {
    fn expand(&self,
              ecx: &mut ExtCtxt,
              span: Span,
              _meta_item: &MetaItem,
              an: Annotatable)
              -> Vec<Annotatable> {
        let tts = get_tts(&an);
        let res = expand_namedarg_plugin(ecx, span, tts);
        res.make_items().unwrap().into_iter()
            .map(|item| Annotatable::Item(item)).collect()
    }
}

#[rustc_derive_registrar]
fn hack_plugin_registrar(reg: &mut Registry) {
    //panic!("registrar!");
    let ptr = &reg as *const &mut Registry as *const *const *mut Vec<LoadedMacro>;
    let lms = unsafe { &mut ***ptr };
    lms.push(LoadedMacro::CustomDerive("_namedarg_fake_derive".to_string(),
                                       Box::new(MyMIM)));
}
#[rustc_macro_derive(_namedarg_dummy)]
pub fn namedarg_dummy(_: TokenStream) -> TokenStream {
    panic!("{:?}", hack_plugin_registrar as fn(&mut Registry) as usize)
}
