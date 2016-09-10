#![feature(rustc_macro, rustc_macro_lib)]

use std::cell::UnsafeCell;
use std::str::FromStr;
use std::str;

extern crate rustc_macro;
use rustc_macro::TokenStream;

#[path = "../../namedarg_plugin/src/lib.rs"]
mod main;

use main::{Storage, TTReader, do_transform, Context, DummyExtCtxt};

#[rustc_macro_derive(_namedarg_fake_derive)]
pub fn namedarg_fake_derive(input: TokenStream) -> TokenStream {
    let text: String = format!("{}", input);
    let nab_idx = text.find("_namedarg_body!").expect("no _namedarg_body!");
    let mut xtext = &text[..];
    let mut xtext_base: usize = 0;
    loop {
        let open_idx = xtext.find('{').expect("no {");
        let close_idx = xtext.rfind('}').expect("no }");
        xtext = &xtext[open_idx+1..close_idx];
        xtext_base += open_idx;
        if xtext_base >= nab_idx {
            break;
        }
    }
    let storage = UnsafeCell::new(Storage::new());
    let mut tr = TTReader::new(xtext.as_bytes(), &storage);
    let cx = DummyExtCtxt::new();
    {
        let mut ctx = Context { cx: &cx, use_valid_idents: true };
        do_transform(&mut tr, &mut ctx);
    };
    if cx.err_count() > 0 {
        panic!("namedarg got errors");
    }
    if let Some(output) = tr.output() {
        let s = str::from_utf8(&output).unwrap();
        //println!("--> {}", s);
        TokenStream::from_str(s).unwrap()
    } else {
        input
    }
}
