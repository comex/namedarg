use std::cell::UnsafeCell;
use std::str::FromStr;
use std::str;
use rustc_macro::TokenStream;
use namedarg::{Storage, TTReader, do_transform, Context, DummyExtCtxt};


/* for testing (to find the cause of LexErrors):
extern crate syntax;
use rustc_macro::__internal;
use self::syntax::parse;
fn test(s: &str) {
    __internal::with_parse_sess(|sess| {
        let name = "rustc-macro source code".to_string();
        let mut parser = parse::new_parser_from_source_str(sess, Vec::new(), name, s.to_string());
        loop {
            match parser.parse_item() {
                Ok(Some(_)) => (),
                Ok(None) => break,
                Err(err) => { panic!("{:?}", err); },
            }
        }
    });
}
*/

#[rustc_macro_derive(_namedarg_fake_derive)]
pub fn namedarg_fake_derive(input: TokenStream) -> TokenStream {
    let text: String = format!("{}", input);
    let start_idx = text.find("_namedarg_body").expect("no _namedarg_body!");
    let end_idx = text.rfind("_namedarg_body_end").expect("no _namedarg_body_end");
    let xtext = &text[start_idx..end_idx];
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
        println!("--> {}", s);
        //test(s);

        TokenStream::from_str(s).expect("failed to re-parse after transforming")
    } else {
        input
    }
}
