#![feature(rustc_private)]
#[allow(plugin_as_library)]
extern crate namedarg;
use namedarg::{TTReader, do_transform, Context};
extern crate syntax;
use syntax::print::pprust;
use syntax::parse::{ParseSess, token, filemap_to_tts};
use syntax::tokenstream::TokenTree;
use std::cell::UnsafeCell;
use std::io::{Read, Write};
use std::time::{Instant, Duration};

fn nanos(d: Duration) -> u64 { d.as_secs() * 1000000000u64 + (d.subsec_nanos() as u64) }

fn write_to_file(filename: &str, what: &str) {
    std::fs::File::create(filename).unwrap().write_all(what.as_bytes()).unwrap();
}

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let mut source = String::new();
    std::fs::File::open(&filename).unwrap().read_to_string(&mut source).unwrap();
    let source = format!("namedarg! {{\n{}\n}} // namedarg\n", source);
    let ps = ParseSess::new();
    let time_0 = Instant::now();
    let tts: Vec<TokenTree> = filemap_to_tts(&ps, ps.codemap().new_filemap(filename.clone(), None, source));
    let time_1 = Instant::now();

    let cx = &ps.span_diagnostic;
    let token_storage = UnsafeCell::new(token::DotDot);
    let mut tr = TTReader::new(&tts, &token_storage);
    {
        let mut ctx = Context { cx: cx, use_valid_idents: true };
        do_transform(&mut tr, &mut ctx);
    }
    let time_2 = Instant::now();
    println!("parse:{} transform:{}", nanos(time_1 - time_0), nanos(time_2 - time_1));
    if tr.output.is_some() {
        println!("{}: we transformed :( writing to /tmp/pp[ab]", filename);
        write_to_file("/tmp/ppa", &pprust::tts_to_string(&tts));
        write_to_file("/tmp/ppb", &pprust::tts_to_string(tr.output_as_slice()));
        std::process::exit(1);
    } else if cx.err_count() > 0 {
        println!("{}: got errors", filename);
        std::process::exit(1);
    } else {
        println!("ok");
    }
}
