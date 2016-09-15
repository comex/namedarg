#![cfg_attr(not(feature = "force_macros11_mode"), feature(rustc_private))]

#[path = "../../namedarg_rustc_macro/src/namedarg.rs"]
mod namedarg;

use std::time::Duration;
pub fn nanos(d: Duration) -> u64 { d.as_secs() * 1000000000u64 + (d.subsec_nanos() as u64) }

#[cfg(feature = "force_macros11_mode")]
mod m {
    use std;
    use std::cell::UnsafeCell;
    use std::io::{Read, Write};
    use std::time::Instant;
    use std::str;
    use namedarg::{Storage, TTReader, do_transform, Context};
    use namedarg::DummyExtCtxt;
    fn write_to_file(filename: &str, what: &[u8]) {
        std::fs::File::create(filename).unwrap().write_all(what).unwrap();
    }

    pub fn main() {
        let mut filename: Option<String> = None;
        let mut tokens: bool = false;
        for arg in std::env::args().skip(1) {
            if arg == "--tokens" {
                tokens = true;
            } else if filename.is_some() {
                panic!("too many arguments");
            } else {
                filename = Some(arg);
            }
        }
        let filename = filename.expect("no filename");
        let mut source: Vec<u8> = Vec::new();
        std::fs::File::open(&filename).unwrap().read_to_end(&mut source).unwrap();
        let cx = DummyExtCtxt::new();
        let storage = UnsafeCell::new(Storage::new());
        let mut tr = TTReader::new(&source, &storage);
        if tokens {
            let mut prev_pos = 0;
            while let Some(st) = tr.next() {
                println!(">| {}", str::from_utf8(&source[prev_pos..st.span.pos]).unwrap());
                println!("{}:{} {:?}",
                         st.span.line, st.span.col, st.token);
                prev_pos = st.span.pos;
            }
            return;
        }
        let time_1 = Instant::now();
        {
            let mut ctx = Context { cx: &cx, use_valid_idents: true };
            do_transform(&mut tr, &mut ctx);
        }
        let time_2 = Instant::now();
        println!("transform:{}", ::nanos(time_2 - time_1));
        let output = tr.output();
        if let Some(output) = output {
            println!("{}: we transformed :( writing to /tmp/pp[ab]", filename);
            write_to_file("/tmp/ppa", &source);
            write_to_file("/tmp/ppb", &output);
            std::process::exit(1);
        }
        if cx.err_count() > 0 {
            println!("{}: got errors", filename);
            std::process::exit(1);
        }
        println!("ok");
    }
}

#[cfg(not(feature = "force_macros11_mode"))]
mod m {
    //#![feature(rustc_private)]
    use std;
    use std::cell::UnsafeCell;
    use std::io::{Read, Write};
    use std::time::Instant;
    use namedarg::{Storage, TTReader, do_transform, Context};
    extern crate syntax;
    use self::syntax::print::pprust;
    use self::syntax::parse::{ParseSess, filemap_to_tts};
    use self::syntax::tokenstream::TokenTree;
    fn write_to_file(filename: &str, what: &str) {
        std::fs::File::create(filename).unwrap().write_all(what.as_bytes()).unwrap();
    }

    pub fn main() {
        let filename = std::env::args().nth(1).unwrap();
        let mut source = String::new();
        std::fs::File::open(&filename).unwrap().read_to_string(&mut source).unwrap();
        let source = format!("namedarg! {{\n{}\n}} // namedarg\n", source);
        let ps = ParseSess::new();
        let time_0 = Instant::now();
        let tts: Vec<TokenTree> = filemap_to_tts(&ps, ps.codemap().new_filemap(filename.clone(), None, source));
        let time_1 = Instant::now();

        let cx = &ps.span_diagnostic;
        let storage = UnsafeCell::new(Storage::new());
        let mut tr = TTReader::new(&tts, &storage);
        {
            let mut ctx = Context { cx: cx, use_valid_idents: true };
            do_transform(&mut tr, &mut ctx);
        }
        let time_2 = Instant::now();
        println!("parse:{} transform:{}", ::nanos(time_1 - time_0), ::nanos(time_2 - time_1));
        if tr.output.is_some() {
            println!("{}: we transformed :( writing to /tmp/pp[ab]", filename);
            write_to_file("/tmp/ppa", &pprust::tts_to_string(&tts));
            write_to_file("/tmp/ppb", &pprust::tts_to_string(tr.output_as_slice()));
            std::process::exit(1);
        }
        if cx.err_count() > 0 {
            println!("{}: got errors", filename);
            std::process::exit(1);
        }
        println!("ok");
    }
}

fn main() {
    m::main()
}
