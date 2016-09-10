#!/bin/sh
cargo build --release
export DYLD_LIBRARY_PATH=/Users/comex/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/ # stupid
export RUST_BACKTRACE=1
RUSTSRC=/usr/src/rust
set -e
find "$RUSTSRC" -name '*.rs' | \
    pcregrep -v '(compile-fail.*|parse-fail.*|libcore/ops\.rs|utf8-bom\.rs|libcollectionstest/str\.rs)$' | \
        parallel -v --halt 1 \
        "./target/release/test_file {}"
