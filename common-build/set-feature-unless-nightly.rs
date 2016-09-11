use std::process::{Command, Stdio};
fn main() {
    let rustc = std::env::var_os("RUSTC").expect("RUSTC environment variable not set");
    let op = Command::new(rustc)
                .arg("--version")
                .stderr(Stdio::inherit())
                .output()
                .expect("couldn't launch $RUSTC --version");
    if !op.status.success() {
        panic!("$RUSTC --version failed - {:?}", op.status);
    }
    // note that this refers to the release channel - it doesn't have to literally be a nightly,
    // self-built should output the same
    let is_nightly = op.stdout.windows(9).any(|w| w == b"-nightly ");

    if !is_nightly {
        println!("cargo:rustc-cfg=feature = \"force_macros11_mode\"");
    }
}
