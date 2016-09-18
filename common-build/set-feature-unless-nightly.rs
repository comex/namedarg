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
    let stdout = String::from_utf8_lossy(&op.stdout);
    // note that this refers to the release channel
    let is_nightly = stdout.contains("-nightly "); || stdout.contains("-dev ");
    if !is_nightly {
        println!("cargo:rustc-cfg=feature = \"force_macros11_mode\"");
    }
}
