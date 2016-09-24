#![cfg_attr(not(feature = "force_macros11_mode"),
   feature(rustc_attrs, rustc_private,
           rustc_macro_internals, plugin))]
#![cfg_attr(not(feature = "force_macros11_mode"),
   plugin(namedarg_hack))]
#![feature(rustc_macro, rustc_macro_lib)]
// for testing: #![feature(rustc_macro_internals, rustc_private)]
extern crate rustc_macro;
mod namedarg;

#[cfg(feature = "force_macros11_mode")]
include!("macros11.rs");
#[cfg(not(feature = "force_macros11_mode"))]
include!("nightly.rs");
