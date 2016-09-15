#![cfg_attr(not(feature = "force_macros11_mode"),
   feature(plugin_registrar, rustc_private))]
#[cfg(not(feature = "force_macros11_mode"))]
include!("lib.rs");
