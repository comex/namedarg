#![cfg_attr(feature = "nightly_mode", feature(plugin))]
#![cfg_attr(feature = "nightly_mode", plugin(namedarg_plugin))]

#[cfg(feature = "macros11_mode")]
#[macro_export]
macro_rules! namedarg {
    { $($stuff:tt)* } => {
        #[macro_use]
        extern crate namedarg_rustc_macro;
        #[derive(_namedarg_fake_derive)]
        enum _namedarg_fake_enum {
            a = _namedarg_body!({$($stuff)*})
        }
    }
}

#[cfg(feature = "nightly_mode")]
#[macro_export]
macro_rules! namedarg {
    { $($stuff:tt)* } => { namedarg_plugin! { $($stuff)* } }
}

#[cfg(and(feature = "nightly_mode", feature = "macros11_mode"))]
foo!
