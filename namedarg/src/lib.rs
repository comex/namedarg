#![cfg_attr(not(feature = "force_macros11_mode"), feature(plugin))]
#![cfg_attr(not(feature = "force_macros11_mode"), plugin(namedarg_plugin))]

#[cfg(feature = "force_macros11_mode")]
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

#[cfg(not(feature = "force_macros11_mode"))]
#[macro_export]
macro_rules! namedarg {
    { $($stuff:tt)* } => {
        include!("/tmp/wtf.rs");
        mod A {
            #![feature(plugin)]
            #![plugin(asdf)]
        }
        //_namedarg_plugin! { $($stuff)* }
    }
}
