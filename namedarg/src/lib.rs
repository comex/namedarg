#[macro_export]
macro_rules! namedarg {
    { $($stuff:tt)* } => {
        #[macro_use]
        extern crate namedarg_rustc_macro;
        #[derive(_namedarg_fake_derive)]
        enum _namedarg_fake_enum {
            a = _namedarg_obody! { _namedarg_body! { $($stuff)* } _namedarg_body_end }
        }
    }
}

#[macro_export]
macro_rules! namedarg_inner {
    { $($stuff:tt)* } => {
        #[derive(_namedarg_fake_derive)]
        enum _namedarg_fake_enum {
            a = _namedarg_obody! { _namedarg_body! { $($stuff)* } _namedarg_body_end }
        }
    }
}

#[macro_export]
macro_rules! _namedarg_body {
    { $($stuff:tt)* } => { $($stuff)* }
}
