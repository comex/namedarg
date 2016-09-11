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
