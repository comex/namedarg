#![feature(rustc_macro, custom_derive)] // for now
#![cfg_attr(not(feature = "force_macros11_mode"),
            feature(type_ascription))]
#[macro_use]
extern crate namedarg;
namedarg! {
    use std::str::FromStr;
    use std::fmt::Debug;

    fn test_ref_pattern(a &x: &i32) -> i32 {
        x
    }

    trait NongenericParentTrait : Debug {
        fn foo(#[default] a _: Option<i32>);
    }
    trait NongenericNoParentTrait {
        fn foo(&mut self, #[default] a _: Option<i32>);
    }
    trait GenericParentTrait<T> : Debug {
        fn foo(#[default] a _: Option<T>) {}
    }
    trait GenericNoParentTrait<T> {
        fn foo(&self, #[default] a _: Option<T>) {}
    }
    impl<T> GenericNoParentTrait<T> for T {
        fn foo(&self, #[default] a _: Option<T>) {}
    }
    impl NongenericNoParentTrait for i32 {
        fn foo(&mut self, #[default] a _: Option<i32>) {}
    }
    struct Test;
    impl Test {
        fn foo(self, #[default] a _: Option<i32>) {}
    }
    trait TraitUsingBareTypes {
        fn foo(i32, #[default] a _: Option<i32>) {}
    }

    fn default_func(x x: i32,
                    (y, z): (i32, i32),
                    #[default] k: Option<i32>,
                    #[default] _ arg: Option<i32>)
                    -> i32 {
        x + y + z + k.unwrap_or(1) + arg.unwrap_or(2)
    }
    fn generic_default_func<T>(#[default] _ t: Option<T>) -> T
        where T: FromStr,
              <T as FromStr>::Err: Debug {
        t.unwrap_or_else(|| FromStr::from_str("123").unwrap())
    }
    fn named_func(arg foo: i32, _ arg2: i32) -> i32 { foo + arg2 }
    fn right_named_func(arg: i32, _ arg2: i32) -> i32 { arg + arg2 }
    fn left_named_func(_ arg: i32, arg2: i32) -> i32 { arg + arg2 }
    #[test]
    fn test() {
        assert_eq!(named_func(arg: 5, arg2: 1), 6);
        assert_eq!(right_named_func(5, arg2: 1), 6);
        assert_eq!(left_named_func(arg: 5, 1), 6);
        assert_eq!(default_func(x: 0, (1, 1), Some(2), arg: Some(5)), 9);
        assert_eq!(default_func(x: 0, (1, 1)), 5);
        assert_eq!(generic_default_func::<i32>(), 123);
        assert_eq!(test_ref_pattern(a: &1), 1);
        assert_eq!(named_func{arg:arg2:}(5, 1), 6);
        let x = right_named_func{:arg2:};
        assert_eq!(x(1, 2), 3);
        let y = left_named_func{arg::};
        assert_eq!(y(1, 2), 3);
    }

    #[allow(dead_code)]
    #[test]
    fn test_untransformed() {
        let xs = [1, 2, 3];
        let res = xs.into_iter().fold(0, |a: usize, &b: &usize| 10 * a + b);
        assert_eq!(res, 123);
        assert_eq!(r###"huh?" foo(a: b) "huh?"###,
                   "huh?\" foo(a: b) \"huh?");
        struct X { a: usize }
        fn ignore<T>(_: T) {}
        ignore({struct Y { a: usize } 5});
        ignore(X { a: 5 });
        let X { a } = X { a: 5 };
        let _ = a;
        let t = 1;
        if 1 == t { { } } // double-open is a special case for MaybeFuncRef
    }
    #[cfg(not(feature = "force_macros11_mode"))]
    #[test]
    fn test_type_ascription() {
        fn ignore<T>(_: T) {}
        ignore(5: usize);
        let a = 5;
        ignore((a: usize));
    }
}
