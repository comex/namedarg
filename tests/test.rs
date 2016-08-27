#![feature(plugin)]
#![plugin(namedarg)]
namedarg! {
    fn named_func(arg foo: i32, _ arg2: i32) -> i32 { foo + arg2 }
    fn right_named_func(arg: i32, _ arg2: i32) -> i32 { arg + arg2 }
    fn left_named_func(_ arg: i32, arg2: i32) -> i32 { arg + arg2 }
    #[test]
    fn test() {
        assert_eq!(named_func(arg: 5, arg2: 1), 6);
        assert_eq!(right_named_func(5, arg2: 1), 6);
        assert_eq!(left_named_func(arg: 5, 1), 6);
    }
}
