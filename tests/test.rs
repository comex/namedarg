#![feature(plugin)]
#![plugin(namedarg)]
namedarg! {
    fn named_func(arg foo: i32, _ arg2: i32) -> i32 { foo + arg2 }
    fn partial_named_func(arg: i32, _ arg2: i32) -> i32 { foo + arg2 }
    #[test]
    fn test() {
        assert_eq!(named_func(arg: 5, arg2: 1), 6);
        assert_eq!(partial_named_func(5, arg2: 1), 6);
    }
}
