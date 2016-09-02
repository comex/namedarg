#![feature(plugin)]
#![plugin(namedarg)]
namedarg! {
    fn default_func(x x: i32, (y, z): (i32, i32), #[default] _ arg: Option<i32>) -> i32 { x + y + z + arg.unwrap_or(2) }
    fn named_func(arg foo: i32, _ arg2: i32) -> i32 { foo + arg2 }
    fn right_named_func(arg: i32, _ arg2: i32) -> i32 { arg + arg2 }
    fn left_named_func(_ arg: i32, arg2: i32) -> i32 { arg + arg2 }
    #[test]
    fn test() {
        assert_eq!(named_func(arg: 5, arg2: 1), 6);
        assert_eq!(right_named_func(5, arg2: 1), 6);
        assert_eq!(left_named_func(arg: 5, 1), 6);
        assert_eq!(default_func(x: 0, (1, 1), arg: Some(5)), 7);
        assert_eq!(default_func(x: 0, (1, 1)), 4);
    }
}
