#![feature(plugin)]
#![plugin(namedarg)]
namedarg! {
    fn named_func(arg arg: &str) {}

    #[test]
    fn test() {
        println!("hello");
    }
}
