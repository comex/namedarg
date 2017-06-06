![crates.io badge](https://img.shields.io/crates/v/namedarg.svg)

# Named arguments for Rust

An **experimental** (probably broken) procedural macro.  Instead of code like this:
```rust
parser.parse_item(vec![], true, false)
```
this macro lets you write code like this!
```rust
parser.parse_item(attrs: vec![], macros_allowed: true, attributes_allowed: false)
```

Written partly as a proof of concept for a potential future Rust feature, but
also to be practical and usable today.  Sort of.  In theory.

## Syntax

Strongly Swift-inspired.

Like Swift, argument names form a **mandatory** part of the function signature,
and are not reorderable; also, an argument may have separate 'external' and
'internal' names, where the former is used when calling the function, and the
latter is the name of the resulting variable.

In fact, the declaration syntax is quite similar to Swift, but with two forms
swapped around:

| Variant          | Rust                      | Swift                       | Called as            | Variable name |
| ---              | ---                       | ---                         | ---                  | ---           |
| Unnamed          | `fn foo(bar: String)`     | `func foo(_ bar: String)`   | `foo(string)`        | `bar`         |
| Basic named      | `fn foo(_ bar: String)`   | `func foo(bar: String)`     | `foo(bar: string)`   | `bar`         |
| Different names  | `fn foo(bar baz: String)` | `func foo(bar baz: String)` | `foo(bar: string)`   | `baz`         |

Swift defaults to named arguments and uses `_` for unnamed arguments, while the
extended Rust syntax defaults to *unnamed* (for compatibility with existing
Rust code), and uses `_` to indicate a *named* argument, with equivalent
external and internal names.  For different external and internal names, write
them separated by a space.

Named arguments are implemented by **appending the argument names to the
function name**, at both definitions and call sites.  As a natural consequence:

- You can **overload** functions with the same name but different argument
  labels.  This doesn't have the same pitfalls as purely type-based overloading
  as seen in C++/Java/etc., so there's no reason not to use it.  For example,
  consider constructors.  For zero- and one-argument constructors, Rust's
  existing convention of `::new()` and `::with_foo(foo)` works pretty well:

  ```rust
  HashMap::new()
  HashMap::with_capacity(100)
  ```

  But with two or more arguments, it starts to get ugly:

  ```rust
  HashMap::with_capacity_and_hasher(100, Default::default())
  ```

  In an API designed with named arguments, the progression can be more natural:

  ```rust
  HashMap::new()
  HashMap::new(capacity: 100)
  HashMap::new(capacity: 100, hasher: Default::default())
  ```

  (Note: different numbers of unnamed arguments can also be distinguished, but
  only after the first named argument.)


- Named argument syntax works both for bare fns and trait methods (in both
  trait definitions and impls).  But it doesn't work in either lambdas or
  function *types* (either bare `fn(...)` or the `Fn/FnMut/FnOnce(...)`
  traits); the type drops argument names.

  To **name** a function with named arguments without calling it, use this
  syntax (loosely inspired by Objective-C):
  
  | Called as                   | Named as
  | ---                         | ---
  | `foo()` / `foo(123)`        | `foo`
  | `foo(bar: 123)`             | `foo{bar:}`
  | `foo(123, baz: 456, 789)`   | `foo{:baz::}`

  It can then be called without labels or coerced to the appropriate `fn` type.

- ...Error messages can get a bit confusing.  I meant "appending to the
  function name" literally.  Like any other Rust procedural macro, this one
  performs a purely syntactic transformation and then hands some output code to
  the compiler, without much ability to control compilation after that.
  The following input:

  ```rust
  fn foo(_ bar: u32) {}
  foo(bar: 5);
  ```

  gets transformed into:

  ```rust
  // In "nightly" mode:
  fn foo{bar:}(bar: u32) {}
  // ^^^^^^^^^ this whole thing is stuffed into an Ident token so it's treated as
  //           a name, even though it's not a valid ident in vanilla Rust)
  // ditto for the call:
  foo{bar:}(bar: 5);

  // In "macros 1.1" mode, we have to use valid code:
  fn foo_namedarg_bar(bar: u32) {}
  foo_namedarg_bar(5);
  ```

  The nightly version makes for somewhat better-looking errors, but either way,
  if you accidentally call `foo(5)` (without argument names), the error message
  will just say **<code>unresolved name \`foo\`</code>**.  It doesn't know that you just
  messed up the labels.  (So don't forget!)

  Note: It would be nice if I could improve this with a lint somehow, but early
  lints are too early and late lints aren't run if compilation fails.

## Patterns

In Rust, unlike Swift, arguments don't actually have 'names'; they have
'patterns' just like `let` or `match`.  So this is valid, standard Rust:
```rust
fn foo(Config { a, b }: Config) {}
fn foo(TupleStruct(a, b): TupleStruct) {}
fn foo((a, b): (i32, i32)) {}
```
With this macro, patterns can be used with named arguments too... sometimes:
```rust
fn foo(config Config { a, b }: Config) {} // OK
fn foo(ts TupleStruct(a, b): TupleStruct) {} // OK
fn foo(pair (a, b): (i32, i32)) {} // No good...
```

Unfortunately, the last one creates a syntactic ambiguity: it's intended to be
a parameter named `pair` that takes a tuple, but it's already valid Rust syntax
for an *unnamed* parameter accepting a tuple-like struct called `pair`:

```rust
struct pair(i32, i32); // Lowercasing a struct name is bad style, but possible!
fn foo(pair(a, b): pair) {} // Only the space distinguishes this from the other example.
```

(Well, and the type, but using the type to differentiate wouldn't work because
of type aliases.)

Currently, the only workaround is to move destructuring into the function body:

```rust
fn foo(pair pair: (i32, i32)) { let (a, b) = pair; ... }
```

This works well enough, but is inelegant.  A future version of this plugin may
change the syntax to remove the ambiguity.  (Any official named argument
feature for Rust should probably do the same...)

## Interaction with type ascription

In theory, named argument call syntax can conflict with type ascription (which
is not stable).  Does the expression `foo(a: b)` contain a named argument `a`
with value `b`, or an unnamed argument whose value is the variable `a` coerced
to the *type* `b`?

In this plugin, named argument syntax is only triggered when the start of a
function argument consists of an identifier followed by a colon.  So type
ascription is unaffected when its expression is anything other than a plain
variable: `foo(5: usize)` or `foo(x.collect(): Vec<bool>` works fine.  And if
the expression is a variable, you can add parentheses: `foo((a: usize))`.

But if you think about it, why would you want to type-ascribe a variable in the
first place?  In almost all cases, it would be better to give the variable
definition a type annotation rather than using type ascription.  Exceptions
might include variables defined in complex patterns or match statements, which
cannot be given types, but even then there's usually a better way to write it.

## Usage

Add this to your Cargo.toml:

```toml
[dependencies]
namedarg = "0.1"
namedarg_rustc_macro = "0.1"
```

And wrap your source file in a macro invocation:

```rust
#![feature(rustc_macro, custom_derive)]
#[macro_use] extern crate namedarg;
namedarg! {

// your code here

}
```

Yeah, it's a little ugly.  Once macros 1.1 is stabilized, the first line can be omitted.

"Macros 1.1" mode is automatically enabled if a **non-nightly** rustc is
detected, although at time of writing this is useless because it is not
actually stable yet.  It has the *sliight* downside of **breaking line
numbering for everything inside `namedarg!`**, which is inherent in the design
of macros 1.1.  Like, all errors will show up pointing to the line containing
`namedarg!`.  Not exactly usable for development, but it should be possible
(post stabilization) to develop a crate on nightly and have it still compile on
stable this way.

You can force macros 1.1 mode on using a Cargo feature:

```toml
[dependencies.namedarg]
version = "0.1"
features = ["force_macros11_mode"]
[dependencies.namedarg_rustc_macro]
version = "0.1"
features = ["force_macros11_mode"]
```

## Default arguments

There's also a *very barebones* and even more experimental syntax for *default*
arguments, i.e. arguments that can be omitted when calling for a default value.
I, comex, like Swift's syntax for named arguments, but I'm not satisfied with
how any languages I know of treat default arguments.  The problem comes when
writing functions that forward their arguments to other functions, typically
the kind of small wrapper you might call a 'convenience function'.  For
example, imagine we have this Swift code:

```swift
struct Player {
    func doSomething(useFlubber: Bool = false) {}
}

func doSomethingToPlayer(username: String, useFlubber: Bool = false) {
    // Convenience function to lookup a Player by name and call doSomething
    Player(username: username)!.doSomething(useFlubber: useFlubber)
}
```

It works, but we had to write the default value twice, a DRY violation.  If we
later decided to change the default for `doSomething`, we'd probably want to
make the same change to `doSomethingToPlayer` - but if we forgot, the compiler
wouldn't complain!  Normally, refactoring functions in static languages pretty
reliably produces type errors on spots we haven't fixed up yet, but having a
different default is never a type error.

One way to avoid this issue might be to use a wrapper "config" struct whose
constructor is responsible for dealing with defaults.  (This is also often
proposed as an alternative to named arguments in general.)  This is a good
solution if there's a lot of arguments - after all, repeating the argument
*names* is already a DRY violation by itself - but it feels pretty verbose in
the common case that there's only one or two arguments to forward.

Instead, the version of default arguments I've implemented in this macro is
based on Option.  It's pretty simple: if you put #[default] before the argument
name, it's passed by default as None.  It looks like this:

```rust
impl Player {
    fn do_something(&self, #[default] _ use_flubber: Option<bool>) {
        let use_flubber = use_flubber.unwrap_or(false);
    }
}
fn do_something_to_player(username: &str, #[default] _ use_flubber: Option<bool>) {
    Player::new(username: username).unwrap().do_something(use_flubber: use_flubber)
}
do_something_to_player("someone"); // implicitly passes None
do_something_to_player("someone", Some(true)); // kinda verbose but...
```

Note: Unnamed arguments can be defaulted too.

There are two significant limitations to defaults as currently implemented.
The first is apparent in the above snippet: if you want to pass the value
rather than leaving it at the default, you have to explicitly write `Some`,
which is more verbose.

The second is that you **can't skip over** default arguments and specify later
keyword arguments:

```rust
fn foo(#[default] _ a: Option<bool>, #[default] _ b: Option<bool>) -> u32 { /* ... */ }
foo(b: Some(true)) // doesn't work!
foo(a: None, b: Some(true)) // you have to do this.
```


Before I explain the source of these limitations, I suppose I should show how
the transformation works.  The above definition gets expanded to a series of
overloaded functions:

```rust
fn foo() -> u32 { foo(a: None, b: None) }
fn foo(_ a: Option<bool>) -> u32 { foo(a: a, b: None) }
fn foo(_ a: Option<bool>, _ b: Option<bool>) -> u32 { /* ... */ }
```

A series of stub functions gets generated, one per prefix of the argument list,
which forward to the real function.  There's no better way to do it under the
function-renaming scheme (and for that matter I don't think there are any
particularly good alternatives to function renaming, though there are some
theoretical possibilities involving types that are far more complex).

Thus the second limitation: If skipping over default arguments were supported,
a stub would have to be generated for every *subset* of the argument list
rather than every prefix, so adding an argument would double the number of
stubs; this can get out of hand rather quickly.  While I don't *recommend*
having functions with high numbers of default arguments in the first place (as
I said at some point it's better to use a struct), I don't like the idea of an
implementation that outright fails on such functions.

The first limitation, having to write `Some`, would be easier to overcome.
First of all, we could imagine having the generated overloads not take
`Option`, something like:

```rust
fn foo() -> u32 { foo(a: None, b: None) }
fn foo(_ a: bool) -> u32 { foo(a: Some(a), b: None) }
fn foo(_ a: bool, _ b: bool) -> u32 { foo_IMPL(a: Some(a), b: Some(b)) }
fn foo_IMPL(_ a: bool, _ b: bool) -> u32 { /* ... */ }
```

But then how would we pass an Option when forwarding?

```rust
fn x_foo(#[default] _ a: Option<bool>) {
  foo(a: a) // type error: expected bool, found Option<bool>
}
```

For that matter, how would we pass None when (explicitly) skipping over an
argument?

There would have to be some special syntax to mean "pass through this Option
argument untouched", though it would be tricky even then (requiring either
generating even more stubs or changing the named-argument transformation).

Another possibility would be relying on the type system to disambiguate Option
and non-Option arguments:

```rust
trait OptionOrJust<T> { ... }
impl<T> OptionOrJust<T> for T { ... }
impl<T> OptionOrJust<T> for Option<T> { ... }
fn foo<A: OptionOrJust<bool>>(_ a: A)

// both work!
foo(a: true);
foo(a: Some(true));
```

It would even work correctly if the argument was itself Option-typed... usually.  But sometimes it would just get confusing:

```rust
// original:
fn foo<T>(#[default] _ x: T) { ... }
// transformed by the plugin:
fn foo<T, X: OptionOrJust<T>>(_ x: X) { ... }
// but wait...
foo(x: Some(123)) // what is T??
foo::<u32>(x: Some(123)) // error: too few type parameters provided
foo::<u32, _>(x: Some(123)) // OK, but what is this phantom type parameter?
```

...And even in nicer cases, it might make type inference a bit worse.


## Other limitations

- `macro_rules!` inside `namedarg!` doesn't work properly because of a [rustc
  bug](https://github.com/rust-lang/rust/issues/35853).  Trying to work around
  this by, say, putting `namedarg!` inside the macro expansion doesn't work
  very well - for one thing, since the implementation is based on custom
  derives, the inside of `namedarg!` must be a full set of items (things like
  `fn`s, struct declarations, etc.), not, say, an expression.  Also, because
  `namedarg!`'s expansion contains `extern crate`, it only works if specified
  once, at the crate root.  You can work around that, at least, by using
  `namedarg_inner!`, which is the same as `namedarg!` but without the `extern
  crate`: so there must be a `namedarg!` in the same file for it to work.  But
  the full-item limitation remains.  I should add some kind of escaping
  mechanism to better work around the rustc bug, allowing any macros to just be
  defined inside `namedarg!`.

- The nightly implementation is a ludicrous hack.  It used to just be a regular
  nightly compiler plugin, but that presents a worse user experience: it's not
  possible for the expansion of a `macro_rules!` macro to import a compiler
  plugin, so the user code would have to contain the logic for deciding whether
  to use the nightly or macros-1.1 implementation, which is quite a lot of
  clutter on top of what is already required.  A sane person would submit a PR
  to rustc to improve the interaction of `macro_rules!` and plugins.  I instead
  noted that expansions *can* load macros-1.1 custom derives, and wrote some
  highly unsafe code to make an unstable plugin masquerade as a macros-1.1
  plugin.
