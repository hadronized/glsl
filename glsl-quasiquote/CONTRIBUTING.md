# How to contribute

> **If you have a doubt on how you should contribute, feel free to open an issue and ask! There’s no
> question that shouldn’t be answered.**

## Project workflow

  - Fork the project to your GitHub workspace.
  - Create a new branch with a name that reflects your work. This project adopts a somewhat similar
    branch naming as *gitflow*:
      - If you’re working on a *feature*, prefix the branch name by `feature/`.
      - If you’re working on a *bug fix*, prefix the branch name by `fix/`.
      - If you’re adding documentation, enhancing code readability, refactoring and making the place
        a better code base to work with, prefix the branch name by `enhancement/`.
  - Add some commits to your branch. Keep in mind that your commits should be atomic enough so that
    the commit message doesn’t get to complicated. For instance, it’s highly discouraged to create
    commits that touch to a lot of files with several algorithms, concepts and ideas altered at the
    same time. Feel free to use `git commit -p`, `git add` and even `git reset` to edit your commits
    hygiene.
  - Once you’re done and your branch pushed, open a pull request on the project page.

## Code conventions

  - No `rustfmt` (so far) because of how specific formatting can become.
  - Put `{` on the same line as the item, not after. Example:
    ```rust
    struct Foo { // good
      // …
    }

    struct Foo // not good
    {
      // …
    }
    ```
  - Type ascriptions must be done on the same line, without aligning and with no space between the
    value and the `:` and a single space between the `:` and the type. However, please try to avoid
    them as much as the rustc type inference engine can deduce it for us. Example:
    ```rust
    let x: u32 = 0; // good
    let y: &'static str = "hello"; // not good, the type inference can deduce
    let a : Foo = Foo::new(); // not good: extra space between a and :
    ```
  - Short function signatures have the `where` clause – if any – on the same line. Example:
    ```rust
    fn short_fn<T>(t: T) where T: Clone + Display {
      // …
    }
    ```
  - However, long function signatures, long `trait`s and `impl`s have the the `where` clause on the
    line below the list of trait bounds, with each trait bound on its specific line, finishing with
    the last bound with the opening bracket `{`. Also, the arguments are laid out on several lines.
    Example:
    ```rust
    fn very_long_function_name_with_lot_of_trait_bounds<T, Q>(
      t: T,
      foo: &Foo<Q>,
      name: &str
    ) -> impl MyTrait
    where T: Clone + Display,
          Q: From<T> {
      // …
    }
    ```
  - All public symbols (functions, types, traits, trait implementations, macros, etc.) must be
    documented. This is not mandatory for contributions but really highly appreciated.
  - Please comment the internals of your code and don’t obfuscate. However, good onelines are still
    good oneliners. ;)
  - Test are more than welcomed.
