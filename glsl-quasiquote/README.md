<!-- cargo-sync-readme start -->

# GLSL quasiquoting.

This crate exports a procedural macro: `glsl!`. It enables quasiquoting by allowing you to
embed GLSL source code directly into rust via the syntax:

```
#![feature(proc_macro_hygiene)]

use glsl::syntax::TranslationUnit;
use glsl_quasiquote::glsl;

let tu: TranslationUnit = glsl!{
  // your GLSL code here
  void main() {
  }
};
```

The `glsl!` macro accepts the GLSL code directly. You can then write plain GLSL. Especially,
since version **0.2**, the macro accepts plain GLSL pragmas (both `#version` and `#extension`).

The `glsl!` procedural macro resolves at compile-time to `glsl::syntax::TranslationUnit`,
allowing you to manipulate the GLSL AST directly. Feel free to have a look at the
[`glsl`](https://crates.io/crates/glsl) crate for further information.

# Getting started

Add the following to your dependencies in your `Cargo.toml`:

```toml
glsl = "1"
glsl-quasiquote = "1"
```

Then, you currently need to have a nightly compiler and the following feature enabled:

```
#![feature(proc_macro_hygiene)]
```

Then, depending on which you’re using the 2018 edition or not:

> *Non-2018 edition*

```
extern crate glsl;
#[macro_use] extern crate glsl_quasiquote;
```

> *2018 edition*

```
extern crate glsl;
use glsl_quasiquote::glsl;
```

# Special warnings and considerations

Because of the nature of the Rust tokenizer, dots (`.`) at the beginning of a token is not part
of the token. For instance, `.3` is reinterpreted as `.` and `3` (two tokens). This will lead
to incorrect parsing if you try to represent the number `0.3` with `.3`. While accepted by
[glsl](https://crates.io/crates/glsl), this is not accepted by this crate. This limitation is
due to how Rust tokenizes input in procedural macro and is very unlikely to change.

<!-- cargo-sync-readme end -->
