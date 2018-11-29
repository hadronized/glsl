# GLSL quasiquoting.

This crate exports a procedural macro: `glsl!`. It enables quasiquoting by allowing you to
embed GLSL source code directly into rust via the syntax:

```ignore
glsl!{
  // your GLSL code here
  void main() {
  }
}
```

! The `glsl!` macro accepts the GLSL code directly. You can then write plain GLSL. Especially,
! since version **0.2**, the macro accepts plain GLSL pragmas (both `#version` and `#extension`).

The `glsl!` procedural macro resolves at compile-time to `glsl::syntax::TranslationUnit`,
allowing you to manipulate the GLSL AST directly. Feel free to have a look at the
[`glsl`](https://crates.io/crates/glsl) crate for further information.

# Getting started

Add the following to your dependencies in your `Cargo.toml`:

```ignore
glsl = "0.11"
glsl-quasiquote = "0.2"
```

Then, you currently need to have a nightly compiler and the following feature enabled:

```ignore
#![feature(proc_macro_hygiene)]
```

Then, depending on which you’re using the 2018 edition or not:

> *Non-2018 edition*

```ignore
extern crate glsl;
#[macro_use] extern crate glsl_quasiquote;
```

> *2018 edition*

```ignore
extern crate glsl;
use glsl_quasiquote::glsl;
```
