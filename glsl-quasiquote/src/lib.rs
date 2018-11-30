#![feature(proc_macro_span)]

//! # GLSL quasiquoting.
//!
//! This crate exports a procedural macro: `glsl!`. It enables quasiquoting by allowing you to
//! embed GLSL source code directly into rust via the syntax:
//!
//! ```ignore
//! glsl!{
//!   // your GLSL code here
//!   void main() {
//!   }
//! }
//! ```
//!
//! The `glsl!` macro accepts the GLSL code directly. You can then write plain GLSL. Especially,
//! since version **0.2**, the macro accepts plain GLSL pragmas (both `#version` and `#extension`).
//!
//! The `glsl!` procedural macro resolves at compile-time to `glsl::syntax::TranslationUnit`,
//! allowing you to manipulate the GLSL AST directly. Feel free to have a look at the
//! [`glsl`](https://crates.io/crates/glsl) crate for further information.
//!
//! # Getting started
//!
//! Add the following to your dependencies in your `Cargo.toml`:
//!
//! ```ignore
//! glsl = "0.11"
//! glsl-quasiquote = "0.2"
//! ```
//!
//! Then, you currently need to have a nightly compiler and the following feature enabled:
//!
//! ```ignore
//! #![feature(proc_macro_hygiene)]
//! ```
//!
//! Then, depending on which you’re using the 2018 edition or not:
//!
//! > *Non-2018 edition*
//!
//! ```ignore
//! extern crate glsl;
//! #[macro_use] extern crate glsl_quasiquote;
//! ```
//!
//! > *2018 edition*
//!
//! ```ignore
//! extern crate glsl;
//! use glsl_quasiquote::glsl;
//! ```
//!
//! # Special warnings and considerations
//!
//! Because of the nature of the Rust tokenizer, dots (`.`) at the beginning of a token is not part
//! of the token. For instance, `.3` is reinterpreted as `.` and `3` (two tokens). This will lead
//! to incorrect parsing if you try to represent the number `0.3` with `.3`. While accepted by
//! [glsl](https://crates.io/crates/glsl), this is not accepted by this crate. This limitation is
//! due to how Rust tokenizes input in procedural macro and is very unlikely to change.

extern crate glsl;
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use] extern crate quote;

use glsl::parser::{Parse, ParseResult};
use glsl::syntax;
use proc_macro2::TokenStream;
use std::iter::FromIterator;

use tokenize::Tokenize;

mod quoted;
mod tokenize;

/// Create a [`TranslationUnit`].
#[proc_macro]
pub fn glsl(mut input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  // prior to parsing, we try to detect annotations
  let mut code = Vec::new();

  loop {
    let (pp, input_rest) = recognize_pragma(input);

    input = input_rest;

    match pp {
      Some(pp) => {
        code.push(syntax::ExternalDeclaration::Preprocessor(pp));
      }

      None => break
    }
  }

  // annotation detection done, we can go on normally
  let s = format!("{}", input);
  let parsed: ParseResult<syntax::TranslationUnit> = Parse::parse_str(s.as_str());

  if let ParseResult::Ok(mut tu) = parsed {
    // add the eventual annotations
    code.append(&mut (tu.0).0);

    // create the stream and return it
    let mut stream = TokenStream::new();
    syntax::TranslationUnit(syntax::NonEmpty(code)).tokenize(&mut stream);

    stream.into()
  } else {
    panic!("GLSL error: {:?}", parsed);
  }
}

// Recognize a # pragma.
fn recognize_pragma(
  input: proc_macro::TokenStream
) -> (Option<syntax::Preprocessor>, proc_macro::TokenStream) {
  let mut iter = input.into_iter().peekable();

  // get the span info on the dash, if any
  let dash_start = iter.peek().and_then(|token| {
    match token {
      proc_macro::TokenTree::Punct(ref dash_p) if dash_p.as_char() == '#' => {
        Some(dash_p.span().start())
      }

      _ => None
    }
  });

  match dash_start {
    Some(ref dash_start) => {
      // drop the dash
      iter.next().unwrap();

      let mut pragma_tokens = Vec::new();

      loop {
        // peek the next token and check whether it belongs to the same stream (same line)
        let belong_to_stream = iter.peek().map(|peeked| peeked.span().start().line == dash_start.line).unwrap_or(false);

        if belong_to_stream {
          let token = iter.next().unwrap(); // safe because of peeked
          pragma_tokens.push(token);
        } else {
          // the token is on a different line; abort mission, please.
          break;
        }
      }

      let pragma_stream = proc_macro::TokenStream::from_iter(pragma_tokens);
      let content_str = format!("#{}\n", pragma_stream);

      match Parse::parse_str(content_str.as_str()) {
        ParseResult::Ok(pp) => {
          (Some(pp), proc_macro::TokenStream::from_iter(iter))
        }

        res => panic!("cannot parse GLSL annotation: {:?}", res)
      }
    }

    _ => (None, proc_macro::TokenStream::from_iter(iter))
  }
}

