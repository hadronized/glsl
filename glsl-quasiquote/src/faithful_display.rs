//! A module that provides a better Display impl for proc_macro.
//!
//! The idea is that the impl of [`Display`] for proc_macro types doesn’t respect the input’s
//! layout. So you will loose your formatting, indentation, etc.
//!
//! This module provides an implementation of [`Display`] that respects the input’s formatting.

use std::fmt::{self, Display, Write};
use proc_macro::{Delimiter, Ident, Group, Literal, Punct, Span, TokenStream, TokenTree};

pub fn as_faithful_display<'a>(stream: &'a TokenStream) -> impl Display + 'a {
  struct D<'a>(&'a TokenStream);

  impl<'a> fmt::Display for D<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
      self.0.clone().faithful_fmt(f, Span::call_site()).map(|_| ())
    }
  }

  D(stream)
}

/// Automatically adjust with whitespaces a formatter based on the current span and the previous
/// one.
fn whitespace_adjust_span(
  f: &mut fmt::Formatter,
  prev_span: Span,
  current_span: Span
) -> Result<(), fmt::Error> {
  if current_span.start().line == prev_span.end().line {
    // we are on the same line, we just have to adjust the number of spaces
    let nb_spaces = current_span.start().column - prev_span.end().column;
    f.write_str(" ".repeat(nb_spaces).as_str())
  } else {
    // we are on different lines; first add the newlines difference, then adjust with spaces
    let nb_newlines = current_span.start().line - prev_span.end().line;
    let nb_spaces = current_span.start().column;
    f.write_str("\n".repeat(nb_newlines).as_str())?;
    f.write_str(" ".repeat(nb_spaces).as_str())
  }
}

pub trait FaithfulDisplay {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error>;
}

impl FaithfulDisplay for Ident {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error> {
    let current_span = self.span();
    whitespace_adjust_span(f, prev_span, current_span)?;

    self.fmt(f).map(|_| current_span)
  }
}

impl FaithfulDisplay for Literal {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error> {
    let current_span = self.span();
    whitespace_adjust_span(f, prev_span, current_span)?;

    self.fmt(f).map(|_| current_span)
  }
}

impl FaithfulDisplay for Punct {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error> {
    let current_span = self.span();
    whitespace_adjust_span(f, prev_span, current_span)?;

    f.write_char(self.as_char()).map(|_| current_span)
  }
}

impl FaithfulDisplay for Group {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error> {
    let mut current_span = self.span_open();
    whitespace_adjust_span(f, prev_span, current_span)?;

    match self.delimiter() {
      Delimiter::Parenthesis => {
        faithful_delimited(f, '(', ')', self.stream(), current_span)?;
      }

      Delimiter::Brace => {
        faithful_delimited(f, '{', '}', self.stream(), current_span)?;
      }

      Delimiter::Bracket => {
        faithful_delimited(f, '[', ']', self.stream(), current_span)?;
      }

      Delimiter::None => {
        current_span = self.stream().faithful_fmt(f, current_span)?;
        whitespace_adjust_span(f, prev_span, current_span)?;
      }
    }

    Ok(self.span_close())
  }
}

impl FaithfulDisplay for TokenStream {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error> {
    let mut current_span = prev_span;

    for tree in self {
      current_span = tree.faithful_fmt(f, current_span)?;
    }

    Ok(current_span)
  }
}

impl FaithfulDisplay for TokenTree {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: Span) -> Result<Span, fmt::Error> {
    match self {
      TokenTree::Group(gr) => gr.faithful_fmt(f, prev_span),
      TokenTree::Ident(ident) => ident.faithful_fmt(f, prev_span),
      TokenTree::Punct(p) => p.faithful_fmt(f, prev_span),
      TokenTree::Literal(lit) => lit.faithful_fmt(f, prev_span)
    }
  }
}

fn faithful_delimited(
  f: &mut fmt::Formatter,
  del_first: char,
  del_end: char,
  stream: TokenStream,
  prev_span: Span
) -> Result<(), fmt::Error> {
  f.write_char(del_first)?;

  let current_span = stream.faithful_fmt(f, prev_span)?;

  whitespace_adjust_span(f, prev_span, current_span)?;
  f.write_char(del_end)
}
