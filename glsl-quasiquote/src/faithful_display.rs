//! A module that provides a better Display impl for proc_macro.
//!
//! The idea is that the impl of [`Display`] for proc_macro types doesn’t respect the input’s
//! layout. So you will loose your formatting, indentation, etc.
//!
//! This module provides an implementation of [`Display`] that respects the input’s formatting.

use std::fmt::{self, Display, Write};
use proc_macro::{Delimiter, Group, Ident, LineColumn, Literal, Punct, TokenStream, TokenTree};

pub trait FaithfulDisplay {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error>;
}

impl FaithfulDisplay for Ident {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error> {
    let current_span = self.span();
    whitespace_adjust_span(f, prev_span, current_span.start())?;

    self.fmt(f).map(|_| current_span.end())
  }
}

impl FaithfulDisplay for Literal {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error> {
    let current_span = self.span();
    whitespace_adjust_span(f, prev_span, current_span.start())?;

    self.fmt(f).map(|_| current_span.end())
  }
}

impl FaithfulDisplay for Punct {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error> {
    let current_span = self.span();
    whitespace_adjust_span(f, prev_span, current_span.start())?;

    f.write_char(self.as_char()).map(|_| current_span.end())
  }
}

impl FaithfulDisplay for Group {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error> {
    let current_span = self.span_open();
    whitespace_adjust_span(f, prev_span, current_span.start())?;

    match self.delimiter() {
      Delimiter::Parenthesis => {
        faithful_delimited(f, '(', ')', self.stream(), current_span.end(), self.span_close().start())?;
      }

      Delimiter::Brace => {
        faithful_delimited(f, '{', '}', self.stream(), current_span.end(), self.span_close().start())?;
      }

      Delimiter::Bracket => {
        faithful_delimited(f, '[', ']', self.stream(), current_span.end(), self.span_close().start())?;
      }

      Delimiter::None => {
        let line_col = self.stream().faithful_fmt(f, current_span.end())?;
        whitespace_adjust_span(f, prev_span, line_col)?;
      }
    }

    Ok(self.span_close().end())
  }
}

impl FaithfulDisplay for TokenStream {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error> {
    let mut current_span = prev_span;

    for tree in self {
      current_span = tree.faithful_fmt(f, current_span)?;
    }

    Ok(current_span)
  }
}

impl FaithfulDisplay for TokenTree {
  fn faithful_fmt(self, f: &mut fmt::Formatter, prev_span: LineColumn) -> Result<LineColumn, fmt::Error> {
    match self {
      TokenTree::Group(gr) => gr.faithful_fmt(f, prev_span),
      TokenTree::Ident(ident) => ident.faithful_fmt(f, prev_span),
      TokenTree::Punct(p) => p.faithful_fmt(f, prev_span),
      TokenTree::Literal(lit) => lit.faithful_fmt(f, prev_span)
    }
  }
}

pub fn as_faithful_display<'a>(stream: &'a TokenStream) -> impl Display + 'a {
  struct D<'a>(&'a TokenStream);

  impl<'a> fmt::Display for D<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
      // get the first span, if any
      let first =
        self.0.clone().into_iter().next() // ewww, clone
        .map(|tree| tree.span().start()).unwrap_or(LineColumn { line:0, column: 0 });

      self.0.clone().faithful_fmt(f, first).map(|_| ())
    }
  }

  D(stream)
}

/// Automatically adjust with whitespaces a formatter based on the current span and the previous
/// one.
fn whitespace_adjust_span(
  f: &mut fmt::Formatter,
  prev_span: LineColumn,
  current_span: LineColumn 
) -> Result<(), fmt::Error> {
  if current_span.line == prev_span.line {
    // we are on the same line, we just have to adjust the number of spaces
    let nb_spaces = current_span.column - prev_span.column;
    f.write_str(" ".repeat(nb_spaces).as_str())
  } else {
    // we are on different lines; first add the newlines difference, then adjust with spaces
    let nb_newlines = current_span.line - prev_span.line;
    let nb_spaces = current_span.column;
    f.write_str("\n".repeat(nb_newlines).as_str())?;
    f.write_str(" ".repeat(nb_spaces).as_str())
  }
}

fn faithful_delimited(
  f: &mut fmt::Formatter,
  del_first: char,
  del_end: char,
  stream: TokenStream,
  prev_span: LineColumn,
  final_span: LineColumn
) -> Result<(), fmt::Error> {
  f.write_char(del_first)?;

  let current_span = stream.faithful_fmt(f, prev_span)?;

  whitespace_adjust_span(f, current_span, final_span)?;
  f.write_char(del_end)
}
