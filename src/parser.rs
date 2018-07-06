//! GLSL parsing.
//!
//! This module gives you several functions and types to deal with GLSL parsing, transforming an
//! input source into an AST. The AST is defined in the `syntax` module.
//!
//! You want to use the `parse` or `parse_str` functions along with parsers defined in
//! the `parsers` module.

use nom::{Err as NomErr, ErrorKind, IResult, Needed};
use std::error::Error;
use std::fmt;
use std::str::{from_utf8_unchecked};

/// A parse error. It contains an `ErrorKind` along with a `String` giving information on the reason
/// why the parser failed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
  kind: ErrorKind,
  info: String
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "error ({:?}): {}", self.kind, self.info)
  }
}

impl Error for ParseError {}

/// Parse result. It can either be parsed, incomplete or errored.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseResult<T> {
  /// The source was successfully parsed.
  Ok(T),
  /// The parser failed with a `ParseError`.
  Err(ParseError),
  /// More data is required to go on.
  Incomplete(Needed)
}

/// Run a parser.
///
/// This parser runs over bytes. If you need to parse a `str` instead, use `parse_str`.
pub fn parse<P, T>(source: &[u8], parser: P) -> ParseResult<T>
    where P: FnOnce(&[u8]) -> IResult<&[u8], T> {
  match parser(source) {
    IResult::Done(i, x) => {
      if i.is_empty() {
        ParseResult::Ok(x)
      } else {
        let kind = ErrorKind::Custom(0); // FIXME: use our own error kind
        let msg = unsafe { from_utf8_unchecked(i).to_owned() };
        let info = msg.lines().next().unwrap_or("").to_owned();
        ParseResult::Err(ParseError { kind, info })
      }
    },
    IResult::Error(err) => match err {
      NomErr::Code(k) => ParseResult::Err(ParseError { kind: k, info: String::new() }),
      NomErr::Node(kind, trace) => {
        let info = format!("{:#?}", trace);
        ParseResult::Err(ParseError { kind, info })
      },
      NomErr::Position(kind, p) => {
        let msg = unsafe { from_utf8_unchecked(p).to_owned() };
        let info = msg.lines().next().unwrap_or("").to_owned();

        ParseResult::Err(ParseError { kind, info })
      },
      NomErr::NodePosition(kind, p, trace) => {
        let p_msg = unsafe { from_utf8_unchecked(p) };
        let info = format!("{}: {:#?}", p_msg, trace);

        ParseResult::Err(ParseError { kind, info })
      }
    },
    IResult::Incomplete(n) => ParseResult::Incomplete(n)
  }
}

/// Run a parser over a `str`.
pub fn parse_str<'a, I, P, T>(source: I, parser: P) -> ParseResult<T>
    where I: Into<&'a str>,
          P: FnOnce(&[u8]) -> IResult<&[u8], T> {
  parse(source.into().as_bytes(), parser)
}

