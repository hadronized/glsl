//! Various nom parser helpers.

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, line_ending, multispace1};
use nom::combinator::{map, recognize, value};
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::fold_many0;
use nom::{Err as NomErr, IResult};

use nom::{AsBytes, InputLength, Slice};

use super::ParseInput;

pub type ParserResult<'c, 'd, 'e, O> =
  IResult<ParseInput<'c, 'd, 'e>, O, VerboseError<ParseInput<'c, 'd, 'e>>>;

// A constant parser that just forwards the value it’s parametered with without reading anything
// from the input. Especially useful as “fallback” in an alternative parser.
pub fn cnst<'c, 'd, 'e, T, E>(
  t: T,
) -> impl FnMut(ParseInput<'c, 'd, 'e>) -> Result<(ParseInput<'c, 'd, 'e>, T), E>
where
  'c: 'd + 'e,
  'd: 'e,
  T: 'c + Clone,
{
  move |i| Ok((i, t.clone()))
}

// End-of-input parser.
//
// Yields `()` if the parser is at the end of the input; an error otherwise.
pub fn eoi<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  if i.input_len() == 0 {
    Ok((i, ()))
  } else {
    Err(NomErr::Error(VerboseError {
      errors: vec![(i, VerboseErrorKind::Nom(ErrorKind::Eof))],
    }))
  }
}

// A newline parser that accepts:
//
// - A newline.
// - The end of input.
pub fn eol<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  alt((
    eoi, // this one goes first because it’s very cheap
    value((), line_ending),
  ))(i)
}

// Apply the `f` parser until `g` succeeds. Both parsers consume the input.
pub fn till<'c, 'd, 'e, A, B, F, G>(
  mut f: F,
  mut g: G,
) -> impl FnMut(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()>
where
  'c: 'd + 'e,
  'd: 'e,
  F: FnMut(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, A>,
  G: FnMut(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, B>,
{
  move |mut i| loop {
    if let Ok((i2, _)) = g(i) {
      break Ok((i2, ()));
    }

    let (i2, _) = f(i)?;
    i = i2;
  }
}

// A version of many0 that discards the result of the parser, preventing allocating.
pub fn many0_<'c, 'd, 'e, A, F>(
  mut f: F,
) -> impl FnMut(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()>
where
  'c: 'd + 'e,
  'd: 'e,
  F: FnMut(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, A>,
{
  move |i| fold_many0(&mut f, || (), |_, _| ())(i)
}

/// Parse a string until the end of line.
///
/// This parser accepts the multiline annotation (\) to break the string on several lines.
///
/// Discard any leading newline.
pub fn str_till_eol<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  map(
    recognize(till(alt((value((), tag("\\\n")), value((), anychar))), eol)),
    |i| {
      if i.as_bytes().last() == Some(&b'\n') {
        i.slice(0..i.input_len() - 1)
      } else {
        i
      }
    },
  )(i)
}

// Blank base parser.
//
// This parser succeeds with multispaces and multiline annotation.
//
// Taylor Swift loves it.
pub fn blank_space<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  recognize(many0_(alt((multispace1, tag("\\\n")))))(i)
}
