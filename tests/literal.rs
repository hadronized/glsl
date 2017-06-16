extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;

#[test]
fn parse_unsigned_suffix() {
  assert_eq!(parser::unsigned_suffix(&b"u"[..]), IResult::Done(&b""[..], 'u'));
  assert_eq!(parser::unsigned_suffix(&b"U"[..]), IResult::Done(&b""[..], 'U'));
}

#[test]
fn parse_nonzero_digit() {
  assert_eq!(parser::nonzero_digit(&b"3"[..]), IResult::Done(&b""[..], &b"3"[..]));
  assert_eq!(parser::nonzero_digit(&b"12345953"[..]), IResult::Done(&b""[..], &b"12345953"[..]));
  assert_eq!(parser::nonzero_digit(&b"03"[..]), IResult::Error(ErrorKind::Verify));
}

#[test]
fn parse_decimal_lit() {
  assert_eq!(parser::decimal_lit(&b"3"[..]), IResult::Done(&b""[..], &b"3"[..]));
  assert_eq!(parser::decimal_lit(&b"3 "[..]), IResult::Done(&b" "[..], &b"3"[..]));
  assert_eq!(parser::decimal_lit(&b"03"[..]), IResult::Error(ErrorKind::Verify));
}

#[test]
fn parse_octal_lit() {
  assert_eq!(parser::octal_lit(&b"3"[..]), IResult::Error(ErrorKind::Char));
  assert_eq!(parser::octal_lit(&b"03 "[..]), IResult::Done(&b" "[..], &b"03"[..]));
  assert_eq!(parser::octal_lit(&b"07654321234567 "[..]), IResult::Done(&b" "[..], &b"07654321234567"[..]));
  assert_eq!(parser::octal_lit(&b"07654321934567 "[..]), IResult::Error(ErrorKind::Verify));
}

#[test]
fn parse_hexadecimal_lit() {
  assert_eq!(parser::hexadecimal_lit(&b"3"[..]), IResult::Error(ErrorKind::Alt));
  assert_eq!(parser::hexadecimal_lit(&b"03"[..]), IResult::Error(ErrorKind::Alt));
  assert_eq!(parser::hexadecimal_lit(&b"0x3 "[..]), IResult::Done(&b" "[..], &b"0x3"[..]));
  assert_eq!(parser::hexadecimal_lit(&b"0x0123456789ABCDEF"[..]), IResult::Done(&b""[..], &b"0x0123456789ABCDEF"[..]));
  assert_eq!(parser::hexadecimal_lit(&b"0x0123456789abcdef"[..]), IResult::Done(&b""[..], &b"0x0123456789abcdef"[..]));
  assert_eq!(parser::hexadecimal_lit(&b"0x0123g456789abcdef"[..]), IResult::Error(ErrorKind::Verify));
}

#[test]
fn parse_integral_lit() {
  assert_eq!(parser::decimal_lit(&b"3"[..]), IResult::Done(&b""[..], &b"3"[..]));
  assert_eq!(parser::integral_lit(&b"3 "[..]), IResult::Done(&b" "[..], &b"3"[..]));
  assert_eq!(parser::integral_lit(&b"03 "[..]), IResult::Done(&b" "[..], &b"03"[..]));
  assert_eq!(parser::integral_lit(&b"07654321234567 "[..]), IResult::Done(&b" "[..], &b"07654321234567"[..]));
  assert_eq!(parser::integral_lit(&b"07654321934567 "[..]), IResult::Error(ErrorKind::Alt));
  assert_eq!(parser::integral_lit(&b"0x3 "[..]), IResult::Done(&b" "[..], &b"0x3"[..]));
  assert_eq!(parser::integral_lit(&b"0x0123456789ABCDEF"[..]), IResult::Done(&b""[..], &b"0x0123456789ABCDEF"[..]));
  assert_eq!(parser::integral_lit(&b"0x0123456789ABCDEF"[..]), IResult::Done(&b""[..], &b"0x0123456789ABCDEF"[..]));
  assert_eq!(parser::integral_lit(&b"0x0123456789abcdef"[..]), IResult::Done(&b""[..], &b"0x0123456789abcdef"[..]));
  assert_eq!(parser::integral_lit(&b"0x0123456789abcdef"[..]), IResult::Done(&b""[..], &b"0x0123456789abcdef"[..]));
  assert_eq!(parser::integral_lit(&b"0x0123g456789abcdef"[..]), IResult::Error(ErrorKind::Alt));
}

#[test]
fn parse_float_lit() {
  assert_eq!(parser::float_lit(&b"0"[..]), IResult::Incomplete(Needed::Size(2)));
  assert_eq!(parser::float_lit(&b"0."[..]), IResult::Incomplete(Needed::Unknown));
  assert_eq!(parser::float_lit(&b".0"[..]), IResult::Incomplete(Needed::Size(3)));
  assert_eq!(parser::float_lit(&b".035 "[..]), IResult::Done(&b" "[..], &b".035"[..]));
  assert_eq!(parser::float_lit(&b"0. "[..]), IResult::Done(&b" "[..], &b"0."[..]));
  assert_eq!(parser::float_lit(&b"0.035 "[..]), IResult::Done(&b" "[..], &b"0.035"[..]));
  assert_eq!(parser::float_lit(&b".035f"[..]), IResult::Done(&b""[..], &b".035f"[..]));
  assert_eq!(parser::float_lit(&b"0.f"[..]), IResult::Done(&b""[..], &b"0.f"[..]));
  assert_eq!(parser::float_lit(&b"0.035f"[..]), IResult::Done(&b""[..], &b"0.035f"[..]));
  assert_eq!(parser::float_lit(&b".035F"[..]), IResult::Done(&b""[..], &b".035F"[..]));
  assert_eq!(parser::float_lit(&b"0.F"[..]), IResult::Done(&b""[..], &b"0.F"[..]));
  assert_eq!(parser::float_lit(&b"0.035F"[..]), IResult::Done(&b""[..], &b"0.035F"[..]));
  assert_eq!(parser::float_lit(&b"1.03e+34 "[..]), IResult::Done(&b" "[..], &b"1.03e+34"[..]));
  assert_eq!(parser::float_lit(&b"1.03E+34 "[..]), IResult::Done(&b" "[..], &b"1.03E+34"[..]));
  assert_eq!(parser::float_lit(&b"1.03e-34 "[..]), IResult::Done(&b" "[..], &b"1.03e-34"[..]));
  assert_eq!(parser::float_lit(&b"1.03E-34 "[..]), IResult::Done(&b" "[..], &b"1.03E-34"[..]));
  assert_eq!(parser::float_lit(&b"1.03e+34f"[..]), IResult::Done(&b""[..], &b"1.03e+34f"[..]));
  assert_eq!(parser::float_lit(&b"1.03E+34f"[..]), IResult::Done(&b""[..], &b"1.03E+34f"[..]));
  assert_eq!(parser::float_lit(&b"1.03e-34f"[..]), IResult::Done(&b""[..], &b"1.03e-34f"[..]));
  assert_eq!(parser::float_lit(&b"1.03E-34f"[..]), IResult::Done(&b""[..], &b"1.03E-34f"[..]));
  assert_eq!(parser::float_lit(&b"1.03e+34F"[..]), IResult::Done(&b""[..], &b"1.03e+34F"[..]));
  assert_eq!(parser::float_lit(&b"1.03E+34F"[..]), IResult::Done(&b""[..], &b"1.03E+34F"[..]));
  assert_eq!(parser::float_lit(&b"1.03e-34F"[..]), IResult::Done(&b""[..], &b"1.03e-34F"[..]));
  assert_eq!(parser::float_lit(&b"1.03E-34F"[..]), IResult::Done(&b""[..], &b"1.03E-34F"[..]));
}

#[test]
fn parse_double_lit() {
  assert_eq!(parser::double_lit(&b"0"[..]), IResult::Incomplete(Needed::Size(2)));
  assert_eq!(parser::double_lit(&b"0."[..]), IResult::Incomplete(Needed::Unknown));
  assert_eq!(parser::double_lit(&b".0"[..]), IResult::Incomplete(Needed::Size(3)));
  assert_eq!(parser::double_lit(&b".035 "[..]), IResult::Done(&b" "[..], &b".035"[..]));
  assert_eq!(parser::double_lit(&b"0. "[..]), IResult::Done(&b" "[..], &b"0."[..]));
  assert_eq!(parser::double_lit(&b"0.035 "[..]), IResult::Done(&b" "[..], &b"0.035"[..]));
  assert_eq!(parser::double_lit(&b"0.lf"[..]), IResult::Done(&b""[..], &b"0.lf"[..]));
  assert_eq!(parser::double_lit(&b"0.035lf"[..]), IResult::Done(&b""[..], &b"0.035lf"[..]));
  assert_eq!(parser::double_lit(&b".035lf"[..]), IResult::Done(&b""[..], &b".035lf"[..]));
  assert_eq!(parser::double_lit(&b".035LF"[..]), IResult::Done(&b""[..], &b".035LF"[..]));
  assert_eq!(parser::double_lit(&b"0.LF"[..]), IResult::Done(&b""[..], &b"0.LF"[..]));
  assert_eq!(parser::double_lit(&b"0.035LF"[..]), IResult::Done(&b""[..], &b"0.035LF"[..]));
  assert_eq!(parser::double_lit(&b"1.03e+34lf"[..]), IResult::Done(&b""[..], &b"1.03e+34lf"[..]));
  assert_eq!(parser::double_lit(&b"1.03E+34lf"[..]), IResult::Done(&b""[..], &b"1.03E+34lf"[..]));
  assert_eq!(parser::double_lit(&b"1.03e-34lf"[..]), IResult::Done(&b""[..], &b"1.03e-34lf"[..]));
  assert_eq!(parser::double_lit(&b"1.03E-34lf"[..]), IResult::Done(&b""[..], &b"1.03E-34lf"[..]));
  assert_eq!(parser::double_lit(&b"1.03e+34LF"[..]), IResult::Done(&b""[..], &b"1.03e+34LF"[..]));
  assert_eq!(parser::double_lit(&b"1.03E+34LF"[..]), IResult::Done(&b""[..], &b"1.03E+34LF"[..]));
  assert_eq!(parser::double_lit(&b"1.03e-34LF"[..]), IResult::Done(&b""[..], &b"1.03e-34LF"[..]));
  assert_eq!(parser::double_lit(&b"1.03E-34LF"[..]), IResult::Done(&b""[..], &b"1.03E-34LF"[..]));
}
