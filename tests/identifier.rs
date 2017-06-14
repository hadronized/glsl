extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;

#[test]
fn parse_identifier() {
  assert_eq!(parser::identifier(&b"a"[..]), IResult::Done(&b""[..], &b"a"[..]));
  assert_eq!(parser::identifier(&b"ab_cd"[..]), IResult::Done(&b""[..], &b"ab_cd"[..]));
  assert_eq!(parser::identifier(&b"Ab_cd"[..]), IResult::Done(&b""[..], &b"Ab_cd"[..]));
  assert_eq!(parser::identifier(&b"Ab_c8d"[..]), IResult::Done(&b""[..], &b"Ab_c8d"[..]));
  assert_eq!(parser::identifier(&b"Ab_c8d9"[..]), IResult::Done(&b""[..], &b"Ab_c8d9"[..]));
  assert_eq!(parser::identifier(&b"0Ab_c8d9"[..]), IResult::Error(ErrorKind::Verify));
}
