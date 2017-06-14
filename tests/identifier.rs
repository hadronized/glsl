extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;

#[test]
fn parse_identifier() {
  assert_eq!(parser::identifier(&b"a"[..]), IResult::Done(&b""[..], "a".to_owned()));
  assert_eq!(parser::identifier(&b"ab_cd"[..]), IResult::Done(&b""[..], "ab_cd".to_owned()));
  assert_eq!(parser::identifier(&b"Ab_cd"[..]), IResult::Done(&b""[..], "Ab_cd".to_owned()));
  assert_eq!(parser::identifier(&b"Ab_c8d"[..]), IResult::Done(&b""[..], "Ab_c8d".to_owned()));
  assert_eq!(parser::identifier(&b"Ab_c8d9"[..]), IResult::Done(&b""[..], "Ab_c8d9".to_owned()));
  assert_eq!(parser::identifier(&b"0Ab_c8d9"[..]), IResult::Error(ErrorKind::Verify));
}
