extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;
use glsl::syntax;

#[test]
fn parse_struct_field() {
  let expected = syntax::StructField { ty: syntax::BasicTy::Vec4, identifiers: vec!["foo".to_owned()] };

  assert_eq!(parser::struct_field(&b"vec4 foo"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_field(&b"  vec4     foo  "[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_struct_field_several() {
  let expected = syntax::StructField { ty: syntax::BasicTy::Vec4, identifiers: vec!["foo".to_owned(), "bar".to_owned(), "zoo".to_owned()] };

  assert_eq!(parser::struct_field(&b"vec4 foo, bar, zoo"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_field(&b"  vec4     foo , bar  , zoo "[..]), IResult::Done(&b""[..], expected.clone()));
}
