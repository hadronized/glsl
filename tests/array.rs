extern crate glsl;
extern crate nom;

use nom::IResult;

use glsl::parser;
use glsl::syntax;

#[test]
fn parse_array_specifier_unsized() {
  assert_eq!(parser::array_specifier(&b"[]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::Unsized));
  assert_eq!(parser::array_specifier(&b"[ ]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::Unsized));
  assert_eq!(parser::array_specifier(&b"  [\n]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::Unsized));
}

#[test]
fn parse_array_specifier_sized() {
  let ix = syntax::Expr::IntConst("0".to_owned());
  assert_eq!(parser::array_specifier(&b"[0]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::ExplicitlySized(Box::new(ix.clone()))));
}
