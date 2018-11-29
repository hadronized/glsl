extern crate glsl;

use glsl::parser::{Parse, ParseResult};
use glsl::syntax::TranslationUnit;

#[test]
fn missing_zero_float_is_valid() {
  let r = TranslationUnit::parse_str("
    void main() {
      float x = 1. * .5;
    }");

  assert!(r.is_ok());
}
