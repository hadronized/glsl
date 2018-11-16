//! [pest](https://crates.io/crates/pest) parser.

use pest::Parser as PestParser;

use syntax;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct Parser;

/// Class of types that can be parsed.
pub trait Parse: Sized {
  /// Parse an item from a string.
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str>;
}

impl Parse for syntax::Identifier {
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str> {
    Parser::parse(Rule::identifier, input.into())
      .map(|p| p.as_str().to_owned())
      .map_err(|e| format!("{}", e))
  }
}

impl Parse for syntax::TypeSpecifierNonArray {
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str> {
    let pairs = Parser::parse(Rule::type_specifier_nonarray, input.into()).map_err(|e| format!("{}", e))?;

    eprintln!("{:?}", pairs);
    Ok(syntax::TypeSpecifierNonArray::Void)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_identifier() {
    assert_eq!(<syntax::Identifier as Parse>::parse("a").unwrap().as_str(), "a");
    assert_eq!(<syntax::Identifier as Parse>::parse("ab_cd").unwrap().as_str(), "ab_cd");
    assert_eq!(<syntax::Identifier as Parse>::parse("Ab_cd").unwrap().as_str(), "Ab_cd");
    assert_eq!(<syntax::Identifier as Parse>::parse("Ab_c8d").unwrap().as_str(), "Ab_c8d");
    assert_eq!(<syntax::Identifier as Parse>::parse("Ab_c8d9").unwrap().as_str(), "Ab_c8d9");
    assert!(<syntax::Identifier as Parse>::parse("3Ab_c8d9").is_err());
  }

  #[test]
  fn parse_type_specifier_non_array() {
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("intrteiret").unwrap(), syntax::TypeSpecifierNonArray::Int);
  }
}
