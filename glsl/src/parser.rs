//! GLSL parsing.
//!
//! This module gives you several functions and types to deal with GLSL parsing, transforming an
//! input source into an AST. The AST is defined in the [`syntax`] module.
//!
//! You want to use the [`Parse`]’s methods to get starting with parsing and pattern match on
//! [`ParseResult`].
//!
//! [`Parse`]: parser::Parse
//! [`ParseResult`]: parser::ParseResult

use nom::{Err as NomErr, IResult, Needed};
use std::fmt;
use std::str::{from_utf8_unchecked};

use syntax;

/// A parse error. It contains a`String` giving information on the reason why the parser failed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
  info: String
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "error: {}", self.info)
  }
}

/// Parse result. It can either be parsed, incomplete or errored.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseResult<T> {
  /// The source was successfully parsed.
  Ok(T),
  /// The parser failed with a [`ParseError`].
  Err(ParseError),
  /// More data is required to go on.
  Incomplete(Needed)
}

impl<T> ParseResult<T> {
  /// Returns true if the [`ParseResult`] is [`ParseResult::Ok`].
  pub fn is_ok(&self) -> bool {
    if let ParseResult::Ok(_) = *self {
      true
    } else {
      false
    }
  }

  /// Returns true if the [`ParseResult`] is [`ParseResult::Err`].
  pub fn is_err(&self) -> bool {
    if let ParseResult::Err(_) = *self {
      true
    } else {
      false
    }
  }

  /// Returns true if the [`ParseResult`] is [`ParseResult::Incomplete`].
  pub fn is_incomplete(&self) -> bool {
    if let ParseResult::Incomplete(_) = *self {
      true
    } else {
      false
    }
  }
}

/// Run a parser over a byte slice.
fn run_parser<P, T>(source: &[u8], parser: P) -> ParseResult<T>
    where P: FnOnce(&[u8]) -> IResult<&[u8], T> {
  match parser(source) {
    IResult::Done(i, x) => {
      if i.is_empty() {
        ParseResult::Ok(x)
      } else {
        let msg = unsafe { from_utf8_unchecked(i).to_owned() };
        let info = msg.lines().next().unwrap_or("").to_owned();
        ParseResult::Err(ParseError { info })
      }
    },
    IResult::Error(err) => match err {
      NomErr::Code(_) => ParseResult::Err(ParseError { info: String::new() }),
      NomErr::Node(_, trace) => {
        let info = format!("{:#?}", trace);
        ParseResult::Err(ParseError {  info })
      },
      NomErr::Position(_, p) => {
        let msg = unsafe { from_utf8_unchecked(p).to_owned() };
        let info = msg.lines().next().unwrap_or("").to_owned();

        ParseResult::Err(ParseError { info })
      },
      NomErr::NodePosition(_, p, trace) => {
        let p_msg = unsafe { from_utf8_unchecked(p) };
        let info = format!("{}: {:#?}", p_msg, trace);

        ParseResult::Err(ParseError { info })
      }
    },
    IResult::Incomplete(n) => ParseResult::Incomplete(n)
  }
}

/// Class of types that can be parsed.
///
/// This trait exposes two methods:
/// 
///   - [`Parse::parse`], that runs on bytes.
///   - [`Parse::parse_str`], a convenient function that runs on strings.
///
/// If you want to implement [`Parse`], only [`Parse::parse`] is mandatory – [`Parse::parse_str`]
/// has a default implementation using [`Parse::parse`].
///
/// The methods from this trait are the standard way to parse data into GLSL ASTs.
pub trait Parse: Sized {
  /// Parse from a byte slice.
  fn parse<B>(source: B) -> ParseResult<Self> where B: AsRef<[u8]>;

  /// Parse from a string.
  fn parse_str<S>(source: S) -> ParseResult<Self> where S: AsRef<str> {
    let s = source.as_ref().as_bytes();
    Self::parse(s)
  }
}

/// Macro to implement Parse for a given type.
macro_rules! impl_parse {
  ($type_name:ty, $parser_name:ident) => {
    impl Parse for $type_name {
      fn parse<B>(source: B) -> ParseResult<Self> where B: AsRef<[u8]> {
        run_parser(source.as_ref(), $crate::parsers::$parser_name)
      }
    }
  }
}

impl_parse!(syntax::Identifier, identifier);
impl_parse!(syntax::TypeSpecifierNonArray, type_specifier_non_array);
impl_parse!(syntax::TypeSpecifier, type_specifier);
impl_parse!(syntax::UnaryOp, unary_op);
impl_parse!(syntax::StructFieldSpecifier, struct_field_specifier);
impl_parse!(syntax::StructSpecifier, struct_specifier);
impl_parse!(syntax::StorageQualifier, storage_qualifier);
impl_parse!(syntax::LayoutQualifier, layout_qualifier);
impl_parse!(syntax::PrecisionQualifier, precision_qualifier);
impl_parse!(syntax::InterpolationQualifier, interpolation_qualifier);
impl_parse!(syntax::TypeQualifier, type_qualifier);
impl_parse!(syntax::TypeQualifierSpec, type_qualifier_spec);
impl_parse!(syntax::FullySpecifiedType, fully_specified_type);
impl_parse!(syntax::ArraySpecifier, array_specifier);
impl_parse!(syntax::Expr, expr);
impl_parse!(syntax::Declaration, declaration);
impl_parse!(syntax::FunctionPrototype, function_prototype);
impl_parse!(syntax::InitDeclaratorList, init_declarator_list);
impl_parse!(syntax::SingleDeclaration, single_declaration);
impl_parse!(syntax::Initializer, initializer);
impl_parse!(syntax::FunIdentifier, function_identifier);
impl_parse!(syntax::AssignmentOp, assignment_op);
impl_parse!(syntax::SimpleStatement, simple_statement);
impl_parse!(syntax::ExprStatement, expr_statement);
impl_parse!(syntax::SelectionStatement, selection_statement);
impl_parse!(syntax::SwitchStatement, switch_statement);
impl_parse!(syntax::CaseLabel, case_label);
impl_parse!(syntax::IterationStatement, iteration_statement);
impl_parse!(syntax::JumpStatement, jump_statement);
impl_parse!(syntax::Condition, condition);
impl_parse!(syntax::Statement, statement);
impl_parse!(syntax::CompoundStatement, compound_statement);
impl_parse!(syntax::FunctionDefinition, function_definition);
impl_parse!(syntax::ExternalDeclaration, external_declaration);
impl_parse!(syntax::TranslationUnit, translation_unit);
impl_parse!(syntax::Preprocessor, preprocessor);
impl_parse!(syntax::PreprocessorVersion, pp_version);
impl_parse!(syntax::PreprocessorVersionProfile, pp_version_profile);
impl_parse!(syntax::PreprocessorExtensionName, pp_extension_name);
impl_parse!(syntax::PreprocessorExtensionBehavior, pp_extension_behavior);
impl_parse!(syntax::PreprocessorExtension, pp_extension);

