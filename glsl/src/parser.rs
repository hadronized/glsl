//! GLSL parsing.
//!
//! This module gives you several functions and types to deal with GLSL parsing, transforming an
//! input source into an AST. The AST is defined in the [`syntax`] module.
//!
//! You want to use the [`Parse`]’s methods to get starting with parsing and pattern match on
//! [`ParserResult`].
//!
//! [`Parse`]: parser::Parse
//! [`ParserResult`]: parser::ParserResult

use nom::Err as NomErr;
use nom::error::{VerboseError, convert_error};
use std::fmt;
use std::str::from_utf8;

use crate::parsers::ParserResult;
use crate::syntax;

/// A parse error. It contains an [`ErrorKind`] along with a [`String`] giving information on the reason
/// why the parser failed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParseError {
  pub info: String
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    write!(f, "error: {}", self.info)
  }
}

/// Run a parser over a byte slice.
pub(crate) fn run_parser<P, T>(
  source: &[u8],
  parser: P
) -> Result<T, ParseError>
where P: FnOnce(&[u8]) -> ParserResult<&[u8], T> {
  match parser(source) {
    Ok((_, x)) => {
      Ok(x)
    }

    Err(e) => match e {
      NomErr::Incomplete(_) => {
        Err(ParseError { info: "incomplete parser".to_owned() })
      }

      NomErr::Error(err) | NomErr::Failure(err) => {
        let ve = str_verbose_error(err).ok_or(ParseError { info: "non-UTF-8".to_owned() })?;
        let s = from_utf8(source).map_err(|_| ParseError { info: "non-UTF-8".to_owned() })?;
        let info = convert_error(s, ve);
        Err(ParseError { info })
      }
    }
  }
}

// Transform a VerboseError<&[u8]> into a VerboseError<&str>, if possible.
fn str_verbose_error(ve: VerboseError<&[u8]>) -> Option<VerboseError<&str>> {
  let errors = ve.errors.into_iter()
    .map(|(i, kind)| from_utf8(i).map(|s| (s, kind)))
    .collect::<Result<_, _>>().ok()?;

  Some(VerboseError { errors })
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
  fn parse<B>(source: B) -> Result<Self, ParseError> where B: AsRef<[u8]>;

  /// Parse from a string.
  fn parse_str<S>(source: S) -> Result<Self, ParseError> where S: AsRef<str> {
    let s = source.as_ref().as_bytes();
    Self::parse(s)
  }
}

/// Macro to implement Parse for a given type.
macro_rules! impl_parse {
  ($type_name:ty, $parser_name:ident) => {
    impl Parse for $type_name {
      fn parse<B>(source: B) -> Result<Self, ParseError> where B: AsRef<[u8]> {
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
