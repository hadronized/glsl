extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;
use glsl::syntax;

#[test]
fn parse_precise_qualifier() {
  assert_eq!(parser::precise_qualifier(&b"precise"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Precise));
}

#[test]
fn parse_invariant_qualifier() {
  assert_eq!(parser::invariant_qualifier(&b"invariant"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Invariant));
}

#[test]
fn parse_interpolation_qualifier() {
  assert_eq!(parser::interpolation_qualifier(&b"smooth"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Interpolation(syntax::InterpolationQualifier::Smooth)));
  assert_eq!(parser::interpolation_qualifier(&b"flat"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Interpolation(syntax::InterpolationQualifier::Flat)));
  assert_eq!(parser::interpolation_qualifier(&b"noperspective"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Interpolation(syntax::InterpolationQualifier::NoPerspective)));
}

#[test]
fn parse_precision_qualifier() {
  assert_eq!(parser::precision_qualifier(&b"high"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Precision(syntax::PrecisionQualifier::High)));
  assert_eq!(parser::precision_qualifier(&b"medium"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Precision(syntax::PrecisionQualifier::Medium)));
  assert_eq!(parser::precision_qualifier(&b"low"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Precision(syntax::PrecisionQualifier::Low)));
}

#[test]
fn parse_storage_qualifier() {
  assert_eq!(parser::storage_qualifier(&b"const"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Const)));
  assert_eq!(parser::storage_qualifier(&b"inout"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::InOut)));
  assert_eq!(parser::storage_qualifier(&b"in "[..]), IResult::Done(&b" "[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::In)));
  assert_eq!(parser::storage_qualifier(&b"out"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Out)));
  assert_eq!(parser::storage_qualifier(&b"centroid"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Centroid)));
  assert_eq!(parser::storage_qualifier(&b"patch"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Patch)));
  assert_eq!(parser::storage_qualifier(&b"sample"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Sample)));
  assert_eq!(parser::storage_qualifier(&b"uniform"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Uniform)));
  assert_eq!(parser::storage_qualifier(&b"buffer"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Buffer)));
  assert_eq!(parser::storage_qualifier(&b"shared"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Shared)));
  assert_eq!(parser::storage_qualifier(&b"coherent"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Coherent)));
  assert_eq!(parser::storage_qualifier(&b"volatile"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Volatile)));
  assert_eq!(parser::storage_qualifier(&b"restrict"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Restrict)));
  assert_eq!(parser::storage_qualifier(&b"readonly"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::ReadOnly)));
  assert_eq!(parser::storage_qualifier(&b"writeonly"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::WriteOnly)));
  assert_eq!(parser::storage_qualifier(&b"subroutine a"[..]), IResult::Done(&b" a"[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Subroutine(vec![]))));

  let a = "vec3".to_owned();
  let b = "float".to_owned();
  let c = "dmat43".to_owned();
  let types = vec![a, b, c];
  assert_eq!(parser::storage_qualifier(&b"subroutine (vec3, float, dmat43)"[..]), IResult::Done(&b""[..], syntax::TypeQualifier::Storage(syntax::StorageQualifier::Subroutine(types))));
}
