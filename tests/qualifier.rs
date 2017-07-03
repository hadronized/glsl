extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;
use glsl::syntax;

#[test]
fn parse_precise_qualifier() {
  assert_eq!(parser::precise_qualifier(&b"precise"[..]), IResult::Done(&b""[..], ()));
}

#[test]
fn parse_invariant_qualifier() {
  assert_eq!(parser::invariant_qualifier(&b"invariant"[..]), IResult::Done(&b""[..], ()));
}

#[test]
fn parse_interpolation_qualifier() {
  assert_eq!(parser::interpolation_qualifier(&b"smooth"[..]), IResult::Done(&b""[..], syntax::InterpolationQualifier::Smooth));
  assert_eq!(parser::interpolation_qualifier(&b"flat"[..]), IResult::Done(&b""[..], syntax::InterpolationQualifier::Flat));
  assert_eq!(parser::interpolation_qualifier(&b"noperspective"[..]), IResult::Done(&b""[..], syntax::InterpolationQualifier::NoPerspective));
}

#[test]
fn parse_precision_qualifier() {
  assert_eq!(parser::precision_qualifier(&b"high"[..]), IResult::Done(&b""[..], syntax::PrecisionQualifier::High));
  assert_eq!(parser::precision_qualifier(&b"medium"[..]), IResult::Done(&b""[..], syntax::PrecisionQualifier::Medium));
  assert_eq!(parser::precision_qualifier(&b"low"[..]), IResult::Done(&b""[..], syntax::PrecisionQualifier::Low));
}

#[test]
fn parse_storage_qualifier() {
  assert_eq!(parser::storage_qualifier(&b"const"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Const));
  assert_eq!(parser::storage_qualifier(&b"inout"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::InOut));
  assert_eq!(parser::storage_qualifier(&b"in "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::In));
  assert_eq!(parser::storage_qualifier(&b"out"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Out));
  assert_eq!(parser::storage_qualifier(&b"centroid"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Centroid));
  assert_eq!(parser::storage_qualifier(&b"patch"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Patch));
  assert_eq!(parser::storage_qualifier(&b"sample"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Sample));
  assert_eq!(parser::storage_qualifier(&b"uniform"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Uniform));
  assert_eq!(parser::storage_qualifier(&b"buffer"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Buffer));
  assert_eq!(parser::storage_qualifier(&b"shared"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Shared));
  assert_eq!(parser::storage_qualifier(&b"coherent"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Coherent));
  assert_eq!(parser::storage_qualifier(&b"volatile"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Volatile));
  assert_eq!(parser::storage_qualifier(&b"restrict"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Restrict));
  assert_eq!(parser::storage_qualifier(&b"readonly"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::ReadOnly));
  assert_eq!(parser::storage_qualifier(&b"writeonly"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::WriteOnly));
  assert_eq!(parser::storage_qualifier(&b"subroutine a"[..]), IResult::Done(&b" a"[..], syntax::StorageQualifier::Subroutine(vec![])));

  let a = "vec3".to_owned();
  let b = "float".to_owned();
  let c = "dmat43".to_owned();
  let types = vec![a, b, c];
  assert_eq!(parser::storage_qualifier(&b"subroutine (vec3, float, dmat43)"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Subroutine(types)));
}

#[test]
fn parse_layout_qualifier_std430() {
  let expected = syntax::LayoutQualifier { ids: vec![syntax::LayoutQualifierSpec::Identifier("std430".to_owned(), None)] };

  assert_eq!(parser::layout_qualifier(&b"layout (std430)"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b" layout  (std430   )"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b" layout \n\t (  std430  )"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b" layout(std430)"[..]), IResult::Done(&b""[..], expected));
}

#[test]
fn parse_layout_qualifier_shared() {
  let expected = syntax::LayoutQualifier { ids: vec![syntax::LayoutQualifierSpec::Shared] };

  assert_eq!(parser::layout_qualifier(&b"layout (shared)"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b"   layout ( shared )"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b"   layout(shared)"[..]), IResult::Done(&b""[..], expected));
}

#[test]
fn parse_layout_qualifier_list() {
  let id_0 = syntax::LayoutQualifierSpec::Shared;
  let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".to_owned(), None);
  let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".to_owned(), Some(Box::new(syntax::Expr::IntConst("3".to_owned()))));
  let expected = syntax::LayoutQualifier { ids: vec![id_0, id_1, id_2] };

  assert_eq!(parser::layout_qualifier(&b"layout (shared, std140, max_vertices = 3)"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b"layout(shared,std140,max_vertices=3)"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::layout_qualifier(&b"   layout\n\n\t (    shared , std140, max_vertices= 3)"[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_type_qualifier() {
  let storage_qual = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Const);
  let id_0 = syntax::LayoutQualifierSpec::Shared;
  let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".to_owned(), None);
  let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".to_owned(), Some(Box::new(syntax::Expr::IntConst("3".to_owned()))));
  let layout_qual = syntax::TypeQualifierSpec::Layout(syntax::LayoutQualifier { ids: vec![id_0, id_1, id_2] });
  let expected = syntax::TypeQualifier { qualifiers: vec![storage_qual, layout_qual] };

  assert_eq!(parser::type_qualifier(&b"const layout (shared, std140, max_vertices = 3)"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::type_qualifier(&b"    const layout(shared,std140,max_vertices=3)"[..]), IResult::Done(&b""[..], expected));
}
