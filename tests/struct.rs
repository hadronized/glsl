extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;
use glsl::syntax;

#[test]
fn parse_struct_field() {
  let expected = syntax::StructField { ty: syntax::BasicTy::Vec4, identifiers: vec!["foo".to_owned()] };

  assert_eq!(parser::struct_field(&b"vec4 foo;"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_field(&b"  vec4     foo ; "[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_struct_field_several() {
  let expected = syntax::StructField { ty: syntax::BasicTy::Vec4, identifiers: vec!["foo".to_owned(), "bar".to_owned(), "zoo".to_owned()] };

  assert_eq!(parser::struct_field(&b"vec4 foo, bar, zoo;"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_field(&b"  vec4     foo , bar  , zoo ; "[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_struct_one_field() {
  let field = syntax::StructField { ty: syntax::BasicTy::Vec4, identifiers: vec!["foo".to_owned()] };
  let expected = syntax::Struct { name: Some("TestStruct".to_owned()), fields: vec![field] };

  assert_eq!(parser::struct_(&b"struct TestStruct { vec4 foo; }"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_(&b"   struct      TestStruct \n \n\n {\n    vec4   foo  ;\n }"[..]), IResult::Done(&b""[..], expected));
}

#[test]
fn parse_struct_multi_fields() {
  let a = syntax::StructField { ty: syntax::BasicTy::Vec4, identifiers: vec!["foo".to_owned()] };
  let b = syntax::StructField { ty: syntax::BasicTy::Float, identifiers: vec!["bar".to_owned()] };
  let c = syntax::StructField { ty: syntax::BasicTy::UInt, identifiers: vec!["zoo".to_owned()] };
  let d = syntax::StructField { ty: syntax::BasicTy::BVec3, identifiers: vec!["foo_BAR_zoo3497_34".to_owned()] };
  let expected = syntax::Struct { name: Some("_TestStruct_934i".to_owned()), fields: vec![a, b, c, d] };

  assert_eq!(parser::struct_(&b"struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; }"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_(&b"struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;}"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_(&b"   struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ;}"[..]), IResult::Done(&b""[..], expected));
}
