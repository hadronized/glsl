extern crate glsl;
extern crate nom;

use nom::{ErrorKind, IResult, Needed};
use glsl::parser;
use glsl::syntax;

#[test]
fn parse_struct_field_specifier() {
  let expected = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::Vec4), identifiers: vec!["foo".to_owned()] };

  assert_eq!(parser::struct_field_specifier(&b"vec4 foo;"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_field_specifier(&b"  vec4     foo ; "[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_struct_field_specifier_several() {
  let expected = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::Vec4), identifiers: vec!["foo".to_owned(), "bar".to_owned(), "zoo".to_owned()] };

  assert_eq!(parser::struct_field_specifier(&b"vec4 foo, bar, zoo;"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_field_specifier(&b"  vec4     foo , bar  , zoo ; "[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_struct_specifier_one_field() {
  let field = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::Vec4), identifiers: vec!["foo".to_owned()] };
  let expected = syntax::StructSpecifier { name: Some("TestStruct".to_owned()), fields: vec![field] };

  assert_eq!(parser::struct_specifier(&b"struct TestStruct { vec4 foo; }"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_specifier(&b"   struct      TestStruct \n \n\n {\n    vec4   foo  ;\n }"[..]), IResult::Done(&b""[..], expected));
}

#[test]
fn parse_struct_specifier_multi_fields() {
  let a = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::Vec4), identifiers: vec!["foo".to_owned()] };
  let b = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::Float), identifiers: vec!["bar".to_owned()] };
  let c = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::UInt), identifiers: vec!["zoo".to_owned()] };
  let d = syntax::StructFieldSpecifier { ty: syntax::Ty::BasicTy(syntax::BasicTy::BVec3), identifiers: vec!["foo_BAR_zoo3497_34".to_owned()] };
  let e = syntax::StructFieldSpecifier { ty: syntax::Ty::Struct("S0238_3".to_owned()), identifiers: vec!["x".to_owned()] };
  let expected = syntax::StructSpecifier { name: Some("_TestStruct_934i".to_owned()), fields: vec![a, b, c, d, e] };

  assert_eq!(parser::struct_specifier(&b"struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_specifier(&b"struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::struct_specifier(&b"   struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}"[..]), IResult::Done(&b""[..], expected));
}
