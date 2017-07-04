extern crate glsl;
extern crate nom;

use nom::IResult;

use glsl::parser;
use glsl::syntax;

#[test]
fn parse_primary_expr_intconst() {
  assert_eq!(parser::primary_expr(&b"0 "[..]), IResult::Done(&b" "[..], syntax::Expr::IntConst("0".to_owned())));
  assert_eq!(parser::primary_expr(&b"1 "[..]), IResult::Done(&b" "[..], syntax::Expr::IntConst("1".to_owned())));
}

#[test]
fn parse_primary_expr_uintconst() {
  assert_eq!(parser::primary_expr(&b"0u "[..]), IResult::Done(&b" "[..], syntax::Expr::UIntConst("0u".to_owned())));
  assert_eq!(parser::primary_expr(&b"1u "[..]), IResult::Done(&b" "[..], syntax::Expr::UIntConst("1u".to_owned())));
}

#[test]
fn parse_primary_expr_floatconst() {
  assert_eq!(parser::primary_expr(&b"0. "[..]), IResult::Done(&b" "[..], syntax::Expr::FloatConst("0.".to_owned())));
  assert_eq!(parser::primary_expr(&b"1. "[..]), IResult::Done(&b" "[..], syntax::Expr::FloatConst("1.".to_owned())));
}

#[test]
fn parse_primary_expr_doubleconst() {
  assert_eq!(parser::primary_expr(&b"0.lf "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst("0.lf".to_owned())));
  assert_eq!(parser::primary_expr(&b"1.lf "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst("1.lf".to_owned())));
}

#[test]
fn parse_primary_expr_boolconst() {
  assert_eq!(parser::primary_expr(&b"false "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst("false".to_owned())));
  assert_eq!(parser::primary_expr(&b"true "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst("true".to_owned())));
}
