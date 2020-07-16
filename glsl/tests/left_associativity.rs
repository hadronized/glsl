use glsl::{assert_ceq, parser::Parse, syntax};

#[test]
fn left_associativity() {
  for (opstr, opname) in [
    ("+", syntax::BinaryOp::Add),
    ("&&", syntax::BinaryOp::And),
    ("||", syntax::BinaryOp::Or),
  ]
  .iter()
  {
    let s = format!(
      "void main() {{
        x = a {op} b {op} c;
      }}",
      op = opstr
    );

    let r = syntax::TranslationUnit::parse(&s);

    let expected = syntax::TranslationUnit::from_non_empty_iter(vec![syntax::Node {
      contents: syntax::ExternalDeclarationData::FunctionDefinition(
        syntax::FunctionDefinitionData {
          prototype: syntax::FunctionPrototypeData {
            ty: syntax::FullySpecifiedType {
              qualifier: None,
              ty: syntax::TypeSpecifier {
                ty: syntax::TypeSpecifierNonArray::Void,
                array_specifier: None,
              },
            },
            name: "main".into(),
            parameters: Vec::new(),
          }
          .into(),
          statement: syntax::CompoundStatementData {
            statement_list: vec![syntax::Statement::Simple(Box::new(
              syntax::SimpleStatement::Expression(Some(syntax::Expr::Assignment(
                Box::new(syntax::Expr::Variable("x".into())),
                syntax::AssignmentOp::Equal,
                Box::new(syntax::Expr::Binary(
                  opname.clone(),
                  Box::new(syntax::Expr::Binary(
                    opname.clone(),
                    Box::new(syntax::Expr::Variable("a".into())),
                    Box::new(syntax::Expr::Variable("b".into())),
                  )),
                  Box::new(syntax::Expr::Variable("c".into())),
                )),
              ))),
            ))],
          }
          .into(),
        }
        .into(),
      ),
      span: None,
    }])
    .unwrap();

    assert_ceq!(r, Ok(expected));
  }
}
