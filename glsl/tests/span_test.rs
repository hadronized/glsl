use glsl::parser::{Parse, ParseContextData};
use glsl::syntax::*;

#[test]
fn span_test() {
  let src = "// Get the target alpha value
float getAlpha() {
  // Fully-opaque value
  return 1.;
}

// Assign the output color
void main() {
  /* This is the color black */
  gl_FragColor = vec4(0., 0., 0., getAlpha());
}";

  // More test source to check source_id is used properly
  let src2 = "// Some extra comment
float someExtraFunction() {
  return 0.;
}";

  // Context data
  let mut data = ParseContextData::with_comments();

  // Parse source
  let tu = TranslationUnit::parse_with_context(src, &mut data).expect("failed to parse source");

  // Parse extra source
  let tu2 =
    TranslationUnit::parse_with_context(src2, &mut data).expect("failed to parse extra source");

  // For debugging, output AST and span information
  eprintln!("data: {:#?}", data);
  eprintln!("ast: {:#?}", tu);

  // Process a declaration
  let decl_cb = |start_span: &mut glsl::syntax::NodeSpan, decl: ExternalDeclaration| {
    // Look for the last comment before each declaration
    if let ExternalDeclarationData::FunctionDefinition(fndef) = &*decl {
      let span = decl.span.as_ref().unwrap();

      // Find all comments in spans between the start span and the declaration span
      let all_spans = data
        .comments()
        .unwrap()
        .range(&start_span.to_end_location()..span);

      // Get comments by their span id
      for (comment_span, cmt) in all_spans {
        assert!(comment_span < span);
        println!("comment for {}: {}", fndef.prototype.name.0, cmt.text(),);
      }

      *start_span = *span;
    }
  };

  // Do this for the first translation unit
  let mut start_span = glsl::syntax::NodeSpan::new_start(0);
  for decl in tu.0 {
    decl_cb(&mut start_span, decl);
  }

  // Do this for the second translation unit
  let mut start_span = glsl::syntax::NodeSpan::new_start(1);
  for decl in tu2.0 {
    decl_cb(&mut start_span, decl);
  }
}
