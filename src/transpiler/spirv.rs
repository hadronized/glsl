//! SPIR-V transpiler.
//!
//! The current implementation uses the [shaderc](https://crates.io/crates/shaderc) crate to
//! transpile GLSL to SPIR-V. This is not ideal but will provide a default and starting
//! implementation.

use shaderc;
use std::fmt::Write;

use syntax;
use transpiler::glsl as glsl_transpiler;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShaderKind {
  TessControl,
  TessEvaluation,
  Vertex,
  Geometry,
  Fragment
}

impl From<ShaderKind> for shaderc::ShaderKind {
  fn from(kind: ShaderKind) -> Self {
    match kind {
      ShaderKind::TessControl => shaderc::ShaderKind::TessControl,
      ShaderKind::TessEvaluation => shaderc::ShaderKind::TessEvaluation,
      ShaderKind::Vertex => shaderc::ShaderKind::Vertex,
      ShaderKind::Geometry => shaderc::ShaderKind::Geometry,
      ShaderKind::Fragment => shaderc::ShaderKind::Fragment,
    }
  }
}

/// Transpile a GLSL AST into a SPIR-V internal buffer and write it to the given buffer.
///
/// The current implementation is highly inefficient as it relies on internal allocations and
/// [shaderc](https://crates.io/crates/shaderc).
///
/// If any error happens while transpiling, they’re returned as an opaque string.
pub fn transpile_translation_unit<F>(
  f: &mut F,
  tu: &syntax::TranslationUnit,
  kind: ShaderKind
) -> Result<(), String> where F: Write {
  // write as GLSL in an intermediate buffer
  let mut glsl_buffer = String::new();
  glsl_transpiler::show_translation_unit(&mut glsl_buffer, tu);

  // pass the GLSL-formatted string to shaderc
  let mut compiler = shaderc::Compiler::new().unwrap();
  let options = shaderc::CompileOptions::new().unwrap();
  let kind = kind.into();
  let output =
    compiler.compile_into_spirv_assembly(&glsl_buffer,
                                         kind,
                                         "glsl input",
                                         "main",
                                         Some(&options)
    ).map_err(|e| format!("{}", e))?;

  let _ = f.write_str(&output.as_text());

  Ok(())
}
