//! A GLSL450 writer that takes a syntax tree and writes it as a plain raw GLSL `String`.
//!
//! This module exports several functions that just transforms a part of a syntax tree into its raw
//! GLSL `String` representation.
//!
//! > Important note: this module – and actually, any `writer::*` module – is not responsible in
//! > optimizing the syntax tree nor semantically check its validity. This is done in other stages
//! > of the compilation process.
//!
//! In order to achieve that purpose, you could:
//!
//! - for each elements in the AST, return a `String` or `Cow<str>`;
//! - insert the string representation via a formatter.
//!
//! The second solution is better because it let the user handles the memory the way they want: they
//! might just use a dynamic buffer that implements `Write` or simply pass a `&mut String`. It’s up
//! to you.

use std::fmt::Write;
use syntax;

pub fn show_identifier<F>(f: &mut F, i: &syntax::Identifier) where F: Write {
  let _ = f.write_str(i);
}

pub fn show_type_name<F>(f: &mut F, t: &syntax::TypeName) where F: Write {
  let _ = f.write_str(t); 
}

pub fn show_type_specifier<F>(f: &mut F, t: &syntax::TypeSpecifier) where F: Write {
  match *t {
    syntax::TypeSpecifier::Void => { let _ = f.write_str("void"); }
    syntax::TypeSpecifier::Bool => { let _ = f.write_str("bool"); }
    syntax::TypeSpecifier::Int => { let _ = f.write_str("int"); }
    syntax::TypeSpecifier::UInt => { let _ = f.write_str("uint"); }
    syntax::TypeSpecifier::Float => { let _ = f.write_str("float"); }
    syntax::TypeSpecifier::Double => { let _ = f.write_str("double"); }
    syntax::TypeSpecifier::Vec2 => { let _ = f.write_str("vec2"); }
    syntax::TypeSpecifier::Vec3 => { let _ = f.write_str("vec3"); }
    syntax::TypeSpecifier::Vec4 => { let _ = f.write_str("vec4"); }
    syntax::TypeSpecifier::DVec2 => { let _ = f.write_str("dvec2"); }
    syntax::TypeSpecifier::DVec3 => { let _ = f.write_str("dvec3"); }
    syntax::TypeSpecifier::DVec4 => { let _ = f.write_str("dvec4"); }
    syntax::TypeSpecifier::BVec2 => { let _ = f.write_str("bvec2"); }
    syntax::TypeSpecifier::BVec3 => { let _ = f.write_str("bvec3"); }
    syntax::TypeSpecifier::BVec4 => { let _ = f.write_str("bvec4"); }
    syntax::TypeSpecifier::IVec2 => { let _ = f.write_str("idvec2"); }
    syntax::TypeSpecifier::IVec3 => { let _ = f.write_str("idvec3"); }
    syntax::TypeSpecifier::IVec4 => { let _ = f.write_str("idvec4"); }
    syntax::TypeSpecifier::UVec2 => { let _ = f.write_str("uvec2"); }
    syntax::TypeSpecifier::UVec3 => { let _ = f.write_str("uvec3"); }
    syntax::TypeSpecifier::UVec4 => { let _ = f.write_str("uvec4"); }
    syntax::TypeSpecifier::Mat2 => { let _ = f.write_str("mat2"); }
    syntax::TypeSpecifier::Mat3 => { let _ = f.write_str("mat3"); }
    syntax::TypeSpecifier::Mat4 => { let _ = f.write_str("mat4"); }
    syntax::TypeSpecifier::Mat23 => { let _ = f.write_str("mat23"); }
    syntax::TypeSpecifier::Mat24 => { let _ = f.write_str("mat24"); }
    syntax::TypeSpecifier::Mat32 => { let _ = f.write_str("mat32"); }
    syntax::TypeSpecifier::Mat34 => { let _ = f.write_str("mat34"); }
    syntax::TypeSpecifier::Mat42 => { let _ = f.write_str("mat42"); }
    syntax::TypeSpecifier::Mat43 => { let _ = f.write_str("mat43"); }
    syntax::TypeSpecifier::DMat2 => { let _ = f.write_str("dmat2"); }
    syntax::TypeSpecifier::DMat3 => { let _ = f.write_str("dmat3"); }
    syntax::TypeSpecifier::DMat4 => { let _ = f.write_str("dmat4"); }
    syntax::TypeSpecifier::DMat23 => { let _ = f.write_str("dmat23"); }
    syntax::TypeSpecifier::DMat24 => { let _ = f.write_str("dmat24"); }
    syntax::TypeSpecifier::DMat32 => { let _ = f.write_str("dmat32"); }
    syntax::TypeSpecifier::DMat34 => { let _ = f.write_str("dmat34"); }
    syntax::TypeSpecifier::DMat42 => { let _ = f.write_str("dmat42"); }
    syntax::TypeSpecifier::DMat43 => { let _ = f.write_str("dmat43"); }
    syntax::TypeSpecifier::Sampler1D => { let _ = f.write_str("sampler1D"); }
    syntax::TypeSpecifier::Image1D => { let _ = f.write_str("image1D"); }
    syntax::TypeSpecifier::Sampler2D => { let _ = f.write_str("sampler2D"); }
    syntax::TypeSpecifier::Image2D => { let _ = f.write_str("image2D"); }
    syntax::TypeSpecifier::Sampler3D => { let _ = f.write_str("sampler3D"); }
    syntax::TypeSpecifier::Image3D => { let _ = f.write_str("image3D"); }
    syntax::TypeSpecifier::SamplerCube => { let _ = f.write_str("samplerCube"); }
    syntax::TypeSpecifier::ImageCube => { let _ = f.write_str("imageCube"); }
    syntax::TypeSpecifier::Sampler2DRect => { let _ = f.write_str("sampler2DRect"); }
    syntax::TypeSpecifier::Image2DRect => { let _ = f.write_str("image2DRect"); }
    syntax::TypeSpecifier::Sampler1DArray => { let _ = f.write_str("sampler1DArray"); }
    syntax::TypeSpecifier::Image1DArray => { let _ = f.write_str("image1DArray"); }
    syntax::TypeSpecifier::Sampler2DArray => { let _ = f.write_str("sampler2DArray"); }
    syntax::TypeSpecifier::Image2DArray => { let _ = f.write_str("image2DArray"); }
    syntax::TypeSpecifier::SamplerBuffer => { let _ = f.write_str("samplerBuffer"); }
    syntax::TypeSpecifier::ImageBuffer => { let _ = f.write_str("imageBuffer"); }
    syntax::TypeSpecifier::Sampler2DMS => { let _ = f.write_str("sampler2DMS"); }
    syntax::TypeSpecifier::Image2DMS => { let _ = f.write_str("image2DMS"); }
    syntax::TypeSpecifier::Sampler2DMSArray => { let _ = f.write_str("sampler2DMSArray"); }
    syntax::TypeSpecifier::Image2DMSArray => { let _ = f.write_str("image2DMSArray"); }
    syntax::TypeSpecifier::SamplerCubeArray => { let _ = f.write_str("samplerCubeArray"); }
    syntax::TypeSpecifier::ImageCubeArray => { let _ = f.write_str("imageCubeArray"); }
    syntax::TypeSpecifier::Sampler1DShadow => { let _ = f.write_str("sampler1DShadow"); }
    syntax::TypeSpecifier::Sampler2DShadow => { let _ = f.write_str("sampler2DShadow"); }
    syntax::TypeSpecifier::Sampler2DRectShadow => { let _ = f.write_str("sampler2DRectShadow"); }
    syntax::TypeSpecifier::Sampler1DArrayShadow => { let _ = f.write_str("sampler1DArrayShadow"); }
    syntax::TypeSpecifier::Sampler2DArrayShadow => { let _ = f.write_str("sampler2DArrayShadow"); }
    syntax::TypeSpecifier::SamplerCubeShadow => { let _ = f.write_str("samplerCubeShadow"); }
    syntax::TypeSpecifier::SamplerCubeArrayShadow => { let _ = f.write_str("samplerCubeArrayShadow"); }
    syntax::TypeSpecifier::ISampler1D => { let _ = f.write_str("isampler1D"); }
    syntax::TypeSpecifier::IImage1D => { let _ = f.write_str("iimage1D"); }
    syntax::TypeSpecifier::ISampler2D => { let _ = f.write_str("isampler2D"); }
    syntax::TypeSpecifier::IImage2D => { let _ = f.write_str("iimage2D"); }
    syntax::TypeSpecifier::ISampler3D => { let _ = f.write_str("isampler3D"); }
    syntax::TypeSpecifier::IImage3D => { let _ = f.write_str("iimage3D"); }
    syntax::TypeSpecifier::ISamplerCube => { let _ = f.write_str("isamplerCube"); }
    syntax::TypeSpecifier::IImageCube => { let _ = f.write_str("iimageCube"); }
    syntax::TypeSpecifier::ISampler2DRect => { let _ = f.write_str("isampler2DRect"); }
    syntax::TypeSpecifier::IImage2DRect => { let _ = f.write_str("iimage2DRect"); }
    syntax::TypeSpecifier::ISampler1DArray => { let _ = f.write_str("isampler1DArray"); }
    syntax::TypeSpecifier::IImage1DArray => { let _ = f.write_str("iimage1DArray"); }
    syntax::TypeSpecifier::ISampler2DArray => { let _ = f.write_str("isampler2DArray"); }
    syntax::TypeSpecifier::IImage2DArray => { let _ = f.write_str("iimage2DArray"); }
    syntax::TypeSpecifier::ISamplerBuffer => { let _ = f.write_str("isamplerBuffer"); }
    syntax::TypeSpecifier::IImageBuffer => { let _ = f.write_str("iimageBuffer"); }
    syntax::TypeSpecifier::ISampler2DMS => { let _ = f.write_str("isampler2MS"); }
    syntax::TypeSpecifier::IImage2DMS => { let _ = f.write_str("iimage2DMS"); }
    syntax::TypeSpecifier::ISampler2DMSArray => { let _ = f.write_str("isampler2DMSArray"); }
    syntax::TypeSpecifier::IImage2DMSArray => { let _ = f.write_str("iimage2DMSArray"); }
    syntax::TypeSpecifier::ISamplerCubeArray => { let _ = f.write_str("isamplerCubeArray"); }
    syntax::TypeSpecifier::IImageCubeArray => { let _ = f.write_str("iimageCubeArray"); }
    syntax::TypeSpecifier::AtomicUInt => { let _ = f.write_str("atomic_uint"); }
    syntax::TypeSpecifier::USampler1D => { let _ = f.write_str("usampler1D"); }
    syntax::TypeSpecifier::UImage1D => { let _ = f.write_str("uimage1D"); }
    syntax::TypeSpecifier::USampler2D => { let _ = f.write_str("usampler2D"); }
    syntax::TypeSpecifier::UImage2D => { let _ = f.write_str("uimage2D"); }
    syntax::TypeSpecifier::USampler3D => { let _ = f.write_str("usampler3D"); }
    syntax::TypeSpecifier::UImage3D => { let _ = f.write_str("uimage3D"); }
    syntax::TypeSpecifier::USamplerCube => { let _ = f.write_str("usamplerCube"); }
    syntax::TypeSpecifier::UImageCube => { let _ = f.write_str("uimageCube"); }
    syntax::TypeSpecifier::USampler2DRect => { let _ = f.write_str("usampler2DRect"); }
    syntax::TypeSpecifier::UImage2DRect => { let _ = f.write_str("uimage2DRect"); }
    syntax::TypeSpecifier::USampler1DArray => { let _ = f.write_str("usampler1DArray"); }
    syntax::TypeSpecifier::UImage1DArray => { let _ = f.write_str("uimage1DArray"); }
    syntax::TypeSpecifier::USampler2DArray => { let _ = f.write_str("usampler2DArray"); }
    syntax::TypeSpecifier::UImage2DArray => { let _ = f.write_str("uimage2DArray"); }
    syntax::TypeSpecifier::USamplerBuffer => { let _ = f.write_str("usamplerBuffer"); }
    syntax::TypeSpecifier::UImageBuffer => { let _ = f.write_str("uimageBuffer"); }
    syntax::TypeSpecifier::USampler2DMS => { let _ = f.write_str("usampler2DMS"); }
    syntax::TypeSpecifier::UImage2DMS => { let _ = f.write_str("uimage2DMS"); }
    syntax::TypeSpecifier::USampler2DMSArray => { let _ = f.write_str("usamplerDMSArray"); }
    syntax::TypeSpecifier::UImage2DMSArray => { let _ = f.write_str("uimage2DMSArray"); }
    syntax::TypeSpecifier::USamplerCubeArray => { let _ = f.write_str("usamplerCubeArray"); }
    syntax::TypeSpecifier::UImageCubeArray => { let _ = f.write_str("uimageCubeArray"); }
    syntax::TypeSpecifier::Struct(ref s) => show_struct(f, s),
    syntax::TypeSpecifier::TypeName(ref tn) => show_type_name(f, tn)
  }
}

pub fn show_fully_specified_type<F>(f: &mut F, t: &syntax::FullySpecifiedType) where F: Write {
  if let Some(ref qual) = t.qualifier {
    show_type_qualifier(f, &qual);
    let _ = f.write_str(" ");
  }

  show_type_specifier(f, &t.ty);
}

pub fn show_struct<F>(f: &mut F, s: &syntax::StructSpecifier) where F: Write {
  let _ = f.write_str("struct ");

  if let Some(ref name) = s.name {
    let _ = write!(f, "{} ", name);
  }

  let _ = f.write_str("{\n");

  for field in &s.fields {
    show_struct_field(f, field);
  }

  let _ = f.write_str("}\n");
}

pub fn show_struct_field<F>(f: &mut F, field: &syntax::StructFieldSpecifier) where F: Write {
  if let Some(ref qual) = field.qualifier {
    show_type_qualifier(f, &qual);
    let _ = f.write_str(" ");
  }

  show_type_specifier(f, &field.ty);
  let _ = f.write_str(" ");

  // there’s at least one identifier
  let mut identifiers = field.identifiers.iter();
  let &(ref first_identifier, ref first_array_spec) = identifiers.next().unwrap();

  show_arrayed_identifier(f, (first_identifier, first_array_spec));

  // write the rest of the identifiers
  for &(ref identifier, ref array_spec) in identifiers {
    let _ = f.write_str(", ");
    show_arrayed_identifier(f, (identifier, array_spec));
  }

  let _ = f.write_str(";\n");
}

pub fn show_array_spec<F>(f: &mut F, a: &syntax::ArraySpecifier) where F: Write {
  match *a {
    syntax::ArraySpecifier::Unsized => { let _ = f.write_str("[]"); }
    syntax::ArraySpecifier::ExplicitlySized(ref e) => {
      let _ = f.write_str("[");
      show_expr(f, &e);
      let _ = f.write_str("]");
    }
  }
}

pub fn show_arrayed_identifier<F>(f: &mut F, a: (&syntax::Identifier, &Option<syntax::ArraySpecifier>)) where F: Write {
  let _ = write!(f, "{}", a.0);

  if let Some(ref arr_spec) = *a.1 {
    show_array_spec(f, arr_spec);
  }
}

pub fn show_type_qualifier<F>(f: &mut F, q: &syntax::TypeQualifier) where F: Write {
  let mut qualifiers = q.qualifiers.iter();
  let first = qualifiers.next().unwrap();

  show_type_qualifier_spec(f, first);

  for qual_spec in qualifiers {
    let _ = f.write_str(" ");
    show_type_qualifier_spec(f, qual_spec)
  }
}

pub fn show_type_qualifier_spec<F>(f: &mut F, q: &syntax::TypeQualifierSpec) where F: Write {
  match *q {
    syntax::TypeQualifierSpec::Storage(ref s) => show_storage_qualifier(f, &s),
    syntax::TypeQualifierSpec::Layout(ref l) => show_layout_qualifier(f, &l),
    syntax::TypeQualifierSpec::Precision(ref p) => show_precision_qualifier(f, &p),
    syntax::TypeQualifierSpec::Interpolation(ref i) => show_interpolation_qualifier(f, &i),
    syntax::TypeQualifierSpec::Invariant => { let _ = f.write_str("invariant"); },
    syntax::TypeQualifierSpec::Precise => { let _ = f.write_str("precise"); }
  }
}

pub fn show_storage_qualifier<F>(f: &mut F, q: &syntax::StorageQualifier) where F: Write {
  match *q {
    syntax::StorageQualifier::Const => { let _ = f.write_str("const"); }
    syntax::StorageQualifier::InOut => { let _ = f.write_str("inout"); }
    syntax::StorageQualifier::In => { let _ = f.write_str("in"); }
    syntax::StorageQualifier::Out => { let _ = f.write_str("out"); }
    syntax::StorageQualifier::Centroid => { let _ = f.write_str("centroid"); }
    syntax::StorageQualifier::Patch => { let _ = f.write_str("patch"); }
    syntax::StorageQualifier::Sample => { let _ = f.write_str("sample"); }
    syntax::StorageQualifier::Uniform => { let _ = f.write_str("uniform"); }
    syntax::StorageQualifier::Buffer => { let _ = f.write_str("buffer"); }
    syntax::StorageQualifier::Shared => { let _ = f.write_str("shared"); }
    syntax::StorageQualifier::Coherent => { let _ = f.write_str("coherent"); }
    syntax::StorageQualifier::Volatile => { let _ = f.write_str("volatile"); }
    syntax::StorageQualifier::Restrict => { let _ = f.write_str("restrict"); }
    syntax::StorageQualifier::ReadOnly => { let _ = f.write_str("readonly"); }
    syntax::StorageQualifier::WriteOnly => { let _ = f.write_str("writeonly"); }
    syntax::StorageQualifier::Subroutine(ref n) => show_subroutine(f, &n)
  }
}

pub fn show_subroutine<F>(f: &mut F, types: &Vec<syntax::TypeName>) where F: Write {
  let _ = f.write_str("subroutine");

  if !types.is_empty() {
    let _ = f.write_str("(");

    let mut types_iter = types.iter();
    let first = types_iter.next().unwrap();

    show_type_name(f, first);

    for type_name in types_iter {
      let _ = f.write_str(", ");
      show_type_name(f, type_name);
    }

    let _ = f.write_str(")");
  }
}

pub fn show_layout_qualifier<F>(f: &mut F, l: &syntax::LayoutQualifier) where F: Write {
  let mut qualifiers = l.ids.iter();
  let first = qualifiers.next().unwrap();

  let _ = f.write_str("layout (");
  show_layout_qualifier_spec(f, first);

  for qual_spec in qualifiers {
    let _ = f.write_str(", ");
    show_layout_qualifier_spec(f, qual_spec);
  }

  let _ = f.write_str(")");
}

pub fn show_layout_qualifier_spec<F>(f: &mut F, l: &syntax::LayoutQualifierSpec) where F: Write {
  match *l {
    syntax::LayoutQualifierSpec::Identifier(ref i, Some(ref e)) => {
      let _ = write!(f, "{} = ", i);
      show_expr(f, &e);
    }
    syntax::LayoutQualifierSpec::Identifier(ref i, None) => show_identifier(f, &i),
    syntax::LayoutQualifierSpec::Shared => { let _ = f.write_str("shared"); }
  }
}

pub fn show_precision_qualifier<F>(f: &mut F, p: &syntax::PrecisionQualifier) where F: Write {
  match *p {
    syntax::PrecisionQualifier::High => { let _ = f.write_str("highp"); }
    syntax::PrecisionQualifier::Medium => { let _ = f.write_str("mediump"); }
    syntax::PrecisionQualifier::Low => { let _ = f.write_str("low"); }
  }
}

pub fn show_interpolation_qualifier<F>(f: &mut F, i: &syntax::InterpolationQualifier) where F: Write {
  match *i {
    syntax::InterpolationQualifier::Smooth => { let _ = f.write_str("smooth"); }
    syntax::InterpolationQualifier::Flat => { let _ = f.write_str("flat"); }
    syntax::InterpolationQualifier::NoPerspective => { let _ = f.write_str("noperspective"); }
  }
}

// FIXME: better parens scheme, maybe?
pub fn show_expr<F>(f: &mut F, expr: &syntax::Expr) where F: Write {
  match *expr {
    syntax::Expr::Variable(ref i) => show_identifier(f, &i),
    syntax::Expr::IntConst(ref x) => { let _ = write!(f, "{}", x); }
    syntax::Expr::UIntConst(ref x) => { let _ = write!(f, "{}", x); }
    syntax::Expr::BoolConst(ref x) => { let _ = write!(f, "{}", x); }
    syntax::Expr::FloatConst(ref x) => { let _ = write!(f, "{}", x); }
    syntax::Expr::DoubleConst(ref x) => { let _ = write!(f, "{}", x); }
    syntax::Expr::Unary(ref op, ref e) => {
      show_unary_op(f, &op);
      let _ = f.write_str("(");
      show_expr(f, &e);
      let _ = f.write_str(")");
    }
    syntax::Expr::Binary(ref op, ref l, ref r) => {
      let _ = f.write_str("(");
      show_expr(f, &l);
      let _ = f.write_str(")");
      show_binary_op(f, &op);
      let _ = f.write_str("(");
      show_expr(f, &r);
      let _ = f.write_str(")");
    }
    syntax::Expr::Ternary(ref c, ref s, ref e) => {
      show_expr(f, &c);
      let _ = f.write_str(" ? ");
      show_expr(f, &s);
      let _ = f.write_str(" : ");
      show_expr(f, &e);
    }
    syntax::Expr::Assignment(ref v, ref op, ref e) => {
      show_expr(f, &v);
      let _ = f.write_str(" ");
      show_assignment_op(f, &op);
      let _ = f.write_str(" ");
      show_expr(f, &e);
    }
    syntax::Expr::Bracket(ref e, ref a) => {
      show_expr(f, &e);
      show_array_spec(f, &a);
    }
    syntax::Expr::FunCall(ref fun, ref args) => {
      show_function_identifier(f, &fun);
      let _ = f.write_str("(");
      
      if !args.is_empty() {
        let mut args_iter = args.iter();
        let first = args_iter.next().unwrap();
        show_expr(f, first);

        for e in args_iter {
          let _ = f.write_str(", ");
          show_expr(f, e);
        }
      }

      let _ = f.write_str(")");
    }
    syntax::Expr::Dot(ref e, ref i) => {
      show_expr(f, &e);
      let _ = f.write_str(".");
      show_identifier(f, &i);
    }
    syntax::Expr::PostInc(ref e) => {
      show_expr(f, &e);
      let _ = f.write_str("++");
    }
    syntax::Expr::PostDec(ref e) => {
      show_expr(f, &e);
      let _ = f.write_str("--");
    }
    syntax::Expr::Comma(ref a, ref b) => {
      show_expr(f, &a);
      let _ = f.write_str(", ");
      show_expr(f, &b);
    }
  }
}

pub fn show_unary_op<F>(f: &mut F, op: &syntax::UnaryOp) where F: Write {
  match *op {
    syntax::UnaryOp::Inc => { let _ = f.write_str("++"); }
    syntax::UnaryOp::Dec => { let _ = f.write_str("--"); }
    syntax::UnaryOp::Add => { let _ = f.write_str("+"); }
    syntax::UnaryOp::Minus => { let _ = f.write_str("-"); }
    syntax::UnaryOp::Not => { let _ = f.write_str("!"); }
    syntax::UnaryOp::Complement => { let _ = f.write_str("~"); }
  }
}

pub fn show_binary_op<F>(f: &mut F, op: &syntax::BinaryOp) where F: Write {
  match *op {
    syntax::BinaryOp::Or => { let _ = f.write_str("||"); }
    syntax::BinaryOp::Xor => { let _ = f.write_str("^^"); }
    syntax::BinaryOp::And => { let _ = f.write_str("&&"); }
    syntax::BinaryOp::BitOr => { let _ = f.write_str("|"); }
    syntax::BinaryOp::BitXor => { let _ = f.write_str("^"); }
    syntax::BinaryOp::BitAnd => { let _ = f.write_str("&"); }
    syntax::BinaryOp::Equal => { let _ = f.write_str("=="); }
    syntax::BinaryOp::NonEqual => { let _ = f.write_str("!="); }
    syntax::BinaryOp::LT => { let _ = f.write_str("<"); }
    syntax::BinaryOp::GT => { let _ = f.write_str(">"); }
    syntax::BinaryOp::LTE => { let _ = f.write_str("<="); }
    syntax::BinaryOp::GTE => { let _ = f.write_str(">="); }
    syntax::BinaryOp::LShift => { let _ = f.write_str("<<"); }
    syntax::BinaryOp::RShift => { let _ = f.write_str(">>"); }
    syntax::BinaryOp::Add => { let _ = f.write_str("+"); }
    syntax::BinaryOp::Sub => { let _ = f.write_str("-"); }
    syntax::BinaryOp::Mult => { let _ = f.write_str("*"); }
    syntax::BinaryOp::Div => { let _ = f.write_str("/"); }
    syntax::BinaryOp::Mod => { let _ = f.write_str("%"); }
  }
}

pub fn show_assignment_op<F>(f: &mut F, op: &syntax::AssignmentOp) where F: Write {
  match *op {
    syntax::AssignmentOp::Equal => { let _ = f.write_str("="); }
    syntax::AssignmentOp::Mult => { let _ = f.write_str("*="); }
    syntax::AssignmentOp::Div => { let _ = f.write_str("/="); }
    syntax::AssignmentOp::Mod => { let _ = f.write_str("%="); }
    syntax::AssignmentOp::Add => { let _ = f.write_str("+="); }
    syntax::AssignmentOp::Sub => { let _ = f.write_str("-="); }
    syntax::AssignmentOp::LShift => { let _ = f.write_str("<<="); }
    syntax::AssignmentOp::RShift => { let _ = f.write_str(">>="); }
    syntax::AssignmentOp::And => { let _ = f.write_str("&="); }
    syntax::AssignmentOp::Xor => { let _ = f.write_str("^="); }
    syntax::AssignmentOp::Or => { let _ = f.write_str("|="); }
  }
}

pub fn show_function_identifier<F>(f: &mut F, i: &syntax::FunIdentifier) where F: Write {
  match *i {
    syntax::FunIdentifier::Identifier(ref n) => show_identifier(f, &n)
  }
}

pub fn show_declaration<F>(f: &mut F, d: &syntax::Declaration) where F: Write {
  match *d {
    syntax::Declaration::FunctionPrototype(ref proto) => {
      show_function_prototype(f, &proto);
      let _ = f.write_str(";\n");
    }
    syntax::Declaration::InitDeclaratorList(ref list) => {
      show_init_declarator_list(f, &list);
      let _ = f.write_str(";\n");
    }
    syntax::Declaration::Precision(ref qual, ref ty) => {
      show_precision_qualifier(f, &qual);
      show_type_specifier(f, &ty);
      let _ = f.write_str(";\n");
    }
    syntax::Declaration::Block(ref block) => {
      show_block(f, &block);
      let _ = f.write_str(";\n");
    }
    syntax::Declaration::Global(ref qual, ref identifiers) => {
      show_type_qualifier(f, &qual);

      if !identifiers.is_empty() {
        let mut iter = identifiers.iter();
        let first = iter.next().unwrap();
        show_identifier(f, first);

        for identifier in iter {
          let _ = write!(f, ", {}", identifier);
        }
      }

      let _ = f.write_str(";\n");
    }
  }
}

pub fn show_function_prototype<F>(f: &mut F, fp: &syntax::FunctionPrototype) where F: Write {
  show_fully_specified_type(f, &fp.ty);
  let _ = f.write_str(" ");
  show_identifier(f, &fp.name);

  let _ = f.write_str("(");

  if !fp.parameters.is_empty() {
    let mut iter = fp.parameters.iter();
    let first = iter.next().unwrap();
    show_function_parameter_declaration(f, first);

    for param in iter {
      let _ = f.write_str(", ");
      show_function_parameter_declaration(f, param);
    }
  }

  let _ = f.write_str(")");
}
pub fn show_function_parameter_declaration<F>(f: &mut F, p: &syntax::FunctionParameterDeclaration) where F: Write {
  match *p {
    syntax::FunctionParameterDeclaration::Named(ref qual, ref fpd) => {
      if let Some(ref q) = *qual {
        show_type_qualifier(f, q);
        let _ = f.write_str(" ");
      }

      show_function_parameter_declarator(f, fpd);
    }
    syntax::FunctionParameterDeclaration::Unnamed(ref qual, ref ty) => {
      if let Some(ref q) = *qual {
        show_type_qualifier(f, q);
        let _ = f.write_str(" ");
      }

      show_type_specifier(f, ty);
    }
  }
}

pub fn show_function_parameter_declarator<F>(f: &mut F, p: &syntax::FunctionParameterDeclarator) where F: Write {
  show_type_specifier(f, &p.ty);
  let _ = f.write_str(" ");
  show_arrayed_identifier(f, (&p.name, &p.array_spec));
}

pub fn show_init_declarator_list<F>(f: &mut F, i: &syntax::InitDeclaratorList) where F: Write {
  show_single_declaration(f, &i.head);

  for decl in &i.tail {
    let _ = f.write_str(", ");
    show_single_declaration_no_type(f, decl);
  }
}

pub fn show_single_declaration<F>(f: &mut F, d: &syntax::SingleDeclaration) where F: Write {
  show_fully_specified_type(f, &d.ty);

  if let Some(ref name) = d.name {
    let _ = f.write_str(" ");
    show_identifier(f, name);
  }

  if let Some(ref arr_spec) = d.array_specifier {
    show_array_spec(f, arr_spec);
  }

  if let Some(ref initializer) = d.initializer {
    let _ = f.write_str(" = ");
    show_initializer(f, initializer);
  }
}

pub fn show_single_declaration_no_type<F>(f: &mut F, d: &syntax::SingleDeclarationNoType) where F: Write {
  show_arrayed_identifier(f, (&d.name, &d.array_specifier));

  if let Some(ref initializer) = d.initializer {
    let _ = f.write_str(" = ");
    show_initializer(f, initializer);
  }
}

pub fn show_initializer<F>(f: &mut F, i: &syntax::Initializer) where F: Write {
  match *i {
    syntax::Initializer::Simple(ref e) => show_expr(f, e),
    syntax::Initializer::List(ref list) => {
      let mut iter = list.iter();
      let first = iter.next().unwrap();

      let _ = f.write_str("{ ");
      show_initializer(f, first);

      for ini in iter {
        let _ = f.write_str(", ");
        show_initializer(f, ini);
      }

      let _ = f.write_str(" }");
    }
  }
}

pub fn show_block<F>(f: &mut F, b: &syntax::Block) where F: Write {
  show_type_qualifier(f, &b.qualifier);
  let _ = f.write_str(" ");
  show_identifier(f, &b.name);
  let _ = f.write_str(" {");

  for field in &b.fields {
    show_struct_field(f, field);
    let _ = f.write_str("\n");
  }

  let _ = f.write_str("}");
}

pub fn show_function_definition<F>(f: &mut F, fd: &syntax::FunctionDefinition) where F: Write {
  show_function_prototype(f, &fd.prototype);
  let _ = f.write_str(" ");
  show_compound_statement(f, &fd.statement);
}

pub fn show_compound_statement<F>(f: &mut F, cst: &syntax::CompoundStatement) where F: Write {
  let _ = f.write_str("{\n");

  for st in &cst.statement_list {
    show_statement(f, st);
  }

  let _ = f.write_str("}\n");
}

pub fn show_statement<F>(f: &mut F, st: &syntax::Statement) where F: Write {
  match *st {
    syntax::Statement::Compound(ref cst) => show_compound_statement(f, cst),
    syntax::Statement::Simple(ref sst) => show_simple_statement(f, sst)
  }
}

pub fn show_simple_statement<F>(f: &mut F, sst: &syntax::SimpleStatement) where F: Write {
  match *sst {
    syntax::SimpleStatement::Declaration(ref d) => show_declaration(f, d),
    syntax::SimpleStatement::Expression(ref e) => show_expression_statement(f, e),
    syntax::SimpleStatement::Selection(ref s) => show_selection_statement(f, s),
    syntax::SimpleStatement::Switch(ref s) => show_switch_statement(f, s),
    syntax::SimpleStatement::CaseLabel(ref cl) => show_case_label(f, cl),
    syntax::SimpleStatement::Iteration(ref i) => show_iteration_statement(f, i),
    syntax::SimpleStatement::Jump(ref j) => show_jump_statement(f, j)
  }
}

pub fn show_expression_statement<F>(f: &mut F, est: &syntax::ExprStatement) where F: Write {
  if let Some(ref e) = *est {
    show_expr(f, e);
  }

  let _ = f.write_str(";\n");
}

pub fn show_selection_statement<F>(f: &mut F, sst: &syntax::SelectionStatement) where F: Write {
  let _ = f.write_str("if (");
  show_expr(f, &sst.cond);
  let _= f.write_str(") {\n");
  show_selection_rest_statement(f, &sst.rest);
}

pub fn show_selection_rest_statement<F>(f: &mut F, sst: &syntax::SelectionRestStatement) where F: Write {
  match *sst {
    syntax::SelectionRestStatement::Statement(ref if_st) => {
      show_statement(f, if_st);
      let _ = f.write_str("}\n");
    }
    syntax::SelectionRestStatement::Else(ref if_st, ref else_st) => {
      show_statement(f, if_st);
      let _ = f.write_str("} else ");
      show_statement(f, else_st);
    }
  }
}

pub fn show_switch_statement<F>(f: &mut F, sst: &syntax::SwitchStatement) where F: Write {
  let _ = f.write_str("switch (");
  show_expr(f, &sst.head);
  let _ = f.write_str(") {\n");

  for st in &sst.body {
    show_statement(f, st);
  }

  let _ = f.write_str("}\n");
}

pub fn show_case_label<F>(f: &mut F, cl: &syntax::CaseLabel) where F: Write {
  match *cl {
    syntax::CaseLabel::Case(ref e) => {
      let _ = f.write_str("case ");
      show_expr(f, e);
      let _ = f.write_str(":\n");
    }
    syntax::CaseLabel::Def => { let _ = f.write_str("default:\n"); }
  }
}

pub fn show_iteration_statement<F>(f: &mut F, ist: &syntax::IterationStatement) where F: Write {
  match *ist {
    syntax::IterationStatement::While(ref cond, ref body) => {
      let _ = f.write_str("while (");
      show_condition(f, cond);
      let _ = f.write_str(") ");
      show_statement(f, body);
    }
    syntax::IterationStatement::DoWhile(ref body, ref cond) => {
      let _ = f.write_str("do ");
      show_statement(f, body);
      let _ = f.write_str(" while (");
      show_expr(f, cond);
      let _ = f.write_str(")\n");
    }
    syntax::IterationStatement::For(ref init, ref rest, ref body) => {
      let _ = f.write_str("for (");
      show_for_init_statement(f, init);
      show_for_rest_statement(f, rest);
      let _ = f.write_str(") ");
      show_statement(f, body);
    }
  }
}

pub fn show_condition<F>(f: &mut F, c: &syntax::Condition) where F: Write {
  match *c {
    syntax::Condition::Expr(ref e) => show_expr(f, e),
    syntax::Condition::Assignment(ref ty, ref name, ref initializer) => {
      show_fully_specified_type(f, ty);
      let _ = f.write_str(" ");
      show_identifier(f, name);
      let _ = f.write_str(" = ");
      show_initializer(f, initializer);
    }
  }
}

pub fn show_for_init_statement<F>(f: &mut F, i: &syntax::ForInitStatement) where F: Write {
  match *i {
    syntax::ForInitStatement::Expression(ref expr) => {
      if let Some(ref e) = *expr {
        show_expr(f, e);
      }
    }
    syntax::ForInitStatement::Declaration(ref d) => show_declaration(f, d)
  }
}

pub fn show_for_rest_statement<F>(f: &mut F, r: &syntax::ForRestStatement) where F: Write {
  if let Some(ref cond) = r.condition {
    show_condition(f, cond);
  }

  let _ = f.write_str("; ");

  if let Some(ref e) = r.post_expr {
    show_expr(f, e);
  }
}

pub fn show_jump_statement<F>(f: &mut F, j: &syntax::JumpStatement) where F: Write {
  match *j {
    syntax::JumpStatement::Continue => { let _ = f.write_str("continue;\n"); }
    syntax::JumpStatement::Break => { let _ = f.write_str("break;\n"); }
    syntax::JumpStatement::Discard => { let _ = f.write_str("discard;\n"); }
    syntax::JumpStatement::Return(ref e) => {
      let _ = f.write_str("return ");
      show_expr(f, e);
      let _ = f.write_str(";\n");
    }
  }
}

pub fn show_preprocessor<F>(f: &mut F, pp: &syntax::Preprocessor) where F: Write {
  match *pp {
    syntax::Preprocessor::Version(ref pv) => show_preprocessor_version(f, pv),
    syntax::Preprocessor::Extension(ref pe) => show_preprocessor_extension(f, pe)
  }
}

pub fn show_preprocessor_version<F>(f: &mut F, pv: &syntax::PreprocessorVersion) where F: Write {
  let _ = write!(f, "#version {}", pv.version);

  if let Some(ref profile) = pv.profile {
    match *profile {
      syntax::PreprocessorVersionProfile::Core => { let _ = f.write_str(" core"); }
      syntax::PreprocessorVersionProfile::Compatibility => { let _ = f.write_str(" compatibility"); }
      syntax::PreprocessorVersionProfile::ES => { let _ = f.write_str(" es"); }
    }
  }

  let _ = f.write_str("\n");
}

pub fn show_preprocessor_extension<F>(f: &mut F, pe: &syntax::PreprocessorExtension) where F: Write {
  let _ = f.write_str("#extension ");

  match pe.name {
    syntax::PreprocessorExtensionName::All => { let _ = f.write_str("all"); }
    syntax::PreprocessorExtensionName::Specific(ref n) => { let _ = f.write_str(n); }
  }

  if let Some(ref behavior) = pe.behavior {
    match *behavior {
      syntax::PreprocessorExtensionBehavior::Require => { let _ = f.write_str(" : require"); }
      syntax::PreprocessorExtensionBehavior::Enable => { let _ = f.write_str(" : enable"); }
      syntax::PreprocessorExtensionBehavior::Warn => { let _ = f.write_str(" : warn"); }
      syntax::PreprocessorExtensionBehavior::Disable => { let _ = f.write_str(" : disable"); }
    }
  }

  let _ = f.write_str("\n");
}

pub fn show_external_declaration<F>(f: &mut F, ed: &syntax::ExternalDeclaration) where F: Write {
  match *ed {
    syntax::ExternalDeclaration::Preprocessor(ref pp) => show_preprocessor(f, pp),
  syntax::ExternalDeclaration::FunctionDefinition(ref fd) => show_function_definition(f, fd),
  syntax::ExternalDeclaration::Declaration(ref d) => show_declaration(f, d)
  }
}

pub fn show_translation_unit<F>(f: &mut F, tu: &syntax::TranslationUnit) where F: Write {
  for ed in tu {
    show_external_declaration(f, ed);
  }
}
