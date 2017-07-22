// FIXME: general note on parsers:
//
//     E <- A | E, A
//
// This is a grammar that states that in order to successfully parse a E, we need to parse
// a A, and it’s possible to have several A. It’s the definition of a non-empty list.
//
// If we implement that with the following nom pseudo-code:
//
//    E = alt!(A, do_parse!(E >> char!(',') >> A))
//
// The semantic is not the same, as we will try to parse A first. Though, if it fails, it’ll
// try the second branch, which is… a recursive call to the same function. That will
// basically loop forever.
use nom::{ErrorKind, IResult, digit, sp};
use std::str::{from_utf8_unchecked};

use syntax;

/// Parse a single comment.
named!(pub comment,
  recognize!(alt!(
    ws!(preceded!(tag!("//"), take_until!("\n"))) |
    ws!(delimited!(tag!("/*"), take_until!("*/"), tag!("*/"))) |
    sp
  ))
);

/// Parse several comments.
named!(pub comments, recognize!(many0!(comment)));

/// Parser rewriter, discarding whitespaces and comments.
macro_rules! bl {
  ($i:expr, $($args:tt)*) => {{
    sep!($i, comments, $($args)*)
  }}
}

// Turn a &[u8] into a &str.
#[inline]
fn bytes_to_str(bytes: &[u8]) -> &str {
  unsafe { from_utf8_unchecked(bytes) }
}

// Turn a &[u8] into a String.
#[inline]
fn bytes_to_string(bytes: &[u8]) -> String {
  bytes_to_str(bytes).to_owned()
}

/// Parse an identifier (raw version).
named!(identifier_str,
  ws!(do_parse!(
    name: verify!(take_while1!(identifier_pred), verify_identifier) >>
    (name)
  ))
);

/// Parse an identifier.
named!(pub identifier<&[u8], syntax::Identifier>, map!(identifier_str, bytes_to_string));

#[inline]
fn identifier_pred(c: u8) -> bool {
  let ch = char::from(c);
  ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn verify_identifier(s: &[u8]) -> bool {
  !char::from(s[0]).is_digit(10)
}

/// Parse a non-empty list of identifiers, delimited by comma (,).
named!(nonempty_identifiers<&[u8], Vec<syntax::Identifier>>,
  ws!(do_parse!(
    first: identifier >>
    rest: many0!(do_parse!(char!(',') >> i: ws!(identifier) >> (i))) >>

    ({
      let mut identifiers = rest.clone();
      identifiers.insert(0, first);
      identifiers
    })
  ))
);

/// Parse a type specifier non struct.
pub fn type_specifier_non_struct(i: &[u8]) -> IResult<&[u8], syntax::TypeSpecifier> {
  let (i1, t) = try_parse!(i, identifier_str);

  match unsafe { from_utf8_unchecked(t) } {
    "bool" => IResult::Done(i1, syntax::TypeSpecifier::Bool),
    "int" => IResult::Done(i1, syntax::TypeSpecifier::Int),
    "uint" => IResult::Done(i1, syntax::TypeSpecifier::UInt),
    "float" => IResult::Done(i1, syntax::TypeSpecifier::Float),
    "double" => IResult::Done(i1, syntax::TypeSpecifier::Double),
    "vec2" => IResult::Done(i1, syntax::TypeSpecifier::Vec2),
    "vec3" => IResult::Done(i1, syntax::TypeSpecifier::Vec3),
    "vec4" => IResult::Done(i1, syntax::TypeSpecifier::Vec4),
    "dvec2" => IResult::Done(i1, syntax::TypeSpecifier::DVec2),
    "dvec3" => IResult::Done(i1, syntax::TypeSpecifier::DVec3),
    "dvec4" => IResult::Done(i1, syntax::TypeSpecifier::DVec4),
    "bvec2" => IResult::Done(i1, syntax::TypeSpecifier::BVec2),
    "bvec3" => IResult::Done(i1, syntax::TypeSpecifier::BVec3),
    "bvec4" => IResult::Done(i1, syntax::TypeSpecifier::BVec4),
    "ivec2" => IResult::Done(i1, syntax::TypeSpecifier::IVec2),
    "ivec3" => IResult::Done(i1, syntax::TypeSpecifier::IVec3),
    "ivec4" => IResult::Done(i1, syntax::TypeSpecifier::IVec4),
    "uvec2" => IResult::Done(i1, syntax::TypeSpecifier::UVec2),
    "uvec3" => IResult::Done(i1, syntax::TypeSpecifier::UVec3),
    "uvec4" => IResult::Done(i1, syntax::TypeSpecifier::UVec4),
    "mat2" => IResult::Done(i1, syntax::TypeSpecifier::Mat2),
    "mat3" => IResult::Done(i1, syntax::TypeSpecifier::Mat3),
    "mat4" => IResult::Done(i1, syntax::TypeSpecifier::Mat4),
    "mat2x2" => IResult::Done(i1, syntax::TypeSpecifier::Mat2),
    "mat2x3" => IResult::Done(i1, syntax::TypeSpecifier::Mat23),
    "mat2x4" => IResult::Done(i1, syntax::TypeSpecifier::Mat24),
    "mat3x2" => IResult::Done(i1, syntax::TypeSpecifier::Mat32),
    "mat3x3" => IResult::Done(i1, syntax::TypeSpecifier::Mat3),
    "mat3x4" => IResult::Done(i1, syntax::TypeSpecifier::Mat34),
    "mat4x2" => IResult::Done(i1, syntax::TypeSpecifier::Mat42),
    "mat4x3" => IResult::Done(i1, syntax::TypeSpecifier::Mat43),
    "mat4x4" => IResult::Done(i1, syntax::TypeSpecifier::Mat4),
    "dmat2" => IResult::Done(i1, syntax::TypeSpecifier::DMat2),
    "dmat3" => IResult::Done(i1, syntax::TypeSpecifier::DMat3),
    "dmat4" => IResult::Done(i1, syntax::TypeSpecifier::DMat4),
    "dmat2x2" => IResult::Done(i1, syntax::TypeSpecifier::DMat2),
    "dmat2x3" => IResult::Done(i1, syntax::TypeSpecifier::DMat23),
    "dmat2x4" => IResult::Done(i1, syntax::TypeSpecifier::DMat24),
    "dmat3x2" => IResult::Done(i1, syntax::TypeSpecifier::DMat32),
    "dmat3x3" => IResult::Done(i1, syntax::TypeSpecifier::DMat3),
    "dmat3x4" => IResult::Done(i1, syntax::TypeSpecifier::DMat34),
    "dmat4x2" => IResult::Done(i1, syntax::TypeSpecifier::DMat42),
    "dmat4x3" => IResult::Done(i1, syntax::TypeSpecifier::DMat43),
    "dmat4x4" => IResult::Done(i1, syntax::TypeSpecifier::DMat4),
    "sampler1D" => IResult::Done(i1, syntax::TypeSpecifier::Sampler1D),
    "image1D" => IResult::Done(i1, syntax::TypeSpecifier::Image1D),
    "sampler2D" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2D),
    "image2D" => IResult::Done(i1, syntax::TypeSpecifier::Image2D),
    "sampler3D" => IResult::Done(i1, syntax::TypeSpecifier::Sampler3D),
    "image3D" => IResult::Done(i1, syntax::TypeSpecifier::Image3D),
    "samplerCube" => IResult::Done(i1, syntax::TypeSpecifier::SamplerCube),
    "imageCube" => IResult::Done(i1, syntax::TypeSpecifier::ImageCube),
    "sampler2DRect" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DRect),
    "image2DRect" => IResult::Done(i1, syntax::TypeSpecifier::Image2DRect),
    "sampler1DArray" => IResult::Done(i1, syntax::TypeSpecifier::Sampler1DArray),
    "image1DArray" => IResult::Done(i1, syntax::TypeSpecifier::Image1DArray),
    "sampler2DArray" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DArray),
    "image2DArray" => IResult::Done(i1, syntax::TypeSpecifier::Image2DArray),
    "samplerBuffer" => IResult::Done(i1, syntax::TypeSpecifier::SamplerBuffer),
    "imageBuffer" => IResult::Done(i1, syntax::TypeSpecifier::ImageBuffer),
    "sampler2DMS" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DMS),
    "image2DMS" => IResult::Done(i1, syntax::TypeSpecifier::Image2DMS),
    "sampler2DMSArray" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DMSArray),
    "image2DMSArray" => IResult::Done(i1, syntax::TypeSpecifier::Image2DMSArray),
    "samplerCubeArray" => IResult::Done(i1, syntax::TypeSpecifier::SamplerCubeArray),
    "imageCubeArray" => IResult::Done(i1, syntax::TypeSpecifier::ImageCubeArray),
    "sampler1DShadow" => IResult::Done(i1, syntax::TypeSpecifier::Sampler1DShadow),
    "sampler2DShadow" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DShadow),
    "sampler2DRectShadow" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DRectShadow),
    "sampler1DArrayShadow" => IResult::Done(i1, syntax::TypeSpecifier::Sampler1DArrayShadow),
    "sampler2DArrayShadow" => IResult::Done(i1, syntax::TypeSpecifier::Sampler2DArrayShadow),
    "samplerCubeShadow" => IResult::Done(i1, syntax::TypeSpecifier::SamplerCubeShadow),
    "samplerCubeArrayShadow" => IResult::Done(i1, syntax::TypeSpecifier::SamplerCubeArrayShadow),
    "isampler1D" => IResult::Done(i1, syntax::TypeSpecifier::ISampler1D),
    "iimage1D" => IResult::Done(i1, syntax::TypeSpecifier::IImage1D),
    "isampler2D" => IResult::Done(i1, syntax::TypeSpecifier::ISampler2D),
    "iimage2D" => IResult::Done(i1, syntax::TypeSpecifier::IImage2D),
    "isampler3D" => IResult::Done(i1, syntax::TypeSpecifier::ISampler3D),
    "iimage3D" => IResult::Done(i1, syntax::TypeSpecifier::IImage3D),
    "isamplerCube" => IResult::Done(i1, syntax::TypeSpecifier::ISamplerCube),
    "iimageCube" => IResult::Done(i1, syntax::TypeSpecifier::IImageCube),
    "isampler2DRect" => IResult::Done(i1, syntax::TypeSpecifier::ISampler2DRect),
    "iimage2DRect" => IResult::Done(i1, syntax::TypeSpecifier::IImage2DRect),
    "isampler1DArray" => IResult::Done(i1, syntax::TypeSpecifier::ISampler1DArray),
    "iimage1DArray" => IResult::Done(i1, syntax::TypeSpecifier::IImage1DArray),
    "isampler2DArray" => IResult::Done(i1, syntax::TypeSpecifier::ISampler2DArray),
    "iimage2DArray" => IResult::Done(i1, syntax::TypeSpecifier::IImage2DArray),
    "isamplerBuffer" => IResult::Done(i1, syntax::TypeSpecifier::ISamplerBuffer),
    "iimageBuffer" => IResult::Done(i1, syntax::TypeSpecifier::IImageBuffer),
    "isampler2DMS" => IResult::Done(i1, syntax::TypeSpecifier::ISampler2DMS),
    "iimage2DMS" => IResult::Done(i1, syntax::TypeSpecifier::IImage2DMS),
    "isampler2DMSArray" => IResult::Done(i1, syntax::TypeSpecifier::ISampler2DMSArray),
    "iimage2DMSArray" => IResult::Done(i1, syntax::TypeSpecifier::IImage2DMSArray),
    "isamplerCubeArray" => IResult::Done(i1, syntax::TypeSpecifier::ISamplerCubeArray),
    "iimageCubeArray" => IResult::Done(i1, syntax::TypeSpecifier::IImageCubeArray),
    "atomic_uint" => IResult::Done(i1, syntax::TypeSpecifier::AtomicUInt),
    "usampler1D" => IResult::Done(i1, syntax::TypeSpecifier::USampler1D),
    "uimage1D" => IResult::Done(i1, syntax::TypeSpecifier::UImage1D),
    "usampler2D" => IResult::Done(i1, syntax::TypeSpecifier::USampler2D),
    "uimage2D" => IResult::Done(i1, syntax::TypeSpecifier::UImage2D),
    "usampler3D" => IResult::Done(i1, syntax::TypeSpecifier::USampler3D),
    "uimage3D" => IResult::Done(i1, syntax::TypeSpecifier::UImage3D),
    "usamplerCube" => IResult::Done(i1, syntax::TypeSpecifier::USamplerCube),
    "uimageCube" => IResult::Done(i1, syntax::TypeSpecifier::UImageCube),
    "usampler2DRect" => IResult::Done(i1, syntax::TypeSpecifier::USampler2DRect),
    "uimage2DRect" => IResult::Done(i1, syntax::TypeSpecifier::UImage2DRect),
    "usampler1DArray" => IResult::Done(i1, syntax::TypeSpecifier::USampler1DArray),
    "uimage1DArray" => IResult::Done(i1, syntax::TypeSpecifier::UImage1DArray),
    "usampler2DArray" => IResult::Done(i1, syntax::TypeSpecifier::USampler2DArray),
    "uimage2DArray" => IResult::Done(i1, syntax::TypeSpecifier::UImage2DArray),
    "usamplerBuffer" => IResult::Done(i1, syntax::TypeSpecifier::USamplerBuffer),
    "uimageBuffer" => IResult::Done(i1, syntax::TypeSpecifier::UImageBuffer),
    "usampler2DMS" => IResult::Done(i1, syntax::TypeSpecifier::USampler2DMS),
    "uimage2DMS" => IResult::Done(i1, syntax::TypeSpecifier::UImage2DMS),
    "usampler2DMSArray" => IResult::Done(i1, syntax::TypeSpecifier::USampler2DMSArray),
    "uimage2DMSArray" => IResult::Done(i1, syntax::TypeSpecifier::UImage2DMSArray),
    "usamplerCubeArray" => IResult::Done(i1, syntax::TypeSpecifier::USamplerCubeArray),
    "uimageCubeArray" => IResult::Done(i1, syntax::TypeSpecifier::UImageCubeArray),
    _ => IResult::Error(ErrorKind::AlphaNumeric)
  }
}

/// Parse a type specifier.
named!(pub type_specifier<&[u8], syntax::TypeSpecifier>,
  alt!(
    type_specifier_non_struct |
    map!(struct_specifier, syntax::TypeSpecifier::Struct) |
    map!(identifier, syntax::TypeSpecifier::TypeName)
  )
);

/// Parse the void type.
named!(pub void<&[u8], ()>, value!((), tag!("void")));

/// Parse a digit that precludes a leading 0.
named!(nonzero_digit, verify!(digit, |s:&[u8]| s[0] != b'0'));

/// Parse a decimal literal string.
named!(decimal_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    nonzero_digit >>
    (())
  )
);

/// Parse a decimal literal.
named!(decimal_lit, recognize!(decimal_lit_));

#[inline]
fn is_octal(s: &[u8]) -> bool {
  s[0] == b'0' && s.iter().all(|&c| c >= b'0' && c <= b'7')
}

/// Parse an octal literal string.
named!(octal_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    verify!(digit, is_octal) >>
    (())
  )
);

/// Parse an octal literal.
named!(octal_lit, recognize!(octal_lit_));

#[inline]
fn all_hexa(s: &[u8]) -> bool {
  s.iter().all(|&c| c >= b'0' && c <= b'9' || c >= b'a' && c <= b'f' || c >= b'A' && c <= b'F')
}

#[inline]
fn alphanumeric_no_u(c: u8) -> bool {
  char::from(c).is_alphanumeric() && c != b'u' && c != b'U'
}

/// Parse an hexadecimal literal string.
named!(hexadecimal_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    alt!(tag!("0x") | tag!("0X")) >>
    verify!(take_while1!(alphanumeric_no_u), all_hexa) >>
    (())
  )
);

/// Parse an hexadecimal literal.
named!(hexadecimal_lit, recognize!(hexadecimal_lit_));

named!(integral_lit_,
  alt!(
    hexadecimal_lit |
    octal_lit |
    decimal_lit
  )
);

/// Parse a literal integral string.
named!(pub integral_lit<&[u8], i32>,
  do_parse!(
    i: integral_lit_ >>
    ({
      if i.len() > 2 {
        if i[0] == b'-' {
          let i_ = &i[1..];

          if i_.starts_with(b"0x") | i_.starts_with(b"0X") {
            -i32::from_str_radix(bytes_to_str(&i_[2..]), 16).unwrap()
          } else {
            bytes_to_str(i).parse::<i32>().unwrap()
          }
        } else if i.starts_with(b"0x") | i.starts_with(b"0X") {
          i32::from_str_radix(bytes_to_str(&i[2..]), 16).unwrap()
        } else {
          bytes_to_str(i).parse::<i32>().unwrap()
        }
      } else {
        bytes_to_str(i).parse::<i32>().unwrap()
      }
    })
  )
);

/// Parse the unsigned suffix.
named!(unsigned_suffix<&[u8], char>, alt!(char!('u') | char!('U')));

/// Parse a literal unsigned string.
named!(pub unsigned_lit<&[u8], u32>,
  do_parse!(
    i: integral_lit_ >>
    unsigned_suffix >>
    ({
      if i.len() > 2 {
        if i[0] == b'-' {
          let i_ = &i[1..];

          if i_.starts_with(b"0x") | i_.starts_with(b"0X") {
            u32::wrapping_sub(0, u32::from_str_radix(bytes_to_str(&i_[2..]), 16).unwrap())
          } else {
            bytes_to_str(i).parse::<u32>().unwrap()
          }
        } else if i.starts_with(b"0x") | i.starts_with(b"0X") {
          u32::from_str_radix(bytes_to_str(&i[2..]), 16).unwrap()
        } else {
          bytes_to_str(i).parse::<u32>().unwrap()
        }
      } else {
        bytes_to_str(i).parse::<u32>().unwrap()
      }
    })
  )
);

/// Parse a floating point suffix.
named!(float_suffix,
  alt!(
    tag!("f") |
    tag!("F")
  )
);

/// Parse a double point suffix.
named!(double_suffix,
  alt!(
    tag!("lf") |
    tag!("LF")
  )
);


/// Parse the exponent part of a floating point literal.
named!(floating_exponent<&[u8], ()>,
  do_parse!(
    alt!(char!('e') | char!('E')) >>
    opt!(alt!(char!('+') | char!('-'))) >>
    digit >>
    (())
  )
);

/// Parse the fractional constant part of a floating point literal.
named!(floating_frac<&[u8], ()>,
  alt!(
    do_parse!(char!('.') >> digit >> (())) |
    do_parse!(digit >> tag!(".") >> digit >> (())) |
    do_parse!(digit >> tag!(".") >> (()))
  )
);

/// Parse the « middle » part of a floating value – i.e. fractional and exponential parts.
named!(floating_middle, recognize!(preceded!(floating_frac, opt!(floating_exponent))));

/// Parse a float literal string.
named!(pub float_lit<&[u8], f32>,
  do_parse!(
    sign: ws!(opt!(char!('-'))) >>
    f: floating_middle >>
    opt!(float_suffix) >>

    ({
      // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
      let n = if f[0] == b'.' {
        let mut f_ = f.to_owned();
        f_.insert(0, b'0');

        bytes_to_str(&f_).parse::<f32>().unwrap()
      } else {
        bytes_to_str(f).parse().unwrap()
      };

      // handle the sign and return
      if sign.is_some() { -n } else { n }
    })
  )
);

/// Parse a double literal string.
named!(pub double_lit<&[u8], f64>,
  do_parse!(
    sign: ws!(opt!(char!('-'))) >>
    f: floating_middle >>
    not!(float_suffix) >> // prevent from parsing 3.f ("f", Double(3.)) while it should be ("", Float(3.))
    opt!(double_suffix) >>

    ({
      // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
      let n = if f[0] == b'.' {
        let mut f_ = f.to_owned();
        f_.insert(0, b'0');

        bytes_to_str(&f_).parse::<f64>().unwrap()
      } else {
        bytes_to_str(f).parse().unwrap()
      };

      // handle the sign and return
      if sign.is_some() { -n } else { n }
    })
  )
);

/// Parse a constant boolean.
named!(pub bool_lit<&[u8], bool>,
  alt!(
    value!(true, tag!("true")) |
    value!(false, tag!("false"))
  )
);

/// Parse a unary operator.
named!(pub unary_op<&[u8], syntax::UnaryOp>,
  alt!(
    value!(syntax::UnaryOp::Inc, tag!("++")) |
    value!(syntax::UnaryOp::Dec, tag!("--")) |
    value!(syntax::UnaryOp::Add, char!('+')) |
    value!(syntax::UnaryOp::Minus, char!('-')) |
    value!(syntax::UnaryOp::Not, char!('!')) |
    value!(syntax::UnaryOp::Complement, char!('~'))
  )
);

/// Parse a struct field declaration.
named!(pub struct_field_specifier<&[u8], syntax::StructFieldSpecifier>,
  ws!(do_parse!(
    ty: type_specifier >>
    identifiers: nonempty_identifiers >>
    char!(';') >>

    (syntax::StructFieldSpecifier { ty: ty, identifiers: identifiers })
  ))
);

/// Parse a struct.
named!(pub struct_specifier<&[u8], syntax::StructSpecifier>,
  ws!(do_parse!(
    tag!("struct") >>
    name: opt!(identifier) >>
    fields: delimited!(char!('{'), many1!(struct_field_specifier), char!('}')) >>
    (syntax::StructSpecifier { name: name, fields: fields })
  ))
);

/// Parse a storage qualifier subroutine rule with a list of type names.
named!(pub storage_qualifier_subroutine_list<&[u8], syntax::StorageQualifier>,
  ws!(do_parse!(
    tag!("subroutine") >>
    identifiers: delimited!(char!('('),
                            nonempty_identifiers,
                            char!(')')) >>
    (syntax::StorageQualifier::Subroutine(identifiers))
  ))
);

/// Parse a storage qualifier subroutine rule.
named!(pub storage_qualifier_subroutine<&[u8], syntax::StorageQualifier>,
  alt!(
    storage_qualifier_subroutine_list |
    value!(syntax::StorageQualifier::Subroutine(Vec::new()), tag!("subroutine"))
  )
);

/// Parse a storage qualifier.
named!(pub storage_qualifier<&[u8], syntax::StorageQualifier>,
  alt!(
    value!(syntax::StorageQualifier::Const, tag!("const")) |
    value!(syntax::StorageQualifier::InOut, tag!("inout")) |
    value!(syntax::StorageQualifier::In, tag!("in")) |
    value!(syntax::StorageQualifier::Out, tag!("out")) |
    value!(syntax::StorageQualifier::Centroid, tag!("centroid")) |
    value!(syntax::StorageQualifier::Patch, tag!("patch")) |
    value!(syntax::StorageQualifier::Sample, tag!("sample")) |
    value!(syntax::StorageQualifier::Uniform, tag!("uniform")) |
    value!(syntax::StorageQualifier::Buffer, tag!("buffer")) |
    value!(syntax::StorageQualifier::Shared, tag!("shared")) |
    value!(syntax::StorageQualifier::Coherent, tag!("coherent")) |
    value!(syntax::StorageQualifier::Volatile, tag!("volatile")) |
    value!(syntax::StorageQualifier::Restrict, tag!("restrict")) |
    value!(syntax::StorageQualifier::ReadOnly, tag!("readonly")) |
    value!(syntax::StorageQualifier::WriteOnly, tag!("writeonly")) |
    storage_qualifier_subroutine
  )
);

/// Parse a layout qualifier.
named!(pub layout_qualifier<&[u8], syntax::LayoutQualifier>,
  ws!(do_parse!(
    tag!("layout") >>
    x: delimited!(char!('('), layout_qualifier_inner, char!(')')) >>
    (x)
  ))
);

named!(layout_qualifier_inner<&[u8], syntax::LayoutQualifier>,
  ws!(do_parse!(
    first: layout_qualifier_spec >>
    rest: many0!(do_parse!(char!(',') >> x: ws!(layout_qualifier_spec) >> (x))) >>

    ({
      let mut ids = rest.clone();
      ids.insert(0, first);

      syntax::LayoutQualifier { ids: ids }
    })
  ))
);

named!(layout_qualifier_spec<&[u8], syntax::LayoutQualifierSpec>,
  alt!(
    value!(syntax::LayoutQualifierSpec::Shared, tag!("shared")) |
    ws!(do_parse!(
      i: identifier >>
      char!('=') >>
      e: expr >>
      (syntax::LayoutQualifierSpec::Identifier(i, Some(Box::new(e))))
    )) |
    map!(identifier, |i| syntax::LayoutQualifierSpec::Identifier(i, None))
  )
);

/// Parse a precision qualifier.
named!(pub precision_qualifier<&[u8], syntax::PrecisionQualifier>,
  alt!(
    value!(syntax::PrecisionQualifier::High, tag!("high")) |
    value!(syntax::PrecisionQualifier::Medium, tag!("medium")) |
    value!(syntax::PrecisionQualifier::Low, tag!("low"))
  )
);

/// Parse an interpolation qualifier.
named!(pub interpolation_qualifier<&[u8], syntax::InterpolationQualifier>,
  alt!(
    value!(syntax::InterpolationQualifier::Smooth, tag!("smooth")) |
    value!(syntax::InterpolationQualifier::Flat, tag!("flat")) |
    value!(syntax::InterpolationQualifier::NoPerspective, tag!("noperspective"))
  )
);

/// Parse an invariant qualifier.
named!(pub invariant_qualifier<&[u8], ()>,
  value!((), tag!("invariant")));

/// Parse a precise qualifier.
named!(pub precise_qualifier<&[u8], ()>,
  value!((), tag!("precise")));

/// Parse a type qualifier.
named!(pub type_qualifier<&[u8], syntax::TypeQualifier>,
  do_parse!(
    qualifiers: many1!(ws!(type_qualifier_spec)) >>
    (syntax::TypeQualifier { qualifiers: qualifiers })
  )
);

/// Parse a type qualifier spec.
named!(pub type_qualifier_spec<&[u8], syntax::TypeQualifierSpec>,
  alt!(
    map!(storage_qualifier, syntax::TypeQualifierSpec::Storage) |
    map!(layout_qualifier, syntax::TypeQualifierSpec::Layout) |
    map!(precision_qualifier, syntax::TypeQualifierSpec::Precision) |
    map!(interpolation_qualifier, syntax::TypeQualifierSpec::Interpolation) |
    value!(syntax::TypeQualifierSpec::Invariant, invariant_qualifier) |
    value!(syntax::TypeQualifierSpec::Precise, precise_qualifier)
  )
);

/// Parse a fully specified type.
named!(pub fully_specified_type<&[u8], syntax::FullySpecifiedType>,
  ws!(do_parse!(
    qualifier: opt!(type_qualifier) >>
    ty: type_specifier >>

    (syntax::FullySpecifiedType { qualifier: qualifier, ty: ty })
  ))
);

/// Parse an array specifier with no size information.
named!(pub array_specifier<&[u8], syntax::ArraySpecifier>,
  alt!(
    ws!(do_parse!(char!('[') >> char!(']') >> (syntax::ArraySpecifier::Unsized))) |
    ws!(do_parse!(char!('[') >> e: cond_expr >> char!(']') >> (syntax::ArraySpecifier::ExplicitlySized(Box::new(e)))))
  )
);

/// Parse a primary expression.
named!(pub primary_expr<&[u8], syntax::Expr>,
  alt!(
    map!(double_lit, syntax::Expr::DoubleConst) |
    map!(float_lit, syntax::Expr::FloatConst) |
    map!(unsigned_lit, syntax::Expr::UIntConst) |
    map!(integral_lit, syntax::Expr::IntConst) |
    map!(bool_lit, syntax::Expr::BoolConst) |
    map!(identifier, syntax::Expr::Variable) |
    parens_expr
  )
);

/// Parse a postfix expression.
named!(pub postfix_expr<&[u8], syntax::Expr>,
  alt!(
    function_call |

    // bracket
    do_parse!(
      pfe: primary_expr >>
      a: array_specifier >>
      (syntax::Expr::Bracket(Box::new(pfe.clone()), a))
    ) |

    // dot
    do_parse!(
      pfe: primary_expr >>
      fs: dot_field_selection >>
      (syntax::Expr::Dot(Box::new(pfe.clone()), fs))
    ) |

    // inc
    do_parse!(
      pfe: primary_expr >>
      tag!("++") >>
      (syntax::Expr::PostInc(Box::new(pfe.clone())))
    ) |

    // dec
    do_parse!(
      pfe: primary_expr >>
      tag!("--") >>
      (syntax::Expr::PostDec(Box::new(pfe)))
    ) |

    primary_expr
  )
);

/// Parse a unary expression.
named!(pub unary_expr<&[u8], syntax::Expr>,
  alt!(
    do_parse!(
      op: unary_op >>
      e: unary_expr >>
      (syntax::Expr::Unary(op, Box::new(e)))
    ) |

    postfix_expr
  )
);

/// Parse an expression between parens.
named!(pub parens_expr<&[u8], syntax::Expr>, ws!(delimited!(char!('('), ws!(expr), char!(')'))));

/// Parse a dot field selection.
named!(pub dot_field_selection<&[u8], syntax::FieldSelection>,
  do_parse!(
    char!('.') >>
    field: identifier >>
    a: opt!(array_specifier) >>
    next: map!(opt!(dot_field_selection), |x| x.map(Box::new)) >>

    (syntax::FieldSelection {
      field: field,
      array_specifier: a,
      next: next
    })
  )
);

/// Parse a declaration.
named!(declaration<&[u8], syntax::Declaration>,
  alt!(
    map!(function_prototype, syntax::Declaration::FunctionPrototype) |
    map!(init_declarator_list, syntax::Declaration::InitDeclaratorList) |
    precision_declaration |
    block_declaration |
    global_declaration
  )
);

/// Parse a precision declaration.
named!(precision_declaration<&[u8], syntax::Declaration>,
  ws!(do_parse!(
    tag!("precision") >>
    qual: precision_qualifier >>
    ty: type_specifier >>
    char!(';') >>

    (syntax::Declaration::Precision(qual, ty))
  ))
);

/// Parse a block declaration.
named!(block_declaration<&[u8], syntax::Declaration>,
  ws!(do_parse!(
    qual: type_qualifier >>
    name: identifier >>
    char!('{') >>
    fields: many1!(struct_field_specifier) >>
    char!('}') >>
    a: alt!(
         value!(None, char!(';')) |
         ws!(do_parse!(
           a: alt!(
                map!(identifier, |i| Some((i, None))) |
                ws!(do_parse!(
                  i: identifier >>
                  arr_spec: array_specifier >>

                  (Some((i, Some(arr_spec))))
                ))
              ) >>
           char!(';') >>

           (a)
         ))
       ) >>

    (syntax::Declaration::Block(qual, name, fields, a))
  ))
);

/// Parse a global declaration.
named!(global_declaration<&[u8], syntax::Declaration>,
  ws!(do_parse!(
    qual: type_qualifier >>
    identifiers: many0!(ws!(do_parse!(char!(',') >> i: identifier >> (i)))) >>
    (syntax::Declaration::Global(qual, identifiers))
  ))
);

/// Parse a function prototype.
named!(function_prototype<&[u8], syntax::FunctionPrototype>,
  ws!(do_parse!(
    fp: function_declarator >>
    char!(')') >>
    (fp)
  ))
);

/// Parse an init declarator list.
named!(init_declarator_list<&[u8], syntax::InitDeclaratorList>,
  alt!(
    map!(single_declaration, syntax::InitDeclaratorList::Single)
  )
);

// TODO: refactor the alt branches into something smarter
/// Parse a single declaration.
named!(single_declaration<&[u8], syntax::SingleDeclaration>,
  alt!(
    ws!(do_parse!(
      ty: fully_specified_type >>
      (syntax::SingleDeclaration {
        ty: ty,
        name: None,
        array_specifier: None,
        initializer: None
      })
    )) |

    ws!(do_parse!(
      ty: fully_specified_type >>
      name: identifier >>
      (syntax::SingleDeclaration {
        ty: ty,
        name: Some(name),
        array_specifier: None,
        initializer: None
      })
    )) |

    ws!(do_parse!(
      ty: fully_specified_type >>
      name: identifier >>
      a: array_specifier >>
      (syntax::SingleDeclaration {
        ty: ty,
        name: Some(name),
        array_specifier: Some(a),
        initializer: None
      })
    )) |

    ws!(do_parse!(
      ty: fully_specified_type >>
      name: identifier >>
      a: opt!(array_specifier) >>
      char!('=') >>
      ini: initializer >>
      (syntax::SingleDeclaration {
        ty: ty,
        name: Some(name),
        array_specifier: a,
        initializer: Some(ini)
      })
    ))
  )
);

/// Parse an initializer.
named!(initializer<&[u8], syntax::Initializer>,
  alt!(
    map!(assignment_expr, |e| syntax::Initializer::AssignmentExpr(Box::new(e))) |
    ws!(do_parse!(
      char!('{') >>
      il: initializer_list >>
      opt!(char!(',')) >>
      char!('}') >>

      (syntax::Initializer::List(il))
    ))
  )
);

/// Parse an initializer list.
named!(initializer_list<&[u8], Vec<syntax::Initializer>>,
  ws!(do_parse!(
    first: initializer >>
    rest: many0!(ws!(do_parse!(char!(',') >> ini: initializer >> (ini)))) >>

    ({
      let mut inis = rest.clone();
      inis.insert(0, first);
      (inis)
    })
  ))
);

named!(function_declarator<&[u8], syntax::FunctionPrototype>,
  alt!(
    map!(function_header, |(ret_ty, fun_name)| syntax::FunctionPrototype { ty: ret_ty, name: fun_name, parameters: Vec::new() }) |
    function_header_with_parameters
  )
);

named!(function_header<&[u8], (syntax::FullySpecifiedType, syntax::Identifier)>,
  ws!(do_parse!(
    ret_ty: fully_specified_type >>
    fun_name: identifier >>
    char!('(') >>
    (ret_ty, fun_name)
  ))
);

named!(function_header_with_parameters<&[u8], syntax::FunctionPrototype>,
  ws!(do_parse!(
    header: function_header >>
    first_param: function_parameter_declaration >>
    rest_params: many0!(ws!(do_parse!(char!(',') >> param: function_parameter_declaration >> (param)))) >>

    ({
      let mut params = rest_params.clone();
      params.insert(0, first_param);
      syntax::FunctionPrototype {
        ty: header.0,
        name: header.1,
        parameters: params
      }
    })
  ))
);

named!(function_parameter_declaration<&[u8], syntax::FunctionParameterDeclaration>,
  alt!(function_parameter_declaration_named | function_parameter_declaration_unnamed));

named!(function_parameter_declaration_named<&[u8], syntax::FunctionParameterDeclaration>,
  ws!(do_parse!(
    ty_qual: opt!(type_qualifier) >>
    fpd: function_parameter_declarator >>
    (syntax::FunctionParameterDeclaration::Named(ty_qual, fpd))
  ))
);

named!(function_parameter_declaration_unnamed<&[u8], syntax::FunctionParameterDeclaration>,
  ws!(do_parse!(
    ty_qual: opt!(type_qualifier) >>
    ty_spec: type_specifier >>
    (syntax::FunctionParameterDeclaration::Unnamed(ty_qual, ty_spec))
  ))
);

named!(function_parameter_declarator<&[u8], syntax::FunctionParameterDeclarator>,
  ws!(do_parse!(
    ty: type_specifier >>
    name: identifier >>
    a: opt!(array_specifier) >>
    (syntax::FunctionParameterDeclarator {
      ty: ty,
      name: name,
      array_spec: a
    })
  ))
);

/// Parse a function call.
named!(pub function_call<&[u8], syntax::Expr>,
  alt!(
    function_call_header_no_parameter |
    function_call_header_with_parameters
  )
);

named!(function_call_header_no_parameter<&[u8], syntax::Expr>,
  ws!(do_parse!(
    fi: function_call_header >>
    opt!(void) >>
    char!(')') >>

    (syntax::Expr::FunCall(fi, Vec::new()))
  ))
);

named!(function_call_header_with_parameters<&[u8], syntax::Expr>,
  ws!(do_parse!(
    fi: function_call_header >>
    first_arg: assignment_expr >>
    rest_args: many0!(ws!(do_parse!(char!(',') >> arg: assignment_expr >> (arg)))) >>
    char!(')') >>

    ({
      let mut args = rest_args.clone();
      args.insert(0, first_arg);
      syntax::Expr::FunCall(fi, args)
    })
  ))
);

named!(function_call_header<&[u8], syntax::FunIdentifier>,
  ws!(do_parse!(
    fi: function_identifier >>
    char!('(') >>
    (fi)
  ))
);

/// Parse a function identifier.
named!(pub function_identifier<&[u8], syntax::FunIdentifier>,
  alt!(
    map!(type_specifier, syntax::FunIdentifier::TypeSpecifier) //|
    //map!(postfix_expr, |e| syntax::FunIdentifier::Expr(Box::new(e))) // FIXME
  )
);

/// Parse the most general expression.
named!(pub expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    first: assignment_expr >>
    a: alt!(
         ws!(do_parse!(
           char!(',') >>
           next: expr >>
           (syntax::Expr::Comma(Box::new(first.clone()), Box::new(next)))
         )) |
         value!(first)
       ) >>
    (a)
  ))
);

/// Parse an assignment expression.
named!(pub assignment_expr<&[u8], syntax::Expr>,
  alt!(
    ws!(do_parse!(
      e: unary_expr >>
      o: assignment_op >>
      v: assignment_expr >>

      (syntax::Expr::Assignment(Box::new(e), o, Box::new(v)))
    )) |
    cond_expr
  )
);

/// Parse an assignment operator.
named!(pub assignment_op<&[u8], syntax::AssignmentOp>,
  alt!(
    value!(syntax::AssignmentOp::Equal, char!('=')) |
    value!(syntax::AssignmentOp::Mult, tag!("*=")) |
    value!(syntax::AssignmentOp::Div, tag!("/=")) |
    value!(syntax::AssignmentOp::Mod, tag!("%=")) |
    value!(syntax::AssignmentOp::Add, tag!("+=")) |
    value!(syntax::AssignmentOp::Sub, tag!("-=")) |
    value!(syntax::AssignmentOp::LShift, tag!("<<=")) |
    value!(syntax::AssignmentOp::RShift, tag!(">>=")) |
    value!(syntax::AssignmentOp::And, tag!("&=")) |
    value!(syntax::AssignmentOp::Xor, tag!("^=")) |
    value!(syntax::AssignmentOp::Or, tag!("|="))
  )
);

/// Parse a conditional expression.
named!(pub cond_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: logical_or_expr >>
    e: alt!(
         ws!(do_parse!(
           char!('?') >>
           b: expr >>
           char!(':') >>
           c: assignment_expr >>

           (syntax::Expr::Ternary(Box::new(a.clone()), Box::new(b), Box::new(c)))
         )) |
         value!(a)
       ) >>
    (e)
  ))
);

/// Parse a logical OR expression.
named!(pub logical_or_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: logical_xor_expr >>
    n: alt!(
         ws!(do_parse!(
           tag!("||") >>
           b: logical_or_expr >>
           (syntax::Expr::Binary(syntax::BinaryOp::Or, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a logical XOR expression.
named!(pub logical_xor_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: logical_and_expr >>
    n: alt!(
         ws!(do_parse!(
           tag!("^^") >>
           b: logical_xor_expr >>
           (syntax::Expr::Binary(syntax::BinaryOp::Xor, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a logical AND expression.
named!(pub logical_and_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: inclusive_or_expr >>
    n: alt!(
         ws!(do_parse!(
           tag!("&&") >>
           b: logical_and_expr >>
           (syntax::Expr::Binary(syntax::BinaryOp::And, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a bitwise OR expression.
named!(pub inclusive_or_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: exclusive_or_expr >>
    n: alt!(
         ws!(do_parse!(
           char!('|') >>
           b: inclusive_or_expr >>
           (syntax::Expr::Binary(syntax::BinaryOp::BitOr, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a bitwise XOR expression.
named!(pub exclusive_or_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: and_expr >>
    n: alt!(
         ws!(do_parse!(
           char!('^') >>
           b: exclusive_or_expr >>
           (syntax::Expr::Binary(syntax::BinaryOp::BitXor, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a bitwise AND expression.
named!(pub and_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: equality_expr >>
    n: alt!(
         ws!(do_parse!(
           char!('&') >>
           b: and_expr >>
           (syntax::Expr::Binary(syntax::BinaryOp::BitAnd, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse an equality expression.
named!(pub equality_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: rel_expr >>
    n: alt!(
         ws!(do_parse!(
           op: alt!(
                 value!(syntax::BinaryOp::Equal, tag!("==")) |
                 value!(syntax::BinaryOp::NonEqual, tag!("!="))
               ) >>
           b: equality_expr >>
           (syntax::Expr::Binary(op, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a relational expression.
named!(pub rel_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: shift_expr >>
    n: alt!(
         ws!(do_parse!(
           op: alt!(
                 value!(syntax::BinaryOp::LT, char!('<')) |
                 value!(syntax::BinaryOp::GT, char!('>')) |
                 value!(syntax::BinaryOp::LTE, tag!("<=")) |
                 value!(syntax::BinaryOp::GTE, tag!(">="))
               ) >>
           b: rel_expr >>
           (syntax::Expr::Binary(op, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a shift expression.
named!(pub shift_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: additive_expr >>
    n: alt!(
         ws!(do_parse!(
           op: alt!(
                 value!(syntax::BinaryOp::LShift, tag!("<<")) |
                 value!(syntax::BinaryOp::RShift, tag!(">>"))
               ) >>
           b: shift_expr >>
           (syntax::Expr::Binary(op, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse an additive expression.
named!(pub additive_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: multiplicative_expr >>
    n: alt!(
         ws!(do_parse!(
           op: alt!(
                 value!(syntax::BinaryOp::Add, char!('+')) |
                 value!(syntax::BinaryOp::Sub, char!('-'))
               ) >>
           b: additive_expr >>
           (syntax::Expr::Binary(op, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a multiplicative expression.
named!(pub multiplicative_expr<&[u8], syntax::Expr>,
  ws!(do_parse!(
    a: unary_expr >>
    n: alt!(
         ws!(do_parse!(
           op: alt!(
                 value!(syntax::BinaryOp::Mult, char!('*')) |
                 value!(syntax::BinaryOp::Div, char!('/')) |
                 value!(syntax::BinaryOp::Mod, char!('%'))
               ) >>
           b: multiplicative_expr >>
           (syntax::Expr::Binary(op, Box::new(a.clone()), Box::new(b)))
         )) |
         value!(a)
       ) >>
    (n)
  ))
);

/// Parse a simple statement.
named!(simple_statement<&[u8], syntax::SimpleStatement>,
  alt!(
    map!(declaration, syntax::SimpleStatement::Declaration) |
    map!(expr_statement, syntax::SimpleStatement::Expression) |
    map!(selection_statement, syntax::SimpleStatement::Selection) |
    map!(switch_statement, syntax::SimpleStatement::Switch) |
    map!(case_label, syntax::SimpleStatement::CaseLabel) |
    map!(iteration_statement, syntax::SimpleStatement::Iteration) |
    map!(jump_statement, syntax::SimpleStatement::Jump)
  )
);

/// Parse an expression statement.
named!(expr_statement<&[u8], syntax::ExprStatement>,
  ws!(do_parse!(
    e: opt!(expr) >>
    char!(';') >>
    (e)
  ))
);

/// Parse a selection statement.
named!(selection_statement<&[u8], syntax::SelectionStatement>,
  ws!(do_parse!(
    tag!("if") >>
    char!('(') >>
    cond_expr: expr >>
    char!(')') >>
    srs: selection_rest_statement >>
    (syntax::SelectionStatement::If(Box::new(cond_expr), srs))
  ))
);

named!(selection_rest_statement<&[u8], syntax::SelectionRestStatement>,
  alt!(
    map!(statement, |st| syntax::SelectionRestStatement::Statement(Box::new(st))) |
    ws!(do_parse!(
      cond: statement >>
      tag!("else") >>
      rest: statement >>
      (syntax::SelectionRestStatement::Else(Box::new(cond), Box::new(rest)))
    ))
  )
);

/// Parse a switch statement.
named!(switch_statement<&[u8], syntax::SwitchStatement>,
  ws!(do_parse!(
    tag!("switch") >>
    char!('(') >>
    head: expr >>
    char!(')') >>
    char!('{') >>
    body: many0!(statement) >>
    char!('}') >>

    (syntax::SwitchStatement { head: Box::new(head), body: body })
  ))
);

/// Parse a case label.
named!(case_label<&[u8], syntax::CaseLabel>,
  alt!(
    ws!(do_parse!(
      tag!("case") >>
      e: expr >>
      char!(';') >>
      (syntax::CaseLabel::Case(Box::new(e)))
    )) |
    ws!(do_parse!(
      tag!("default") >>
      char!(';') >>
      (syntax::CaseLabel::Def)
    ))
  )
);

/// Parse an iteration statement.
named!(iteration_statement<&[u8], syntax::IterationStatement>,
  alt!(
    iteration_statement_while |
    iteration_statement_do_while |
    iteration_statement_for
  )
);

named!(iteration_statement_while<&[u8], syntax::IterationStatement>,
  ws!(do_parse!(
    tag!("while") >>
    char!('(') >>
    cond: condition >>
    char!(')') >>
    st: statement >>
    (syntax::IterationStatement::While(cond, Box::new(st)))
  ))
);

named!(iteration_statement_do_while<&[u8], syntax::IterationStatement>,
  ws!(do_parse!(
    tag!("do") >>
    st: statement >>
    tag!("while") >>
    char!('(') >>
    e: expr >>
    char!(')') >>
    char!(';') >>
    (syntax::IterationStatement::DoWhile(Box::new(st), Box::new(e)))
  ))
);

named!(iteration_statement_for<&[u8], syntax::IterationStatement>,
  ws!(do_parse!(
    tag!("for") >>
    char!('(') >>
    head: iteration_statement_for_init_statement >>
    rest: iteration_statement_for_rest_statement >>
    char!(')') >>
    body: statement >>
    (syntax::IterationStatement::For(head, rest, Box::new(body)))
  ))
);

named!(iteration_statement_for_init_statement<&[u8], syntax::ForInitStatement>,
  alt!(
    map!(expr_statement, syntax::ForInitStatement::Expression) |
    map!(declaration, |d| syntax::ForInitStatement::Declaration(Box::new(d)))
  )
);

named!(iteration_statement_for_rest_statement<&[u8], syntax::ForRestStatement>,
  ws!(do_parse!(
    cond: opt!(condition) >>
    char!(';') >>
    e: opt!(expr) >>
    (syntax::ForRestStatement { condition: cond, post_expr: e.map(Box::new) })
  ))
);

/// Parse a jump statement.
named!(jump_statement<&[u8], syntax::JumpStatement>,
  alt!(
    jump_statement_continue |
    jump_statement_break |
    jump_statement_return |
    jump_statement_discard
  )
);

named!(jump_statement_continue<&[u8], syntax::JumpStatement>,
  ws!(do_parse!(tag!("continue") >> char!(';') >> (syntax::JumpStatement::Continue)))
);

named!(jump_statement_break<&[u8], syntax::JumpStatement>,
  ws!(do_parse!(tag!("break") >> char!(';') >> (syntax::JumpStatement::Break)))
);

named!(jump_statement_discard<&[u8], syntax::JumpStatement>,
  ws!(do_parse!(tag!("discard") >> char!(';') >> (syntax::JumpStatement::Discard)))
);

named!(jump_statement_return<&[u8], syntax::JumpStatement>,
  ws!(do_parse!(
    tag!("return") >>
    e: expr >>
    char!(';') >>
    (syntax::JumpStatement::Return(Box::new(e)))
  ))
);

/// Parse a condition.
named!(condition<&[u8], syntax::Condition>,
  alt!(
    map!(expr, |e| syntax::Condition::Expr(Box::new(e))) |
    condition_assignment
  )
);

named!(condition_assignment<&[u8], syntax::Condition>,
  ws!(do_parse!(
    ty: fully_specified_type >>
    id: identifier >>
    char!('=') >>
    ini: initializer >>
    (syntax::Condition::Assignment(ty, id, ini))
  ))
);

/// Parse a statement.
named!(statement<&[u8], syntax::Statement>,
  alt!(
    map!(compound_statement, |c| syntax::Statement::Compound(Box::new(c))) |
    map!(simple_statement, |s| syntax::Statement::Simple(Box::new(s)))
  )
);

/// Parse a compound statement.
named!(compound_statement<&[u8], syntax::CompoundStatement>,
  ws!(do_parse!(
    char!('{') >>
    stl: many0!(statement) >>
    char!('}') >>
    (syntax::CompoundStatement { statement_list: stl })
  ))
);

/// Parse a function definition.
named!(function_definition<&[u8], syntax::FunctionDefinition>,
  ws!(do_parse!(
    prototype: function_prototype >>
    st: compound_statement >>
    (syntax::FunctionDefinition { prototype: prototype, statement: st })
  ))
);

/// Parse an external declaration.
named!(external_declaration<&[u8], syntax::ExternalDeclaration>,
  alt!(
    map!(function_definition, syntax::ExternalDeclaration::FunctionDefinition) |
    map!(declaration, syntax::ExternalDeclaration::Declaration)
  )
);

/// Parse a translation unit (entry point).
named!(translation_unit<&[u8], syntax::TranslationUnit>,
  alt!(
    map!(external_declaration, syntax::TranslationUnit::ExternalDeclaration) |
    ws!(do_parse!(
      tl: translation_unit >>
      ed: external_declaration >>
      (syntax::TranslationUnit::Comma(Box::new(tl), ed))
    ))
  )
);

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_uniline_comment() {
    assert_eq!(comment(&b"// lol\nfoo"[..]), IResult::Done(&b"foo"[..], &b"// lol\n"[..]));
  }

  #[test]
  fn parse_uniline_comments() {
    assert_eq!(comments(&b"// lol\n// test\n"[..]), IResult::Done(&b""[..], &b"// lol\n// test\n"[..]));
  }

  #[test]
  fn parse_multiline_comment() {
    assert_eq!(comment(&b"/* lol\nfoo\n*/bar"[..]), IResult::Done(&b"bar"[..], &b"/* lol\nfoo\n*/"[..]));
  }

  #[test]
  fn parse_multiline_comments() {
    assert_eq!(comments(&b"/* lol\nfoo\n*/\n/*bar\n*/"[..]), IResult::Done(&b""[..], &b"/* lol\nfoo\n*/\n/*bar\n*/"[..]));
  }

  #[test]
  fn parse_unsigned_suffix() {
    assert_eq!(unsigned_suffix(&b"u"[..]), IResult::Done(&b""[..], 'u'));
    assert_eq!(unsigned_suffix(&b"U"[..]), IResult::Done(&b""[..], 'U'));
  }
  
  #[test]
  fn parse_nonzero_digit() {
    assert_eq!(nonzero_digit(&b"3"[..]), IResult::Done(&b""[..], &b"3"[..]));
    assert_eq!(nonzero_digit(&b"12345953"[..]), IResult::Done(&b""[..], &b"12345953"[..]));
    assert_eq!(nonzero_digit(&b"03"[..]), IResult::Error(ErrorKind::Verify));
  }
  
  #[test]
  fn parse_decimal_lit() {
    assert_eq!(decimal_lit(&b"3"[..]), IResult::Done(&b""[..], &b"3"[..]));
    assert_eq!(decimal_lit(&b"3 "[..]), IResult::Done(&b" "[..], &b"3"[..]));
    assert_eq!(decimal_lit(&b"03"[..]), IResult::Error(ErrorKind::Verify));
  }
  
  #[test]
  fn parse_octal_lit() {
    assert_eq!(octal_lit(&b"3"[..]), IResult::Error(ErrorKind::Verify));
    assert_eq!(octal_lit(&b"03 "[..]), IResult::Done(&b" "[..], &b"03"[..]));
    assert_eq!(octal_lit(&b"07654321234567 "[..]), IResult::Done(&b" "[..], &b"07654321234567"[..]));
    assert_eq!(octal_lit(&b"07654321934567 "[..]), IResult::Error(ErrorKind::Verify));
  }
  
  #[test]
  fn parse_hexadecimal_lit() {
    assert_eq!(hexadecimal_lit(&b"3"[..]), IResult::Error(ErrorKind::Alt));
    assert_eq!(hexadecimal_lit(&b"03"[..]), IResult::Error(ErrorKind::Alt));
    assert_eq!(hexadecimal_lit(&b"0x3 "[..]), IResult::Done(&b" "[..], &b"0x3"[..]));
    assert_eq!(hexadecimal_lit(&b"0x0123456789ABCDEF"[..]), IResult::Done(&b""[..], &b"0x0123456789ABCDEF"[..]));
    assert_eq!(hexadecimal_lit(&b"0x0123456789abcdef"[..]), IResult::Done(&b""[..], &b"0x0123456789abcdef"[..]));
    assert_eq!(hexadecimal_lit(&b"0x0123g456789abcdef"[..]), IResult::Error(ErrorKind::Verify));
  }
  
  #[test]
  fn parse_integral_lit() {
    assert_eq!(integral_lit(&b"3"[..]), IResult::Done(&b""[..], 3));
    assert_eq!(integral_lit(&b"3 "[..]), IResult::Done(&b" "[..], 3));
    assert_eq!(integral_lit(&b"03 "[..]), IResult::Done(&b" "[..], 3));
    assert_eq!(integral_lit(&b"076556 "[..]), IResult::Done(&b" "[..], 76556));
    assert_eq!(integral_lit(&b"07654321934567 "[..]), IResult::Error(ErrorKind::Alt));
    assert_eq!(integral_lit(&b"0x3 "[..]), IResult::Done(&b" "[..], 0x3));
    assert_eq!(integral_lit(&b"0x9ABCDEF"[..]), IResult::Done(&b""[..], 0x9ABCDEF));
    assert_eq!(integral_lit(&b"0x9ABCDEF"[..]), IResult::Done(&b""[..], 0x9ABCDEF));
    assert_eq!(integral_lit(&b"0x9abcdef"[..]), IResult::Done(&b""[..], 0x9abcdef));
    assert_eq!(integral_lit(&b"0x9abcdef"[..]), IResult::Done(&b""[..], 0x9abcdef));
  }
  
  #[test]
  fn parse_integral_neg_lit() {
    assert_eq!(integral_lit(&b"-3"[..]), IResult::Done(&b""[..], -3));
    assert_eq!(integral_lit(&b"-3 "[..]), IResult::Done(&b" "[..], -3));
    assert_eq!(integral_lit(&b"-03 "[..]), IResult::Done(&b" "[..], -3));
    assert_eq!(integral_lit(&b"-076556 "[..]), IResult::Done(&b" "[..], -76556));
    assert_eq!(integral_lit(&b"-07654321934567 "[..]), IResult::Error(ErrorKind::Alt));
    assert_eq!(integral_lit(&b"-0x3 "[..]), IResult::Done(&b" "[..], -0x3));
    assert_eq!(integral_lit(&b"-0x9ABCDEF"[..]), IResult::Done(&b""[..], -0x9ABCDEF));
    assert_eq!(integral_lit(&b"-0x9ABCDEF"[..]), IResult::Done(&b""[..], -0x9ABCDEF));
    assert_eq!(integral_lit(&b"-0x9abcdef"[..]), IResult::Done(&b""[..], -0x9abcdef));
    assert_eq!(integral_lit(&b"-0x9abcdef"[..]), IResult::Done(&b""[..], -0x9abcdef));
  }
  
  #[test]
  fn parse_float_lit() {
    assert_eq!(float_lit(&b"0.;"[..]), IResult::Done(&b";"[..], 0.));
    assert_eq!(float_lit(&b".0;"[..]), IResult::Done(&b";"[..], 0.));
    assert_eq!(float_lit(&b".035 "[..]), IResult::Done(&b" "[..], 0.035));
    assert_eq!(float_lit(&b"0. "[..]), IResult::Done(&b" "[..], 0.));
    assert_eq!(float_lit(&b"0.035 "[..]), IResult::Done(&b" "[..], 0.035));
    assert_eq!(float_lit(&b".035f"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(float_lit(&b"0.f"[..]), IResult::Done(&b""[..], 0.));
    assert_eq!(float_lit(&b"314.f"[..]), IResult::Done(&b""[..], 314.));
    assert_eq!(float_lit(&b"0.035f"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(float_lit(&b".035F"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(float_lit(&b"0.F"[..]), IResult::Done(&b""[..], 0.));
    assert_eq!(float_lit(&b"0.035F"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(float_lit(&b"1.03e+34 "[..]), IResult::Done(&b" "[..], 1.03e+34));
    assert_eq!(float_lit(&b"1.03E+34 "[..]), IResult::Done(&b" "[..], 1.03E+34));
    assert_eq!(float_lit(&b"1.03e-34 "[..]), IResult::Done(&b" "[..], 1.03e-34));
    assert_eq!(float_lit(&b"1.03E-34 "[..]), IResult::Done(&b" "[..], 1.03E-34));
    assert_eq!(float_lit(&b"1.03e+34f"[..]), IResult::Done(&b""[..], 1.03e+34));
    assert_eq!(float_lit(&b"1.03E+34f"[..]), IResult::Done(&b""[..], 1.03E+34));
    assert_eq!(float_lit(&b"1.03e-34f"[..]), IResult::Done(&b""[..], 1.03e-34));
    assert_eq!(float_lit(&b"1.03E-34f"[..]), IResult::Done(&b""[..], 1.03E-34));
    assert_eq!(float_lit(&b"1.03e+34F"[..]), IResult::Done(&b""[..], 1.03e+34));
    assert_eq!(float_lit(&b"1.03E+34F"[..]), IResult::Done(&b""[..], 1.03E+34));
    assert_eq!(float_lit(&b"1.03e-34F"[..]), IResult::Done(&b""[..], 1.03e-34));
    assert_eq!(float_lit(&b"1.03E-34F"[..]), IResult::Done(&b""[..], 1.03E-34));
  }
  
  #[test]
  fn parse_float_neg_lit() {
    assert_eq!(float_lit(&b"-.035 "[..]), IResult::Done(&b" "[..], -0.035));
    assert_eq!(float_lit(&b"-0. "[..]), IResult::Done(&b" "[..], -0.));
    assert_eq!(float_lit(&b"-0.035 "[..]), IResult::Done(&b" "[..], -0.035));
    assert_eq!(float_lit(&b"-.035f"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(float_lit(&b"-0.f"[..]), IResult::Done(&b""[..], -0.));
    assert_eq!(float_lit(&b"-0.035f"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(float_lit(&b"-.035F"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(float_lit(&b"-0.F"[..]), IResult::Done(&b""[..], -0.));
    assert_eq!(float_lit(&b"-0.035F"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(float_lit(&b"-1.03e+34 "[..]), IResult::Done(&b" "[..], -1.03e+34));
    assert_eq!(float_lit(&b"-1.03E+34 "[..]), IResult::Done(&b" "[..], -1.03E+34));
    assert_eq!(float_lit(&b"-1.03e-34 "[..]), IResult::Done(&b" "[..], -1.03e-34));
    assert_eq!(float_lit(&b"-1.03E-34 "[..]), IResult::Done(&b" "[..], -1.03E-34));
    assert_eq!(float_lit(&b"-1.03e+34f"[..]), IResult::Done(&b""[..], -1.03e+34));
    assert_eq!(float_lit(&b"-1.03E+34f"[..]), IResult::Done(&b""[..], -1.03E+34));
    assert_eq!(float_lit(&b"-1.03e-34f"[..]), IResult::Done(&b""[..], -1.03e-34));
    assert_eq!(float_lit(&b"-1.03E-34f"[..]), IResult::Done(&b""[..], -1.03E-34));
    assert_eq!(float_lit(&b"-1.03e+34F"[..]), IResult::Done(&b""[..], -1.03e+34));
    assert_eq!(float_lit(&b"-1.03E+34F"[..]), IResult::Done(&b""[..], -1.03E+34));
    assert_eq!(float_lit(&b"-1.03e-34F"[..]), IResult::Done(&b""[..], -1.03e-34));
    assert_eq!(float_lit(&b"-1.03E-34F"[..]), IResult::Done(&b""[..], -1.03E-34));
  }
  
  #[test]
  fn parse_double_lit() {
    assert_eq!(double_lit(&b"0.;"[..]), IResult::Done(&b";"[..], 0.));
    assert_eq!(double_lit(&b".0;"[..]), IResult::Done(&b";"[..], 0.));
    assert_eq!(double_lit(&b".035 "[..]), IResult::Done(&b" "[..], 0.035));
    assert_eq!(double_lit(&b"0. "[..]), IResult::Done(&b" "[..], 0.));
    assert_eq!(double_lit(&b"0.035 "[..]), IResult::Done(&b" "[..], 0.035));
    assert_eq!(double_lit(&b"0.lf"[..]), IResult::Done(&b""[..], 0.));
    assert_eq!(double_lit(&b"0.035lf"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(double_lit(&b".035lf"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(double_lit(&b".035LF"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(double_lit(&b"0.LF"[..]), IResult::Done(&b""[..], 0.));
    assert_eq!(double_lit(&b"0.035LF"[..]), IResult::Done(&b""[..], 0.035));
    assert_eq!(double_lit(&b"1.03e+34lf"[..]), IResult::Done(&b""[..], 1.03e+34));
    assert_eq!(double_lit(&b"1.03E+34lf"[..]), IResult::Done(&b""[..], 1.03E+34));
    assert_eq!(double_lit(&b"1.03e-34lf"[..]), IResult::Done(&b""[..], 1.03e-34));
    assert_eq!(double_lit(&b"1.03E-34lf"[..]), IResult::Done(&b""[..], 1.03E-34));
    assert_eq!(double_lit(&b"1.03e+34LF"[..]), IResult::Done(&b""[..], 1.03e+34));
    assert_eq!(double_lit(&b"1.03E+34LF"[..]), IResult::Done(&b""[..], 1.03E+34));
    assert_eq!(double_lit(&b"1.03e-34LF"[..]), IResult::Done(&b""[..], 1.03e-34));
    assert_eq!(double_lit(&b"1.03E-34LF"[..]), IResult::Done(&b""[..], 1.03E-34));
  }
  
  #[test]
  fn parse_double_neg_lit() {
    assert_eq!(double_lit(&b"-0.;"[..]), IResult::Done(&b";"[..], -0.));
    assert_eq!(double_lit(&b"-.0;"[..]), IResult::Done(&b";"[..], -0.));
    assert_eq!(double_lit(&b"-.035 "[..]), IResult::Done(&b" "[..], -0.035));
    assert_eq!(double_lit(&b"-0. "[..]), IResult::Done(&b" "[..], -0.));
    assert_eq!(double_lit(&b"-0.035 "[..]), IResult::Done(&b" "[..], -0.035));
    assert_eq!(double_lit(&b"-0.lf"[..]), IResult::Done(&b""[..], -0.));
    assert_eq!(double_lit(&b"-0.035lf"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(double_lit(&b"-.035lf"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(double_lit(&b"-.035LF"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(double_lit(&b"-0.LF"[..]), IResult::Done(&b""[..], -0.));
    assert_eq!(double_lit(&b"-0.035LF"[..]), IResult::Done(&b""[..], -0.035));
    assert_eq!(double_lit(&b"-1.03e+34lf"[..]), IResult::Done(&b""[..], -1.03e+34));
    assert_eq!(double_lit(&b"-1.03E+34lf"[..]), IResult::Done(&b""[..], -1.03E+34));
    assert_eq!(double_lit(&b"-1.03e-34lf"[..]), IResult::Done(&b""[..], -1.03e-34));
    assert_eq!(double_lit(&b"-1.03E-34lf"[..]), IResult::Done(&b""[..], -1.03E-34));
    assert_eq!(double_lit(&b"-1.03e+34LF"[..]), IResult::Done(&b""[..], -1.03e+34));
    assert_eq!(double_lit(&b"-1.03E+34LF"[..]), IResult::Done(&b""[..], -1.03E+34));
    assert_eq!(double_lit(&b"-1.03e-34LF"[..]), IResult::Done(&b""[..], -1.03e-34));
    assert_eq!(double_lit(&b"-1.03E-34LF"[..]), IResult::Done(&b""[..], -1.03E-34));
  }

  #[test]
  fn parse_identifier() {
    assert_eq!(identifier(&b"a"[..]), IResult::Done(&b""[..], "a".to_owned()));
    assert_eq!(identifier(&b"ab_cd"[..]), IResult::Done(&b""[..], "ab_cd".to_owned()));
    assert_eq!(identifier(&b"Ab_cd"[..]), IResult::Done(&b""[..], "Ab_cd".to_owned()));
    assert_eq!(identifier(&b"Ab_c8d"[..]), IResult::Done(&b""[..], "Ab_c8d".to_owned()));
    assert_eq!(identifier(&b"Ab_c8d9"[..]), IResult::Done(&b""[..], "Ab_c8d9".to_owned()));
    assert_eq!(identifier(&b"0Ab_c8d9"[..]), IResult::Error(ErrorKind::Verify));
  }

  #[test]
  fn parse_unary_op_add() {
    assert_eq!(unary_op(&b"+ "[..]), IResult::Done(&b" "[..], syntax::UnaryOp::Add));
  }

  #[test]
  fn parse_unary_op_minus() {
    assert_eq!(unary_op(&b"- "[..]), IResult::Done(&b" "[..], syntax::UnaryOp::Minus));
  }

  #[test]
  fn parse_unary_op_not() {
    assert_eq!(unary_op(&b"!"[..]), IResult::Done(&b""[..], syntax::UnaryOp::Not));
  }

  #[test]
  fn parse_unary_op_complement() {
    assert_eq!(unary_op(&b"~"[..]), IResult::Done(&b""[..], syntax::UnaryOp::Complement));
  }

  #[test]
  fn parse_unary_op_inc() {
    assert_eq!(unary_op(&b"++"[..]), IResult::Done(&b""[..], syntax::UnaryOp::Inc));
  }

  #[test]
  fn parse_unary_op_dec() {
    assert_eq!(unary_op(&b"--"[..]), IResult::Done(&b""[..], syntax::UnaryOp::Dec));
  }

  #[test]
  fn parse_array_specifier_unsized() {
    assert_eq!(array_specifier(&b"[]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::Unsized));
    assert_eq!(array_specifier(&b"[ ]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::Unsized));
    assert_eq!(array_specifier(&b"  [\n]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::Unsized));
  }
  
  #[test]
  fn parse_array_specifier_sized() {
    let ix = syntax::Expr::IntConst(0);
    assert_eq!(array_specifier(&b"[0]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::ExplicitlySized(Box::new(ix.clone()))));
  }

  #[test]
  fn parse_precise_qualifier() {
    assert_eq!(precise_qualifier(&b"precise"[..]), IResult::Done(&b""[..], ()));
  }
  
  #[test]
  fn parse_invariant_qualifier() {
    assert_eq!(invariant_qualifier(&b"invariant"[..]), IResult::Done(&b""[..], ()));
  }
  
  #[test]
  fn parse_interpolation_qualifier() {
    assert_eq!(interpolation_qualifier(&b"smooth"[..]), IResult::Done(&b""[..], syntax::InterpolationQualifier::Smooth));
    assert_eq!(interpolation_qualifier(&b"flat"[..]), IResult::Done(&b""[..], syntax::InterpolationQualifier::Flat));
    assert_eq!(interpolation_qualifier(&b"noperspective"[..]), IResult::Done(&b""[..], syntax::InterpolationQualifier::NoPerspective));
  }
  
  #[test]
  fn parse_precision_qualifier() {
    assert_eq!(precision_qualifier(&b"high"[..]), IResult::Done(&b""[..], syntax::PrecisionQualifier::High));
    assert_eq!(precision_qualifier(&b"medium"[..]), IResult::Done(&b""[..], syntax::PrecisionQualifier::Medium));
    assert_eq!(precision_qualifier(&b"low"[..]), IResult::Done(&b""[..], syntax::PrecisionQualifier::Low));
  }
  
  #[test]
  fn parse_storage_qualifier() {
    assert_eq!(storage_qualifier(&b"const"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Const));
    assert_eq!(storage_qualifier(&b"inout"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::InOut));
    assert_eq!(storage_qualifier(&b"in "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::In));
    assert_eq!(storage_qualifier(&b"out"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Out));
    assert_eq!(storage_qualifier(&b"centroid"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Centroid));
    assert_eq!(storage_qualifier(&b"patch"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Patch));
    assert_eq!(storage_qualifier(&b"sample"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Sample));
    assert_eq!(storage_qualifier(&b"uniform"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Uniform));
    assert_eq!(storage_qualifier(&b"buffer"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Buffer));
    assert_eq!(storage_qualifier(&b"shared"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Shared));
    assert_eq!(storage_qualifier(&b"coherent"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Coherent));
    assert_eq!(storage_qualifier(&b"volatile"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Volatile));
    assert_eq!(storage_qualifier(&b"restrict"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Restrict));
    assert_eq!(storage_qualifier(&b"readonly"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::ReadOnly));
    assert_eq!(storage_qualifier(&b"writeonly"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::WriteOnly));
    assert_eq!(storage_qualifier(&b"subroutine a"[..]), IResult::Done(&b" a"[..], syntax::StorageQualifier::Subroutine(vec![])));
  
    let a = "vec3".to_owned();
    let b = "float".to_owned();
    let c = "dmat43".to_owned();
    let types = vec![a, b, c];
    assert_eq!(storage_qualifier(&b"subroutine (vec3, float, dmat43)"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Subroutine(types)));
  }
  
  #[test]
  fn parse_layout_qualifier_std430() {
    let expected = syntax::LayoutQualifier { ids: vec![syntax::LayoutQualifierSpec::Identifier("std430".to_owned(), None)] };
  
    assert_eq!(layout_qualifier(&b"layout (std430)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b" layout  (std430   )"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b" layout \n\t (  std430  )"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b" layout(std430)"[..]), IResult::Done(&b""[..], expected));
  }
  
  #[test]
  fn parse_layout_qualifier_shared() {
    let expected = syntax::LayoutQualifier { ids: vec![syntax::LayoutQualifierSpec::Shared] };
  
    assert_eq!(layout_qualifier(&b"layout (shared)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"   layout ( shared )"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"   layout(shared)"[..]), IResult::Done(&b""[..], expected));
  }
  
  #[test]
  fn parse_layout_qualifier_list() {
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".to_owned(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".to_owned(), Some(Box::new(syntax::Expr::IntConst(3))));
    let expected = syntax::LayoutQualifier { ids: vec![id_0, id_1, id_2] };
  
    assert_eq!(layout_qualifier(&b"layout (shared, std140, max_vertices = 3)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"layout(shared,std140,max_vertices=3)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"   layout\n\n\t (    shared , std140, max_vertices= 3)"[..]), IResult::Done(&b""[..], expected.clone()));
  }
  
  #[test]
  fn parse_type_qualifier() {
    let storage_qual = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Const);
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".to_owned(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".to_owned(), Some(Box::new(syntax::Expr::IntConst(3))));
    let layout_qual = syntax::TypeQualifierSpec::Layout(syntax::LayoutQualifier { ids: vec![id_0, id_1, id_2] });
    let expected = syntax::TypeQualifier { qualifiers: vec![storage_qual, layout_qual] };
  
    assert_eq!(type_qualifier(&b"const layout (shared, std140, max_vertices = 3)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(type_qualifier(&b"    const layout(shared,std140,max_vertices=3)"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_struct_field_specifier() {
    let expected = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::Vec4, identifiers: vec!["foo".to_owned()] };
  
    assert_eq!(struct_field_specifier(&b"vec4 foo;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_field_specifier(&b"  vec4     foo ; "[..]), IResult::Done(&b""[..], expected.clone()));
  }
  
  #[test]
  fn parse_struct_field_specifier_type_name() {
    let expected = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::TypeName("S0238_3".to_owned()), identifiers: vec!["x".to_owned()] };
  
    assert_eq!(struct_field_specifier(&b"S0238_3 x;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_field_specifier(&b"  S0238_3     x ; "[..]), IResult::Done(&b""[..], expected.clone()));
  }
  
  #[test]
  fn parse_struct_field_specifier_several() {
    let expected = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::Vec4, identifiers: vec!["foo".to_owned(), "bar".to_owned(), "zoo".to_owned()] };
  
    assert_eq!(struct_field_specifier(&b"vec4 foo, bar, zoo;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_field_specifier(&b"  vec4     foo , bar  , zoo ; "[..]), IResult::Done(&b""[..], expected.clone()));
  }
  
  #[test]
  fn parse_struct_specifier_one_field() {
    let field = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::Vec4, identifiers: vec!["foo".to_owned()] };
    let expected = syntax::StructSpecifier { name: Some("TestStruct".to_owned()), fields: vec![field] };
  
    assert_eq!(struct_specifier(&b"struct TestStruct { vec4 foo; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_specifier(&b"   struct      TestStruct \n \n\n {\n    vec4   foo  ;\n }"[..]), IResult::Done(&b""[..], expected));
  }
  
  #[test]
  fn parse_struct_specifier_multi_fields() {
    let a = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::Vec4, identifiers: vec!["foo".to_owned()] };
    let b = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::Float, identifiers: vec!["bar".to_owned()] };
    let c = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::UInt, identifiers: vec!["zoo".to_owned()] };
    let d = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::BVec3, identifiers: vec!["foo_BAR_zoo3497_34".to_owned()] };
    let e = syntax::StructFieldSpecifier { ty: syntax::TypeSpecifier::TypeName("S0238_3".to_owned()), identifiers: vec!["x".to_owned()] };
    let expected = syntax::StructSpecifier { name: Some("_TestStruct_934i".to_owned()), fields: vec![a, b, c, d, e] };
  
    assert_eq!(struct_specifier(&b"struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_specifier(&b"struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_specifier(&b"   struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_type_specifier() {
    assert_eq!(type_specifier(&b"bool"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Bool));
    assert_eq!(type_specifier(&b"int"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Int));
    assert_eq!(type_specifier(&b"uint"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UInt));
    assert_eq!(type_specifier(&b"float"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Float));
    assert_eq!(type_specifier(&b"double"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Double));
    assert_eq!(type_specifier(&b"vec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Vec2));
    assert_eq!(type_specifier(&b"vec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Vec3));
    assert_eq!(type_specifier(&b"vec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Vec4));
    assert_eq!(type_specifier(&b"dvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DVec2));
    assert_eq!(type_specifier(&b"dvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DVec3));
    assert_eq!(type_specifier(&b"dvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DVec4));
    assert_eq!(type_specifier(&b"bvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::BVec2));
    assert_eq!(type_specifier(&b"bvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::BVec3));
    assert_eq!(type_specifier(&b"bvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::BVec4));
    assert_eq!(type_specifier(&b"ivec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IVec2));
    assert_eq!(type_specifier(&b"ivec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IVec3));
    assert_eq!(type_specifier(&b"ivec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IVec4));
    assert_eq!(type_specifier(&b"uvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UVec2));
    assert_eq!(type_specifier(&b"uvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UVec3));
    assert_eq!(type_specifier(&b"uvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UVec4));
    assert_eq!(type_specifier(&b"mat2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat2));
    assert_eq!(type_specifier(&b"mat3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat3));
    assert_eq!(type_specifier(&b"mat4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat4));
    assert_eq!(type_specifier(&b"mat2x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat2));
    assert_eq!(type_specifier(&b"mat2x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat23));
    assert_eq!(type_specifier(&b"mat2x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat24));
    assert_eq!(type_specifier(&b"mat3x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat32));
    assert_eq!(type_specifier(&b"mat3x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat3));
    assert_eq!(type_specifier(&b"mat3x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat34));
    assert_eq!(type_specifier(&b"mat4x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat42));
    assert_eq!(type_specifier(&b"mat4x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat43));
    assert_eq!(type_specifier(&b"mat4x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat4));
    assert_eq!(type_specifier(&b"dmat2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat2));
    assert_eq!(type_specifier(&b"dmat3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat3));
    assert_eq!(type_specifier(&b"dmat4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat4));
    assert_eq!(type_specifier(&b"dmat2x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat2));
    assert_eq!(type_specifier(&b"dmat2x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat23));
    assert_eq!(type_specifier(&b"dmat2x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat24));
    assert_eq!(type_specifier(&b"dmat3x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat32));
    assert_eq!(type_specifier(&b"dmat3x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat3));
    assert_eq!(type_specifier(&b"dmat3x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat34));
    assert_eq!(type_specifier(&b"dmat4x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat42));
    assert_eq!(type_specifier(&b"dmat4x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat43));
    assert_eq!(type_specifier(&b"dmat4x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat4));
    assert_eq!(type_specifier(&b"sampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1D));
    assert_eq!(type_specifier(&b"image1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image1D));
    assert_eq!(type_specifier(&b"sampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2D));
    assert_eq!(type_specifier(&b"image2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2D));
    assert_eq!(type_specifier(&b"sampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler3D));
    assert_eq!(type_specifier(&b"image3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image3D));
    assert_eq!(type_specifier(&b"samplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCube));
    assert_eq!(type_specifier(&b"imageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ImageCube));
    assert_eq!(type_specifier(&b"sampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DRect));
    assert_eq!(type_specifier(&b"image2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DRect));
    assert_eq!(type_specifier(&b"sampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1DArray));
    assert_eq!(type_specifier(&b"image1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image1DArray));
    assert_eq!(type_specifier(&b"sampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DArray));
    assert_eq!(type_specifier(&b"image2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DArray));
    assert_eq!(type_specifier(&b"samplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerBuffer));
    assert_eq!(type_specifier(&b"imageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ImageBuffer));
    assert_eq!(type_specifier(&b"sampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DMS));
    assert_eq!(type_specifier(&b"image2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DMS));
    assert_eq!(type_specifier(&b"sampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DMSArray));
    assert_eq!(type_specifier(&b"image2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DMSArray));
    assert_eq!(type_specifier(&b"samplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCubeArray));
    assert_eq!(type_specifier(&b"imageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ImageCubeArray));
    assert_eq!(type_specifier(&b"sampler1DShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1DShadow));
    assert_eq!(type_specifier(&b"sampler2DShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DShadow));
    assert_eq!(type_specifier(&b"sampler2DRectShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DRectShadow));
    assert_eq!(type_specifier(&b"sampler1DArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1DArrayShadow));
    assert_eq!(type_specifier(&b"sampler2DArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DArrayShadow));
    assert_eq!(type_specifier(&b"samplerCubeShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCubeShadow));
    assert_eq!(type_specifier(&b"samplerCubeArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCubeArrayShadow));
    assert_eq!(type_specifier(&b"isampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler1D));
    assert_eq!(type_specifier(&b"iimage1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage1D));
    assert_eq!(type_specifier(&b"isampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2D));
    assert_eq!(type_specifier(&b"iimage2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2D));
    assert_eq!(type_specifier(&b"isampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler3D));
    assert_eq!(type_specifier(&b"iimage3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage3D));
    assert_eq!(type_specifier(&b"isamplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISamplerCube));
    assert_eq!(type_specifier(&b"iimageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImageCube));
    assert_eq!(type_specifier(&b"isampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DRect));
    assert_eq!(type_specifier(&b"iimage2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DRect));
    assert_eq!(type_specifier(&b"isampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler1DArray));
    assert_eq!(type_specifier(&b"iimage1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage1DArray));
    assert_eq!(type_specifier(&b"isampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DArray));
    assert_eq!(type_specifier(&b"iimage2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DArray));
    assert_eq!(type_specifier(&b"isamplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISamplerBuffer));
    assert_eq!(type_specifier(&b"iimageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImageBuffer));
    assert_eq!(type_specifier(&b"isampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DMS));
    assert_eq!(type_specifier(&b"iimage2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DMS));
    assert_eq!(type_specifier(&b"isampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DMSArray));
    assert_eq!(type_specifier(&b"iimage2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DMSArray));
    assert_eq!(type_specifier(&b"isamplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISamplerCubeArray));
    assert_eq!(type_specifier(&b"iimageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImageCubeArray));
    assert_eq!(type_specifier(&b"atomic_uint"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::AtomicUInt));
    assert_eq!(type_specifier(&b"usampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler1D));
    assert_eq!(type_specifier(&b"uimage1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage1D));
    assert_eq!(type_specifier(&b"usampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2D));
    assert_eq!(type_specifier(&b"uimage2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2D));
    assert_eq!(type_specifier(&b"usampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler3D));
    assert_eq!(type_specifier(&b"uimage3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage3D));
    assert_eq!(type_specifier(&b"usamplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USamplerCube));
    assert_eq!(type_specifier(&b"uimageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImageCube));
    assert_eq!(type_specifier(&b"usampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DRect));
    assert_eq!(type_specifier(&b"uimage2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DRect));
    assert_eq!(type_specifier(&b"usampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler1DArray));
    assert_eq!(type_specifier(&b"uimage1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage1DArray));
    assert_eq!(type_specifier(&b"usampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DArray));
    assert_eq!(type_specifier(&b"uimage2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DArray));
    assert_eq!(type_specifier(&b"usamplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USamplerBuffer));
    assert_eq!(type_specifier(&b"uimageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImageBuffer));
    assert_eq!(type_specifier(&b"usampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DMS));
    assert_eq!(type_specifier(&b"uimage2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DMS));
    assert_eq!(type_specifier(&b"usampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DMSArray));
    assert_eq!(type_specifier(&b"uimage2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DMSArray));
    assert_eq!(type_specifier(&b"usamplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USamplerCubeArray));
    assert_eq!(type_specifier(&b"uimageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImageCubeArray));
  }
  
  #[test]
  fn parse_fully_specified_type() {
    let ty = syntax::TypeSpecifier::IImage2DMSArray;
    let expected = syntax::FullySpecifiedType { qualifier: None, ty: ty };
  
    assert_eq!(fully_specified_type(&b"iimage2DMSArray"[..]), IResult::Done(&b""[..], expected.clone()));
  }
  
  #[test]
  fn parse_fully_specified_type_with_qualifier() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Subroutine(vec!["vec2".to_owned(), "S032_29k".to_owned()]));
    let qual = syntax::TypeQualifier { qualifiers: vec![qual_spec] };
    let ty = syntax::TypeSpecifier::IImage2DMSArray;
    let expected = syntax::FullySpecifiedType { qualifier: Some(qual), ty: ty };
  
    assert_eq!(fully_specified_type(&b"subroutine (vec2, S032_29k) iimage2DMSArray"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(fully_specified_type(&b"  subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray "[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(fully_specified_type(&b"subroutine(vec2,S032_29k)iimage2DMSArray"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_primary_expr_intconst() {
    assert_eq!(primary_expr(&b"0 "[..]), IResult::Done(&b" "[..], syntax::Expr::IntConst(0)));
    assert_eq!(primary_expr(&b"1 "[..]), IResult::Done(&b" "[..], syntax::Expr::IntConst(1)));
  }
  
  #[test]
  fn parse_primary_expr_uintconst() {
    assert_eq!(primary_expr(&b"0u "[..]), IResult::Done(&b" "[..], syntax::Expr::UIntConst(0)));
    assert_eq!(primary_expr(&b"1u "[..]), IResult::Done(&b" "[..], syntax::Expr::UIntConst(1)));
  }
  
  #[test]
  fn parse_primary_expr_floatconst() {
    assert_eq!(primary_expr(&b"0.f "[..]), IResult::Done(&b" "[..], syntax::Expr::FloatConst(0.)));
    assert_eq!(primary_expr(&b"1.f "[..]), IResult::Done(&b" "[..], syntax::Expr::FloatConst(1.)));
    assert_eq!(primary_expr(&b"0.F "[..]), IResult::Done(&b" "[..], syntax::Expr::FloatConst(0.)));
    assert_eq!(primary_expr(&b"1.F "[..]), IResult::Done(&b" "[..], syntax::Expr::FloatConst(1.)));
  }
  
  #[test]
  fn parse_primary_expr_doubleconst() {
    assert_eq!(primary_expr(&b"0. "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst(0.)));
    assert_eq!(primary_expr(&b"1. "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst(1.)));
    assert_eq!(primary_expr(&b"0.lf "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst(0.)));
    assert_eq!(primary_expr(&b"1.lf "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst(1.)));
    assert_eq!(primary_expr(&b"0.LF "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst(0.)));
    assert_eq!(primary_expr(&b"1.LF "[..]), IResult::Done(&b" "[..], syntax::Expr::DoubleConst(1.)));
  }
  
  #[test]
  fn parse_primary_expr_boolconst() {
    assert_eq!(primary_expr(&b"false "[..]), IResult::Done(&b" "[..], syntax::Expr::BoolConst(false.to_owned())));
    assert_eq!(primary_expr(&b"true "[..]), IResult::Done(&b" "[..], syntax::Expr::BoolConst(true.to_owned())));
  }
  
  #[test]
  fn parse_primary_expr_parens() {
    assert_eq!(primary_expr(&b"(0)"[..]), IResult::Done(&b""[..], syntax::Expr::IntConst(0)));
    assert_eq!(primary_expr(&b"  (  0 ) "[..]), IResult::Done(&b""[..], syntax::Expr::IntConst(0)));
    assert_eq!(primary_expr(&b"  (  .0 ) "[..]), IResult::Done(&b""[..], syntax::Expr::DoubleConst(0.)));
    assert_eq!(primary_expr(&b"  (  (.0) ) "[..]), IResult::Done(&b""[..], syntax::Expr::DoubleConst(0.)));
    assert_eq!(primary_expr(&b"(true) "[..]), IResult::Done(&b""[..], syntax::Expr::BoolConst(true)));
  }
  
  #[test]
  fn parse_postfix_function_call_no_args() {
    let fun = syntax::FunIdentifier::TypeSpecifier(syntax::TypeSpecifier::Vec3);
    let args = Vec::new();
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"vec3()"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(postfix_expr(&b" vec3   (  ) "[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(postfix_expr(&b" vec3   (\nvoid\n) "[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_postfix_function_call_one_arg() {
    let fun = syntax::FunIdentifier::TypeSpecifier(syntax::TypeSpecifier::TypeName("foo".to_owned()));
    let args = vec![syntax::Expr::IntConst(0)];
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"foo(0)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo   ( 0 ) "[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo   (\n0\t\n) "[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_postfix_function_call_multi_arg() {
    let fun = syntax::FunIdentifier::TypeSpecifier(syntax::TypeSpecifier::TypeName("foo".to_owned()));
    let args = vec![
      syntax::Expr::IntConst(0),
      syntax::Expr::BoolConst(false),
      syntax::Expr::Variable("bar".to_owned()),
   ];
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"foo(0, false, bar)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo   ( 0\t, false    ,\t\tbar) "[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_postfix_expr_bracket() {
    let id = syntax::Expr::Variable("foo".to_owned());
    let array_spec = syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(7354)));
    let expected = syntax::Expr::Bracket(Box::new(id), array_spec);
  
    assert_eq!(postfix_expr(&b"foo[7354]"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo[7354]"[..]), IResult::Done(&b""[..], expected));
  }
 
  #[test]
  fn parse_postfix_expr_dot() {
    let foo = Box::new(syntax::Expr::Variable("foo".to_owned()));
    let bar = syntax::FieldSelection {
      field: "bar".to_owned(),
      array_specifier: None,
      next: None
    };
    let expected = syntax::Expr::Dot(foo, bar);

    assert_eq!(postfix_expr(&b"foo.bar;"[..]), IResult::Done(&b";"[..], expected));
  }
  
  #[test]
  fn parse_postfix_expr_dot_several() {
    let foo = Box::new(syntax::Expr::Variable("foo".to_owned()));
    let zoo = syntax::FieldSelection {
      field: "zoo".to_owned(),
      array_specifier: None,
      next: None
    };
    let bar = syntax::FieldSelection {
      field: "bar".to_owned(),
      array_specifier: None,
      next: Some(Box::new(zoo))
    };
    let expected = syntax::Expr::Dot(foo, bar);

    assert_eq!(postfix_expr(&b"foo.bar.zoo;"[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_postfix_postinc() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::PostInc(Box::new(foo));

    assert_eq!(postfix_expr(&b"foo++"[..]), IResult::Done(&b""[..], expected.clone()));
  }

  #[test]
  fn parse_postfix_postdec() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::PostDec(Box::new(foo));

    assert_eq!(postfix_expr(&b"foo--"[..]), IResult::Done(&b""[..], expected.clone()));
  }

  #[test]
  fn parse_unary_add() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Add, Box::new(foo));

    assert_eq!(unary_expr(&b"+foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_minus() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Minus, Box::new(foo));

    assert_eq!(unary_expr(&b"-foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_not() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Not, Box::new(foo));

    assert_eq!(unary_expr(&b"!foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_complement() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Complement, Box::new(foo));

    assert_eq!(unary_expr(&b"~foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_inc() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Inc, Box::new(foo));

    assert_eq!(unary_expr(&b"++foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_dec() {
    let foo = syntax::Expr::Variable("foo".to_owned());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Dec, Box::new(foo));

    assert_eq!(unary_expr(&b"--foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_expr_float() {
    assert_eq!(expr(&b"314.;"[..]), IResult::Done(&b";"[..], syntax::Expr::DoubleConst(314.)));
    assert_eq!(expr(&b"314.f;"[..]), IResult::Done(&b";"[..], syntax::Expr::FloatConst(314.)));
  }

  #[test]
  fn parse_expr_add_2() {
    let one = Box::new(syntax::Expr::IntConst(1));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, one.clone(), one);

    assert_eq!(expr(&b" 1 + 1 ;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(expr(&b"1+1;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(expr(&b"(1 + 1);"[..]), IResult::Done(&b";"[..], expected));
  }
  
  #[test]
  fn parse_expr_add_3() {
    let one = Box::new(syntax::Expr::UIntConst(1));
    let two = Box::new(syntax::Expr::UIntConst(2));
    let three = Box::new(syntax::Expr::UIntConst(3));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, one, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)));

    assert_eq!(expr(&b" 1u + 2u + 3u ;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(expr(&b"1u+2u+3u;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(expr(&b"(1u + (2u + 3u));"[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_expr_add_mult_3() {
    let one = Box::new(syntax::Expr::UIntConst(1));
    let two = Box::new(syntax::Expr::UIntConst(2));
    let three = Box::new(syntax::Expr::UIntConst(3));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, two)), three);

    assert_eq!(expr(&b" 1u * 2u + 3u ;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(expr(&b"1u*2u+3u;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(expr(&b"(1u * 2u) + 3u;"[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_expr_add_sub_mult_div() {
    let one = Box::new(syntax::Expr::IntConst(1));
    let two = Box::new(syntax::Expr::IntConst(2));
    let three = Box::new(syntax::Expr::IntConst(3));
    let four = Box::new(syntax::Expr::IntConst(4));
    let five = Box::new(syntax::Expr::IntConst(5));
    let six = Box::new(syntax::Expr::IntConst(6));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)))), Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, four, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, five, six)))));

    assert_eq!(expr(&b"1 * (2 + 3) + 4 * (5 + 6);"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_function_identifier_typename() {
    let expected = syntax::FunIdentifier::TypeSpecifier(syntax::TypeSpecifier::TypeName("foo".to_owned()));
    assert_eq!(function_identifier(&b"foo"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_function_identifier_cast() {
    let expected = syntax::FunIdentifier::TypeSpecifier(syntax::TypeSpecifier::Vec3);
    assert_eq!(function_identifier(&b"vec3"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_function_def() {
    let rt = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier::IImage2DArray
    };
    let fp = syntax::FunctionPrototype {
      ty: rt,
      name: "foo".to_owned(),
      parameters: Vec::new()
    };
    let expected = syntax::FunctionDefinition {
      prototype: fp,
      statement: syntax::CompoundStatement {
        statement_list: Vec::new()
      }
    };
    let src = b"iimage2DArray foo() {}";

    assert_eq!(function_definition(&src[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_void() {
    assert_eq!(void(&b"void"[..]), IResult::Done(&b""[..], ()));
  }

  #[test]
  fn parse_assignment_op_equal() {
    assert_eq!(assignment_op(&b"= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Equal));
  }

  #[test]
  fn parse_assignment_op_mult() {
    assert_eq!(assignment_op(&b"*= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Mult));
  }

  #[test]
  fn parse_assignment_op_div() {
    assert_eq!(assignment_op(&b"/= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Div));
  }

  #[test]
  fn parse_assignment_op_mod() {
    assert_eq!(assignment_op(&b"%= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Mod));
  }

  #[test]
  fn parse_assignment_op_add() {
    assert_eq!(assignment_op(&b"+= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Add));
  }

  #[test]
  fn parse_assignment_op_sub() {
    assert_eq!(assignment_op(&b"-= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Sub));
  }

  #[test]
  fn parse_assignment_op_lshift() {
    assert_eq!(assignment_op(&b"<<= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::LShift));
  }

  #[test]
  fn parse_assignment_op_rshift() {
    assert_eq!(assignment_op(&b">>= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::RShift));
  }

  #[test]
  fn parse_assignment_op_and() {
    assert_eq!(assignment_op(&b"&= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::And));
  }

  #[test]
  fn parse_assignment_op_xor() {
    assert_eq!(assignment_op(&b"^= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Xor));
  }

  #[test]
  fn parse_assignment_op_or() {
    assert_eq!(assignment_op(&b"|= "[..]), IResult::Done(&b" "[..], syntax::AssignmentOp::Or));
  }

  #[test]
  fn parse_expr_statement() {
    let expected = Some(syntax::Expr::Assignment(Box::new(syntax::Expr::Variable("foo".to_owned())),
                                                 syntax::AssignmentOp::Equal,
                                                 Box::new(syntax::Expr::FloatConst(314.))));

    assert_eq!(expr_statement(&b"foo = 314.f;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(expr_statement(&b"foo=314.f;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(expr_statement(&b"\n\t foo\n\t=  \n314.f\n   ;"[..]), IResult::Done(&b""[..], expected));
  }
}
