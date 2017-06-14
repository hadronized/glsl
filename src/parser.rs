use nom::{ErrorKind, IResult, Needed, alphanumeric, digit};
use std::fmt::Debug;
use std::str::{FromStr, from_utf8_unchecked};

use syntax;

// Turn a &[u8] into a String.
#[inline]
fn bytes_to_string(bytes: &[u8]) -> String {
  unsafe { from_utf8_unchecked(bytes).to_owned() }
}

// /// Parse a natural number.
// #[inline]
// fn natural<T>(s: &[u8]) -> IResult<&[u8], T> where T: FromStr, <T as FromStr>::Err: Debug {
//   let (s1, utf8_s) = unsafe { try_parse!(s, map!(digit, from_utf8_unchecked)) };
//   IResult::Done(s1, utf8_s.parse().unwrap())
// }

/// Parse an identifier.
named!(pub identifier<&[u8], syntax::Identifier>,
  do_parse!(
    name: verify!(take_while1!(identifier_pred), verify_identifier) >>
    (bytes_to_string(name))
  )
);

#[inline]
fn identifier_pred(c: u8) -> bool {
  let ch = char::from(c);
  ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn verify_identifier(s: &[u8]) -> bool {
  !char::from(s[0]).is_digit(10)
}


/// Parse a basic type.
fn basic_ty(i: &[u8]) -> IResult<&[u8], syntax::BasicTy> {
  let (i1, t) = try_parse!(i, alphanumeric);

  match unsafe { from_utf8_unchecked(t) } {
    "bool" => IResult::Done(i1, syntax::BasicTy::Bool),
    "int" => IResult::Done(i1, syntax::BasicTy::Int),
    "uint" => IResult::Done(i1, syntax::BasicTy::UInt),
    "float" => IResult::Done(i1, syntax::BasicTy::Float),
    "double" => IResult::Done(i1, syntax::BasicTy::Double),
    "vec2" => IResult::Done(i1, syntax::BasicTy::Vec2),
    "vec3" => IResult::Done(i1, syntax::BasicTy::Vec3),
    "vec4" => IResult::Done(i1, syntax::BasicTy::Vec4),
    "dvec2" => IResult::Done(i1, syntax::BasicTy::DVec2),
    "dvec3" => IResult::Done(i1, syntax::BasicTy::DVec3),
    "dvec4" => IResult::Done(i1, syntax::BasicTy::DVec4),
    "bvec2" => IResult::Done(i1, syntax::BasicTy::BVec2),
    "bvec3" => IResult::Done(i1, syntax::BasicTy::BVec3),
    "bvec4" => IResult::Done(i1, syntax::BasicTy::BVec4),
    "ivec2" => IResult::Done(i1, syntax::BasicTy::IVec2),
    "ivec3" => IResult::Done(i1, syntax::BasicTy::IVec3),
    "ivec4" => IResult::Done(i1, syntax::BasicTy::IVec4),
    "uvec2" => IResult::Done(i1, syntax::BasicTy::UVec2),
    "uvec3" => IResult::Done(i1, syntax::BasicTy::UVec3),
    "uvec4" => IResult::Done(i1, syntax::BasicTy::UVec4),
    "mat2" => IResult::Done(i1, syntax::BasicTy::Mat2),
    "mat3" => IResult::Done(i1, syntax::BasicTy::Mat3),
    "mat4" => IResult::Done(i1, syntax::BasicTy::Mat4),
    "mat2x2" => IResult::Done(i1, syntax::BasicTy::Mat2),
    "mat2x3" => IResult::Done(i1, syntax::BasicTy::Mat23),
    "mat2x4" => IResult::Done(i1, syntax::BasicTy::Mat24),
    "mat3x2" => IResult::Done(i1, syntax::BasicTy::Mat32),
    "mat3x3" => IResult::Done(i1, syntax::BasicTy::Mat3),
    "mat3x4" => IResult::Done(i1, syntax::BasicTy::Mat34),
    "mat4x2" => IResult::Done(i1, syntax::BasicTy::Mat42),
    "mat4x3" => IResult::Done(i1, syntax::BasicTy::Mat43),
    "mat4x4" => IResult::Done(i1, syntax::BasicTy::Mat4),
    "dmat2" => IResult::Done(i1, syntax::BasicTy::DMat2),
    "dmat3" => IResult::Done(i1, syntax::BasicTy::DMat3),
    "dmat4" => IResult::Done(i1, syntax::BasicTy::DMat4),
    "dmat2x2" => IResult::Done(i1, syntax::BasicTy::DMat2),
    "dmat2x3" => IResult::Done(i1, syntax::BasicTy::DMat23),
    "dmat2x4" => IResult::Done(i1, syntax::BasicTy::DMat24),
    "dmat3x2" => IResult::Done(i1, syntax::BasicTy::DMat32),
    "dmat3x3" => IResult::Done(i1, syntax::BasicTy::DMat3),
    "dmat3x4" => IResult::Done(i1, syntax::BasicTy::DMat34),
    "dmat4x2" => IResult::Done(i1, syntax::BasicTy::DMat42),
    "dmat4x3" => IResult::Done(i1, syntax::BasicTy::DMat43),
    "dmat4x4" => IResult::Done(i1, syntax::BasicTy::DMat4),
    "sampler1D" => IResult::Done(i1, syntax::BasicTy::Sampler1D),
    "image1D" => IResult::Done(i1, syntax::BasicTy::Image1D),
    "sampler2D" => IResult::Done(i1, syntax::BasicTy::Sampler2D),
    "image2D" => IResult::Done(i1, syntax::BasicTy::Image2D),
    "sampler3D" => IResult::Done(i1, syntax::BasicTy::Sampler3D),
    "image3D" => IResult::Done(i1, syntax::BasicTy::Image3D),
    "samplerCube" => IResult::Done(i1, syntax::BasicTy::SamplerCube),
    "imageCube" => IResult::Done(i1, syntax::BasicTy::ImageCube),
    "sampler2DRect" => IResult::Done(i1, syntax::BasicTy::Sampler2DRect),
    "image2DRect" => IResult::Done(i1, syntax::BasicTy::Image2DRect),
    "sampler1DArray" => IResult::Done(i1, syntax::BasicTy::Sampler1DArray),
    "image1DArray" => IResult::Done(i1, syntax::BasicTy::Image1DArray),
    "sampler2DArray" => IResult::Done(i1, syntax::BasicTy::Sampler2DArray),
    "image2DArray" => IResult::Done(i1, syntax::BasicTy::Image2DArray),
    "samplerBuffer" => IResult::Done(i1, syntax::BasicTy::SamplerBuffer),
    "imageBuffer" => IResult::Done(i1, syntax::BasicTy::ImageBuffer),
    "sampler2DMS" => IResult::Done(i1, syntax::BasicTy::Sampler2DMS),
    "image2DMS" => IResult::Done(i1, syntax::BasicTy::Image2DMS),
    "sampler2DMSArray" => IResult::Done(i1, syntax::BasicTy::Sampler2DMSArray),
    "image2DMSArray" => IResult::Done(i1, syntax::BasicTy::Image2DMSArray),
    "samplerCubeArray" => IResult::Done(i1, syntax::BasicTy::SamplerCubeArray),
    "imageCubeArray" => IResult::Done(i1, syntax::BasicTy::ImageCubeArray),
    "sampler1DShadow" => IResult::Done(i1, syntax::BasicTy::Sampler1DShadow),
    "sampler2DShadow" => IResult::Done(i1, syntax::BasicTy::Sampler2DShadow),
    "sampler2DRectShadow" => IResult::Done(i1, syntax::BasicTy::Sampler2DRectShadow),
    "sampler1DArrayShadow" => IResult::Done(i1, syntax::BasicTy::Sampler1DArrayShadow),
    "sampler2DArrayShadow" => IResult::Done(i1, syntax::BasicTy::Sampler2DArrayShadow),
    "samplerCubeShadow" => IResult::Done(i1, syntax::BasicTy::SamplerCubeShadow),
    "samplerCubeArrayShadow" => IResult::Done(i1, syntax::BasicTy::SamplerCubeArrayShadow),
    "isampler1D" => IResult::Done(i1, syntax::BasicTy::ISampler1D),
    "iimage1D" => IResult::Done(i1, syntax::BasicTy::IImage1D),
    "isampler2D" => IResult::Done(i1, syntax::BasicTy::ISampler2D),
    "iimage2D" => IResult::Done(i1, syntax::BasicTy::IImage2D),
    "isampler3D" => IResult::Done(i1, syntax::BasicTy::ISampler3D),
    "iimage3D" => IResult::Done(i1, syntax::BasicTy::IImage3D),
    "isamplerCube" => IResult::Done(i1, syntax::BasicTy::ISamplerCube),
    "iimageCube" => IResult::Done(i1, syntax::BasicTy::IImageCube),
    "isampler2DRect" => IResult::Done(i1, syntax::BasicTy::ISampler2DRect),
    "iimage2DRect" => IResult::Done(i1, syntax::BasicTy::IImage2DRect),
    "isampler1DArray" => IResult::Done(i1, syntax::BasicTy::ISampler1DArray),
    "iimage1DArray" => IResult::Done(i1, syntax::BasicTy::IImage1DArray),
    "isampler2DArray" => IResult::Done(i1, syntax::BasicTy::ISampler2DArray),
    "iimage2DArray" => IResult::Done(i1, syntax::BasicTy::IImage2DArray),
    "isamplerBuffer" => IResult::Done(i1, syntax::BasicTy::ISamplerBuffer),
    "iimageBuffer" => IResult::Done(i1, syntax::BasicTy::IImageBuffer),
    "isampler2MS" => IResult::Done(i1, syntax::BasicTy::ISampler2DMS),
    "iimage2DMS" => IResult::Done(i1, syntax::BasicTy::IIMage2DMS),
    "isampler2DMSArray" => IResult::Done(i1, syntax::BasicTy::ISampler2DMSArray),
    "iimage2DMSArray" => IResult::Done(i1, syntax::BasicTy::IImage2DMSArray),
    "isamplerCubeArray" => IResult::Done(i1, syntax::BasicTy::ISamplerCubeArray),
    "iimageCubeArray" => IResult::Done(i1, syntax::BasicTy::IImageCubeArray),
    "atomic_uint" => IResult::Done(i1, syntax::BasicTy::AtomicUInt),
    "usampler1D" => IResult::Done(i1, syntax::BasicTy::USampler1D),
    "uimage1D" => IResult::Done(i1, syntax::BasicTy::UImage1D),
    "usampler2D" => IResult::Done(i1, syntax::BasicTy::USampler2D),
    "uimage2D" => IResult::Done(i1, syntax::BasicTy::UImage2D),
    "usampler3D" => IResult::Done(i1, syntax::BasicTy::USampler3D),
    "uimage3D" => IResult::Done(i1, syntax::BasicTy::UImage3D),
    "usamplerCube" => IResult::Done(i1, syntax::BasicTy::USamplerCube),
    "uimageCube" => IResult::Done(i1, syntax::BasicTy::UImageCube),
    "usampler2DRect" => IResult::Done(i1, syntax::BasicTy::USampler2DRect),
    "uimage2DRect" => IResult::Done(i1, syntax::BasicTy::UImage2DRect),
    "uisampler1DArray" => IResult::Done(i1, syntax::BasicTy::USampler1DArray),
    "uimage1DArray" => IResult::Done(i1, syntax::BasicTy::UImage1DArray),
    "usampler2DArray" => IResult::Done(i1, syntax::BasicTy::USampler2DArray),
    "uimage2DArray" => IResult::Done(i1, syntax::BasicTy::UImage2DArray),
    "usamplerBuffer" => IResult::Done(i1, syntax::BasicTy::USamplerBuffer),
    "uimageBuffer" => IResult::Done(i1, syntax::BasicTy::UImageBuffer),
    "usampler2DMS" => IResult::Done(i1, syntax::BasicTy::USampler2DMS),
    "uimage2DMS" => IResult::Done(i1, syntax::BasicTy::UImage2DMS),
    "usampler2DMSArray" => IResult::Done(i1, syntax::BasicTy::USampler2DMSArray),
    "uimage2DMSArray" => IResult::Done(i1, syntax::BasicTy::UImage2DMSArray),
    "usamplerCubeArray" => IResult::Done(i1, syntax::BasicTy::USamplerCubeArray),
    "uimageCubeArray" => IResult::Done(i1, syntax::BasicTy::UImageCubeArray),
    _ => IResult::Error(ErrorKind::AlphaNumeric)
  }
}

/// Parse the void type.
named!(pub void_ty<&[u8], ()>, value!((), tag!("void")));

/// Parse a digit that precludes a leading 0.
named!(pub nonzero_digit, verify!(digit, |s:&[u8]| s[0] != b'0'));

/// Parse the unsigned suffix.
named!(pub unsigned_suffix<&[u8], char>, alt!(char!('u') | char!('U')));

/// Parse a decimal literal string.
named!(decimal_lit_,
  do_parse!(
    n: nonzero_digit >>
    opt!(unsigned_suffix) >>
    (n)
  )
);

/// Parse a decimal literal.
named!(pub decimal_lit, recognize!(decimal_lit_));

#[inline]
fn all_octal(s: &[u8]) -> bool {
  s.iter().all(|&c| c >= b'0' && c <= b'7')
}

/// Parse an octal literal string.
named!(octal_lit_,
  do_parse!(
    char!('0') >>
    n: verify!(digit, all_octal) >>
    opt!(unsigned_suffix) >>
    (n)
  )
);

/// Parse an octal literal.
named!(pub octal_lit, recognize!(octal_lit_));

#[inline]
fn all_hexa(s: &[u8]) -> bool {
  s.iter().all(|&c| c >= b'0' && c <= b'9' || c >= b'a' && c <= b'f' || c >= b'A' && c <= b'F')
}

#[inline]
fn alphanumeric_no_u(c: u8) -> bool {
  char::from(c).is_alphanumeric() && c != b'u' && c != b'U'
}

/// Parse an hexadecimal literal string.
named!(hexadecimal_lit_,
  do_parse!(
    alt!(tag!("0x") | tag!("0X")) >>
    n: verify!(take_while!(alphanumeric_no_u), all_hexa) >>
    opt!(unsigned_suffix) >>
    (n)
  )
);

/// Parse an hexadecimal literal.
named!(pub hexadecimal_lit, recognize!(hexadecimal_lit_));

/// Parse a literal integral string.
named!(pub integral_lit,
  alt!(
    decimal_lit |
    octal_lit |
    hexadecimal_lit
  )
);

/// Parse a floating point suffix.
named!(floating_suffix,
  alt!(
    tag!("f") |
    tag!("F") |
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
    do_parse!(digit >> tag!(".") >> (())) |
    do_parse!(digit >> (()))
  )
);

/// Parse a floating point literal string.
named!(floating_lit_<&[u8], ()>,
  do_parse!(floating_frac >> opt!(floating_exponent) >> opt!(floating_suffix) >> (()))
);
  
/// Parse a floating point literal.
named!(pub floating_lit, recognize!(floating_lit_));

/// Parse a struct field declaration.
named!(pub struct_field<&[u8], syntax::StructField>,
  ws!(do_parse!(
    ty: basic_ty >>
    identifiers: many1!(identifier) >>
    (syntax::StructField { ty: ty, identifiers: identifiers })
  ))
);
