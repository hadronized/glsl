// TODO: switch to terminated instead of preceded for spaces to prevent non-necessary try-over

//! GLSL parsers.
//!
//! The more general parser is `translation_unit`, that recognizes the most external form of a GLSL
//! source (a shader, basically).
//!
//! Other parsers are exported if you want more control on how you want to parse your source.

use nom::{Err as NomErr, IResult, ParseTo};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while1};
use nom::character::{is_hex_digit, is_oct_digit};
use nom::character::complete::{char, digit1, multispace0, newline, space0};
use nom::combinator::{map, not, opt, peek, recognize, value, verify};
use nom::error::{ErrorKind, ParseError as _, VerboseError, VerboseErrorKind};
use nom::multi::{many0, many0_count,  many1, separated_list};
use std::num::ParseIntError;
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use std::str::{from_utf8_unchecked};

use crate::syntax;

pub type ParserResult<I, O> = IResult<I, O, VerboseError<I>>;

// A constant parser that just forwards the value it’s parametered with without reading anything
// from the input. Especially useful as “fallback” in an alternative parser.
fn cnst<I, T>(t: T) -> impl Fn(I) -> ParserResult<I, T> where T: Clone {
  move |i| Ok((i, t.clone()))
}

/// Parse a single comment.
pub fn comment(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  alt((
    preceded(tag("//"), terminated(take_until("\n"), newline)),
    preceded(tag("/*"), terminated(take_until("*/"), tag("*/"))),
  ))(i)
}

/// Parse several comments.
pub fn comments(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  recognize(
    many0_count(terminated(comment, multispace0))
  )(i)
}

/// In-between token parser (spaces and comments).
fn blank(i: &[u8]) -> ParserResult<&[u8], ()> {
  value((), comments)(i)
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

#[inline]
fn identifier_pred(c: u8) -> bool {
  let ch = char::from(c);
  ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn verify_identifier(s: &[u8]) -> bool {
  !char::from(s[0]).is_digit(10)
}

/// Parse an identifier (raw version).
fn identifier_str(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  verify(take_while1(identifier_pred), verify_identifier)(i)
}

/// Parse a string that could be used as an identifier.
pub fn string(i: &[u8]) -> ParserResult<&[u8], String> {
  map(identifier_str, bytes_to_string)(i)
}

/// Parse an identifier.
pub fn identifier(i: &[u8]) -> ParserResult<&[u8], syntax::Identifier> {
  map(string, syntax::Identifier)(i)
}

/// Parse a type name.
pub fn type_name(i: &[u8]) -> ParserResult<&[u8], syntax::TypeName> {
  map(string, syntax::TypeName)(i)
}

/// Parse a non-empty list of type names, delimited by comma (,).
fn nonempty_type_names(i: &[u8]) -> ParserResult<&[u8], Vec<syntax::TypeName>> {
  let (i, (first, mut names)) = tuple((
    type_name,
    many0(preceded(char(','), preceded(blank, type_name)))
  ))(i)?;

  names.insert(0, first);
  Ok((i, names))
}

/// Parse a type specifier non struct.
pub fn type_specifier_non_struct(i: &[u8]) -> ParserResult<&[u8], syntax::TypeSpecifierNonArray> {
  let (i1, t) = identifier_str(i)?;

  match unsafe { from_utf8_unchecked(t) } {
    "void" => Ok((i1, syntax::TypeSpecifierNonArray::Void)),
    "bool" => Ok((i1, syntax::TypeSpecifierNonArray::Bool)),
    "int" => Ok((i1, syntax::TypeSpecifierNonArray::Int)),
    "uint" => Ok((i1, syntax::TypeSpecifierNonArray::UInt)),
    "float" => Ok((i1, syntax::TypeSpecifierNonArray::Float)),
    "double" => Ok((i1, syntax::TypeSpecifierNonArray::Double)),
    "vec2" => Ok((i1, syntax::TypeSpecifierNonArray::Vec2)),
    "vec3" => Ok((i1, syntax::TypeSpecifierNonArray::Vec3)),
    "vec4" => Ok((i1, syntax::TypeSpecifierNonArray::Vec4)),
    "dvec2" => Ok((i1, syntax::TypeSpecifierNonArray::DVec2)),
    "dvec3" => Ok((i1, syntax::TypeSpecifierNonArray::DVec3)),
    "dvec4" => Ok((i1, syntax::TypeSpecifierNonArray::DVec4)),
    "bvec2" => Ok((i1, syntax::TypeSpecifierNonArray::BVec2)),
    "bvec3" => Ok((i1, syntax::TypeSpecifierNonArray::BVec3)),
    "bvec4" => Ok((i1, syntax::TypeSpecifierNonArray::BVec4)),
    "ivec2" => Ok((i1, syntax::TypeSpecifierNonArray::IVec2)),
    "ivec3" => Ok((i1, syntax::TypeSpecifierNonArray::IVec3)),
    "ivec4" => Ok((i1, syntax::TypeSpecifierNonArray::IVec4)),
    "uvec2" => Ok((i1, syntax::TypeSpecifierNonArray::UVec2)),
    "uvec3" => Ok((i1, syntax::TypeSpecifierNonArray::UVec3)),
    "uvec4" => Ok((i1, syntax::TypeSpecifierNonArray::UVec4)),
    "mat2" => Ok((i1, syntax::TypeSpecifierNonArray::Mat2)),
    "mat3" => Ok((i1, syntax::TypeSpecifierNonArray::Mat3)),
    "mat4" => Ok((i1, syntax::TypeSpecifierNonArray::Mat4)),
    "mat2x2" => Ok((i1, syntax::TypeSpecifierNonArray::Mat2)),
    "mat2x3" => Ok((i1, syntax::TypeSpecifierNonArray::Mat23)),
    "mat2x4" => Ok((i1, syntax::TypeSpecifierNonArray::Mat24)),
    "mat3x2" => Ok((i1, syntax::TypeSpecifierNonArray::Mat32)),
    "mat3x3" => Ok((i1, syntax::TypeSpecifierNonArray::Mat3)),
    "mat3x4" => Ok((i1, syntax::TypeSpecifierNonArray::Mat34)),
    "mat4x2" => Ok((i1, syntax::TypeSpecifierNonArray::Mat42)),
    "mat4x3" => Ok((i1, syntax::TypeSpecifierNonArray::Mat43)),
    "mat4x4" => Ok((i1, syntax::TypeSpecifierNonArray::Mat4)),
    "dmat2" => Ok((i1, syntax::TypeSpecifierNonArray::DMat2)),
    "dmat3" => Ok((i1, syntax::TypeSpecifierNonArray::DMat3)),
    "dmat4" => Ok((i1, syntax::TypeSpecifierNonArray::DMat4)),
    "dmat2x2" => Ok((i1, syntax::TypeSpecifierNonArray::DMat2)),
    "dmat2x3" => Ok((i1, syntax::TypeSpecifierNonArray::DMat23)),
    "dmat2x4" => Ok((i1, syntax::TypeSpecifierNonArray::DMat24)),
    "dmat3x2" => Ok((i1, syntax::TypeSpecifierNonArray::DMat32)),
    "dmat3x3" => Ok((i1, syntax::TypeSpecifierNonArray::DMat3)),
    "dmat3x4" => Ok((i1, syntax::TypeSpecifierNonArray::DMat34)),
    "dmat4x2" => Ok((i1, syntax::TypeSpecifierNonArray::DMat42)),
    "dmat4x3" => Ok((i1, syntax::TypeSpecifierNonArray::DMat43)),
    "dmat4x4" => Ok((i1, syntax::TypeSpecifierNonArray::DMat4)),
    "sampler1D" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler1D)),
    "image1D" => Ok((i1, syntax::TypeSpecifierNonArray::Image1D)),
    "sampler2D" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2D)),
    "image2D" => Ok((i1, syntax::TypeSpecifierNonArray::Image2D)),
    "sampler3D" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler3D)),
    "image3D" => Ok((i1, syntax::TypeSpecifierNonArray::Image3D)),
    "samplerCube" => Ok((i1, syntax::TypeSpecifierNonArray::SamplerCube)),
    "imageCube" => Ok((i1, syntax::TypeSpecifierNonArray::ImageCube)),
    "sampler2DRect" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DRect)),
    "image2DRect" => Ok((i1, syntax::TypeSpecifierNonArray::Image2DRect)),
    "sampler1DArray" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler1DArray)),
    "image1DArray" => Ok((i1, syntax::TypeSpecifierNonArray::Image1DArray)),
    "sampler2DArray" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DArray)),
    "image2DArray" => Ok((i1, syntax::TypeSpecifierNonArray::Image2DArray)),
    "samplerBuffer" => Ok((i1, syntax::TypeSpecifierNonArray::SamplerBuffer)),
    "imageBuffer" => Ok((i1, syntax::TypeSpecifierNonArray::ImageBuffer)),
    "sampler2DMS" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DMS)),
    "image2DMS" => Ok((i1, syntax::TypeSpecifierNonArray::Image2DMS)),
    "sampler2DMSArray" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DMSArray)),
    "image2DMSArray" => Ok((i1, syntax::TypeSpecifierNonArray::Image2DMSArray)),
    "samplerCubeArray" => Ok((i1, syntax::TypeSpecifierNonArray::SamplerCubeArray)),
    "imageCubeArray" => Ok((i1, syntax::TypeSpecifierNonArray::ImageCubeArray)),
    "sampler1DShadow" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler1DShadow)),
    "sampler2DShadow" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DShadow)),
    "sampler2DRectShadow" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DRectShadow)),
    "sampler1DArrayShadow" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler1DArrayShadow)),
    "sampler2DArrayShadow" => Ok((i1, syntax::TypeSpecifierNonArray::Sampler2DArrayShadow)),
    "samplerCubeShadow" => Ok((i1, syntax::TypeSpecifierNonArray::SamplerCubeShadow)),
    "samplerCubeArrayShadow" => Ok((i1, syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow)),
    "isampler1D" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler1D)),
    "iimage1D" => Ok((i1, syntax::TypeSpecifierNonArray::IImage1D)),
    "isampler2D" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler2D)),
    "iimage2D" => Ok((i1, syntax::TypeSpecifierNonArray::IImage2D)),
    "isampler3D" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler3D)),
    "iimage3D" => Ok((i1, syntax::TypeSpecifierNonArray::IImage3D)),
    "isamplerCube" => Ok((i1, syntax::TypeSpecifierNonArray::ISamplerCube)),
    "iimageCube" => Ok((i1, syntax::TypeSpecifierNonArray::IImageCube)),
    "isampler2DRect" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler2DRect)),
    "iimage2DRect" => Ok((i1, syntax::TypeSpecifierNonArray::IImage2DRect)),
    "isampler1DArray" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler1DArray)),
    "iimage1DArray" => Ok((i1, syntax::TypeSpecifierNonArray::IImage1DArray)),
    "isampler2DArray" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler2DArray)),
    "iimage2DArray" => Ok((i1, syntax::TypeSpecifierNonArray::IImage2DArray)),
    "isamplerBuffer" => Ok((i1, syntax::TypeSpecifierNonArray::ISamplerBuffer)),
    "iimageBuffer" => Ok((i1, syntax::TypeSpecifierNonArray::IImageBuffer)),
    "isampler2DMS" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler2DMS)),
    "iimage2DMS" => Ok((i1, syntax::TypeSpecifierNonArray::IImage2DMS)),
    "isampler2DMSArray" => Ok((i1, syntax::TypeSpecifierNonArray::ISampler2DMSArray)),
    "iimage2DMSArray" => Ok((i1, syntax::TypeSpecifierNonArray::IImage2DMSArray)),
    "isamplerCubeArray" => Ok((i1, syntax::TypeSpecifierNonArray::ISamplerCubeArray)),
    "iimageCubeArray" => Ok((i1, syntax::TypeSpecifierNonArray::IImageCubeArray)),
    "atomic_uint" => Ok((i1, syntax::TypeSpecifierNonArray::AtomicUInt)),
    "usampler1D" => Ok((i1, syntax::TypeSpecifierNonArray::USampler1D)),
    "uimage1D" => Ok((i1, syntax::TypeSpecifierNonArray::UImage1D)),
    "usampler2D" => Ok((i1, syntax::TypeSpecifierNonArray::USampler2D)),
    "uimage2D" => Ok((i1, syntax::TypeSpecifierNonArray::UImage2D)),
    "usampler3D" => Ok((i1, syntax::TypeSpecifierNonArray::USampler3D)),
    "uimage3D" => Ok((i1, syntax::TypeSpecifierNonArray::UImage3D)),
    "usamplerCube" => Ok((i1, syntax::TypeSpecifierNonArray::USamplerCube)),
    "uimageCube" => Ok((i1, syntax::TypeSpecifierNonArray::UImageCube)),
    "usampler2DRect" => Ok((i1, syntax::TypeSpecifierNonArray::USampler2DRect)),
    "uimage2DRect" => Ok((i1, syntax::TypeSpecifierNonArray::UImage2DRect)),
    "usampler1DArray" => Ok((i1, syntax::TypeSpecifierNonArray::USampler1DArray)),
    "uimage1DArray" => Ok((i1, syntax::TypeSpecifierNonArray::UImage1DArray)),
    "usampler2DArray" => Ok((i1, syntax::TypeSpecifierNonArray::USampler2DArray)),
    "uimage2DArray" => Ok((i1, syntax::TypeSpecifierNonArray::UImage2DArray)),
    "usamplerBuffer" => Ok((i1, syntax::TypeSpecifierNonArray::USamplerBuffer)),
    "uimageBuffer" => Ok((i1, syntax::TypeSpecifierNonArray::UImageBuffer)),
    "usampler2DMS" => Ok((i1, syntax::TypeSpecifierNonArray::USampler2DMS)),
    "uimage2DMS" => Ok((i1, syntax::TypeSpecifierNonArray::UImage2DMS)),
    "usampler2DMSArray" => Ok((i1, syntax::TypeSpecifierNonArray::USampler2DMSArray)),
    "uimage2DMSArray" => Ok((i1, syntax::TypeSpecifierNonArray::UImage2DMSArray)),
    "usamplerCubeArray" => Ok((i1, syntax::TypeSpecifierNonArray::USamplerCubeArray)),
    "uimageCubeArray" => Ok((i1, syntax::TypeSpecifierNonArray::UImageCubeArray)),
    _ => {
      let vek = VerboseErrorKind::Context("unknown type specifier non array");
      let ve = VerboseError { errors: vec![(i1, vek)] };
      Err(NomErr::Failure(ve))
    }
  }
}

/// Parse a type specifier (non-array version).
pub fn type_specifier_non_array(i: &[u8]) -> ParserResult<&[u8], syntax::TypeSpecifierNonArray> {
  alt((
    type_specifier_non_struct,
    map(struct_specifier, syntax::TypeSpecifierNonArray::Struct),
    map(type_name, syntax::TypeSpecifierNonArray::TypeName)
  ))(i)
}

/// Parse a type specifier.
pub fn type_specifier(i: &[u8]) -> ParserResult<&[u8], syntax::TypeSpecifier> {
  map(
    pair(type_specifier_non_array, opt(preceded(blank, array_specifier))),
    |(ty, array_specifier)| syntax::TypeSpecifier { ty, array_specifier }
  )(i)
}

/// Parse the void type.
pub fn void(i: &[u8]) -> ParserResult<&[u8], ()> {
  value((), tag("void"))(i)
}

/// Parse a digit that precludes a leading 0.
fn nonzero_digits(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  verify(digit1, |s: &[u8]| s[0] != b'0')(i)
}

#[inline]
fn is_octal(s: &[u8]) -> bool {
  s[0] == b'0' && s.iter().all(|&c| is_oct_digit(c))
}

#[inline]
fn all_hexa(s: &[u8]) -> bool {
  s.iter().all(|&c| is_hex_digit(c))
}

#[inline]
fn alphanumeric_no_u(c: u8) -> bool {
  char::from(c).is_alphanumeric() && c != b'u' && c != b'U'
}

/// Parse an hexadecimal literal.
fn hexadecimal_lit(i: &[u8]) -> ParserResult<&[u8], Result<u32, ParseIntError>> {
  preceded(
    preceded(char('0'), alt((char('x'), char('X')))), // 0x | 0X
    map(
      verify(take_while1(alphanumeric_no_u), all_hexa),
      |i| u32::from_str_radix(bytes_to_str(i), 16)
    )
  )(i)
}

/// Parse an octal literal.
fn octal_lit(i: &[u8]) -> ParserResult<&[u8], Result<u32, ParseIntError>> {
  map(
    verify(take_while1(alphanumeric_no_u), is_octal),
    |i| u32::from_str_radix(bytes_to_str(i), 8)
  )(i)
}

/// Parse a decimal literal.
fn decimal_lit(i: &[u8]) -> ParserResult<&[u8], Result<u32, ParseIntError>> {
  map(nonzero_digits, |i| bytes_to_str(i).parse())(i)
}

/// Parse a literal integral string.
///
/// From the GLSL 4.30 spec:
///
///    "No white space is allowed between the digits of an integer
///     constant, including after the leading 0 or after the leading
///     0x or 0X of a constant, or before the suffix u or U. When
///     tokenizing, the maximal token matching the above will be
///     recognized before a new token is started. When the suffix u or
///     U is present, the literal has type uint, otherwise the type is
///     int. A leading unary minus sign (-) is interpreted as an
///     arithmetic unary negation, not as part of the constant. Hence,
///     literals themselves are always expressed with non-negative
///     syntax, though they could result in a negative value.
///
///     It is a compile-time error to provide a literal integer whose
///     bit pattern cannot fit in 32 bits. The bit pattern of the
///     literal is always used unmodified. So a signed literal whose
///     bit pattern includes a set sign bit creates a negative value."
pub fn integral_lit_try(i: &[u8]) -> ParserResult<&[u8], Result<u32, ParseIntError>> {
  let (i, sign) = opt(char('-'))(i)?;

  map(
    alt((
      octal_lit,
      hexadecimal_lit,
      decimal_lit
    )),
    move |lit| {
      if sign.is_some() {
        lit.map(|v| -(v as i32) as u32)
      } else {
        lit
      }
    }
  )(i)
}

pub fn integral_lit(i: &[u8]) -> ParserResult<&[u8], i32> {
  match integral_lit_try(i) {
    Ok((i, v)) => {
      match v {
        Ok(v) => Ok((i, v as i32)),
        _ => Err(NomErr::Failure(VerboseError::from_error_kind(i, ErrorKind::AlphaNumeric))),
      }
    }

    Err(NomErr::Failure(x)) | Err(NomErr::Error(x)) => Err(NomErr::Failure(x)),

    Err(NomErr::Incomplete(n)) => Err(NomErr::Incomplete(n))
  }
}

/// Parse the unsigned suffix.
fn unsigned_suffix(i: &[u8]) -> ParserResult<&[u8], char> {
  alt((char('u'), char('U')))(i)
}

/// Parse a literal unsigned string.
pub fn unsigned_lit(i: &[u8]) -> ParserResult<&[u8], u32> {
  map(
    terminated(integral_lit, unsigned_suffix),
    |lit| lit as u32
  )(i)
}

/// Parse a floating point suffix.
fn float_suffix(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  alt((tag("f"), tag("F")))(i)
}

/// Parse a double point suffix.
fn double_suffix(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  alt((tag("lf"), tag("LF")))(i)
}

/// Parse the exponent part of a floating point literal.
fn floating_exponent(i: &[u8]) -> ParserResult<&[u8], ()> {
  value(
    (),
    preceded(
      alt((char('e'), char('E'))),
      preceded(
        opt(alt((char('+'), char('-')))),
        digit1
      )
    )
  )(i)
}

/// Parse the fractional constant part of a floating point literal.
fn floating_frac(i: &[u8]) -> ParserResult<&[u8], ()> {
  alt((
    value((), preceded(char('.'), digit1)),
    value((), delimited(digit1, char('.'), opt(digit1))),
  ))(i)
}

/// Parse the « middle » part of a floating value – i.e. fractional and exponential parts.
fn floating_middle(i: &[u8]) -> ParserResult<&[u8], &[u8]> {
  recognize(preceded(floating_frac, opt(floating_exponent)))(i)
}

/// Parse a float literal string.
pub fn float_lit(i: &[u8]) -> ParserResult<&[u8], f32> {
  let (i, (sign, f)) = tuple((
    opt(char('-')),
    terminated(floating_middle, opt(float_suffix))
  ))(i)?;

  // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
  let n = if f[0] == b'.' {
    let mut f_ = f.to_owned();
    f_.insert(0, b'0');

    bytes_to_str(&f_).parse::<f32>().unwrap()
  } else {
    bytes_to_str(f).parse().unwrap()
  };

  // handle the sign and return
  let r = if sign.is_some() { -n } else { n };
  Ok((i, r))
}

/// Parse a double literal string.
pub fn double_lit(i: &[u8]) -> ParserResult<&[u8], f64> {
  let (i, (sign, f)) = tuple((
    opt(char('-')),
    terminated(floating_middle, pair(not(float_suffix), opt(double_suffix)))
  ))(i)?;

  // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
  let n = if f[0] == b'.' {
    let mut f_ = f.to_owned();
    f_.insert(0, b'0');

    bytes_to_str(&f_).parse::<f64>().unwrap()
  } else {
    bytes_to_str(f).parse().unwrap()
  };

  // handle the sign and return
  let r = if sign.is_some() { -n } else { n };
  Ok((i, r))
}

/// Parse a constant boolean.
pub fn bool_lit(i: &[u8]) -> ParserResult<&[u8], bool> {
  alt((
    value(true, tag("true")),
    value(false, tag("false"))
  ))(i)
}

/// Parse a unary operator.
pub fn unary_op(i:&[u8]) -> ParserResult<&[u8], syntax::UnaryOp> {
  alt((
    value(syntax::UnaryOp::Inc, tag("++")),
    value(syntax::UnaryOp::Dec, tag("--")),
    value(syntax::UnaryOp::Add, char('+')),
    value(syntax::UnaryOp::Minus, char('-')),
    value(syntax::UnaryOp::Not, char('!')),
    value(syntax::UnaryOp::Complement, char('~'))

  ))(i)
}

/// Parse an identifier with an optional array specifier.
pub fn arrayed_identifier(i: &[u8]) -> ParserResult<&[u8], syntax::ArrayedIdentifier> {
  map(
    pair(identifier, opt(array_specifier)),
    |(i, a)| syntax::ArrayedIdentifier::new(i, a)
  )(i)
}

/// Parse a struct field declaration.
pub fn struct_field_specifier(i: &[u8]) -> ParserResult<&[u8], syntax::StructFieldSpecifier> {
  let (i, (qualifier, ty, identifiers, _)) = tuple((
    opt(type_qualifier),
    preceded(blank, type_specifier),
    separated_list(delimited(blank, char(','), blank), arrayed_identifier),
    char(';')
  ))(i)?;

  let r = syntax::StructFieldSpecifier {
    qualifier,
    ty,
    identifiers: syntax::NonEmpty(identifiers)
  };

  Ok((i, r))
}

/// Parse a struct.
pub fn struct_specifier(i: &[u8]) -> ParserResult<&[u8], syntax::StructSpecifier> {
  preceded(
    tag("struct"),
    map(
      pair(
        opt(type_name),
        delimited(char('{'), many1(struct_field_specifier), char('}'))
      ),
      |(name, fields)| syntax::StructSpecifier { name, fields: syntax::NonEmpty(fields) }
    )
  )(i)
}

/// Parse a storage qualifier subroutine rule with a list of type names.
pub fn storage_qualifier_subroutine_list(i: &[u8]) -> ParserResult<&[u8], syntax::StorageQualifier> {
  map(
    preceded(
      tag("subroutine"),
      delimited(
        preceded(blank, char('(')),
        preceded(blank, nonempty_type_names),
        preceded(blank, char(')'))
      )
    ),
    syntax::StorageQualifier::Subroutine
  )(i)
}

/// Parse a storage qualifier subroutine rule.
pub fn storage_qualifier_subroutine(i: &[u8]) -> ParserResult<&[u8], syntax::StorageQualifier> {
  alt((
    storage_qualifier_subroutine_list,
    value(syntax::StorageQualifier::Subroutine(Vec::new()), tag("subroutine"))
  ))(i)
}

/// Parse a storage qualifier.
pub fn storage_qualifier(i: &[u8]) -> ParserResult<&[u8], syntax::StorageQualifier> {
  alt((
    value(syntax::StorageQualifier::Const, tag("const")),
    value(syntax::StorageQualifier::InOut, tag("inout")),
    value(syntax::StorageQualifier::In, tag("in")),
    value(syntax::StorageQualifier::Out, tag("out")),
    value(syntax::StorageQualifier::Centroid, tag("centroid")),
    value(syntax::StorageQualifier::Patch, tag("patch")),
    value(syntax::StorageQualifier::Sample, tag("sample")),
    value(syntax::StorageQualifier::Uniform, tag("uniform")),
    value(syntax::StorageQualifier::Buffer, tag("buffer")),
    value(syntax::StorageQualifier::Shared, tag("shared")),
    value(syntax::StorageQualifier::Coherent, tag("coherent")),
    value(syntax::StorageQualifier::Volatile, tag("volatile")),
    value(syntax::StorageQualifier::Restrict, tag("restrict")),
    value(syntax::StorageQualifier::ReadOnly, tag("readonly")),
    value(syntax::StorageQualifier::WriteOnly, tag("writeonly")),
    storage_qualifier_subroutine
  ))(i)
}

/// Parse a layout qualifier.
pub fn layout_qualifier(i: &[u8]) -> ParserResult<&[u8], syntax::LayoutQualifier> {
  preceded(
    tag("layout"),
    delimited(
      preceded(blank, char('(')),
      preceded(blank, layout_qualifier_inner),
      preceded(blank, char(')'))
    )
  )(i)
}

fn layout_qualifier_inner(i: &[u8]) -> ParserResult<&[u8], syntax::LayoutQualifier> {
  map(
    separated_list(delimited(blank, char(','), blank), layout_qualifier_spec),
    |ids| syntax::LayoutQualifier { ids: syntax::NonEmpty(ids) }
  )(i)
}

fn layout_qualifier_spec(i: &[u8]) -> ParserResult<&[u8], syntax::LayoutQualifierSpec> {
  alt((
    value(syntax::LayoutQualifierSpec::Shared, tag("shared")),
    map(
      separated_pair(identifier, char('='), cond_expr),
      |(i, e)| syntax::LayoutQualifierSpec::Identifier(i, Some(Box::new(e)))
    ),
    map(identifier, |i| syntax::LayoutQualifierSpec::Identifier(i, None))
  ))(i)
}

/// Parse a precision qualifier.
pub fn precision_qualifier(i: &[u8]) -> ParserResult<&[u8], syntax::PrecisionQualifier> {
  alt((
    value(syntax::PrecisionQualifier::High, tag("highp")),
    value(syntax::PrecisionQualifier::Medium, tag("mediump")),
    value(syntax::PrecisionQualifier::Low, tag("lowp")),
  ))(i)
}

/// Parse an interpolation qualifier.
pub fn interpolation_qualifier(i: &[u8]) -> ParserResult<&[u8], syntax::InterpolationQualifier> {
  alt((
    value(syntax::InterpolationQualifier::Smooth, tag("smooth")),
    value(syntax::InterpolationQualifier::Flat, tag("flat")),
    value(syntax::InterpolationQualifier::NoPerspective, tag("noperspective")),
  ))(i)
}

/// Parse an invariant qualifier.
pub fn invariant_qualifier(i: &[u8]) -> ParserResult<&[u8], ()> {
  value((), tag("invariant"))(i)
}

/// Parse a precise qualifier.
pub fn precise_qualifier(i: &[u8]) -> ParserResult<&[u8], ()> {
  value((), tag("precise"))(i)
}

/// Parse a type qualifier.
pub fn type_qualifier(i: &[u8]) -> ParserResult<&[u8], syntax::TypeQualifier> {
  map(
    many1(terminated(type_qualifier_spec, blank)),
    |qlfs| syntax::TypeQualifier { qualifiers: syntax::NonEmpty(qlfs) }
  )(i)
}

/// Parse a type qualifier spec.
pub fn type_qualifier_spec(i: &[u8]) -> ParserResult<&[u8], syntax::TypeQualifierSpec> {
  alt((
    map(storage_qualifier, syntax::TypeQualifierSpec::Storage),
    map(layout_qualifier, syntax::TypeQualifierSpec::Layout),
    map(precision_qualifier, syntax::TypeQualifierSpec::Precision),
    map(interpolation_qualifier, syntax::TypeQualifierSpec::Interpolation),
    value(syntax::TypeQualifierSpec::Invariant, invariant_qualifier),
    value(syntax::TypeQualifierSpec::Precise, precise_qualifier),
  ))(i)
}

/// Parse a fully specified type.
pub fn fully_specified_type(i: &[u8]) -> ParserResult<&[u8], syntax::FullySpecifiedType> {
  map(
    pair(
      opt(type_qualifier),
      type_specifier
    ),
    |(qualifier, ty)| syntax::FullySpecifiedType { qualifier, ty }
  )(i)
}

/// Parse an array specifier with no size information.
pub fn array_specifier(i: &[u8]) -> ParserResult<&[u8], syntax::ArraySpecifier> {
  alt((
    value(syntax::ArraySpecifier::Unsized, delimited(char('['), blank, char(']'))),
    map(
      delimited(char('['), cond_expr, char(']')),
      |e| syntax::ArraySpecifier::ExplicitlySized(Box::new(e))
    )
  ))(i)
}

/// Parse a primary expression.
pub fn primary_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  alt((
    parens_expr,
    map(double_lit, syntax::Expr::DoubleConst),
    map(float_lit, syntax::Expr::FloatConst),
    map(unsigned_lit, syntax::Expr::UIntConst),
    map(integral_lit, syntax::Expr::IntConst),
    map(bool_lit, syntax::Expr::BoolConst),
    map(identifier, syntax::Expr::Variable)
  ))(i)
}

/// Parse a postfix expression.
pub fn postfix_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, e) = alt((
    function_call,
    primary_expr
  ))(i)?;

  postfix_part(i, e)
}

// Parse the postfix part of a primary expression. This function will just parse until it cannot
// find any more postfix construct.
fn postfix_part(i: &[u8], e: syntax::Expr) -> ParserResult<&[u8], syntax::Expr> {
  let r = alt((
    map(array_specifier, |a| syntax::Expr::Bracket(Box::new(e.clone()), a)),
    map(dot_field_selection, |i| syntax::Expr::Dot(Box::new(e.clone()), i)),
    value(syntax::Expr::PostInc(Box::new(e.clone())), tag("++")),
    value(syntax::Expr::PostDec(Box::new(e.clone())), tag("--"))
  ))(i);

  match r {
    Ok((i, e)) => postfix_part(i, e),
    Err(NomErr::Error(_)) => Ok((i, e)),
    _ => r
  }
}

/// Parse a unary expression.
pub fn unary_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  alt((
    map(
      separated_pair(
        unary_op,
        blank,
        unary_expr
      ),
      |(op, e)| syntax::Expr::Unary(op, Box::new(e))
    ),
    postfix_expr
  ))(i)
}

/// Parse an expression between parens.
pub fn parens_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  delimited(terminated(char('('), blank), expr, preceded(blank, char(')')))(i)
}

/// Parse a dot field selection identifier.
pub fn dot_field_selection(i: &[u8]) -> ParserResult<&[u8], syntax::Identifier> {
  preceded(char('.'), identifier)(i)
}

/// Parse a declaration.
pub fn declaration(i: &[u8]) -> ParserResult<&[u8], syntax::Declaration> {
  alt((
    map(terminated(function_prototype, char(';')), syntax::Declaration::FunctionPrototype),
    map(terminated(init_declarator_list, char(';')), syntax::Declaration::InitDeclaratorList),
    precision_declaration,
    block_declaration,
    global_declaration
  ))(i)
}

/// Parse a precision declaration.
pub fn precision_declaration(i: &[u8]) -> ParserResult<&[u8], syntax::Declaration> {
  delimited(
    tag("precision"),
    map(
      pair(precision_qualifier, type_specifier),
      |(qual, ty)| syntax::Declaration::Precision(qual, ty)
    ),
    preceded(blank, char(';'))
  )(i)
}

/// Parse a block declaration.
pub fn block_declaration(i: &[u8]) -> ParserResult<&[u8], syntax::Declaration> {
  map(
    tuple((
      type_qualifier,
      preceded(blank, identifier),
      delimited(
        preceded(blank, char('{')),
        many1(preceded(blank, struct_field_specifier)),
        preceded(blank, char('}'))
      ),
      alt((
        value(None, preceded(blank, char(';'))),
        terminated(opt(preceded(blank, arrayed_identifier)), preceded(blank, char(';')))
      ))
    )),
    |(qualifier, name, fields, identifier)| {
      syntax::Declaration::Block(
        syntax::Block { qualifier, name, fields, identifier }
      )
    }
  )(i)
}

/// Parse a global declaration.
pub fn global_declaration(i: &[u8]) -> ParserResult<&[u8], syntax::Declaration> {
  map(
    pair(
      terminated(type_qualifier, blank),
      many0(
        delimited(
          terminated(char(','), blank),
          identifier,
          blank
        )
      )
    ),
    |(qual, idents)| syntax::Declaration::Global(qual, idents)
  )(i)
}

/// Parse a function prototype.
pub fn function_prototype(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionPrototype> {
  terminated(function_declarator, terminated(blank, char(')')))(i)
}

/// Parse an init declarator list.
pub fn init_declarator_list(i: &[u8]) -> ParserResult<&[u8], syntax::InitDeclaratorList> {
  map(
    pair(
      single_declaration,
      many0(
        map(
          tuple((
              preceded(terminated(char(','), blank), terminated(identifier, blank)),
              opt(terminated(array_specifier, blank)),
              opt(delimited(char('='), initializer, blank))
          )),
          |(name, arr_spec, init)| syntax::SingleDeclarationNoType {
            ident: syntax::ArrayedIdentifier::new(name, arr_spec),
            initializer: init
          }
        )
      )
    ),
    |(head, tail)| syntax::InitDeclaratorList { head, tail }
  )(i)
}

/// Parse a single declaration.
pub fn single_declaration(i: &[u8]) -> ParserResult<&[u8], syntax::SingleDeclaration> {
  let (i, ty) = fully_specified_type(i)?;
  let ty_ = ty.clone();

  alt((
    map(
      tuple((
        terminated(identifier, blank),
        opt(terminated(array_specifier, blank)),
        opt(delimited(terminated(char('='), blank), initializer, blank))
      )),
      move |(name, array_specifier, initializer)| syntax::SingleDeclaration {
        ty: ty_.clone(),
        name: Some(name),
        array_specifier,
        initializer
      }
    ),
    cnst(
      syntax::SingleDeclaration {
        ty,
        name: None,
        array_specifier: None,
        initializer: None
      }
    )
  ))(i)
}

/// Parse an initializer.
pub fn initializer(i: &[u8]) -> ParserResult<&[u8], syntax::Initializer> {
  alt((
    map(assignment_expr, |e| syntax::Initializer::Simple(Box::new(e))),
    map(
      delimited(
        terminated(char('{'), blank),
        terminated(initializer_list, terminated(blank, opt(terminated(char(','), blank)))),
        char('}')
      ),
      |il| syntax::Initializer::List(syntax::NonEmpty(il))
    )
  ))(i)
}

/// Parse an initializer list.
pub fn initializer_list(i: &[u8]) -> ParserResult<&[u8], Vec<syntax::Initializer>> {
  separated_list(delimited(blank, char(','), blank), initializer)(i)
}

fn function_declarator(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionPrototype> {
  alt((
    function_header_with_parameters,
    map(
      function_header,
      |(ty, name)| syntax::FunctionPrototype { ty, name, parameters: Vec::new() }
    )
  ))(i)
}

fn function_header(i: &[u8]) -> ParserResult<&[u8], (syntax::FullySpecifiedType, syntax::Identifier)> {
  pair(
    terminated(fully_specified_type, blank),
    terminated(identifier, terminated(blank, char('(')))
  )(i)
}

fn function_header_with_parameters(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionPrototype> {
  map(
    pair(
      function_header,
      separated_list(delimited(blank, char(','), blank), function_parameter_declaration)
    ),
    |(header, parameters)| syntax::FunctionPrototype {
      ty: header.0,
      name: header.1,
      parameters
    }
  )(i)
}

fn function_parameter_declaration(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionParameterDeclaration> {
  alt((
    function_parameter_declaration_named,
    function_parameter_declaration_unnamed
  ))(i)
}

fn function_parameter_declaration_named(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionParameterDeclaration> {
  map(
    pair(
      opt(terminated(type_qualifier, blank)),
      function_parameter_declarator
    ),
    |(ty_qual, fpd)| syntax::FunctionParameterDeclaration::Named(ty_qual, fpd)
  )(i)
}

fn function_parameter_declaration_unnamed(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionParameterDeclaration> {
  map(
    pair(
      opt(terminated(type_qualifier, blank)),
     type_specifier
    ),
    |(ty_qual, ty_spec)| syntax::FunctionParameterDeclaration::Unnamed(ty_qual, ty_spec)
  )(i)
}

fn function_parameter_declarator(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionParameterDeclarator> {
  map(
    tuple((
      terminated(type_specifier, blank),
      terminated(identifier, blank),
      opt(array_specifier)
    )),
    |(ty, name, a)| syntax::FunctionParameterDeclarator {
      ty,
      ident: syntax::ArrayedIdentifier::new(name, a)
    }
  )(i)
}

/// Parse a function call.
pub fn function_call(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  alt((
    function_call_header_no_parameter,
    function_call_header_with_parameters
  ))(i)
}

fn function_call_header_no_parameter(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  map(
    terminated(function_call_header, terminated(blank, terminated(opt(void), terminated(blank, char(')'))))),
    |fi| syntax::Expr::FunCall(fi, Vec::new())
  )(i)
}

fn function_call_header_with_parameters(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  map(
    pair(
      terminated(function_call_header, blank),
      separated_list(terminated(char(','), blank), terminated(assignment_expr, blank))
    ),
    |(fi, args)| syntax::Expr::FunCall(fi, args)
  )(i)
}

fn function_call_header(i: &[u8]) -> ParserResult<&[u8], syntax::FunIdentifier> {
  terminated(function_identifier, terminated(blank, char('(')))(i)
}

/// Parse a function identifier just behind a function list argument.
pub fn function_identifier(i: &[u8]) -> ParserResult<&[u8], syntax::FunIdentifier> {
  alt((
    map(
      terminated(identifier, terminated(blank, peek(char('(')))),
      |ident| syntax::FunIdentifier::Identifier(ident)
    ),
    |i| {
      let (i, e) = primary_expr(i)?;
      postfix_part(i, e).map(|(i, pfe)| (i, syntax::FunIdentifier::Expr(Box::new(pfe))))
    }
  ))(i)
}

/// Parse the most general expression.
pub fn expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, first) = assignment_expr(i)?;
  let first_ = first.clone();

  alt((
    map(
      preceded(terminated(char(','), blank), expr),
      move |next| syntax::Expr::Comma(Box::new(first_.clone()), Box::new(next))
    ),
    cnst(first)
  ))(i)
}

/// Parse an assignment expression.
pub fn assignment_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  alt((
    map(
      tuple((
        terminated(unary_expr, blank),
        terminated(assignment_op, blank),
        assignment_expr
      )),
      |(e, o, v)| syntax::Expr::Assignment(Box::new(e), o, Box::new(v))
    ),
    cond_expr
  ))(i)
}

/// Parse an assignment operator.
pub fn assignment_op(i: &[u8]) -> ParserResult<&[u8], syntax::AssignmentOp> {
  alt((
    value(syntax::AssignmentOp::Equal, char('=')),
    value(syntax::AssignmentOp::Mult, tag("*=")),
    value(syntax::AssignmentOp::Div, tag("/=")),
    value(syntax::AssignmentOp::Mod, tag("%=")),
    value(syntax::AssignmentOp::Add, tag("+=")),
    value(syntax::AssignmentOp::Sub, tag("-=")),
    value(syntax::AssignmentOp::LShift, tag("<<=")),
    value(syntax::AssignmentOp::RShift, tag(">>=")),
    value(syntax::AssignmentOp::And, tag("&=")),
    value(syntax::AssignmentOp::Xor, tag("^=")),
    value(syntax::AssignmentOp::Or, tag("|="))
  ))(i)
}

/// Parse a conditional expression.
pub fn cond_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(logical_or_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      tuple((
        terminated(char('?'), blank),
        terminated(expr, blank),
        terminated(char(':'), blank),
        assignment_expr
      )),
      move |(_, b, _, c)| syntax::Expr::Ternary(Box::new(a_.clone()), Box::new(b), Box::new(c))
    ),
    cnst(a)
  ))(i)
}

/// Parse a logical OR expression.
pub fn logical_or_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(logical_xor_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(terminated(tag("||"), blank), logical_or_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::Or, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a logical XOR expression.
pub fn logical_xor_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(logical_and_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(terminated(tag("^^"), blank), logical_xor_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::Xor, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a logical AND expression.
pub fn logical_and_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(inclusive_or_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(terminated(tag("&&"), blank), logical_and_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::And, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a bitwise OR expression.
pub fn inclusive_or_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(exclusive_or_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(terminated(char('|'), blank), inclusive_or_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::BitOr, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a bitwise XOR expression.
pub fn exclusive_or_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(and_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(terminated(char('^'), blank), exclusive_or_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::BitXor, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a bitwise AND expression.
pub fn and_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(equality_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(terminated(char('&'), blank), and_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::BitAnd, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse an equality expression.
pub fn equality_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(rel_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        terminated(
          alt((
            value(syntax::BinaryOp::Equal, tag("==")),
            value(syntax::BinaryOp::NonEqual, tag("!="))
          )),
          blank
        ),
        equality_expr
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a relational expression.
pub fn rel_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(shift_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        terminated(
          alt((
            value(syntax::BinaryOp::LTE, tag("<=")),
            value(syntax::BinaryOp::GTE, tag(">=")),
            value(syntax::BinaryOp::LT, char('<')),
            value(syntax::BinaryOp::GT, char('>')),
          )),
          blank
        ),
        rel_expr
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a shift expression.
pub fn shift_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(additive_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        terminated(
          alt((
            value(syntax::BinaryOp::LShift, tag("<<")),
            value(syntax::BinaryOp::RShift, tag(">>"))
          )),
          blank
        ),
        shift_expr
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse an additive expression.
pub fn additive_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(multiplicative_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        terminated(
          alt((
            value(syntax::BinaryOp::Add, char('+')),
            value(syntax::BinaryOp::Sub, char('-'))
          )),
          blank
        ),
        additive_expr
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a multiplicative expression.
pub fn multiplicative_expr(i: &[u8]) -> ParserResult<&[u8], syntax::Expr> {
  let (i, a) = terminated(unary_expr, blank)(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        terminated(
          alt((
            value(syntax::BinaryOp::Mult, char('*')),
            value(syntax::BinaryOp::Div, char('/')),
            value(syntax::BinaryOp::Mod, char('%'))
          )),
          blank
        ),
       multiplicative_expr
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b))
    ),
    cnst(a)
  ))(i)
}

/// Parse a simple statement.
pub fn simple_statement(i: &[u8]) -> ParserResult<&[u8], syntax::SimpleStatement> {
  alt((
    map(jump_statement, syntax::SimpleStatement::Jump),
    map(iteration_statement, syntax::SimpleStatement::Iteration),
    map(case_label, syntax::SimpleStatement::CaseLabel),
    map(switch_statement, syntax::SimpleStatement::Switch),
    map(selection_statement, syntax::SimpleStatement::Selection),
    map(declaration, syntax::SimpleStatement::Declaration),
    map(expr_statement, syntax::SimpleStatement::Expression)
  ))(i)
}

/// Parse an expression statement.
pub fn expr_statement(i: &[u8]) -> ParserResult<&[u8], syntax::ExprStatement> {
  terminated(terminated(opt(expr), blank), char(';'))(i)
}

/// Parse a selection statement.
pub fn selection_statement(i: &[u8]) -> ParserResult<&[u8], syntax::SelectionStatement> {
  map(
    tuple((
        terminated(tag("if"), blank),
        terminated(char('('), blank),
        terminated(expr, blank),
        terminated(char(')'), blank),
        selection_rest_statement
    )),
    |(_, _, cond_expr, _, rest)| syntax::SelectionStatement {
      cond: Box::new(cond_expr),
      rest
    }
  )(i)
}

fn selection_rest_statement(i: &[u8]) -> ParserResult<&[u8], syntax::SelectionRestStatement> {
  let (i, st) = statement(i)?;
  let st_ = st.clone();

  alt((
    map(
      preceded(
        terminated(tag("else"), blank),
        statement
      ),
      move |rest| syntax::SelectionRestStatement::Else(Box::new(st_.clone()), Box::new(rest))
    ),
    cnst(syntax::SelectionRestStatement::Statement(Box::new(st)))
  ))(i)
}

/// Parse a switch statement.
pub fn switch_statement(i: &[u8]) -> ParserResult<&[u8], syntax::SwitchStatement> {
  map(
    tuple((
      terminated(tag("switch"), blank),
      terminated(char('('), blank),
      terminated(expr, blank),
      terminated(char(')'), blank),
      terminated(char('{'), blank),
      many0(terminated(statement, blank)),
      char('}')
    )),
    |(_, _, head, _, _, body, _)| syntax::SwitchStatement { head: Box::new(head), body }
  )(i)
}

/// Parse a case label.
pub fn case_label(i: &[u8]) -> ParserResult<&[u8], syntax::CaseLabel> {
  alt((
    map(
      delimited(
        terminated(tag("case"), blank),
        terminated(expr, blank),
        char(':')
      ),
      |e| syntax::CaseLabel::Case(Box::new(e))
    ),
    value(
      syntax::CaseLabel::Def,
      preceded(terminated(tag("default"), blank), char(':'))
    )
  ))(i)
}

/// Parse an iteration statement.
pub fn iteration_statement(i: &[u8]) -> ParserResult<&[u8], syntax::IterationStatement> {
  alt((
    iteration_statement_while,
    iteration_statement_do_while,
    iteration_statement_for
  ))(i)
}

/// Parse a while statement.
pub fn iteration_statement_while(i: &[u8]) -> ParserResult<&[u8], syntax::IterationStatement> {
  map(
    tuple((
      terminated(tag("while"), blank),
      terminated(char('('), blank),
      terminated(condition, blank),
      terminated(char(')'), blank),
      statement
    )),
    |(_, _, cond, _, st)| syntax::IterationStatement::While(cond, Box::new(st))
  )(i)
}

/// Parse a while statement.
pub fn iteration_statement_do_while(i: &[u8]) -> ParserResult<&[u8], syntax::IterationStatement> {
  map(
    tuple((
      terminated(tag("do"), blank),
      terminated(statement, blank),
      terminated(tag("while"), blank),
      terminated(char('('), blank),
      terminated(expr, blank),
      terminated(char(')'), blank),
      char(';')
    )),
    |(_, st, _, _, e, _, _)| syntax::IterationStatement::DoWhile(Box::new(st), Box::new(e))
  )(i)
}

// Parse a for statement.
pub fn iteration_statement_for(i: &[u8]) -> ParserResult<&[u8], syntax::IterationStatement> {
  map(
    tuple((
      terminated(tag("for"), blank),
      terminated(char('('), blank),
      terminated(iteration_statement_for_init_statement, blank),
      terminated(iteration_statement_for_rest_statement, blank),
      terminated(char(')'), blank),
      statement
    )),
    |(_, _, head, rest, _, body)| syntax::IterationStatement::For(head, rest, Box::new(body))
  )(i)
}

fn iteration_statement_for_init_statement(i: &[u8]) -> ParserResult<&[u8], syntax::ForInitStatement> {
  alt((
    map(expr_statement, syntax::ForInitStatement::Expression),
    map(declaration, |d| syntax::ForInitStatement::Declaration(Box::new(d)))
  ))(i)
}

fn iteration_statement_for_rest_statement(i: &[u8]) -> ParserResult<&[u8], syntax::ForRestStatement> {
  map(
    separated_pair(
      opt(terminated(condition, blank)),
      terminated(char(';'), blank),
      opt(expr)
    ),
    |(condition, e)| syntax::ForRestStatement { condition, post_expr: e.map(Box::new) }
  )(i)
}

/// Parse a jump statement.
pub fn jump_statement(i: &[u8]) -> ParserResult<&[u8], syntax::JumpStatement> {
  alt((
    jump_statement_continue,
    jump_statement_break,
    jump_statement_return,
    jump_statement_discard
  ))(i)
}

// Parse a continue statement.
pub fn jump_statement_continue(i: &[u8]) -> ParserResult<&[u8], syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Continue,
    terminated(tag("continue"), terminated(blank, char(';')))
  )(i)
}

// Parse a break statement.
pub fn jump_statement_break(i: &[u8]) -> ParserResult<&[u8], syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Break,
    terminated(tag("break"), terminated(blank, char(';')))
  )(i)
}

// Parse a discard statement.
pub fn jump_statement_discard(i: &[u8]) -> ParserResult<&[u8], syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Discard,
    terminated(tag("discard"), terminated(blank, char(';')))
  )(i)
}

// Parse a return statement.
pub fn jump_statement_return(i: &[u8]) -> ParserResult<&[u8], syntax::JumpStatement> {
  map(
    delimited(
      terminated(tag("return"), blank),
      terminated(expr, blank),
      char(';')
    ),
    |e| syntax::JumpStatement::Return(Box::new(e))
  )(i)
}

/// Parse a condition.
pub fn condition(i: &[u8]) -> ParserResult<&[u8], syntax::Condition> {
  alt((
    map(expr, |e| syntax::Condition::Expr(Box::new(e))),
    condition_assignment
  ))(i)
}

fn condition_assignment(i: &[u8]) -> ParserResult<&[u8], syntax::Condition> {
  map(
    tuple((
      terminated(fully_specified_type, blank),
      terminated(identifier, blank),
      terminated(char('='), blank),
      initializer
    )),
    |(ty, id, _, ini)| syntax::Condition::Assignment(ty, id, ini)
  )(i)
}

/// Parse a statement.
pub fn statement(i: &[u8]) -> ParserResult<&[u8], syntax::Statement> {
  alt((
    map(compound_statement, |c| syntax::Statement::Compound(Box::new(c))),
    map(simple_statement, |s| syntax::Statement::Simple(Box::new(s))),
  ))(i)
}

/// Parse a compound statement.
pub fn compound_statement(i: &[u8]) -> ParserResult<&[u8], syntax::CompoundStatement> {
  map(
    delimited(
      terminated(char('{'), blank),
      many0(terminated(statement, blank)),
      char('}')
    ),
    |statement_list| syntax::CompoundStatement { statement_list }
  )(i)
}

/// Parse a function definition.
pub fn function_definition(i: &[u8]) -> ParserResult<&[u8], syntax::FunctionDefinition> {
  map(
    pair(
      terminated(function_prototype, blank),
      compound_statement
    ),
    |(prototype, statement)| syntax::FunctionDefinition { prototype, statement }
  )(i)
}

/// Parse an external declaration.
pub fn external_declaration(i: &[u8]) -> ParserResult<&[u8], syntax::ExternalDeclaration> {
  alt((
    map(preprocessor, syntax::ExternalDeclaration::Preprocessor),
    map(function_definition, syntax::ExternalDeclaration::FunctionDefinition),
    map(declaration, syntax::ExternalDeclaration::Declaration),
  ))(i)
}

/// Parse a translation unit (entry point).
pub fn translation_unit(i: &[u8]) -> ParserResult<&[u8], syntax::TranslationUnit> {
  map(
    many1(terminated(external_declaration, blank)),
    |eds| syntax::TranslationUnit(syntax::NonEmpty(eds))
  )(i)
}

/// Parse a preprocessor command.
pub fn preprocessor(i: &[u8]) -> ParserResult<&[u8], syntax::Preprocessor> {
  alt((
    map(pp_define, syntax::Preprocessor::Define),
    map(pp_version, syntax::Preprocessor::Version),
    map(pp_extension, syntax::Preprocessor::Extension),
  ))(i)
}

/// Parse a #version number.
pub fn pp_version_number(i: &[u8]) -> ParserResult<&[u8], u16> {
  map(digit1, |x: &[u8]| x.parse_to().unwrap())(i)
}

/// Parse a #version profile.
pub fn pp_version_profile(i: &[u8]) -> ParserResult<&[u8], syntax::PreprocessorVersionProfile> {
  alt((
    value(syntax::PreprocessorVersionProfile::Core, tag("core")),
    value(syntax::PreprocessorVersionProfile::Compatibility, tag("compatibility")),
    value(syntax::PreprocessorVersionProfile::ES, tag("es")),
  ))(i)
}

/// Parse a #define
pub fn pp_define(i: &[u8]) -> ParserResult<&[u8], syntax::PreprocessorDefine> {
  map(
    tuple((
      terminated(char('#'), space0),
      terminated(tag("define"), space0),
      terminated(identifier, space0),
      terminated(primary_expr, space0),
      newline
    )),
    |(_, _, name, value, _)| syntax::PreprocessorDefine { name, value }
  )(i)
}

/// Parse a #version.
pub fn pp_version(i: &[u8]) -> ParserResult<&[u8], syntax::PreprocessorVersion> {
  map(
    tuple((
      terminated(char('#'), space0),
      terminated(tag("version"), space0),
      terminated(pp_version_number, space0),
      opt(terminated(pp_version_profile, space0)),
      newline
    )),
    |(_, _, version, profile, _)| syntax::PreprocessorVersion { version, profile }
  )(i)
}

/// Parse an #extension name.
pub fn pp_extension_name(i: &[u8]) -> ParserResult<&[u8], syntax::PreprocessorExtensionName> {
  alt((
    value(syntax::PreprocessorExtensionName::All, tag("all")),
    map(string, syntax::PreprocessorExtensionName::Specific)
  ))(i)
}

/// Parse an #extension behavior.
pub fn pp_extension_behavior(i: &[u8]) -> ParserResult<&[u8], syntax::PreprocessorExtensionBehavior> {
  alt((
    value(syntax::PreprocessorExtensionBehavior::Require, tag("require")),
    value(syntax::PreprocessorExtensionBehavior::Enable, tag("enable")),
    value(syntax::PreprocessorExtensionBehavior::Warn, tag("warn")),
    value(syntax::PreprocessorExtensionBehavior::Disable, tag("disable")),
  ))(i)
}

/// Parse an #extension.
pub fn pp_extension(i: &[u8]) -> ParserResult<&[u8], syntax::PreprocessorExtension> {
  map(
    tuple((
      terminated(char('#'), space0),
      terminated(tag("extension"), space0),
      terminated(pp_extension_name, space0),
      opt(preceded(terminated(char(':'), space0), terminated(pp_extension_behavior, space0))),
      newline
    )),
    |(_, _, name, behavior, _)| syntax::PreprocessorExtension { name, behavior }
  )(i)
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::parser;

  // Run a parser and print errors if any and then panic.
  fn run_parser<P, T>(
    source: &[u8],
    parser: P
  ) -> Result<T, parser::ParseError>
  where P: FnOnce(&[u8]) -> ParserResult<&[u8], T> {
    match parser::run_parser(source, parser) {
      Ok(x) => Ok(x),
      Err(e) => {
        eprintln!("{}", e.info);
        Err(e)
      }
    }
  }

  #[test]
  fn parse_uniline_comment() {
    assert_eq!(comment(&b"// lol\nfoo"[..]), Ok((&b"foo"[..], &b" lol"[..])));
  }

  #[test]
  fn parse_multiline_comment() {
    assert_eq!(comment(&b"/* lol\nfoo\n*/bar"[..]), Ok((&b"bar"[..], &b" lol\nfoo\n"[..])));
  }

  #[test]
  fn parse_unsigned_suffix() {
    assert_eq!(unsigned_suffix(&b"u"[..]), Ok((&b""[..], 'u')));
    assert_eq!(unsigned_suffix(&b"U"[..]), Ok((&b""[..], 'U')));
  }

  #[test]
  fn parse_nonzero_digits() {
    assert_eq!(nonzero_digits(&b"3"[..]), Ok((&b""[..], &b"3"[..])));
    assert_eq!(nonzero_digits(&b"12345953"[..]), Ok((&b""[..], &b"12345953"[..])));
  }

  #[test]
  fn parse_decimal_lit() {
    assert_eq!(decimal_lit(&b"3"[..]), Ok((&b""[..], Ok(3))));
    assert_eq!(decimal_lit(&b"3 "[..]), Ok((&b" "[..], Ok(3))));
    assert_eq!(decimal_lit(&b"13 "[..]), Ok((&b" "[..], Ok(13))));
    assert_eq!(decimal_lit(&b"42 "[..]), Ok((&b" "[..], Ok(42))));
    assert_eq!(decimal_lit(&b"123456 "[..]), Ok((&b" "[..], Ok(123456))));
  }

  #[test]
  fn parse_octal_lit() {
    assert_eq!(octal_lit(&b"0 "[..]), Ok((&b" "[..], Ok(0o0))));
    assert_eq!(octal_lit(&b"03 "[..]), Ok((&b" "[..], Ok(0o3))));
    assert_eq!(octal_lit(&b"012 "[..]), Ok((&b" "[..], Ok(0o12))));
    assert_eq!(octal_lit(&b"07654321 "[..]), Ok((&b" "[..], Ok(0o7654321))));
  }

  #[test]
  fn parse_hexadecimal_lit() {
    assert_eq!(hexadecimal_lit(&b"0x3 "[..]), Ok((&b" "[..], Ok(0x3))));
    assert_eq!(hexadecimal_lit(&b"0x0123789"[..]), Ok((&b""[..], Ok(0x0123789))));
    assert_eq!(hexadecimal_lit(&b"0xABCDEF"[..]), Ok((&b""[..], Ok(0xabcdef))));
    assert_eq!(hexadecimal_lit(&b"0xabcdef"[..]), Ok((&b""[..], Ok(0xabcdef))));
  }

  #[test]
  fn parse_integral_lit() {
    assert_eq!(integral_lit(&b"0"[..]), Ok((&b""[..], 0)));
    assert_eq!(integral_lit(&b"3"[..]), Ok((&b""[..], 3)));
    assert_eq!(integral_lit(&b"3 "[..]), Ok((&b" "[..], 3)));
    assert_eq!(integral_lit(&b"03 "[..]), Ok((&b" "[..], 3)));
    assert_eq!(integral_lit(&b"076556 "[..]), Ok((&b" "[..], 0o76556)));
    assert_eq!(integral_lit(&b"012 "[..]), Ok((&b" "[..], 0o12)));
    assert_eq!(integral_lit(&b"0x3 "[..]), Ok((&b" "[..], 0x3)));
    assert_eq!(integral_lit(&b"0x9ABCDEF"[..]), Ok((&b""[..], 0x9ABCDEF)));
    assert_eq!(integral_lit(&b"0x9ABCDEF"[..]), Ok((&b""[..], 0x9ABCDEF)));
    assert_eq!(integral_lit(&b"0x9abcdef"[..]), Ok((&b""[..], 0x9abcdef)));
    assert_eq!(integral_lit(&b"0x9abcdef"[..]), Ok((&b""[..], 0x9abcdef)));
    assert!(integral_lit(&b"\n1"[..]).is_err());
    assert!(integral_lit(&b"\n0x1"[..]).is_err());
    assert!(integral_lit(&b"\n01"[..]).is_err());
    assert!(integral_lit(&b"0xfffffffff"[..]).is_err());
    assert_eq!(integral_lit(&b"0xffffffff"[..]), Ok((&b""[..], 0xffffffffu32 as i32)));
  }

  #[test]
  fn parse_integral_neg_lit() {
    assert_eq!(integral_lit(&b"-3"[..]), Ok((&b""[..], -3)));
    assert_eq!(integral_lit(&b"-3 "[..]), Ok((&b" "[..], -3)));
    assert_eq!(integral_lit(&b"-03 "[..]), Ok((&b" "[..], -3)));
    assert_eq!(integral_lit(&b"-076556 "[..]), Ok((&b" "[..], -0o76556)));
    assert_eq!(integral_lit(&b"-012 "[..]), Ok((&b" "[..], -0o12)));
    assert_eq!(integral_lit(&b"-0x3 "[..]), Ok((&b" "[..], -0x3)));
    assert_eq!(integral_lit(&b"-0x9ABCDEF"[..]), Ok((&b""[..], -0x9ABCDEF)));
    assert_eq!(integral_lit(&b"-0x9ABCDEF"[..]), Ok((&b""[..], -0x9ABCDEF)));
    assert_eq!(integral_lit(&b"-0x9abcdef"[..]), Ok((&b""[..], -0x9abcdef)));
    assert_eq!(integral_lit(&b"-0x9abcdef"[..]), Ok((&b""[..], -0x9abcdef)));
  }

  #[test]
  fn parse_unsigned_lit() {
    assert_eq!(unsigned_lit(&b"0xffffffffU"[..]), Ok((&b""[..], 0xffffffff as u32)));
    assert_eq!(unsigned_lit(&b"-1u"[..]), Ok((&b""[..], 0xffffffff as u32)));
    assert!(unsigned_lit(&b"0xfffffffffU"[..]).is_err());
  }

  #[test]
  fn parse_float_lit() {
    assert_eq!(float_lit(&b"0.;"[..]), Ok((&b";"[..], 0.)));
    assert_eq!(float_lit(&b".0;"[..]), Ok((&b";"[..], 0.)));
    assert_eq!(float_lit(&b".035 "[..]), Ok((&b" "[..], 0.035)));
    assert_eq!(float_lit(&b"0. "[..]), Ok((&b" "[..], 0.)));
    assert_eq!(float_lit(&b"0.035 "[..]), Ok((&b" "[..], 0.035)));
    assert_eq!(float_lit(&b".035f"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(float_lit(&b"0.f"[..]), Ok((&b""[..], 0.)));
    assert_eq!(float_lit(&b"314.f"[..]), Ok((&b""[..], 314.)));
    assert_eq!(float_lit(&b"0.035f"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(float_lit(&b".035F"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(float_lit(&b"0.F"[..]), Ok((&b""[..], 0.)));
    assert_eq!(float_lit(&b"0.035F"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(float_lit(&b"1.03e+34 "[..]), Ok((&b" "[..], 1.03e+34)));
    assert_eq!(float_lit(&b"1.03E+34 "[..]), Ok((&b" "[..], 1.03E+34)));
    assert_eq!(float_lit(&b"1.03e-34 "[..]), Ok((&b" "[..], 1.03e-34)));
    assert_eq!(float_lit(&b"1.03E-34 "[..]), Ok((&b" "[..], 1.03E-34)));
    assert_eq!(float_lit(&b"1.03e+34f"[..]), Ok((&b""[..], 1.03e+34)));
    assert_eq!(float_lit(&b"1.03E+34f"[..]), Ok((&b""[..], 1.03E+34)));
    assert_eq!(float_lit(&b"1.03e-34f"[..]), Ok((&b""[..], 1.03e-34)));
    assert_eq!(float_lit(&b"1.03E-34f"[..]), Ok((&b""[..], 1.03E-34)));
    assert_eq!(float_lit(&b"1.03e+34F"[..]), Ok((&b""[..], 1.03e+34)));
    assert_eq!(float_lit(&b"1.03E+34F"[..]), Ok((&b""[..], 1.03E+34)));
    assert_eq!(float_lit(&b"1.03e-34F"[..]), Ok((&b""[..], 1.03e-34)));
    assert_eq!(float_lit(&b"1.03E-34F"[..]), Ok((&b""[..], 1.03E-34)));
  }

  #[test]
  fn parse_float_neg_lit() {
    assert_eq!(float_lit(&b"-.035 "[..]), Ok((&b" "[..], -0.035)));
    assert_eq!(float_lit(&b"-0. "[..]), Ok((&b" "[..], -0.)));
    assert_eq!(float_lit(&b"-0.035 "[..]), Ok((&b" "[..], -0.035)));
    assert_eq!(float_lit(&b"-.035f"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(float_lit(&b"-0.f"[..]), Ok((&b""[..], -0.)));
    assert_eq!(float_lit(&b"-0.035f"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(float_lit(&b"-.035F"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(float_lit(&b"-0.F"[..]), Ok((&b""[..], -0.)));
    assert_eq!(float_lit(&b"-0.035F"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(float_lit(&b"-1.03e+34 "[..]), Ok((&b" "[..], -1.03e+34)));
    assert_eq!(float_lit(&b"-1.03E+34 "[..]), Ok((&b" "[..], -1.03E+34)));
    assert_eq!(float_lit(&b"-1.03e-34 "[..]), Ok((&b" "[..], -1.03e-34)));
    assert_eq!(float_lit(&b"-1.03E-34 "[..]), Ok((&b" "[..], -1.03E-34)));
    assert_eq!(float_lit(&b"-1.03e+34f"[..]), Ok((&b""[..], -1.03e+34)));
    assert_eq!(float_lit(&b"-1.03E+34f"[..]), Ok((&b""[..], -1.03E+34)));
    assert_eq!(float_lit(&b"-1.03e-34f"[..]), Ok((&b""[..], -1.03e-34)));
    assert_eq!(float_lit(&b"-1.03E-34f"[..]), Ok((&b""[..], -1.03E-34)));
    assert_eq!(float_lit(&b"-1.03e+34F"[..]), Ok((&b""[..], -1.03e+34)));
    assert_eq!(float_lit(&b"-1.03E+34F"[..]), Ok((&b""[..], -1.03E+34)));
    assert_eq!(float_lit(&b"-1.03e-34F"[..]), Ok((&b""[..], -1.03e-34)));
    assert_eq!(float_lit(&b"-1.03E-34F"[..]), Ok((&b""[..], -1.03E-34)));
  }

  #[test]
  fn parse_double_lit() {
    assert_eq!(double_lit(&b"0.;"[..]), Ok((&b";"[..], 0.)));
    assert_eq!(double_lit(&b".0;"[..]), Ok((&b";"[..], 0.)));
    assert_eq!(double_lit(&b".035 "[..]), Ok((&b" "[..], 0.035)));
    assert_eq!(double_lit(&b"0. "[..]), Ok((&b" "[..], 0.)));
    assert_eq!(double_lit(&b"0.035 "[..]), Ok((&b" "[..], 0.035)));
    assert_eq!(double_lit(&b"0.lf"[..]), Ok((&b""[..], 0.)));
    assert_eq!(double_lit(&b"0.035lf"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(double_lit(&b".035lf"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(double_lit(&b".035LF"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(double_lit(&b"0.LF"[..]), Ok((&b""[..], 0.)));
    assert_eq!(double_lit(&b"0.035LF"[..]), Ok((&b""[..], 0.035)));
    assert_eq!(double_lit(&b"1.03e+34lf"[..]), Ok((&b""[..], 1.03e+34)));
    assert_eq!(double_lit(&b"1.03E+34lf"[..]), Ok((&b""[..], 1.03E+34)));
    assert_eq!(double_lit(&b"1.03e-34lf"[..]), Ok((&b""[..], 1.03e-34)));
    assert_eq!(double_lit(&b"1.03E-34lf"[..]), Ok((&b""[..], 1.03E-34)));
    assert_eq!(double_lit(&b"1.03e+34LF"[..]), Ok((&b""[..], 1.03e+34)));
    assert_eq!(double_lit(&b"1.03E+34LF"[..]), Ok((&b""[..], 1.03E+34)));
    assert_eq!(double_lit(&b"1.03e-34LF"[..]), Ok((&b""[..], 1.03e-34)));
    assert_eq!(double_lit(&b"1.03E-34LF"[..]), Ok((&b""[..], 1.03E-34)));
  }

  #[test]
  fn parse_double_neg_lit() {
    assert_eq!(double_lit(&b"-0.;"[..]), Ok((&b";"[..], -0.)));
    assert_eq!(double_lit(&b"-.0;"[..]), Ok((&b";"[..], -0.)));
    assert_eq!(double_lit(&b"-.035 "[..]), Ok((&b" "[..], -0.035)));
    assert_eq!(double_lit(&b"-0. "[..]), Ok((&b" "[..], -0.)));
    assert_eq!(double_lit(&b"-0.035 "[..]), Ok((&b" "[..], -0.035)));
    assert_eq!(double_lit(&b"-0.lf"[..]), Ok((&b""[..], -0.)));
    assert_eq!(double_lit(&b"-0.035lf"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(double_lit(&b"-.035lf"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(double_lit(&b"-.035LF"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(double_lit(&b"-0.LF"[..]), Ok((&b""[..], -0.)));
    assert_eq!(double_lit(&b"-0.035LF"[..]), Ok((&b""[..], -0.035)));
    assert_eq!(double_lit(&b"-1.03e+34lf"[..]), Ok((&b""[..], -1.03e+34)));
    assert_eq!(double_lit(&b"-1.03E+34lf"[..]), Ok((&b""[..], -1.03E+34)));
    assert_eq!(double_lit(&b"-1.03e-34lf"[..]), Ok((&b""[..], -1.03e-34)));
    assert_eq!(double_lit(&b"-1.03E-34lf"[..]), Ok((&b""[..], -1.03E-34)));
    assert_eq!(double_lit(&b"-1.03e+34LF"[..]), Ok((&b""[..], -1.03e+34)));
    assert_eq!(double_lit(&b"-1.03E+34LF"[..]), Ok((&b""[..], -1.03E+34)));
    assert_eq!(double_lit(&b"-1.03e-34LF"[..]), Ok((&b""[..], -1.03e-34)));
    assert_eq!(double_lit(&b"-1.03E-34LF"[..]), Ok((&b""[..], -1.03E-34)));
  }

  #[test]
  fn parse_bool_lit() {
    assert_eq!(run_parser(&b"false"[..], bool_lit), Ok(false));
    assert_eq!(run_parser(&b"true"[..], bool_lit), Ok(true));
  }

  #[test]
  fn parse_identifier() {
    assert_eq!(identifier(&b"a"[..]), Ok((&b""[..], "a".into())));
    assert_eq!(identifier(&b"ab_cd"[..]), Ok((&b""[..], "ab_cd".into())));
    assert_eq!(identifier(&b"Ab_cd"[..]), Ok((&b""[..], "Ab_cd".into())));
    assert_eq!(identifier(&b"Ab_c8d"[..]), Ok((&b""[..], "Ab_c8d".into())));
    assert_eq!(identifier(&b"Ab_c8d9"[..]), Ok((&b""[..], "Ab_c8d9".into())));
  }

  #[test]
  fn parse_unary_op_add() {
    assert_eq!(unary_op(&b"+ "[..]), Ok((&b" "[..], syntax::UnaryOp::Add)));
  }

  #[test]
  fn parse_unary_op_minus() {
    assert_eq!(unary_op(&b"- "[..]), Ok((&b" "[..], syntax::UnaryOp::Minus)));
  }

  #[test]
  fn parse_unary_op_not() {
    assert_eq!(unary_op(&b"!"[..]), Ok((&b""[..], syntax::UnaryOp::Not)));
  }

  #[test]
  fn parse_unary_op_complement() {
    assert_eq!(unary_op(&b"~"[..]), Ok((&b""[..], syntax::UnaryOp::Complement)));
  }

  #[test]
  fn parse_unary_op_inc() {
    assert_eq!(unary_op(&b"++"[..]), Ok((&b""[..], syntax::UnaryOp::Inc)));
  }

  #[test]
  fn parse_unary_op_dec() {
    assert_eq!(unary_op(&b"--"[..]), Ok((&b""[..], syntax::UnaryOp::Dec)));
  }

  #[test]
  fn parse_array_specifier_unsized() {
    assert_eq!(array_specifier(&b"[]"[..]), Ok((&b""[..], syntax::ArraySpecifier::Unsized)));
    assert_eq!(array_specifier(&b"[ ]"[..]), Ok((&b""[..], syntax::ArraySpecifier::Unsized)));
    assert_eq!(array_specifier(&b"  [\n]"[..]), Ok((&b""[..], syntax::ArraySpecifier::Unsized)));
  }

  #[test]
  fn parse_array_specifier_sized() {
    let ix = syntax::Expr::IntConst(0);

    assert_eq!(array_specifier(&b"[0]"[..]), Ok((&b""[..], syntax::ArraySpecifier::ExplicitlySized(Box::new(ix.clone())))));
    assert_eq!(array_specifier(&b"[\n0   \t]"[..]), Ok((&b""[..], syntax::ArraySpecifier::ExplicitlySized(Box::new(ix)))));
  }

  #[test]
  fn parse_precise_qualifier() {
    assert_eq!(precise_qualifier(&b"precise "[..]), Ok((&b" "[..], ())));
  }

  #[test]
  fn parse_invariant_qualifier() {
    assert_eq!(invariant_qualifier(&b"invariant "[..]), Ok((&b" "[..], ())));
  }

  #[test]
  fn parse_interpolation_qualifier() {
    assert_eq!(interpolation_qualifier(&b"smooth "[..]), Ok((&b" "[..], syntax::InterpolationQualifier::Smooth)));
    assert_eq!(interpolation_qualifier(&b"flat "[..]), Ok((&b" "[..], syntax::InterpolationQualifier::Flat)));
    assert_eq!(interpolation_qualifier(&b"noperspective "[..]), Ok((&b" "[..], syntax::InterpolationQualifier::NoPerspective)));
  }

  #[test]
  fn parse_precision_qualifier() {
    assert_eq!(precision_qualifier(&b"highp "[..]), Ok((&b" "[..], syntax::PrecisionQualifier::High)));
    assert_eq!(precision_qualifier(&b"mediump "[..]), Ok((&b" "[..], syntax::PrecisionQualifier::Medium)));
    assert_eq!(precision_qualifier(&b"lowp "[..]), Ok((&b" "[..], syntax::PrecisionQualifier::Low)));
  }

  #[test]
  fn parse_storage_qualifier() {
    assert_eq!(storage_qualifier(&b"const "[..]), Ok((&b" "[..], syntax::StorageQualifier::Const)));
    assert_eq!(storage_qualifier(&b"inout "[..]), Ok((&b" "[..], syntax::StorageQualifier::InOut)));
    assert_eq!(storage_qualifier(&b"in "[..]), Ok((&b" "[..], syntax::StorageQualifier::In)));
    assert_eq!(storage_qualifier(&b"out "[..]), Ok((&b" "[..], syntax::StorageQualifier::Out)));
    assert_eq!(storage_qualifier(&b"centroid "[..]), Ok((&b" "[..], syntax::StorageQualifier::Centroid)));
    assert_eq!(storage_qualifier(&b"patch "[..]), Ok((&b" "[..], syntax::StorageQualifier::Patch)));
    assert_eq!(storage_qualifier(&b"sample "[..]), Ok((&b" "[..], syntax::StorageQualifier::Sample)));
    assert_eq!(storage_qualifier(&b"uniform "[..]), Ok((&b" "[..], syntax::StorageQualifier::Uniform)));
    assert_eq!(storage_qualifier(&b"buffer "[..]), Ok((&b" "[..], syntax::StorageQualifier::Buffer)));
    assert_eq!(storage_qualifier(&b"shared "[..]), Ok((&b" "[..], syntax::StorageQualifier::Shared)));
    assert_eq!(storage_qualifier(&b"coherent "[..]), Ok((&b" "[..], syntax::StorageQualifier::Coherent)));
    assert_eq!(storage_qualifier(&b"volatile "[..]), Ok((&b" "[..], syntax::StorageQualifier::Volatile)));
    assert_eq!(storage_qualifier(&b"restrict "[..]), Ok((&b" "[..], syntax::StorageQualifier::Restrict)));
    assert_eq!(storage_qualifier(&b"readonly "[..]), Ok((&b" "[..], syntax::StorageQualifier::ReadOnly)));
    assert_eq!(storage_qualifier(&b"writeonly "[..]), Ok((&b" "[..], syntax::StorageQualifier::WriteOnly)));
    assert_eq!(storage_qualifier(&b"subroutine a"[..]), Ok((&b" a"[..], syntax::StorageQualifier::Subroutine(vec![]))));

    let a = syntax::TypeName("vec3".to_owned());
    let b = syntax::TypeName("float".to_owned());
    let c = syntax::TypeName("dmat43".to_owned());
    let types = vec![a, b, c];
    assert_eq!(storage_qualifier(&b"subroutine (vec3, float, dmat43)"[..]), Ok((&b""[..], syntax::StorageQualifier::Subroutine(types))));
  }

  #[test]
  fn parse_layout_qualifier_std430() {
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Identifier("std430".into(), None)])
    };

    assert_eq!(layout_qualifier(&b"layout (std430)"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b" layout  (std430   )"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b" layout \n\t (  std430  )"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b" layout(std430)"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_layout_qualifier_shared() {
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Shared])
    };

    assert_eq!(layout_qualifier(&b"layout (shared)"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b"   layout ( shared )"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b"   layout(shared)"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_layout_qualifier_list() {
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".into(), Some(Box::new(syntax::Expr::IntConst(3))));
    let expected = syntax::LayoutQualifier { ids: syntax::NonEmpty(vec![id_0, id_1, id_2]) };

    assert_eq!(layout_qualifier(&b"layout (shared, std140, max_vertices = 3)"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b"layout(shared,std140,max_vertices=3)"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(layout_qualifier(&b"   layout\n\n\t (    shared , std140, max_vertices= 3)"[..]), Ok((&b""[..], expected.clone())));
  }

  #[test]
  fn parse_type_qualifier() {
    let storage_qual = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Const);
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".into(), Some(Box::new(syntax::Expr::IntConst(3))));
    let layout_qual = syntax::TypeQualifierSpec::Layout(syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![id_0, id_1, id_2])
    });
    let expected = syntax::TypeQualifier { qualifiers: syntax::NonEmpty(vec![storage_qual, layout_qual]) };

    assert_eq!(type_qualifier(&b"const layout (shared, std140, max_vertices = 3)"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(type_qualifier(&b"    const layout(shared,std140,max_vertices=3)"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_struct_field_specifier() {
    let expected = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["foo".into()])
    };

    assert_eq!(struct_field_specifier(&b"vec4 foo;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(struct_field_specifier(&b"  vec4     foo ; "[..]), Ok((&b""[..], expected.clone())));
  }

  #[test]
  fn parse_struct_field_specifier_type_name() {
    let expected = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("S0238_3".into()),
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["x".into()])
    };

    assert_eq!(struct_field_specifier(&b"S0238_3 x;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(struct_field_specifier(&b"  S0238_3     x ; "[..]), Ok((&b""[..], expected.clone())));
  }

  #[test]
  fn parse_struct_field_specifier_several() {
    let expected = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["foo".into(), "bar".into(), "zoo".into()])
    };

    assert_eq!(struct_field_specifier(&b"vec4 foo, bar, zoo;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(struct_field_specifier(&b"  vec4     foo , bar  , zoo ; "[..]), Ok((&b""[..], expected.clone())));
  }

  #[test]
  fn parse_struct_specifier_one_field() {
    let field = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["foo".into()])
    };
    let expected = syntax::StructSpecifier {
      name: Some("TestStruct".into()),
      fields: syntax::NonEmpty(vec![field])
    };

    assert_eq!(struct_specifier(&b"struct TestStruct { vec4 foo; }"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(struct_specifier(&b"   struct      TestStruct \n \n\n {\n    vec4   foo  ;\n }"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_struct_specifier_multi_fields() {
    let a = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["foo".into()])
    };
    let b = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["bar".into()])
    };
    let c = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::UInt,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["zoo".into()])
    };
    let d = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::BVec3,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["foo_BAR_zoo3497_34".into()])
    };
    let e = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("S0238_3".into()),
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["x".into()])
    };
    let expected = syntax::StructSpecifier {
      name: Some("_TestStruct_934i".into()),
      fields: syntax::NonEmpty(vec![a, b, c, d, e])
    };

    assert_eq!(struct_specifier(&b"struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(struct_specifier(&b"struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(struct_specifier(&b"   struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_type_specifier_non_array() {
    assert_eq!(type_specifier_non_array(&b"bool"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Bool)));
    assert_eq!(type_specifier_non_array(&b"int"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Int)));
    assert_eq!(type_specifier_non_array(&b"uint"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UInt)));
    assert_eq!(type_specifier_non_array(&b"float"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Float)));
    assert_eq!(type_specifier_non_array(&b"double"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Double)));
    assert_eq!(type_specifier_non_array(&b"vec2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Vec2)));
    assert_eq!(type_specifier_non_array(&b"vec3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Vec3)));
    assert_eq!(type_specifier_non_array(&b"vec4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Vec4)));
    assert_eq!(type_specifier_non_array(&b"dvec2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DVec2)));
    assert_eq!(type_specifier_non_array(&b"dvec3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DVec3)));
    assert_eq!(type_specifier_non_array(&b"dvec4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DVec4)));
    assert_eq!(type_specifier_non_array(&b"bvec2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::BVec2)));
    assert_eq!(type_specifier_non_array(&b"bvec3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::BVec3)));
    assert_eq!(type_specifier_non_array(&b"bvec4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::BVec4)));
    assert_eq!(type_specifier_non_array(&b"ivec2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IVec2)));
    assert_eq!(type_specifier_non_array(&b"ivec3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IVec3)));
    assert_eq!(type_specifier_non_array(&b"ivec4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IVec4)));
    assert_eq!(type_specifier_non_array(&b"uvec2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UVec2)));
    assert_eq!(type_specifier_non_array(&b"uvec3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UVec3)));
    assert_eq!(type_specifier_non_array(&b"uvec4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UVec4)));
    assert_eq!(type_specifier_non_array(&b"mat2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat2)));
    assert_eq!(type_specifier_non_array(&b"mat3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat3)));
    assert_eq!(type_specifier_non_array(&b"mat4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat4)));
    assert_eq!(type_specifier_non_array(&b"mat2x2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat2)));
    assert_eq!(type_specifier_non_array(&b"mat2x3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat23)));
    assert_eq!(type_specifier_non_array(&b"mat2x4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat24)));
    assert_eq!(type_specifier_non_array(&b"mat3x2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat32)));
    assert_eq!(type_specifier_non_array(&b"mat3x3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat3)));
    assert_eq!(type_specifier_non_array(&b"mat3x4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat34)));
    assert_eq!(type_specifier_non_array(&b"mat4x2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat42)));
    assert_eq!(type_specifier_non_array(&b"mat4x3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat43)));
    assert_eq!(type_specifier_non_array(&b"mat4x4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Mat4)));
    assert_eq!(type_specifier_non_array(&b"dmat2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat2)));
    assert_eq!(type_specifier_non_array(&b"dmat3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat3)));
    assert_eq!(type_specifier_non_array(&b"dmat4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat4)));
    assert_eq!(type_specifier_non_array(&b"dmat2x2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat2)));
    assert_eq!(type_specifier_non_array(&b"dmat2x3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat23)));
    assert_eq!(type_specifier_non_array(&b"dmat2x4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat24)));
    assert_eq!(type_specifier_non_array(&b"dmat3x2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat32)));
    assert_eq!(type_specifier_non_array(&b"dmat3x3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat3)));
    assert_eq!(type_specifier_non_array(&b"dmat3x4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat34)));
    assert_eq!(type_specifier_non_array(&b"dmat4x2"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat42)));
    assert_eq!(type_specifier_non_array(&b"dmat4x3"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat43)));
    assert_eq!(type_specifier_non_array(&b"dmat4x4"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::DMat4)));
    assert_eq!(type_specifier_non_array(&b"sampler1D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler1D)));
    assert_eq!(type_specifier_non_array(&b"image1D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image1D)));
    assert_eq!(type_specifier_non_array(&b"sampler2D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2D)));
    assert_eq!(type_specifier_non_array(&b"image2D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image2D)));
    assert_eq!(type_specifier_non_array(&b"sampler3D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler3D)));
    assert_eq!(type_specifier_non_array(&b"image3D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image3D)));
    assert_eq!(type_specifier_non_array(&b"samplerCube"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::SamplerCube)));
    assert_eq!(type_specifier_non_array(&b"imageCube"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ImageCube)));
    assert_eq!(type_specifier_non_array(&b"sampler2DRect"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DRect)));
    assert_eq!(type_specifier_non_array(&b"image2DRect"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image2DRect)));
    assert_eq!(type_specifier_non_array(&b"sampler1DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler1DArray)));
    assert_eq!(type_specifier_non_array(&b"image1DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image1DArray)));
    assert_eq!(type_specifier_non_array(&b"sampler2DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DArray)));
    assert_eq!(type_specifier_non_array(&b"image2DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image2DArray)));
    assert_eq!(type_specifier_non_array(&b"samplerBuffer"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::SamplerBuffer)));
    assert_eq!(type_specifier_non_array(&b"imageBuffer"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ImageBuffer)));
    assert_eq!(type_specifier_non_array(&b"sampler2DMS"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DMS)));
    assert_eq!(type_specifier_non_array(&b"image2DMS"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image2DMS)));
    assert_eq!(type_specifier_non_array(&b"sampler2DMSArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DMSArray)));
    assert_eq!(type_specifier_non_array(&b"image2DMSArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Image2DMSArray)));
    assert_eq!(type_specifier_non_array(&b"samplerCubeArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::SamplerCubeArray)));
    assert_eq!(type_specifier_non_array(&b"imageCubeArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ImageCubeArray)));
    assert_eq!(type_specifier_non_array(&b"sampler1DShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler1DShadow)));
    assert_eq!(type_specifier_non_array(&b"sampler2DShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DShadow)));
    assert_eq!(type_specifier_non_array(&b"sampler2DRectShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DRectShadow)));
    assert_eq!(type_specifier_non_array(&b"sampler1DArrayShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler1DArrayShadow)));
    assert_eq!(type_specifier_non_array(&b"sampler2DArrayShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::Sampler2DArrayShadow)));
    assert_eq!(type_specifier_non_array(&b"samplerCubeShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::SamplerCubeShadow)));
    assert_eq!(type_specifier_non_array(&b"samplerCubeArrayShadow"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow)));
    assert_eq!(type_specifier_non_array(&b"isampler1D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler1D)));
    assert_eq!(type_specifier_non_array(&b"iimage1D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage1D)));
    assert_eq!(type_specifier_non_array(&b"isampler2D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler2D)));
    assert_eq!(type_specifier_non_array(&b"iimage2D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage2D)));
    assert_eq!(type_specifier_non_array(&b"isampler3D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler3D)));
    assert_eq!(type_specifier_non_array(&b"iimage3D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage3D)));
    assert_eq!(type_specifier_non_array(&b"isamplerCube"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISamplerCube)));
    assert_eq!(type_specifier_non_array(&b"iimageCube"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImageCube)));
    assert_eq!(type_specifier_non_array(&b"isampler2DRect"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler2DRect)));
    assert_eq!(type_specifier_non_array(&b"iimage2DRect"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage2DRect)));
    assert_eq!(type_specifier_non_array(&b"isampler1DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler1DArray)));
    assert_eq!(type_specifier_non_array(&b"iimage1DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage1DArray)));
    assert_eq!(type_specifier_non_array(&b"isampler2DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler2DArray)));
    assert_eq!(type_specifier_non_array(&b"iimage2DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage2DArray)));
    assert_eq!(type_specifier_non_array(&b"isamplerBuffer"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISamplerBuffer)));
    assert_eq!(type_specifier_non_array(&b"iimageBuffer"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImageBuffer)));
    assert_eq!(type_specifier_non_array(&b"isampler2DMS"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler2DMS)));
    assert_eq!(type_specifier_non_array(&b"iimage2DMS"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage2DMS)));
    assert_eq!(type_specifier_non_array(&b"isampler2DMSArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISampler2DMSArray)));
    assert_eq!(type_specifier_non_array(&b"iimage2DMSArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImage2DMSArray)));
    assert_eq!(type_specifier_non_array(&b"isamplerCubeArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::ISamplerCubeArray)));
    assert_eq!(type_specifier_non_array(&b"iimageCubeArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::IImageCubeArray)));
    assert_eq!(type_specifier_non_array(&b"atomic_uint"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::AtomicUInt)));
    assert_eq!(type_specifier_non_array(&b"usampler1D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler1D)));
    assert_eq!(type_specifier_non_array(&b"uimage1D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage1D)));
    assert_eq!(type_specifier_non_array(&b"usampler2D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler2D)));
    assert_eq!(type_specifier_non_array(&b"uimage2D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage2D)));
    assert_eq!(type_specifier_non_array(&b"usampler3D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler3D)));
    assert_eq!(type_specifier_non_array(&b"uimage3D"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage3D)));
    assert_eq!(type_specifier_non_array(&b"usamplerCube"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USamplerCube)));
    assert_eq!(type_specifier_non_array(&b"uimageCube"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImageCube)));
    assert_eq!(type_specifier_non_array(&b"usampler2DRect"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler2DRect)));
    assert_eq!(type_specifier_non_array(&b"uimage2DRect"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage2DRect)));
    assert_eq!(type_specifier_non_array(&b"usampler1DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler1DArray)));
    assert_eq!(type_specifier_non_array(&b"uimage1DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage1DArray)));
    assert_eq!(type_specifier_non_array(&b"usampler2DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler2DArray)));
    assert_eq!(type_specifier_non_array(&b"uimage2DArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage2DArray)));
    assert_eq!(type_specifier_non_array(&b"usamplerBuffer"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USamplerBuffer)));
    assert_eq!(type_specifier_non_array(&b"uimageBuffer"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImageBuffer)));
    assert_eq!(type_specifier_non_array(&b"usampler2DMS"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler2DMS)));
    assert_eq!(type_specifier_non_array(&b"uimage2DMS"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage2DMS)));
    assert_eq!(type_specifier_non_array(&b"usampler2DMSArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USampler2DMSArray)));
    assert_eq!(type_specifier_non_array(&b"uimage2DMSArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImage2DMSArray)));
    assert_eq!(type_specifier_non_array(&b"usamplerCubeArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::USamplerCubeArray)));
    assert_eq!(type_specifier_non_array(&b"uimageCubeArray"[..]), Ok((&b""[..], syntax::TypeSpecifierNonArray::UImageCubeArray)));
    assert_eq!(type_specifier_non_array(
      &b"ReturnType"[..]),
      Ok((&b""[..], syntax::TypeSpecifierNonArray::TypeName(syntax::TypeName::new("ReturnType").unwrap()))
    ));
  }

  #[test]
  fn parse_type_specifier() {
    assert_eq!(type_specifier(&b"uint;"[..]), Ok((&b";"[..], syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::UInt,
      array_specifier: None
    })));
    assert_eq!(type_specifier(&b"iimage2DMSArray[35];"[..]), Ok((&b";"[..], syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: Some(syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(35))))
    })));
  }

  #[test]
  fn parse_fully_specified_type() {
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: None
    };
    let expected = syntax::FullySpecifiedType { qualifier: None, ty };

    assert_eq!(fully_specified_type(&b"iimage2DMSArray;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_fully_specified_type_with_qualifier() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Subroutine(vec!["vec2".into(), "S032_29k".into()]));
    let qual = syntax::TypeQualifier { qualifiers: syntax::NonEmpty(vec![qual_spec]) };
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: None
    };
    let expected = syntax::FullySpecifiedType { qualifier: Some(qual), ty };

    assert_eq!(fully_specified_type(&b"subroutine (vec2, S032_29k) iimage2DMSArray;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(fully_specified_type(&b"  subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray ;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(fully_specified_type(&b"subroutine(vec2,S032_29k)iimage2DMSArray;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_primary_expr_intconst() {
    assert_eq!(primary_expr(&b"0 "[..]), Ok((&b" "[..], syntax::Expr::IntConst(0))));
    assert_eq!(primary_expr(&b"1 "[..]), Ok((&b" "[..], syntax::Expr::IntConst(1))));
  }

  #[test]
  fn parse_primary_expr_uintconst() {
    assert_eq!(primary_expr(&b"0u "[..]), Ok((&b" "[..], syntax::Expr::UIntConst(0))));
    assert_eq!(primary_expr(&b"1u "[..]), Ok((&b" "[..], syntax::Expr::UIntConst(1))));
  }

  #[test]
  fn parse_primary_expr_floatconst() {
    assert_eq!(primary_expr(&b"0.f "[..]), Ok((&b" "[..], syntax::Expr::FloatConst(0.))));
    assert_eq!(primary_expr(&b"1.f "[..]), Ok((&b" "[..], syntax::Expr::FloatConst(1.))));
    assert_eq!(primary_expr(&b"0.F "[..]), Ok((&b" "[..], syntax::Expr::FloatConst(0.))));
    assert_eq!(primary_expr(&b"1.F "[..]), Ok((&b" "[..], syntax::Expr::FloatConst(1.))));
  }

  #[test]
  fn parse_primary_expr_doubleconst() {
    assert_eq!(primary_expr(&b"0. "[..]), Ok((&b" "[..], syntax::Expr::DoubleConst(0.))));
    assert_eq!(primary_expr(&b"1. "[..]), Ok((&b" "[..], syntax::Expr::DoubleConst(1.))));
    assert_eq!(primary_expr(&b"0.lf "[..]), Ok((&b" "[..], syntax::Expr::DoubleConst(0.))));
    assert_eq!(primary_expr(&b"1.lf "[..]), Ok((&b" "[..], syntax::Expr::DoubleConst(1.))));
    assert_eq!(primary_expr(&b"0.LF "[..]), Ok((&b" "[..], syntax::Expr::DoubleConst(0.))));
    assert_eq!(primary_expr(&b"1.LF "[..]), Ok((&b" "[..], syntax::Expr::DoubleConst(1.))));
  }

  #[test]
  fn parse_primary_expr_boolconst() {
    assert_eq!(primary_expr(&b"false "[..]), Ok((&b" "[..], syntax::Expr::BoolConst(false.to_owned()))));
    assert_eq!(primary_expr(&b"true "[..]), Ok((&b" "[..], syntax::Expr::BoolConst(true.to_owned()))));
  }

  #[test]
  fn parse_primary_expr_parens() {
    assert_eq!(primary_expr(&b"(0)"[..]), Ok((&b""[..], syntax::Expr::IntConst(0))));
    assert_eq!(primary_expr(&b"  (  0 ) "[..]), Ok((&b""[..], syntax::Expr::IntConst(0))));
    assert_eq!(primary_expr(&b"  (  .0 ) "[..]), Ok((&b""[..], syntax::Expr::DoubleConst(0.))));
    assert_eq!(primary_expr(&b"  (  (.0) ) "[..]), Ok((&b""[..], syntax::Expr::DoubleConst(0.))));
    assert_eq!(primary_expr(&b"(true) "[..]), Ok((&b""[..], syntax::Expr::BoolConst(true))));
  }

  #[test]
  fn parse_postfix_function_call_no_args() {
    let fun = syntax::FunIdentifier::Identifier("vec3".into());
    let args = Vec::new();
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"vec3();"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b" vec3   (  ) ;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b" vec3   (\nvoid\n) ;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_postfix_function_call_one_arg() {
    let fun = syntax::FunIdentifier::Identifier("foo".into());
    let args = vec![syntax::Expr::IntConst(0)];
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"foo(0);"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b" foo   ( 0 ) ;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b" foo   (\n0\t\n) ;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_postfix_function_call_multi_arg() {
    let fun = syntax::FunIdentifier::Identifier("foo".into());
    let args = vec![
      syntax::Expr::IntConst(0),
      syntax::Expr::BoolConst(false),
      syntax::Expr::Variable("bar".into()),
   ];
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"foo(0, false, bar);"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b" foo   ( 0\t, false    ,\t\tbar) ;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_postfix_expr_bracket() {
    let id = syntax::Expr::Variable("foo".into());
    let array_spec = syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(7354)));
    let expected = syntax::Expr::Bracket(Box::new(id), array_spec);

    assert_eq!(postfix_expr(&b"foo[7354];"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b" foo[7354];"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_postfix_expr_dot() {
    let foo = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::Expr::Dot(foo, "bar".into());

    assert_eq!(postfix_expr(&b"foo.bar;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b"(foo).bar;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_postfix_expr_dot_several() {
    let foo = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::Expr::Dot(Box::new(syntax::Expr::Dot(foo, "bar".into())), "zoo".into());

    assert_eq!(postfix_expr(&b"foo.bar.zoo;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b"(foo).bar.zoo;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(postfix_expr(&b"(foo.bar).zoo;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_postfix_postinc() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::PostInc(Box::new(foo));

    assert_eq!(postfix_expr(&b"foo++;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_postfix_postdec() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::PostDec(Box::new(foo));

    assert_eq!(postfix_expr(&b"foo--;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_unary_add() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Add, Box::new(foo));

    assert_eq!(unary_expr(&b"+foo;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_unary_minus() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Minus, Box::new(foo));

    assert_eq!(unary_expr(&b"-foo;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_unary_not() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Not, Box::new(foo));

    assert_eq!(run_parser(&b"!foo;"[..], unary_expr), Ok(expected));
  }

  #[test]
  fn parse_unary_complement() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Complement, Box::new(foo));

    assert_eq!(unary_expr(&b"~foo;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_unary_inc() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Inc, Box::new(foo));

    assert_eq!(unary_expr(&b"++foo;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_unary_dec() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Dec, Box::new(foo));

    assert_eq!(unary_expr(&b"--foo;"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_expr_float() {
    assert_eq!(expr(&b"314.;"[..]), Ok((&b";"[..], syntax::Expr::DoubleConst(314.))));
    assert_eq!(expr(&b"314.f;"[..]), Ok((&b";"[..], syntax::Expr::FloatConst(314.))));
  }

  #[test]
  fn parse_expr_add_2() {
    let one = Box::new(syntax::Expr::IntConst(1));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, one.clone(), one);

    assert_eq!(expr(&b" 1 + 1 ;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(expr(&b"1+1;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(expr(&b"(1 + 1);"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_expr_add_3() {
    let one = Box::new(syntax::Expr::UIntConst(1));
    let two = Box::new(syntax::Expr::UIntConst(2));
    let three = Box::new(syntax::Expr::UIntConst(3));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, one, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)));

    assert_eq!(expr(&b" 1u + 2u + 3u ;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(expr(&b"1u+2u+3u;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(expr(&b"(1u + (2u + 3u));"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_expr_add_mult_3() {
    let one = Box::new(syntax::Expr::UIntConst(1));
    let two = Box::new(syntax::Expr::UIntConst(2));
    let three = Box::new(syntax::Expr::UIntConst(3));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, two)), three);

    assert_eq!(expr(&b" 1u * 2u + 3u ;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(expr(&b"1u*2u+3u;"[..]), Ok((&b";"[..], expected.clone())));
    assert_eq!(expr(&b"(1u * 2u) + 3u;"[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_expr_add_sub_mult_div() {
    let one = Box::new(syntax::Expr::IntConst(1));
    let two = Box::new(syntax::Expr::IntConst(2));
    let three = Box::new(syntax::Expr::IntConst(3));
    let four = Box::new(syntax::Expr::IntConst(4));
    let five = Box::new(syntax::Expr::IntConst(5));
    let six = Box::new(syntax::Expr::IntConst(6));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)))), Box::new(syntax::Expr::Binary(syntax::BinaryOp::Div, four, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, five, six)))));

    assert_eq!(expr(&b"1 * (2 + 3) + 4 / (5 + 6);"[..]), Ok((&b";"[..], expected.clone())));
  }

  #[test]
  fn parse_complex_expr() {
    let input = b"normalize((inverse(view) * vec4(ray.dir, 0.)).xyz);";
    let zero = syntax::Expr::DoubleConst(0.);
    let ray = syntax::Expr::Variable("ray".into());
    let raydir = syntax::Expr::Dot(Box::new(ray), "dir".into());
    let vec4 = syntax::Expr::FunCall(syntax::FunIdentifier::Identifier("vec4".into()), vec![raydir, zero]);
    let view = syntax::Expr::Variable("view".into());
    let iview = syntax::Expr::FunCall(syntax::FunIdentifier::Identifier("inverse".into()), vec![view]);
    let mul = syntax::Expr::Binary(syntax::BinaryOp::Mult, Box::new(iview), Box::new(vec4));
    let xyz = syntax::Expr::Dot(Box::new(mul), "xyz".into());
    let normalize = syntax::Expr::FunCall(syntax::FunIdentifier::Identifier("normalize".into()), vec![xyz]);
    let expected = normalize;

    assert_eq!(expr(&input[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_function_identifier_typename() {
    let expected = syntax::FunIdentifier::Identifier("foo".into());
    assert_eq!(function_identifier(&b"foo("[..]), Ok((&b"("[..], expected.clone())));
    assert_eq!(function_identifier(&b"  foo\n\t("[..]), Ok((&b"("[..], expected.clone())));
    assert_eq!(function_identifier(&b" \tfoo\n ("[..]), Ok((&b"("[..], expected)));
  }

  #[test]
  fn parse_function_identifier_cast() {
    let expected = syntax::FunIdentifier::Identifier("vec3".into());
    assert_eq!(function_identifier(&b"vec3("[..]), Ok((&b"("[..], expected.clone())));
    assert_eq!(function_identifier(&b"  vec3 ("[..]), Ok((&b"("[..], expected.clone())));
    assert_eq!(function_identifier(&b" \t\n vec3\t\n\n \t ("[..]), Ok((&b"("[..], expected)));
  }

  #[test]
  fn parse_function_identifier_cast_array_unsized() {
    let expected =
      syntax::FunIdentifier::Expr(
        Box::new(
          syntax::Expr::Bracket(Box::new(syntax::Expr::Variable("vec3".into())),
                                syntax::ArraySpecifier::Unsized
          )
        )
      );

    assert_eq!(function_identifier(&b"vec3[]("[..]), Ok((&b"("[..], expected.clone())));
    assert_eq!(function_identifier(&b"\n\tvec3  [\t\n]("[..]), Ok((&b"("[..], expected)));
  }

  #[test]
  fn parse_function_identifier_cast_array_sized() {
    let expected =
      syntax::FunIdentifier::Expr(
        Box::new(
          syntax::Expr::Bracket(
            Box::new(syntax::Expr::Variable("vec3".into())),
            syntax::ArraySpecifier::ExplicitlySized(
              Box::new(
                syntax::Expr::IntConst(12)
              )
            )
          )
        )
      );

    assert_eq!(function_identifier(&b"vec3[12]("[..]), Ok((&b"("[..], expected.clone())));
    assert_eq!(function_identifier(&b"\n\tvec3  [\t 12\n]("[..]), Ok((&b"("[..], expected)));
  }

  #[test]
  fn parse_void() {
    assert_eq!(void(&b"void "[..]), Ok((&b" "[..], ())));
  }

  #[test]
  fn parse_assignment_op_equal() {
    assert_eq!(assignment_op(&b"= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Equal)));
  }

  #[test]
  fn parse_assignment_op_mult() {
    assert_eq!(assignment_op(&b"*= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Mult)));
  }

  #[test]
  fn parse_assignment_op_div() {
    assert_eq!(assignment_op(&b"/= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Div)));
  }

  #[test]
  fn parse_assignment_op_mod() {
    assert_eq!(assignment_op(&b"%= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Mod)));
  }

  #[test]
  fn parse_assignment_op_add() {
    assert_eq!(assignment_op(&b"+= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Add)));
  }

  #[test]
  fn parse_assignment_op_sub() {
    assert_eq!(assignment_op(&b"-= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Sub)));
  }

  #[test]
  fn parse_assignment_op_lshift() {
    assert_eq!(assignment_op(&b"<<= "[..]), Ok((&b" "[..], syntax::AssignmentOp::LShift)));
  }

  #[test]
  fn parse_assignment_op_rshift() {
    assert_eq!(assignment_op(&b">>= "[..]), Ok((&b" "[..], syntax::AssignmentOp::RShift)));
  }

  #[test]
  fn parse_assignment_op_and() {
    assert_eq!(assignment_op(&b"&= "[..]), Ok((&b" "[..], syntax::AssignmentOp::And)));
  }

  #[test]
  fn parse_assignment_op_xor() {
    assert_eq!(assignment_op(&b"^= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Xor)));
  }

  #[test]
  fn parse_assignment_op_or() {
    assert_eq!(assignment_op(&b"|= "[..]), Ok((&b" "[..], syntax::AssignmentOp::Or)));
  }

  #[test]
  fn parse_expr_statement() {
    let expected = Some(syntax::Expr::Assignment(Box::new(syntax::Expr::Variable("foo".into())),
                                                 syntax::AssignmentOp::Equal,
                                                 Box::new(syntax::Expr::FloatConst(314.))));

    assert_eq!(expr_statement(&b"foo = 314.f;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(expr_statement(&b"foo=314.f;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(expr_statement(&b"\n\t foo\n\t=  \n314.f\n   ;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_function_prototype() {
    let rt = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None
      }
    };
    let arg0_ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec2,
      array_specifier: None
    };
    let arg0 = syntax::FunctionParameterDeclaration::Unnamed(None, arg0_ty);
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Out);
    let qual = syntax::TypeQualifier { qualifiers: syntax::NonEmpty(vec![qual_spec]) };
    let arg1 = syntax::FunctionParameterDeclaration::Named(Some(qual), syntax::FunctionParameterDeclarator {
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None
      },
      ident: "the_arg".into(),
    });
    let fp = syntax::FunctionPrototype {
      ty: rt,
      name: "foo".into(),
      parameters: vec![arg0, arg1]
    };
    let expected = syntax::Declaration::FunctionPrototype(fp);

    assert_eq!(declaration(&b"vec3 foo(vec2, out float the_arg);"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"  vec3 \nfoo ( vec2\n, out float \n\tthe_arg )\n;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"vec3 foo(vec2,out float the_arg);"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_init_declarator_list_single() {
    let ty = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Int,
        array_specifier: None
      }
    };
    let sd = syntax::SingleDeclaration {
      ty,
      name: Some("foo".into()),
      array_specifier: None,
      initializer: Some(syntax::Initializer::Simple(Box::new(syntax::Expr::IntConst(34))))
    };
    let idl = syntax::InitDeclaratorList { head: sd, tail: Vec::new() };
    let expected = syntax::Declaration::InitDeclaratorList(idl);

    assert_eq!(declaration(&b"int foo = 34;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"int foo=34;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"\n\t int    \t  \nfoo =\t34  ;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_init_declarator_list_complex() {
    let ty = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Int,
        array_specifier: None
      }
    };
    let sd = syntax::SingleDeclaration {
      ty,
      name: Some("foo".into()),
      array_specifier: None,
      initializer: Some(syntax::Initializer::Simple(Box::new(syntax::Expr::IntConst(34))))
    };
    let sdnt = syntax::SingleDeclarationNoType {
      ident: "bar".into(),
      initializer: Some(syntax::Initializer::Simple(Box::new(syntax::Expr::IntConst(12))))
    };
    let expected = syntax::Declaration::InitDeclaratorList(syntax::InitDeclaratorList {
      head: sd,
      tail: vec![sdnt]
    });

    assert_eq!(declaration(&b"int foo = 34, bar = 12;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"int foo=34,bar=12;"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"\n\t int    \t  \nfoo =\t34 \n,\tbar=      12\n ;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_precision_low() {
    let qual = syntax::PrecisionQualifier::Low;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration(&b"precision lowp float;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_precision_medium() {
    let qual = syntax::PrecisionQualifier::Medium;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration(&b"precision mediump float;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_precision_high() {
    let qual = syntax::PrecisionQualifier::High;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration(&b"precision highp float;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_uniform_block() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Uniform);
    let qual = syntax::TypeQualifier { qualifiers: syntax::NonEmpty(vec![qual_spec]) };
    let f0 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["a".into()])
    };
    let f1 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["b".into()])
    };
    let f2 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("foo".into()),
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["c".into(), "d".into()])
    };
    let expected =
      syntax::Declaration::Block(
        syntax::Block {
          qualifier: qual,
          name: "UniformBlockTest".into(),
          fields: vec![f0, f1, f2],
          identifier: None
        }
      );

    assert_eq!(declaration(&b"uniform UniformBlockTest { float a; vec3 b; foo c, d; };"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"\n\tuniform   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_declaration_buffer_block() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer);
    let qual = syntax::TypeQualifier { qualifiers: syntax::NonEmpty(vec![qual_spec]) };
    let f0 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["a".into()])
    };
    let f1 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec![syntax::ArrayedIdentifier::new("b", Some(syntax::ArraySpecifier::Unsized))])
    };
    let f2 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("foo".into()),
        array_specifier: None
      },
      identifiers: syntax::NonEmpty(vec!["c".into(), "d".into()])
    };
    let expected =
      syntax::Declaration::Block(
        syntax::Block {
          qualifier: qual,
          name: "UniformBlockTest".into(),
          fields: vec![f0, f1, f2],
          identifier: None
        }
      );

    assert_eq!(declaration(&b"buffer UniformBlockTest { float a; vec3 b[]; foo c, d; };"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(declaration(&b"\n\tbuffer   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b   [   ]\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_selection_statement_if() {
    let cond = syntax::Expr::Binary(syntax::BinaryOp::LT,
                                    Box::new(syntax::Expr::Variable("foo".into())),
                                    Box::new(syntax::Expr::IntConst(10)));
    let ret = Box::new(syntax::Expr::BoolConst(false));
    let st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(ret))));
    let body = syntax::Statement::Compound(Box::new(syntax::CompoundStatement { statement_list: vec![st] }));
    let rest = syntax::SelectionRestStatement::Statement(Box::new(body));
    let expected = syntax::SelectionStatement {
      cond: Box::new(cond),
      rest: rest
    };

    assert_eq!(selection_statement(&b"if (foo < 10) { return false; }K"[..]), Ok((&b"K"[..], expected.clone())));
    assert_eq!(selection_statement(&b" if \n(foo<10\n) \t{return false;}K"[..]), Ok((&b"K"[..], expected)));
  }

  #[test]
  fn parse_selection_statement_if_else() {
    let cond = syntax::Expr::Binary(syntax::BinaryOp::LT,
                                    Box::new(syntax::Expr::Variable("foo".into())),
                                    Box::new(syntax::Expr::IntConst(10)));
    let if_ret = Box::new(syntax::Expr::FloatConst(0.));
    let if_st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(if_ret))));
    let if_body = syntax::Statement::Compound(Box::new(syntax::CompoundStatement { statement_list: vec![if_st] }));
    let else_ret = Box::new(syntax::Expr::Variable("foo".into()));
    let else_st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(else_ret))));
    let else_body = syntax::Statement::Compound(Box::new(syntax::CompoundStatement { statement_list: vec![else_st] }));
    let rest = syntax::SelectionRestStatement::Else(Box::new(if_body), Box::new(else_body));
    let expected = syntax::SelectionStatement {
      cond: Box::new(cond),
      rest: rest
    };

    assert_eq!(selection_statement(&b"if (foo < 10) { return 0.f; } else { return foo; }"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(selection_statement(&b" if \n(foo<10\n) \t{return 0.f\t;\n\n}\n else{\n\t return foo   ;}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_switch_statement_empty() {
    let head = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::SwitchStatement {
      head: head,
      body: Vec::new()
    };

    assert_eq!(switch_statement(&b"switch (foo) {}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(switch_statement(&b"switch(foo){}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(switch_statement(&b"  \tswitch\n\n (  foo  \t   \n) { \n\n   }"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_switch_statement_cases() {
    let head = Box::new(syntax::Expr::Variable("foo".into()));
    let case0 = syntax::Statement::Simple(
                  Box::new(
                    syntax::SimpleStatement::CaseLabel(
                      syntax::CaseLabel::Case(
                        Box::new(
                          syntax::Expr::IntConst(0)
                        )
                      )
                    )
                  ));
    let case1 = syntax::Statement::Simple(
                  Box::new(
                    syntax::SimpleStatement::CaseLabel(
                      syntax::CaseLabel::Case(
                        Box::new(
                          syntax::Expr::IntConst(1)
                        )
                      )
                    )
                  ));
    let ret = syntax::Statement::Simple(
                  Box::new(
                    syntax::SimpleStatement::Jump(
                      syntax::JumpStatement::Return(
                        Box::new(
                          syntax::Expr::UIntConst(12)
                        )
                      )
                    )
                  ));
    let expected = syntax::SwitchStatement {
      head: head,
      body: vec![case0, case1, ret]
    };

    assert_eq!(switch_statement(&b"switch (foo) { case 0: case 1: return 12u; }"[..]), Ok((&b""[..], expected.clone())));
  }

  #[test]
  fn parse_case_label_def() {
    assert_eq!(case_label(&b"default:"[..]), Ok((&b""[..], syntax::CaseLabel::Def)));
    assert_eq!(case_label(&b"  default   : "[..]), Ok((&b""[..], syntax::CaseLabel::Def)));
  }

  #[test]
  fn parse_case_label() {
    let expected = syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(3)));

    assert_eq!(case_label(&b"case 3:"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(case_label(&b"  case\n\t 3   : "[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_iteration_statement_while_empty() {
    let cond = syntax::Condition::Expr(
                 Box::new(
                   syntax::Expr::Binary(syntax::BinaryOp::GTE,
                                        Box::new(syntax::Expr::Variable("a".into())),
                                        Box::new(syntax::Expr::Variable("b".into())))
                 )
               );
    let st = syntax::Statement::Compound(Box::new(syntax::CompoundStatement { statement_list: Vec::new() }));
    let expected = syntax::IterationStatement::While(cond, Box::new(st));

    assert_eq!(iteration_statement(&b"while (a >= b) {}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(iteration_statement(&b"while(a>=b){}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(iteration_statement(&b"\t\n  while (  a >=\n\tb  )\t  {   \n}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_iteration_statement_do_while_empty() {
    let st = syntax::Statement::Compound(Box::new(syntax::CompoundStatement { statement_list: Vec::new() }));
    let cond = Box::new(
                 syntax::Expr::Binary(syntax::BinaryOp::GTE,
                                      Box::new(syntax::Expr::Variable("a".into())),
                                      Box::new(syntax::Expr::Variable("b".into())))
               );
    let expected = syntax::IterationStatement::DoWhile(Box::new(st), cond);

    assert_eq!(iteration_statement(&b"do {} while (a >= b);"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(iteration_statement(&b"do{}while(a>=b);"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(iteration_statement(&b"\tdo \n {\n} while (  a >=\n\tb  )\t  \n;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_iteration_statement_for_empty() {
    let init = syntax::ForInitStatement::Declaration(
                 Box::new(
                   syntax::Declaration::InitDeclaratorList(
                     syntax::InitDeclaratorList {
                       head: syntax::SingleDeclaration {
                               ty: syntax::FullySpecifiedType {
                                 qualifier: None,
                                 ty: syntax::TypeSpecifier {
                                   ty: syntax::TypeSpecifierNonArray::Float,
                                   array_specifier: None
                                 }
                               },
                               name: Some("i".into()),
                               array_specifier: None,
                               initializer: Some(syntax::Initializer::Simple(Box::new(syntax::Expr::FloatConst(0.))))
                             },
                       tail: Vec::new()
                     }
                   )
                 )
               );
    let rest = syntax::ForRestStatement {
      condition: Some(syntax::Condition::Expr(Box::new(syntax::Expr::Binary(syntax::BinaryOp::LTE,
                                                                            Box::new(syntax::Expr::Variable("i".into())),
                                                                            Box::new(syntax::Expr::FloatConst(10.)))))),
      post_expr: Some(Box::new(syntax::Expr::Unary(syntax::UnaryOp::Inc, Box::new(syntax::Expr::Variable("i".into())))))
    };
    let st = syntax::Statement::Compound(Box::new(syntax::CompoundStatement { statement_list: Vec::new() }));
    let expected = syntax::IterationStatement::For(init, rest, Box::new(st));

    assert_eq!(iteration_statement(&b"for (float i = 0.f; i <= 10.f; ++i) {}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(iteration_statement(&b"for(float i=0.f;i<=10.f;++i){}"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(iteration_statement(&b"   for\n\t (  \t\n\nfloat \ni \t=\n0.f\n;\ni\t<=  10.f; \n++i\n)\n{\n}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_jump_continue() {
    assert_eq!(jump_statement(&b"continue;"[..]), Ok((&b""[..], syntax::JumpStatement::Continue)));
  }

  #[test]
  fn parse_jump_break() {
    assert_eq!(jump_statement(&b"break;"[..]), Ok((&b""[..], syntax::JumpStatement::Break)));
  }

  #[test]
  fn parse_jump_return() {
    let expected = syntax::JumpStatement::Return(Box::new(syntax::Expr::IntConst(3)));
    assert_eq!(jump_statement(&b"return 3;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_jump_discard() {
    assert_eq!(jump_statement(&b"discard;"[..]), Ok((&b""[..], syntax::JumpStatement::Discard)));
  }

  #[test]
  fn parse_simple_statement_return() {
    let e = syntax::Expr::BoolConst(false);
    let expected = syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(Box::new(e)));

    assert_eq!(simple_statement(&b"return false;"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_compound_statement_empty() {
    let expected = syntax::CompoundStatement { statement_list: Vec::new() };

    assert_eq!(compound_statement(&b"{}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_compound_statement() {
    let st0 = syntax::Statement::Simple(
                Box::new(
                  syntax::SimpleStatement::Selection(
                    syntax::SelectionStatement {
                      cond: Box::new(syntax::Expr::BoolConst(true)),
                      rest: syntax::SelectionRestStatement::Statement(
                              Box::new(
                                syntax::Statement::Compound(
                                  Box::new(syntax::CompoundStatement { statement_list: Vec::new() })
                                )
                              )
                            )
                    }
                  )
                )
              );
    let st1 = syntax::Statement::Simple(
                Box::new(
                  syntax::SimpleStatement::Declaration(
                    syntax::Declaration::InitDeclaratorList(
                      syntax::InitDeclaratorList {
                        head: syntax::SingleDeclaration {
                          ty: syntax::FullySpecifiedType {
                            qualifier: None,
                            ty: syntax::TypeSpecifier {
                              ty: syntax::TypeSpecifierNonArray::ISampler3D,
                              array_specifier: None
                            }
                          },
                          name: Some("x".into()),
                          array_specifier: None,
                          initializer: None
                        },
                        tail: Vec::new()
                      }
                    )
                  )
                )
              );
    let st2 = syntax::Statement::Simple(
                Box::new(
                  syntax::SimpleStatement::Jump(
                    syntax::JumpStatement::Return(
                      Box::new(
                        syntax::Expr::IntConst(42)
                      )
                    )
                  )
                )
              );
    let expected = syntax::CompoundStatement {
      statement_list: vec![st0, st1, st2]
    };

    assert_eq!(compound_statement(&b"{ if (true) {} isampler3D x; return 42 ; }"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(compound_statement(&b"{if(true){}isampler3D x;return 42;}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_function_definition() {
    let rt = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::IImage2DArray,
        array_specifier: None
      }
    };
    let fp = syntax::FunctionPrototype {
      ty: rt,
      name: "foo".into(),
      parameters: Vec::new()
    };
    let st0 = syntax::Statement::Simple(
                Box::new(
                  syntax::SimpleStatement::Jump(
                    syntax::JumpStatement::Return(
                      Box::new(
                        syntax::Expr::Variable("bar".into())
                      )
                    )
                  )
                )
              );
    let expected = syntax::FunctionDefinition {
      prototype: fp,
      statement: syntax::CompoundStatement {
        statement_list: vec![st0]
      }
    };

    assert_eq!(function_definition(&b"iimage2DArray foo() { return bar; }"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(function_definition(&b"  \niimage2DArray \tfoo\n()\n \n{\n return \nbar\n;\n }"[..]), Ok((&b""[..], expected.clone())));
    assert_eq!(function_definition(&b"iimage2DArray foo(){return bar;}"[..]), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_buffer_block_0() {
    let src = include_bytes!("../data/tests/buffer_block_0.glsl");
    let main_fn = syntax::ExternalDeclaration::FunctionDefinition(syntax::FunctionDefinition {
      prototype: syntax::FunctionPrototype {
        ty: syntax::FullySpecifiedType {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::Void,
            array_specifier: None
          }
        },
        name: "main".into(),
        parameters: Vec::new(),
      },
      statement: syntax::CompoundStatement {
        statement_list: Vec::new()
      }
    });
    let buffer_block =
      syntax::ExternalDeclaration::Declaration(
        syntax::Declaration::Block(
          syntax::Block {
            qualifier: syntax::TypeQualifier {
              qualifiers: syntax::NonEmpty(vec![syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer)])
            },
            name: "Foo".into(),
            fields: vec![
              syntax::StructFieldSpecifier {
                qualifier: None,
                ty: syntax::TypeSpecifier {
                  ty: syntax::TypeSpecifierNonArray::TypeName("char".into()),
                  array_specifier: None
                },
                identifiers: syntax::NonEmpty(vec![syntax::ArrayedIdentifier::new("tiles", Some(syntax::ArraySpecifier::Unsized))])
              }
            ],
            identifier: Some("main_tiles".into())
          }
        )
      );
    let expected = syntax::TranslationUnit(syntax::NonEmpty(vec![buffer_block, main_fn]));

    assert_eq!(translation_unit(src), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_layout_buffer_block_0() {
    let src = include_bytes!("../data/tests/layout_buffer_block_0.glsl");
    let layout = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![
        syntax::LayoutQualifierSpec::Identifier("set".into(), Some(Box::new(syntax::Expr::IntConst(0)))),
        syntax::LayoutQualifierSpec::Identifier("binding".into(), Some(Box::new(syntax::Expr::IntConst(0)))),
      ])
    };
    let type_qual = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![
        syntax::TypeQualifierSpec::Layout(layout),
        syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer)
      ])
    };
    let block =
      syntax::ExternalDeclaration::Declaration(
        syntax::Declaration::Block(
          syntax::Block {
            qualifier: type_qual,
            name: "Foo".into(),
            fields: vec![
              syntax::StructFieldSpecifier {
                qualifier: None,
                ty: syntax::TypeSpecifier {
                  ty: syntax::TypeSpecifierNonArray::TypeName("char".into()),
                  array_specifier: None
                },
                identifiers: syntax::NonEmpty(vec!["a".into()])
              }
            ],
            identifier: Some("foo".into())
          }
        )
      );

    let expected = syntax::TranslationUnit(syntax::NonEmpty(vec![block]));

    assert_eq!(translation_unit(src), Ok((&b""[..], expected)));
  }

  #[test]
  fn parse_pp_version_number() {
    assert_eq!(pp_version_number(&b"450 "[..]), Ok((&b" "[..], 450)));
  }

  #[test]
  fn parse_pp_version_profile() {
    assert_eq!(pp_version_profile(&b"core "[..]), Ok((&b" "[..], syntax::PreprocessorVersionProfile::Core)));
    assert_eq!(pp_version_profile(&b"compatibility "[..]), Ok((&b" "[..], syntax::PreprocessorVersionProfile::Compatibility)));
    assert_eq!(pp_version_profile(&b"es "[..]), Ok((&b" "[..], syntax::PreprocessorVersionProfile::ES)));
  }

  #[test]
  fn parse_pp_version() {
    assert_eq!(
      pp_version(&b"#version 450\n"[..]),
      Ok((&b""[..],
         syntax::PreprocessorVersion {
           version: 450,
           profile: None,
         }
      ))
    );

    assert_eq!(
      pp_version(&b"#version 450 core\n"[..]),
      Ok((&b""[..],
         syntax::PreprocessorVersion {
           version: 450,
           profile: Some(syntax::PreprocessorVersionProfile::Core)
         }
      ))
    );
  }

  #[test]
  fn parse_pp_version_newline() {
    assert_eq!(
      preprocessor(&b"\n\t \n#version 450\n"[..]),
      Ok((&b""[..],
         syntax::Preprocessor::Version(syntax::PreprocessorVersion {
           version: 450,
           profile: None,
         })
      ))
    );

    assert_eq!(
      preprocessor(&b"\n\t \n#version 450 core\n"[..]),
      Ok((&b""[..],
         syntax::Preprocessor::Version(syntax::PreprocessorVersion {
           version: 450,
           profile: Some(syntax::PreprocessorVersionProfile::Core)
         })
      ))
    );
  }

  #[test]
  fn parse_define() {
    assert_eq!(
      preprocessor(&b"#define test 1.0\n"[..]),
      Ok((&b""[..],
         syntax::Preprocessor::Define(syntax::PreprocessorDefine {
           name: "test".into(),
           value: syntax::Expr::DoubleConst(1.0)
         })
      ))
    );

    assert_eq!(
      preprocessor(&b"#define test123 .0f\n"[..]),
      Ok((&b""[..],
         syntax::Preprocessor::Define(syntax::PreprocessorDefine {
           name: "test123".into(),
           value: syntax::Expr::FloatConst(0.0)
         })
      ))
    );

    assert_eq!(
      preprocessor(&b"#define test 1\n"[..]),
      Ok((&b""[..],
         syntax::Preprocessor::Define(syntax::PreprocessorDefine {
           name: "test".into(),
           value: syntax::Expr::IntConst(1)
         })
      ))
    );
  }

  #[test]
  fn parse_pp_extension_name() {
    assert_eq!(pp_extension_name(&b"all"[..]), Ok((&b""[..], syntax::PreprocessorExtensionName::All)));
    assert_eq!(pp_extension_name(&b"GL_foobar_extension "[..]), Ok((&b""[..], syntax::PreprocessorExtensionName::Specific("GL_foobar_extension".to_owned()))));
  }

  #[test]
  fn parse_pp_extension_behavior() {
    assert_eq!(pp_extension_behavior(&b"require"[..]), Ok((&b""[..], syntax::PreprocessorExtensionBehavior::Require)));
    assert_eq!(pp_extension_behavior(&b"enable"[..]), Ok((&b""[..], syntax::PreprocessorExtensionBehavior::Enable)));
    assert_eq!(pp_extension_behavior(&b"warn"[..]), Ok((&b""[..], syntax::PreprocessorExtensionBehavior::Warn)));
    assert_eq!(pp_extension_behavior(&b"disable"[..]), Ok((&b""[..], syntax::PreprocessorExtensionBehavior::Disable)));
  }

  #[test]
  fn parse_pp_extension() {
    assert_eq!(
      pp_extension(&b"#extension all: require\n"[..]),
      Ok((&b""[..],
         syntax::PreprocessorExtension {
           name: syntax::PreprocessorExtensionName::All,
           behavior: Some(syntax::PreprocessorExtensionBehavior::Require)
         }
      ))
    );
  }

  #[test]
  fn parse_dot_field_expr_array() {
    let src = b"a[0].xyz;";
    let expected =
      syntax::Expr::Dot(
        Box::new(syntax::Expr::Bracket(
          Box::new(syntax::Expr::Variable("a".into())),
          syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(0))))),
        "xyz".into()
      );

    assert_eq!(expr(&src[..]), Ok((&b";"[..], expected)));
  }

  #[test]
  fn parse_dot_field_expr_statement() {
    let src = b"vec3 v = smoothstep(vec3(border_width), vec3(0.0), v_barycenter).zyx;";
    let fun = syntax::FunIdentifier::Identifier("smoothstep".into());
    let args =
      vec![
        syntax::Expr::FunCall(syntax::FunIdentifier::Identifier("vec3".into()), vec![syntax::Expr::Variable("border_width".into())]),
        syntax::Expr::FunCall(syntax::FunIdentifier::Identifier("vec3".into()), vec![syntax::Expr::DoubleConst(0.)]),
        syntax::Expr::Variable("v_barycenter".into())
      ];
    let ini =
      syntax::Initializer::Simple(
        Box::new(
          syntax::Expr::Dot(Box::new(syntax::Expr::FunCall(fun, args)), "zyx".into())
        )
      );
    let sd = syntax::SingleDeclaration {
      ty: syntax::FullySpecifiedType {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::Vec3,
            array_specifier: None
          }
      },
      name: Some("v".into()),
      array_specifier: None,
      initializer: Some(ini)
    };
    let expected =
      syntax::Statement::Simple(
        Box::new(
          syntax::SimpleStatement::Declaration(
            syntax::Declaration::InitDeclaratorList(
              syntax::InitDeclaratorList {
                head: sd,
                tail: Vec::new()
              }
            )
          )
        )
      );

    assert_eq!(statement(&src[..]), Ok((&b""[..], expected)));
  }
}
