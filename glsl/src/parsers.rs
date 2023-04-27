//! GLSL parsers.
//!
//! The more general parser is `translation_unit`, that recognizes the most external form of a GLSL
//! source (a shader, basically).
//!
//! Other parsers are exported if you want more control on how you want to parse your source.

mod nom_helpers;

mod parse_input;
pub use parse_input::ParseContextData;
pub use parse_input::*;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while1};
use nom::character::complete::{anychar, char, digit1, space0, space1};
use nom::character::{is_hex_digit, is_oct_digit};
use nom::combinator::{cut, map, not, opt, peek, recognize, value, verify};
use nom::error::{ErrorKind, ParseError as _, VerboseError, VerboseErrorKind};
use nom::multi::{fold_many0, many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{Err as NomErr, ParseTo};
use std::num::ParseIntError;

use nom::{AsBytes, Slice};
use nom_locate::position;
use std::borrow::Cow;

pub use self::nom_helpers::ParserResult;
use self::nom_helpers::{blank_space, cnst, eol, many0_, str_till_eol};
use crate::syntax;

macro_rules! parse_located {
  ($i:ident, $b:block) => {{
    let s = $i;

    // Record start position of declaration
    let ($i, start) = position($i)?;

    let ($i, res) = $b?;

    let ($i, end) = position($i)?;

    Ok((
      $i,
      syntax::Node::new(
        res,
        Some(
          (
            s.extra.map(ParseContext::current_source).unwrap_or(0),
            s.slice(0..(end.location_offset() - start.location_offset())),
          )
            .into(),
        ),
      ),
    ))
  }};
}

// Parse a keyword. A keyword is just a regular string that must be followed by punctuation.
fn keyword<'e, 'd: 'e, 'c: 'd + 'e>(
  kwd: &'c str,
) -> impl FnMut(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  terminated(
    tag(kwd),
    not(verify(peek(anychar), |&c| identifier_pred(c))),
  )
}

/// Parse a single comment.
pub fn comment<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Comment<'c>> {
  let (i, res) = parse_located!(i, {
    preceded(
      char('/'),
      alt((
        preceded(
          char('/'),
          cut(map(str_till_eol, |i| {
            syntax::Comment::Single(Cow::Borrowed(i.fragment()))
          })),
        ),
        preceded(
          char('*'),
          map(
            terminated(take_until("*/"), tag("*/")),
            |i: ParseInput<'c, 'd, 'e>| syntax::Comment::Multi(Cow::Borrowed(i.fragment())),
          ),
        ),
      )),
    )(i)
  })?;

  i.extra.map(|c| c.add_comment(res.clone()));
  Ok((i, res.into_inner()))
}

/// Parse several comments.
pub fn comments<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  recognize(many0_(terminated(comment, blank_space)))(i)
}

/// In-between token parser (spaces and comments).
///
/// This parser also allows to break a line into two by finishing the line with a backslack ('\').
fn blank<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  value((), preceded(blank_space, comments))(i)
}

#[inline]
fn identifier_pred<'c, 'd, 'e>(ch: char) -> bool {
  ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn verify_identifier<'c, 'd, 'e>(s: &ParseInput<'c, 'd, 'e>) -> bool {
  !char::from(s.as_bytes()[0]).is_digit(10)
}

/// Parse an identifier (raw version).
fn identifier_str<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  verify(take_while1(identifier_pred), verify_identifier)(i)
}

/// Parse a string that could be used as an identifier.
pub fn string<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, String> {
  map(identifier_str, |s| s.to_string())(i)
}

/// Parse an identifier.
pub fn identifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Identifier> {
  parse_located!(i, { map(string, syntax::IdentifierData)(i) })
}

/// Parse a type name.
pub fn type_name<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TypeName> {
  map(string, syntax::TypeName)(i)
}

/// Parse a non-empty list of type names, delimited by comma (,).
fn nonempty_type_names<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Vec<syntax::TypeName>> {
  separated_list0(terminated(char(','), blank), terminated(type_name, blank))(i)
}

/// Parse a type specifier non struct.
pub fn type_specifier_non_struct<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TypeSpecifierNonArray> {
  let (i1, t) = identifier_str(i)?;

  match *t.fragment() {
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
      let ve = VerboseError {
        errors: vec![(i1, vek)],
      };
      Err(NomErr::Error(ve))
    }
  }
}

/// Parse a type specifier (non-array version).
pub fn type_specifier_non_array<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TypeSpecifierNonArray> {
  alt((
    type_specifier_non_struct,
    map(struct_specifier, syntax::TypeSpecifierNonArray::Struct),
    map(type_name, syntax::TypeSpecifierNonArray::TypeName),
  ))(i)
}

/// Parse a type specifier.
pub fn type_specifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TypeSpecifier> {
  map(
    pair(
      type_specifier_non_array,
      opt(preceded(blank, array_specifier)),
    ),
    |(ty, array_specifier)| syntax::TypeSpecifier {
      ty,
      array_specifier,
    },
  )(i)
}

/// Parse the void type.
pub fn void<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  value((), keyword("void"))(i)
}

/// Parse a digit that precludes a leading 0.
pub fn nonzero_digits<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  verify(digit1, |s: &ParseInput<'c, 'd, 'e>| s.as_bytes()[0] != b'0')(i)
}

#[inline]
fn is_octal<'c, 'd, 'e>(s: &ParseInput<'c, 'd, 'e>) -> bool {
  s.as_bytes()[0] == b'0' && s.as_bytes().iter().cloned().all(is_oct_digit)
}

#[inline]
fn all_hexa<'c, 'd, 'e>(s: &ParseInput<'c, 'd, 'e>) -> bool {
  s.as_bytes().iter().cloned().all(is_hex_digit)
}

#[inline]
fn alphanumeric_no_u<'c, 'd, 'e>(c: char) -> bool {
  c.is_alphanumeric() && c != 'u' && c != 'U'
}

/// Parse an hexadecimal literal.
pub fn hexadecimal_lit<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Result<u32, ParseIntError>> {
  preceded(
    preceded(char('0'), cut(alt((char('x'), char('X'))))), // 0x | 0X
    cut(map(
      verify(take_while1(alphanumeric_no_u), all_hexa),
      |i: ParseInput<'c, 'd, 'e>| u32::from_str_radix(i.fragment(), 16),
    )),
  )(i)
}

/// Parse an octal literal.
pub fn octal_lit<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Result<u32, ParseIntError>> {
  map(
    verify(take_while1(alphanumeric_no_u), is_octal),
    |i: ParseInput<'c, 'd, 'e>| u32::from_str_radix(i.fragment(), 8),
  )(i)
}

/// Parse a decimal literal.
pub fn decimal_lit<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Result<u32, ParseIntError>> {
  map(nonzero_digits, |i| i.fragment().parse())(i)
}

/// Parse a literal integral string.
///
/// From the GLSL 4.30 spec:
///
/// > No white space is allowed between the digits of an integer
/// > constant, including after the leading 0 or after the leading
/// > 0x or 0X of a constant, or before the suffix u or U. When
/// > tokenizing, the maximal token matching the above will be
/// > recognized before a new token is started. When the suffix u or
/// > U is present, the literal has type uint, otherwise the type is
/// > int. A leading unary minus sign (-) is interpreted as an
/// > arithmetic unary negation, not as part of the constant. Hence,
/// > literals themselves are always expressed with non-negative
/// > syntax, though they could result in a negative value.
///
/// > It is a compile-time error to provide a literal integer whose
/// > bit pattern cannot fit in 32 bits. The bit pattern of the
/// > literal is always used unmodified. So a signed literal whose
/// > bit pattern includes a set sign bit creates a negative value.
pub fn integral_lit_try<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Result<i32, ParseIntError>> {
  let (i, sign) = opt(char('-'))(i)?;

  map(alt((octal_lit, hexadecimal_lit, decimal_lit)), move |lit| {
    lit.map(|v| {
      let v = v as i32;

      if sign.is_some() {
        -v
      } else {
        v
      }
    })
  })(i)
}

pub fn integral_lit<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, i32> {
  match integral_lit_try(i) {
    Ok((i, v)) => match v {
      Ok(v) => Ok((i, v)),
      _ => Err(NomErr::Failure(VerboseError::from_error_kind(
        i,
        ErrorKind::AlphaNumeric,
      ))),
    },

    Err(NomErr::Failure(x)) | Err(NomErr::Error(x)) => Err(NomErr::Error(x)),

    Err(NomErr::Incomplete(n)) => Err(NomErr::Incomplete(n)),
  }
}

/// Parse the unsigned suffix.
pub fn unsigned_suffix<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, char> {
  alt((char('u'), char('U')))(i)
}

/// Parse a literal unsigned string.
pub fn unsigned_lit<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, u32> {
  map(terminated(integral_lit, unsigned_suffix), |lit| lit as u32)(i)
}

/// Parse a floating point suffix.
fn float_suffix<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  alt((keyword("f"), keyword("F")))(i)
}

/// Parse a double point suffix.
fn double_suffix<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  alt((keyword("lf"), keyword("LF")))(i)
}

/// Parse the exponent part of a floating point literal.
fn floating_exponent<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  value(
    (),
    preceded(
      alt((char('e'), char('E'))),
      preceded(opt(alt((char('+'), char('-')))), digit1),
    ),
  )(i)
}

/// Parse the fractional constant part of a floating point literal.
fn floating_frac<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  alt((
    value((), preceded(char('.'), digit1)),
    value((), delimited(digit1, char('.'), opt(digit1))),
  ))(i)
}

/// Parse the « middle » part of a floating value – i.e. fractional and exponential parts.
fn floating_middle<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  recognize(alt((
    value((), preceded(floating_frac, opt(floating_exponent))),
    value((), preceded(nonzero_digits, floating_exponent)),
  )))(i)
}

/// Parse a float literal string.
pub fn float_lit<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, f32> {
  let (i, (sign, f)) = tuple((
    opt(char('-')),
    terminated(floating_middle, pair(opt(float_suffix), not(double_suffix))),
  ))(i)?;

  // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
  let n: f32 = if f.as_bytes()[0] == b'.' {
    let mut f_ = f.to_string();
    f_.insert(0, '0');

    f_.parse().unwrap()
  } else {
    f.fragment().parse().unwrap()
  };

  // handle the sign and return
  let r = if sign.is_some() { -n } else { n };
  Ok((i, r))
}

/// Parse a double literal string.
pub fn double_lit<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, f64> {
  let (i, (sign, f)) = tuple((
    opt(char('-')),
    terminated(floating_middle, pair(not(float_suffix), opt(double_suffix))),
  ))(i)?;

  // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
  let n: f64 = if f.as_bytes()[0] == b'.' {
    let mut f_ = f.to_string();
    f_.insert(0, '0');
    f_.parse().unwrap()
  } else {
    f.fragment().parse().unwrap()
  };

  // handle the sign and return
  let r = if sign.is_some() { -n } else { n };
  Ok((i, r))
}

/// Parse a constant boolean.
pub fn bool_lit<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, bool> {
  alt((value(true, keyword("true")), value(false, keyword("false"))))(i)
}

/// Parse a path literal.
pub fn path_lit<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Path> {
  alt((
    map(path_lit_absolute, syntax::Path::Absolute),
    map(path_lit_relative, syntax::Path::Relative),
  ))(i)
}

/// Parse a path literal with angle brackets.
pub fn path_lit_absolute<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, String> {
  map(
    delimited(char('<'), cut(take_until(">")), cut(char('>'))),
    |s: ParseInput<'c, 'd, 'e>| s.to_string(),
  )(i)
}

/// Parse a path literal with double quotes.
pub fn path_lit_relative<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, String> {
  map(
    delimited(char('"'), cut(take_until("\"")), cut(char('"'))),
    |s: ParseInput<'c, 'd, 'e>| s.to_string(),
  )(i)
}

/// Parse a unary operator.
pub fn unary_op<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::UnaryOp> {
  alt((
    value(syntax::UnaryOp::Inc, tag("++")),
    value(syntax::UnaryOp::Dec, tag("--")),
    value(syntax::UnaryOp::Add, char('+')),
    value(syntax::UnaryOp::Minus, char('-')),
    value(syntax::UnaryOp::Not, char('!')),
    value(syntax::UnaryOp::Complement, char('~')),
  ))(i)
}

/// Parse an identifier with an optional array specifier.
pub fn arrayed_identifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ArrayedIdentifier> {
  map(
    pair(identifier, opt(preceded(blank, array_specifier))),
    |(i, a)| syntax::ArrayedIdentifier::new(i, a),
  )(i)
}

/// Parse a struct field declaration.
pub fn struct_field_specifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::StructFieldSpecifier> {
  let (i, (qualifier, ty, identifiers, _)) = tuple((
    opt(terminated(type_qualifier, blank)),
    terminated(type_specifier, blank),
    cut(separated_list0(
      terminated(char(','), blank),
      terminated(arrayed_identifier, blank),
    )),
    cut(char(';')),
  ))(i)?;

  let r = syntax::StructFieldSpecifier {
    qualifier,
    ty,
    identifiers: syntax::NonEmpty(identifiers),
  };

  Ok((i, r))
}

/// Parse a struct.
pub fn struct_specifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::StructSpecifier> {
  preceded(
    terminated(keyword("struct"), blank),
    map(
      pair(
        opt(terminated(type_name, blank)),
        cut(delimited(
          terminated(char('{'), blank),
          many1(terminated(struct_field_specifier, blank)),
          char('}'),
        )),
      ),
      |(name, fields)| syntax::StructSpecifier {
        name,
        fields: syntax::NonEmpty(fields),
      },
    ),
  )(i)
}

/// Parse a storage qualifier subroutine rule with a list of type names.
pub fn storage_qualifier_subroutine_list<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::StorageQualifier> {
  map(
    preceded(
      terminated(keyword("subroutine"), blank),
      delimited(
        terminated(char('('), blank),
        cut(terminated(nonempty_type_names, blank)),
        cut(char(')')),
      ),
    ),
    syntax::StorageQualifier::Subroutine,
  )(i)
}

/// Parse a storage qualifier subroutine rule.
pub fn storage_qualifier_subroutine<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::StorageQualifier> {
  alt((
    storage_qualifier_subroutine_list,
    value(
      syntax::StorageQualifier::Subroutine(Vec::new()),
      keyword("subroutine"),
    ),
  ))(i)
}

/// Parse a storage qualifier.
pub fn storage_qualifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::StorageQualifier> {
  alt((
    value(syntax::StorageQualifier::Const, keyword("const")),
    value(syntax::StorageQualifier::InOut, keyword("inout")),
    value(syntax::StorageQualifier::In, keyword("in")),
    value(syntax::StorageQualifier::Out, keyword("out")),
    value(syntax::StorageQualifier::Centroid, keyword("centroid")),
    value(syntax::StorageQualifier::Patch, keyword("patch")),
    value(syntax::StorageQualifier::Sample, keyword("sample")),
    value(syntax::StorageQualifier::Uniform, keyword("uniform")),
    value(syntax::StorageQualifier::Attribute, keyword("attribute")),
    value(syntax::StorageQualifier::Varying, keyword("varying")),
    value(syntax::StorageQualifier::Buffer, keyword("buffer")),
    value(syntax::StorageQualifier::Shared, keyword("shared")),
    value(syntax::StorageQualifier::Coherent, keyword("coherent")),
    value(syntax::StorageQualifier::Volatile, keyword("volatile")),
    value(syntax::StorageQualifier::Restrict, keyword("restrict")),
    value(syntax::StorageQualifier::ReadOnly, keyword("readonly")),
    value(syntax::StorageQualifier::WriteOnly, keyword("writeonly")),
    storage_qualifier_subroutine,
  ))(i)
}

/// Parse a layout qualifier.
pub fn layout_qualifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::LayoutQualifier> {
  preceded(
    terminated(keyword("layout"), blank),
    delimited(
      terminated(char('('), blank),
      cut(layout_qualifier_inner),
      cut(char(')')),
    ),
  )(i)
}

fn layout_qualifier_inner<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::LayoutQualifier> {
  map(
    separated_list0(
      terminated(char(','), blank),
      terminated(layout_qualifier_spec, blank),
    ),
    |ids| syntax::LayoutQualifier {
      ids: syntax::NonEmpty(ids),
    },
  )(i)
}

fn layout_qualifier_spec<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::LayoutQualifierSpec> {
  alt((
    value(syntax::LayoutQualifierSpec::Shared, keyword("shared")),
    map(
      separated_pair(
        terminated(identifier, blank),
        terminated(char('='), blank),
        cond_expr,
      ),
      |(i, e)| syntax::LayoutQualifierSpec::Identifier(i, Some(Box::new(e))),
    ),
    map(identifier, |i| {
      syntax::LayoutQualifierSpec::Identifier(i, None)
    }),
  ))(i)
}

/// Parse a precision qualifier.
pub fn precision_qualifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PrecisionQualifier> {
  alt((
    value(syntax::PrecisionQualifier::High, keyword("highp")),
    value(syntax::PrecisionQualifier::Medium, keyword("mediump")),
    value(syntax::PrecisionQualifier::Low, keyword("lowp")),
  ))(i)
}

/// Parse an interpolation qualifier.
pub fn interpolation_qualifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::InterpolationQualifier> {
  alt((
    value(syntax::InterpolationQualifier::Smooth, keyword("smooth")),
    value(syntax::InterpolationQualifier::Flat, keyword("flat")),
    value(
      syntax::InterpolationQualifier::NoPerspective,
      keyword("noperspective"),
    ),
  ))(i)
}

/// Parse an invariant qualifier.
pub fn invariant_qualifier<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  value((), keyword("invariant"))(i)
}

/// Parse a precise qualifier.
pub fn precise_qualifier<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, ()> {
  value((), keyword("precise"))(i)
}

/// Parse a type qualifier.
pub fn type_qualifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TypeQualifier> {
  map(many1(terminated(type_qualifier_spec, blank)), |qlfs| {
    syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(qlfs),
    }
  })(i)
}

/// Parse a type qualifier spec.
pub fn type_qualifier_spec<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TypeQualifierSpec> {
  alt((
    map(storage_qualifier, syntax::TypeQualifierSpec::Storage),
    map(layout_qualifier, syntax::TypeQualifierSpec::Layout),
    map(precision_qualifier, syntax::TypeQualifierSpec::Precision),
    map(
      interpolation_qualifier,
      syntax::TypeQualifierSpec::Interpolation,
    ),
    value(syntax::TypeQualifierSpec::Invariant, invariant_qualifier),
    value(syntax::TypeQualifierSpec::Precise, precise_qualifier),
  ))(i)
}

/// Parse a fully specified type.
pub fn fully_specified_type<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FullySpecifiedType> {
  map(
    pair(opt(type_qualifier), type_specifier),
    |(qualifier, ty)| syntax::FullySpecifiedType { qualifier, ty },
  )(i)
}

/// Parse an array specifier
pub fn array_specifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ArraySpecifier> {
  map(
    many1(delimited(blank, array_specifier_dimension, blank)),
    |dimensions| syntax::ArraySpecifier {
      dimensions: syntax::NonEmpty(dimensions),
    },
  )(i)
}

/// Parse an array specifier dimension.
pub fn array_specifier_dimension<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ArraySpecifierDimension> {
  alt((
    value(
      syntax::ArraySpecifierDimension::Unsized,
      delimited(char('['), blank, char(']')),
    ),
    map(
      delimited(
        terminated(char('['), blank),
        cut(cond_expr),
        preceded(blank, cut(char(']'))),
      ),
      |e| syntax::ArraySpecifierDimension::ExplicitlySized(Box::new(e)),
    ),
  ))(i)
}

/// Parse a primary expression.
pub fn primary_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  alt((
    parens_expr,
    map(float_lit, syntax::Expr::FloatConst),
    map(double_lit, syntax::Expr::DoubleConst),
    map(unsigned_lit, syntax::Expr::UIntConst),
    map(integral_lit, syntax::Expr::IntConst),
    map(bool_lit, syntax::Expr::BoolConst),
    map(identifier, syntax::Expr::Variable),
  ))(i)
}

/// Parse a postfix expression.
pub fn postfix_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, e) = alt((
    function_call_with_identifier,
    function_call_with_expr_ident_or_expr,
  ))(i)?;

  postfix_part(i, e)
}

// Parse the postfix part of a primary expression. This function will just parse until it cannot
// find any more postfix construct.
fn postfix_part<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
  e: syntax::Expr,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let r = alt((
    map(preceded(blank, array_specifier), |a| {
      syntax::Expr::Bracket(Box::new(e.clone()), a)
    }),
    map(preceded(blank, dot_field_selection), |i| {
      syntax::Expr::Dot(Box::new(e.clone()), i)
    }),
    value(
      syntax::Expr::PostInc(Box::new(e.clone())),
      preceded(blank, tag("++")),
    ),
    value(
      syntax::Expr::PostDec(Box::new(e.clone())),
      preceded(blank, tag("--")),
    ),
  ))(i);

  match r {
    Ok((i, e)) => postfix_part(i, e),
    Err(NomErr::Error(_)) => Ok((i, e)),
    _ => r,
  }
}

/// Parse a unary expression.
pub fn unary_expr<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  alt((
    map(separated_pair(unary_op, blank, unary_expr), |(op, e)| {
      syntax::Expr::Unary(op, Box::new(e))
    }),
    postfix_expr,
  ))(i)
}

/// Parse an expression between parens.
pub fn parens_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  delimited(
    terminated(char('('), blank),
    expr,
    preceded(blank, cut(char(')'))),
  )(i)
}

/// Parse a dot field selection identifier.
pub fn dot_field_selection<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Identifier> {
  preceded(terminated(char('.'), blank), cut(identifier))(i)
}

/// Parse a declaration.
pub fn declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Declaration> {
  parse_located!(i, {
    alt((
      map(
        terminated(function_prototype, terminated(blank, char(';'))),
        syntax::DeclarationData::FunctionPrototype,
      ),
      map(
        terminated(init_declarator_list, terminated(blank, char(';'))),
        syntax::DeclarationData::InitDeclaratorList,
      ),
      precision_declaration,
      block_declaration,
      global_declaration,
    ))(i)
  })
}

/// Parse a precision declaration.
pub fn precision_declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::DeclarationData> {
  delimited(
    terminated(keyword("precision"), blank),
    map(
      cut(pair(
        terminated(precision_qualifier, blank),
        terminated(type_specifier, blank),
      )),
      |(qual, ty)| syntax::DeclarationData::Precision(qual, ty),
    ),
    char(';'),
  )(i)
}

/// Parse a block declaration.
pub fn block_declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::DeclarationData> {
  map(
    tuple((
      terminated(type_qualifier, blank),
      terminated(identifier, blank),
      delimited(
        terminated(char('{'), blank),
        many1(terminated(struct_field_specifier, blank)),
        cut(terminated(char('}'), blank)),
      ),
      alt((
        value(None, preceded(blank, char(';'))),
        terminated(
          opt(preceded(blank, arrayed_identifier)),
          preceded(blank, cut(char(';'))),
        ),
      )),
    )),
    |(qualifier, name, fields, identifier)| {
      syntax::DeclarationData::Block(syntax::Block {
        qualifier,
        name,
        fields,
        identifier,
      })
    },
  )(i)
}

/// Parse a global declaration.
pub fn global_declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::DeclarationData> {
  map(
    pair(
      terminated(type_qualifier, blank),
      many0(delimited(terminated(char(','), blank), identifier, blank)),
    ),
    |(qual, idents)| syntax::DeclarationData::Global(qual, idents),
  )(i)
}

/// Parse a function prototype.
pub fn function_prototype<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionPrototype> {
  terminated(function_declarator, terminated(blank, cut(char(')'))))(i)
}

/// Parse an init declarator list.
pub fn init_declarator_list<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::InitDeclaratorList> {
  map(
    pair(
      single_declaration,
      many0(map(
        tuple((
          preceded(delimited(blank, char(','), blank), cut(identifier)),
          opt(preceded(blank, array_specifier)),
          opt(preceded(delimited(blank, char('='), blank), initializer)),
        )),
        |(name, arr_spec, init)| syntax::SingleDeclarationNoType {
          ident: syntax::ArrayedIdentifier::new(name, arr_spec),
          initializer: init,
        },
      )),
    ),
    |(head, tail)| syntax::InitDeclaratorList { head, tail },
  )(i)
}

/// Parse a single declaration.
pub fn single_declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::SingleDeclaration> {
  let (i, ty) = fully_specified_type(i)?;
  let ty_ = ty.clone();

  alt((
    map(
      tuple((
        preceded(blank, identifier),
        opt(preceded(blank, array_specifier)),
        opt(preceded(
          delimited(blank, char('='), blank),
          cut(initializer),
        )),
      )),
      move |(name, array_specifier, initializer)| syntax::SingleDeclaration {
        ty: ty_.clone(),
        name: Some(name),
        array_specifier,
        initializer,
      },
    ),
    cnst(syntax::SingleDeclaration {
      ty,
      name: None,
      array_specifier: None,
      initializer: None,
    }),
  ))(i)
}

/// Parse an initializer.
pub fn initializer<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Initializer> {
  alt((
    map(assignment_expr, |e| {
      syntax::Initializer::Simple(Box::new(e))
    }),
    map(
      delimited(
        terminated(char('{'), blank),
        terminated(
          cut(initializer_list),
          terminated(blank, opt(terminated(char(','), blank))),
        ),
        cut(char('}')),
      ),
      |il| syntax::Initializer::List(syntax::NonEmpty(il)),
    ),
  ))(i)
}

/// Parse an initializer list.
pub fn initializer_list<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Vec<syntax::Initializer>> {
  separated_list0(delimited(blank, char(','), blank), initializer)(i)
}

fn function_declarator<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionPrototype> {
  parse_located!(i, {
    alt((
      function_header_with_parameters,
      map(function_header, |(ty, name)| {
        syntax::FunctionPrototypeData {
          ty,
          name,
          parameters: Vec::new(),
        }
      }),
    ))(i)
  })
}

fn function_header<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, (syntax::FullySpecifiedType, syntax::Identifier)> {
  pair(
    terminated(fully_specified_type, blank),
    terminated(identifier, terminated(blank, char('('))),
  )(i)
}

fn function_header_with_parameters<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionPrototypeData> {
  map(
    pair(
      function_header,
      separated_list0(
        preceded(blank, char(',')),
        preceded(blank, function_parameter_declaration),
      ),
    ),
    |(header, parameters)| syntax::FunctionPrototypeData {
      ty: header.0,
      name: header.1,
      parameters,
    },
  )(i)
}

fn function_parameter_declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionParameterDeclaration> {
  alt((
    function_parameter_declaration_named,
    function_parameter_declaration_unnamed,
  ))(i)
}

fn function_parameter_declaration_named<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionParameterDeclaration> {
  parse_located!(i, {
    map(
      pair(
        opt(terminated(type_qualifier, blank)),
        function_parameter_declarator,
      ),
      |(ty_qual, fpd)| syntax::FunctionParameterDeclarationData::Named(ty_qual, fpd),
    )(i)
  })
}

fn function_parameter_declaration_unnamed<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionParameterDeclaration> {
  parse_located!(i, {
    map(
      pair(opt(terminated(type_qualifier, blank)), type_specifier),
      |(ty_qual, ty_spec)| syntax::FunctionParameterDeclarationData::Unnamed(ty_qual, ty_spec),
    )(i)
  })
}

fn function_parameter_declarator<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionParameterDeclarator> {
  map(
    tuple((
      terminated(type_specifier, blank),
      terminated(identifier, blank),
      opt(array_specifier),
    )),
    |(ty, name, a)| syntax::FunctionParameterDeclarator {
      ty,
      ident: syntax::ArrayedIdentifier::new(name, a),
    },
  )(i)
}

fn function_call_with_identifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  map(
    tuple((function_identifier_identifier, function_call_args)),
    |(fi, args)| syntax::Expr::FunCall(fi, args),
  )(i)
}

fn function_call_with_expr_ident_or_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  map(
    tuple((function_identifier_expr, opt(function_call_args))),
    |(expr, args)| match args {
      Some(args) => syntax::Expr::FunCall(expr, args),
      None => expr.into_expr().unwrap(),
    },
  )(i)
}

fn function_call_args<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, Vec<syntax::Expr>> {
  preceded(
    terminated(terminated(blank, char('(')), blank),
    alt((
      map(
        terminated(blank, terminated(opt(void), terminated(blank, char(')')))),
        |_| vec![],
      ),
      terminated(
        separated_list0(
          terminated(char(','), blank),
          cut(terminated(assignment_expr, blank)),
        ),
        cut(char(')')),
      ),
    )),
  )(i)
}

fn function_identifier_identifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunIdentifier> {
  map(
    terminated(identifier, terminated(blank, peek(char('(')))),
    syntax::FunIdentifier::Identifier,
  )(i)
}

fn function_identifier_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunIdentifier> {
  (|i| {
    let (i, e) = primary_expr(i)?;
    postfix_part(i, e).map(|(i, pfe)| (i, syntax::FunIdentifier::Expr(Box::new(pfe))))
  })(i)
}

/// Parse a function identifier just behind a function list argument.
pub fn function_identifier<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunIdentifier> {
  alt((function_identifier_identifier, function_identifier_expr))(i)
}

/// Parse the most general expression.
pub fn expr<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, first) = assignment_expr(i)?;
  let first_ = first.clone();

  alt((
    map(preceded(terminated(char(','), blank), expr), move |next| {
      syntax::Expr::Comma(Box::new(first_.clone()), Box::new(next))
    }),
    cnst(first),
  ))(i)
}

/// Parse an assignment expression.
pub fn assignment_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  alt((
    map(
      tuple((
        terminated(unary_expr, blank),
        terminated(assignment_op, blank),
        assignment_expr,
      )),
      |(e, o, v)| syntax::Expr::Assignment(Box::new(e), o, Box::new(v)),
    ),
    cond_expr,
  ))(i)
}

/// Parse an assignment operator.
pub fn assignment_op<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::AssignmentOp> {
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
    value(syntax::AssignmentOp::Or, tag("|=")),
  ))(i)
}

/// Parse a conditional expression.
pub fn cond_expr<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = logical_or_expr(i)?;

  fold_many0(
    tuple((
      delimited(blank, char('?'), blank),
      cut(terminated(expr, blank)),
      cut(terminated(char(':'), blank)),
      cut(assignment_expr),
    )),
    move || a.clone(),
    move |acc, (_, b, _, c)| syntax::Expr::Ternary(Box::new(acc), Box::new(b), Box::new(c)),
  )(i)
}

/// Parse a logical OR expression.
pub fn logical_or_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = logical_xor_expr(i)?;

  fold_many0(
    preceded(delimited(blank, tag("||"), blank), logical_xor_expr),
    move || a.clone(),
    move |acc, b| syntax::Expr::Binary(syntax::BinaryOp::Or, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a logical XOR expression.
pub fn logical_xor_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = logical_and_expr(i)?;

  fold_many0(
    preceded(delimited(blank, tag("^^"), blank), logical_and_expr),
    move || a.clone(),
    move |acc, b| syntax::Expr::Binary(syntax::BinaryOp::Xor, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a logical AND expression.
pub fn logical_and_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = inclusive_or_expr(i)?;

  fold_many0(
    preceded(delimited(blank, tag("&&"), blank), inclusive_or_expr),
    move || a.clone(),
    move |acc, b| syntax::Expr::Binary(syntax::BinaryOp::And, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a bitwise OR expression.
pub fn inclusive_or_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = exclusive_or_expr(i)?;

  fold_many0(
    preceded(delimited(blank, char('|'), blank), inclusive_or_expr),
    move || a.clone(),
    move |acc, b| syntax::Expr::Binary(syntax::BinaryOp::BitOr, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a bitwise XOR expression.
pub fn exclusive_or_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = and_expr(i)?;

  fold_many0(
    preceded(delimited(blank, char('^'), blank), exclusive_or_expr),
    move || a.clone(),
    move |acc, b| syntax::Expr::Binary(syntax::BinaryOp::BitXor, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a bitwise AND expression.
pub fn and_expr<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = equality_expr(i)?;

  fold_many0(
    preceded(delimited(blank, char('&'), blank), and_expr),
    move || a.clone(),
    move |acc, b| syntax::Expr::Binary(syntax::BinaryOp::BitAnd, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse an equality expression.
pub fn equality_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = rel_expr(i)?;

  fold_many0(
    pair(
      delimited(
        blank,
        alt((
          value(syntax::BinaryOp::Equal, tag("==")),
          value(syntax::BinaryOp::NonEqual, tag("!=")),
        )),
        blank,
      ),
      rel_expr,
    ),
    move || a.clone(),
    move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a relational expression.
pub fn rel_expr<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = shift_expr(i)?;

  fold_many0(
    pair(
      delimited(
        blank,
        alt((
          value(syntax::BinaryOp::LTE, tag("<=")),
          value(syntax::BinaryOp::GTE, tag(">=")),
          value(syntax::BinaryOp::LT, char('<')),
          value(syntax::BinaryOp::GT, char('>')),
        )),
        blank,
      ),
      shift_expr,
    ),
    move || a.clone(),
    move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a shift expression.
pub fn shift_expr<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = additive_expr(i)?;

  fold_many0(
    pair(
      delimited(
        blank,
        alt((
          value(syntax::BinaryOp::LShift, tag("<<")),
          value(syntax::BinaryOp::RShift, tag(">>")),
        )),
        blank,
      ),
      additive_expr,
    ),
    move || a.clone(),
    move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse an additive expression.
pub fn additive_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = multiplicative_expr(i)?;

  fold_many0(
    pair(
      delimited(
        blank,
        alt((
          value(syntax::BinaryOp::Add, char('+')),
          value(syntax::BinaryOp::Sub, char('-')),
        )),
        blank,
      ),
      multiplicative_expr,
    ),
    move || a.clone(),
    move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a multiplicative expression.
pub fn multiplicative_expr<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Expr> {
  let (i, a) = unary_expr(i)?;

  fold_many0(
    pair(
      delimited(
        blank,
        alt((
          value(syntax::BinaryOp::Mult, char('*')),
          value(syntax::BinaryOp::Div, char('/')),
          value(syntax::BinaryOp::Mod, char('%')),
        )),
        blank,
      ),
      unary_expr,
    ),
    move || a.clone(),
    move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc), Box::new(b)),
  )(i)
}

/// Parse a simple statement.
pub fn simple_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::SimpleStatement> {
  alt((
    map(jump_statement, syntax::SimpleStatement::Jump),
    map(iteration_statement, syntax::SimpleStatement::Iteration),
    map(case_label, syntax::SimpleStatement::CaseLabel),
    map(switch_statement, syntax::SimpleStatement::Switch),
    map(selection_statement, syntax::SimpleStatement::Selection),
    map(declaration, |d| syntax::SimpleStatement::Declaration(d)),
    map(expr_statement, syntax::SimpleStatement::Expression),
  ))(i)
}

/// Parse an expression statement.
pub fn expr_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ExprStatement> {
  terminated(terminated(opt(expr), blank), char(';'))(i)
}

/// Parse a selection statement.
pub fn selection_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::SelectionStatement> {
  map(
    tuple((
      terminated(keyword("if"), blank),
      cut(terminated(char('('), blank)),
      cut(terminated(expr, blank)),
      cut(terminated(char(')'), blank)),
      cut(selection_rest_statement),
    )),
    |(_, _, cond_expr, _, rest)| syntax::SelectionStatement {
      cond: Box::new(cond_expr),
      rest,
    },
  )(i)
}

fn selection_rest_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::SelectionRestStatement> {
  let (i, st) = statement(i)?;
  let st_ = st.clone();

  alt((
    map(
      preceded(delimited(blank, keyword("else"), blank), cut(statement)),
      move |rest| syntax::SelectionRestStatement::Else(Box::new(st_.clone()), Box::new(rest)),
    ),
    cnst(syntax::SelectionRestStatement::Statement(Box::new(st))),
  ))(i)
}

/// Parse a switch statement.
pub fn switch_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::SwitchStatement> {
  map(
    tuple((
      terminated(keyword("switch"), blank),
      cut(terminated(char('('), blank)),
      cut(terminated(expr, blank)),
      cut(terminated(char(')'), blank)),
      cut(terminated(char('{'), blank)),
      cut(many0(terminated(statement, blank))),
      cut(char('}')),
    )),
    |(_, _, head, _, _, body, _)| syntax::SwitchStatement {
      head: Box::new(head),
      body,
    },
  )(i)
}

/// Parse a case label.
pub fn case_label<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::CaseLabel> {
  alt((
    map(
      delimited(
        terminated(keyword("case"), blank),
        cut(terminated(expr, blank)),
        cut(char(':')),
      ),
      |e| syntax::CaseLabel::Case(Box::new(e)),
    ),
    value(
      syntax::CaseLabel::Def,
      preceded(terminated(keyword("default"), blank), char(':')),
    ),
  ))(i)
}

/// Parse an iteration statement.
pub fn iteration_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::IterationStatement> {
  alt((
    iteration_statement_while,
    iteration_statement_do_while,
    iteration_statement_for,
  ))(i)
}

/// Parse a while statement.
pub fn iteration_statement_while<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::IterationStatement> {
  map(
    tuple((
      terminated(keyword("while"), blank),
      cut(terminated(char('('), blank)),
      cut(terminated(condition, blank)),
      cut(terminated(char(')'), blank)),
      cut(statement),
    )),
    |(_, _, cond, _, st)| syntax::IterationStatement::While(cond, Box::new(st)),
  )(i)
}

/// Parse a while statement.
pub fn iteration_statement_do_while<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::IterationStatement> {
  map(
    tuple((
      terminated(keyword("do"), blank),
      cut(terminated(statement, blank)),
      cut(terminated(keyword("while"), blank)),
      cut(terminated(char('('), blank)),
      cut(terminated(expr, blank)),
      cut(terminated(char(')'), blank)),
      cut(char(';')),
    )),
    |(_, st, _, _, e, _, _)| syntax::IterationStatement::DoWhile(Box::new(st), Box::new(e)),
  )(i)
}

// Parse a for statement.
pub fn iteration_statement_for<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::IterationStatement> {
  map(
    tuple((
      terminated(keyword("for"), blank),
      cut(terminated(char('('), blank)),
      cut(terminated(iteration_statement_for_init_statement, blank)),
      cut(terminated(iteration_statement_for_rest_statement, blank)),
      cut(terminated(char(')'), blank)),
      cut(statement),
    )),
    |(_, _, head, rest, _, body)| syntax::IterationStatement::For(head, rest, Box::new(body)),
  )(i)
}

fn iteration_statement_for_init_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ForInitStatement> {
  alt((
    map(expr_statement, syntax::ForInitStatement::Expression),
    map(declaration, |d| {
      syntax::ForInitStatement::Declaration(Box::new(d))
    }),
  ))(i)
}

fn iteration_statement_for_rest_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ForRestStatement> {
  map(
    separated_pair(
      opt(terminated(condition, blank)),
      terminated(char(';'), blank),
      opt(expr),
    ),
    |(condition, e)| syntax::ForRestStatement {
      condition,
      post_expr: e.map(Box::new),
    },
  )(i)
}

/// Parse a jump statement.
pub fn jump_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::JumpStatement> {
  alt((
    jump_statement_continue,
    jump_statement_break,
    jump_statement_return,
    jump_statement_discard,
  ))(i)
}

// Parse a continue statement.
pub fn jump_statement_continue<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Continue,
    terminated(keyword("continue"), cut(terminated(blank, char(';')))),
  )(i)
}

// Parse a break statement.
pub fn jump_statement_break<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Break,
    terminated(keyword("break"), cut(terminated(blank, char(';')))),
  )(i)
}

// Parse a discard statement.
pub fn jump_statement_discard<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Discard,
    terminated(keyword("discard"), cut(terminated(blank, char(';')))),
  )(i)
}

// Parse a return statement.
pub fn jump_statement_return<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::JumpStatement> {
  map(
    delimited(
      terminated(keyword("return"), blank),
      opt(terminated(expr, blank)),
      cut(char(';')),
    ),
    |e| syntax::JumpStatement::Return(e.map(|e| Box::new(e))),
  )(i)
}

/// Parse a condition.
pub fn condition<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Condition> {
  alt((
    map(expr, |e| syntax::Condition::Expr(Box::new(e))),
    condition_assignment,
  ))(i)
}

fn condition_assignment<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Condition> {
  map(
    tuple((
      terminated(fully_specified_type, blank),
      terminated(identifier, blank),
      terminated(char('='), blank),
      cut(initializer),
    )),
    |(ty, id, _, ini)| syntax::Condition::Assignment(ty, id, ini),
  )(i)
}

/// Parse a statement.
pub fn statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Statement> {
  alt((
    map(compound_statement, |c| {
      syntax::Statement::Compound(Box::new(c))
    }),
    map(simple_statement, |s| syntax::Statement::Simple(Box::new(s))),
  ))(i)
}

/// Parse a compound statement.
pub fn compound_statement<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::CompoundStatement> {
  parse_located!(i, {
    map(
      delimited(
        terminated(char('{'), blank),
        many0(terminated(statement, blank)),
        cut(char('}')),
      ),
      |statement_list| syntax::CompoundStatementData { statement_list },
    )(i)
  })
}

/// Parse a function definition.
pub fn function_definition<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::FunctionDefinition> {
  parse_located!(i, {
    map(
      pair(terminated(function_prototype, blank), compound_statement),
      |(prototype, statement)| syntax::FunctionDefinitionData {
        prototype,
        statement,
      },
    )(i)
  })
}

/// Parse an external declaration.
pub fn external_declaration<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::ExternalDeclaration> {
  parse_located!(i, {
    alt((
      map(preprocessor, syntax::ExternalDeclarationData::Preprocessor),
      map(
        function_definition,
        syntax::ExternalDeclarationData::FunctionDefinition,
      ),
      map(declaration, syntax::ExternalDeclarationData::Declaration),
      preceded(
        delimited(blank, char(';'), blank),
        cut(|i| external_declaration(i).map(|(i, r)| (i, r.into_inner()))),
      ),
    ))(i)
  })
}

/// Parse a translation unit (entry point).
pub fn translation_unit<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::TranslationUnit> {
  map(
    many1(delimited(blank, external_declaration, blank)),
    |eds| syntax::TranslationUnit(syntax::NonEmpty(eds)),
  )(i)
}

/// Parse a preprocessor directive.
pub fn preprocessor<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Preprocessor> {
  parse_located!(i, {
    preceded(
      terminated(char('#'), pp_space0),
      cut(alt((
        map(pp_define, syntax::PreprocessorData::Define),
        value(syntax::PreprocessorData::Else, pp_else),
        map(pp_elseif, syntax::PreprocessorData::ElseIf),
        value(syntax::PreprocessorData::EndIf, pp_endif),
        map(pp_error, syntax::PreprocessorData::Error),
        map(pp_if, syntax::PreprocessorData::If),
        map(pp_ifdef, syntax::PreprocessorData::IfDef),
        map(pp_ifndef, syntax::PreprocessorData::IfNDef),
        map(pp_include, syntax::PreprocessorData::Include),
        map(pp_line, syntax::PreprocessorData::Line),
        map(pp_pragma, syntax::PreprocessorData::Pragma),
        map(pp_undef, syntax::PreprocessorData::Undef),
        map(pp_version, syntax::PreprocessorData::Version),
        map(pp_extension, syntax::PreprocessorData::Extension),
      ))),
    )(i)
  })
}

/// Parse a preprocessor version number.
pub fn pp_version_number<'c, 'd, 'e>(i: ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, u16> {
  map(digit1, |x: ParseInput<'c, 'd, 'e>| x.parse_to().unwrap())(i)
}

/// Parse a preprocessor version profile.
pub fn pp_version_profile<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorVersionProfile> {
  alt((
    value(syntax::PreprocessorVersionProfile::Core, keyword("core")),
    value(
      syntax::PreprocessorVersionProfile::Compatibility,
      keyword("compatibility"),
    ),
    value(syntax::PreprocessorVersionProfile::ES, keyword("es")),
  ))(i)
}

/// The space parser in preprocessor directives.
///
/// This parser is needed to authorize breaking a line with the multiline annotation (\).
pub fn pp_space0<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, ParseInput<'c, 'd, 'e>> {
  recognize(many0_(alt((space1, tag("\\\n")))))(i)
}

/// Parse a preprocessor define.
pub fn pp_define<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorDefine> {
  let (i, ident) = map(
    tuple((terminated(keyword("define"), pp_space0), cut(identifier))),
    |(_, ident)| ident,
  )(i)?;

  alt((
    pp_define_function_like(ident.clone()),
    pp_define_object_like(ident),
  ))(i)
}

// Parse an object-like #define content.
pub fn pp_define_object_like<'e, 'd: 'e, 'c: 'd + 'e>(
  ident: syntax::Identifier,
) -> impl Fn(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorDefine> {
  move |i| {
    map(preceded(pp_space0, cut(str_till_eol)), |value| {
      syntax::PreprocessorDefine::ObjectLike {
        ident: ident.clone(),
        value: value.to_string(),
      }
    })(i)
  }
}

// Parse a function-like #define content.
pub fn pp_define_function_like<'e, 'd: 'e, 'c: 'd + 'e>(
  ident: syntax::Identifier,
) -> impl Fn(ParseInput<'c, 'd, 'e>) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorDefine> {
  move |i| {
    map(
      tuple((
        terminated(char('('), pp_space0),
        separated_list0(
          terminated(char(','), pp_space0),
          cut(terminated(identifier, pp_space0)),
        ),
        cut(terminated(char(')'), pp_space0)),
        cut(map(str_till_eol, |s| s.to_string())),
      )),
      |(_, args, _, value)| syntax::PreprocessorDefine::FunctionLike {
        ident: ident.clone(),
        args,
        value,
      },
    )(i)
  }
}

/// Parse a preprocessor else.
pub fn pp_else<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Preprocessor> {
  parse_located!(i, {
    value(
      syntax::PreprocessorData::Else,
      tuple((terminated(keyword("else"), pp_space0), cut(eol))),
    )(i)
  })
}

/// Parse a preprocessor elseif.
pub fn pp_elseif<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorElseIf> {
  map(
    tuple((
      terminated(keyword("elseif"), pp_space0),
      cut(map(str_till_eol, |s| s.to_string())),
    )),
    |(_, condition)| syntax::PreprocessorElseIf { condition },
  )(i)
}

/// Parse a preprocessor endif.
pub fn pp_endif<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::Preprocessor> {
  parse_located!(i, {
    map(
      tuple((terminated(keyword("endif"), space0), cut(eol))),
      |(_, _)| syntax::PreprocessorData::EndIf,
    )(i)
  })
}

/// Parse a preprocessor error.
pub fn pp_error<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorError> {
  map(
    tuple((terminated(keyword("error"), pp_space0), cut(str_till_eol))),
    |(_, message)| syntax::PreprocessorError {
      message: message.to_string(),
    },
  )(i)
}

/// Parse a preprocessor if.
pub fn pp_if<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorIf> {
  map(
    tuple((
      terminated(keyword("if"), pp_space0),
      cut(map(str_till_eol, |s| s.to_string())),
    )),
    |(_, condition)| syntax::PreprocessorIf { condition },
  )(i)
}

/// Parse a preprocessor ifdef.
pub fn pp_ifdef<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorIfDef> {
  map(
    tuple((
      terminated(keyword("ifdef"), pp_space0),
      cut(terminated(identifier, pp_space0)),
      eol,
    )),
    |(_, ident, _)| syntax::PreprocessorIfDef { ident },
  )(i)
}

/// Parse a preprocessor ifndef.
pub fn pp_ifndef<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorIfNDef> {
  map(
    tuple((
      terminated(keyword("ifndef"), pp_space0),
      cut(terminated(identifier, pp_space0)),
      eol,
    )),
    |(_, ident, _)| syntax::PreprocessorIfNDef { ident },
  )(i)
}

/// Parse a preprocessor include.
pub fn pp_include<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorInclude> {
  map(
    tuple((
      terminated(keyword("include"), pp_space0),
      cut(terminated(path_lit, pp_space0)),
      cut(eol),
    )),
    |(_, path, _)| syntax::PreprocessorInclude { path },
  )(i)
}

/// Parse a preprocessor line.
pub fn pp_line<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorLine> {
  map(
    tuple((
      terminated(keyword("line"), pp_space0),
      cut(terminated(integral_lit, pp_space0)),
      opt(terminated(integral_lit, pp_space0)),
      cut(eol),
    )),
    |(_, line, source_string_number, _)| syntax::PreprocessorLine {
      line: line as u32,
      source_string_number: source_string_number.map(|n| n as u32),
    },
  )(i)
}

/// Parse a preprocessor pragma.
pub fn pp_pragma<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorPragma> {
  map(
    tuple((terminated(keyword("pragma"), pp_space0), cut(str_till_eol))),
    |(_, command)| syntax::PreprocessorPragma {
      command: command.to_string(),
    },
  )(i)
}

/// Parse a preprocessor undef.
pub fn pp_undef<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorUndef> {
  map(
    tuple((
      terminated(keyword("undef"), pp_space0),
      cut(terminated(identifier, pp_space0)),
      eol,
    )),
    |(_, name, _)| syntax::PreprocessorUndef { name },
  )(i)
}

/// Parse a preprocessor version.
pub fn pp_version<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorVersion> {
  map(
    tuple((
      terminated(keyword("version"), pp_space0),
      cut(terminated(pp_version_number, pp_space0)),
      opt(terminated(pp_version_profile, pp_space0)),
      cut(eol),
    )),
    |(_, version, profile, _)| syntax::PreprocessorVersion { version, profile },
  )(i)
}

/// Parse a preprocessor extension name.
pub fn pp_extension_name<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorExtensionName> {
  alt((
    value(syntax::PreprocessorExtensionName::All, keyword("all")),
    map(string, syntax::PreprocessorExtensionName::Specific),
  ))(i)
}

/// Parse a preprocessor extension behavior.
pub fn pp_extension_behavior<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorExtensionBehavior> {
  alt((
    value(
      syntax::PreprocessorExtensionBehavior::Require,
      keyword("require"),
    ),
    value(
      syntax::PreprocessorExtensionBehavior::Enable,
      keyword("enable"),
    ),
    value(syntax::PreprocessorExtensionBehavior::Warn, keyword("warn")),
    value(
      syntax::PreprocessorExtensionBehavior::Disable,
      keyword("disable"),
    ),
  ))(i)
}

/// Parse a preprocessor extension.
pub fn pp_extension<'c, 'd, 'e>(
  i: ParseInput<'c, 'd, 'e>,
) -> ParserResult<'c, 'd, 'e, syntax::PreprocessorExtension> {
  map(
    tuple((
      terminated(keyword("extension"), pp_space0),
      cut(terminated(pp_extension_name, pp_space0)),
      opt(preceded(
        terminated(char(':'), pp_space0),
        cut(terminated(pp_extension_behavior, pp_space0)),
      )),
      cut(eol),
    )),
    |(_, name, behavior, _)| syntax::PreprocessorExtension { name, behavior },
  )(i)
}
