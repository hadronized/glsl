//! GLSL parsers.
//!
//! The more general parser is `translation_unit`, that recognizes the most external form of a GLSL
//! source (a shader, basically).
//!
//! Other parsers are exported if you want more control on how you want to parse your source.

mod nom_helpers;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while1};
use nom::character::complete::{anychar, char, digit1, space0, space1};
use nom::character::{is_hex_digit, is_oct_digit};
use nom::combinator::{cut, map, not, opt, peek, recognize, value, verify};
use nom::error::{ErrorKind, ParseError as _, VerboseError, VerboseErrorKind};
use nom::multi::{many0, many1, separated_list, fold_many0};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{Err as NomErr, ParseTo};
use std::num::ParseIntError;

pub use self::nom_helpers::ParserResult;
use self::nom_helpers::{blank_space, cnst, eol, many0_, str_till_eol};
use crate::syntax;

// Parse a keyword. A keyword is just a regular string that must be followed by punctuation.
fn keyword<'a>(kwd: &'a str) -> impl Fn(&'a str) -> ParserResult<'a, &'a str> {
  terminated(
    tag(kwd),
    not(verify(peek(anychar), |&c| identifier_pred(c))),
  )
}

/// Parse a single comment.
pub fn comment(i: &str) -> ParserResult<&str> {
  preceded(
    char('/'),
    alt((
      preceded(char('/'), cut(str_till_eol)),
      preceded(char('*'), cut(terminated(take_until("*/"), tag("*/")))),
    )),
  )(i)
}

/// Parse several comments.
pub fn comments(i: &str) -> ParserResult<&str> {
  recognize(many0_(terminated(comment, blank_space)))(i)
}

/// In-between token parser (spaces and comments).
///
/// This parser also allows to break a line into two by finishing the line with a backslack ('\').
fn blank(i: &str) -> ParserResult<()> {
  value((), preceded(blank_space, comments))(i)
}

#[inline]
fn identifier_pred(ch: char) -> bool {
  ch.is_alphanumeric() || ch == '_'
}

#[inline]
fn verify_identifier(s: &str) -> bool {
  !char::from(s.as_bytes()[0]).is_digit(10)
}

/// Parse an identifier (raw version).
fn identifier_str(i: &str) -> ParserResult<&str> {
  verify(take_while1(identifier_pred), verify_identifier)(i)
}

/// Parse a string that could be used as an identifier.
pub fn string(i: &str) -> ParserResult<String> {
  map(identifier_str, String::from)(i)
}

/// Parse an identifier.
pub fn identifier(i: &str) -> ParserResult<syntax::Identifier> {
  map(string, syntax::Identifier)(i)
}

/// Parse a type name.
pub fn type_name(i: &str) -> ParserResult<syntax::TypeName> {
  map(string, syntax::TypeName)(i)
}

/// Parse a non-empty list of type names, delimited by comma (,).
fn nonempty_type_names(i: &str) -> ParserResult<Vec<syntax::TypeName>> {
  separated_list(terminated(char(','), blank), terminated(type_name, blank))(i)
}

/// Parse a type specifier non struct.
pub fn type_specifier_non_struct(i: &str) -> ParserResult<syntax::TypeSpecifierNonArray> {
  let (i1, t) = identifier_str(i)?;

  match t {
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
pub fn type_specifier_non_array(i: &str) -> ParserResult<syntax::TypeSpecifierNonArray> {
  alt((
    type_specifier_non_struct,
    map(struct_specifier, syntax::TypeSpecifierNonArray::Struct),
    map(type_name, syntax::TypeSpecifierNonArray::TypeName),
  ))(i)
}

/// Parse a type specifier.
pub fn type_specifier(i: &str) -> ParserResult<syntax::TypeSpecifier> {
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
pub fn void(i: &str) -> ParserResult<()> {
  value((), keyword("void"))(i)
}

/// Parse a digit that precludes a leading 0.
fn nonzero_digits(i: &str) -> ParserResult<&str> {
  verify(digit1, |s: &str| s.as_bytes()[0] != b'0')(i)
}

#[inline]
fn is_octal(s: &str) -> bool {
  s.as_bytes()[0] == b'0' && s.bytes().all(is_oct_digit)
}

#[inline]
fn all_hexa(s: &str) -> bool {
  s.bytes().all(is_hex_digit)
}

#[inline]
fn alphanumeric_no_u(c: char) -> bool {
  c.is_alphanumeric() && c != 'u' && c != 'U'
}

/// Parse an hexadecimal literal.
fn hexadecimal_lit(i: &str) -> ParserResult<Result<u32, ParseIntError>> {
  preceded(
    preceded(char('0'), cut(alt((char('x'), char('X'))))), // 0x | 0X
    cut(map(verify(take_while1(alphanumeric_no_u), all_hexa), |i| {
      u32::from_str_radix(i, 16)
    })),
  )(i)
}

/// Parse an octal literal.
fn octal_lit(i: &str) -> ParserResult<Result<u32, ParseIntError>> {
  map(verify(take_while1(alphanumeric_no_u), is_octal), |i| {
    u32::from_str_radix(i, 8)
  })(i)
}

/// Parse a decimal literal.
fn decimal_lit(i: &str) -> ParserResult<Result<u32, ParseIntError>> {
  map(nonzero_digits, |i| i.parse())(i)
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
pub fn integral_lit_try(i: &str) -> ParserResult<Result<i32, ParseIntError>> {
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

pub fn integral_lit(i: &str) -> ParserResult<i32> {
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
fn unsigned_suffix(i: &str) -> ParserResult<char> {
  alt((char('u'), char('U')))(i)
}

/// Parse a literal unsigned string.
pub fn unsigned_lit(i: &str) -> ParserResult<u32> {
  map(terminated(integral_lit, unsigned_suffix), |lit| lit as u32)(i)
}

/// Parse a floating point suffix.
fn float_suffix(i: &str) -> ParserResult<&str> {
  alt((keyword("f"), keyword("F")))(i)
}

/// Parse a double point suffix.
fn double_suffix(i: &str) -> ParserResult<&str> {
  alt((keyword("lf"), keyword("LF")))(i)
}

/// Parse the exponent part of a floating point literal.
fn floating_exponent(i: &str) -> ParserResult<()> {
  value(
    (),
    preceded(
      alt((char('e'), char('E'))),
      preceded(opt(alt((char('+'), char('-')))), digit1),
    ),
  )(i)
}

/// Parse the fractional constant part of a floating point literal.
fn floating_frac(i: &str) -> ParserResult<()> {
  alt((
    value((), preceded(char('.'), digit1)),
    value((), delimited(digit1, char('.'), opt(digit1))),
  ))(i)
}

/// Parse the « middle » part of a floating value – i.e. fractional and exponential parts.
fn floating_middle(i: &str) -> ParserResult<&str> {
  recognize(alt((
    value((), preceded(floating_frac, opt(floating_exponent))),
    value((), preceded(nonzero_digits, floating_exponent)),
  )))(i)
}

/// Parse a float literal string.
pub fn float_lit(i: &str) -> ParserResult<f32> {
  let (i, (sign, f)) = tuple((
    opt(char('-')),
    terminated(floating_middle, opt(float_suffix)),
  ))(i)?;

  // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
  let n: f32 = if f.as_bytes()[0] == b'.' {
    let mut f_ = f.to_owned();
    f_.insert(0, '0');

    f_.parse().unwrap()
  } else {
    f.parse().unwrap()
  };

  // handle the sign and return
  let r = if sign.is_some() { -n } else { n };
  Ok((i, r))
}

/// Parse a double literal string.
pub fn double_lit(i: &str) -> ParserResult<f64> {
  let (i, (sign, f)) = tuple((
    opt(char('-')),
    terminated(floating_middle, pair(not(float_suffix), opt(double_suffix))),
  ))(i)?;

  // if the parsed data is in the accepted form ".394634…", we parse it as if it was < 0
  let n: f64 = if f.as_bytes()[0] == b'.' {
    let mut f_ = f.to_owned();
    f_.insert(0, '0');
    f_.parse().unwrap()
  } else {
    f.parse().unwrap()
  };

  // handle the sign and return
  let r = if sign.is_some() { -n } else { n };
  Ok((i, r))
}

/// Parse a constant boolean.
pub fn bool_lit(i: &str) -> ParserResult<bool> {
  alt((value(true, keyword("true")), value(false, keyword("false"))))(i)
}

/// Parse a path literal.
pub fn path_lit(i: &str) -> ParserResult<syntax::Path> {
  alt((
    map(path_lit_absolute, syntax::Path::Absolute),
    map(path_lit_relative, syntax::Path::Relative),
  ))(i)
}

/// Parse a path literal with angle brackets.
pub fn path_lit_absolute(i: &str) -> ParserResult<String> {
  map(
    delimited(char('<'), cut(take_until(">")), cut(char('>'))),
    |s: &str| s.to_owned(),
  )(i)
}

/// Parse a path literal with double quotes.
pub fn path_lit_relative(i: &str) -> ParserResult<String> {
  map(
    delimited(char('"'), cut(take_until("\"")), cut(char('"'))),
    |s: &str| s.to_owned(),
  )(i)
}

/// Parse a unary operator.
pub fn unary_op(i: &str) -> ParserResult<syntax::UnaryOp> {
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
pub fn arrayed_identifier(i: &str) -> ParserResult<syntax::ArrayedIdentifier> {
  map(
    pair(identifier, opt(preceded(blank, array_specifier))),
    |(i, a)| syntax::ArrayedIdentifier::new(i, a),
  )(i)
}

/// Parse a struct field declaration.
pub fn struct_field_specifier(i: &str) -> ParserResult<syntax::StructFieldSpecifier> {
  let (i, (qualifier, ty, identifiers, _)) = tuple((
    opt(terminated(type_qualifier, blank)),
    terminated(type_specifier, blank),
    cut(separated_list(
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
pub fn struct_specifier(i: &str) -> ParserResult<syntax::StructSpecifier> {
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
pub fn storage_qualifier_subroutine_list(i: &str) -> ParserResult<syntax::StorageQualifier> {
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
pub fn storage_qualifier_subroutine(i: &str) -> ParserResult<syntax::StorageQualifier> {
  alt((
    storage_qualifier_subroutine_list,
    value(
      syntax::StorageQualifier::Subroutine(Vec::new()),
      keyword("subroutine"),
    ),
  ))(i)
}

/// Parse a storage qualifier.
pub fn storage_qualifier(i: &str) -> ParserResult<syntax::StorageQualifier> {
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
pub fn layout_qualifier(i: &str) -> ParserResult<syntax::LayoutQualifier> {
  preceded(
    terminated(keyword("layout"), blank),
    delimited(
      terminated(char('('), blank),
      cut(layout_qualifier_inner),
      cut(char(')')),
    ),
  )(i)
}

fn layout_qualifier_inner(i: &str) -> ParserResult<syntax::LayoutQualifier> {
  map(
    separated_list(
      terminated(char(','), blank),
      terminated(layout_qualifier_spec, blank),
    ),
    |ids| syntax::LayoutQualifier {
      ids: syntax::NonEmpty(ids),
    },
  )(i)
}

fn layout_qualifier_spec(i: &str) -> ParserResult<syntax::LayoutQualifierSpec> {
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
pub fn precision_qualifier(i: &str) -> ParserResult<syntax::PrecisionQualifier> {
  alt((
    value(syntax::PrecisionQualifier::High, keyword("highp")),
    value(syntax::PrecisionQualifier::Medium, keyword("mediump")),
    value(syntax::PrecisionQualifier::Low, keyword("lowp")),
  ))(i)
}

/// Parse an interpolation qualifier.
pub fn interpolation_qualifier(i: &str) -> ParserResult<syntax::InterpolationQualifier> {
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
pub fn invariant_qualifier(i: &str) -> ParserResult<()> {
  value((), keyword("invariant"))(i)
}

/// Parse a precise qualifier.
pub fn precise_qualifier(i: &str) -> ParserResult<()> {
  value((), keyword("precise"))(i)
}

/// Parse a type qualifier.
pub fn type_qualifier(i: &str) -> ParserResult<syntax::TypeQualifier> {
  map(many1(terminated(type_qualifier_spec, blank)), |qlfs| {
    syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(qlfs),
    }
  })(i)
}

/// Parse a type qualifier spec.
pub fn type_qualifier_spec(i: &str) -> ParserResult<syntax::TypeQualifierSpec> {
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
pub fn fully_specified_type(i: &str) -> ParserResult<syntax::FullySpecifiedType> {
  map(
    pair(opt(type_qualifier), type_specifier),
    |(qualifier, ty)| syntax::FullySpecifiedType { qualifier, ty },
  )(i)
}

/// Parse an array specifier with no size information.
pub fn array_specifier(i: &str) -> ParserResult<syntax::ArraySpecifier> {
  alt((
    value(
      syntax::ArraySpecifier::Unsized,
      delimited(char('['), blank, char(']')),
    ),
    map(
      delimited(
        terminated(char('['), blank),
        cut(cond_expr),
        preceded(blank, cut(char(']'))),
      ),
      |e| syntax::ArraySpecifier::ExplicitlySized(Box::new(e)),
    ),
  ))(i)
}

/// Parse a primary expression.
pub fn primary_expr(i: &str) -> ParserResult<syntax::Expr> {
  alt((
    parens_expr,
    map(double_lit, syntax::Expr::DoubleConst),
    map(float_lit, syntax::Expr::FloatConst),
    map(unsigned_lit, syntax::Expr::UIntConst),
    map(integral_lit, syntax::Expr::IntConst),
    map(bool_lit, syntax::Expr::BoolConst),
    map(identifier, syntax::Expr::Variable),
  ))(i)
}

/// Parse a postfix expression.
pub fn postfix_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, e) = alt((
    function_call_with_identifier,
    function_call_with_expr_ident_or_expr,
  ))(i)?;

  postfix_part(i, e)
}

// Parse the postfix part of a primary expression. This function will just parse until it cannot
// find any more postfix construct.
fn postfix_part(i: &str, e: syntax::Expr) -> ParserResult<syntax::Expr> {
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
pub fn unary_expr(i: &str) -> ParserResult<syntax::Expr> {
  alt((
    map(separated_pair(unary_op, blank, unary_expr), |(op, e)| {
      syntax::Expr::Unary(op, Box::new(e))
    }),
    postfix_expr,
  ))(i)
}

/// Parse an expression between parens.
pub fn parens_expr(i: &str) -> ParserResult<syntax::Expr> {
  delimited(
    terminated(char('('), blank),
    expr,
    preceded(blank, cut(char(')'))),
  )(i)
}

/// Parse a dot field selection identifier.
pub fn dot_field_selection(i: &str) -> ParserResult<syntax::Identifier> {
  preceded(terminated(char('.'), blank), cut(identifier))(i)
}

/// Parse a declaration.
pub fn declaration(i: &str) -> ParserResult<syntax::Declaration> {
  alt((
    map(
      terminated(function_prototype, terminated(blank, char(';'))),
      syntax::Declaration::FunctionPrototype,
    ),
    map(
      terminated(init_declarator_list, terminated(blank, char(';'))),
      syntax::Declaration::InitDeclaratorList,
    ),
    precision_declaration,
    block_declaration,
    global_declaration,
  ))(i)
}

/// Parse a precision declaration.
pub fn precision_declaration(i: &str) -> ParserResult<syntax::Declaration> {
  delimited(
    terminated(keyword("precision"), blank),
    map(
      cut(pair(
        terminated(precision_qualifier, blank),
        terminated(type_specifier, blank),
      )),
      |(qual, ty)| syntax::Declaration::Precision(qual, ty),
    ),
    char(';'),
  )(i)
}

/// Parse a block declaration.
pub fn block_declaration(i: &str) -> ParserResult<syntax::Declaration> {
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
      syntax::Declaration::Block(syntax::Block {
        qualifier,
        name,
        fields,
        identifier,
      })
    },
  )(i)
}

/// Parse a global declaration.
pub fn global_declaration(i: &str) -> ParserResult<syntax::Declaration> {
  map(
    pair(
      terminated(type_qualifier, blank),
      many0(delimited(terminated(char(','), blank), identifier, blank)),
    ),
    |(qual, idents)| syntax::Declaration::Global(qual, idents),
  )(i)
}

/// Parse a function prototype.
pub fn function_prototype(i: &str) -> ParserResult<syntax::FunctionPrototype> {
  terminated(function_declarator, terminated(blank, cut(char(')'))))(i)
}

/// Parse an init declarator list.
pub fn init_declarator_list(i: &str) -> ParserResult<syntax::InitDeclaratorList> {
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
pub fn single_declaration(i: &str) -> ParserResult<syntax::SingleDeclaration> {
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
pub fn initializer(i: &str) -> ParserResult<syntax::Initializer> {
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
pub fn initializer_list(i: &str) -> ParserResult<Vec<syntax::Initializer>> {
  separated_list(delimited(blank, char(','), blank), initializer)(i)
}

fn function_declarator(i: &str) -> ParserResult<syntax::FunctionPrototype> {
  alt((
    function_header_with_parameters,
    map(function_header, |(ty, name)| syntax::FunctionPrototype {
      ty,
      name,
      parameters: Vec::new(),
    }),
  ))(i)
}

fn function_header(i: &str) -> ParserResult<(syntax::FullySpecifiedType, syntax::Identifier)> {
  pair(
    terminated(fully_specified_type, blank),
    terminated(identifier, terminated(blank, char('('))),
  )(i)
}

fn function_header_with_parameters(i: &str) -> ParserResult<syntax::FunctionPrototype> {
  map(
    pair(
      function_header,
      separated_list(
        preceded(blank, char(',')),
        preceded(blank, function_parameter_declaration),
      ),
    ),
    |(header, parameters)| syntax::FunctionPrototype {
      ty: header.0,
      name: header.1,
      parameters,
    },
  )(i)
}

fn function_parameter_declaration(i: &str) -> ParserResult<syntax::FunctionParameterDeclaration> {
  alt((
    function_parameter_declaration_named,
    function_parameter_declaration_unnamed,
  ))(i)
}

fn function_parameter_declaration_named(
  i: &str,
) -> ParserResult<syntax::FunctionParameterDeclaration> {
  map(
    pair(
      opt(terminated(type_qualifier, blank)),
      function_parameter_declarator,
    ),
    |(ty_qual, fpd)| syntax::FunctionParameterDeclaration::Named(ty_qual, fpd),
  )(i)
}

fn function_parameter_declaration_unnamed(
  i: &str,
) -> ParserResult<syntax::FunctionParameterDeclaration> {
  map(
    pair(opt(terminated(type_qualifier, blank)), type_specifier),
    |(ty_qual, ty_spec)| syntax::FunctionParameterDeclaration::Unnamed(ty_qual, ty_spec),
  )(i)
}

fn function_parameter_declarator(i: &str) -> ParserResult<syntax::FunctionParameterDeclarator> {
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

fn function_call_with_identifier(i: &str) -> ParserResult<syntax::Expr> {
  map(
    tuple((function_identifier_identifier, function_call_args)),
    |(fi, args)| syntax::Expr::FunCall(fi, args),
  )(i)
}

fn function_call_with_expr_ident_or_expr(i: &str) -> ParserResult<syntax::Expr> {
  map(
    tuple((function_identifier_expr, opt(function_call_args))),
    |(expr, args)| match args {
      Some(args) => syntax::Expr::FunCall(expr, args),
      None => expr.into_expr().unwrap(),
    },
  )(i)
}

fn function_call_args(i: &str) -> ParserResult<Vec<syntax::Expr>> {
  preceded(
    terminated(terminated(blank, char('(')), blank),
    alt((
      map(
        terminated(blank, terminated(opt(void), terminated(blank, char(')')))),
        |_| vec![],
      ),
      terminated(
        separated_list(
          terminated(char(','), blank),
          cut(terminated(assignment_expr, blank)),
        ),
        cut(char(')')),
      ),
    )),
  )(i)
}

fn function_identifier_identifier(i: &str) -> ParserResult<syntax::FunIdentifier> {
  map(
    terminated(identifier, terminated(blank, peek(char('(')))),
    syntax::FunIdentifier::Identifier,
  )(i)
}

fn function_identifier_expr(i: &str) -> ParserResult<syntax::FunIdentifier> {
  (|i| {
    let (i, e) = primary_expr(i)?;
    postfix_part(i, e).map(|(i, pfe)| (i, syntax::FunIdentifier::Expr(Box::new(pfe))))
  })(i)
}

/// Parse a function identifier just behind a function list argument.
pub fn function_identifier(i: &str) -> ParserResult<syntax::FunIdentifier> {
  alt((function_identifier_identifier, function_identifier_expr))(i)
}

/// Parse the most general expression.
pub fn expr(i: &str) -> ParserResult<syntax::Expr> {
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
pub fn assignment_expr(i: &str) -> ParserResult<syntax::Expr> {
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
pub fn assignment_op(i: &str) -> ParserResult<syntax::AssignmentOp> {
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
pub fn cond_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = logical_or_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      tuple((
        delimited(blank, char('?'), blank),
        cut(terminated(expr, blank)),
        cut(terminated(char(':'), blank)),
        cut(assignment_expr),
      )),
      move |(_, b, _, c)| syntax::Expr::Ternary(Box::new(a_.clone()), Box::new(b), Box::new(c)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a logical OR expression.
pub fn logical_or_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = logical_xor_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(delimited(blank, tag("||"), blank), logical_or_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::Or, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a logical XOR expression.
pub fn logical_xor_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = logical_and_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(delimited(blank, tag("^^"), blank), logical_xor_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::Xor, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a logical AND expression.
pub fn logical_and_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = inclusive_or_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(delimited(blank, tag("&&"), blank), logical_and_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::And, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a bitwise OR expression.
pub fn inclusive_or_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = exclusive_or_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(delimited(blank, char('|'), blank), inclusive_or_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::BitOr, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a bitwise XOR expression.
pub fn exclusive_or_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = and_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(delimited(blank, char('^'), blank), exclusive_or_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::BitXor, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a bitwise AND expression.
pub fn and_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = equality_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      preceded(delimited(blank, char('&'), blank), and_expr),
      move |b| syntax::Expr::Binary(syntax::BinaryOp::BitAnd, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse an equality expression.
pub fn equality_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = rel_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        delimited(
          blank,
          alt((
            value(syntax::BinaryOp::Equal, tag("==")),
            value(syntax::BinaryOp::NonEqual, tag("!=")),
          )),
          blank,
        ),
        equality_expr,
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a relational expression.
pub fn rel_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = shift_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
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
        rel_expr,
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse a shift expression.
pub fn shift_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = additive_expr(i)?;
  let a_ = a.clone();

  alt((
    map(
      pair(
        delimited(
          blank,
          alt((
            value(syntax::BinaryOp::LShift, tag("<<")),
            value(syntax::BinaryOp::RShift, tag(">>")),
          )),
          blank,
        ),
        shift_expr,
      ),
      move |(op, b)| syntax::Expr::Binary(op, Box::new(a_.clone()), Box::new(b)),
    ),
    cnst(a),
  ))(i)
}

/// Parse an additive expression.
pub fn additive_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = multiplicative_expr(i)?;
  let a_ = a.clone();

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
    a_,
    move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc.clone()), Box::new(b)),
  )(i)
}

/// Parse a multiplicative expression.
pub fn multiplicative_expr(i: &str) -> ParserResult<syntax::Expr> {
  let (i, a) = unary_expr(i)?;
  let a_ = a.clone();
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
      a_,
      move |acc, (op, b)| syntax::Expr::Binary(op, Box::new(acc.clone()), Box::new(b)),
  )(i)
}

/// Parse a simple statement.
pub fn simple_statement(i: &str) -> ParserResult<syntax::SimpleStatement> {
  alt((
    map(jump_statement, syntax::SimpleStatement::Jump),
    map(iteration_statement, syntax::SimpleStatement::Iteration),
    map(case_label, syntax::SimpleStatement::CaseLabel),
    map(switch_statement, syntax::SimpleStatement::Switch),
    map(selection_statement, syntax::SimpleStatement::Selection),
    map(declaration, syntax::SimpleStatement::Declaration),
    map(expr_statement, syntax::SimpleStatement::Expression),
  ))(i)
}

/// Parse an expression statement.
pub fn expr_statement(i: &str) -> ParserResult<syntax::ExprStatement> {
  terminated(terminated(opt(expr), blank), char(';'))(i)
}

/// Parse a selection statement.
pub fn selection_statement(i: &str) -> ParserResult<syntax::SelectionStatement> {
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

fn selection_rest_statement(i: &str) -> ParserResult<syntax::SelectionRestStatement> {
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
pub fn switch_statement(i: &str) -> ParserResult<syntax::SwitchStatement> {
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
pub fn case_label(i: &str) -> ParserResult<syntax::CaseLabel> {
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
pub fn iteration_statement(i: &str) -> ParserResult<syntax::IterationStatement> {
  alt((
    iteration_statement_while,
    iteration_statement_do_while,
    iteration_statement_for,
  ))(i)
}

/// Parse a while statement.
pub fn iteration_statement_while(i: &str) -> ParserResult<syntax::IterationStatement> {
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
pub fn iteration_statement_do_while(i: &str) -> ParserResult<syntax::IterationStatement> {
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
pub fn iteration_statement_for(i: &str) -> ParserResult<syntax::IterationStatement> {
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

fn iteration_statement_for_init_statement(i: &str) -> ParserResult<syntax::ForInitStatement> {
  alt((
    map(expr_statement, syntax::ForInitStatement::Expression),
    map(declaration, |d| {
      syntax::ForInitStatement::Declaration(Box::new(d))
    }),
  ))(i)
}

fn iteration_statement_for_rest_statement(i: &str) -> ParserResult<syntax::ForRestStatement> {
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
pub fn jump_statement(i: &str) -> ParserResult<syntax::JumpStatement> {
  alt((
    jump_statement_continue,
    jump_statement_break,
    jump_statement_return,
    jump_statement_discard,
  ))(i)
}

// Parse a continue statement.
pub fn jump_statement_continue(i: &str) -> ParserResult<syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Continue,
    terminated(keyword("continue"), cut(terminated(blank, char(';')))),
  )(i)
}

// Parse a break statement.
pub fn jump_statement_break(i: &str) -> ParserResult<syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Break,
    terminated(keyword("break"), cut(terminated(blank, char(';')))),
  )(i)
}

// Parse a discard statement.
pub fn jump_statement_discard(i: &str) -> ParserResult<syntax::JumpStatement> {
  value(
    syntax::JumpStatement::Discard,
    terminated(keyword("discard"), cut(terminated(blank, char(';')))),
  )(i)
}

// Parse a return statement.
pub fn jump_statement_return(i: &str) -> ParserResult<syntax::JumpStatement> {
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
pub fn condition(i: &str) -> ParserResult<syntax::Condition> {
  alt((
    map(expr, |e| syntax::Condition::Expr(Box::new(e))),
    condition_assignment,
  ))(i)
}

fn condition_assignment(i: &str) -> ParserResult<syntax::Condition> {
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
pub fn statement(i: &str) -> ParserResult<syntax::Statement> {
  alt((
    map(compound_statement, |c| {
      syntax::Statement::Compound(Box::new(c))
    }),
    map(simple_statement, |s| syntax::Statement::Simple(Box::new(s))),
  ))(i)
}

/// Parse a compound statement.
pub fn compound_statement(i: &str) -> ParserResult<syntax::CompoundStatement> {
  map(
    delimited(
      terminated(char('{'), blank),
      many0(terminated(statement, blank)),
      cut(char('}')),
    ),
    |statement_list| syntax::CompoundStatement { statement_list },
  )(i)
}

/// Parse a function definition.
pub fn function_definition(i: &str) -> ParserResult<syntax::FunctionDefinition> {
  map(
    pair(terminated(function_prototype, blank), compound_statement),
    |(prototype, statement)| syntax::FunctionDefinition {
      prototype,
      statement,
    },
  )(i)
}

/// Parse an external declaration.
pub fn external_declaration(i: &str) -> ParserResult<syntax::ExternalDeclaration> {
  alt((
    map(preprocessor, syntax::ExternalDeclaration::Preprocessor),
    map(
      function_definition,
      syntax::ExternalDeclaration::FunctionDefinition,
    ),
    map(declaration, syntax::ExternalDeclaration::Declaration),
    preceded(
      delimited(blank, char(';'), blank),
      cut(external_declaration),
    ),
  ))(i)
}

/// Parse a translation unit (entry point).
pub fn translation_unit(i: &str) -> ParserResult<syntax::TranslationUnit> {
  map(
    many1(delimited(blank, external_declaration, blank)),
    |eds| syntax::TranslationUnit(syntax::NonEmpty(eds)),
  )(i)
}

/// Parse a preprocessor directive.
pub fn preprocessor(i: &str) -> ParserResult<syntax::Preprocessor> {
  preceded(
    terminated(char('#'), pp_space0),
    cut(alt((
      map(pp_define, syntax::Preprocessor::Define),
      value(syntax::Preprocessor::Else, pp_else),
      map(pp_elseif, syntax::Preprocessor::ElseIf),
      value(syntax::Preprocessor::EndIf, pp_endif),
      map(pp_error, syntax::Preprocessor::Error),
      map(pp_if, syntax::Preprocessor::If),
      map(pp_ifdef, syntax::Preprocessor::IfDef),
      map(pp_ifndef, syntax::Preprocessor::IfNDef),
      map(pp_include, syntax::Preprocessor::Include),
      map(pp_line, syntax::Preprocessor::Line),
      map(pp_pragma, syntax::Preprocessor::Pragma),
      map(pp_undef, syntax::Preprocessor::Undef),
      map(pp_version, syntax::Preprocessor::Version),
      map(pp_extension, syntax::Preprocessor::Extension),
    ))),
  )(i)
}

/// Parse a preprocessor version number.
pub(crate) fn pp_version_number(i: &str) -> ParserResult<u16> {
  map(digit1, |x: &str| x.parse_to().unwrap())(i)
}

/// Parse a preprocessor version profile.
pub(crate) fn pp_version_profile(i: &str) -> ParserResult<syntax::PreprocessorVersionProfile> {
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
pub(crate) fn pp_space0(i: &str) -> ParserResult<&str> {
  recognize(many0_(alt((space1, tag("\\\n")))))(i)
}

/// Parse a preprocessor define.
pub(crate) fn pp_define(i: &str) -> ParserResult<syntax::PreprocessorDefine> {
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
pub(crate) fn pp_define_object_like<'a>(
  ident: syntax::Identifier,
) -> impl Fn(&'a str) -> ParserResult<'a, syntax::PreprocessorDefine> {
  move |i| {
    map(preceded(pp_space0, cut(str_till_eol)), |value| {
      syntax::PreprocessorDefine::ObjectLike {
        ident: ident.clone(),
        value: value.to_owned(),
      }
    })(i)
  }
}

// Parse a function-like #define content.
pub(crate) fn pp_define_function_like<'a>(
  ident: syntax::Identifier,
) -> impl Fn(&'a str) -> ParserResult<'a, syntax::PreprocessorDefine> {
  move |i| {
    map(
      tuple((
        terminated(char('('), pp_space0),
        separated_list(terminated(char(','), pp_space0), cut(identifier)),
        cut(terminated(char(')'), pp_space0)),
        cut(map(str_till_eol, String::from)),
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
pub(crate) fn pp_else(i: &str) -> ParserResult<syntax::Preprocessor> {
  value(
    syntax::Preprocessor::Else,
    tuple((terminated(keyword("else"), pp_space0), cut(eol))),
  )(i)
}

/// Parse a preprocessor elseif.
pub(crate) fn pp_elseif(i: &str) -> ParserResult<syntax::PreprocessorElseIf> {
  map(
    tuple((
      terminated(keyword("elseif"), pp_space0),
      cut(map(str_till_eol, String::from)),
    )),
    |(_, condition)| syntax::PreprocessorElseIf { condition },
  )(i)
}

/// Parse a preprocessor endif.
pub(crate) fn pp_endif(i: &str) -> ParserResult<syntax::Preprocessor> {
  map(
    tuple((terminated(keyword("endif"), space0), cut(eol))),
    |(_, _)| syntax::Preprocessor::EndIf,
  )(i)
}

/// Parse a preprocessor error.
pub(crate) fn pp_error(i: &str) -> ParserResult<syntax::PreprocessorError> {
  map(
    tuple((terminated(keyword("error"), pp_space0), cut(str_till_eol))),
    |(_, message)| syntax::PreprocessorError {
      message: message.to_owned(),
    },
  )(i)
}

/// Parse a preprocessor if.
pub(crate) fn pp_if(i: &str) -> ParserResult<syntax::PreprocessorIf> {
  map(
    tuple((
      terminated(keyword("if"), pp_space0),
      cut(map(str_till_eol, String::from)),
    )),
    |(_, condition)| syntax::PreprocessorIf { condition },
  )(i)
}

/// Parse a preprocessor ifdef.
pub(crate) fn pp_ifdef(i: &str) -> ParserResult<syntax::PreprocessorIfDef> {
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
pub(crate) fn pp_ifndef(i: &str) -> ParserResult<syntax::PreprocessorIfNDef> {
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
pub(crate) fn pp_include(i: &str) -> ParserResult<syntax::PreprocessorInclude> {
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
pub(crate) fn pp_line(i: &str) -> ParserResult<syntax::PreprocessorLine> {
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
pub(crate) fn pp_pragma(i: &str) -> ParserResult<syntax::PreprocessorPragma> {
  map(
    tuple((terminated(keyword("pragma"), pp_space0), cut(str_till_eol))),
    |(_, command)| syntax::PreprocessorPragma {
      command: command.to_owned(),
    },
  )(i)
}

/// Parse a preprocessor undef.
pub(crate) fn pp_undef(i: &str) -> ParserResult<syntax::PreprocessorUndef> {
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
pub(crate) fn pp_version(i: &str) -> ParserResult<syntax::PreprocessorVersion> {
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
pub(crate) fn pp_extension_name(i: &str) -> ParserResult<syntax::PreprocessorExtensionName> {
  alt((
    value(syntax::PreprocessorExtensionName::All, keyword("all")),
    map(string, syntax::PreprocessorExtensionName::Specific),
  ))(i)
}

/// Parse a preprocessor extension behavior.
pub(crate) fn pp_extension_behavior(
  i: &str,
) -> ParserResult<syntax::PreprocessorExtensionBehavior> {
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
pub(crate) fn pp_extension(i: &str) -> ParserResult<syntax::PreprocessorExtension> {
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_uniline_comment() {
    assert_eq!(comment("// lol"), Ok(("", " lol")));
    assert_eq!(comment("// lol\nfoo"), Ok(("foo", " lol")));
    assert_eq!(comment("// lol\\\nfoo"), Ok(("", " lol\\\nfoo")));
    assert_eq!(
      comment("// lol   \\\n   foo\n"),
      Ok(("", " lol   \\\n   foo"))
    );
  }

  #[test]
  fn parse_multiline_comment() {
    assert_eq!(comment("/* lol\nfoo\n*/bar"), Ok(("bar", " lol\nfoo\n")));
  }

  #[test]
  fn parse_unsigned_suffix() {
    assert_eq!(unsigned_suffix("u"), Ok(("", 'u')));
    assert_eq!(unsigned_suffix("U"), Ok(("", 'U')));
  }

  #[test]
  fn parse_nonzero_digits() {
    assert_eq!(nonzero_digits("3"), Ok(("", "3")));
    assert_eq!(nonzero_digits("12345953"), Ok(("", "12345953")));
  }

  #[test]
  fn parse_decimal_lit() {
    assert_eq!(decimal_lit("3"), Ok(("", Ok(3))));
    assert_eq!(decimal_lit("3"), Ok(("", Ok(3))));
    assert_eq!(decimal_lit("13"), Ok(("", Ok(13))));
    assert_eq!(decimal_lit("42"), Ok(("", Ok(42))));
    assert_eq!(decimal_lit("123456"), Ok(("", Ok(123456))));
  }

  #[test]
  fn parse_octal_lit() {
    assert_eq!(octal_lit("0"), Ok(("", Ok(0o0))));
    assert_eq!(octal_lit("03 "), Ok((" ", Ok(0o3))));
    assert_eq!(octal_lit("012 "), Ok((" ", Ok(0o12))));
    assert_eq!(octal_lit("07654321 "), Ok((" ", Ok(0o7654321))));
  }

  #[test]
  fn parse_hexadecimal_lit() {
    assert_eq!(hexadecimal_lit("0x3 "), Ok((" ", Ok(0x3))));
    assert_eq!(hexadecimal_lit("0x0123789"), Ok(("", Ok(0x0123789))));
    assert_eq!(hexadecimal_lit("0xABCDEF"), Ok(("", Ok(0xabcdef))));
    assert_eq!(hexadecimal_lit("0xabcdef"), Ok(("", Ok(0xabcdef))));
  }

  #[test]
  fn parse_integral_lit() {
    assert_eq!(integral_lit("0"), Ok(("", 0)));
    assert_eq!(integral_lit("3"), Ok(("", 3)));
    assert_eq!(integral_lit("3 "), Ok((" ", 3)));
    assert_eq!(integral_lit("03 "), Ok((" ", 3)));
    assert_eq!(integral_lit("076556 "), Ok((" ", 0o76556)));
    assert_eq!(integral_lit("012 "), Ok((" ", 0o12)));
    assert_eq!(integral_lit("0x3 "), Ok((" ", 0x3)));
    assert_eq!(integral_lit("0x9ABCDEF"), Ok(("", 0x9ABCDEF)));
    assert_eq!(integral_lit("0x9ABCDEF"), Ok(("", 0x9ABCDEF)));
    assert_eq!(integral_lit("0x9abcdef"), Ok(("", 0x9abcdef)));
    assert_eq!(integral_lit("0x9abcdef"), Ok(("", 0x9abcdef)));
    assert_eq!(integral_lit("0xffffffff"), Ok(("", 0xffffffffu32 as i32)));
  }

  #[test]
  fn parse_integral_neg_lit() {
    assert_eq!(integral_lit("-3"), Ok(("", -3)));
    assert_eq!(integral_lit("-3 "), Ok((" ", -3)));
    assert_eq!(integral_lit("-03 "), Ok((" ", -3)));
    assert_eq!(integral_lit("-076556 "), Ok((" ", -0o76556)));
    assert_eq!(integral_lit("-012 "), Ok((" ", -0o12)));
    assert_eq!(integral_lit("-0x3 "), Ok((" ", -0x3)));
    assert_eq!(integral_lit("-0x9ABCDEF"), Ok(("", -0x9ABCDEF)));
    assert_eq!(integral_lit("-0x9ABCDEF"), Ok(("", -0x9ABCDEF)));
    assert_eq!(integral_lit("-0x9abcdef"), Ok(("", -0x9abcdef)));
    assert_eq!(integral_lit("-0x9abcdef"), Ok(("", -0x9abcdef)));
  }

  #[test]
  fn parse_unsigned_lit() {
    assert_eq!(unsigned_lit("0xffffffffU"), Ok(("", 0xffffffff as u32)));
    assert_eq!(unsigned_lit("-1u"), Ok(("", 0xffffffff as u32)));
    assert!(unsigned_lit("0xfffffffffU").is_err());
  }

  #[test]
  fn parse_float_lit() {
    assert_eq!(float_lit("0.;"), Ok((";", 0.)));
    assert_eq!(float_lit(".0;"), Ok((";", 0.)));
    assert_eq!(float_lit(".035 "), Ok((" ", 0.035)));
    assert_eq!(float_lit("0. "), Ok((" ", 0.)));
    assert_eq!(float_lit("0.035 "), Ok((" ", 0.035)));
    assert_eq!(float_lit(".035f"), Ok(("", 0.035)));
    assert_eq!(float_lit("0.f"), Ok(("", 0.)));
    assert_eq!(float_lit("314.f"), Ok(("", 314.)));
    assert_eq!(float_lit("0.035f"), Ok(("", 0.035)));
    assert_eq!(float_lit(".035F"), Ok(("", 0.035)));
    assert_eq!(float_lit("0.F"), Ok(("", 0.)));
    assert_eq!(float_lit("0.035F"), Ok(("", 0.035)));
    assert_eq!(float_lit("1.03e+34 "), Ok((" ", 1.03e+34)));
    assert_eq!(float_lit("1.03E+34 "), Ok((" ", 1.03E+34)));
    assert_eq!(float_lit("1.03e-34 "), Ok((" ", 1.03e-34)));
    assert_eq!(float_lit("1.03E-34 "), Ok((" ", 1.03E-34)));
    assert_eq!(float_lit("1.03e+34f"), Ok(("", 1.03e+34)));
    assert_eq!(float_lit("1.03E+34f"), Ok(("", 1.03E+34)));
    assert_eq!(float_lit("1.03e-34f"), Ok(("", 1.03e-34)));
    assert_eq!(float_lit("1.03E-34f"), Ok(("", 1.03E-34)));
    assert_eq!(float_lit("1.03e+34F"), Ok(("", 1.03e+34)));
    assert_eq!(float_lit("1.03E+34F"), Ok(("", 1.03E+34)));
    assert_eq!(float_lit("1.03e-34F"), Ok(("", 1.03e-34)));
    assert_eq!(float_lit("1.03E-34F"), Ok(("", 1.03E-34)));
  }

  #[test]
  fn parse_float_neg_lit() {
    assert_eq!(float_lit("-.035 "), Ok((" ", -0.035)));
    assert_eq!(float_lit("-0. "), Ok((" ", -0.)));
    assert_eq!(float_lit("-0.035 "), Ok((" ", -0.035)));
    assert_eq!(float_lit("-.035f"), Ok(("", -0.035)));
    assert_eq!(float_lit("-0.f"), Ok(("", -0.)));
    assert_eq!(float_lit("-0.035f"), Ok(("", -0.035)));
    assert_eq!(float_lit("-.035F"), Ok(("", -0.035)));
    assert_eq!(float_lit("-0.F"), Ok(("", -0.)));
    assert_eq!(float_lit("-0.035F"), Ok(("", -0.035)));
    assert_eq!(float_lit("-1.03e+34 "), Ok((" ", -1.03e+34)));
    assert_eq!(float_lit("-1.03E+34 "), Ok((" ", -1.03E+34)));
    assert_eq!(float_lit("-1.03e-34 "), Ok((" ", -1.03e-34)));
    assert_eq!(float_lit("-1.03E-34 "), Ok((" ", -1.03E-34)));
    assert_eq!(float_lit("-1.03e+34f"), Ok(("", -1.03e+34)));
    assert_eq!(float_lit("-1.03E+34f"), Ok(("", -1.03E+34)));
    assert_eq!(float_lit("-1.03e-34f"), Ok(("", -1.03e-34)));
    assert_eq!(float_lit("-1.03E-34f"), Ok(("", -1.03E-34)));
    assert_eq!(float_lit("-1.03e+34F"), Ok(("", -1.03e+34)));
    assert_eq!(float_lit("-1.03E+34F"), Ok(("", -1.03E+34)));
    assert_eq!(float_lit("-1.03e-34F"), Ok(("", -1.03e-34)));
    assert_eq!(float_lit("-1.03E-34F"), Ok(("", -1.03E-34)));
  }

  #[test]
  fn parse_double_lit() {
    assert_eq!(double_lit("0.;"), Ok((";", 0.)));
    assert_eq!(double_lit(".0;"), Ok((";", 0.)));
    assert_eq!(double_lit(".035 "), Ok((" ", 0.035)));
    assert_eq!(double_lit("0. "), Ok((" ", 0.)));
    assert_eq!(double_lit("0.035 "), Ok((" ", 0.035)));
    assert_eq!(double_lit("0.lf"), Ok(("", 0.)));
    assert_eq!(double_lit("0.035lf"), Ok(("", 0.035)));
    assert_eq!(double_lit(".035lf"), Ok(("", 0.035)));
    assert_eq!(double_lit(".035LF"), Ok(("", 0.035)));
    assert_eq!(double_lit("0.LF"), Ok(("", 0.)));
    assert_eq!(double_lit("0.035LF"), Ok(("", 0.035)));
    assert_eq!(double_lit("1.03e+34lf"), Ok(("", 1.03e+34)));
    assert_eq!(double_lit("1.03E+34lf"), Ok(("", 1.03E+34)));
    assert_eq!(double_lit("1.03e-34lf"), Ok(("", 1.03e-34)));
    assert_eq!(double_lit("1.03E-34lf"), Ok(("", 1.03E-34)));
    assert_eq!(double_lit("1.03e+34LF"), Ok(("", 1.03e+34)));
    assert_eq!(double_lit("1.03E+34LF"), Ok(("", 1.03E+34)));
    assert_eq!(double_lit("1.03e-34LF"), Ok(("", 1.03e-34)));
    assert_eq!(double_lit("1.03E-34LF"), Ok(("", 1.03E-34)));
  }

  #[test]
  fn parse_double_neg_lit() {
    assert_eq!(double_lit("-0.;"), Ok((";", -0.)));
    assert_eq!(double_lit("-.0;"), Ok((";", -0.)));
    assert_eq!(double_lit("-.035 "), Ok((" ", -0.035)));
    assert_eq!(double_lit("-0. "), Ok((" ", -0.)));
    assert_eq!(double_lit("-0.035 "), Ok((" ", -0.035)));
    assert_eq!(double_lit("-0.lf"), Ok(("", -0.)));
    assert_eq!(double_lit("-0.035lf"), Ok(("", -0.035)));
    assert_eq!(double_lit("-.035lf"), Ok(("", -0.035)));
    assert_eq!(double_lit("-.035LF"), Ok(("", -0.035)));
    assert_eq!(double_lit("-0.LF"), Ok(("", -0.)));
    assert_eq!(double_lit("-0.035LF"), Ok(("", -0.035)));
    assert_eq!(double_lit("-1.03e+34lf"), Ok(("", -1.03e+34)));
    assert_eq!(double_lit("-1.03E+34lf"), Ok(("", -1.03E+34)));
    assert_eq!(double_lit("-1.03e-34lf"), Ok(("", -1.03e-34)));
    assert_eq!(double_lit("-1.03E-34lf"), Ok(("", -1.03E-34)));
    assert_eq!(double_lit("-1.03e+34LF"), Ok(("", -1.03e+34)));
    assert_eq!(double_lit("-1.03E+34LF"), Ok(("", -1.03E+34)));
    assert_eq!(double_lit("-1.03e-34LF"), Ok(("", -1.03e-34)));
    assert_eq!(double_lit("-1.03E-34LF"), Ok(("", -1.03E-34)));
  }

  #[test]
  fn parse_bool_lit() {
    assert_eq!(bool_lit("false"), Ok(("", false)));
    assert_eq!(bool_lit("true"), Ok(("", true)));
  }

  #[test]
  fn parse_identifier() {
    assert_eq!(identifier("a"), Ok(("", "a".into())));
    assert_eq!(identifier("ab_cd"), Ok(("", "ab_cd".into())));
    assert_eq!(identifier("Ab_cd"), Ok(("", "Ab_cd".into())));
    assert_eq!(identifier("Ab_c8d"), Ok(("", "Ab_c8d".into())));
    assert_eq!(identifier("Ab_c8d9"), Ok(("", "Ab_c8d9".into())));
  }

  #[test]
  fn parse_unary_op_add() {
    assert_eq!(unary_op("+ "), Ok((" ", syntax::UnaryOp::Add)));
  }

  #[test]
  fn parse_unary_op_minus() {
    assert_eq!(unary_op("- "), Ok((" ", syntax::UnaryOp::Minus)));
  }

  #[test]
  fn parse_unary_op_not() {
    assert_eq!(unary_op("!"), Ok(("", syntax::UnaryOp::Not)));
  }

  #[test]
  fn parse_unary_op_complement() {
    assert_eq!(unary_op("~"), Ok(("", syntax::UnaryOp::Complement)));
  }

  #[test]
  fn parse_unary_op_inc() {
    assert_eq!(unary_op("++"), Ok(("", syntax::UnaryOp::Inc)));
  }

  #[test]
  fn parse_unary_op_dec() {
    assert_eq!(unary_op("--"), Ok(("", syntax::UnaryOp::Dec)));
  }

  #[test]
  fn parse_array_specifier_unsized() {
    assert_eq!(
      array_specifier("[]"),
      Ok(("", syntax::ArraySpecifier::Unsized))
    );
    assert_eq!(
      array_specifier("[ ]"),
      Ok(("", syntax::ArraySpecifier::Unsized))
    );
    assert_eq!(
      array_specifier("[\n]"),
      Ok(("", syntax::ArraySpecifier::Unsized))
    );
  }

  #[test]
  fn parse_array_specifier_sized() {
    let ix = syntax::Expr::IntConst(0);

    assert_eq!(
      array_specifier("[0]"),
      Ok((
        "",
        syntax::ArraySpecifier::ExplicitlySized(Box::new(ix.clone()))
      ))
    );
    assert_eq!(
      array_specifier("[\n0   \t]"),
      Ok(("", syntax::ArraySpecifier::ExplicitlySized(Box::new(ix))))
    );
  }

  #[test]
  fn parse_precise_qualifier() {
    assert_eq!(precise_qualifier("precise "), Ok((" ", ())));
  }

  #[test]
  fn parse_invariant_qualifier() {
    assert_eq!(invariant_qualifier("invariant "), Ok((" ", ())));
  }

  #[test]
  fn parse_interpolation_qualifier() {
    assert_eq!(
      interpolation_qualifier("smooth "),
      Ok((" ", syntax::InterpolationQualifier::Smooth))
    );
    assert_eq!(
      interpolation_qualifier("flat "),
      Ok((" ", syntax::InterpolationQualifier::Flat))
    );
    assert_eq!(
      interpolation_qualifier("noperspective "),
      Ok((" ", syntax::InterpolationQualifier::NoPerspective))
    );
  }

  #[test]
  fn parse_precision_qualifier() {
    assert_eq!(
      precision_qualifier("highp "),
      Ok((" ", syntax::PrecisionQualifier::High))
    );
    assert_eq!(
      precision_qualifier("mediump "),
      Ok((" ", syntax::PrecisionQualifier::Medium))
    );
    assert_eq!(
      precision_qualifier("lowp "),
      Ok((" ", syntax::PrecisionQualifier::Low))
    );
  }

  #[test]
  fn parse_storage_qualifier() {
    assert_eq!(
      storage_qualifier("const "),
      Ok((" ", syntax::StorageQualifier::Const))
    );
    assert_eq!(
      storage_qualifier("inout "),
      Ok((" ", syntax::StorageQualifier::InOut))
    );
    assert_eq!(
      storage_qualifier("in "),
      Ok((" ", syntax::StorageQualifier::In))
    );
    assert_eq!(
      storage_qualifier("out "),
      Ok((" ", syntax::StorageQualifier::Out))
    );
    assert_eq!(
      storage_qualifier("centroid "),
      Ok((" ", syntax::StorageQualifier::Centroid))
    );
    assert_eq!(
      storage_qualifier("patch "),
      Ok((" ", syntax::StorageQualifier::Patch))
    );
    assert_eq!(
      storage_qualifier("sample "),
      Ok((" ", syntax::StorageQualifier::Sample))
    );
    assert_eq!(
      storage_qualifier("uniform "),
      Ok((" ", syntax::StorageQualifier::Uniform))
    );
    assert_eq!(
      storage_qualifier("attribute "),
      Ok((" ", syntax::StorageQualifier::Attribute))
    );
    assert_eq!(
      storage_qualifier("varying "),
      Ok((" ", syntax::StorageQualifier::Varying))
    );
    assert_eq!(
      storage_qualifier("buffer "),
      Ok((" ", syntax::StorageQualifier::Buffer))
    );
    assert_eq!(
      storage_qualifier("shared "),
      Ok((" ", syntax::StorageQualifier::Shared))
    );
    assert_eq!(
      storage_qualifier("coherent "),
      Ok((" ", syntax::StorageQualifier::Coherent))
    );
    assert_eq!(
      storage_qualifier("volatile "),
      Ok((" ", syntax::StorageQualifier::Volatile))
    );
    assert_eq!(
      storage_qualifier("restrict "),
      Ok((" ", syntax::StorageQualifier::Restrict))
    );
    assert_eq!(
      storage_qualifier("readonly "),
      Ok((" ", syntax::StorageQualifier::ReadOnly))
    );
    assert_eq!(
      storage_qualifier("writeonly "),
      Ok((" ", syntax::StorageQualifier::WriteOnly))
    );
    assert_eq!(
      storage_qualifier("subroutine a"),
      Ok((" a", syntax::StorageQualifier::Subroutine(vec![])))
    );

    let a = syntax::TypeName("vec3".to_owned());
    let b = syntax::TypeName("float".to_owned());
    let c = syntax::TypeName("dmat43".to_owned());
    let types = vec![a, b, c];
    assert_eq!(
      storage_qualifier("subroutine (  vec3 , float \\\n, dmat43)"),
      Ok(("", syntax::StorageQualifier::Subroutine(types)))
    );
  }

  #[test]
  fn parse_layout_qualifier_std430() {
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Identifier(
        "std430".into(),
        None,
      )]),
    };

    assert_eq!(
      layout_qualifier("layout (std430)"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      layout_qualifier("layout  (std430   )"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      layout_qualifier("layout \n\t (  std430  )"),
      Ok(("", expected.clone()))
    );
    assert_eq!(layout_qualifier("layout(std430)"), Ok(("", expected)));
  }

  #[test]
  fn parse_layout_qualifier_shared() {
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Shared]),
    };

    assert_eq!(
      layout_qualifier("layout (shared)"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      layout_qualifier("layout ( shared )"),
      Ok(("", expected.clone()))
    );
    assert_eq!(layout_qualifier("layout(shared)"), Ok(("", expected)));
  }

  #[test]
  fn parse_layout_qualifier_list() {
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier(
      "max_vertices".into(),
      Some(Box::new(syntax::Expr::IntConst(3))),
    );
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![id_0, id_1, id_2]),
    };

    assert_eq!(
      layout_qualifier("layout (shared, std140, max_vertices = 3)"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      layout_qualifier("layout(shared,std140,max_vertices=3)"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      layout_qualifier("layout\n\n\t (    shared , std140, max_vertices= 3)"),
      Ok(("", expected.clone()))
    );
  }

  #[test]
  fn parse_type_qualifier() {
    let storage_qual = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Const);
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier(
      "max_vertices".into(),
      Some(Box::new(syntax::Expr::IntConst(3))),
    );
    let layout_qual = syntax::TypeQualifierSpec::Layout(syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![id_0, id_1, id_2]),
    });
    let expected = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![storage_qual, layout_qual]),
    };

    assert_eq!(
      type_qualifier("const layout (shared, std140, max_vertices = 3)"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      type_qualifier("const layout(shared,std140,max_vertices=3)"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_struct_field_specifier() {
    let expected = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["foo".into()]),
    };

    assert_eq!(
      struct_field_specifier("vec4 foo;"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      struct_field_specifier("vec4     foo ; "),
      Ok((" ", expected.clone()))
    );
  }

  #[test]
  fn parse_struct_field_specifier_type_name() {
    let expected = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("S0238_3".into()),
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["x".into()]),
    };

    assert_eq!(
      struct_field_specifier("S0238_3 x;"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      struct_field_specifier("S0238_3     x ;"),
      Ok(("", expected.clone()))
    );
  }

  #[test]
  fn parse_struct_field_specifier_several() {
    let expected = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["foo".into(), "bar".into(), "zoo".into()]),
    };

    assert_eq!(
      struct_field_specifier("vec4 foo, bar, zoo;"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      struct_field_specifier("vec4     foo , bar  , zoo ;"),
      Ok(("", expected.clone()))
    );
  }

  #[test]
  fn parse_struct_specifier_one_field() {
    let field = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["foo".into()]),
    };
    let expected = syntax::StructSpecifier {
      name: Some("TestStruct".into()),
      fields: syntax::NonEmpty(vec![field]),
    };

    assert_eq!(
      struct_specifier("struct TestStruct { vec4 foo; }"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      struct_specifier("struct      TestStruct \n \n\n {\n    vec4   foo  ;}"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_struct_specifier_multi_fields() {
    let a = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec4,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["foo".into()]),
    };
    let b = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["bar".into()]),
    };
    let c = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::UInt,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["zoo".into()]),
    };
    let d = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::BVec3,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["foo_BAR_zoo3497_34".into()]),
    };
    let e = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("S0238_3".into()),
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["x".into()]),
    };
    let expected = syntax::StructSpecifier {
      name: Some("_TestStruct_934i".into()),
      fields: syntax::NonEmpty(vec![a, b, c, d, e]),
    };

    assert_eq!(
      struct_specifier(
        "struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }"
      ),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      struct_specifier(
        "struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}"
      ),
      Ok(("", expected.clone()))
    );
    assert_eq!(struct_specifier("struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}"), Ok(("", expected)));
  }

  #[test]
  fn parse_type_specifier_non_array() {
    assert_eq!(
      type_specifier_non_array("bool"),
      Ok(("", syntax::TypeSpecifierNonArray::Bool))
    );
    assert_eq!(
      type_specifier_non_array("int"),
      Ok(("", syntax::TypeSpecifierNonArray::Int))
    );
    assert_eq!(
      type_specifier_non_array("uint"),
      Ok(("", syntax::TypeSpecifierNonArray::UInt))
    );
    assert_eq!(
      type_specifier_non_array("float"),
      Ok(("", syntax::TypeSpecifierNonArray::Float))
    );
    assert_eq!(
      type_specifier_non_array("double"),
      Ok(("", syntax::TypeSpecifierNonArray::Double))
    );
    assert_eq!(
      type_specifier_non_array("vec2"),
      Ok(("", syntax::TypeSpecifierNonArray::Vec2))
    );
    assert_eq!(
      type_specifier_non_array("vec3"),
      Ok(("", syntax::TypeSpecifierNonArray::Vec3))
    );
    assert_eq!(
      type_specifier_non_array("vec4"),
      Ok(("", syntax::TypeSpecifierNonArray::Vec4))
    );
    assert_eq!(
      type_specifier_non_array("dvec2"),
      Ok(("", syntax::TypeSpecifierNonArray::DVec2))
    );
    assert_eq!(
      type_specifier_non_array("dvec3"),
      Ok(("", syntax::TypeSpecifierNonArray::DVec3))
    );
    assert_eq!(
      type_specifier_non_array("dvec4"),
      Ok(("", syntax::TypeSpecifierNonArray::DVec4))
    );
    assert_eq!(
      type_specifier_non_array("bvec2"),
      Ok(("", syntax::TypeSpecifierNonArray::BVec2))
    );
    assert_eq!(
      type_specifier_non_array("bvec3"),
      Ok(("", syntax::TypeSpecifierNonArray::BVec3))
    );
    assert_eq!(
      type_specifier_non_array("bvec4"),
      Ok(("", syntax::TypeSpecifierNonArray::BVec4))
    );
    assert_eq!(
      type_specifier_non_array("ivec2"),
      Ok(("", syntax::TypeSpecifierNonArray::IVec2))
    );
    assert_eq!(
      type_specifier_non_array("ivec3"),
      Ok(("", syntax::TypeSpecifierNonArray::IVec3))
    );
    assert_eq!(
      type_specifier_non_array("ivec4"),
      Ok(("", syntax::TypeSpecifierNonArray::IVec4))
    );
    assert_eq!(
      type_specifier_non_array("uvec2"),
      Ok(("", syntax::TypeSpecifierNonArray::UVec2))
    );
    assert_eq!(
      type_specifier_non_array("uvec3"),
      Ok(("", syntax::TypeSpecifierNonArray::UVec3))
    );
    assert_eq!(
      type_specifier_non_array("uvec4"),
      Ok(("", syntax::TypeSpecifierNonArray::UVec4))
    );
    assert_eq!(
      type_specifier_non_array("mat2"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat2))
    );
    assert_eq!(
      type_specifier_non_array("mat3"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat3))
    );
    assert_eq!(
      type_specifier_non_array("mat4"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat4))
    );
    assert_eq!(
      type_specifier_non_array("mat2x2"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat2))
    );
    assert_eq!(
      type_specifier_non_array("mat2x3"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat23))
    );
    assert_eq!(
      type_specifier_non_array("mat2x4"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat24))
    );
    assert_eq!(
      type_specifier_non_array("mat3x2"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat32))
    );
    assert_eq!(
      type_specifier_non_array("mat3x3"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat3))
    );
    assert_eq!(
      type_specifier_non_array("mat3x4"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat34))
    );
    assert_eq!(
      type_specifier_non_array("mat4x2"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat42))
    );
    assert_eq!(
      type_specifier_non_array("mat4x3"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat43))
    );
    assert_eq!(
      type_specifier_non_array("mat4x4"),
      Ok(("", syntax::TypeSpecifierNonArray::Mat4))
    );
    assert_eq!(
      type_specifier_non_array("dmat2"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat2))
    );
    assert_eq!(
      type_specifier_non_array("dmat3"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat3))
    );
    assert_eq!(
      type_specifier_non_array("dmat4"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat4))
    );
    assert_eq!(
      type_specifier_non_array("dmat2x2"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat2))
    );
    assert_eq!(
      type_specifier_non_array("dmat2x3"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat23))
    );
    assert_eq!(
      type_specifier_non_array("dmat2x4"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat24))
    );
    assert_eq!(
      type_specifier_non_array("dmat3x2"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat32))
    );
    assert_eq!(
      type_specifier_non_array("dmat3x3"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat3))
    );
    assert_eq!(
      type_specifier_non_array("dmat3x4"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat34))
    );
    assert_eq!(
      type_specifier_non_array("dmat4x2"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat42))
    );
    assert_eq!(
      type_specifier_non_array("dmat4x3"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat43))
    );
    assert_eq!(
      type_specifier_non_array("dmat4x4"),
      Ok(("", syntax::TypeSpecifierNonArray::DMat4))
    );
    assert_eq!(
      type_specifier_non_array("sampler1D"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler1D))
    );
    assert_eq!(
      type_specifier_non_array("image1D"),
      Ok(("", syntax::TypeSpecifierNonArray::Image1D))
    );
    assert_eq!(
      type_specifier_non_array("sampler2D"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2D))
    );
    assert_eq!(
      type_specifier_non_array("image2D"),
      Ok(("", syntax::TypeSpecifierNonArray::Image2D))
    );
    assert_eq!(
      type_specifier_non_array("sampler3D"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler3D))
    );
    assert_eq!(
      type_specifier_non_array("image3D"),
      Ok(("", syntax::TypeSpecifierNonArray::Image3D))
    );
    assert_eq!(
      type_specifier_non_array("samplerCube"),
      Ok(("", syntax::TypeSpecifierNonArray::SamplerCube))
    );
    assert_eq!(
      type_specifier_non_array("imageCube"),
      Ok(("", syntax::TypeSpecifierNonArray::ImageCube))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DRect"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DRect))
    );
    assert_eq!(
      type_specifier_non_array("image2DRect"),
      Ok(("", syntax::TypeSpecifierNonArray::Image2DRect))
    );
    assert_eq!(
      type_specifier_non_array("sampler1DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler1DArray))
    );
    assert_eq!(
      type_specifier_non_array("image1DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::Image1DArray))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DArray))
    );
    assert_eq!(
      type_specifier_non_array("image2DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::Image2DArray))
    );
    assert_eq!(
      type_specifier_non_array("samplerBuffer"),
      Ok(("", syntax::TypeSpecifierNonArray::SamplerBuffer))
    );
    assert_eq!(
      type_specifier_non_array("imageBuffer"),
      Ok(("", syntax::TypeSpecifierNonArray::ImageBuffer))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DMS"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DMS))
    );
    assert_eq!(
      type_specifier_non_array("image2DMS"),
      Ok(("", syntax::TypeSpecifierNonArray::Image2DMS))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DMSArray"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DMSArray))
    );
    assert_eq!(
      type_specifier_non_array("image2DMSArray"),
      Ok(("", syntax::TypeSpecifierNonArray::Image2DMSArray))
    );
    assert_eq!(
      type_specifier_non_array("samplerCubeArray"),
      Ok(("", syntax::TypeSpecifierNonArray::SamplerCubeArray))
    );
    assert_eq!(
      type_specifier_non_array("imageCubeArray"),
      Ok(("", syntax::TypeSpecifierNonArray::ImageCubeArray))
    );
    assert_eq!(
      type_specifier_non_array("sampler1DShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler1DShadow))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DShadow))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DRectShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DRectShadow))
    );
    assert_eq!(
      type_specifier_non_array("sampler1DArrayShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler1DArrayShadow))
    );
    assert_eq!(
      type_specifier_non_array("sampler2DArrayShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::Sampler2DArrayShadow))
    );
    assert_eq!(
      type_specifier_non_array("samplerCubeShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::SamplerCubeShadow))
    );
    assert_eq!(
      type_specifier_non_array("samplerCubeArrayShadow"),
      Ok(("", syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow))
    );
    assert_eq!(
      type_specifier_non_array("isampler1D"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler1D))
    );
    assert_eq!(
      type_specifier_non_array("iimage1D"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage1D))
    );
    assert_eq!(
      type_specifier_non_array("isampler2D"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler2D))
    );
    assert_eq!(
      type_specifier_non_array("iimage2D"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage2D))
    );
    assert_eq!(
      type_specifier_non_array("isampler3D"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler3D))
    );
    assert_eq!(
      type_specifier_non_array("iimage3D"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage3D))
    );
    assert_eq!(
      type_specifier_non_array("isamplerCube"),
      Ok(("", syntax::TypeSpecifierNonArray::ISamplerCube))
    );
    assert_eq!(
      type_specifier_non_array("iimageCube"),
      Ok(("", syntax::TypeSpecifierNonArray::IImageCube))
    );
    assert_eq!(
      type_specifier_non_array("isampler2DRect"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler2DRect))
    );
    assert_eq!(
      type_specifier_non_array("iimage2DRect"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage2DRect))
    );
    assert_eq!(
      type_specifier_non_array("isampler1DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler1DArray))
    );
    assert_eq!(
      type_specifier_non_array("iimage1DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage1DArray))
    );
    assert_eq!(
      type_specifier_non_array("isampler2DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler2DArray))
    );
    assert_eq!(
      type_specifier_non_array("iimage2DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage2DArray))
    );
    assert_eq!(
      type_specifier_non_array("isamplerBuffer"),
      Ok(("", syntax::TypeSpecifierNonArray::ISamplerBuffer))
    );
    assert_eq!(
      type_specifier_non_array("iimageBuffer"),
      Ok(("", syntax::TypeSpecifierNonArray::IImageBuffer))
    );
    assert_eq!(
      type_specifier_non_array("isampler2DMS"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler2DMS))
    );
    assert_eq!(
      type_specifier_non_array("iimage2DMS"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage2DMS))
    );
    assert_eq!(
      type_specifier_non_array("isampler2DMSArray"),
      Ok(("", syntax::TypeSpecifierNonArray::ISampler2DMSArray))
    );
    assert_eq!(
      type_specifier_non_array("iimage2DMSArray"),
      Ok(("", syntax::TypeSpecifierNonArray::IImage2DMSArray))
    );
    assert_eq!(
      type_specifier_non_array("isamplerCubeArray"),
      Ok(("", syntax::TypeSpecifierNonArray::ISamplerCubeArray))
    );
    assert_eq!(
      type_specifier_non_array("iimageCubeArray"),
      Ok(("", syntax::TypeSpecifierNonArray::IImageCubeArray))
    );
    assert_eq!(
      type_specifier_non_array("atomic_uint"),
      Ok(("", syntax::TypeSpecifierNonArray::AtomicUInt))
    );
    assert_eq!(
      type_specifier_non_array("usampler1D"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler1D))
    );
    assert_eq!(
      type_specifier_non_array("uimage1D"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage1D))
    );
    assert_eq!(
      type_specifier_non_array("usampler2D"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler2D))
    );
    assert_eq!(
      type_specifier_non_array("uimage2D"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage2D))
    );
    assert_eq!(
      type_specifier_non_array("usampler3D"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler3D))
    );
    assert_eq!(
      type_specifier_non_array("uimage3D"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage3D))
    );
    assert_eq!(
      type_specifier_non_array("usamplerCube"),
      Ok(("", syntax::TypeSpecifierNonArray::USamplerCube))
    );
    assert_eq!(
      type_specifier_non_array("uimageCube"),
      Ok(("", syntax::TypeSpecifierNonArray::UImageCube))
    );
    assert_eq!(
      type_specifier_non_array("usampler2DRect"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler2DRect))
    );
    assert_eq!(
      type_specifier_non_array("uimage2DRect"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage2DRect))
    );
    assert_eq!(
      type_specifier_non_array("usampler1DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler1DArray))
    );
    assert_eq!(
      type_specifier_non_array("uimage1DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage1DArray))
    );
    assert_eq!(
      type_specifier_non_array("usampler2DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler2DArray))
    );
    assert_eq!(
      type_specifier_non_array("uimage2DArray"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage2DArray))
    );
    assert_eq!(
      type_specifier_non_array("usamplerBuffer"),
      Ok(("", syntax::TypeSpecifierNonArray::USamplerBuffer))
    );
    assert_eq!(
      type_specifier_non_array("uimageBuffer"),
      Ok(("", syntax::TypeSpecifierNonArray::UImageBuffer))
    );
    assert_eq!(
      type_specifier_non_array("usampler2DMS"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler2DMS))
    );
    assert_eq!(
      type_specifier_non_array("uimage2DMS"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage2DMS))
    );
    assert_eq!(
      type_specifier_non_array("usampler2DMSArray"),
      Ok(("", syntax::TypeSpecifierNonArray::USampler2DMSArray))
    );
    assert_eq!(
      type_specifier_non_array("uimage2DMSArray"),
      Ok(("", syntax::TypeSpecifierNonArray::UImage2DMSArray))
    );
    assert_eq!(
      type_specifier_non_array("usamplerCubeArray"),
      Ok(("", syntax::TypeSpecifierNonArray::USamplerCubeArray))
    );
    assert_eq!(
      type_specifier_non_array("uimageCubeArray"),
      Ok(("", syntax::TypeSpecifierNonArray::UImageCubeArray))
    );
    assert_eq!(
      type_specifier_non_array("ReturnType"),
      Ok((
        "",
        syntax::TypeSpecifierNonArray::TypeName(syntax::TypeName::new("ReturnType").unwrap())
      ))
    );
  }

  #[test]
  fn parse_type_specifier() {
    assert_eq!(
      type_specifier("uint;"),
      Ok((
        ";",
        syntax::TypeSpecifier {
          ty: syntax::TypeSpecifierNonArray::UInt,
          array_specifier: None
        }
      ))
    );
    assert_eq!(
      type_specifier("iimage2DMSArray[35];"),
      Ok((
        ";",
        syntax::TypeSpecifier {
          ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
          array_specifier: Some(syntax::ArraySpecifier::ExplicitlySized(Box::new(
            syntax::Expr::IntConst(35)
          )))
        }
      ))
    );
  }

  #[test]
  fn parse_fully_specified_type() {
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: None,
    };
    let expected = syntax::FullySpecifiedType {
      qualifier: None,
      ty,
    };

    assert_eq!(
      fully_specified_type("iimage2DMSArray;"),
      Ok((";", expected.clone()))
    );
  }

  #[test]
  fn parse_fully_specified_type_with_qualifier() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Subroutine(vec![
      "vec2".into(),
      "S032_29k".into(),
    ]));
    let qual = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![qual_spec]),
    };
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: None,
    };
    let expected = syntax::FullySpecifiedType {
      qualifier: Some(qual),
      ty,
    };

    assert_eq!(
      fully_specified_type("subroutine (vec2, S032_29k) iimage2DMSArray;"),
      Ok((";", expected.clone()))
    );
    assert_eq!(
      fully_specified_type("subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray ;"),
      Ok((" ;", expected.clone()))
    );
    assert_eq!(
      fully_specified_type("subroutine(vec2,S032_29k)iimage2DMSArray;"),
      Ok((";", expected))
    );
  }

  #[test]
  fn parse_primary_expr_intconst() {
    assert_eq!(primary_expr("0 "), Ok((" ", syntax::Expr::IntConst(0))));
    assert_eq!(primary_expr("1 "), Ok((" ", syntax::Expr::IntConst(1))));
  }

  #[test]
  fn parse_primary_expr_uintconst() {
    assert_eq!(primary_expr("0u "), Ok((" ", syntax::Expr::UIntConst(0))));
    assert_eq!(primary_expr("1u "), Ok((" ", syntax::Expr::UIntConst(1))));
  }

  #[test]
  fn parse_primary_expr_floatconst() {
    assert_eq!(
      primary_expr("0.f "),
      Ok((" ", syntax::Expr::FloatConst(0.)))
    );
    assert_eq!(
      primary_expr("1.f "),
      Ok((" ", syntax::Expr::FloatConst(1.)))
    );
    assert_eq!(
      primary_expr("0.F "),
      Ok((" ", syntax::Expr::FloatConst(0.)))
    );
    assert_eq!(
      primary_expr("1.F "),
      Ok((" ", syntax::Expr::FloatConst(1.)))
    );
  }

  #[test]
  fn parse_primary_expr_doubleconst() {
    assert_eq!(
      primary_expr("0. "),
      Ok((" ", syntax::Expr::DoubleConst(0.)))
    );
    assert_eq!(
      primary_expr("1. "),
      Ok((" ", syntax::Expr::DoubleConst(1.)))
    );
    assert_eq!(
      primary_expr("0.lf "),
      Ok((" ", syntax::Expr::DoubleConst(0.)))
    );
    assert_eq!(
      primary_expr("1.lf "),
      Ok((" ", syntax::Expr::DoubleConst(1.)))
    );
    assert_eq!(
      primary_expr("0.LF "),
      Ok((" ", syntax::Expr::DoubleConst(0.)))
    );
    assert_eq!(
      primary_expr("1.LF "),
      Ok((" ", syntax::Expr::DoubleConst(1.)))
    );
  }

  #[test]
  fn parse_primary_expr_boolconst() {
    assert_eq!(
      primary_expr("false"),
      Ok(("", syntax::Expr::BoolConst(false.to_owned())))
    );
    assert_eq!(
      primary_expr("true"),
      Ok(("", syntax::Expr::BoolConst(true.to_owned())))
    );
  }

  #[test]
  fn parse_primary_expr_parens() {
    assert_eq!(primary_expr("(0)"), Ok(("", syntax::Expr::IntConst(0))));
    assert_eq!(primary_expr("(  0 )"), Ok(("", syntax::Expr::IntConst(0))));
    assert_eq!(
      primary_expr("(  .0 )"),
      Ok(("", syntax::Expr::DoubleConst(0.)))
    );
    assert_eq!(
      primary_expr("(  (.0) )"),
      Ok(("", syntax::Expr::DoubleConst(0.)))
    );
    assert_eq!(
      primary_expr("(true) "),
      Ok((" ", syntax::Expr::BoolConst(true)))
    );
  }

  #[test]
  fn parse_postfix_function_call_no_args() {
    let fun = syntax::FunIdentifier::Identifier("vec3".into());
    let args = Vec::new();
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr("vec3();"), Ok((";", expected.clone())));
    assert_eq!(postfix_expr("vec3   (  ) ;"), Ok((" ;", expected.clone())));
    assert_eq!(postfix_expr("vec3   (\nvoid\n) ;"), Ok((" ;", expected)));
  }

  #[test]
  fn parse_postfix_function_call_one_arg() {
    let fun = syntax::FunIdentifier::Identifier("foo".into());
    let args = vec![syntax::Expr::IntConst(0)];
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr("foo(0);"), Ok((";", expected.clone())));
    assert_eq!(postfix_expr("foo   ( 0 ) ;"), Ok((" ;", expected.clone())));
    assert_eq!(postfix_expr("foo   (\n0\t\n) ;"), Ok((" ;", expected)));
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

    assert_eq!(
      postfix_expr("foo(0, false, bar);"),
      Ok((";", expected.clone()))
    );
    assert_eq!(
      postfix_expr("foo   ( 0\t, false    ,\t\tbar) ;"),
      Ok((" ;", expected))
    );
  }

  #[test]
  fn parse_postfix_expr_bracket() {
    let id = syntax::Expr::Variable("foo".into());
    let array_spec =
      syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(7354)));
    let expected = syntax::Expr::Bracket(Box::new(id), array_spec);

    assert_eq!(postfix_expr("foo[7354];"), Ok((";", expected.clone())));
    assert_eq!(postfix_expr("foo[\n  7354    ] ;"), Ok((" ;", expected)));
  }

  #[test]
  fn parse_postfix_expr_dot() {
    let foo = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::Expr::Dot(foo, "bar".into());

    assert_eq!(postfix_expr("foo.bar;"), Ok((";", expected.clone())));
    assert_eq!(postfix_expr("(foo).bar;"), Ok((";", expected)));
  }

  #[test]
  fn parse_postfix_expr_dot_several() {
    let foo = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::Expr::Dot(Box::new(syntax::Expr::Dot(foo, "bar".into())), "zoo".into());

    assert_eq!(postfix_expr("foo.bar.zoo;"), Ok((";", expected.clone())));
    assert_eq!(postfix_expr("(foo).bar.zoo;"), Ok((";", expected.clone())));
    assert_eq!(postfix_expr("(foo.bar).zoo;"), Ok((";", expected)));
  }

  #[test]
  fn parse_postfix_postinc() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::PostInc(Box::new(foo));

    assert_eq!(postfix_expr("foo++;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_postfix_postdec() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::PostDec(Box::new(foo));

    assert_eq!(postfix_expr("foo--;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_unary_add() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Add, Box::new(foo));

    assert_eq!(unary_expr("+foo;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_unary_minus() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Minus, Box::new(foo));

    assert_eq!(unary_expr("-foo;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_unary_not() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Not, Box::new(foo));

    assert_eq!(unary_expr("!foo;"), Ok((";", expected)));
  }

  #[test]
  fn parse_unary_complement() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Complement, Box::new(foo));

    assert_eq!(unary_expr("~foo;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_unary_inc() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Inc, Box::new(foo));

    assert_eq!(unary_expr("++foo;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_unary_dec() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Dec, Box::new(foo));

    assert_eq!(unary_expr("--foo;"), Ok((";", expected.clone())));
  }

  #[test]
  fn parse_expr_float() {
    assert_eq!(expr("314.;"), Ok((";", syntax::Expr::DoubleConst(314.))));
    assert_eq!(expr("314.f;"), Ok((";", syntax::Expr::FloatConst(314.))));
  }

  #[test]
  fn parse_expr_add_2() {
    let one = Box::new(syntax::Expr::IntConst(1));
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, one.clone(), one);

    assert_eq!(expr("1 + 1;"), Ok((";", expected.clone())));
    assert_eq!(expr("1+1;"), Ok((";", expected.clone())));
    assert_eq!(expr("(1 + 1);"), Ok((";", expected)));
  }

  #[test]
  fn parse_expr_add_3() {
    let one = Box::new(syntax::Expr::UIntConst(1));
    let two = Box::new(syntax::Expr::UIntConst(2));
    let three = Box::new(syntax::Expr::UIntConst(3));
    let expected = syntax::Expr::Binary(
      syntax::BinaryOp::Add,
      Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, one, two)),
      three,
    );

    assert_eq!(expr("1u + 2u + 3u"), Ok(("", expected.clone())));
    assert_eq!(expr("1u + 2u + 3u   "), Ok(("   ", expected.clone())));
    assert_eq!(expr("1u+2u+3u"), Ok(("", expected.clone())));
    assert_eq!(expr("((1u + 2u) + 3u)"), Ok(("", expected)));
  }

  #[test]
  fn parse_expr_add_mult_3() {
    let one = Box::new(syntax::Expr::UIntConst(1));
    let two = Box::new(syntax::Expr::UIntConst(2));
    let three = Box::new(syntax::Expr::UIntConst(3));
    let expected = syntax::Expr::Binary(
      syntax::BinaryOp::Add,
      Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, two)),
      three,
    );

    assert_eq!(expr("1u * 2u + 3u ;"), Ok((" ;", expected.clone())));
    assert_eq!(expr("1u*2u+3u;"), Ok((";", expected.clone())));
    assert_eq!(expr("(1u * 2u) + 3u;"), Ok((";", expected)));
  }

  #[test]
  fn parse_expr_add_sub_mult_div() {
    let one = Box::new(syntax::Expr::IntConst(1));
    let two = Box::new(syntax::Expr::IntConst(2));
    let three = Box::new(syntax::Expr::IntConst(3));
    let four = Box::new(syntax::Expr::IntConst(4));
    let five = Box::new(syntax::Expr::IntConst(5));
    let six = Box::new(syntax::Expr::IntConst(6));
    let expected = syntax::Expr::Binary(
      syntax::BinaryOp::Add,
      Box::new(syntax::Expr::Binary(
        syntax::BinaryOp::Mult,
        one,
        Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)),
      )),
      Box::new(syntax::Expr::Binary(
        syntax::BinaryOp::Div,
        four,
        Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, five, six)),
      )),
    );

    assert_eq!(
      expr("1 * (2 + 3) + 4 / (5 + 6);"),
      Ok((";", expected.clone()))
    );
  }

  #[test]
  fn parse_complex_expr() {
    let input = "normalize((inverse(view) * vec4(ray.dir, 0.)).xyz);";
    let zero = syntax::Expr::DoubleConst(0.);
    let ray = syntax::Expr::Variable("ray".into());
    let raydir = syntax::Expr::Dot(Box::new(ray), "dir".into());
    let vec4 = syntax::Expr::FunCall(
      syntax::FunIdentifier::Identifier("vec4".into()),
      vec![raydir, zero],
    );
    let view = syntax::Expr::Variable("view".into());
    let iview = syntax::Expr::FunCall(
      syntax::FunIdentifier::Identifier("inverse".into()),
      vec![view],
    );
    let mul = syntax::Expr::Binary(syntax::BinaryOp::Mult, Box::new(iview), Box::new(vec4));
    let xyz = syntax::Expr::Dot(Box::new(mul), "xyz".into());
    let normalize = syntax::Expr::FunCall(
      syntax::FunIdentifier::Identifier("normalize".into()),
      vec![xyz],
    );
    let expected = normalize;

    assert_eq!(expr(&input[..]), Ok((";", expected)));
  }

  #[test]
  fn parse_function_identifier_typename() {
    let expected = syntax::FunIdentifier::Identifier("foo".into());
    assert_eq!(function_identifier("foo("), Ok(("(", expected.clone())));
    assert_eq!(function_identifier("foo\n\t("), Ok(("(", expected.clone())));
    assert_eq!(function_identifier("foo\n ("), Ok(("(", expected)));
  }

  #[test]
  fn parse_function_identifier_cast() {
    let expected = syntax::FunIdentifier::Identifier("vec3".into());
    assert_eq!(function_identifier("vec3("), Ok(("(", expected.clone())));
    assert_eq!(function_identifier("vec3 ("), Ok(("(", expected.clone())));
    assert_eq!(function_identifier("vec3\t\n\n \t ("), Ok(("(", expected)));
  }

  #[test]
  fn parse_function_identifier_cast_array_unsized() {
    let expected = syntax::FunIdentifier::Expr(Box::new(syntax::Expr::Bracket(
      Box::new(syntax::Expr::Variable("vec3".into())),
      syntax::ArraySpecifier::Unsized,
    )));

    assert_eq!(function_identifier("vec3[]("), Ok(("(", expected.clone())));
    assert_eq!(function_identifier("vec3  [\t\n]("), Ok(("(", expected)));
  }

  #[test]
  fn parse_function_identifier_cast_array_sized() {
    let expected = syntax::FunIdentifier::Expr(Box::new(syntax::Expr::Bracket(
      Box::new(syntax::Expr::Variable("vec3".into())),
      syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(12))),
    )));

    assert_eq!(
      function_identifier("vec3[12]("),
      Ok(("(", expected.clone()))
    );
    assert_eq!(function_identifier("vec3  [\t 12\n]("), Ok(("(", expected)));
  }

  #[test]
  fn parse_void() {
    assert_eq!(void("void "), Ok((" ", ())));
  }

  #[test]
  fn parse_assignment_op_equal() {
    assert_eq!(assignment_op("= "), Ok((" ", syntax::AssignmentOp::Equal)));
  }

  #[test]
  fn parse_assignment_op_mult() {
    assert_eq!(assignment_op("*= "), Ok((" ", syntax::AssignmentOp::Mult)));
  }

  #[test]
  fn parse_assignment_op_div() {
    assert_eq!(assignment_op("/= "), Ok((" ", syntax::AssignmentOp::Div)));
  }

  #[test]
  fn parse_assignment_op_mod() {
    assert_eq!(assignment_op("%= "), Ok((" ", syntax::AssignmentOp::Mod)));
  }

  #[test]
  fn parse_assignment_op_add() {
    assert_eq!(assignment_op("+= "), Ok((" ", syntax::AssignmentOp::Add)));
  }

  #[test]
  fn parse_assignment_op_sub() {
    assert_eq!(assignment_op("-= "), Ok((" ", syntax::AssignmentOp::Sub)));
  }

  #[test]
  fn parse_assignment_op_lshift() {
    assert_eq!(
      assignment_op("<<= "),
      Ok((" ", syntax::AssignmentOp::LShift))
    );
  }

  #[test]
  fn parse_assignment_op_rshift() {
    assert_eq!(
      assignment_op(">>= "),
      Ok((" ", syntax::AssignmentOp::RShift))
    );
  }

  #[test]
  fn parse_assignment_op_and() {
    assert_eq!(assignment_op("&= "), Ok((" ", syntax::AssignmentOp::And)));
  }

  #[test]
  fn parse_assignment_op_xor() {
    assert_eq!(assignment_op("^= "), Ok((" ", syntax::AssignmentOp::Xor)));
  }

  #[test]
  fn parse_assignment_op_or() {
    assert_eq!(assignment_op("|= "), Ok((" ", syntax::AssignmentOp::Or)));
  }

  #[test]
  fn parse_expr_statement() {
    let expected = Some(syntax::Expr::Assignment(
      Box::new(syntax::Expr::Variable("foo".into())),
      syntax::AssignmentOp::Equal,
      Box::new(syntax::Expr::FloatConst(314.)),
    ));

    assert_eq!(expr_statement("foo = 314.f;"), Ok(("", expected.clone())));
    assert_eq!(expr_statement("foo=314.f;"), Ok(("", expected.clone())));
    assert_eq!(expr_statement("foo\n\t=  \n314.f;"), Ok(("", expected)));
  }

  #[test]
  fn parse_declaration_function_prototype() {
    let rt = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None,
      },
    };
    let arg0_ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec2,
      array_specifier: None,
    };
    let arg0 = syntax::FunctionParameterDeclaration::Unnamed(None, arg0_ty);
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Out);
    let qual = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![qual_spec]),
    };
    let arg1 = syntax::FunctionParameterDeclaration::Named(
      Some(qual),
      syntax::FunctionParameterDeclarator {
        ty: syntax::TypeSpecifier {
          ty: syntax::TypeSpecifierNonArray::Float,
          array_specifier: None,
        },
        ident: "the_arg".into(),
      },
    );
    let fp = syntax::FunctionPrototype {
      ty: rt,
      name: "foo".into(),
      parameters: vec![arg0, arg1],
    };
    let expected = syntax::Declaration::FunctionPrototype(fp);

    assert_eq!(
      declaration("vec3 foo(vec2, out float the_arg);"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      declaration("vec3 \nfoo ( vec2\n, out float \n\tthe_arg )\n;"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      declaration("vec3 foo(vec2,out float the_arg);"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_declaration_init_declarator_list_single() {
    let ty = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Int,
        array_specifier: None,
      },
    };
    let sd = syntax::SingleDeclaration {
      ty,
      name: Some("foo".into()),
      array_specifier: None,
      initializer: Some(syntax::Initializer::Simple(Box::new(
        syntax::Expr::IntConst(34),
      ))),
    };
    let idl = syntax::InitDeclaratorList {
      head: sd,
      tail: Vec::new(),
    };
    let expected = syntax::Declaration::InitDeclaratorList(idl);

    assert_eq!(declaration("int foo = 34;"), Ok(("", expected.clone())));
    assert_eq!(declaration("int foo=34;"), Ok(("", expected.clone())));
    assert_eq!(declaration("int    \t  \nfoo =\t34  ;"), Ok(("", expected)));
  }

  #[test]
  fn parse_declaration_init_declarator_list_complex() {
    let ty = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Int,
        array_specifier: None,
      },
    };
    let sd = syntax::SingleDeclaration {
      ty,
      name: Some("foo".into()),
      array_specifier: None,
      initializer: Some(syntax::Initializer::Simple(Box::new(
        syntax::Expr::IntConst(34),
      ))),
    };
    let sdnt = syntax::SingleDeclarationNoType {
      ident: "bar".into(),
      initializer: Some(syntax::Initializer::Simple(Box::new(
        syntax::Expr::IntConst(12),
      ))),
    };
    let expected = syntax::Declaration::InitDeclaratorList(syntax::InitDeclaratorList {
      head: sd,
      tail: vec![sdnt],
    });

    assert_eq!(
      declaration("int foo = 34, bar = 12;"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      declaration("int foo=34,bar=12;"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      declaration("int    \t  \nfoo =\t34 \n,\tbar=      12\n ;"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_declaration_precision_low() {
    let qual = syntax::PrecisionQualifier::Low;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None,
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration("precision lowp float;"), Ok(("", expected)));
  }

  #[test]
  fn parse_declaration_precision_medium() {
    let qual = syntax::PrecisionQualifier::Medium;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None,
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration("precision mediump float;"), Ok(("", expected)));
  }

  #[test]
  fn parse_declaration_precision_high() {
    let qual = syntax::PrecisionQualifier::High;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None,
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration("precision highp float;"), Ok(("", expected)));
  }

  #[test]
  fn parse_declaration_uniform_block() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Uniform);
    let qual = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![qual_spec]),
    };
    let f0 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["a".into()]),
    };
    let f1 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["b".into()]),
    };
    let f2 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("foo".into()),
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["c".into(), "d".into()]),
    };
    let expected = syntax::Declaration::Block(syntax::Block {
      qualifier: qual,
      name: "UniformBlockTest".into(),
      fields: vec![f0, f1, f2],
      identifier: None,
    });

    assert_eq!(
      declaration("uniform UniformBlockTest { float a; vec3 b; foo c, d; };"),
      Ok(("", expected.clone()))
    );
    assert_eq!(declaration("uniform   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;"), Ok(("", expected)));
  }

  #[test]
  fn parse_declaration_buffer_block() {
    let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer);
    let qual = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![qual_spec]),
    };
    let f0 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["a".into()]),
    };
    let f1 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec![syntax::ArrayedIdentifier::new(
        "b",
        Some(syntax::ArraySpecifier::Unsized),
      )]),
    };
    let f2 = syntax::StructFieldSpecifier {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::TypeName("foo".into()),
        array_specifier: None,
      },
      identifiers: syntax::NonEmpty(vec!["c".into(), "d".into()]),
    };
    let expected = syntax::Declaration::Block(syntax::Block {
      qualifier: qual,
      name: "UniformBlockTest".into(),
      fields: vec![f0, f1, f2],
      identifier: None,
    });

    assert_eq!(
      declaration("buffer UniformBlockTest { float a; vec3 b[]; foo c, d; };"),
      Ok(("", expected.clone()))
    );
    assert_eq!(declaration("buffer   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b   [   ]\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;"), Ok(("", expected)));
  }

  #[test]
  fn parse_selection_statement_if() {
    let cond = syntax::Expr::Binary(
      syntax::BinaryOp::LT,
      Box::new(syntax::Expr::Variable("foo".into())),
      Box::new(syntax::Expr::IntConst(10)),
    );
    let ret = Box::new(syntax::Expr::BoolConst(false));
    let st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
      syntax::JumpStatement::Return(Some(ret)),
    )));
    let body = syntax::Statement::Compound(Box::new(syntax::CompoundStatement {
      statement_list: vec![st],
    }));
    let rest = syntax::SelectionRestStatement::Statement(Box::new(body));
    let expected = syntax::SelectionStatement {
      cond: Box::new(cond),
      rest,
    };

    assert_eq!(
      selection_statement("if (foo < 10) { return false; }K"),
      Ok(("K", expected.clone()))
    );
    assert_eq!(
      selection_statement("if \n(foo<10\n) \t{return false;}K"),
      Ok(("K", expected))
    );
  }

  #[test]
  fn parse_selection_statement_if_else() {
    let cond = syntax::Expr::Binary(
      syntax::BinaryOp::LT,
      Box::new(syntax::Expr::Variable("foo".into())),
      Box::new(syntax::Expr::IntConst(10)),
    );
    let if_ret = Box::new(syntax::Expr::FloatConst(0.));
    let if_st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
      syntax::JumpStatement::Return(Some(if_ret)),
    )));
    let if_body = syntax::Statement::Compound(Box::new(syntax::CompoundStatement {
      statement_list: vec![if_st],
    }));
    let else_ret = Box::new(syntax::Expr::Variable("foo".into()));
    let else_st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
      syntax::JumpStatement::Return(Some(else_ret)),
    )));
    let else_body = syntax::Statement::Compound(Box::new(syntax::CompoundStatement {
      statement_list: vec![else_st],
    }));
    let rest = syntax::SelectionRestStatement::Else(Box::new(if_body), Box::new(else_body));
    let expected = syntax::SelectionStatement {
      cond: Box::new(cond),
      rest,
    };

    assert_eq!(
      selection_statement("if (foo < 10) { return 0.f; } else { return foo; }"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      selection_statement("if \n(foo<10\n) \t{return 0.f\t;\n\n}\n else{\n\t return foo   ;}"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_switch_statement_empty() {
    let head = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::SwitchStatement {
      head,
      body: Vec::new(),
    };

    assert_eq!(
      switch_statement("switch (foo) {}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      switch_statement("switch(foo){}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      switch_statement("switch\n\n (  foo  \t   \n) { \n\n   }"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_switch_statement_cases() {
    let head = Box::new(syntax::Expr::Variable("foo".into()));
    let case0 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::CaseLabel(
      syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(0))),
    )));
    let case1 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::CaseLabel(
      syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(1))),
    )));
    let ret = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
      syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::UIntConst(12)))),
    )));
    let expected = syntax::SwitchStatement {
      head,
      body: vec![case0, case1, ret],
    };

    assert_eq!(
      switch_statement("switch (foo) { case 0: case 1: return 12u; }"),
      Ok(("", expected.clone()))
    );
  }

  #[test]
  fn parse_case_label_def() {
    assert_eq!(case_label("default:"), Ok(("", syntax::CaseLabel::Def)));
    assert_eq!(case_label("default   :"), Ok(("", syntax::CaseLabel::Def)));
  }

  #[test]
  fn parse_case_label() {
    let expected = syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(3)));

    assert_eq!(case_label("case 3:"), Ok(("", expected.clone())));
    assert_eq!(case_label("case\n\t 3   :"), Ok(("", expected)));
  }

  #[test]
  fn parse_iteration_statement_while_empty() {
    let cond = syntax::Condition::Expr(Box::new(syntax::Expr::Binary(
      syntax::BinaryOp::GTE,
      Box::new(syntax::Expr::Variable("a".into())),
      Box::new(syntax::Expr::Variable("b".into())),
    )));
    let st = syntax::Statement::Compound(Box::new(syntax::CompoundStatement {
      statement_list: Vec::new(),
    }));
    let expected = syntax::IterationStatement::While(cond, Box::new(st));

    assert_eq!(
      iteration_statement("while (a >= b) {}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      iteration_statement("while(a>=b){}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      iteration_statement("while (  a >=\n\tb  )\t  {   \n}"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_iteration_statement_do_while_empty() {
    let st = syntax::Statement::Compound(Box::new(syntax::CompoundStatement {
      statement_list: Vec::new(),
    }));
    let cond = Box::new(syntax::Expr::Binary(
      syntax::BinaryOp::GTE,
      Box::new(syntax::Expr::Variable("a".into())),
      Box::new(syntax::Expr::Variable("b".into())),
    ));
    let expected = syntax::IterationStatement::DoWhile(Box::new(st), cond);

    assert_eq!(
      iteration_statement("do {} while (a >= b);"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      iteration_statement("do{}while(a>=b);"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      iteration_statement("do \n {\n} while (  a >=\n\tb  )\t  \n;"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_iteration_statement_for_empty() {
    let init = syntax::ForInitStatement::Declaration(Box::new(
      syntax::Declaration::InitDeclaratorList(syntax::InitDeclaratorList {
        head: syntax::SingleDeclaration {
          ty: syntax::FullySpecifiedType {
            qualifier: None,
            ty: syntax::TypeSpecifier {
              ty: syntax::TypeSpecifierNonArray::Float,
              array_specifier: None,
            },
          },
          name: Some("i".into()),
          array_specifier: None,
          initializer: Some(syntax::Initializer::Simple(Box::new(
            syntax::Expr::FloatConst(0.),
          ))),
        },
        tail: Vec::new(),
      }),
    ));
    let rest = syntax::ForRestStatement {
      condition: Some(syntax::Condition::Expr(Box::new(syntax::Expr::Binary(
        syntax::BinaryOp::LTE,
        Box::new(syntax::Expr::Variable("i".into())),
        Box::new(syntax::Expr::FloatConst(10.)),
      )))),
      post_expr: Some(Box::new(syntax::Expr::Unary(
        syntax::UnaryOp::Inc,
        Box::new(syntax::Expr::Variable("i".into())),
      ))),
    };
    let st = syntax::Statement::Compound(Box::new(syntax::CompoundStatement {
      statement_list: Vec::new(),
    }));
    let expected = syntax::IterationStatement::For(init, rest, Box::new(st));

    assert_eq!(
      iteration_statement("for (float i = 0.f; i <= 10.f; ++i) {}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      iteration_statement("for(float i=0.f;i<=10.f;++i){}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      iteration_statement("for\n\t (  \t\n\nfloat \ni \t=\n0.f\n;\ni\t<=  10.f; \n++i\n)\n{\n}"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_jump_continue() {
    assert_eq!(
      jump_statement("continue;"),
      Ok(("", syntax::JumpStatement::Continue))
    );
  }

  #[test]
  fn parse_jump_break() {
    assert_eq!(
      jump_statement("break;"),
      Ok(("", syntax::JumpStatement::Break))
    );
  }

  #[test]
  fn parse_jump_return() {
    let expected = syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::IntConst(3))));
    assert_eq!(jump_statement("return 3;"), Ok(("", expected)));
  }

  #[test]
  fn parse_jump_empty_return() {
    let expected = syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(None));
    assert_eq!(simple_statement("return;"), Ok(("", expected)));
  }

  #[test]
  fn parse_jump_discard() {
    assert_eq!(
      jump_statement("discard;"),
      Ok(("", syntax::JumpStatement::Discard))
    );
  }

  #[test]
  fn parse_simple_statement_return() {
    let e = syntax::Expr::BoolConst(false);
    let expected = syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(Some(Box::new(e))));

    assert_eq!(simple_statement("return false;"), Ok(("", expected)));
  }

  #[test]
  fn parse_compound_statement_empty() {
    let expected = syntax::CompoundStatement {
      statement_list: Vec::new(),
    };

    assert_eq!(compound_statement("{}"), Ok(("", expected)));
  }

  #[test]
  fn parse_compound_statement() {
    let st0 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Selection(
      syntax::SelectionStatement {
        cond: Box::new(syntax::Expr::BoolConst(true)),
        rest: syntax::SelectionRestStatement::Statement(Box::new(syntax::Statement::Compound(
          Box::new(syntax::CompoundStatement {
            statement_list: Vec::new(),
          }),
        ))),
      },
    )));
    let st1 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Declaration(
      syntax::Declaration::InitDeclaratorList(syntax::InitDeclaratorList {
        head: syntax::SingleDeclaration {
          ty: syntax::FullySpecifiedType {
            qualifier: None,
            ty: syntax::TypeSpecifier {
              ty: syntax::TypeSpecifierNonArray::ISampler3D,
              array_specifier: None,
            },
          },
          name: Some("x".into()),
          array_specifier: None,
          initializer: None,
        },
        tail: Vec::new(),
      }),
    )));
    let st2 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
      syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::IntConst(42)))),
    )));
    let expected = syntax::CompoundStatement {
      statement_list: vec![st0, st1, st2],
    };

    assert_eq!(
      compound_statement("{ if (true) {} isampler3D x; return 42 ; }"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      compound_statement("{if(true){}isampler3D x;return 42;}"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_function_definition() {
    let rt = syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::IImage2DArray,
        array_specifier: None,
      },
    };
    let fp = syntax::FunctionPrototype {
      ty: rt,
      name: "foo".into(),
      parameters: Vec::new(),
    };
    let st0 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
      syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::Variable("bar".into())))),
    )));
    let expected = syntax::FunctionDefinition {
      prototype: fp,
      statement: syntax::CompoundStatement {
        statement_list: vec![st0],
      },
    };

    assert_eq!(
      function_definition("iimage2DArray foo() { return bar; }"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      function_definition("iimage2DArray \tfoo\n()\n \n{\n return \nbar\n;}"),
      Ok(("", expected.clone()))
    );
    assert_eq!(
      function_definition("iimage2DArray foo(){return bar;}"),
      Ok(("", expected))
    );
  }

  #[test]
  fn parse_buffer_block_0() {
    let src = include_str!("../data/tests/buffer_block_0.glsl");
    let main_fn = syntax::ExternalDeclaration::FunctionDefinition(syntax::FunctionDefinition {
      prototype: syntax::FunctionPrototype {
        ty: syntax::FullySpecifiedType {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::Void,
            array_specifier: None,
          },
        },
        name: "main".into(),
        parameters: Vec::new(),
      },
      statement: syntax::CompoundStatement {
        statement_list: Vec::new(),
      },
    });
    let buffer_block =
      syntax::ExternalDeclaration::Declaration(syntax::Declaration::Block(syntax::Block {
        qualifier: syntax::TypeQualifier {
          qualifiers: syntax::NonEmpty(vec![syntax::TypeQualifierSpec::Storage(
            syntax::StorageQualifier::Buffer,
          )]),
        },
        name: "Foo".into(),
        fields: vec![syntax::StructFieldSpecifier {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::TypeName("char".into()),
            array_specifier: None,
          },
          identifiers: syntax::NonEmpty(vec![syntax::ArrayedIdentifier::new(
            "tiles",
            Some(syntax::ArraySpecifier::Unsized),
          )]),
        }],
        identifier: Some("main_tiles".into()),
      }));
    let expected = syntax::TranslationUnit(syntax::NonEmpty(vec![buffer_block, main_fn]));

    assert_eq!(translation_unit(src), Ok(("", expected)));
  }

  #[test]
  fn parse_layout_buffer_block_0() {
    let src = include_str!("../data/tests/layout_buffer_block_0.glsl");
    let layout = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![
        syntax::LayoutQualifierSpec::Identifier(
          "set".into(),
          Some(Box::new(syntax::Expr::IntConst(0))),
        ),
        syntax::LayoutQualifierSpec::Identifier(
          "binding".into(),
          Some(Box::new(syntax::Expr::IntConst(0))),
        ),
      ]),
    };
    let type_qual = syntax::TypeQualifier {
      qualifiers: syntax::NonEmpty(vec![
        syntax::TypeQualifierSpec::Layout(layout),
        syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer),
      ]),
    };
    let block =
      syntax::ExternalDeclaration::Declaration(syntax::Declaration::Block(syntax::Block {
        qualifier: type_qual,
        name: "Foo".into(),
        fields: vec![syntax::StructFieldSpecifier {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::TypeName("char".into()),
            array_specifier: None,
          },
          identifiers: syntax::NonEmpty(vec!["a".into()]),
        }],
        identifier: Some("foo".into()),
      }));

    let expected = syntax::TranslationUnit(syntax::NonEmpty(vec![block]));

    assert_eq!(translation_unit(src), Ok(("", expected)));
  }

  #[test]
  fn parse_pp_space0() {
    assert_eq!(pp_space0("   \\\n  "), Ok(("", "   \\\n  ")));
    assert_eq!(pp_space0(""), Ok(("", "")));
  }

  #[test]
  fn parse_pp_version_number() {
    assert_eq!(pp_version_number("450"), Ok(("", 450)));
  }

  #[test]
  fn parse_pp_version_profile() {
    assert_eq!(
      pp_version_profile("core"),
      Ok(("", syntax::PreprocessorVersionProfile::Core))
    );
    assert_eq!(
      pp_version_profile("compatibility"),
      Ok(("", syntax::PreprocessorVersionProfile::Compatibility))
    );
    assert_eq!(
      pp_version_profile("es"),
      Ok(("", syntax::PreprocessorVersionProfile::ES))
    );
  }

  #[test]
  fn parse_pp_version() {
    assert_eq!(
      preprocessor("#version 450\n"),
      Ok((
        "",
        syntax::Preprocessor::Version(syntax::PreprocessorVersion {
          version: 450,
          profile: None,
        })
      ))
    );

    assert_eq!(
      preprocessor("#version 450 core\n"),
      Ok((
        "",
        syntax::Preprocessor::Version(syntax::PreprocessorVersion {
          version: 450,
          profile: Some(syntax::PreprocessorVersionProfile::Core)
        })
      ))
    );
  }

  #[test]
  fn parse_pp_version_newline() {
    assert_eq!(
      preprocessor("#version 450\n"),
      Ok((
        "",
        syntax::Preprocessor::Version(syntax::PreprocessorVersion {
          version: 450,
          profile: None,
        })
      ))
    );

    assert_eq!(
      preprocessor("#version 450 core\n"),
      Ok((
        "",
        syntax::Preprocessor::Version(syntax::PreprocessorVersion {
          version: 450,
          profile: Some(syntax::PreprocessorVersionProfile::Core)
        })
      ))
    );
  }

  #[test]
  fn parse_pp_define() {
    let expect = |v: &str| {
      Ok((
        "",
        syntax::Preprocessor::Define(syntax::PreprocessorDefine::ObjectLike {
          ident: "test".into(),
          value: v.to_owned(),
        }),
      ))
    };

    assert_eq!(preprocessor("#define test 1.0"), expect("1.0"));
    assert_eq!(preprocessor("#define test \\\n   1.0"), expect("1.0"));
    assert_eq!(preprocessor("#define test 1.0\n"), expect("1.0"));

    assert_eq!(
      preprocessor("#define test123 .0f\n"),
      Ok((
        "",
        syntax::Preprocessor::Define(syntax::PreprocessorDefine::ObjectLike {
          ident: "test123".into(),
          value: ".0f".to_owned()
        })
      ))
    );

    assert_eq!(
      preprocessor("#define test 1\n"),
      Ok((
        "",
        syntax::Preprocessor::Define(syntax::PreprocessorDefine::ObjectLike {
          ident: "test".into(),
          value: "1".to_owned()
        })
      ))
    );
  }

  #[test]
  fn parse_pp_define_with_args() {
    assert_eq!(
      preprocessor("#define \\\n add(x, y) \\\n (x + y)"),
      Ok((
        "",
        syntax::Preprocessor::Define(syntax::PreprocessorDefine::FunctionLike {
          ident: "add".into(),
          args: vec![
            syntax::Identifier::new("x").unwrap(),
            syntax::Identifier::new("y").unwrap()
          ],
          value: "(x + y)".to_owned(),
        })
      ))
    );
  }

  #[test]
  fn parse_pp_define_multiline() {
    assert_eq!(
      preprocessor(
        r#"#define foo \
         32"#
      ),
      Ok((
        "",
        syntax::Preprocessor::Define(syntax::PreprocessorDefine::ObjectLike {
          ident: "foo".into(),
          value: "32".to_owned(),
        })
      ))
    );
  }

  #[test]
  fn parse_pp_else() {
    assert_eq!(
      preprocessor("#    else\n"),
      Ok(("", syntax::Preprocessor::Else))
    );
  }

  #[test]
  fn parse_pp_elseif() {
    assert_eq!(
      preprocessor("#   elseif \\\n42\n"),
      Ok((
        "",
        syntax::Preprocessor::ElseIf(syntax::PreprocessorElseIf {
          condition: "42".to_owned()
        })
      ))
    );
  }

  #[test]
  fn parse_pp_endif() {
    assert_eq!(
      preprocessor("#\\\nendif"),
      Ok(("", syntax::Preprocessor::EndIf))
    );
  }

  #[test]
  fn parse_pp_error() {
    assert_eq!(
      preprocessor("#error \\\n     some message"),
      Ok((
        "",
        syntax::Preprocessor::Error(syntax::PreprocessorError {
          message: "some message".to_owned()
        })
      ))
    );
  }

  #[test]
  fn parse_pp_if() {
    assert_eq!(
      preprocessor("# \\\nif 42"),
      Ok((
        "",
        syntax::Preprocessor::If(syntax::PreprocessorIf {
          condition: "42".to_owned()
        })
      ))
    );
  }

  #[test]
  fn parse_pp_ifdef() {
    assert_eq!(
      preprocessor("#ifdef       FOO\n"),
      Ok((
        "",
        syntax::Preprocessor::IfDef(syntax::PreprocessorIfDef {
          ident: syntax::Identifier("FOO".to_owned())
        })
      ))
    );
  }

  #[test]
  fn parse_pp_ifndef() {
    assert_eq!(
      preprocessor("#\\\nifndef \\\n   FOO\n"),
      Ok((
        "",
        syntax::Preprocessor::IfNDef(syntax::PreprocessorIfNDef {
          ident: syntax::Identifier("FOO".to_owned())
        })
      ))
    );
  }

  #[test]
  fn parse_pp_include() {
    assert_eq!(
      preprocessor("#include <filename>\n"),
      Ok((
        "",
        syntax::Preprocessor::Include(syntax::PreprocessorInclude {
          path: syntax::Path::Absolute("filename".to_owned())
        })
      ))
    );

    assert_eq!(
      preprocessor("#include \\\n\"filename\"\n"),
      Ok((
        "",
        syntax::Preprocessor::Include(syntax::PreprocessorInclude {
          path: syntax::Path::Relative("filename".to_owned())
        })
      ))
    );
  }

  #[test]
  fn parse_pp_line() {
    assert_eq!(
      preprocessor("#   line \\\n2\n"),
      Ok((
        "",
        syntax::Preprocessor::Line(syntax::PreprocessorLine {
          line: 2,
          source_string_number: None,
        })
      ))
    );

    assert_eq!(
      preprocessor("#line 2 \\\n 4\n"),
      Ok((
        "",
        syntax::Preprocessor::Line(syntax::PreprocessorLine {
          line: 2,
          source_string_number: Some(4),
        })
      ))
    );
  }

  #[test]
  fn parse_pp_pragma() {
    assert_eq!(
      preprocessor("#\\\npragma  some   flag"),
      Ok((
        "",
        syntax::Preprocessor::Pragma(syntax::PreprocessorPragma {
          command: "some   flag".to_owned()
        })
      ))
    );
  }

  #[test]
  fn parse_pp_undef() {
    assert_eq!(
      preprocessor("# undef \\\n FOO"),
      Ok((
        "",
        syntax::Preprocessor::Undef(syntax::PreprocessorUndef {
          name: syntax::Identifier("FOO".to_owned())
        })
      ))
    );
  }

  #[test]
  fn parse_pp_extension_name() {
    assert_eq!(
      pp_extension_name("all"),
      Ok(("", syntax::PreprocessorExtensionName::All))
    );
    assert_eq!(
      pp_extension_name("GL_foobar_extension "),
      Ok((
        " ",
        syntax::PreprocessorExtensionName::Specific("GL_foobar_extension".to_owned())
      ))
    );
  }

  #[test]
  fn parse_pp_extension_behavior() {
    assert_eq!(
      pp_extension_behavior("require"),
      Ok(("", syntax::PreprocessorExtensionBehavior::Require))
    );
    assert_eq!(
      pp_extension_behavior("enable"),
      Ok(("", syntax::PreprocessorExtensionBehavior::Enable))
    );
    assert_eq!(
      pp_extension_behavior("warn"),
      Ok(("", syntax::PreprocessorExtensionBehavior::Warn))
    );
    assert_eq!(
      pp_extension_behavior("disable"),
      Ok(("", syntax::PreprocessorExtensionBehavior::Disable))
    );
  }

  #[test]
  fn parse_pp_extension() {
    assert_eq!(
      preprocessor("#extension all: require\n"),
      Ok((
        "",
        syntax::Preprocessor::Extension(syntax::PreprocessorExtension {
          name: syntax::PreprocessorExtensionName::All,
          behavior: Some(syntax::PreprocessorExtensionBehavior::Require)
        })
      ))
    );
  }

  #[test]
  fn parse_dot_field_expr_array() {
    let src = "a[0].xyz;";
    let expected = syntax::Expr::Dot(
      Box::new(syntax::Expr::Bracket(
        Box::new(syntax::Expr::Variable("a".into())),
        syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(0))),
      )),
      "xyz".into(),
    );

    assert_eq!(expr(src), Ok((";", expected)));
  }

  #[test]
  fn parse_dot_field_expr_statement() {
    let src = "vec3 v = smoothstep(vec3(border_width), vec3(0.0), v_barycenter).zyx;";
    let fun = syntax::FunIdentifier::Identifier("smoothstep".into());
    let args = vec![
      syntax::Expr::FunCall(
        syntax::FunIdentifier::Identifier("vec3".into()),
        vec![syntax::Expr::Variable("border_width".into())],
      ),
      syntax::Expr::FunCall(
        syntax::FunIdentifier::Identifier("vec3".into()),
        vec![syntax::Expr::DoubleConst(0.)],
      ),
      syntax::Expr::Variable("v_barycenter".into()),
    ];
    let ini = syntax::Initializer::Simple(Box::new(syntax::Expr::Dot(
      Box::new(syntax::Expr::FunCall(fun, args)),
      "zyx".into(),
    )));
    let sd = syntax::SingleDeclaration {
      ty: syntax::FullySpecifiedType {
        qualifier: None,
        ty: syntax::TypeSpecifier {
          ty: syntax::TypeSpecifierNonArray::Vec3,
          array_specifier: None,
        },
      },
      name: Some("v".into()),
      array_specifier: None,
      initializer: Some(ini),
    };
    let expected = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Declaration(
      syntax::Declaration::InitDeclaratorList(syntax::InitDeclaratorList {
        head: sd,
        tail: Vec::new(),
      }),
    )));

    assert_eq!(statement(src), Ok(("", expected)));
  }

  #[test]
  fn parse_arrayed_identifier() {
    let expected = syntax::ArrayedIdentifier::new("foo", syntax::ArraySpecifier::Unsized);

    assert_eq!(arrayed_identifier("foo[]"), Ok(("", expected.clone())));
    assert_eq!(arrayed_identifier("foo \t\n  [\n\t ]"), Ok(("", expected)));
  }

  #[test]
  fn parse_nested_parens() {
    let start = std::time::Instant::now();
    parens_expr("((((((((1.0f))))))))").unwrap();
    let elapsed = start.elapsed();
    assert!(elapsed.as_millis() < 100, "{} ms", elapsed.as_millis());
  }
}
