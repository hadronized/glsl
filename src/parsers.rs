//! GLSL parsers.
//!
//! The more general parser is `translation_unit`, that recognizes the most external form of a GLSL
//! source (a shader, basically).
//!
//! Other parsers are exported if you want more control on how you want to parse your source.

use nom::{Err as NomErr, ErrorKind, IResult, ParseTo, digit, is_hex_digit, is_oct_digit, sp};
use std::str::{from_utf8_unchecked};
use std::num::ParseIntError;

use syntax;

/// Parse a single comment.
named!(pub comment,
  delimited!(sp,
             alt!(
               complete!(preceded!(tag!("//"), take_until!("\n"))) |
               complete!(delimited!(tag!("/*"), take_until!("*/"), tag!("*/"))) |
               sp
             ),
             sp)
);

/// Parse an alphanumeric separator. An alphanumeric separator is a char used to separate
/// alphanumeric tokens. For instance, "in vec3 x" contains three alphanumeric tokens, while
/// "int x = 3, y, z = 12;" contains six alphanumeric tokens ('=', ',' and ';' are separators).
///
/// Whitespace are also considered such separators.
named!(pub alphasep<&[u8], char>, peek!(one_of!(" \t\n,;:.<>{}[]()+-%*/=^?\"'")));

/// Parse a tag followed by an alphanumeric separator.
macro_rules! atag {
  ($i:expr, $s:expr) => {{
    terminated!($i, tag!($s), alphasep)
  }}
}

/// Parse several comments.
named!(pub comments, recognize!(many0!(comment)));

/// Parser rewriter, discarding whitespaces and comments.
macro_rules! bl {
  ($i:expr, $($args:tt)*) => {{
    sep!($i, comment, $($args)*)
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
  bl!(do_parse!(
    name: verify!(take_while1!(identifier_pred), verify_identifier) >>
    (name)
  ))
);

/// Parse a string that could be used as an identifier.
named!(pub string<&[u8], String>, map!(identifier_str, bytes_to_string));

/// Parse an identifier.
named!(pub identifier<&[u8], syntax::Identifier>, map!(identifier_str, |b| syntax::Identifier(bytes_to_string(b))));

/// Parse a type name.
named!(pub type_name<&[u8], syntax::TypeName>, map!(identifier_str, |b| syntax::TypeName(bytes_to_string(b))));

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
  bl!(do_parse!(
    first: identifier >>
    rest: many0!(do_parse!(char!(',') >> i: bl!(identifier) >> (i))) >>

    ({
      let mut identifiers = rest.clone();
      identifiers.insert(0, first);
      identifiers
    })
  ))
);

/// Parse a non-empty list of type names, delimited by comma (,).
named!(nonempty_type_names<&[u8], Vec<syntax::TypeName>>,
  bl!(do_parse!(
    first: type_name >>
    rest: many0!(do_parse!(char!(',') >> i: bl!(type_name) >> (i))) >>

    ({
      let mut names = rest.clone();
      names.insert(0, first);
      names
    })
  ))
);

/// Parse a type specifier non struct.
pub fn type_specifier_non_struct(i: &[u8]) -> IResult<&[u8], syntax::TypeSpecifierNonArray> {
  let (i1, t) = try_parse!(i, identifier_str);

  match unsafe { from_utf8_unchecked(t) } {
    "void" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Void),
    "bool" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Bool),
    "int" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Int),
    "uint" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UInt),
    "float" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Float),
    "double" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Double),
    "vec2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Vec2),
    "vec3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Vec3),
    "vec4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Vec4),
    "dvec2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DVec2),
    "dvec3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DVec3),
    "dvec4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DVec4),
    "bvec2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::BVec2),
    "bvec3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::BVec3),
    "bvec4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::BVec4),
    "ivec2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IVec2),
    "ivec3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IVec3),
    "ivec4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IVec4),
    "uvec2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UVec2),
    "uvec3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UVec3),
    "uvec4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UVec4),
    "mat2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat2),
    "mat3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat3),
    "mat4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat4),
    "mat2x2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat2),
    "mat2x3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat23),
    "mat2x4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat24),
    "mat3x2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat32),
    "mat3x3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat3),
    "mat3x4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat34),
    "mat4x2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat42),
    "mat4x3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat43),
    "mat4x4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Mat4),
    "dmat2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat2),
    "dmat3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat3),
    "dmat4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat4),
    "dmat2x2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat2),
    "dmat2x3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat23),
    "dmat2x4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat24),
    "dmat3x2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat32),
    "dmat3x3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat3),
    "dmat3x4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat34),
    "dmat4x2" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat42),
    "dmat4x3" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat43),
    "dmat4x4" => IResult::Done(i1, syntax::TypeSpecifierNonArray::DMat4),
    "sampler1D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler1D),
    "image1D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image1D),
    "sampler2D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2D),
    "image2D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image2D),
    "sampler3D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler3D),
    "image3D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image3D),
    "samplerCube" => IResult::Done(i1, syntax::TypeSpecifierNonArray::SamplerCube),
    "imageCube" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ImageCube),
    "sampler2DRect" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DRect),
    "image2DRect" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image2DRect),
    "sampler1DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler1DArray),
    "image1DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image1DArray),
    "sampler2DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DArray),
    "image2DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image2DArray),
    "samplerBuffer" => IResult::Done(i1, syntax::TypeSpecifierNonArray::SamplerBuffer),
    "imageBuffer" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ImageBuffer),
    "sampler2DMS" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DMS),
    "image2DMS" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image2DMS),
    "sampler2DMSArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DMSArray),
    "image2DMSArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Image2DMSArray),
    "samplerCubeArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::SamplerCubeArray),
    "imageCubeArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ImageCubeArray),
    "sampler1DShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler1DShadow),
    "sampler2DShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DShadow),
    "sampler2DRectShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DRectShadow),
    "sampler1DArrayShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler1DArrayShadow),
    "sampler2DArrayShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::Sampler2DArrayShadow),
    "samplerCubeShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::SamplerCubeShadow),
    "samplerCubeArrayShadow" => IResult::Done(i1, syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow),
    "isampler1D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler1D),
    "iimage1D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage1D),
    "isampler2D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler2D),
    "iimage2D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage2D),
    "isampler3D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler3D),
    "iimage3D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage3D),
    "isamplerCube" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISamplerCube),
    "iimageCube" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImageCube),
    "isampler2DRect" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler2DRect),
    "iimage2DRect" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage2DRect),
    "isampler1DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler1DArray),
    "iimage1DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage1DArray),
    "isampler2DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler2DArray),
    "iimage2DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage2DArray),
    "isamplerBuffer" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISamplerBuffer),
    "iimageBuffer" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImageBuffer),
    "isampler2DMS" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler2DMS),
    "iimage2DMS" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage2DMS),
    "isampler2DMSArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISampler2DMSArray),
    "iimage2DMSArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImage2DMSArray),
    "isamplerCubeArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::ISamplerCubeArray),
    "iimageCubeArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::IImageCubeArray),
    "atomic_uint" => IResult::Done(i1, syntax::TypeSpecifierNonArray::AtomicUInt),
    "usampler1D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler1D),
    "uimage1D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage1D),
    "usampler2D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler2D),
    "uimage2D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage2D),
    "usampler3D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler3D),
    "uimage3D" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage3D),
    "usamplerCube" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USamplerCube),
    "uimageCube" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImageCube),
    "usampler2DRect" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler2DRect),
    "uimage2DRect" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage2DRect),
    "usampler1DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler1DArray),
    "uimage1DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage1DArray),
    "usampler2DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler2DArray),
    "uimage2DArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage2DArray),
    "usamplerBuffer" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USamplerBuffer),
    "uimageBuffer" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImageBuffer),
    "usampler2DMS" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler2DMS),
    "uimage2DMS" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage2DMS),
    "usampler2DMSArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USampler2DMSArray),
    "uimage2DMSArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImage2DMSArray),
    "usamplerCubeArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::USamplerCubeArray),
    "uimageCubeArray" => IResult::Done(i1, syntax::TypeSpecifierNonArray::UImageCubeArray),
    _ => IResult::Error(NomErr::Code(ErrorKind::AlphaNumeric))
  }
}

/// Parse a type specifier (non-array version).
named!(pub type_specifier_non_array<&[u8], syntax::TypeSpecifierNonArray>,
  alt!(
    type_specifier_non_struct |
    map!(struct_specifier, syntax::TypeSpecifierNonArray::Struct) |
    map!(type_name, syntax::TypeSpecifierNonArray::TypeName)
  )
);

/// Parse a type specifier.
named!(pub type_specifier<&[u8], syntax::TypeSpecifier>,
  map!(ws!(pair!(type_specifier_non_array, opt!(array_specifier))), |(ty, array_specifier)|
    syntax::TypeSpecifier { ty, array_specifier }));

/// Parse the void type.
named!(pub void<&[u8], ()>, value!((), atag!("void")));

/// Parse a digit that precludes a leading 0.
named!(nonzero_digits, verify!(digit, |s:&[u8]| s[0] != b'0'));

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
named!(hexadecimal_lit<Result<u32, ParseIntError>>,
  do_parse!(
    alt!(tag!("0x") | tag!("0X")) >>
    i: verify!(take_while1!(alphanumeric_no_u), all_hexa) >>
    (u32::from_str_radix(bytes_to_str(i), 16))
  )
);

/// Parse an octal literal.
named!(octal_lit<Result<u32, ParseIntError>>,
  do_parse!(
    i: verify!(take_while1!(alphanumeric_no_u), is_octal) >>
    (u32::from_str_radix(bytes_to_str(i), 8))
  )
);

named!(decimal_lit<Result<u32, ParseIntError>>,
  do_parse!(
    i: nonzero_digits >>
    (bytes_to_str(i).parse::<u32>())
  )
);

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
named!(pub integral_lit_try<Result<u32, ParseIntError>>,
  do_parse!(
    sign: opt!(char!('-')) >>
    i: alt!(
      // Match octal first, so that hexadecimal doesn't throw
      // Incomplete on '0'.
      octal_lit |
      hexadecimal_lit |
      decimal_lit
    ) >>
    ({
      if sign.is_some() {
        i.map(|v| -(v as i32) as u32)
      } else {
        i
      }
    })
  )
);

pub fn integral_lit(i: &[u8]) -> IResult<&[u8], i32> {
  match integral_lit_try(i) {
    IResult::Done(i, v) => {
      match v {
        Ok(v) => IResult::Done(i, v as i32),
        _ => IResult::Error(NomErr::Code(ErrorKind::AlphaNumeric)),
      }
    },
    IResult::Error(x) => IResult::Error(x),
    IResult::Incomplete(n) => IResult::Incomplete(n),
  }
}

/// Parse the unsigned suffix.
named!(unsigned_suffix<&[u8], char>, alt!(char!('u') | char!('U')));

/// Parse a literal unsigned string.
named!(pub unsigned_lit<&[u8], u32>,
  do_parse!(
    i: integral_lit >>
    unsigned_suffix >>
    (i as u32)
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
    sign: bl!(opt!(char!('-'))) >>
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
    sign: bl!(opt!(char!('-'))) >>
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
    value!(true, atag!("true")) |
    value!(false, atag!("false"))
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

/// Parse an identifier with an optional array specifier.
named!(arrayed_identifier<&[u8], syntax::ArrayedIdentifier>,
  do_parse!(
    i: identifier >>
    a: opt!(array_specifier) >>
    (syntax::ArrayedIdentifier::new(i, a))
  )
);

/// Parse a struct field declaration.
named!(pub struct_field_specifier<&[u8], syntax::StructFieldSpecifier>,
  bl!(do_parse!(
    qual: opt!(type_qualifier) >>
    ty: type_specifier >>
    identifiers: bl!(do_parse!(
                   first: arrayed_identifier >>
                   rest: many0!(do_parse!(char!(',') >> i: bl!(arrayed_identifier) >> (i))) >>

                   ({
                     let mut identifiers = rest.clone();
                     identifiers.insert(0, first);
                     identifiers
                   })
                 )) >>
    char!(';') >>

    (syntax::StructFieldSpecifier {
      qualifier: qual,
      ty: ty,
      identifiers: syntax::NonEmpty(identifiers)
    })
  ))
);

/// Parse a struct.
named!(pub struct_specifier<&[u8], syntax::StructSpecifier>,
  bl!(do_parse!(
    atag!("struct") >>
    name: opt!(type_name) >>
    fields: delimited!(char!('{'), many1!(struct_field_specifier), char!('}')) >>
    (syntax::StructSpecifier { name: name, fields: syntax::NonEmpty(fields) })
  ))
);

/// Parse a storage qualifier subroutine rule with a list of type names.
named!(pub storage_qualifier_subroutine_list<&[u8], syntax::StorageQualifier>,
  bl!(do_parse!(
    atag!("subroutine") >>
    identifiers: delimited!(char!('('),
                            nonempty_type_names,
                            char!(')')) >>
    (syntax::StorageQualifier::Subroutine(identifiers))
  ))
);

/// Parse a storage qualifier subroutine rule.
named!(pub storage_qualifier_subroutine<&[u8], syntax::StorageQualifier>,
  alt!(
    storage_qualifier_subroutine_list |
    value!(syntax::StorageQualifier::Subroutine(Vec::new()), atag!("subroutine"))
  )
);

/// Parse a storage qualifier.
named!(pub storage_qualifier<&[u8], syntax::StorageQualifier>,
  alt!(
    value!(syntax::StorageQualifier::Const, atag!("const")) |
    value!(syntax::StorageQualifier::InOut, atag!("inout")) |
    value!(syntax::StorageQualifier::In, atag!("in")) |
    value!(syntax::StorageQualifier::Out, atag!("out")) |
    value!(syntax::StorageQualifier::Centroid, atag!("centroid")) |
    value!(syntax::StorageQualifier::Patch, atag!("patch")) |
    value!(syntax::StorageQualifier::Sample, atag!("sample")) |
    value!(syntax::StorageQualifier::Uniform, atag!("uniform")) |
    value!(syntax::StorageQualifier::Buffer, atag!("buffer")) |
    value!(syntax::StorageQualifier::Shared, atag!("shared")) |
    value!(syntax::StorageQualifier::Coherent, atag!("coherent")) |
    value!(syntax::StorageQualifier::Volatile, atag!("volatile")) |
    value!(syntax::StorageQualifier::Restrict, atag!("restrict")) |
    value!(syntax::StorageQualifier::ReadOnly, atag!("readonly")) |
    value!(syntax::StorageQualifier::WriteOnly, atag!("writeonly")) |
    storage_qualifier_subroutine
  )
);

/// Parse a layout qualifier.
named!(pub layout_qualifier<&[u8], syntax::LayoutQualifier>,
  bl!(do_parse!(
    atag!("layout") >>
    x: delimited!(char!('('), layout_qualifier_inner, char!(')')) >>
    (x)
  ))
);

named!(layout_qualifier_inner<&[u8], syntax::LayoutQualifier>,
  bl!(do_parse!(
    first: layout_qualifier_spec >>
    rest: many0!(do_parse!(char!(',') >> x: bl!(layout_qualifier_spec) >> (x))) >>

    ({
      let mut ids = rest.clone();
      ids.insert(0, first);

      syntax::LayoutQualifier { ids: syntax::NonEmpty(ids) }
    })
  ))
);

named!(layout_qualifier_spec<&[u8], syntax::LayoutQualifierSpec>,
  alt!(
    value!(syntax::LayoutQualifierSpec::Shared, atag!("shared")) |
    bl!(do_parse!(
      i: identifier >>
      char!('=') >>
      e: cond_expr >>
      (syntax::LayoutQualifierSpec::Identifier(i, Some(Box::new(e))))
    )) |
    map!(identifier, |i| syntax::LayoutQualifierSpec::Identifier(i, None))
  )
);

/// Parse a precision qualifier.
named!(pub precision_qualifier<&[u8], syntax::PrecisionQualifier>,
  alt!(
    value!(syntax::PrecisionQualifier::High, atag!("highp")) |
    value!(syntax::PrecisionQualifier::Medium, atag!("mediump")) |
    value!(syntax::PrecisionQualifier::Low, atag!("lowp"))
  )
);

/// Parse an interpolation qualifier.
named!(pub interpolation_qualifier<&[u8], syntax::InterpolationQualifier>,
  alt!(
    value!(syntax::InterpolationQualifier::Smooth, atag!("smooth")) |
    value!(syntax::InterpolationQualifier::Flat, atag!("flat")) |
    value!(syntax::InterpolationQualifier::NoPerspective, atag!("noperspective"))
  )
);

/// Parse an invariant qualifier.
named!(pub invariant_qualifier<&[u8], ()>,
  value!((), atag!("invariant")));

/// Parse a precise qualifier.
named!(pub precise_qualifier<&[u8], ()>,
  value!((), atag!("precise")));

/// Parse a type qualifier.
named!(pub type_qualifier<&[u8], syntax::TypeQualifier>,
  do_parse!(
    qualifiers: many1!(bl!(type_qualifier_spec)) >>
    (syntax::TypeQualifier { qualifiers: syntax::NonEmpty(qualifiers) })
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
  bl!(do_parse!(
    qualifier: opt!(type_qualifier) >>
    ty: type_specifier >>

    (syntax::FullySpecifiedType { qualifier: qualifier, ty: ty })
  ))
);

/// Parse an array specifier with no size information.
named!(pub array_specifier<&[u8], syntax::ArraySpecifier>,
  alt!(
    bl!(do_parse!(char!('[') >> char!(']') >> (syntax::ArraySpecifier::Unsized))) |
    bl!(do_parse!(char!('[') >> e: cond_expr >> char!(']') >> (syntax::ArraySpecifier::ExplicitlySized(Box::new(e)))))
  )
);

/// Parse a primary expression.
named!(pub primary_expr<&[u8], syntax::Expr>,
  alt!(
    parens_expr |
    map!(double_lit, syntax::Expr::DoubleConst) |
    map!(float_lit, syntax::Expr::FloatConst) |
    map!(unsigned_lit, syntax::Expr::UIntConst) |
    map!(integral_lit, syntax::Expr::IntConst) |
    map!(bool_lit, syntax::Expr::BoolConst) |
    map!(identifier, syntax::Expr::Variable)
  )
);

/// Parse a postfix expression.
named!(pub postfix_expr<&[u8], syntax::Expr>,
  do_parse!(
    e: alt!(function_call | primary_expr) >>
    pfe: call!(postfix_part, e) >>
    (pfe)
  )
);

// Parse the postfix part of a primary expression. This function will just parse the until it cannot
// find any more postfix construct.
fn postfix_part(i: &[u8], e: syntax::Expr) -> IResult<&[u8], syntax::Expr> {
  let r = alt!(i,
    map!(array_specifier, |a| syntax::Expr::Bracket(Box::new(e.clone()), a)) |
    map!(dot_field_selection, |i| syntax::Expr::Dot(Box::new(e.clone()), i)) |
    value!(syntax::Expr::PostInc(Box::new(e.clone())), tag!("++")) |
    value!(syntax::Expr::PostDec(Box::new(e.clone())), tag!("--"))
  );

  match r {
    IResult::Done(i1, e1) => postfix_part(i1, e1),
    IResult::Error(_) => IResult::Done(i, e),
    _ => r
  }
}

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
named!(pub parens_expr<&[u8], syntax::Expr>, bl!(delimited!(char!('('), bl!(expr), char!(')'))));

/// Parse a dot field selection identifier.
named!(pub dot_field_selection<&[u8], syntax::Identifier>, preceded!(char!('.'), identifier));

/// Parse a declaration.
named!(pub declaration<&[u8], syntax::Declaration>,
  alt!(
    map!(terminated!(function_prototype, char!(';')), syntax::Declaration::FunctionPrototype) |
    map!(terminated!(init_declarator_list, char!(';')), syntax::Declaration::InitDeclaratorList) |
    precision_declaration |
    block_declaration |
    global_declaration
  )
);

/// Parse a precision declaration.
named!(pub precision_declaration<&[u8], syntax::Declaration>,
  bl!(do_parse!(
    atag!("precision") >>
    qual: precision_qualifier >>
    ty: type_specifier >>
    char!(';') >>

    (syntax::Declaration::Precision(qual, ty))
  ))
);

/// Parse a block declaration.
named!(pub block_declaration<&[u8], syntax::Declaration>,
  bl!(do_parse!(
    qual: type_qualifier >>
    name: identifier >>
    char!('{') >>
    fields: many1!(struct_field_specifier) >>
    char!('}') >>
    a: alt!(
         value!(None, char!(';')) |
         bl!(do_parse!(
           a: opt!(arrayed_identifier) >>
           char!(';') >>
           (a)
         ))
       ) >>

    (syntax::Declaration::Block(
      syntax::Block {
        qualifier: qual,
        name,
        fields,
        identifier: a
      }
    ))
  ))
);

/// Parse a global declaration.
named!(pub global_declaration<&[u8], syntax::Declaration>,
  bl!(do_parse!(
    qual: type_qualifier >>
    identifiers: many0!(bl!(do_parse!(char!(',') >> i: identifier >> (i)))) >>
    (syntax::Declaration::Global(qual, identifiers))
  ))
);

/// Parse a function prototype.
named!(pub function_prototype<&[u8], syntax::FunctionPrototype>,
  bl!(do_parse!(
    fp: function_declarator >>
    char!(')') >>
    (fp)
  ))
);

/// Parse an init declarator list.
named!(pub init_declarator_list<&[u8], syntax::InitDeclaratorList>,
  bl!(do_parse!(
    first: single_declaration >>
    rest: many0!(bl!(do_parse!(
            char!(',') >>
            name: identifier >>
            arr_spec: opt!(array_specifier) >>
            init: opt!(preceded!(char!('='), initializer)) >>
            (syntax::SingleDeclarationNoType {
              ident: syntax::ArrayedIdentifier::new(name, arr_spec),
              initializer: init
            })
          ))) >>
    (syntax::InitDeclaratorList {
      head: first,
      tail: rest
    })
  ))
);


/// Parse a single declaration.
named!(pub single_declaration<&[u8], syntax::SingleDeclaration>,
  bl!(do_parse!(
    ty: fully_specified_type >>
    a: alt!(
         bl!(do_parse!(
           name: identifier >>
           arr_spec: opt!(array_specifier) >>
           init: opt!(preceded!(char!('='), initializer)) >>
           (syntax::SingleDeclaration {
             ty: ty.clone(),
             name: Some(name),
             array_specifier: arr_spec,
             initializer: init
           })
         )) |

         value!(syntax::SingleDeclaration {
           ty: ty,
           name: None,
           array_specifier: None,
           initializer: None
         })
       ) >>
    (a)
  ))
);

/// Parse an initializer.
named!(pub initializer<&[u8], syntax::Initializer>,
  alt!(
    map!(assignment_expr, |e| syntax::Initializer::Simple(Box::new(e))) |
    bl!(do_parse!(
      char!('{') >>
      il: initializer_list >>
      opt!(char!(',')) >>
      char!('}') >>

      (syntax::Initializer::List(syntax::NonEmpty(il)))
    ))
  )
);

/// Parse an initializer list.
named!(pub initializer_list<&[u8], Vec<syntax::Initializer>>,
  bl!(do_parse!(
    first: initializer >>
    rest: many0!(bl!(do_parse!(char!(',') >> ini: initializer >> (ini)))) >>

    ({
      let mut inis = rest.clone();
      inis.insert(0, first);
      (inis)
    })
  ))
);

named!(function_declarator<&[u8], syntax::FunctionPrototype>,
  alt!(
    function_header_with_parameters |
    map!(function_header, |(ret_ty, fun_name)| syntax::FunctionPrototype { ty: ret_ty, name: fun_name, parameters: Vec::new() })
  )
);

named!(function_header<&[u8], (syntax::FullySpecifiedType, syntax::Identifier)>,
  bl!(do_parse!(
    ret_ty: fully_specified_type >>
    fun_name: identifier >>
    char!('(') >>
    (ret_ty, fun_name)
  ))
);

named!(function_header_with_parameters<&[u8], syntax::FunctionPrototype>,
  bl!(do_parse!(
    header: function_header >>
    first_param: function_parameter_declaration >>
    rest_params: many0!(bl!(do_parse!(char!(',') >> param: function_parameter_declaration >> (param)))) >>

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
  bl!(do_parse!(
    ty_qual: opt!(type_qualifier) >>
    fpd: function_parameter_declarator >>
    (syntax::FunctionParameterDeclaration::Named(ty_qual, fpd))
  ))
);

named!(function_parameter_declaration_unnamed<&[u8], syntax::FunctionParameterDeclaration>,
  bl!(do_parse!(
    ty_qual: opt!(type_qualifier) >>
    ty_spec: type_specifier >>
    (syntax::FunctionParameterDeclaration::Unnamed(ty_qual, ty_spec))
  ))
);

named!(function_parameter_declarator<&[u8], syntax::FunctionParameterDeclarator>,
  bl!(do_parse!(
    ty: type_specifier >>
    name: identifier >>
    a: opt!(array_specifier) >>
    (syntax::FunctionParameterDeclarator {
      ty,
      ident: syntax::ArrayedIdentifier::new(name, a)
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
  bl!(do_parse!(
    fi: function_call_header >>
    opt!(void) >>
    char!(')') >>

    (syntax::Expr::FunCall(fi, Vec::new()))
  ))
);

named!(function_call_header_with_parameters<&[u8], syntax::Expr>,
  bl!(do_parse!(
    fi: function_call_header >>
    first_arg: assignment_expr >>
    rest_args: many0!(bl!(do_parse!(char!(',') >> arg: assignment_expr >> (arg)))) >>
    char!(')') >>

    ({
      let mut args = rest_args.clone();
      args.insert(0, first_arg);
      syntax::Expr::FunCall(fi, args)
    })
  ))
);

named!(function_call_header<&[u8], syntax::FunIdentifier>,
  bl!(do_parse!(
    fi: function_identifier >>
    char!('(') >>
    (fi)
  ))
);

/// Parse a function identifier just behind a function list argument.
named!(pub function_identifier<&[u8], syntax::FunIdentifier>,
  alt!(
    do_parse!(
      i: identifier >>
      peek!(char!('(')) >>
      (syntax::FunIdentifier::Identifier(i))
    ) |

    do_parse!(
      e: primary_expr >>
      pfe: call!(postfix_part, e) >>
      (syntax::FunIdentifier::Expr(Box::new(pfe)))
    )
  )
);

/// Parse the most general expression.
named!(pub expr<&[u8], syntax::Expr>,
  bl!(do_parse!(
    first: assignment_expr >>
    a: alt!(
         bl!(do_parse!(
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
    bl!(do_parse!(
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
  bl!(do_parse!(
    a: logical_or_expr >>
    e: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: logical_xor_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: logical_and_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: inclusive_or_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: exclusive_or_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: and_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: equality_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: rel_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: shift_expr >>
    n: alt!(
         bl!(do_parse!(
           op: alt!(
                 value!(syntax::BinaryOp::LTE, tag!("<=")) |
                 value!(syntax::BinaryOp::GTE, tag!(">=")) |
                 value!(syntax::BinaryOp::LT, char!('<')) |
                 value!(syntax::BinaryOp::GT, char!('>'))
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
  bl!(do_parse!(
    a: additive_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: multiplicative_expr >>
    n: alt!(
         bl!(do_parse!(
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
  bl!(do_parse!(
    a: unary_expr >>
    n: alt!(
         bl!(do_parse!(
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
named!(pub simple_statement<&[u8], syntax::SimpleStatement>,
  alt!(
    map!(jump_statement, syntax::SimpleStatement::Jump) |
    map!(iteration_statement, syntax::SimpleStatement::Iteration) |
    map!(case_label, syntax::SimpleStatement::CaseLabel) |
    map!(switch_statement, syntax::SimpleStatement::Switch) |
    map!(selection_statement, syntax::SimpleStatement::Selection) |
    map!(declaration, syntax::SimpleStatement::Declaration) |
    map!(expr_statement, syntax::SimpleStatement::Expression)
  )
);

/// Parse an expression statement.
named!(pub expr_statement<&[u8], syntax::ExprStatement>,
  bl!(do_parse!(
    e: opt!(expr) >>
    char!(';') >>
    (e)
  ))
);

/// Parse a selection statement.
named!(pub selection_statement<&[u8], syntax::SelectionStatement>,
  bl!(do_parse!(
    atag!("if") >>
    char!('(') >>
    cond_expr: expr >>
    char!(')') >>
    srs: selection_rest_statement >>
    (syntax::SelectionStatement {
      cond: Box::new(cond_expr),
      rest: srs
    })
  ))
);

named!(selection_rest_statement<&[u8], syntax::SelectionRestStatement>,
  bl!(do_parse!(
    st: statement >>
    r: alt!(
         bl!(do_parse!(
           atag!("else") >>
           rest: statement >>
           (syntax::SelectionRestStatement::Else(Box::new(st.clone()), Box::new(rest)))
         )) |

         value!(syntax::SelectionRestStatement::Statement(Box::new(st)))
       ) >>
    (r)
  ))
);

/// Parse a switch statement.
named!(pub switch_statement<&[u8], syntax::SwitchStatement>,
  bl!(do_parse!(
    atag!("switch") >>
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
named!(pub case_label<&[u8], syntax::CaseLabel>,
  alt!(
    bl!(do_parse!(
      atag!("case") >>
      e: expr >>
      char!(':') >>
      (syntax::CaseLabel::Case(Box::new(e)))
    )) |
    bl!(do_parse!(
      atag!("default") >>
      char!(':') >>
      (syntax::CaseLabel::Def)
    ))
  )
);

/// Parse an iteration statement.
named!(pub iteration_statement<&[u8], syntax::IterationStatement>,
  alt!(
    iteration_statement_while |
    iteration_statement_do_while |
    iteration_statement_for
  )
);

named!(pub iteration_statement_while<&[u8], syntax::IterationStatement>,
  bl!(do_parse!(
    atag!("while") >>
    char!('(') >>
    cond: condition >>
    char!(')') >>
    st: statement >>
    (syntax::IterationStatement::While(cond, Box::new(st)))
  ))
);

named!(pub iteration_statement_do_while<&[u8], syntax::IterationStatement>,
  bl!(do_parse!(
    atag!("do") >>
    st: statement >>
    atag!("while") >>
    char!('(') >>
    e: expr >>
    char!(')') >>
    char!(';') >>
    (syntax::IterationStatement::DoWhile(Box::new(st), Box::new(e)))
  ))
);

named!(pub iteration_statement_for<&[u8], syntax::IterationStatement>,
  bl!(do_parse!(
    atag!("for") >>
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
  bl!(do_parse!(
    cond: opt!(condition) >>
    char!(';') >>
    e: opt!(expr) >>
    (syntax::ForRestStatement { condition: cond, post_expr: e.map(Box::new) })
  ))
);

/// Parse a jump statement.
named!(pub jump_statement<&[u8], syntax::JumpStatement>,
  alt!(
    jump_statement_continue |
    jump_statement_break |
    jump_statement_return |
    jump_statement_discard
  )
);

named!(pub jump_statement_continue<&[u8], syntax::JumpStatement>,
  bl!(do_parse!(atag!("continue") >> char!(';') >> (syntax::JumpStatement::Continue)))
);

named!(pub jump_statement_break<&[u8], syntax::JumpStatement>,
  bl!(do_parse!(atag!("break") >> char!(';') >> (syntax::JumpStatement::Break)))
);

named!(pub jump_statement_discard<&[u8], syntax::JumpStatement>,
  bl!(do_parse!(atag!("discard") >> char!(';') >> (syntax::JumpStatement::Discard)))
);

named!(pub jump_statement_return<&[u8], syntax::JumpStatement>,
  bl!(do_parse!(
    atag!("return") >>
    e: expr >>
    char!(';') >>
    (syntax::JumpStatement::Return(Box::new(e)))
  ))
);

/// Parse a condition.
named!(pub condition<&[u8], syntax::Condition>,
  alt!(
    map!(expr, |e| syntax::Condition::Expr(Box::new(e))) |
    condition_assignment
  )
);

named!(condition_assignment<&[u8], syntax::Condition>,
  bl!(do_parse!(
    ty: fully_specified_type >>
    id: identifier >>
    char!('=') >>
    ini: initializer >>
    (syntax::Condition::Assignment(ty, id, ini))
  ))
);

/// Parse a statement.
named!(pub statement<&[u8], syntax::Statement>,
  alt!(
    map!(compound_statement, |c| syntax::Statement::Compound(Box::new(c))) |
    map!(simple_statement, |s| syntax::Statement::Simple(Box::new(s)))
  )
);

/// Parse a compound statement.
named!(pub compound_statement<&[u8], syntax::CompoundStatement>,
  bl!(do_parse!(
    char!('{') >>
    stl: many0!(statement) >>
    char!('}') >>
    (syntax::CompoundStatement { statement_list: stl })
  ))
);

/// Parse a function definition.
named!(pub function_definition<&[u8], syntax::FunctionDefinition>,
  bl!(do_parse!(
    prototype: function_prototype >>
    st: compound_statement >>
    (syntax::FunctionDefinition { prototype: prototype, statement: st })
  ))
);

/// Parse an external declaration.
named!(pub external_declaration<&[u8], syntax::ExternalDeclaration>,
  alt!(
    map!(preprocessor, syntax::ExternalDeclaration::Preprocessor) |
    map!(function_definition, syntax::ExternalDeclaration::FunctionDefinition) |
    map!(declaration, syntax::ExternalDeclaration::Declaration)
  )
);

/// Parse a translation unit (entry point).
named!(pub translation_unit<&[u8], syntax::TranslationUnit>,
  map!(many1!(external_declaration), |v| syntax::TranslationUnit(syntax::NonEmpty(v)))
);

/// Parse a preprocessor command.
named!(pub preprocessor<&[u8], syntax::Preprocessor>,
  bl!(alt!(
    map!(pp_define, syntax::Preprocessor::Define) |
    map!(pp_version, syntax::Preprocessor::Version) |
    map!(pp_extension, syntax::Preprocessor::Extension)
  ))
);

/// Parse a #version number.
named!(pub pp_version_number<&[u8], u16>,
  map!(digit, |i| i.parse_to().unwrap())
);

/// Parse a #version profile.
named!(pub pp_version_profile<&[u8], syntax::PreprocessorVersionProfile>,
  alt!(
    value!(syntax::PreprocessorVersionProfile::Core, tag!("core")) |
    value!(syntax::PreprocessorVersionProfile::Compatibility, tag!("compatibility")) |
    value!(syntax::PreprocessorVersionProfile::ES, tag!("es"))
  )
);

named!(ppws_, eat_separator!(" \t"));

// Eating separator in preprocessor lines.
macro_rules! ppws {
  ($i:expr, $($args:tt)*) => {{
    sep!($i, ppws_, $($args)*)
  }}
}

/// Parse a #define
named!(pub pp_define<&[u8], syntax::PreprocessorDefine>,
  ppws!(do_parse!(
    char!('#') >>
      tag!("define") >>
      name: identifier >>
      value: primary_expr>>
      char!('\n') >>

      (syntax::PreprocessorDefine {
        name: name,
        value: value
      })
  ))
);


/// Parse a #version.
named!(pub pp_version<&[u8], syntax::PreprocessorVersion>,
  ppws!(do_parse!(
    char!('#') >>
    tag!("version") >>
    version: pp_version_number >>
    profile: opt!(pp_version_profile) >>
    char!('\n') >>

    (syntax::PreprocessorVersion {
      version: version as u16,
      profile
    })
  ))
);

/// Parse an #extension name.
named!(pub pp_extension_name<&[u8], syntax::PreprocessorExtensionName>,
  alt!(
    value!(syntax::PreprocessorExtensionName::All, tag!("all")) |
    map!(string, syntax::PreprocessorExtensionName::Specific)
  )
);

/// Parse an #extension behavior.
named!(pub pp_extension_behavior<&[u8], syntax::PreprocessorExtensionBehavior>,
  alt!(
    value!(syntax::PreprocessorExtensionBehavior::Require, tag!("require")) |
    value!(syntax::PreprocessorExtensionBehavior::Enable, tag!("enable")) |
    value!(syntax::PreprocessorExtensionBehavior::Warn, tag!("warn")) |
    value!(syntax::PreprocessorExtensionBehavior::Disable, tag!("disable"))
  )
);

/// Parse an #extension.
named!(pub pp_extension<&[u8], syntax::PreprocessorExtension>,
  ppws!(do_parse!(
    char!('#') >>
    tag!("extension") >>
    name: pp_extension_name >>
    behavior: opt!(ppws!(preceded!(char!(':'), pp_extension_behavior))) >>
    char!('\n') >>

    (syntax::PreprocessorExtension { name, behavior })
  ))
);

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_uniline_comment() {
    assert_eq!(comment(&b"// lol\nfoo"[..]), IResult::Done(&b"foo"[..], &b" lol"[..]));
  }

  #[test]
  fn parse_multiline_comment() {
    assert_eq!(comment(&b"/* lol\nfoo\n*/bar"[..]), IResult::Done(&b"bar"[..], &b" lol\nfoo\n"[..]));
  }

  #[test]
  fn parse_unsigned_suffix() {
    assert_eq!(unsigned_suffix(&b"u"[..]), IResult::Done(&b""[..], 'u'));
    assert_eq!(unsigned_suffix(&b"U"[..]), IResult::Done(&b""[..], 'U'));
  }
  
  #[test]
  fn parse_nonzero_digits() {
    assert_eq!(nonzero_digits(&b"3"[..]), IResult::Done(&b""[..], &b"3"[..]));
    assert_eq!(nonzero_digits(&b"12345953"[..]), IResult::Done(&b""[..], &b"12345953"[..]));
  }

  #[test]
  fn parse_decimal_lit() {
    assert_eq!(decimal_lit(&b"3"[..]), IResult::Done(&b""[..], Ok(3)));
    assert_eq!(decimal_lit(&b"3 "[..]), IResult::Done(&b" "[..], Ok(3)));
    assert_eq!(decimal_lit(&b"13 "[..]), IResult::Done(&b" "[..], Ok(13)));
    assert_eq!(decimal_lit(&b"42 "[..]), IResult::Done(&b" "[..], Ok(42)));
    assert_eq!(decimal_lit(&b"123456 "[..]), IResult::Done(&b" "[..], Ok(123456)));
  }

  #[test]
  fn parse_octal_lit() {
    assert_eq!(octal_lit(&b"0 "[..]), IResult::Done(&b" "[..], Ok(0o0)));
    assert_eq!(octal_lit(&b"03 "[..]), IResult::Done(&b" "[..], Ok(0o3)));
    assert_eq!(octal_lit(&b"012 "[..]), IResult::Done(&b" "[..], Ok(0o12)));
    assert_eq!(octal_lit(&b"07654321 "[..]), IResult::Done(&b" "[..], Ok(0o7654321)));
  }

  #[test]
  fn parse_hexadecimal_lit() {
    assert_eq!(hexadecimal_lit(&b"0x3 "[..]), IResult::Done(&b" "[..], Ok(0x3)));
    assert_eq!(hexadecimal_lit(&b"0x0123789"[..]), IResult::Done(&b""[..], Ok(0x0123789)));
    assert_eq!(hexadecimal_lit(&b"0xABCDEF"[..]), IResult::Done(&b""[..], Ok(0xabcdef)));
    assert_eq!(hexadecimal_lit(&b"0xabcdef"[..]), IResult::Done(&b""[..], Ok(0xabcdef)));
  }

  #[test]
  fn parse_integral_lit() {
    assert_eq!(integral_lit(&b"0"[..]), IResult::Done(&b""[..], 0));
    assert_eq!(integral_lit(&b"3"[..]), IResult::Done(&b""[..], 3));
    assert_eq!(integral_lit(&b"3 "[..]), IResult::Done(&b" "[..], 3));
    assert_eq!(integral_lit(&b"03 "[..]), IResult::Done(&b" "[..], 3));
    assert_eq!(integral_lit(&b"076556 "[..]), IResult::Done(&b" "[..], 0o76556)); 
    assert_eq!(integral_lit(&b"012 "[..]), IResult::Done(&b" "[..], 0o12));
    assert_eq!(integral_lit(&b"0x3 "[..]), IResult::Done(&b" "[..], 0x3));
    assert_eq!(integral_lit(&b"0x9ABCDEF"[..]), IResult::Done(&b""[..], 0x9ABCDEF));
    assert_eq!(integral_lit(&b"0x9ABCDEF"[..]), IResult::Done(&b""[..], 0x9ABCDEF));
    assert_eq!(integral_lit(&b"0x9abcdef"[..]), IResult::Done(&b""[..], 0x9abcdef));
    assert_eq!(integral_lit(&b"0x9abcdef"[..]), IResult::Done(&b""[..], 0x9abcdef));
    assert!(integral_lit(&b"\n1"[..]).is_err());
    assert!(integral_lit(&b"\n0x1"[..]).is_err());
    assert!(integral_lit(&b"\n01"[..]).is_err());
    assert!(integral_lit(&b"0xfffffffff"[..]).is_err());
    assert_eq!(integral_lit(&b"0xffffffff"[..]), IResult::Done(&b""[..], 0xffffffffu32 as i32));
  }
  
  #[test]
  fn parse_integral_neg_lit() {
    assert_eq!(integral_lit(&b"-3"[..]), IResult::Done(&b""[..], -3));
    assert_eq!(integral_lit(&b"-3 "[..]), IResult::Done(&b" "[..], -3));
    assert_eq!(integral_lit(&b"-03 "[..]), IResult::Done(&b" "[..], -3));
    assert_eq!(integral_lit(&b"-076556 "[..]), IResult::Done(&b" "[..], -0o76556)); 
    assert_eq!(integral_lit(&b"-012 "[..]), IResult::Done(&b" "[..], -0o12));
    assert_eq!(integral_lit(&b"-0x3 "[..]), IResult::Done(&b" "[..], -0x3));
    assert_eq!(integral_lit(&b"-0x9ABCDEF"[..]), IResult::Done(&b""[..], -0x9ABCDEF));
    assert_eq!(integral_lit(&b"-0x9ABCDEF"[..]), IResult::Done(&b""[..], -0x9ABCDEF));
    assert_eq!(integral_lit(&b"-0x9abcdef"[..]), IResult::Done(&b""[..], -0x9abcdef));
    assert_eq!(integral_lit(&b"-0x9abcdef"[..]), IResult::Done(&b""[..], -0x9abcdef));
  }
  
  #[test]
  fn parse_unsigned_lit() {
    assert_eq!(unsigned_lit(&b"0xffffffffU"[..]), IResult::Done(&b""[..], 0xffffffff as u32));
    assert_eq!(unsigned_lit(&b"-1u"[..]), IResult::Done(&b""[..], 0xffffffff as u32));
    assert!(unsigned_lit(&b"0xfffffffffU"[..]).is_err());
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
    assert_eq!(identifier(&b"a"[..]), IResult::Done(&b""[..], "a".into()));
    assert_eq!(identifier(&b"ab_cd"[..]), IResult::Done(&b""[..], "ab_cd".into()));
    assert_eq!(identifier(&b"Ab_cd"[..]), IResult::Done(&b""[..], "Ab_cd".into()));
    assert_eq!(identifier(&b"Ab_c8d"[..]), IResult::Done(&b""[..], "Ab_c8d".into()));
    assert_eq!(identifier(&b"Ab_c8d9"[..]), IResult::Done(&b""[..], "Ab_c8d9".into()));
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
    assert_eq!(array_specifier(&b"[\n0   \t]"[..]), IResult::Done(&b""[..], syntax::ArraySpecifier::ExplicitlySized(Box::new(ix))));
  }

  #[test]
  fn parse_precise_qualifier() {
    assert_eq!(precise_qualifier(&b"precise "[..]), IResult::Done(&b" "[..], ()));
  }
  
  #[test]
  fn parse_invariant_qualifier() {
    assert_eq!(invariant_qualifier(&b"invariant "[..]), IResult::Done(&b" "[..], ()));
  }
  
  #[test]
  fn parse_interpolation_qualifier() {
    assert_eq!(interpolation_qualifier(&b"smooth "[..]), IResult::Done(&b" "[..], syntax::InterpolationQualifier::Smooth));
    assert_eq!(interpolation_qualifier(&b"flat "[..]), IResult::Done(&b" "[..], syntax::InterpolationQualifier::Flat));
    assert_eq!(interpolation_qualifier(&b"noperspective "[..]), IResult::Done(&b" "[..], syntax::InterpolationQualifier::NoPerspective));
  }
  
  #[test]
  fn parse_precision_qualifier() {
    assert_eq!(precision_qualifier(&b"highp "[..]), IResult::Done(&b" "[..], syntax::PrecisionQualifier::High));
    assert_eq!(precision_qualifier(&b"mediump "[..]), IResult::Done(&b" "[..], syntax::PrecisionQualifier::Medium));
    assert_eq!(precision_qualifier(&b"lowp "[..]), IResult::Done(&b" "[..], syntax::PrecisionQualifier::Low));
  }
  
  #[test]
  fn parse_storage_qualifier() {
    assert_eq!(storage_qualifier(&b"const "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Const));
    assert_eq!(storage_qualifier(&b"inout "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::InOut));
    assert_eq!(storage_qualifier(&b"in "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::In));
    assert_eq!(storage_qualifier(&b"out "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Out));
    assert_eq!(storage_qualifier(&b"centroid "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Centroid));
    assert_eq!(storage_qualifier(&b"patch "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Patch));
    assert_eq!(storage_qualifier(&b"sample "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Sample));
    assert_eq!(storage_qualifier(&b"uniform "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Uniform));
    assert_eq!(storage_qualifier(&b"buffer "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Buffer));
    assert_eq!(storage_qualifier(&b"shared "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Shared));
    assert_eq!(storage_qualifier(&b"coherent "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Coherent));
    assert_eq!(storage_qualifier(&b"volatile "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Volatile));
    assert_eq!(storage_qualifier(&b"restrict "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::Restrict));
    assert_eq!(storage_qualifier(&b"readonly "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::ReadOnly));
    assert_eq!(storage_qualifier(&b"writeonly "[..]), IResult::Done(&b" "[..], syntax::StorageQualifier::WriteOnly));
    assert_eq!(storage_qualifier(&b"subroutine a"[..]), IResult::Done(&b" a"[..], syntax::StorageQualifier::Subroutine(vec![])));
  
    let a = syntax::TypeName("vec3".to_owned());
    let b = syntax::TypeName("float".to_owned());
    let c = syntax::TypeName("dmat43".to_owned());
    let types = vec![a, b, c];
    assert_eq!(storage_qualifier(&b"subroutine (vec3, float, dmat43)"[..]), IResult::Done(&b""[..], syntax::StorageQualifier::Subroutine(types)));
  }
  
  #[test]
  fn parse_layout_qualifier_std430() {
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Identifier("std430".into(), None)])
    };
  
    assert_eq!(layout_qualifier(&b"layout (std430)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b" layout  (std430   )"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b" layout \n\t (  std430  )"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b" layout(std430)"[..]), IResult::Done(&b""[..], expected));
  }
  
  #[test]
  fn parse_layout_qualifier_shared() {
    let expected = syntax::LayoutQualifier {
      ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Shared])
    };
  
    assert_eq!(layout_qualifier(&b"layout (shared)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"   layout ( shared )"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"   layout(shared)"[..]), IResult::Done(&b""[..], expected));
  }
  
  #[test]
  fn parse_layout_qualifier_list() {
    let id_0 = syntax::LayoutQualifierSpec::Shared;
    let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
    let id_2 = syntax::LayoutQualifierSpec::Identifier("max_vertices".into(), Some(Box::new(syntax::Expr::IntConst(3))));
    let expected = syntax::LayoutQualifier { ids: syntax::NonEmpty(vec![id_0, id_1, id_2]) };
  
    assert_eq!(layout_qualifier(&b"layout (shared, std140, max_vertices = 3)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"layout(shared,std140,max_vertices=3)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(layout_qualifier(&b"   layout\n\n\t (    shared , std140, max_vertices= 3)"[..]), IResult::Done(&b""[..], expected.clone()));
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
  
    assert_eq!(type_qualifier(&b"const layout (shared, std140, max_vertices = 3)"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(type_qualifier(&b"    const layout(shared,std140,max_vertices=3)"[..]), IResult::Done(&b""[..], expected));
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
  
    assert_eq!(struct_field_specifier(&b"vec4 foo;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_field_specifier(&b"  vec4     foo ; "[..]), IResult::Done(&b""[..], expected.clone()));
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
  
    assert_eq!(struct_field_specifier(&b"S0238_3 x;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_field_specifier(&b"  S0238_3     x ; "[..]), IResult::Done(&b""[..], expected.clone()));
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
  
    assert_eq!(struct_field_specifier(&b"vec4 foo, bar, zoo;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_field_specifier(&b"  vec4     foo , bar  , zoo ; "[..]), IResult::Done(&b""[..], expected.clone()));
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
  
    assert_eq!(struct_specifier(&b"struct TestStruct { vec4 foo; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_specifier(&b"   struct      TestStruct \n \n\n {\n    vec4   foo  ;\n }"[..]), IResult::Done(&b""[..], expected));
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
  
    assert_eq!(struct_specifier(&b"struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_specifier(&b"struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(struct_specifier(&b"   struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_type_specifier_non_array() {
    assert_eq!(type_specifier_non_array(&b"bool"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Bool));
    assert_eq!(type_specifier_non_array(&b"int"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Int));
    assert_eq!(type_specifier_non_array(&b"uint"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UInt));
    assert_eq!(type_specifier_non_array(&b"float"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Float));
    assert_eq!(type_specifier_non_array(&b"double"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Double));
    assert_eq!(type_specifier_non_array(&b"vec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Vec2));
    assert_eq!(type_specifier_non_array(&b"vec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Vec3));
    assert_eq!(type_specifier_non_array(&b"vec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Vec4));
    assert_eq!(type_specifier_non_array(&b"dvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DVec2));
    assert_eq!(type_specifier_non_array(&b"dvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DVec3));
    assert_eq!(type_specifier_non_array(&b"dvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DVec4));
    assert_eq!(type_specifier_non_array(&b"bvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::BVec2));
    assert_eq!(type_specifier_non_array(&b"bvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::BVec3));
    assert_eq!(type_specifier_non_array(&b"bvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::BVec4));
    assert_eq!(type_specifier_non_array(&b"ivec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IVec2));
    assert_eq!(type_specifier_non_array(&b"ivec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IVec3));
    assert_eq!(type_specifier_non_array(&b"ivec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IVec4));
    assert_eq!(type_specifier_non_array(&b"uvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UVec2));
    assert_eq!(type_specifier_non_array(&b"uvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UVec3));
    assert_eq!(type_specifier_non_array(&b"uvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UVec4));
    assert_eq!(type_specifier_non_array(&b"mat2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat2));
    assert_eq!(type_specifier_non_array(&b"mat3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat3));
    assert_eq!(type_specifier_non_array(&b"mat4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat4));
    assert_eq!(type_specifier_non_array(&b"mat2x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat2));
    assert_eq!(type_specifier_non_array(&b"mat2x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat23));
    assert_eq!(type_specifier_non_array(&b"mat2x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat24));
    assert_eq!(type_specifier_non_array(&b"mat3x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat32));
    assert_eq!(type_specifier_non_array(&b"mat3x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat3));
    assert_eq!(type_specifier_non_array(&b"mat3x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat34));
    assert_eq!(type_specifier_non_array(&b"mat4x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat42));
    assert_eq!(type_specifier_non_array(&b"mat4x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat43));
    assert_eq!(type_specifier_non_array(&b"mat4x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Mat4));
    assert_eq!(type_specifier_non_array(&b"dmat2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat2));
    assert_eq!(type_specifier_non_array(&b"dmat3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat3));
    assert_eq!(type_specifier_non_array(&b"dmat4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat4));
    assert_eq!(type_specifier_non_array(&b"dmat2x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat2));
    assert_eq!(type_specifier_non_array(&b"dmat2x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat23));
    assert_eq!(type_specifier_non_array(&b"dmat2x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat24));
    assert_eq!(type_specifier_non_array(&b"dmat3x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat32));
    assert_eq!(type_specifier_non_array(&b"dmat3x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat3));
    assert_eq!(type_specifier_non_array(&b"dmat3x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat34));
    assert_eq!(type_specifier_non_array(&b"dmat4x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat42));
    assert_eq!(type_specifier_non_array(&b"dmat4x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat43));
    assert_eq!(type_specifier_non_array(&b"dmat4x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::DMat4));
    assert_eq!(type_specifier_non_array(&b"sampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler1D));
    assert_eq!(type_specifier_non_array(&b"image1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image1D));
    assert_eq!(type_specifier_non_array(&b"sampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2D));
    assert_eq!(type_specifier_non_array(&b"image2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image2D));
    assert_eq!(type_specifier_non_array(&b"sampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler3D));
    assert_eq!(type_specifier_non_array(&b"image3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image3D));
    assert_eq!(type_specifier_non_array(&b"samplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::SamplerCube));
    assert_eq!(type_specifier_non_array(&b"imageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ImageCube));
    assert_eq!(type_specifier_non_array(&b"sampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DRect));
    assert_eq!(type_specifier_non_array(&b"image2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image2DRect));
    assert_eq!(type_specifier_non_array(&b"sampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler1DArray));
    assert_eq!(type_specifier_non_array(&b"image1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image1DArray));
    assert_eq!(type_specifier_non_array(&b"sampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DArray));
    assert_eq!(type_specifier_non_array(&b"image2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image2DArray));
    assert_eq!(type_specifier_non_array(&b"samplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::SamplerBuffer));
    assert_eq!(type_specifier_non_array(&b"imageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ImageBuffer));
    assert_eq!(type_specifier_non_array(&b"sampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DMS));
    assert_eq!(type_specifier_non_array(&b"image2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image2DMS));
    assert_eq!(type_specifier_non_array(&b"sampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DMSArray));
    assert_eq!(type_specifier_non_array(&b"image2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Image2DMSArray));
    assert_eq!(type_specifier_non_array(&b"samplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::SamplerCubeArray));
    assert_eq!(type_specifier_non_array(&b"imageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ImageCubeArray));
    assert_eq!(type_specifier_non_array(&b"sampler1DShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler1DShadow));
    assert_eq!(type_specifier_non_array(&b"sampler2DShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DShadow));
    assert_eq!(type_specifier_non_array(&b"sampler2DRectShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DRectShadow));
    assert_eq!(type_specifier_non_array(&b"sampler1DArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler1DArrayShadow));
    assert_eq!(type_specifier_non_array(&b"sampler2DArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::Sampler2DArrayShadow));
    assert_eq!(type_specifier_non_array(&b"samplerCubeShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::SamplerCubeShadow));
    assert_eq!(type_specifier_non_array(&b"samplerCubeArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow));
    assert_eq!(type_specifier_non_array(&b"isampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler1D));
    assert_eq!(type_specifier_non_array(&b"iimage1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage1D));
    assert_eq!(type_specifier_non_array(&b"isampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler2D));
    assert_eq!(type_specifier_non_array(&b"iimage2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage2D));
    assert_eq!(type_specifier_non_array(&b"isampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler3D));
    assert_eq!(type_specifier_non_array(&b"iimage3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage3D));
    assert_eq!(type_specifier_non_array(&b"isamplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISamplerCube));
    assert_eq!(type_specifier_non_array(&b"iimageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImageCube));
    assert_eq!(type_specifier_non_array(&b"isampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler2DRect));
    assert_eq!(type_specifier_non_array(&b"iimage2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage2DRect));
    assert_eq!(type_specifier_non_array(&b"isampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler1DArray));
    assert_eq!(type_specifier_non_array(&b"iimage1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage1DArray));
    assert_eq!(type_specifier_non_array(&b"isampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler2DArray));
    assert_eq!(type_specifier_non_array(&b"iimage2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage2DArray));
    assert_eq!(type_specifier_non_array(&b"isamplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISamplerBuffer));
    assert_eq!(type_specifier_non_array(&b"iimageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImageBuffer));
    assert_eq!(type_specifier_non_array(&b"isampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler2DMS));
    assert_eq!(type_specifier_non_array(&b"iimage2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage2DMS));
    assert_eq!(type_specifier_non_array(&b"isampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISampler2DMSArray));
    assert_eq!(type_specifier_non_array(&b"iimage2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImage2DMSArray));
    assert_eq!(type_specifier_non_array(&b"isamplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::ISamplerCubeArray));
    assert_eq!(type_specifier_non_array(&b"iimageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::IImageCubeArray));
    assert_eq!(type_specifier_non_array(&b"atomic_uint"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::AtomicUInt));
    assert_eq!(type_specifier_non_array(&b"usampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler1D));
    assert_eq!(type_specifier_non_array(&b"uimage1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage1D));
    assert_eq!(type_specifier_non_array(&b"usampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler2D));
    assert_eq!(type_specifier_non_array(&b"uimage2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage2D));
    assert_eq!(type_specifier_non_array(&b"usampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler3D));
    assert_eq!(type_specifier_non_array(&b"uimage3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage3D));
    assert_eq!(type_specifier_non_array(&b"usamplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USamplerCube));
    assert_eq!(type_specifier_non_array(&b"uimageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImageCube));
    assert_eq!(type_specifier_non_array(&b"usampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler2DRect));
    assert_eq!(type_specifier_non_array(&b"uimage2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage2DRect));
    assert_eq!(type_specifier_non_array(&b"usampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler1DArray));
    assert_eq!(type_specifier_non_array(&b"uimage1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage1DArray));
    assert_eq!(type_specifier_non_array(&b"usampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler2DArray));
    assert_eq!(type_specifier_non_array(&b"uimage2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage2DArray));
    assert_eq!(type_specifier_non_array(&b"usamplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USamplerBuffer));
    assert_eq!(type_specifier_non_array(&b"uimageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImageBuffer));
    assert_eq!(type_specifier_non_array(&b"usampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler2DMS));
    assert_eq!(type_specifier_non_array(&b"uimage2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage2DMS));
    assert_eq!(type_specifier_non_array(&b"usampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USampler2DMSArray));
    assert_eq!(type_specifier_non_array(&b"uimage2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImage2DMSArray));
    assert_eq!(type_specifier_non_array(&b"usamplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::USamplerCubeArray));
    assert_eq!(type_specifier_non_array(&b"uimageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::UImageCubeArray));
    assert_eq!(type_specifier_non_array(
      &b"ReturnType"[..]),
      IResult::Done(&b""[..], syntax::TypeSpecifierNonArray::TypeName(syntax::TypeName::new("ReturnType").unwrap()))
    );
  }

  #[test]
  fn parse_type_specifier() {
    assert_eq!(type_specifier(&b"uint;"[..]), IResult::Done(&b";"[..], syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::UInt,
      array_specifier: None
    }));
    assert_eq!(type_specifier(&b"iimage2DMSArray[35];"[..]), IResult::Done(&b";"[..], syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: Some(syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(35))))
    }));
  }
  
  #[test]
  fn parse_fully_specified_type() {
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
      array_specifier: None
    };
    let expected = syntax::FullySpecifiedType { qualifier: None, ty };
  
    assert_eq!(fully_specified_type(&b"iimage2DMSArray;"[..]), IResult::Done(&b";"[..], expected.clone()));
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
  
    assert_eq!(fully_specified_type(&b"subroutine (vec2, S032_29k) iimage2DMSArray;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(fully_specified_type(&b"  subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray ;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(fully_specified_type(&b"subroutine(vec2,S032_29k)iimage2DMSArray;"[..]), IResult::Done(&b";"[..], expected));
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
    let fun = syntax::FunIdentifier::Identifier("vec3".into());
    let args = Vec::new();
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"vec3();"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b" vec3   (  ) ;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b" vec3   (\nvoid\n) ;"[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_postfix_function_call_one_arg() {
    let fun = syntax::FunIdentifier::Identifier("foo".into());
    let args = vec![syntax::Expr::IntConst(0)];
    let expected = syntax::Expr::FunCall(fun, args);

    assert_eq!(postfix_expr(&b"foo(0);"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo   ( 0 ) ;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo   (\n0\t\n) ;"[..]), IResult::Done(&b";"[..], expected));
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

    assert_eq!(postfix_expr(&b"foo(0, false, bar);"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo   ( 0\t, false    ,\t\tbar) ;"[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_postfix_expr_bracket() {
    let id = syntax::Expr::Variable("foo".into());
    let array_spec = syntax::ArraySpecifier::ExplicitlySized(Box::new(syntax::Expr::IntConst(7354)));
    let expected = syntax::Expr::Bracket(Box::new(id), array_spec);
  
    assert_eq!(postfix_expr(&b"foo[7354];"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b" foo[7354];"[..]), IResult::Done(&b";"[..], expected));
  }
 
  #[test]
  fn parse_postfix_expr_dot() {
    let foo = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::Expr::Dot(foo, "bar".into());

    assert_eq!(postfix_expr(&b"foo.bar;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b"(foo).bar;"[..]), IResult::Done(&b";"[..], expected));
  }
  
  #[test]
  fn parse_postfix_expr_dot_several() {
    let foo = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::Expr::Dot(Box::new(syntax::Expr::Dot(foo, "bar".into())), "zoo".into());

    assert_eq!(postfix_expr(&b"foo.bar.zoo;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b"(foo).bar.zoo;"[..]), IResult::Done(&b";"[..], expected.clone()));
    assert_eq!(postfix_expr(&b"(foo.bar).zoo;"[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_postfix_postinc() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::PostInc(Box::new(foo));

    assert_eq!(postfix_expr(&b"foo++;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_postfix_postdec() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::PostDec(Box::new(foo));

    assert_eq!(postfix_expr(&b"foo--;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_add() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Add, Box::new(foo));

    assert_eq!(unary_expr(&b"+foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_minus() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Minus, Box::new(foo));

    assert_eq!(unary_expr(&b"-foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_not() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Not, Box::new(foo));

    assert_eq!(unary_expr(&b"!foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_complement() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Complement, Box::new(foo));

    assert_eq!(unary_expr(&b"~foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_inc() {
    let foo = syntax::Expr::Variable("foo".into());
    let expected = syntax::Expr::Unary(syntax::UnaryOp::Inc, Box::new(foo));

    assert_eq!(unary_expr(&b"++foo;"[..]), IResult::Done(&b";"[..], expected.clone()));
  }

  #[test]
  fn parse_unary_dec() {
    let foo = syntax::Expr::Variable("foo".into());
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
    let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)))), Box::new(syntax::Expr::Binary(syntax::BinaryOp::Div, four, Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, five, six)))));

    assert_eq!(expr(&b"1 * (2 + 3) + 4 / (5 + 6);"[..]), IResult::Done(&b";"[..], expected.clone()));
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

    assert_eq!(expr(&input[..]), IResult::Done(&b";"[..], expected));
  }

  #[test]
  fn parse_function_identifier_typename() {
    let expected = syntax::FunIdentifier::Identifier("foo".into());
    assert_eq!(function_identifier(&b"foo("[..]), IResult::Done(&b"("[..], expected.clone()));
    assert_eq!(function_identifier(&b"  foo\n\t("[..]), IResult::Done(&b"("[..], expected.clone()));
    assert_eq!(function_identifier(&b" \tfoo\n ("[..]), IResult::Done(&b"("[..], expected));
  }

  #[test]
  fn parse_function_identifier_cast() {
    let expected = syntax::FunIdentifier::Identifier("vec3".into());
    assert_eq!(function_identifier(&b"vec3("[..]), IResult::Done(&b"("[..], expected.clone()));
    assert_eq!(function_identifier(&b"  vec3 ("[..]), IResult::Done(&b"("[..], expected.clone()));
    assert_eq!(function_identifier(&b" \t\n vec3\t\n\n \t ("[..]), IResult::Done(&b"("[..], expected));
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

    assert_eq!(function_identifier(&b"vec3[]("[..]), IResult::Done(&b"("[..], expected.clone()));
    assert_eq!(function_identifier(&b"\n\tvec3  [\t\n]("[..]), IResult::Done(&b"("[..], expected));
  }

  #[test]
  fn parse_function_identifier_cast_array_sized() {
    let expected =
      syntax::FunIdentifier::Expr(
        Box::new(
          syntax::Expr::Bracket(Box::new(syntax::Expr::Variable("vec3".into())),
                                syntax::ArraySpecifier::ExplicitlySized(
                                  Box::new(
                                    syntax::Expr::IntConst(12)
                                  )
                                )
          )
        )
      );

    assert_eq!(function_identifier(&b"vec3[12]("[..]), IResult::Done(&b"("[..], expected.clone()));
    assert_eq!(function_identifier(&b"\n\tvec3  [\t 12\n]("[..]), IResult::Done(&b"("[..], expected));
  }

  #[test]
  fn parse_void() {
    assert_eq!(void(&b"void "[..]), IResult::Done(&b" "[..], ()));
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
    let expected = Some(syntax::Expr::Assignment(Box::new(syntax::Expr::Variable("foo".into())),
                                                 syntax::AssignmentOp::Equal,
                                                 Box::new(syntax::Expr::FloatConst(314.))));

    assert_eq!(expr_statement(&b"foo = 314.f;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(expr_statement(&b"foo=314.f;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(expr_statement(&b"\n\t foo\n\t=  \n314.f\n   ;"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(declaration(&b"vec3 foo(vec2, out float the_arg);"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"  vec3 \nfoo ( vec2\n, out float \n\tthe_arg )\n;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"vec3 foo(vec2,out float the_arg);"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(declaration(&b"int foo = 34;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"int foo=34;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"\n\t int    \t  \nfoo =\t34  ;"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(declaration(&b"int foo = 34, bar = 12;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"int foo=34,bar=12;"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"\n\t int    \t  \nfoo =\t34 \n,\tbar=      12\n ;"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_declaration_precision_low() {
    let qual = syntax::PrecisionQualifier::Low;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration(&b"precision lowp float;"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_declaration_precision_medium() {
    let qual = syntax::PrecisionQualifier::Medium;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration(&b"precision mediump float;"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_declaration_precision_high() {
    let qual = syntax::PrecisionQualifier::High;
    let ty = syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None
    };
    let expected = syntax::Declaration::Precision(qual, ty);

    assert_eq!(declaration(&b"precision highp float;"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(declaration(&b"uniform UniformBlockTest { float a; vec3 b; foo c, d; };"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"\n\tuniform   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(declaration(&b"buffer UniformBlockTest { float a; vec3 b[]; foo c, d; };"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(declaration(&b"\n\tbuffer   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b   [   ]\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(selection_statement(&b"if (foo < 10) { return false; }K"[..]), IResult::Done(&b"K"[..], expected.clone()));
    assert_eq!(selection_statement(&b" if \n(foo<10\n) \t{return false;}K"[..]), IResult::Done(&b"K"[..], expected));
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

    assert_eq!(selection_statement(&b"if (foo < 10) { return 0.f; } else { return foo; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(selection_statement(&b" if \n(foo<10\n) \t{return 0.f\t;\n\n}\n else{\n\t return foo   ;}"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_switch_statement_empty() {
    let head = Box::new(syntax::Expr::Variable("foo".into()));
    let expected = syntax::SwitchStatement {
      head: head,
      body: Vec::new()
    };

    assert_eq!(switch_statement(&b"switch (foo) {}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(switch_statement(&b"switch(foo){}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(switch_statement(&b"  \tswitch\n\n (  foo  \t   \n) { \n\n   }"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(switch_statement(&b"switch (foo) { case 0: case 1: return 12u; }"[..]), IResult::Done(&b""[..], expected.clone()));
  }

  #[test]
  fn parse_case_label_def() {
    assert_eq!(case_label(&b"default:"[..]), IResult::Done(&b""[..], syntax::CaseLabel::Def));
    assert_eq!(case_label(&b"  default   : "[..]), IResult::Done(&b""[..], syntax::CaseLabel::Def));
  }

  #[test]
  fn parse_case_label() {
    let expected = syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(3)));

    assert_eq!(case_label(&b"case 3:"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(case_label(&b"  case\n\t 3   : "[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(iteration_statement(&b"while (a >= b) {}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(iteration_statement(&b"while(a>=b){}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(iteration_statement(&b"\t\n  while (  a >=\n\tb  )\t  {   \n}"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(iteration_statement(&b"do {} while (a >= b);"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(iteration_statement(&b"do{}while(a>=b);"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(iteration_statement(&b"\tdo \n {\n} while (  a >=\n\tb  )\t  \n;"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(iteration_statement(&b"for (float i = 0.f; i <= 10.f; ++i) {}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(iteration_statement(&b"for(float i=0.f;i<=10.f;++i){}"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(iteration_statement(&b"   for\n\t (  \t\n\nfloat \ni \t=\n0.f\n;\ni\t<=  10.f; \n++i\n)\n{\n}"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_jump_continue() {
    assert_eq!(jump_statement(&b"continue;"[..]), IResult::Done(&b""[..], syntax::JumpStatement::Continue));
  }

  #[test]
  fn parse_jump_break() {
    assert_eq!(jump_statement(&b"break;"[..]), IResult::Done(&b""[..], syntax::JumpStatement::Break));
  }

  #[test]
  fn parse_jump_return() {
    let expected = syntax::JumpStatement::Return(Box::new(syntax::Expr::IntConst(3)));
    assert_eq!(jump_statement(&b"return 3;"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_jump_discard() {
    assert_eq!(jump_statement(&b"discard;"[..]), IResult::Done(&b""[..], syntax::JumpStatement::Discard));
  }

  #[test]
  fn parse_simple_statement_return() {
    let e = syntax::Expr::BoolConst(false);
    let expected = syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(Box::new(e)));

    assert_eq!(simple_statement(&b"return false;"[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_compound_statement_empty() {
    let expected = syntax::CompoundStatement { statement_list: Vec::new() };

    assert_eq!(compound_statement(&b"{}"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(compound_statement(&b"{ if (true) {} isampler3D x; return 42 ; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(compound_statement(&b"{if(true){}isampler3D x;return 42;}"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(function_definition(&b"iimage2DArray foo() { return bar; }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(function_definition(&b"  \niimage2DArray \tfoo\n()\n \n{\n return \nbar\n;\n }"[..]), IResult::Done(&b""[..], expected.clone()));
    assert_eq!(function_definition(&b"iimage2DArray foo(){return bar;}"[..]), IResult::Done(&b""[..], expected));
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

    assert_eq!(translation_unit(src), IResult::Done(&b""[..], expected));
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

    assert_eq!(translation_unit(src), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_pp_version_number() {
    assert_eq!(pp_version_number(&b"450 "[..]), IResult::Done(&b" "[..], 450));
  }

  #[test]
  fn parse_pp_version_profile() {
    assert_eq!(pp_version_profile(&b"core "[..]), IResult::Done(&b" "[..], syntax::PreprocessorVersionProfile::Core));
    assert_eq!(pp_version_profile(&b"compatibility "[..]), IResult::Done(&b" "[..], syntax::PreprocessorVersionProfile::Compatibility));
    assert_eq!(pp_version_profile(&b"es "[..]), IResult::Done(&b" "[..], syntax::PreprocessorVersionProfile::ES));
  }

  #[test]
  fn parse_pp_version() {
    assert_eq!(pp_version(&b"#version 450\n"[..]),
               IResult::Done(&b""[..],
                             syntax::PreprocessorVersion {
                               version: 450,
                               profile: None,
                             }));
    assert_eq!(pp_version(&b"#version 450 core\n"[..]),
               IResult::Done(&b""[..],
                             syntax::PreprocessorVersion {
                               version: 450,
                               profile: Some(syntax::PreprocessorVersionProfile::Core)
                             }));
  }

  #[test]
  fn parse_pp_version_newline() {
    assert_eq!(preprocessor(&b"\n\t \n#version 450\n"[..]),
               IResult::Done(&b""[..],
                             syntax::Preprocessor::Version(syntax::PreprocessorVersion {
                               version: 450,
                               profile: None,
                             })));
    assert_eq!(preprocessor(&b"\n\t \n#version 450 core\n"[..]),
               IResult::Done(&b""[..],
                             syntax::Preprocessor::Version(syntax::PreprocessorVersion {
                               version: 450,
                               profile: Some(syntax::PreprocessorVersionProfile::Core)
                             })));
  }

  #[test]
  fn parse_define() {
    assert_eq!(preprocessor(&b"#define test 1.0\n"[..]),
               IResult::Done(&b""[..],
                             syntax::Preprocessor::Define(syntax::PreprocessorDefine {
                               name: "test".into(),
                               value: syntax::Expr::DoubleConst(1.0)
                             })));

    assert_eq!(preprocessor(&b"#define test123 .0f\n"[..]),
               IResult::Done(&b""[..],
                             syntax::Preprocessor::Define(syntax::PreprocessorDefine {
                               name: "test123".into(),
                               value: syntax::Expr::FloatConst(0.0)
                             })));

    assert_eq!(preprocessor(&b"#define test 1\n"[..]),
               IResult::Done(&b""[..],
                             syntax::Preprocessor::Define(syntax::PreprocessorDefine {
                               name: "test".into(),
                               value: syntax::Expr::IntConst(1)
                             })));
  }

  #[test]
  fn parse_pp_extension_name() {
    assert_eq!(pp_extension_name(&b"all"[..]), IResult::Done(&b""[..], syntax::PreprocessorExtensionName::All));
    assert_eq!(pp_extension_name(&b"GL_foobar_extension "[..]), IResult::Done(&b""[..], syntax::PreprocessorExtensionName::Specific("GL_foobar_extension".to_owned())));
  }

  #[test]
  fn parse_pp_extension_behavior() {
    assert_eq!(pp_extension_behavior(&b"require"[..]), IResult::Done(&b""[..], syntax::PreprocessorExtensionBehavior::Require));
    assert_eq!(pp_extension_behavior(&b"enable"[..]), IResult::Done(&b""[..], syntax::PreprocessorExtensionBehavior::Enable));
    assert_eq!(pp_extension_behavior(&b"warn"[..]), IResult::Done(&b""[..], syntax::PreprocessorExtensionBehavior::Warn));
    assert_eq!(pp_extension_behavior(&b"disable"[..]), IResult::Done(&b""[..], syntax::PreprocessorExtensionBehavior::Disable));
  }

  #[test]
  fn parse_pp_extension() {
    assert_eq!(pp_extension(&b"#extension all: require\n"[..]),
               IResult::Done(&b""[..],
                             syntax::PreprocessorExtension {
                               name: syntax::PreprocessorExtensionName::All,
                               behavior: Some(syntax::PreprocessorExtensionBehavior::Require)
                             }));
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

    assert_eq!(expr(&src[..]), IResult::Done(&b";"[..], expected));
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

    assert_eq!(statement(&src[..]), IResult::Done(&b""[..], expected));
  }

  #[test]
  fn parse_bl_tag() {
    let src = b" foo // foobar\n";
    assert_eq!(bl!(&src[..], tag!("foo")), IResult::Done(&b""[..], &b"foo"[..]));
  }
}
