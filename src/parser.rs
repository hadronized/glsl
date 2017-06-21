use nom::{ErrorKind, IResult, alphanumeric, digit};
use std::str::{from_utf8_unchecked};

use syntax;

// Turn a &[u8] into a String.
#[inline]
fn bytes_to_string(bytes: &[u8]) -> String {
  unsafe { from_utf8_unchecked(bytes).to_owned() }
}

/// Parse an identifier (raw version).
named!(pub identifier_str,
  do_parse!(
    name: verify!(take_while1!(identifier_pred), verify_identifier) >>
    (name)
  )
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
named!(pub nonempty_identifiers<&[u8], Vec<syntax::Identifier>>,
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
named!(void_ty<&[u8], ()>, value!((), tag!("void")));

/// Parse a digit that precludes a leading 0.
named!(pub nonzero_digit, verify!(digit, |s:&[u8]| s[0] != b'0'));

/// Parse a decimal literal string.
named!(decimal_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    nonzero_digit >>
    (())
  )
);

/// Parse a decimal literal.
named!(pub decimal_lit, recognize!(decimal_lit_));

#[inline]
fn all_octal(s: &[u8]) -> bool {
  s.iter().all(|&c| c >= b'0' && c <= b'7')
}

/// Parse an octal literal string.
named!(octal_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    char!('0') >>
    n: verify!(digit, all_octal) >>
    (())
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
named!(hexadecimal_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    alt!(tag!("0x") | tag!("0X")) >>
    verify!(take_while!(alphanumeric_no_u), all_hexa) >>
    (())
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

/// Parse the unsigned suffix.
named!(pub unsigned_suffix<&[u8], char>, alt!(char!('u') | char!('U')));

/// Parse a literal unsigned string.
named!(pub unsigned_lit,
  do_parse!(
    n: integral_lit >>
    unsigned_suffix >>
    (n)
  )
);

/// Parse a floating point suffix.
named!(pub float_suffix,
  alt!(
    tag!("f") |
    tag!("F")
  )
);

/// Parse a double point suffix.
named!(pub double_suffix,
  alt!(
    tag!("lf") |
    tag!("LF")
  )
);


/// Parse the exponent part of a floating point literal.
named!(pub floating_exponent<&[u8], ()>,
  do_parse!(
    alt!(char!('e') | char!('E')) >>
    opt!(alt!(char!('+') | char!('-'))) >>
    digit >>
    (())
  )
);

/// Parse the fractional constant part of a floating point literal.
named!(pub floating_frac<&[u8], ()>,
  alt!(
    do_parse!(char!('.') >> digit >> (())) |
    do_parse!(digit >> tag!(".") >> digit >> (())) |
    do_parse!(digit >> tag!(".") >> (())) |
    do_parse!(digit >> (()))
  )
);

/// Parse a float literal string.
named!(float_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    floating_frac >>
    opt!(floating_exponent) >>
    opt!(float_suffix) >>
    (())
  )
);

/// Parse a float litereal.
named!(pub float_lit, recognize!(float_lit_));

/// Parse a double literal string.
named!(double_lit_<&[u8], ()>,
  do_parse!(
    ws!(opt!(char!('-'))) >>
    floating_frac >>
    opt!(floating_exponent) >>
    opt!(double_suffix) >>
    (())
  )
);

/// Parse a double litereal.
named!(pub double_lit, recognize!(double_lit_));

/// Parse a constant boolean.
named!(bool_lit<&[u8], bool>,
  alt!(
    value!(true, tag!("true")) |
    value!(false, tag!("false"))
  )
);

/// Parse a unary operator.
named!(unary_op<&[u8], syntax::UnaryOp>,
  alt!(
    value!(syntax::UnaryOp::Plus, char!('+')) |
    value!(syntax::UnaryOp::Dash, char!('-')) |
    value!(syntax::UnaryOp::Bang, char!('!')) |
    value!(syntax::UnaryOp::Tilde, char!('~'))
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
named!(storage_qualifier_subroutine_list<&[u8], syntax::StorageQualifier>,
  ws!(do_parse!(
    tag!("subroutine") >>
    identifiers: delimited!(char!('('),
                            nonempty_identifiers,
                            char!(')')) >>
    (syntax::StorageQualifier::Subroutine(identifiers))
  ))
);

/// Parse a storage qualifier subroutine rule.
named!(storage_qualifier_subroutine<&[u8], syntax::StorageQualifier>,
  alt!(
    storage_qualifier_subroutine_list |
    value!(syntax::StorageQualifier::Subroutine(Vec::new()), tag!("subroutine"))
  )
);

/// Parse a storage qualifier.
named!(pub storage_qualifier<&[u8], syntax::TypeQualifier>,
  alt!(
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Const), tag!("const")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::InOut), tag!("inout")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::In), tag!("in")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Out), tag!("out")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Centroid), tag!("centroid")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Patch), tag!("patch")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Sample), tag!("sample")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Uniform), tag!("uniform")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Buffer), tag!("buffer")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Shared), tag!("shared")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Coherent), tag!("coherent")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Volatile), tag!("volatile")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::Restrict), tag!("restrict")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::ReadOnly), tag!("readonly")) |
    value!(syntax::TypeQualifier::Storage(syntax::StorageQualifier::WriteOnly), tag!("writeonly")) |
    map!(storage_qualifier_subroutine, syntax::TypeQualifier::Storage)
  )
);

/// Parse a precision qualifier.
named!(pub precision_qualifier<&[u8], syntax::TypeQualifier>,
  alt!(
    value!(syntax::TypeQualifier::Precision(syntax::PrecisionQualifier::High), tag!("high")) |
    value!(syntax::TypeQualifier::Precision(syntax::PrecisionQualifier::Medium), tag!("medium")) |
    value!(syntax::TypeQualifier::Precision(syntax::PrecisionQualifier::Low), tag!("low"))
  )
);

/// Parse an interpolation qualifier.
named!(pub interpolation_qualifier<&[u8], syntax::TypeQualifier>,
  alt!(
    value!(syntax::TypeQualifier::Interpolation(syntax::InterpolationQualifier::Smooth), tag!("smooth")) |
    value!(syntax::TypeQualifier::Interpolation(syntax::InterpolationQualifier::Flat), tag!("flat")) |
    value!(syntax::TypeQualifier::Interpolation(syntax::InterpolationQualifier::NoPerspective), tag!("noperspective"))
  )
);

/// Parse an invariant qualifier.
named!(pub invariant_qualifier<&[u8], syntax::TypeQualifier>,
  value!(syntax::TypeQualifier::Invariant, tag!("invariant")));

/// Parse a precise qualifier.
named!(pub precise_qualifier<&[u8], syntax::TypeQualifier>,
  value!(syntax::TypeQualifier::Precise, tag!("precise")));

/// Parse a type qualifier.
named!(type_qualifier<&[u8], syntax::TypeQualifier>,
  alt!(
    storage_qualifier |
    //layout_qualifier | // FIXME
    precision_qualifier |
    interpolation_qualifier |
    invariant_qualifier |
    precise_qualifier
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
named!(array_specifier_unsized<&[u8], syntax::ArraySpecifier>,
  value!(syntax::ArraySpecifier::Unsized, ws!(do_parse!(char!('[') >> char!(']') >> (()))))
);

// /// Parse a primary expression.
// named!(primary_expr<&[u8], syntax::PrimaryExpr>,
//   alt!(
//     map!(float_lit, |s| syntax::PrimaryExpr::FloatConst(bytes_to_string(s))) |
//     map!(double_lit, |s| syntax::PrimaryExpr::DoubleConst(bytes_to_string(s))) |
//     map!(bool_lit, |s| syntax::PrimaryExpr::BoolConst(s)) |
//     map!(unsigned_lit, |s| syntax::PrimaryExpr::UIntConst(bytes_to_string(s))) |
//     map!(integral_lit, |s| syntax::PrimaryExpr::IntConst(bytes_to_string(s))) |
//     //map!(parens_expr, syntax::PrimaryExpr::Parse) | // TODO
//     map!(identifier, syntax::PrimaryExpr::Identifier)
//   )
// );

///// Parse an array specifier with a size.
//named!(array_specifier_sized<&[u8], syntax::ArraySpecifier>,
//  ws!(do_parse!(
//    char!('[') >>
//    s:
//  ))
//);

///// Parse an array specifier.
//named!(array_specifier,
//  alt!(
//    array_specifier_unsized |
//    array_specifier_sized
//  )
//);
