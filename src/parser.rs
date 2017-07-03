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
named!(void, tag!("void"));

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
named!(type_qualifier<&[u8], syntax::TypeQualifier>,
  alt!(
    map!(storage_qualifier, syntax::TypeQualifier::Storage) |
    //layout_qualifier | // FIXME
    map!(precision_qualifier, syntax::TypeQualifier::Precision) |
    map!(interpolation_qualifier, syntax::TypeQualifier::Interpolation) |
    value!(syntax::TypeQualifier::Invariant, invariant_qualifier) |
    value!(syntax::TypeQualifier::Precise, precise_qualifier)
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
named!(array_specifier<&[u8], syntax::ArraySpecifier>,
  alt!(
    ws!(do_parse!(char!('[') >> char!(']') >> (syntax::ArraySpecifier::Unsized))) |
    ws!(do_parse!(char!('[') >> e: cond_expr >> char!(']') >> (syntax::ArraySpecifier::ExplicitlySized(Box::new(e)))))
  )
);

/// Parse a primary expression.
named!(primary_expr<&[u8], syntax::Expr>,
  alt!(
    // primary expression
    map!(float_lit, |s| syntax::Expr::FloatConst(bytes_to_string(s))) |
    map!(double_lit, |s| syntax::Expr::DoubleConst(bytes_to_string(s))) |
    map!(bool_lit, |s| syntax::Expr::BoolConst(s)) |
    map!(unsigned_lit, |s| syntax::Expr::UIntConst(bytes_to_string(s))) |
    map!(integral_lit, |s| syntax::Expr::IntConst(bytes_to_string(s))) |
    parens_expr
  )
);

/// Parse a postfix expression.
named!(postfix_expr<&[u8], syntax::Expr>,
  alt!(
    primary_expr |
    postfix_expr_bracket |
    function_call |
    //dot_field_selection | // FIXME
    postfix_inc |
    postfix_dec
  )
);

/// Parse a unary expression.
named!(unary_expr<&[u8], syntax::Expr>,
  alt!(
    postfix_expr |
    do_parse!(
      o: unary_op >>
      e: unary_expr >>
      (syntax::Expr::Unary(o, Box::new(e)))
    )
  )
);

/// Parse an expression between parens.
named!(parens_expr<&[u8], syntax::Expr>, delimited!(char!('('), postfix_expr, char!(')')));

/// Parse a postfix expression with brackets (array annotation).
named!(postfix_expr_bracket<&[u8], syntax::Expr>,
  do_parse!(
    e: postfix_expr >>
    char!('[') >>
    a: ws!(expr) >>
    char!(']') >>

    (syntax::Expr::Bracket(Box::new(e), syntax::ArraySpecifier::ExplicitlySized(Box::new(a))))
  )
);

/// Parse a postfix incrementation expression.
named!(postfix_inc<&[u8], syntax::Expr>,
  do_parse!(
    e: postfix_expr >>
    tag!("++") >>

    (syntax::Expr::PostInc(Box::new(e)))
  )
);

/// Parse a postfix decrementation expression.
named!(postfix_dec<&[u8], syntax::Expr>,
  do_parse!(
    e: postfix_expr >>
    tag!("--") >>

    (syntax::Expr::PostDec(Box::new(e)))
  )
);

/// Parse a dot field selection.
named!(dot_field_selection<&[u8], syntax::FieldSelection>,
  do_parse!(
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
named!(function_call<&[u8], syntax::Expr>,
  ws!(do_parse!(
    fc: alt!(function_call_header_with_parameters | function_call_header_no_parameters) >>
    char!(')') >>
    (fc)
  ))
);

named!(function_call_header_no_parameters<&[u8], syntax::Expr>,
  do_parse!(
    fi: function_call_header >>
    opt!(void) >>

    (syntax::Expr::FunCall(fi, Vec::new()))
  )
);

named!(function_call_header_with_parameters<&[u8], syntax::Expr>,
  ws!(do_parse!(
    fi: function_call_header >>
    first_arg: assignment_expr >>
    rest_args: many0!(ws!(do_parse!(char!(',') >> arg: assignment_expr >> (arg)))) >>

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
named!(function_identifier<&[u8], syntax::FunIdentifier>,
  alt!(
    map!(type_specifier, syntax::FunIdentifier::TypeSpecifier) |
    map!(postfix_expr, |e| syntax::FunIdentifier::Expr(Box::new(e)))
  )
);

/// Parse the most general expression.
named!(expr<&[u8], syntax::Expr>,
  alt!(
    assignment_expr |
    ws!(do_parse!(
      a: expr >>
      char!(',') >>
      b: assignment_expr >>

      (syntax::Expr::Comma(Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse an assignment expression.
named!(assignment_expr<&[u8], syntax::Expr>,
  alt!(
    cond_expr |
    ws!(do_parse!(
      e: unary_expr >>
      o: assignment_op >>
      v: assignment_expr >>

      (syntax::Expr::Assignment(Box::new(e), o, Box::new(v)))
    ))
  )
);

/// Parse an assignment operator.
named!(assignment_op<&[u8], syntax::AssignmentOp>,
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
named!(cond_expr<&[u8], syntax::Expr>,
  alt!(
    logical_or_expr |
    ws!(do_parse!(
      a: logical_or_expr >>
      char!('?') >>
      b: expr >>
      char!(':') >>
      c: assignment_expr >>

      (syntax::Expr::Ternary(Box::new(a), Box::new(b), Box::new(c)))
    ))
  )
);

/// Parse a logical OR expression.
named!(logical_or_expr<&[u8], syntax::Expr>,
  alt!(
    logical_xor_expr |
    ws!(do_parse!(
      a: logical_or_expr >>
      tag!("||") >>
      b: logical_xor_expr >>

      (syntax::Expr::Binary(syntax::BinaryOp::Or, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a logical XOR expression.
named!(logical_xor_expr<&[u8], syntax::Expr>,
  alt!(
    logical_and_expr |
    ws!(do_parse!(
      a: logical_xor_expr >>
      tag!("^^") >>
      b: logical_and_expr >>

      (syntax::Expr::Binary(syntax::BinaryOp::Xor, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a logical AND expression.
named!(logical_and_expr<&[u8], syntax::Expr>,
  alt!(
    inclusive_or_expr |
    ws!(do_parse!(
      a: logical_and_expr >>
      tag!("&&") >>
      b: inclusive_or_expr >>

      (syntax::Expr::Binary(syntax::BinaryOp::And, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a bitwise OR expression.
named!(inclusive_or_expr<&[u8], syntax::Expr>,
  alt!(
    exclusive_or_expr |
    ws!(do_parse!(
      a: inclusive_or_expr >>
      char!('|') >>
      b: exclusive_or_expr >>

      (syntax::Expr::Binary(syntax::BinaryOp::BitOr, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a bitwise XOR expression.
named!(exclusive_or_expr<&[u8], syntax::Expr>,
  alt!(
    and_expr |
    ws!(do_parse!(
      a: exclusive_or_expr >>
      char!('|') >>
      b: and_expr >>

      (syntax::Expr::Binary(syntax::BinaryOp::BitXor, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a bitwise AND expression.
named!(and_expr<&[u8], syntax::Expr>,
  alt!(
    equality_expr |
    ws!(do_parse!(
      a: and_expr >>
      char!('|') >>
      b: equality_expr >>

      (syntax::Expr::Binary(syntax::BinaryOp::BitAnd, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse an equality expression.
named!(equality_expr<&[u8], syntax::Expr>,
  alt!(
    rel_expr |
    ws!(do_parse!(
      a: equality_expr >>
      op: alt!(
            value!(syntax::BinaryOp::Equal, tag!("==")) |
            value!(syntax::BinaryOp::NonEqual, tag!("!="))
          ) >>
      b: rel_expr >>

      (syntax::Expr::Binary(op, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a relational expression.
named!(rel_expr<&[u8], syntax::Expr>,
  alt!(
    shift_expr |
    ws!(do_parse!(
      a: rel_expr >>
      op: alt!(
            value!(syntax::BinaryOp::LT, char!('<')) |
            value!(syntax::BinaryOp::GT, char!('>')) |
            value!(syntax::BinaryOp::LTE, tag!("<=")) |
            value!(syntax::BinaryOp::GTE, tag!(">="))
          ) >>
      b: shift_expr >>

      (syntax::Expr::Binary(op, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a shift expression.
named!(shift_expr<&[u8], syntax::Expr>,
  alt!(
    additive_expr |
    ws!(do_parse!(
      a: shift_expr >>
      op: alt!(
            value!(syntax::BinaryOp::LShift, tag!("<<")) |
            value!(syntax::BinaryOp::RShift, tag!(">>"))
          ) >>
      b: additive_expr >>

      (syntax::Expr::Binary(op, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse an additive expression.
named!(additive_expr<&[u8], syntax::Expr>,
  alt!(
    multiplicative_expr |
    ws!(do_parse!(
      a: additive_expr >>
      op: alt!(
            value!(syntax::BinaryOp::Add, char!('+')) |
            value!(syntax::BinaryOp::Sub, char!('-'))
          ) >>
      b: multiplicative_expr >>

      (syntax::Expr::Binary(op, Box::new(a), Box::new(b)))
    ))
  )
);

/// Parse a multiplicative expression.
named!(multiplicative_expr<&[u8], syntax::Expr>,
  alt!(
    unary_expr |
    ws!(do_parse!(
      a: multiplicative_expr >>
      op: alt!(
            value!(syntax::BinaryOp::Mult, char!('*')) |
            value!(syntax::BinaryOp::Div, char!('/')) |
            value!(syntax::BinaryOp::Mod, char!('%'))
          ) >>
      b: unary_expr >>

      (syntax::Expr::Binary(op, Box::new(a), Box::new(b)))
    ))
  )
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
