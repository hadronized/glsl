extern crate glsl;
extern crate nom;

use nom::IResult;

use glsl::parser;
use glsl::syntax;

#[test]
fn parse_type_specifier() {
  assert_eq!(parser::type_specifier(&b"bool"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Bool));
  assert_eq!(parser::type_specifier(&b"int"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Int));
  assert_eq!(parser::type_specifier(&b"uint"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UInt));
  assert_eq!(parser::type_specifier(&b"float"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Float));
  assert_eq!(parser::type_specifier(&b"double"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Double));
  assert_eq!(parser::type_specifier(&b"vec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Vec2));
  assert_eq!(parser::type_specifier(&b"vec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Vec3));
  assert_eq!(parser::type_specifier(&b"vec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Vec4));
  assert_eq!(parser::type_specifier(&b"dvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DVec2));
  assert_eq!(parser::type_specifier(&b"dvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DVec3));
  assert_eq!(parser::type_specifier(&b"dvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DVec4));
  assert_eq!(parser::type_specifier(&b"bvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::BVec2));
  assert_eq!(parser::type_specifier(&b"bvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::BVec3));
  assert_eq!(parser::type_specifier(&b"bvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::BVec4));
  assert_eq!(parser::type_specifier(&b"ivec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IVec2));
  assert_eq!(parser::type_specifier(&b"ivec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IVec3));
  assert_eq!(parser::type_specifier(&b"ivec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IVec4));
  assert_eq!(parser::type_specifier(&b"uvec2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UVec2));
  assert_eq!(parser::type_specifier(&b"uvec3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UVec3));
  assert_eq!(parser::type_specifier(&b"uvec4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UVec4));
  assert_eq!(parser::type_specifier(&b"mat2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat2));
  assert_eq!(parser::type_specifier(&b"mat3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat3));
  assert_eq!(parser::type_specifier(&b"mat4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat4));
  assert_eq!(parser::type_specifier(&b"mat2x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat2));
  assert_eq!(parser::type_specifier(&b"mat2x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat23));
  assert_eq!(parser::type_specifier(&b"mat2x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat24));
  assert_eq!(parser::type_specifier(&b"mat3x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat32));
  assert_eq!(parser::type_specifier(&b"mat3x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat3));
  assert_eq!(parser::type_specifier(&b"mat3x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat34));
  assert_eq!(parser::type_specifier(&b"mat4x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat42));
  assert_eq!(parser::type_specifier(&b"mat4x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat43));
  assert_eq!(parser::type_specifier(&b"mat4x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Mat4));
  assert_eq!(parser::type_specifier(&b"dmat2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat2));
  assert_eq!(parser::type_specifier(&b"dmat3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat3));
  assert_eq!(parser::type_specifier(&b"dmat4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat4));
  assert_eq!(parser::type_specifier(&b"dmat2x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat2));
  assert_eq!(parser::type_specifier(&b"dmat2x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat23));
  assert_eq!(parser::type_specifier(&b"dmat2x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat24));
  assert_eq!(parser::type_specifier(&b"dmat3x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat32));
  assert_eq!(parser::type_specifier(&b"dmat3x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat3));
  assert_eq!(parser::type_specifier(&b"dmat3x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat34));
  assert_eq!(parser::type_specifier(&b"dmat4x2"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat42));
  assert_eq!(parser::type_specifier(&b"dmat4x3"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat43));
  assert_eq!(parser::type_specifier(&b"dmat4x4"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::DMat4));
  assert_eq!(parser::type_specifier(&b"sampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1D));
  assert_eq!(parser::type_specifier(&b"image1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image1D));
  assert_eq!(parser::type_specifier(&b"sampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2D));
  assert_eq!(parser::type_specifier(&b"image2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2D));
  assert_eq!(parser::type_specifier(&b"sampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler3D));
  assert_eq!(parser::type_specifier(&b"image3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image3D));
  assert_eq!(parser::type_specifier(&b"samplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCube));
  assert_eq!(parser::type_specifier(&b"imageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ImageCube));
  assert_eq!(parser::type_specifier(&b"sampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DRect));
  assert_eq!(parser::type_specifier(&b"image2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DRect));
  assert_eq!(parser::type_specifier(&b"sampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1DArray));
  assert_eq!(parser::type_specifier(&b"image1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image1DArray));
  assert_eq!(parser::type_specifier(&b"sampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DArray));
  assert_eq!(parser::type_specifier(&b"image2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DArray));
  assert_eq!(parser::type_specifier(&b"samplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerBuffer));
  assert_eq!(parser::type_specifier(&b"imageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ImageBuffer));
  assert_eq!(parser::type_specifier(&b"sampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DMS));
  assert_eq!(parser::type_specifier(&b"image2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DMS));
  assert_eq!(parser::type_specifier(&b"sampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DMSArray));
  assert_eq!(parser::type_specifier(&b"image2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Image2DMSArray));
  assert_eq!(parser::type_specifier(&b"samplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCubeArray));
  assert_eq!(parser::type_specifier(&b"imageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ImageCubeArray));
  assert_eq!(parser::type_specifier(&b"sampler1DShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1DShadow));
  assert_eq!(parser::type_specifier(&b"sampler2DShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DShadow));
  assert_eq!(parser::type_specifier(&b"sampler2DRectShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DRectShadow));
  assert_eq!(parser::type_specifier(&b"sampler1DArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler1DArrayShadow));
  assert_eq!(parser::type_specifier(&b"sampler2DArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::Sampler2DArrayShadow));
  assert_eq!(parser::type_specifier(&b"samplerCubeShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCubeShadow));
  assert_eq!(parser::type_specifier(&b"samplerCubeArrayShadow"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::SamplerCubeArrayShadow));
  assert_eq!(parser::type_specifier(&b"isampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler1D));
  assert_eq!(parser::type_specifier(&b"iimage1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage1D));
  assert_eq!(parser::type_specifier(&b"isampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2D));
  assert_eq!(parser::type_specifier(&b"iimage2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2D));
  assert_eq!(parser::type_specifier(&b"isampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler3D));
  assert_eq!(parser::type_specifier(&b"iimage3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage3D));
  assert_eq!(parser::type_specifier(&b"isamplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISamplerCube));
  assert_eq!(parser::type_specifier(&b"iimageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImageCube));
  assert_eq!(parser::type_specifier(&b"isampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DRect));
  assert_eq!(parser::type_specifier(&b"iimage2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DRect));
  assert_eq!(parser::type_specifier(&b"isampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler1DArray));
  assert_eq!(parser::type_specifier(&b"iimage1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage1DArray));
  assert_eq!(parser::type_specifier(&b"isampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DArray));
  assert_eq!(parser::type_specifier(&b"iimage2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DArray));
  assert_eq!(parser::type_specifier(&b"isamplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISamplerBuffer));
  assert_eq!(parser::type_specifier(&b"iimageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImageBuffer));
  assert_eq!(parser::type_specifier(&b"isampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DMS));
  assert_eq!(parser::type_specifier(&b"iimage2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DMS));
  assert_eq!(parser::type_specifier(&b"isampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISampler2DMSArray));
  assert_eq!(parser::type_specifier(&b"iimage2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImage2DMSArray));
  assert_eq!(parser::type_specifier(&b"isamplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::ISamplerCubeArray));
  assert_eq!(parser::type_specifier(&b"iimageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::IImageCubeArray));
  assert_eq!(parser::type_specifier(&b"atomic_uint"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::AtomicUInt));
  assert_eq!(parser::type_specifier(&b"usampler1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler1D));
  assert_eq!(parser::type_specifier(&b"uimage1D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage1D));
  assert_eq!(parser::type_specifier(&b"usampler2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2D));
  assert_eq!(parser::type_specifier(&b"uimage2D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2D));
  assert_eq!(parser::type_specifier(&b"usampler3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler3D));
  assert_eq!(parser::type_specifier(&b"uimage3D"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage3D));
  assert_eq!(parser::type_specifier(&b"usamplerCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USamplerCube));
  assert_eq!(parser::type_specifier(&b"uimageCube"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImageCube));
  assert_eq!(parser::type_specifier(&b"usampler2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DRect));
  assert_eq!(parser::type_specifier(&b"uimage2DRect"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DRect));
  assert_eq!(parser::type_specifier(&b"usampler1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler1DArray));
  assert_eq!(parser::type_specifier(&b"uimage1DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage1DArray));
  assert_eq!(parser::type_specifier(&b"usampler2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DArray));
  assert_eq!(parser::type_specifier(&b"uimage2DArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DArray));
  assert_eq!(parser::type_specifier(&b"usamplerBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USamplerBuffer));
  assert_eq!(parser::type_specifier(&b"uimageBuffer"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImageBuffer));
  assert_eq!(parser::type_specifier(&b"usampler2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DMS));
  assert_eq!(parser::type_specifier(&b"uimage2DMS"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DMS));
  assert_eq!(parser::type_specifier(&b"usampler2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USampler2DMSArray));
  assert_eq!(parser::type_specifier(&b"uimage2DMSArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImage2DMSArray));
  assert_eq!(parser::type_specifier(&b"usamplerCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::USamplerCubeArray));
  assert_eq!(parser::type_specifier(&b"uimageCubeArray"[..]), IResult::Done(&b""[..], syntax::TypeSpecifier::UImageCubeArray));
}

#[test]
fn parse_fully_specified_type() {
  let ty = syntax::TypeSpecifier::IImage2DMSArray;
  let expected = syntax::FullySpecifiedType { qualifier: None, ty: ty };

  assert_eq!(parser::fully_specified_type(&b"iimage2DMSArray"[..]), IResult::Done(&b""[..], expected.clone()));
}

#[test]
fn parse_fully_specified_type_with_qualifier() {
  let qual = syntax::TypeQualifier::Storage(syntax::StorageQualifier::Subroutine(vec!["vec2".to_owned(), "S032_29k".to_owned()]));
  let ty = syntax::TypeSpecifier::IImage2DMSArray;
  let expected = syntax::FullySpecifiedType { qualifier: Some(qual), ty: ty };

  assert_eq!(parser::fully_specified_type(&b"subroutine (vec2, S032_29k) iimage2DMSArray"[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::fully_specified_type(&b"  subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray "[..]), IResult::Done(&b""[..], expected.clone()));
  assert_eq!(parser::fully_specified_type(&b"subroutine(vec2,S032_29k)iimage2DMSArray"[..]), IResult::Done(&b""[..], expected));
}
