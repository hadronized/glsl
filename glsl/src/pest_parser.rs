//! [pest](https://crates.io/crates/pest) parser.

use pest::Parser as PestParser;
use pest::iterators::{Pair as PestPair, Pairs as PestPairs};

use syntax;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
struct Parser;

type Pair<'a> = PestPair<'a, Rule>;
type Pairs<'a> = PestPairs<'a, Rule>;

/// Class of types that can be parsed.
pub trait Parse: Sized {
  /// Parse an item from a string.
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str>;
}

impl Parse for syntax::Identifier {
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str> {
    let mut pairs = Parser::parse(Rule::identifier, input.into()).map_err(|e| format!("{}", e))?;
    parse_identifier(pairs.next().unwrap())
  }
}

impl Parse for syntax::TypeSpecifierNonArray {
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str> {
    let mut pairs = Parser::parse(Rule::type_specifier_nonarray, input.into()).map_err(|e| format!("{}", e))?;
    parse_type_specifier_non_array(pairs.next().unwrap())
  }
}

fn parse_identifier<'a>(pair: Pair<'a>) -> Result<syntax::Identifier, String> {
  Ok(pair.as_str().to_owned())
}

fn parse_type_specifier_non_array<'a>(pair: Pair<'a>) -> Result<syntax::TypeSpecifierNonArray, String> {
  let inner = pair.into_inner().next().unwrap();

  match inner.as_rule() {
    Rule::struct_specifier => {
      unimplemented!()
    }

    Rule::identifier => {
      let identifier = inner.as_str();

      match identifier {
        "void" => Ok(syntax::TypeSpecifierNonArray::Void),
        "float" => Ok(syntax::TypeSpecifierNonArray::Float),
        "double" => Ok(syntax::TypeSpecifierNonArray::Double),
        "int" => Ok(syntax::TypeSpecifierNonArray::Int),
        "uint" => Ok(syntax::TypeSpecifierNonArray::UInt),
        "bool" => Ok(syntax::TypeSpecifierNonArray::Bool),
        "vec2" => Ok(syntax::TypeSpecifierNonArray::Vec2),
        "vec3" => Ok(syntax::TypeSpecifierNonArray::Vec3),
        "vec4" => Ok(syntax::TypeSpecifierNonArray::Vec4),
        "dvec2" => Ok(syntax::TypeSpecifierNonArray::DVec2),
        "dvec3" => Ok(syntax::TypeSpecifierNonArray::DVec3),
        "dvec4" => Ok(syntax::TypeSpecifierNonArray::DVec4),
        "bvec2" => Ok(syntax::TypeSpecifierNonArray::BVec2),
        "bvec3" => Ok(syntax::TypeSpecifierNonArray::BVec3),
        "bvec4" => Ok(syntax::TypeSpecifierNonArray::BVec4),
        "ivec2" => Ok(syntax::TypeSpecifierNonArray::IVec2),
        "ivec3" => Ok(syntax::TypeSpecifierNonArray::IVec3),
        "ivec4" => Ok(syntax::TypeSpecifierNonArray::IVec4),
        "uvec2" => Ok(syntax::TypeSpecifierNonArray::UVec2),
        "uvec3" => Ok(syntax::TypeSpecifierNonArray::UVec3),
        "uvec4" => Ok(syntax::TypeSpecifierNonArray::UVec4),
        "mat2x2" => Ok(syntax::TypeSpecifierNonArray::Mat2),
        "mat2x3" => Ok(syntax::TypeSpecifierNonArray::Mat23),
        "mat2x4" => Ok(syntax::TypeSpecifierNonArray::Mat24),
        "mat3x2" => Ok(syntax::TypeSpecifierNonArray::Mat32),
        "mat3x3" => Ok(syntax::TypeSpecifierNonArray::Mat3),
        "mat3x4" => Ok(syntax::TypeSpecifierNonArray::Mat34),
        "mat4x2" => Ok(syntax::TypeSpecifierNonArray::Mat42),
        "mat4x3" => Ok(syntax::TypeSpecifierNonArray::Mat43),
        "mat4x4" => Ok(syntax::TypeSpecifierNonArray::Mat4),
        "mat2" => Ok(syntax::TypeSpecifierNonArray::Mat2),
        "mat3" => Ok(syntax::TypeSpecifierNonArray::Mat3),
        "mat4" => Ok(syntax::TypeSpecifierNonArray::Mat4),
        "dmat2x2" => Ok(syntax::TypeSpecifierNonArray::DMat2),
        "dmat2x3" => Ok(syntax::TypeSpecifierNonArray::DMat23),
        "dmat2x4" => Ok(syntax::TypeSpecifierNonArray::DMat24),
        "dmat3x2" => Ok(syntax::TypeSpecifierNonArray::DMat32),
        "dmat3x3" => Ok(syntax::TypeSpecifierNonArray::DMat3),
        "dmat3x4" => Ok(syntax::TypeSpecifierNonArray::DMat34),
        "dmat4x2" => Ok(syntax::TypeSpecifierNonArray::DMat42),
        "dmat4x3" => Ok(syntax::TypeSpecifierNonArray::DMat43),
        "dmat4x4" => Ok(syntax::TypeSpecifierNonArray::DMat4),
        "dmat2" => Ok(syntax::TypeSpecifierNonArray::DMat2),
        "dmat3" => Ok(syntax::TypeSpecifierNonArray::DMat3),
        "dmat4" => Ok(syntax::TypeSpecifierNonArray::DMat4),
        "atomic_uint" => Ok(syntax::TypeSpecifierNonArray::AtomicUInt),
        "samplerBuffer" => Ok(syntax::TypeSpecifierNonArray::SamplerBuffer),
        "samplerCubeShadow" => Ok(syntax::TypeSpecifierNonArray::SamplerCubeShadow),
        "samplerCubeArrayShadow" => Ok(syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow),
        "samplerCubeArray" => Ok(syntax::TypeSpecifierNonArray::SamplerCubeArray),
        "samplerCube" => Ok(syntax::TypeSpecifierNonArray::SamplerCube),
        "sampler1DShadow" => Ok(syntax::TypeSpecifierNonArray::Sampler1DShadow),
        "sampler1DArrayShadow" => Ok(syntax::TypeSpecifierNonArray::Sampler1DArrayShadow),
        "sampler1DArray" => Ok(syntax::TypeSpecifierNonArray::Sampler1DArray),
        "sampler1D"  => Ok(syntax::TypeSpecifierNonArray::Sampler1D),
        "sampler2DShadow" => Ok(syntax::TypeSpecifierNonArray::Sampler2DShadow),
        "sampler2DArrayShadow" => Ok(syntax::TypeSpecifierNonArray::Sampler2DArrayShadow),
        "sampler2DArray" => Ok(syntax::TypeSpecifierNonArray::Sampler2DArray),
        "sampler2DRectShadow" => Ok(syntax::TypeSpecifierNonArray::Sampler2DRectShadow),
        "sampler2DRect" => Ok(syntax::TypeSpecifierNonArray::Sampler2DRect),
        "sampler2DMSArray" => Ok(syntax::TypeSpecifierNonArray::Sampler2DMSArray),
        "sampler2DMS" => Ok(syntax::TypeSpecifierNonArray::Sampler2DMS),
        "sampler2D" => Ok(syntax::TypeSpecifierNonArray::Sampler2D),
        "sampler3D" => Ok(syntax::TypeSpecifierNonArray::Sampler3D),
        "isamplerBuffer" => Ok(syntax::TypeSpecifierNonArray::ISamplerBuffer),
        "isamplerCubeArray" => Ok(syntax::TypeSpecifierNonArray::ISamplerCubeArray),
        "isamplerCube" => Ok(syntax::TypeSpecifierNonArray::ISamplerCube),
        "isampler1DArray" => Ok(syntax::TypeSpecifierNonArray::ISampler1DArray),
        "isampler1D" => Ok(syntax::TypeSpecifierNonArray::ISampler1D),
        "isampler2DArray" => Ok(syntax::TypeSpecifierNonArray::ISampler2DArray),
        "isampler2DMSArray" => Ok(syntax::TypeSpecifierNonArray::ISampler2DMSArray),
        "isampler2DMS" => Ok(syntax::TypeSpecifierNonArray::ISampler2DMS),
        "isampler2DRect" => Ok(syntax::TypeSpecifierNonArray::ISampler2DRect),
        "isampler2D" => Ok(syntax::TypeSpecifierNonArray::ISampler2D),
        "isampler3D" => Ok(syntax::TypeSpecifierNonArray::ISampler3D),
        "usamplerBuffer" => Ok(syntax::TypeSpecifierNonArray::USamplerBuffer),
        "usamplerCubeArray" => Ok(syntax::TypeSpecifierNonArray::USamplerCubeArray),
        "usamplerCube" => Ok(syntax::TypeSpecifierNonArray::USamplerCube),
        "usampler1DArray" => Ok(syntax::TypeSpecifierNonArray::USampler1DArray),
        "usampler1D" => Ok(syntax::TypeSpecifierNonArray::USampler1D),
        "usampler2DArray" => Ok(syntax::TypeSpecifierNonArray::USampler2DArray),
        "usampler2DRect" => Ok(syntax::TypeSpecifierNonArray::USampler2DRect),
        "usampler2DMSArray" => Ok(syntax::TypeSpecifierNonArray::USampler2DMSArray),
        "usampler2DMS" => Ok(syntax::TypeSpecifierNonArray::USampler2DMS),
        "usampler2D" => Ok(syntax::TypeSpecifierNonArray::USampler2D),
        "usampler3D" => Ok(syntax::TypeSpecifierNonArray::USampler3D),
        "imageBuffer" => Ok(syntax::TypeSpecifierNonArray::ImageBuffer),
        "imageCubeArray" => Ok(syntax::TypeSpecifierNonArray::ImageCubeArray),
        "imageCube" => Ok(syntax::TypeSpecifierNonArray::ImageCube),
        "image1DArray" => Ok(syntax::TypeSpecifierNonArray::Image1DArray),
        "image1D" => Ok(syntax::TypeSpecifierNonArray::Image1D),
        "image2DArray" => Ok(syntax::TypeSpecifierNonArray::Image2DArray),
        "image2DRect" => Ok(syntax::TypeSpecifierNonArray::Image2DRect),
        "image2DMSArray" => Ok(syntax::TypeSpecifierNonArray::Image2DMSArray),
        "image2DMS" => Ok(syntax::TypeSpecifierNonArray::Image2DMS),
        "image2D" => Ok(syntax::TypeSpecifierNonArray::Image2D),
        "image3D" => Ok(syntax::TypeSpecifierNonArray::Image3D),
        "iimageBuffer" => Ok(syntax::TypeSpecifierNonArray::IImageBuffer),
        "iimageCubeArray" => Ok(syntax::TypeSpecifierNonArray::IImageCubeArray),
        "iimageCube" => Ok(syntax::TypeSpecifierNonArray::IImageCube),
        "iimage1DArray" => Ok(syntax::TypeSpecifierNonArray::IImage1DArray),
        "iimage1D" => Ok(syntax::TypeSpecifierNonArray::IImage1D),
        "iimage2DArray" => Ok(syntax::TypeSpecifierNonArray::IImage2DArray),
        "iimage2DRect" => Ok(syntax::TypeSpecifierNonArray::IImage2DRect),
        "iimage2DMSArray" => Ok(syntax::TypeSpecifierNonArray::IImage2DMSArray),
        "iimage2DMS" => Ok(syntax::TypeSpecifierNonArray::IImage2DMS),
        "iimage2D" => Ok(syntax::TypeSpecifierNonArray::IImage2D),
        "iimage3D" => Ok(syntax::TypeSpecifierNonArray::IImage3D),
        "uimageBuffer" => Ok(syntax::TypeSpecifierNonArray::UImageBuffer),
        "uimageCubeArray" => Ok(syntax::TypeSpecifierNonArray::UImageCubeArray),
        "uimageCube" => Ok(syntax::TypeSpecifierNonArray::UImageCube),
        "uimage1DArray" => Ok(syntax::TypeSpecifierNonArray::UImage1DArray),
        "uimage1D" => Ok(syntax::TypeSpecifierNonArray::UImage1D),
        "uimage2DArray" => Ok(syntax::TypeSpecifierNonArray::UImage2DArray),
        "uimage2DRect" => Ok(syntax::TypeSpecifierNonArray::UImage2DRect),
        "uimage2DMSArray" => Ok(syntax::TypeSpecifierNonArray::UImage2DMSArray),
        "uimage2DMS" => Ok(syntax::TypeSpecifierNonArray::UImage2DMS),
        "uimage2D" => Ok(syntax::TypeSpecifierNonArray::UImage2D),
        "uimage3D" => Ok(syntax::TypeSpecifierNonArray::UImage3D),
        _ => Ok(syntax::TypeSpecifierNonArray::TypeName(identifier.to_owned()))
      }
    }

    _ => unreachable!()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_identifier() {
    assert_eq!(<syntax::Identifier as Parse>::parse("a").unwrap().as_str(), "a");
    assert_eq!(<syntax::Identifier as Parse>::parse("ab_cd").unwrap().as_str(), "ab_cd");
    assert_eq!(<syntax::Identifier as Parse>::parse("Ab_cd").unwrap().as_str(), "Ab_cd");
    assert_eq!(<syntax::Identifier as Parse>::parse("Ab_c8d").unwrap().as_str(), "Ab_c8d");
    assert_eq!(<syntax::Identifier as Parse>::parse("Ab_c8d9").unwrap().as_str(), "Ab_c8d9");
    assert!(<syntax::Identifier as Parse>::parse("3Ab_c8d9").is_err());
  }

  #[test]
  fn parse_type_specifier_non_array() {
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("bool").unwrap(), syntax::TypeSpecifierNonArray::Bool);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("int").unwrap(), syntax::TypeSpecifierNonArray::Int);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uint").unwrap(), syntax::TypeSpecifierNonArray::UInt);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("float").unwrap(), syntax::TypeSpecifierNonArray::Float);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("double").unwrap(), syntax::TypeSpecifierNonArray::Double);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("vec2").unwrap(), syntax::TypeSpecifierNonArray::Vec2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("vec3").unwrap(), syntax::TypeSpecifierNonArray::Vec3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("vec4").unwrap(), syntax::TypeSpecifierNonArray::Vec4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dvec2").unwrap(), syntax::TypeSpecifierNonArray::DVec2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dvec3").unwrap(), syntax::TypeSpecifierNonArray::DVec3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dvec4").unwrap(), syntax::TypeSpecifierNonArray::DVec4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("bvec2").unwrap(), syntax::TypeSpecifierNonArray::BVec2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("bvec3").unwrap(), syntax::TypeSpecifierNonArray::BVec3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("bvec4").unwrap(), syntax::TypeSpecifierNonArray::BVec4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("ivec2").unwrap(), syntax::TypeSpecifierNonArray::IVec2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("ivec3").unwrap(), syntax::TypeSpecifierNonArray::IVec3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("ivec4").unwrap(), syntax::TypeSpecifierNonArray::IVec4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uvec2").unwrap(), syntax::TypeSpecifierNonArray::UVec2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uvec3").unwrap(), syntax::TypeSpecifierNonArray::UVec3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uvec4").unwrap(), syntax::TypeSpecifierNonArray::UVec4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat2").unwrap(), syntax::TypeSpecifierNonArray::Mat2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat3").unwrap(), syntax::TypeSpecifierNonArray::Mat3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat4").unwrap(), syntax::TypeSpecifierNonArray::Mat4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat2x2").unwrap(), syntax::TypeSpecifierNonArray::Mat2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat2x3").unwrap(), syntax::TypeSpecifierNonArray::Mat23);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat2x4").unwrap(), syntax::TypeSpecifierNonArray::Mat24);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat3x2").unwrap(), syntax::TypeSpecifierNonArray::Mat32);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat3x3").unwrap(), syntax::TypeSpecifierNonArray::Mat3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat3x4").unwrap(), syntax::TypeSpecifierNonArray::Mat34);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat4x2").unwrap(), syntax::TypeSpecifierNonArray::Mat42);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat4x3").unwrap(), syntax::TypeSpecifierNonArray::Mat43);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("mat4x4").unwrap(), syntax::TypeSpecifierNonArray::Mat4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat2").unwrap(), syntax::TypeSpecifierNonArray::DMat2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat3").unwrap(), syntax::TypeSpecifierNonArray::DMat3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat4").unwrap(), syntax::TypeSpecifierNonArray::DMat4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat2x2").unwrap(), syntax::TypeSpecifierNonArray::DMat2);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat2x3").unwrap(), syntax::TypeSpecifierNonArray::DMat23);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat2x4").unwrap(), syntax::TypeSpecifierNonArray::DMat24);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat3x2").unwrap(), syntax::TypeSpecifierNonArray::DMat32);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat3x3").unwrap(), syntax::TypeSpecifierNonArray::DMat3);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat3x4").unwrap(), syntax::TypeSpecifierNonArray::DMat34);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat4x2").unwrap(), syntax::TypeSpecifierNonArray::DMat42);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat4x3").unwrap(), syntax::TypeSpecifierNonArray::DMat43);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("dmat4x4").unwrap(), syntax::TypeSpecifierNonArray::DMat4);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler1D").unwrap(), syntax::TypeSpecifierNonArray::Sampler1D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image1D").unwrap(), syntax::TypeSpecifierNonArray::Image1D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2D").unwrap(), syntax::TypeSpecifierNonArray::Sampler2D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image2D").unwrap(), syntax::TypeSpecifierNonArray::Image2D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler3D").unwrap(), syntax::TypeSpecifierNonArray::Sampler3D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image3D").unwrap(), syntax::TypeSpecifierNonArray::Image3D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("samplerCube").unwrap(), syntax::TypeSpecifierNonArray::SamplerCube);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("imageCube").unwrap(), syntax::TypeSpecifierNonArray::ImageCube);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DRect").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DRect);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image2DRect").unwrap(), syntax::TypeSpecifierNonArray::Image2DRect);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler1DArray").unwrap(), syntax::TypeSpecifierNonArray::Sampler1DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image1DArray").unwrap(), syntax::TypeSpecifierNonArray::Image1DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DArray").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image2DArray").unwrap(), syntax::TypeSpecifierNonArray::Image2DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("samplerBuffer").unwrap(), syntax::TypeSpecifierNonArray::SamplerBuffer);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("imageBuffer").unwrap(), syntax::TypeSpecifierNonArray::ImageBuffer);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DMS").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DMS);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image2DMS").unwrap(), syntax::TypeSpecifierNonArray::Image2DMS);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DMSArray").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DMSArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("image2DMSArray").unwrap(), syntax::TypeSpecifierNonArray::Image2DMSArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("samplerCubeArray").unwrap(), syntax::TypeSpecifierNonArray::SamplerCubeArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("imageCubeArray").unwrap(), syntax::TypeSpecifierNonArray::ImageCubeArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler1DShadow").unwrap(), syntax::TypeSpecifierNonArray::Sampler1DShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DShadow").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DRectShadow").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DRectShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler1DArrayShadow").unwrap(), syntax::TypeSpecifierNonArray::Sampler1DArrayShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("sampler2DArrayShadow").unwrap(), syntax::TypeSpecifierNonArray::Sampler2DArrayShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("samplerCubeShadow").unwrap(), syntax::TypeSpecifierNonArray::SamplerCubeShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("samplerCubeArrayShadow").unwrap(), syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler1D").unwrap(), syntax::TypeSpecifierNonArray::ISampler1D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage1D").unwrap(), syntax::TypeSpecifierNonArray::IImage1D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler2D").unwrap(), syntax::TypeSpecifierNonArray::ISampler2D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage2D").unwrap(), syntax::TypeSpecifierNonArray::IImage2D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler3D").unwrap(), syntax::TypeSpecifierNonArray::ISampler3D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage3D").unwrap(), syntax::TypeSpecifierNonArray::IImage3D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isamplerCube").unwrap(), syntax::TypeSpecifierNonArray::ISamplerCube);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimageCube").unwrap(), syntax::TypeSpecifierNonArray::IImageCube);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler2DRect").unwrap(), syntax::TypeSpecifierNonArray::ISampler2DRect);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage2DRect").unwrap(), syntax::TypeSpecifierNonArray::IImage2DRect);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler1DArray").unwrap(), syntax::TypeSpecifierNonArray::ISampler1DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage1DArray").unwrap(), syntax::TypeSpecifierNonArray::IImage1DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler2DArray").unwrap(), syntax::TypeSpecifierNonArray::ISampler2DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage2DArray").unwrap(), syntax::TypeSpecifierNonArray::IImage2DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isamplerBuffer").unwrap(), syntax::TypeSpecifierNonArray::ISamplerBuffer);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimageBuffer").unwrap(), syntax::TypeSpecifierNonArray::IImageBuffer);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler2DMS").unwrap(), syntax::TypeSpecifierNonArray::ISampler2DMS);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage2DMS").unwrap(), syntax::TypeSpecifierNonArray::IImage2DMS);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isampler2DMSArray").unwrap(), syntax::TypeSpecifierNonArray::ISampler2DMSArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimage2DMSArray").unwrap(), syntax::TypeSpecifierNonArray::IImage2DMSArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("isamplerCubeArray").unwrap(), syntax::TypeSpecifierNonArray::ISamplerCubeArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("iimageCubeArray").unwrap(), syntax::TypeSpecifierNonArray::IImageCubeArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("atomic_uint").unwrap(), syntax::TypeSpecifierNonArray::AtomicUInt);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler1D").unwrap(), syntax::TypeSpecifierNonArray::USampler1D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage1D").unwrap(), syntax::TypeSpecifierNonArray::UImage1D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler2D").unwrap(), syntax::TypeSpecifierNonArray::USampler2D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage2D").unwrap(), syntax::TypeSpecifierNonArray::UImage2D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler3D").unwrap(), syntax::TypeSpecifierNonArray::USampler3D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage3D").unwrap(), syntax::TypeSpecifierNonArray::UImage3D);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usamplerCube").unwrap(), syntax::TypeSpecifierNonArray::USamplerCube);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimageCube").unwrap(), syntax::TypeSpecifierNonArray::UImageCube);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler2DRect").unwrap(), syntax::TypeSpecifierNonArray::USampler2DRect);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage2DRect").unwrap(), syntax::TypeSpecifierNonArray::UImage2DRect);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler1DArray").unwrap(), syntax::TypeSpecifierNonArray::USampler1DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage1DArray").unwrap(), syntax::TypeSpecifierNonArray::UImage1DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler2DArray").unwrap(), syntax::TypeSpecifierNonArray::USampler2DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage2DArray").unwrap(), syntax::TypeSpecifierNonArray::UImage2DArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usamplerBuffer").unwrap(), syntax::TypeSpecifierNonArray::USamplerBuffer);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimageBuffer").unwrap(), syntax::TypeSpecifierNonArray::UImageBuffer);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler2DMS").unwrap(), syntax::TypeSpecifierNonArray::USampler2DMS);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage2DMS").unwrap(), syntax::TypeSpecifierNonArray::UImage2DMS);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usampler2DMSArray").unwrap(), syntax::TypeSpecifierNonArray::USampler2DMSArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimage2DMSArray").unwrap(), syntax::TypeSpecifierNonArray::UImage2DMSArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("usamplerCubeArray").unwrap(), syntax::TypeSpecifierNonArray::USamplerCubeArray);
    assert_eq!(<syntax::TypeSpecifierNonArray as Parse>::parse("uimageCubeArray").unwrap(), syntax::TypeSpecifierNonArray::UImageCubeArray);
  }
}
