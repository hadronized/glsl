pub type Identifier = String;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BasicTy {
  // transparent types
  Bool,
  Int,
  UInt,
  Float,
  Double,
  Vec2,
  Vec3,
  Vec4,
  DVec2,
  DVec3,
  DVec4,
  BVec2,
  BVec3,
  BVec4,
  IVec2,
  IVec3,
  IVec4,
  UVec2,
  UVec3,
  UVec4,
  Mat2,
  Mat3,
  Mat4,
  Mat23,
  Mat24,
  Mat32,
  Mat34,
  Mat42,
  Mat43,
  DMat2,
  DMat3,
  DMat4,
  DMat23,
  DMat24,
  DMat32,
  DMat34,
  DMat42,
  DMat43,
  // floating point opaque types
  Sampler1D,
  Image1D,
  Sampler2D,
  Image2D,
  Sampler3D,
  Image3D,
  SamplerCube,
  ImageCube,
  Sampler2DRect,
  Image2DRect,
  Sampler1DArray,
  Image1DArray,
  Sampler2DArray,
  Image2DArray,
  SamplerBuffer,
  ImageBuffer,
  Sampler2DMS,
  Image2DMS,
  Sampler2DMSArray,
  Image2DMSArray,
  SamplerCubeArray,
  ImageCubeArray,
  Sampler1DShadow,
  Sampler2DShadow,
  Sampler2DRectShadow,
  Sampler1DArrayShadow,
  Sampler2DArrayShadow,
  SamplerCubeShadow,
  SamplerCubeArrayShadow,
  // signed integer opaque types
  ISampler1D,
  IImage1D,
  ISampler2D,
  IImage2D,
  ISampler3D,
  IImage3D,
  ISamplerCube,
  IImageCube,
  ISampler2DRect,
  IImage2DRect,
  ISampler1DArray,
  IImage1DArray,
  ISampler2DArray,
  IImage2DArray,
  ISamplerBuffer,
  IImageBuffer,
  ISampler2DMS,
  IIMage2DMS,
  ISampler2DMSArray,
  IImage2DMSArray,
  ISamplerCubeArray,
  IImageCubeArray,
  // unsigned integer opaque types
  AtomicUInt,
  USampler1D,
  UImage1D,
  USampler2D,
  UImage2D,
  USampler3D,
  UImage3D,
  USamplerCube,
  UImageCube,
  USampler2DRect,
  UImage2DRect,
  USampler1DArray,
  UImage1DArray,
  USampler2DArray,
  UImage2DArray,
  USamplerBuffer,
  UImageBuffer,
  USampler2DMS,
  UImage2DMS,
  USampler2DMSArray,
  UImage2DMSArray,
  USamplerCubeArray,
  UImageCubeArray
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TySpecifier {
  BasicTy(BasicTy),
  Struct(StructSpecifier),
  Array(Box<TySpecifier>, ArraySpecifier)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ArraySpecifier {
  Unsized,
  ExplicitlySized(Expr)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructSpecifier {
  pub name: Option<String>,
  pub fields: Vec<StructFieldSpecifier>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructFieldSpecifier {
  pub ty: BasicTy, // FIXME: not only BasicTy; can be a struct as well
  pub identifiers: Vec<Identifier> // several identifiers of the same basic type
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IntegerExpr(Box<Expr>);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
  Assignment(AssignmentExpr),
  Comma(Box<Expr>, AssignmentExpr)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AssignmentExpr {
  //Cond(CondExpr),
  Assignment(UnaryExpr, AssignmentOp, Box<AssignmentExpr>)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AssignmentOp {
  Equal,
  MulAssign,
  DivAssign,
  ModAssign,
  AddAssign,
  SubAssign,
  LeftAssign,
  RightAssign,
  AndAssign,
  XorAssign,
  OrAssign
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnaryExpr {
  Unary(PostfixExpr),
  Inc(Box<UnaryExpr>),
  Dec(Box<UnaryExpr>),
  Op(UnaryOp, Box<UnaryExpr>)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOp {
  Plus,
  Minus,
  Not,
  Complement
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOp {
  Mult,
  Div,
  Mod,
  Plus,
  Minus,
  LShift,
  RShift,
  Less,
  Greater,
  LessOrEqual,
  GreaterOrEqual,
  Equal,
  NotEqual,
  BitAnd,
  BitXor,
  BitOr,
  LogicalAnd,
  LogicalXor,
  LogicalOr
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Declaration {
  //FunProto(FunProto), // TODO
  //Init(InitDeclList), // TODO
  //Precision(PrecisionQualifier, TySpecifier), // TODO
  Struct(StructSpecifier, Option<(Identifier, Option<ArraySpecifier>)>),
  //ForwardDecl(TySpecifier, Vec<Identifier>), // TODO
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PostfixExpr {
  Prim(PrimaryExpr),
  Bracket(IntegerExpr),
  //FunCall(FunCall), // TODO
  //Dot(FieldSelection), // TODO
  Inc(Box<PostfixExpr>),
  Dec(Box<PostfixExpr>)
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PrimaryExpr {
  Identifier(Identifier),
  IntConstant(String),
  UIntConstant(String),
  BoolConstant(String),
  FloatConstant(String),
  DoubleConstant(String),
  Parens(Box<Expr>)
}
