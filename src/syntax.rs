/// A generic identifier.
pub type Identifier = String;

/// Type specifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeSpecifier {
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
  UImageCubeArray,
  Struct(StructSpecifier)
}

/// Dimensionality of an arary.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ArraySpecifier {
  Unsized,
  ExplicitlySized(IntegerExpr)
}

/// Struct specifier. Used to create new, user-defined types.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructSpecifier {
  pub name: Option<String>,
  pub fields: Vec<StructFieldSpecifier>,
}

/// Struct field specifier. Used to add fields to struct specifiers.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StructFieldSpecifier {
  pub ty: TypeSpecifier,
  pub identifiers: Vec<Identifier> // several identifiers of the same basic type
}

/// An integer expression. Usually used to index an array or specify a binding index.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IntegerExpr(Box<Expr>);

/// The most general form of an expression. As you can see if you read the variant list, in GLSL, an
/// assignment is an expression. This is a bit silly but think of an assignment as a statement first
/// then an expression which evaluates to what the statement “returns”.
///
/// An expression is either an assignment or a list (comma) of assignments.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
  Assignment(AssignmentExpr),
  Comma(Box<Expr>, AssignmentExpr)
}

/// Assignment expression. It’s either a conditional expression or an assignment that augments a
/// unary expression with another assignment expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AssignmentExpr {
  Cond(CondExpr),
  Assignment(UnaryExpr, AssignmentOp, Box<AssignmentExpr>)
}

/// All possible operators for assigning expressions.
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

/// Logical expression. It’s either a ternary operator use (cond ? a : b) or a logical OR
/// expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CondExpr {
  LogicalOrExpr(LogicalOrExpr),
  Ternary(LogicalOrExpr, Box<Expr>, Box<AssignmentExpr>)
}

/// Logical OR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalOrExpr {
  LogicalXorExpr(LogicalXorExpr),
  Or(Box<LogicalOrExpr>, LogicalXorExpr)
}

/// Logical XOR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalXorExpr {
  LogicalAndExpr(LogicalAndExpr),
  Xor(Box<LogicalXorExpr>, LogicalAndExpr)
}

/// Logical AND expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalAndExpr {
  InclusiveOrExpr(InclusiveOrExpr),
  And(Box<LogicalAndExpr>, InclusiveOrExpr)
}

/// Inclusive OR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InclusiveOrExpr {
  ExclusiveOrExpr(ExclusiveOrExpr),
  InclusiveOr(Box<InclusiveOrExpr>, ExclusiveOrExpr)
}

/// Exclusive OR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ExclusiveOrExpr {
  AndExpr(AndExpr),
  ExclusiveOr(Box<ExclusiveOrExpr>, AndExpr)
}

/// AND expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AndExpr {
  EqualityExpr(EqualityExpr),
  And(Box<AndExpr>, EqualityExpr)
}

/// Equality expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum EqualityExpr {
  RelExpr(RelExpr),
  Equality(Box<EqualityExpr>, RelExpr),
  NonEquality(Box<EqualityExpr>, RelExpr)
}

/// Relational expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RelExpr {
  ShiftExpr(ShiftExpr),
  LessThan(Box<RelExpr>, ShiftExpr),
  GreaterThan(Box<RelExpr>, ShiftExpr),
  LessThanOrEqual(Box<RelExpr>, ShiftExpr),
  GreaterThanOrEqual(Box<RelExpr>, ShiftExpr),
}

/// Shift expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ShiftExpr {
  AdditiveExpr(AdditiveExpr),
  Left(Box<ShiftExpr>, AdditiveExpr),
  Right(Box<ShiftExpr>, AdditiveExpr),
}

/// Additive expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AdditiveExpr {
  MultExpr(MultExpr),
  Plus(Box<AdditiveExpr>, MultExpr),
  Dash(Box<AdditiveExpr>, MultExpr),
}

/// Multiplicative expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MultExpr {
  UnaryExpr(UnaryExpr),
  Star(Box<MultExpr>, UnaryExpr),
  Slash(Box<MultExpr>, UnaryExpr),
  Percent(Box<MultExpr>, UnaryExpr),
}

/// Unary expression. Unary expressions are formed from postfix expressions and augmented via
/// prefixes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnaryExpr {
  Unary(PostfixExpr),
  Op(UnaryOp, Box<UnaryExpr>)
}

/// All unary operators that exist in GLSL.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnaryOp {
  Inc,
  Dec,
  Plus,
  Dash,
  Bang,
  Tilde
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
  //Precision(PrecisionQualifier, TypeSpecifier), // TODO
  Struct(StructSpecifier, Option<(Identifier, Option<ArraySpecifier>)>),
  //ForwardDecl(TypeSpecifier, Vec<Identifier>), // TODO
}

/// Postfix expression. Postfix expressions are formed from primay expressions and extend them
/// with suffix.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PostfixExpr {
  Primary(PrimaryExpr),
  Bracket(IntegerExpr),
  FunCall(FunCall),
  //Dot(FieldSelection), // TODO
  Inc(Box<PostfixExpr>),
  Dec(Box<PostfixExpr>)
}

/// Function call. A function call might contain parameters.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FunCall {
  FunCall(FunIdentifier, Vec<AssignmentExpr>)
}

/// Function identifier. Constructors are recognized via type specifiers and methods (.lenngth),
/// subroutine array calls and identifiers are recognized via postfix expressions.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FunIdentifier {
  TypeSpecifier(TypeSpecifier),
  PostfixExpr(Box<PostfixExpr>)
}

/// Primary expression.
///
/// A primary expression is the base expression. It’s used as a building block to build more
/// complex expression.
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
