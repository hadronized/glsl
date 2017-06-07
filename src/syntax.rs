pub enum Either<A, B> {
  Left(A),
  Right(B)
}

pub enum TranslationUnit {
  TranslationUnit(Vec<ExternalDeclaration>)
}

pub enum ExternalDeclaration {
  FunctionDeclaration(FunctionPrototype),
  FunctionDefinition(FunctionPrototype, Compound),
  Declaration(Declaration)
}

pub enum Declaration {
  InitDeclaration(InvariantOrType, Vec<InitDeclarator>),
  Precision(PrecisionQualifier, TypeSpecifierNoPrecision),
  Block(TypeQualifier, String, Vec<Field>, Option<(String, Option<Option<Expr>>)>),
  TQ(TypeQualifier)
}

pub enum InitDeclarator {
  InitDecl(String, Option<Option<Expr>>, Option<Expr>)
}

pub enum InvariantOrType {
  InvariantDeclarator,
  TypeDeclarator(FullType)
}

pub enum FunctionPrototype {
  FuncProt(FullType, String, Vec<ParameterDeclaration>)
}

pub enum ParameterDeclaration {
  ParameterDeclaration(Option<ParameterTypeQualifier>, Option<ParameterQualifier>, TypeSpecifier, Option<(String, Option<Expr>)>)
}

pub enum FullType {
  FullType(Option<TypeQualifier>, TypeSpecifier)
}

pub enum TypeQualifier {
  TypeQualSto(StorageQualifier),
  TypeQualLay(LayoutQualifier, Option<StorageQualifier>),
  TypeQualInt(InterpolationQualifier, Option<StorageQualifier>),
  TypeQualInv(InvariantQualifier, Option<StorageQualifier>),
  TypeQualInv3(InvariantQualifier, InterpolationQualifier, StorageQualifier)
}

pub enum TypeSpecifier {
  TypeSpecifier(Option<PrecisionQualifier>, Box<TypeSpecifierNoPrecision>)
}

pub enum InvariantQualifier {
  Invariant
}

pub enum InterpolationQualifier {
  Smooth,
  Flat,
  NoPerspective
}

pub enum LayoutQualifier {
  LayoutQualId(String, Option<Expr>)
}

pub enum Statement {
  DeclarationStatement(Declaration),
  Continue,
  Break,
  Return(Option<Expr>),
  Discard,
  CompoundStatement(Compound),
  ExpressionStatement(Option<Expr>),
  SelectionStatement(Expr, Box<Statement>, Option<Box<Statement>>),
  SwitchStatement(Expr, Vec<Statement>),
  CaseLabel(CaseLabel),
  While(Condition, Box<Statement>),
  DoWhile(Box<Statement>, Expr),
  For(Either<Option<Expr>, Declaration>, Option<Condition>, Option<Expr>, Box<Statement>)
}

pub enum Compound {
  Compound(Vec<Statement>)
}

pub enum Condition {
  Condition(Expr),
  InitializedCondition(FullType, String, Expr)
}

pub enum CaseLabel {
  Case(Expr),
  Default
}

pub enum StorageQualifier {
  Const,
  Attribute,
  Varying,
  CentroidVarying,
  In,
  Out,
  CentroidIn,
  CentroidOut,
  Uniform
}

pub enum TypeSpecifierNoPrecision {
  TypeSpecNoPrecision(TypeSpecifierNonArray, Option<Option<Expr>>)
}

pub enum TypeSpecifierNonArray {
  Void,
  Float,
  Int,
  UInt,
  Bool,
  Vec2,
  Vec3,
  Vec4,
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
  Mat2x2,
  Mat2x3,
  Mat2x4,
  Mat3x2,
  Mat3x3,
  Mat3x4,
  Mat4x2,
  Mat4x3,
  Mat4x4,
  Sampler1D,
  Sampler2D,
  Sampler3D,
  SamplerCube,
  Sampler1DShadow,
  Sampler2DShadow,
  SamplerCubeShadow,
  Sampler1DArray,
  Sampler2DArray,
  Sampler1DArrayShadow,
  Sampler2DArrayShadow,
  ISampler1D,
  ISampler2D,
  ISampler3D,
  ISamplerCube,
  ISampler1DArray,
  ISampler2DArray,
  USampler1D,
  USampler2D,
  USampler3D,
  USamplerCube,
  USampler1DArray,
  USampler2DArray,
  Sampler2DRect,
  Sampler2DRectShadow,
  ISampler2DRect,
  USampler2DRect,
  SamplerBuffer,
  ISamplerBuffer,
  USamplerBuffer,
  Sampler2DMS,
  ISampler2DMS,
  USampler2DMS,
  Sampler2DMSArray,
  ISampler2DMSArray,
  USampler2DMSArray,
  StructSpecifier(Option<String>, Vec<Field>),
  TypeName(String)
}

pub enum PrecisionQualifier {
  HighP,
  MediumP,
  LowP
}

pub enum Field {
  Field(Option<TypeQualifier>, TypeSpecifier, Vec<StructDeclarator>)
}

pub enum StructDeclarator {
  StructDeclarator(String, Option<Option<Expr>>)
}

pub enum Expr {
  Variable(String),
  IntConstant(IntConstantKind, u32),
  FloatConstant(f32),
  BoolConstant(bool),
  Bracket(Box<Expr>, Box<Expr>),
  FieldSelection(Box<Expr>, String),
  MethodCall(Box<Expr>, FunctionIdentifier, Parameters),
  FunctionCall(FunctionIdentifier, Parameters),
  PostInc(Box<Expr>),
  PostDec(Box<Expr>),
  PreInc(Box<Expr>),
  PreDec(Box<Expr>),
  UnaryPlus(Box<Expr>),
  UnaryNegate(Box<Expr>),
  UnaryNot(Box<Expr>),
  UnaryOneComplement(Box<Expr>),
  Mul(Box<Expr>, Box<Expr>),
  Div(Box<Expr>, Box<Expr>),
  Mod(Box<Expr>, Box<Expr>),
  Add(Box<Expr>, Box<Expr>),
  Sub(Box<Expr>, Box<Expr>),
  LeftShift(Box<Expr>, Box<Expr>),
  RightShift(Box<Expr>, Box<Expr>),
  Lt(Box<Expr>, Box<Expr>),
  Gt(Box<Expr>, Box<Expr>),
  Lte(Box<Expr>, Box<Expr>),
  Gte(Box<Expr>, Box<Expr>),
  Equ(Box<Expr>, Box<Expr>),
  Neq(Box<Expr>, Box<Expr>),
  BitAnd(Box<Expr>, Box<Expr>),
  BitXor(Box<Expr>, Box<Expr>),
  BitOr(Box<Expr>, Box<Expr>),
  And(Box<Expr>, Box<Expr>),
  Or(Box<Expr>, Box<Expr>),
  Selection(Box<Expr>, Box<Expr>, Box<Expr>),
  Equal(Box<Expr>, Box<Expr>),
  MulAssign(Box<Expr>, Box<Expr>),
  DivAssign(Box<Expr>, Box<Expr>),
  ModAssign(Box<Expr>, Box<Expr>),
  AddAssign(Box<Expr>, Box<Expr>),
  SubAssign(Box<Expr>, Box<Expr>),
  LeftAssign(Box<Expr>, Box<Expr>),
  RightAssign(Box<Expr>, Box<Expr>),
  AndAssign(Box<Expr>, Box<Expr>),
  XorAssign(Box<Expr>, Box<Expr>),
  OrAssign(Box<Expr>, Box<Expr>),
  Sequence(Box<Expr>, Box<Expr>)
}

pub enum IntConstantKind {
  Hexadecimal,
  Octal,
  Decimal
}

pub enum Parameters {
  ParamVoid,
  Params(Vec<Expr>)
}

pub enum ParameterQualifier {
  InParameter,
  OutParameter,
  InOutParameter
}

pub enum ParameterTypeQualifier {
  ConstParameter
}

pub enum FunctionIdentifier {
  FuncIdTypeSpec(TypeSpecifier),
  FundId(String)
}
