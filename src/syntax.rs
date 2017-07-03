// FIXME: as soon as deeply-nested types are truly supported in rustc, remove as many boxes as
// possible. See <https://github.com/rust-lang/rust/issues/42747>.
/// A generic identifier.
pub type Identifier = String;

/// Any type name.
pub type TypeName = String;

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
  IImage2DMS,
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
  Struct(StructSpecifier),
  TypeName(TypeName)
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
  pub identifiers: Vec<Identifier> // several identifiers of the same type
}

/// Type qualifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeQualifier {
  Storage(StorageQualifier),
  Layout(LayoutQualifier),
  Precision(PrecisionQualifier),
  Interpolation(InterpolationQualifier),
  Invariant,
  Precise
}

/// Storage qualifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum StorageQualifier {
  Const,
  InOut,
  In,
  Out,
  Centroid,
  Patch,
  Sample,
  Uniform,
  Buffer,
  Shared,
  Coherent,
  Volatile,
  Restrict,
  ReadOnly,
  WriteOnly,
  Subroutine(Vec<TypeName>),
}

/// Layout qualifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LayoutQualifier {
  Identifier(Identifier, Option<Box<Expr>>),
  Shared
}

/// Precision qualifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PrecisionQualifier {
  High,
  Medium,
  Low
}

/// Interpolation qualifier.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InterpolationQualifier {
  Smooth,
  Flat,
  NoPerspective
}

/// Fully specified type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FullySpecifiedType {
  pub qualifier: Option<TypeQualifier>,
  pub ty: TypeSpecifier
}

/// Dimensionality of an arary.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ArraySpecifier {
  Unsized,
  ExplicitlySized(Box<Expr>)
}

/// A declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Declaration {
  FunctionPrototype(FunctionPrototype),
  InitDeclaratorList(InitDeclaratorList),
  Precision(PrecisionQualifier, TypeSpecifier),
  Block(TypeQualifier, Identifier, Vec<StructFieldSpecifier>, Option<(Identifier, Option<ArraySpecifier>)>),
  Global(TypeQualifier, Vec<Identifier>)
}

/// Function identifier. Constructors are recognized via type specifiers and methods (.length),
/// subroutine array calls and identifiers are recognized via postfix expressions.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FunIdentifier {
  TypeSpecifier(TypeSpecifier),
  Expr(Box<Expr>)
}

/// Function prototype.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionPrototype {
  pub ty: FullySpecifiedType,
  pub name: Identifier,
  pub parameters: Vec<FunctionParameterDeclaration>
}

/// Function parameter declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FunctionParameterDeclaration {
  Named(Option<TypeQualifier>, FunctionParameterDeclarator),
  Unnamed(Option<TypeQualifier>, TypeSpecifier)
}

/// Function parameter declarator.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionParameterDeclarator {
  pub ty: TypeSpecifier,
  pub name: Identifier,
  pub array_spec: Option<ArraySpecifier>
}

/// Init declarator list.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InitDeclaratorList {
  Single(SingleDeclaration),
  Complex(Box<InitDeclaratorList>, Identifier, Option<ArraySpecifier>, Option<Initializer>)
}

/// Single declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SingleDeclaration {
  pub ty: FullySpecifiedType,
  pub name: Option<Identifier>,
  pub array_specifier: Option<ArraySpecifier>,
  pub initializer: Option<Initializer>
}

/// Initializer.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Initializer {
  AssignmentExpr(Box<Expr>),
  List(Vec<Initializer>)
}

/// Field selection.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FieldSelection {
  pub field: Identifier,
  pub array_specifier: Option<ArraySpecifier>,
  pub next: Option<Box<FieldSelection>>
}

/// The most general form of an expression. As you can see if you read the variant list, in GLSL, an
/// assignment is an expression. This is a bit silly but think of an assignment as a statement first
/// then an expression which evaluates to what the statement “returns”.
///
/// An expression is either an assignment or a list (comma) of assignments.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
  /// A variable expression, using an identifier.
  Variable(Identifier),
  /// Integral constant expression.
  IntConst(String),
  /// Unsigned integral constant expression.
  UIntConst(String),
  /// Boolean constant expression.
  BoolConst(bool),
  /// Single precision floating expression.
  FloatConst(String),
  /// Double precision floating expression.
  DoubleConst(String),
  /// A unary expression, gathering a single expression and a unary operator.
  Unary(UnaryOp, Box<Expr>),
  /// A binary expression, gathering two expressions and a binary operator.
  Binary(BinaryOp, Box<Expr>, Box<Expr>),
  /// A ternary conditional expression, gathering three expressions.
  Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
  /// An assignment is also an expression. Gathers an expression that defines what to assign to, an
  /// assignment operator and the value to associate with.
  Assignment(Box<Expr>, AssignmentOp, Box<Expr>),
  /// Add an array specifier to an expression.
  Bracket(Box<Expr>, ArraySpecifier),
  /// A functional call. It has a function identifier and a list of expressions (arguments).
  FunCall(FunIdentifier, Vec<Expr>),
  /// An expression associated with a field selection (struct).
  Dot(Box<Expr>, FieldSelection),
  /// Post-incrementation of an expression.
  PostInc(Box<Expr>),
  /// Post-decrementation of an expression.
  PostDec(Box<Expr>),
  /// An expression that contains several, separated with comma.
  Comma(Box<Expr>, Box<Expr>)
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

/// All binary operators that exist in GLSL.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BinaryOp {
  Or,
  Xor,
  And,
  BitOr,
  BitXor,
  BitAnd,
  Equal,
  NonEqual,
  LT,
  GT,
  LTE,
  GTE,
  LShift,
  RShift,
  Add,
  Sub,
  Mult,
  Div,
  Mod,
}

/// All possible operators for assigning expressions.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AssignmentOp {
  Equal,
  Mult,
  Div,
  Mod,
  Add,
  Sub,
  LShift,
  RShift,
  And,
  Xor,
  Or
}

/// Starting rule.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TranslationUnit {
  ExternalDeclaration(ExternalDeclaration),
  Next(Box<TranslationUnit>)
}

/// External declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ExternalDeclaration {
  FunctionDefinition(FunctionDefinition),
  Declaration(Declaration)
}

/// Function definition.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionDefinition {
  pub prototype: FunctionPrototype,
  pub statement: CompoundStatement,
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct CompoundStatement {
  pub statement_list: Vec<Statement>
}

/// Statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Statement {
  Compound(Box<CompoundStatement>),
  Simple(Box<SimpleStatement>)
}

/// Simple statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SimpleStatement {
  Declaration(Declaration),
  Expression(ExprStatement),
  Selection(SelectionStatement),
  Switch(SwitchStatement),
  CaseLabel(CaseLabel),
  Iteration(IterationStatement),
  Jump(JumpStatement)
}

/// Expression statement.
pub type ExprStatement = Option<Expr>;

/// Selection statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SelectionStatement {
  If(Box<Expr>, SelectionRestStatement)
}

/// Condition.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Condition {
  Expr(Box<Expr>),
  Assignment(FullySpecifiedType, Identifier, Initializer)
}

/// Selection rest statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SelectionRestStatement {
  Statement(Box<Statement>),
  Else(Box<Statement>, Box<Statement>)
}

/// Switch statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SwitchStatement {
  pub head: Box<Expr>,
  pub body: Vec<Statement>
}

/// Case label statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CaseLabel {
  Case(Box<Expr>),
  Def
}

/// Iteration statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IterationStatement {
  While(Condition, Box<Statement>),
  DoWhile(Box<Statement>, Box<Expr>),
  For(ForInitStatement, ForRestStatement, Box<Statement>)
}

/// For init statement
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ForInitStatement {
  Expression(Option<Expr>),
  Declaration(Box<Declaration>)
}

/// For init statement
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ForRestStatement {
  pub condition: Option<Condition>,
  pub post_expr: Option<Box<Expr>>
}

/// Jump statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum JumpStatement {
  Continue,
  Break,
  Return(Box<Expr>),
  Discard
}
