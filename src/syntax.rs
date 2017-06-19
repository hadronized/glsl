// FIXME: as soon as deeply-nested types are truly supported in rustc, remove as many boxes as
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
  Identifier(Identifier, Option<ConstExpr>),
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
  ExplicitlySized(IntegerExpr)
}

/// The most general form of an expression. As you can see if you read the variant list, in GLSL, an
/// assignment is an expression. This is a bit silly but think of an assignment as a statement first
/// then an expression which evaluates to what the statement “returns”.
///
/// An expression is either an assignment or a list (comma) of assignments.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
  Assignment(Box<AssignmentExpr>),
  Comma(Box<Expr>, Box<AssignmentExpr>)
}

/// An integer expression. Usually used to index an array or specify a binding index.
pub type IntegerExpr = Expr;

/// Assignment expression. It’s either a conditional expression or an assignment that augments a
/// unary expression with another assignment expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AssignmentExpr {
  Cond(Box<CondExpr>),
  Assignment(Box<UnaryExpr>, AssignmentOp, Box<AssignmentExpr>)
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

/// Constant expression.
pub type ConstExpr = CondExpr;

/// Logical expression. It’s either a ternary operator use (cond ? a : b) or a logical OR
/// expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CondExpr {
  LogicalOrExpr(Box<LogicalOrExpr>),
  Ternary(Box<LogicalOrExpr>, Box<Expr>, Box<AssignmentExpr>)
}

/// Logical OR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalOrExpr {
  LogicalXorExpr(Box<LogicalXorExpr>),
  Or(Box<LogicalOrExpr>, Box<LogicalXorExpr>)
}

/// Logical XOR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalXorExpr {
  LogicalAndExpr(Box<LogicalAndExpr>),
  Xor(Box<LogicalXorExpr>, Box<LogicalAndExpr>)
}

/// Logical AND expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LogicalAndExpr {
  InclusiveOrExpr(Box<InclusiveOrExpr>),
  And(Box<LogicalAndExpr>, Box<InclusiveOrExpr>)
}

/// Inclusive OR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InclusiveOrExpr {
  ExclusiveOrExpr(Box<ExclusiveOrExpr>),
  InclusiveOr(Box<InclusiveOrExpr>, Box<ExclusiveOrExpr>)
}

/// Exclusive OR expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ExclusiveOrExpr {
  AndExpr(Box<AndExpr>),
  ExclusiveOr(Box<ExclusiveOrExpr>, Box<AndExpr>)
}

/// AND expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AndExpr {
  EqualityExpr(Box<EqualityExpr>),
  And(Box<AndExpr>, Box<EqualityExpr>)
}

/// Equality expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum EqualityExpr {
  RelExpr(Box<RelExpr>),
  Equality(Box<EqualityExpr>, Box<RelExpr>),
  NonEquality(Box<EqualityExpr>, Box<RelExpr>)
}

/// Relational expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RelExpr {
  ShiftExpr(Box<ShiftExpr>),
  LessThan(Box<RelExpr>, Box<ShiftExpr>),
  GreaterThan(Box<RelExpr>, Box<ShiftExpr>),
  LessThanOrEqual(Box<RelExpr>, Box<ShiftExpr>),
  GreaterThanOrEqual(Box<RelExpr>, Box<ShiftExpr>),
}

/// Shift expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ShiftExpr {
  AdditiveExpr(Box<AdditiveExpr>),
  Left(Box<ShiftExpr>, Box<AdditiveExpr>),
  Right(Box<ShiftExpr>, Box<AdditiveExpr>),
}

/// Additive expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AdditiveExpr {
  MultExpr(Box<MultExpr>),
  Plus(Box<AdditiveExpr>, Box<MultExpr>),
  Dash(Box<AdditiveExpr>, Box<MultExpr>),
}

/// Multiplicative expression.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MultExpr {
  UnaryExpr(Box<UnaryExpr>),
  Star(Box<MultExpr>, Box<UnaryExpr>),
  Slash(Box<MultExpr>, Box<UnaryExpr>),
  Percent(Box<MultExpr>, Box<UnaryExpr>),
}

/// Unary expression. Unary expressions are formed from postfix expressions and augmented via
/// prefixes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum UnaryExpr {
  Unary(Box<PostfixExpr>),
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

/// A declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Declaration {
  FunctionPrototype(FunctionPrototype),
  InitDeclaratorList(InitDeclaratorList),
  Precision(PrecisionQualifier, TypeSpecifier),
  Struct(StructSpecifier, Option<(Identifier, Option<ArraySpecifier>)>),
  ForwardDecl(TypeSpecifier, Vec<Identifier>),
}

/// Postfix expression. Postfix expressions are formed from primay expressions and extend them
/// with suffix.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PostfixExpr {
  Primary(Box<PrimaryExpr>),
  Bracket(Box<PostfixExpr>, Box<IntegerExpr>),
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

/// Function prototype.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionPrototype {
  ty: FullySpecifiedType,
  name: Identifier,
  parameters: Vec<FunctionParameterDeclaration>
}

/// Function parameter declaration.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FunctionParameterDeclaration {
  Named(Option<TypeQualifier>, FunctionParameterDeclarator),
  Unamed(Option<TypeQualifier>, TypeSpecifier)
}

/// Function parameter declarator.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct FunctionParameterDeclarator {
  ty: TypeSpecifier,
  name: Identifier,
  array_spec: Option<ArraySpecifier>
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
  pub name: Identifier,
  pub array_specifier: Option<ArraySpecifier>,
  pub initializer: Option<Initializer>
}

/// Initializer.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Initializer {
  AssignmentExpr(AssignmentExpr),
  Comma(Box<Initializer>, Box<Initializer>)
}

/// Field selection.
pub enum FieldSelection {
  Field(Identifier, Option<ArraySpecifier>, Option<Box<FieldSelection>>)
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
  prototype: FunctionPrototype,
  statement: CompoundStatementNoNewScope,
}

/// Statement (with no new scope).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum StatementNoNewScope {
  Compound(CompoundStatementNoNewScope),
  SimpleStatement(SimpleStatement)
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CompoundStatement {
  Empty,
  StatementList(StatementList)
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CompoundStatementNoNewScope {
  Empty,
  StatementList(StatementList)
}

/// Statement list.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum StatementList {
  Statement(Statement),
  Cons(Statement, Box<StatementList>)
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
  Declaration(DeclarationStatement),
  Expression(ExpressionStatement),
  Selection(SelectionStatement),
  Switch(SwitchStatement),
  CaseLabel(CaseLabel),
  Iteration(IterationStatement),
  Jump(JumpStatement)
}

/// Declaration statement.
pub type DeclarationStatement = Declaration;

/// Expression statement.
pub type ExpressionStatement = Vec<Expr>;

/// Selection statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum SelectionStatement {
  If(Expr, Box<SelectionRestStatement>)
}

/// Condition.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Condition {
  Expr(Expr),
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
pub enum SwitchStatement {
  Switch(Expr, SwitchStatementList)
}

/// Switch statement list.
pub type SwitchStatementList = Option<Box<StatementList>>;

/// Case label statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum CaseLabel {
  Case(Expr),
  Def
}

/// Iteration statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IterationStatement {
  While(Expr, Box<StatementNoNewScope>),
  DoWhile(Box<Statement>, Expr),
  For(ForInitStatement, ForRestStatement, Box<StatementNoNewScope>)
}

/// For init statement
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ForInitStatement {
  Expression(Box<ExpressionStatement>),
  Declaration(Box<DeclarationStatement>)
}

/// For init statement
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ForRestStatement {
  condition: Option<Condition>,
  expr: Option<Expr>
}

/// Jump statement.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum JumpStatement {
  Continue,
  Break,
  Return(Expr),
  Discard
}
