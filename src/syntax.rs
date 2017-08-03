//! GLSL abstract syntax tree and grammar.
//!
//! This module exports all the grammar syntax that defines GLSL. You’ll be handling ASTs
//! representing your GLSL source.
//!
//! The most external form of a GLSL parsed AST is `TranslationUnit` (a shader). Some part of the
//! tree are *boxed*. This is due to the two facts
//!
//! - recursion is used, hence we need a way to give our types a static size
//! - because of some very deep variants, runtime size would explode if no indirection weren’t
//!   in place
//!
//! The types are commented so feel free to inspect each of theme. As a starter, you should read
//! the documentation of `Expr`, `FunctionDefinition`, `Statement` and `TranslationUnit`.

// FIXME: as soon as deeply-nested types are truly supported in rustc, remove as many boxes as
// possible. See <https://github.com/rust-lang/rust/issues/42747>.

/// A non-empty `Vec`. It has at least one element.
pub type NonEmpty<T> = Vec<T>;

/// A generic identifier.
pub type Identifier = String;

/// Any type name.
pub type TypeName = String;

/// Type specifier.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
  // transparent types
  Void,
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
#[derive(Clone, Debug, PartialEq)]
pub struct StructSpecifier {
  pub name: Option<String>,
  pub fields: Vec<StructFieldSpecifier>,
}

/// Struct field specifier. Used to add fields to struct specifiers.
#[derive(Clone, Debug, PartialEq)]
pub struct StructFieldSpecifier {
  pub qualifier: Option<TypeQualifier>,
  pub ty: TypeSpecifier,
  pub identifiers: Vec<(Identifier, Option<ArraySpecifier>)> // several identifiers of the same type
}

/// Type qualifier.
#[derive(Clone, Debug, PartialEq)]
pub struct TypeQualifier {
  pub qualifiers: NonEmpty<TypeQualifierSpec>
}

/// Type qualifier spec.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifierSpec {
  Storage(StorageQualifier),
  Layout(LayoutQualifier),
  Precision(PrecisionQualifier),
  Interpolation(InterpolationQualifier),
  Invariant,
  Precise
}

/// Storage qualifier.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct LayoutQualifier {
  pub ids: NonEmpty<LayoutQualifierSpec>
}

/// Layout qualifier spec.
#[derive(Clone, Debug, PartialEq)]
pub enum LayoutQualifierSpec {
  Identifier(Identifier, Option<Box<Expr>>),
  Shared
}

/// Precision qualifier.
#[derive(Clone, Debug, PartialEq)]
pub enum PrecisionQualifier {
  High,
  Medium,
  Low
}

/// Interpolation qualifier.
#[derive(Clone, Debug, PartialEq)]
pub enum InterpolationQualifier {
  Smooth,
  Flat,
  NoPerspective
}

/// Fully specified type.
#[derive(Clone, Debug, PartialEq)]
pub struct FullySpecifiedType {
  pub qualifier: Option<TypeQualifier>,
  pub ty: TypeSpecifier
}

/// Dimensionality of an arary.
#[derive(Clone, Debug, PartialEq)]
pub enum ArraySpecifier {
  Unsized,
  ExplicitlySized(Box<Expr>)
}

/// A declaration.
#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
  FunctionPrototype(FunctionPrototype),
  InitDeclaratorList(InitDeclaratorList),
  Precision(PrecisionQualifier, TypeSpecifier),
  Block(Block),
  Global(TypeQualifier, Vec<Identifier>)
}

/// A general purpose block, containing fields and possibly a list of declared identifiers. Semantic
/// is given with the storage qualifier.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
  pub qualifier: TypeQualifier,
  pub name: Identifier,
  pub fields: Vec<StructFieldSpecifier>, 
  pub identifier: Option<(Identifier, Option<ArraySpecifier>)>
}

/// Function identifier.
#[derive(Clone, Debug, PartialEq)]
pub enum FunIdentifier {
  Identifier(Identifier),
  //Expr(Box<Expr>)
}

/// Function prototype.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionPrototype {
  pub ty: FullySpecifiedType,
  pub name: Identifier,
  pub parameters: Vec<FunctionParameterDeclaration>
}

/// Function parameter declaration.
#[derive(Clone, Debug, PartialEq)]
pub enum FunctionParameterDeclaration {
  Named(Option<TypeQualifier>, FunctionParameterDeclarator),
  Unnamed(Option<TypeQualifier>, TypeSpecifier)
}

/// Function parameter declarator.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionParameterDeclarator {
  pub ty: TypeSpecifier,
  pub name: Identifier,
  pub array_spec: Option<ArraySpecifier>
}

/// Init declarator list.
#[derive(Clone, Debug, PartialEq)]
pub struct InitDeclaratorList {
  pub head: SingleDeclaration,
  pub tail: Vec<SingleDeclarationNoType>
}

// FIXME: the three fields are wrong. It’s not possible to have the last two if the second one
// is not Some(_) – see page 197 of the GLSLangSpec.4.50.pdf document.
/// Single declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct SingleDeclaration {
  pub ty: FullySpecifiedType,
  pub name: Option<Identifier>,
  pub array_specifier: Option<ArraySpecifier>,
  pub initializer: Option<Initializer>
}

/// A single declaration with implicit, already-defined type.
#[derive(Clone, Debug, PartialEq)]
pub struct SingleDeclarationNoType {
  pub name: Identifier,
  pub array_specifier: Option<ArraySpecifier>,
  pub initializer: Option<Initializer>
}

/// Initializer.
#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
  Simple(Box<Expr>),
  List(Vec<Initializer>)
}

/// The most general form of an expression. As you can see if you read the variant list, in GLSL, an
/// assignment is an expression. This is a bit silly but think of an assignment as a statement first
/// then an expression which evaluates to what the statement “returns”.
///
/// An expression is either an assignment or a list (comma) of assignments.
#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
  /// A variable expression, using an identifier.
  Variable(Identifier),
  /// Integral constant expression.
  IntConst(i32),
  /// Unsigned integral constant expression.
  UIntConst(u32),
  /// Boolean constant expression.
  BoolConst(bool),
  /// Single precision floating expression.
  FloatConst(f32),
  /// Double precision floating expression.
  DoubleConst(f64),
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
  Dot(Box<Expr>, Identifier),
  /// Post-incrementation of an expression.
  PostInc(Box<Expr>),
  /// Post-decrementation of an expression.
  PostDec(Box<Expr>),
  /// An expression that contains several, separated with comma.
  Comma(Box<Expr>, Box<Expr>)
}

/// All unary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
  Inc,
  Dec,
  Add,
  Minus,
  Not,
  Complement
}

/// All binary operators that exist in GLSL.
#[derive(Clone, Debug, PartialEq)]
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
  Mod
}

/// All possible operators for assigning expressions.
#[derive(Clone, Debug, PartialEq)]
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
pub type TranslationUnit = NonEmpty<ExternalDeclaration>;

/// External declaration.
#[derive(Clone, Debug, PartialEq)]
pub enum ExternalDeclaration {
  Preprocessor(Preprocessor),
  FunctionDefinition(FunctionDefinition),
  Declaration(Declaration)
}

/// Function definition.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDefinition {
  pub prototype: FunctionPrototype,
  pub statement: CompoundStatement,
}

/// Compound statement (with no new scope).
#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStatement {
  pub statement_list: Vec<Statement>
}

/// Statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
  Compound(Box<CompoundStatement>),
  Simple(Box<SimpleStatement>)
}

/// Simple statement.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
pub struct SelectionStatement {
  pub cond: Box<Expr>,
  pub rest: SelectionRestStatement
}

/// Condition.
#[derive(Clone, Debug, PartialEq)]
pub enum Condition {
  Expr(Box<Expr>),
  Assignment(FullySpecifiedType, Identifier, Initializer)
}

/// Selection rest statement.
#[derive(Clone, Debug, PartialEq)]
pub enum SelectionRestStatement {
  Statement(Box<Statement>),
  Else(Box<Statement>, Box<Statement>)
}

/// Switch statement.
#[derive(Clone, Debug, PartialEq)]
pub struct SwitchStatement {
  pub head: Box<Expr>,
  pub body: Vec<Statement>
}

/// Case label statement.
#[derive(Clone, Debug, PartialEq)]
pub enum CaseLabel {
  Case(Box<Expr>),
  Def
}

/// Iteration statement.
#[derive(Clone, Debug, PartialEq)]
pub enum IterationStatement {
  While(Condition, Box<Statement>),
  DoWhile(Box<Statement>, Box<Expr>),
  For(ForInitStatement, ForRestStatement, Box<Statement>)
}

/// For init statement.
#[derive(Clone, Debug, PartialEq)]
pub enum ForInitStatement {
  Expression(Option<Expr>),
  Declaration(Box<Declaration>)
}

/// For init statement.
#[derive(Clone, Debug, PartialEq)]
pub struct ForRestStatement {
  pub condition: Option<Condition>,
  pub post_expr: Option<Box<Expr>>
}

/// Jump statement.
#[derive(Clone, Debug, PartialEq)]
pub enum JumpStatement {
  Continue,
  Break,
  Return(Box<Expr>),
  Discard
}

/// Some basic preprocessor commands.
///
/// As it’s important to carry them around the AST because they cannot be substituted in a normal
/// preprocessor (they’re used by GPU’s compilers), those preprocessor commands are available for
/// inspection.
///
/// > Important note: others preprocessor commands can be used in your source. However, they’ll get
/// > substituted upfront. For instance, if you use have `#define foo 42` defined in your file, 
/// > every occurrence to `foo` will get replaced by `42` and then treated as a normal GLSL
/// > expression (in that case, ending as an `Expr::IntConst(42)` value). This might be unfortunate
/// > for people seeking minimal size. However, you’re free to use a minifier aftewards to re-enable
/// >_that kind of feature. To be honest, it’s not worth it to interleave the AST with preprocessor
/// > command annotations just so that the resulting code size is minimal. Just use a minifier.
#[derive(Clone, Debug, PartialEq)]
pub enum Preprocessor {
  Version(PreprocessorVersion),
  Extension(PreprocessorExtension)
}

/// A #version preprocessor command.
#[derive(Clone, Debug, PartialEq)]
pub struct PreprocessorVersion {
  pub version: u16,
  pub profile: Option<PreprocessorVersionProfile>
}

/// A #version profile annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum PreprocessorVersionProfile {
  Core,
  Compatibility,
  ES
}

/// An #extension preprocessor command.
#[derive(Clone, Debug, PartialEq)]
pub struct PreprocessorExtension {
  pub name: PreprocessorExtensionName,
  pub behavior: Option<PreprocessorExtensionBehavior>
}

/// An #extension name annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum PreprocessorExtensionName {
  /// All extensions you could ever imagine in your whole lifetime (how crazy is that!).
  All,
  /// A specific extension.
  Specific(String)
}

/// An #extension behavior annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum PreprocessorExtensionBehavior {
  Require,
  Enable,
  Warn,
  Disable
}
