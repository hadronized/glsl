//! # GLSL compiler
//!
//! This crate is a GLSL450 compiler. It’s able to parse valid GLSL450 formatted source into an
//! abstract syntax tree (AST). That AST can then be transformed into SPIR-V, your own format or
//! even folded back to a raw GLSL `String` (think of a minifier, for instance).
//!
//! You’ll find several modules:
//!
//!   - `parser`, which exports the parsing interface.
//!   - `syntax`, which exports the AST and language definitions.
//!   - `transpiler`, which provides you with GLSL transpilers.
//!
//! Feel free to inspect those modules for further information.
//!
//! # Parsing architecture
//!
//! Basically, the [`Parse`] trait gives you all you need to start parsing. This crate is designed
//! around the concept of type-driven parsing: parsers are hidden and you just have to state what
//! result type you expect.
//!
//! The most common type you want to parse to is [`TranslationUnit`], which represents a set of
//! [`ExternalDeclaration`]s. An [`ExternalDeclaration`] is just a declaration at the top-most level
//! of a shader. It can be a global, uniform declarations, vertex attributes, a function, a
//! structure, etc.

#[macro_use]
extern crate nom;

pub mod parser;
mod parsers;
pub mod syntax;
pub mod transpiler;
