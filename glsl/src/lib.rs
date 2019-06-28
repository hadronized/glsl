//! # GLSL compiler
//!
//! This crate is a GLSL450 compiler. It’s able to parse valid GLSL450 formatted source into an
//! abstract syntax tree (AST). That AST can then be transformed into SPIR-V, your own format or
//! even folded back to a raw GLSL [`String`] (think of a minifier, for instance).
//!
//! You’ll find several modules:
//!
//!   - [`parser`], which exports the parsing interface.
//!   - [`syntax`], which exports the AST and language definitions.
//!   - [`transpiler`], which provides you with GLSL transpilers.
//!   - [`visitor`](visitor), which gives you a way to visit AST nodes and mutate them, both with inner and
//!     outer mutation.
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
//!
//! The crate is also getting more and more combinators and functions to transform the AST or create
//! nodes with regular Rust. The [`Visitor`] trait will be a great friend of yours when you will
//! want to cope with deep mutation, filtering and validation.
//!
//! [`Parse`]: crate::parser::Parse
//! [`ExternalDeclaration`]: crate::syntax::ExternalDeclaration
//! [`TranslationUnit`]: crate::syntax::TranslationUnit
//! [`Visitor`]: crate::visitor::Visitor

pub mod parser;
mod parsers;
pub mod syntax;
pub mod transpiler;
pub mod visitor;
