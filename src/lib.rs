//! # GLSL compiler
//!
//! This crate is a GLSL450 compiler. It’s able to parse valid GLSL450 formatted source into an
//! abstract syntax tree (AST). That AST can then be transformed into SPIR-V, your own format or
//! even folded back to a raw GLSL `String` (think of a minifier, for instance).
//!
//! You’ll find two main modules:
//!
//! - `parser`, which exports most of the parsers to parse the whole or a part of GLSL source
//!   (intermediary parsers)
//! - `syntax`, which exports the AST
//!
//! Feel free to inspect those modules for further information.
//!
//! # Quick parsing
//!
//! If you’re just looking for a parser that would give you the AST for a shader, you might be
//! interested in `translation_unit`.
#[macro_use]
extern crate nom;

pub mod parser;
pub mod syntax;
pub mod writer;
