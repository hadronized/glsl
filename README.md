# GLL 450 parser

[![Build Status](https://travis-ci.org/phaazon/glsl.svg?branch=master)](https://travis-ci.org/phaazon/glsl)
[![crates.io](https://img.shields.io/crates/v/glsl.svg)](https://crates.io/crates/glsl)

This is a [GLSL450](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.50.pdf) parser
which aims is to parse a GLSL450-formatted source into an in-memory representation (AST). It
strictly follows the grammar rules defined in the official GLSL 450 specifications.

Currently, the parser expects the input source to be `&[u8]`, which provides a pretty poor
experience in error reporting. This is a serious issue and is being worked on.
