# GLL 450 parser

[![Build Status](https://travis-ci.org/phaazon/glsl.svg?branch=master)](https://travis-ci.org/phaazon/glsl)
[![crates.io](https://img.shields.io/crates/v/glsl.svg)](https://crates.io/crates/glsl)

This is a [GLSL450](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.50.pdf) parser
which aims is to parse a GLSL450-formatted source into an in-memory representation (AST). It
strictly follows the grammar rules defined in the official GLSL 450 specifications.

Currently, the parser expects the input source to be `&[u8]`, which provides a pretty poor
experience in error reporting. This is a serious issue and is being worked on.

# Hacking and contributing

If you wished to contribute, feel free to have a look at the open issues and submit a PR.

On a general note, if no issue interests you, you can open the [parser.rs](src/parser.rs) file and
look for a parser function which is annotated with documentation but private (no `pub` yet). If you
find such a function, you can :

1. add the `pub` keyword to make the parser available;
2. add a set of tests in the [tests/](tests) folder.

The later point is actually mandatory as the crate won’t make it to its initial release untill all
parsers that must be public have their associated tests.
