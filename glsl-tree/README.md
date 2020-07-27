# A debug / GLSL tree (AST) viewer

This small program is an AST viewer that aims to help people debug what is going on while parsing
an input GLSL string.

The binary reads the GLSL input on _stdin_ and outputs the parsed tree on _stdout_ if succeeded, or
prints errors on _stderr_.
