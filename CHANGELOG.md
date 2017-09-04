## 0.6.5

> Monday, September, 4th 2017

- Fixed the formatting of floating values when the fractional part is `0`.

## 0.6.4

> Monday, September, 4th 2017

- Fixed the output for `show_struct_specifier`.

## 0.6.3

> Monday, September, 4th 2017

- Fixed the output for `show_struct_specifier`.

## 0.6.2

> Monday, September, 4th 2017

- Removed a warning.

## 0.6.1

> Monday, September, 4th 2017

- Fixed `show_struct_specifier`.

# 0.6.0

> Friday, September, 1st 2017

- The `TypeSpecifier` type was wrong as it didn’t carry any `ArraySpecifier` information while the
  GLSL specification’s grammar about type specifiers states they should. Fixed.

# 0.5.0

> Monday, August, 7th 2017

- The `parse` and `parse_str` functions now take as second argument the parser to run. This enables
  using those functions and all the neat logic the wrap in dependent projects.

## 0.4.2

> Friday, August, 4th 2017

- A GLSL writer is now available.
- Some parsers yield non-empty list of syntax trees. Those had the incorrect `Vec` type. They were
  replaced by `NonEmpty`, which is an alias to `Vec`, but carry the semantic that it has at least
  one element in it.

## 0.4.1

> Thursday, August, 3rd 2017

- Uni/multi-line comments are now supported.

# 0.4.0

> Wednesday, August, 2nd 2017

- The `Declaration::Block` variant was refactored for a better usage.
- Dot field selections and, in a mory general way, postfix expressions completely fixed. The
  `syntax` module was altered to make it easier to work with dot field selection. Also related,
  the function identifier syntax is now isomorphic to an identifier.

## 0.3.1

> Tuesday, August, 1st 2017

- Fixed the `preprocessor` parser so that it eats surrounding blanks.

# 0.3.0

> Monday, July, 31st 2017

- Added a very minimalistic yet working preprocessor. It parses `#version` and `#extension`
  commands. Those have to be declared at the top of your file, even though this implementation
  accepts them at any place an external declaration could be defined. Feel free to submit a PR
  if you want to change that behavior, I don’t really mind.
- Enhance the runtime error reporting. It’s not perfect, but it’s way better than before!
- `void` is now recognized as `TypeSpecifier::Void` instead of the erroneous
  `TypeSpecifier::TypeName("void")`.

## 0.2.2

> Monday, July, 31st 2017

- The `layout` parser had a nasty bug that would treat a list of key-value pairs as an expression
  assignment. This was fixed and it now treats it as a list of pairs of identifier associated with a
  possible constant expression.
- The `StructFieldSpecifier` type and its associated parser were wrong. Was missing:
  + the type qualifier
  + for each identifier defined in the field specifier, its optional array specifier, as in
    `float foo[3];` or `vec3 bar[];` for unsized ones.

## 0.2.1

> Sunday, July, 30th 2017

- More documentation to help people to get their feet wet.

# 0.2.0

> Saturday, July, 29th 2017

- The whole parsing API is public.

# 0.1.0

- Initial revision.
