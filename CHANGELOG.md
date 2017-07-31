## 0.2.3

> ?

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
