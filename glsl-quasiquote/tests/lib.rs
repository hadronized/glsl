#![feature(proc_macro_hygiene)]

extern crate glsl;
#[macro_use] extern crate glsl_quasiquote;

#[test]
fn void_main_empty() {
  let _  = glsl!{void main() {}};
}

#[test]
fn understands_version_and_extension() {
  let _ = glsl!{
    #version 330 core
    #extension GL_foo_bar : require
    void main() {
    }
  };
}

#[test]
fn understands_define() {
  let _ = glsl!{
    #define foo 32
    void main() {
    }
  };
}

#[test]
fn fn_returns_int() {
  let _ = glsl!{
    int test() {
      return 3.;
    }
  };
}

#[test]
fn simple_struct() {
  let _ = glsl!{
    struct V {
      vec4 p;
      vec2 uv;
    };

    struct F {
      vec4 color;
    };
  };
}

#[test]
fn struct_several_ident_per_field() {
  let _ = glsl!{
    struct S {
      float a, b, c;
    };
  };
}

#[test]
fn struct_with_identifiers() {
  let _ = glsl!{
    struct S {
      float a, b, c;
    } foo, bar, zoo;
  };
}

#[test]
fn struct_with_arrayed_identifiers() {
  let _ = glsl!{
    struct S {
      float a, b, c;
    } foo[3], bar[12], zoo[];
  };
}

#[test]
fn typed_return() {
  let _ = glsl!{
    ReturnType foo() {
    }
  };
}
