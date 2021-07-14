#![feature(proc_macro_hygiene)]

extern crate glsl;
#[macro_use]
extern crate glsl_quasiquote;

#[test]
fn void_main_empty() {
  let _ = glsl! {void main() {}};
}

#[test]
fn understands_version_and_extension() {
  let _ = glsl! {
    #version 330 core
    #extension GL_foo_bar : require
    void main() {
    }
  };
}

#[test]
fn understands_pp_define_undef() {
  let _ = glsl! {
    #define foo 32
    #undef foo
    void main() {
    }
  };
}

#[test]
fn understands_pp_tests() {
  let _ = glsl! {
    #else
    #elif 0
    #endif
    #if 0
    #ifdef foo
    #ifndef foo
    void main() {
    }
  };
}

#[test]
fn understands_pp_files() {
  let _ = glsl! {
    #include <filename>
    #include "filename"
    #line 2
    #line 2 4
    void main() {
    }
  };
}

#[test]
fn understands_pp_error() {
  let _ = glsl! {
    #error some command
    void main() {
    }
  };
}

#[test]
fn understands_pp_pragma() {
  let _ = glsl! {
    #pragma some flag
    void main() {
    }
  };
}

#[test]
fn fn_returns_int() {
  let _ = glsl! {
    int test() {
      return 3.;
    }
  };
}

#[test]
fn simple_struct() {
  let _ = glsl! {
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
  let _ = glsl! {
    struct S {
      float a, b, c;
    };
  };
}

#[test]
fn struct_with_identifiers() {
  let _ = glsl! {
    struct S {
      float a, b, c;
    } foo, bar, zoo;
  };
}

#[test]
fn struct_with_arrayed_identifiers() {
  let _ = glsl! {
    struct S {
      float a, b, c;
    } foo[3], bar[12], zoo[];
  };
}

#[test]
fn typed_return() {
  let _ = glsl! {
    ReturnType foo() {
    }
  };
}

#[test]
fn dot_expr() {
  let _ = glsl! {
    void main() {
      let x = foo.xyz;
      let y = 1.;
      let z = .3;
    }
  };
}
