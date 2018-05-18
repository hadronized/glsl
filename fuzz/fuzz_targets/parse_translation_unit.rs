#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate glsl;

fuzz_target!(|data: &[u8]| {
    glsl::parser::translation_unit(data);
});
