#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate glsl;
extern crate nom;
use nom::IResult;
use std::str::from_utf8;

fuzz_target!(|data: &[u8]| {
    if from_utf8(data).is_ok() {
        let expr = glsl::parsers::expr(data);
        match expr {
            IResult::Done(_, expr) => {
                let mut output = String::new();
                glsl::transpiler::glsl::show_expr(&mut output, &expr);
                output.push(';');
                let readback_expr = glsl::parsers::expr(output.as_bytes());
                match readback_expr {
                    IResult::Done(_, readback_expr) => assert_eq!(expr, readback_expr),
                    _ => panic!("Failed to re-parse '{}'",
                                output),
                }
            }
            _ => {}
        }
    }
});
