use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process;

use serde_derive::Deserialize;
use strum_macros::Display;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use glsl::parser::Parse;
use glsl::syntax::TranslationUnit;

#[derive(Display, Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum TestResult {
  #[strum(serialize = "fail")]
  Fail,
  #[strum(serialize = "pass")]
  Pass,
}

#[derive(Debug, Deserialize)]
struct TestConfig {
  expect_result: TestResult,
  glsl_version: String,
}

fn main() -> anyhow::Result<()> {
  // Load the path to piglit from the first argument
  let piglit_path =
    env::args()
      .skip(1)
      .next()
      .map(PathBuf::from)
      .and_then(|p| if p.exists() { Some(p) } else { None });

  if piglit_path.is_none() {
    eprintln!("usage: cargo run --bin glsl-piglit -- piglit-path");
    process::exit(1);
  }

  let piglit_path = piglit_path.unwrap();

  // Output
  let mut stdout = StandardStream::stdout(ColorChoice::Auto);
  stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;

  // Count number of failures
  let mut failures = 0;
  let mut tests = 0;

  // Check all shaders from the piglit test suite
  for test_dir in &["tests/glslparsertest/shaders", "tests/glslparsertest/glsl2"] {
    let full_path = piglit_path.join(test_dir);

    for entry in fs::read_dir(full_path)? {
      // Read the dir entry
      let entry = entry?;

      // Get the relative path for showing results
      let relative_path = entry.path();
      let relative_path = relative_path.strip_prefix(&piglit_path)?;

      // Read source into memory
      let file_src = fs::read_to_string(entry.path())?;

      // Find the lines containing the config
      if let Some(start_marker) = file_src.find("[config]") {
        // Locate the prefix we'll need to remove
        let nl_before_start_marker = file_src[0..start_marker]
          .rfind('\n')
          .map(|p| p - 1)
          .unwrap_or(0);
        // The prefix is thus
        let prefix = &file_src[nl_before_start_marker..start_marker];
        // Find the config end line
        let end_marker = file_src
          .find("[end config]")
          .expect("failed to find [end config]");
        // Find the last byte before the end config
        let nl_before_end_marker = file_src[0..end_marker].rfind('\n').map(|p| p - 1).unwrap();
        // Find the start of the config after the marker
        let nl_after_start_marker = file_src[start_marker..end_marker]
          .find('\n')
          .map(|p| p + 1)
          .unwrap()
          + start_marker;

        // Build a buffer of the config
        let mut config_src = String::new();
        for l in file_src[nl_after_start_marker..nl_before_end_marker].lines() {
          if prefix.len() < l.len() {
            config_src.push_str(&l[prefix.len()..l.len()]);
            config_src.push('\n');
          }
        }

        // Parse the config
        let config: Result<TestConfig, _> = serde_yaml::from_str(&config_src);

        let config = match config {
          Ok(config) => config,
          Err(e) => {
            stdout.set_color(ColorSpec::new().set_fg(Some(Color::Blue)))?;
            failures += 1;
            tests += 1;

            writeln!(
              &mut stdout,
              "failed to parse config {}: {}\n",
              relative_path.display(),
              e,
            )?;

            continue;
          }
        };

        // Parse the source
        let result = TranslationUnit::parse(file_src);

        // Evaluate the test success
        let ok = match config.expect_result {
          TestResult::Pass => result.is_ok(),
          TestResult::Fail => result.is_err(),
        };

        // The error is useful for debugging failing tests
        let extra_res = match result {
          Ok(_) => format!("none"),
          Err(e) => format!("{}", e),
        };

        // Print result
        if ok {
          stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
        } else {
          stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
          failures += 1;
        }

        tests += 1;

        writeln!(
          &mut stdout,
          "should {} {}: {}\n    reason: {}\n",
          config.expect_result,
          relative_path.display(),
          if ok { "ok" } else { "not ok" },
          extra_res,
        )?;
      } else {
        stdout.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))?;
        writeln!(&mut stdout, "skip {}", relative_path.display())?;
      }
    }
  }

  if failures > 0 {
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Red)))?;
    writeln!(&mut stdout, "{} tests failed out of {}", failures, tests)?;
    process::exit(1);
  } else {
    stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)))?;
    writeln!(&mut stdout, "all {} tests succeeded", tests)?;
    process::exit(0);
  }
}
