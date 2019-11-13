//! The Command Line Interface.

use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
pub struct CLI {
  // Path to the file to parse as GLSL.
  //
  // If no path is passed, read from stdin.
  #[structopt(short = "i", long = "input", default_value = "FileSource::Stdin")]
  glsl_file_path: FileSource,
}

#[derive(Debug)]
pub enum FileSource {
  Path(PathBuf),
  Stdin,
}

impl Default for FileSource {
  fn default() -> Self {
    FileSource::Stdin
  }
}
