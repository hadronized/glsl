use glsl::parser::Parse as _;
use glsl::syntax::ShaderStage;
use std::io;
use std::process::exit;

fn main() {
  let mut content = String::new();

  match io::stdin().read_line(&mut content) {
    Ok(_) => match ShaderStage::parse(&content) {
      Ok(ast) => println!("{:#?}", ast),

      Err(err) => {
        eprintln!("cannot parse GLSL:\n{}", err);
        exit(1);
      }
    },

    Err(err) => {
      eprintln!("cannot read from stdin: {}", err);
      exit(2);
    }
  }
}
