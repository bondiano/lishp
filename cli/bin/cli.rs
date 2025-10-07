use clap::Parser;
use cli::{Cli, Commands, run_eval, run_file, run_files, run_repl};
use colored::*;
use std::process;

fn main() {
  let cli = Cli::parse();

  let result = match cli.command {
    Some(Commands::Repl) | None => run_repl(),
    Some(Commands::Run { eval, files }) => match (eval, files.is_empty()) {
      (Some(expr), _) => run_eval(&expr),
      (None, false) => {
        if files.len() == 1 {
          run_file(&files[0])
        } else {
          run_files(&files)
        }
      }
      (None, true) => {
        eprintln!(
          "{} Either --eval or file paths must be provided",
          "Error:".red().bold()
        );
        process::exit(1);
      }
    },
  };

  if let Err(e) = result {
    eprintln!("{} {}", "Error:".red().bold(), e);
    process::exit(1);
  }
}
