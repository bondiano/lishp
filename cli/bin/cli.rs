use clap::Parser;
use cli::{Cli, Commands, run_repl};
use colored::*;
use std::process;

fn main() {
  let cli = Cli::parse();

  let result = match cli.command {
    Some(Commands::Repl) | None => run_repl().map_err(|e| e.to_string()),
  };

  if let Err(e) = result {
    eprintln!("{} {}", "Error:".red().bold(), e);
    process::exit(1);
  }
}
