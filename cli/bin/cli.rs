use clap::Parser;
use cli::{run_eval, run_file, run_files, run_repl, Cli, Commands};
use colored::*;
use std::{process, thread};

fn main() {
  let cli = Cli::parse();

  let result = match cli.command {
    Some(Commands::Repl) | None => {
      let builder = thread::Builder::new().stack_size(64 * 1024 * 1024); // 64 mb

      let handler = builder
        .spawn(run_repl)
        .expect("Failed to spawn REPL thread");

      handler
        .join()
        .map_err(|_| "Failed to join REPL thread".to_string())
    }
    Some(Commands::Run { eval, files }) => Ok(match (eval, files.is_empty()) {
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
    }),
  };

  if let Err(e) = result {
    eprintln!("{} {}", "Error:".red().bold(), e);
    process::exit(1);
  }
}
