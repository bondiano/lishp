mod repl;

use clap::{Parser, Subcommand};
use lishp::{Evaluator, StdioAdapter, parser};

use crate::repl::ReplSession;

#[derive(Parser)]
#[command(name = "lishp")]
#[command(about = "Lishp - A Lisp interpreter", long_about = None)]
#[command(version)]
pub struct Cli {
  #[command(subcommand)]
  pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
  /// Start an interactive REPL
  Repl,
  /// Run Lishp code
  Run {
    /// Evaluate expression from command line
    #[arg(short, long)]
    eval: Option<String>,
    /// Run Lishp files (multiple files will share context)
    #[arg(value_name = "FILES")]
    files: Vec<String>,
  },
}

pub fn run_repl() -> Result<(), String> {
  let mut session = ReplSession::new().map_err(|e| format!("Failed to initialize REPL: {}", e))?;
  session.run().map_err(|e| format!("REPL error: {}", e))
}

pub fn evaluate_expression(input: &str) -> Result<String, String> {
  let mut remaining = input;
  let mut results = Vec::new();
  let mut io = StdioAdapter::new();
  let mut env = lishp::Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  loop {
    match parser::parse(remaining) {
      Ok(Some((value, rest))) => {
        let evaluated = evaluator
          .eval(&value)
          .map_err(|e| format!("Evaluation error: {}", e))?;
        let result = format!("{}", evaluated);
        results.push(result);
        remaining = rest;

        let trimmed = remaining.trim();
        if trimmed.is_empty() {
          break;
        }
        remaining = trimmed;
      }
      Ok(None) => break,
      Err(e) => return Err(format!("{:?}", e)),
    }
  }

  Ok(results.last().cloned().unwrap_or_else(|| "nil".to_string()))
}

pub fn run_eval(expression: &str) -> Result<(), String> {
  let result = evaluate_expression(expression)?;
  println!("{}", result);
  Ok(())
}

pub fn run_file(path: &str) -> Result<(), String> {
  let contents =
    std::fs::read_to_string(path).map_err(|e| format!("Failed to read file '{}': {}", path, e))?;
  evaluate_expression(&contents)?;
  Ok(())
}

pub fn run_files(paths: &[String]) -> Result<(), String> {
  for path in paths {
    let contents = std::fs::read_to_string(path)
      .map_err(|e| format!("Failed to read file '{}': {}", path, e))?;
    evaluate_expression(&contents)?;
  }

  Ok(())
}
