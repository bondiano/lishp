mod repl;

use clap::{Parser, Subcommand};
use lishp::{parser, Environment, Evaluator, StdioAdapter};
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;

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
    /// Evaluate expression from the command line
    #[arg(short, long)]
    eval: Option<String>,
    /// Run Lishp files (multiple files will share context)
    #[arg(value_name = "FILES")]
    files: Vec<String>,
  },
}

fn get_std_lib_path() -> Option<PathBuf> {
  if let Ok(path) = std::env::var("LISHP_STD_PATH") {
    let path_buf = PathBuf::from(path);
    if path_buf.exists() {
      return Some(path_buf);
    }
  }

  let mut default_paths = vec![];

  default_paths.push(PathBuf::from("std.lsp"));

  if let Some(mut home) = dirs::home_dir() {
    home.push(".lishp");
    home.push("std.lsp");
    default_paths.push(home);
  }

  if let Ok(mut exe_path) = std::env::current_exe() {
    exe_path.pop();
    exe_path.push("std.lsp");
    default_paths.push(exe_path);
  }

  default_paths.into_iter().find(|p| p.exists())
}

/// Load the standard library into the given environment
pub fn load_std_lib(env: &mut Environment) {
  if let Some(std_path) = get_std_lib_path()
    && let Ok(contents) = std::fs::read_to_string(&std_path)
  {
    let mut io = StdioAdapter::new();
    let env_rc = Rc::new(RefCell::new(std::mem::replace(env, Environment::new())));
    let mut evaluator = Evaluator::with_environment(&mut io, env_rc.clone());

    let restore_env = || {
      let final_refcell =
        Rc::try_unwrap(env_rc).unwrap_or_else(|rc| RefCell::new((*rc.borrow()).clone()));
      *env = final_refcell.into_inner();
    };

    let mut remaining = contents.as_str();
    while let Ok(Some((value, rest))) = parser::parse(remaining) {
      if let Err(e) = evaluator.eval(&value) {
        eprintln!("Error loading std.lsp: {}", e);
        std::process::exit(1);
      }
      remaining = rest.trim();
      if remaining.is_empty() {
        break;
      }
    }

    restore_env();
  }
}

pub fn run_repl() -> Result<(), String> {
  let mut session = ReplSession::new().map_err(|e| format!("Failed to initialize REPL: {}", e))?;
  session.run().map_err(|e| format!("REPL error: {}", e))
}

pub fn evaluate_expression(input: &str, env: Rc<RefCell<Environment>>) -> Result<String, String> {
  let mut remaining = input;
  let mut results = Vec::new();
  let mut io = StdioAdapter::new();
  let mut evaluator = Evaluator::with_environment(&mut io, env);

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
  let env = Rc::new(RefCell::new(Environment::new()));
  load_std_lib(&mut env.borrow_mut());
  let result = evaluate_expression(expression, env)?;
  println!("{}", result);
  Ok(())
}

pub fn run_file(path: &str) -> Result<(), String> {
  let env = Rc::new(RefCell::new(Environment::new()));
  load_std_lib(&mut env.borrow_mut());
  let contents =
    std::fs::read_to_string(path).map_err(|e| format!("Failed to read file '{}': {}", path, e))?;
  evaluate_expression(&contents, env)?;
  Ok(())
}

pub fn run_files(paths: &[String]) -> Result<(), String> {
  let env = Rc::new(RefCell::new(Environment::new()));
  load_std_lib(&mut env.borrow_mut());

  for path in paths {
    let contents = std::fs::read_to_string(path)
      .map_err(|e| format!("Failed to read file '{}': {}", path, e))?;
    evaluate_expression(&contents, env.clone())?;
  }

  Ok(())
}
