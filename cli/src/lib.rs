mod repl;

use clap::{Parser, Subcommand};

use crate::repl::ReplSession;

#[derive(Parser)]
#[command(name = "lishp")]
#[command(version)]
pub struct Cli {
  #[command(subcommand)]
  pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
  /// Start an interactive REPL
  Repl,
}

pub fn run_repl() -> Result<(), String> {
  let mut session = ReplSession::new().map_err(|e| format!("Failed to initialize REPL: {}", e))?;
  session.run().map_err(|e| format!("REPL error: {}", e))
}
