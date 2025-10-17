use colored::*;
use lishp::Environment;
use rustyline::error::ReadlineError;
use rustyline::history::History;
use rustyline::{Editor, Helper, Result as RustyResult};

use super::eval::{EvalError, process_input};

pub struct InputHandler {
  buffer: String,
  line_number: usize,
  env: Environment,
}

impl InputHandler {
  pub fn new() -> Self {
    Self {
      buffer: String::new(),
      line_number: 1,
      env: Environment::new(),
    }
  }

  pub fn line_number(&self) -> usize {
    self.line_number
  }

  pub fn is_multiline(&self) -> bool {
    !self.buffer.is_empty()
  }

  pub fn clear_buffer(&mut self) {
    self.buffer.clear();
  }

  pub fn environment_mut(&mut self) -> &mut Environment {
    &mut self.env
  }

  pub fn handle_line<H: Helper, I: History>(
    &mut self,
    line: String,
    editor: &mut Editor<H, I>,
  ) -> RustyResult<LineResult> {
    if !self.buffer.is_empty() {
      self.buffer.push('\n');
    }
    self.buffer.push_str(&line);

    match process_input(&self.buffer, true, &mut self.env) {
      Ok(result) => {
        editor.add_history_entry(self.buffer.as_str())?;
        self.buffer.clear();
        self.line_number += 1;
        Ok(LineResult::Complete(result))
      }
      Err(EvalError::Incomplete) => Ok(LineResult::NeedMore),
      Err(EvalError::Error(msg)) => {
        self.buffer.clear();
        Ok(LineResult::Error(msg))
      }
    }
  }
}

pub enum LineResult {
  Complete(String),
  NeedMore,
  Error(String),
}

pub fn handle_interrupt(handler: &mut InputHandler) {
  println!("{}", "^C".yellow());
  handler.clear_buffer();
}

pub fn handle_eof() {
  println!(
    "\n{} {}",
    "👋".bright_yellow(),
    "Thanks for using Lishp! Goodbye!".bright_cyan().italic()
  );
}

pub fn handle_error(err: ReadlineError) {
  eprintln!("{} {:?}", "Error:".red().bold(), err);
}
