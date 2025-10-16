use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Context, Helper};

pub struct ReplHelper {
  file_completer: FilenameCompleter,
}

impl ReplHelper {
  pub fn new() -> Self {
    Self {
      file_completer: FilenameCompleter::new(),
    }
  }
}

impl Helper for ReplHelper {}

impl Completer for ReplHelper {
  type Candidate = Pair;

  fn complete(
    &self,
    line: &str,
    pos: usize,
    ctx: &Context<'_>,
  ) -> Result<(usize, Vec<Pair>), ReadlineError> {
    if line.starts_with(":l ") || line.starts_with(":load ") {
      // Find the start position of the path argument
      let cmd_end = line.find(' ').unwrap_or(0) + 1;

      let path_part = &line[cmd_end..pos];

      let (start, candidates) = self
        .file_completer
        .complete(path_part, path_part.len(), ctx)?;

      Ok((cmd_end + start, candidates))
    } else {
      Ok((pos, vec![]))
    }
  }
}

impl Hinter for ReplHelper {
  type Hint = String;

  fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<String> {
    None
  }
}

impl Highlighter for ReplHelper {}

impl Validator for ReplHelper {}
