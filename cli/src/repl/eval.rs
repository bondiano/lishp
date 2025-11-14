use lishp::{ParseError, StdioAdapter, parser};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum EvalError {
  Incomplete,
  Error(String),
}

impl From<lishp::EvalError> for EvalError {
  fn from(err: lishp::EvalError) -> Self {
    EvalError::Error(err.to_string())
  }
}

pub fn process_input(
  input: &str,
  interactive_mode: bool,
  env: Rc<RefCell<lishp::Environment>>,
) -> Result<String, EvalError> {
  let mut remaining = input;
  let mut last_result = None;
  let mut results = Vec::new();
  let mut io = StdioAdapter::new();

  let mut evaluator = lishp::Evaluator::with_environment(&mut io, env);

  loop {
    match parser::parse_repl(remaining) {
      Ok(Some((value, rest))) => {
        let evaluated = evaluator.eval(&value)?;
        let result = format!("{}", evaluated);

        if interactive_mode {
          results.push(result.clone());
        }

        last_result = Some(result);
        remaining = rest;

        let trimmed = remaining.trim();
        if trimmed.is_empty() {
          break;
        }
        remaining = trimmed;
      }
      Ok(None) => {
        if last_result.is_none() {
          return Err(EvalError::Error("Empty input".to_string()));
        }
        break;
      }
      Err(ParseError::Incomplete) => {
        return Err(EvalError::Incomplete);
      }
      Err(ParseError::UnmatchedClosing) => {
        return Err(EvalError::Error(
          "Unexpected closing parenthesis ')'".to_string(),
        ));
      }
      Err(ParseError::Error(e)) => {
        return Err(EvalError::Error(e));
      }
    }
  }

  if interactive_mode {
    Ok(results.join("\n"))
  } else {
    last_result.ok_or_else(|| EvalError::Error("No result".to_string()))
  }
}
