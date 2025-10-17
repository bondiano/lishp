use lishp::{eval, ParseError, parser};

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

pub fn process_input(input: &str, interactive_mode: bool) -> Result<String, EvalError> {
  let mut remaining = input;
  let mut last_result = None;
  let mut results = Vec::new();

  loop {
    match parser::parse(remaining) {
      Ok(Some((value, rest))) => {
        // Evaluate the parsed value
        let evaluated = eval(&value)?;
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
        return Err(EvalError::Error("Unexpected closing parenthesis ')'".to_string()));
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
