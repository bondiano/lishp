use lishp::{ParseError, parser};

#[derive(Debug)]
pub enum EvalError {
  Incomplete,
  Error(String),
}

pub fn process_input(input: &str, interactive_mode: bool) -> Result<String, EvalError> {
  let mut remaining = input;
  let mut last_result = None;
  let mut results = Vec::new();

  loop {
    match parser::parse(remaining) {
      Ok(Some((value, rest))) => {
        // TODO: Evaluate the parsed value
        let result = format!("{}", value);

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
