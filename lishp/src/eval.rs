use thiserror::Error;

use crate::value::{
  BinaryOperator, BinaryPredicate, LishpValue, SpecialForm, car, cdr, cons, is_list,
};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum EvalError {
  #[error("Invalid binary operator: {0}")]
  InvalidBinaryOperator(BinaryOperator),

  #[error("Invalid binary predicate: {0}")]
  InvalidBinaryPredicate(BinaryPredicate),

  #[error("Wrong number of arguments for {form}: expected {expected}, got {got}")]
  WrongArgumentCount {
    form: String,
    expected: usize,
    got: usize,
  },

  #[error("Extra elements in list")]
  ExtraElements,

  #[error("Missing elements in list: expected {expected}, got {got}")]
  MissingElements { expected: usize, got: usize },

  #[error("Cannot evaluate empty list (nil)")]
  EvalNil,

  #[error("Wrong head form: expected special form, binary operator, or binary predicate")]
  WrongHeadForm,

  #[error("Expected list, got: {0}")]
  ExpectedList(String),

  #[error("Cannot get car of nil")]
  CarOfNil,

  #[error("Cannot get cdr of nil")]
  CdrOfNil,

  #[error("Type error: {0}")]
  TypeError(String),
}

/// Extract exactly count elements from a list
/// Returns error if there are extra or missing elements
fn get_elements(mut value: &LishpValue, count: usize) -> Result<Vec<LishpValue>, EvalError> {
  let mut elements = Vec::new();

  while !matches!(value, LishpValue::Nil) && elements.len() < count {
    if let Some(head) = car(value) {
      elements.push(head.clone());
      if let Some(tail) = cdr(value) {
        value = tail;
      } else {
        break;
      }
    } else {
      break;
    }
  }

  if !matches!(value, LishpValue::Nil) {
    return Err(EvalError::ExtraElements);
  }

  if elements.len() < count {
    return Err(EvalError::MissingElements {
      expected: count,
      got: elements.len(),
    });
  }

  Ok(elements)
}

/// Convert a value to string representation for string concatenation
fn repr_to_str(value: &LishpValue) -> String {
  match value {
    LishpValue::String(string) => string.to_string(),
    _ => value.to_string(),
  }
}

fn eval_binary_operator(
  operator: BinaryOperator,
  left: LishpValue,
  right: LishpValue,
) -> Result<LishpValue, EvalError> {
  match operator {
    BinaryOperator::Add => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Integer(left_val + right_val))
      }
      (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
        Ok(LishpValue::Double(left_val + right_val))
      }
      _ => Err(EvalError::InvalidBinaryOperator(operator)),
    },
    BinaryOperator::Subtract => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Integer(left_val - right_val))
      }
      (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
        Ok(LishpValue::Double(left_val - right_val))
      }
      _ => Err(EvalError::InvalidBinaryOperator(operator)),
    },
    BinaryOperator::Multiply => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Integer(left_val * right_val))
      }
      (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
        Ok(LishpValue::Double(left_val * right_val))
      }
      _ => Err(EvalError::InvalidBinaryOperator(operator)),
    },
    BinaryOperator::Divide => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Integer(left_val / right_val))
      }
      (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
        Ok(LishpValue::Double(left_val / right_val))
      }
      _ => Err(EvalError::InvalidBinaryOperator(operator)),
    },
    BinaryOperator::Modulo => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Integer(left_val % right_val))
      }
      _ => Err(EvalError::InvalidBinaryOperator(operator)),
    },
    BinaryOperator::StrConcat => {
      let left_str = repr_to_str(&left);
      let right_str = repr_to_str(&right);
      Ok(LishpValue::String((left_str + &right_str).into()))
    }
  }
}

fn eval_binary_predicate(
  predicate: BinaryPredicate,
  left: LishpValue,
  right: LishpValue,
) -> Result<LishpValue, EvalError> {
  match predicate {
    BinaryPredicate::Equals => Ok(LishpValue::Bool(left == right)),
    BinaryPredicate::LessThan => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Bool(left_val < right_val))
      }
      (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
        Ok(LishpValue::Bool(left_val < right_val))
      }
      _ => Err(EvalError::InvalidBinaryPredicate(predicate)),
    },
    BinaryPredicate::GreaterThan => match (left, right) {
      (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
        Ok(LishpValue::Bool(left_val > right_val))
      }
      (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
        Ok(LishpValue::Bool(left_val > right_val))
      }
      _ => Err(EvalError::InvalidBinaryPredicate(predicate)),
    },
  }
}

/// Convert a symbol to an operator, predicate, or special form if possible
fn symbol_to_operator(symbol: &str) -> Option<LishpValue> {
  use std::str::FromStr;

  // Try to convert to BinaryOperator
  if let Ok(op) = BinaryOperator::from_str(symbol) {
    return Some(LishpValue::BinaryOperator(op));
  }

  // Try to convert to BinaryPredicate
  if let Ok(pred) = BinaryPredicate::from_str(symbol) {
    return Some(LishpValue::BinaryPredicate(pred));
  }

  // Try to convert to SpecialForm
  if let Ok(form) = SpecialForm::from_str(symbol) {
    return Some(LishpValue::SpecialForm(form));
  }

  // Also check short aliases for operators
  match symbol {
    "+" => Some(LishpValue::BinaryOperator(BinaryOperator::Add)),
    "-" => Some(LishpValue::BinaryOperator(BinaryOperator::Subtract)),
    "*" => Some(LishpValue::BinaryOperator(BinaryOperator::Multiply)),
    "/" => Some(LishpValue::BinaryOperator(BinaryOperator::Divide)),
    "%" => Some(LishpValue::BinaryOperator(BinaryOperator::Modulo)),
    "++" => Some(LishpValue::BinaryOperator(BinaryOperator::StrConcat)),
    "<" => Some(LishpValue::BinaryPredicate(BinaryPredicate::LessThan)),
    ">" => Some(LishpValue::BinaryPredicate(BinaryPredicate::GreaterThan)),
    "=" => Some(LishpValue::BinaryPredicate(BinaryPredicate::Equals)),
    _ => None,
  }
}

pub fn eval(value: &LishpValue) -> Result<LishpValue, EvalError> {
  match value {
    // Lists are evaluated as function applications
    LishpValue::Cons(_, _) => {
      // Empty list (nil) cannot be evaluated
      if matches!(value, LishpValue::Nil) {
        return Err(EvalError::EvalNil);
      }

      // Evaluate the head to get the operator/function
      let head = car(value).ok_or(EvalError::EvalNil)?;
      let mut evaluated_head = eval(head)?;

      // Convert symbols to operators/predicates/forms if possible
      if let LishpValue::Symbol(sym) = &evaluated_head {
        if let Some(converted) = symbol_to_operator(sym.as_str()) {
          evaluated_head = converted;
        }
      }

      let tail = cdr(value).ok_or(EvalError::EvalNil)?;

      match evaluated_head {
        // Binary operators
        LishpValue::BinaryOperator(operator) => {
          let arguments = get_elements(tail, 2)?;
          let left = eval(&arguments[0])?;
          let right = eval(&arguments[1])?;
          eval_binary_operator(operator, left, right)
        }

        // Binary predicates
        LishpValue::BinaryPredicate(predicate) => {
          let arguments = get_elements(tail, 2)?;
          let left = eval(&arguments[0])?;
          let right = eval(&arguments[1])?;
          eval_binary_predicate(predicate, left, right)
        }

        // Special forms
        LishpValue::SpecialForm(form) => match form {
          SpecialForm::Quote => {
            let arguments = get_elements(tail, 1)?;
            Ok(arguments[0].clone())
          }

          SpecialForm::Cons => {
            let arguments = get_elements(tail, 2)?;
            let first = eval(&arguments[0])?;
            let second = eval(&arguments[1])?;

            if !is_list(&second) {
              return Err(EvalError::ExpectedList(second.to_string()));
            }

            Ok(cons(first, second))
          }

          SpecialForm::Car => {
            let arguments = get_elements(tail, 1)?;
            let list_value = eval(&arguments[0])?;

            if !is_list(&list_value) {
              return Ok(list_value);
            }

            if matches!(list_value, LishpValue::Nil) {
              return Err(EvalError::CarOfNil);
            }

            car(&list_value).cloned().ok_or(EvalError::CarOfNil)
          }

          SpecialForm::Cdr => {
            let arguments = get_elements(tail, 1)?;
            let list_value = eval(&arguments[0])?;

            if !is_list(&list_value) {
              return Ok(LishpValue::Nil);
            }

            if matches!(list_value, LishpValue::Nil) {
              return Err(EvalError::CdrOfNil);
            }

            cdr(&list_value).cloned().ok_or(EvalError::CdrOfNil)
          }

          SpecialForm::TypeOf => {
            let arguments = get_elements(tail, 1)?;
            let evaluated_value = eval(&arguments[0])?;

            let type_name = match evaluated_value {
              LishpValue::Integer(_) => "integer",
              LishpValue::Double(_) => "double",
              LishpValue::String(_) => "string",
              LishpValue::Symbol(_) => "symbol",
              LishpValue::Bool(_) => "bool",
              LishpValue::Nil => "nil",
              LishpValue::Cons(_, _) => "cons",
              LishpValue::SpecialForm(_) => "special-form",
              LishpValue::BinaryOperator(_) => "binary-operator",
              LishpValue::BinaryPredicate(_) => "binary-predicate",
            };

            Ok(LishpValue::String(type_name.into()))
          }

          // Placeholder implementations for unimplemented special forms
          SpecialForm::Define => Err(EvalError::TypeError(
            "Define not yet implemented".to_string(),
          )),

          SpecialForm::If => Err(EvalError::TypeError("If not yet implemented".to_string())),

          SpecialForm::Eval => Err(EvalError::TypeError("Eval not yet implemented".to_string())),

          SpecialForm::Do => Err(EvalError::TypeError("Do not yet implemented".to_string())),

          SpecialForm::Read => Err(EvalError::TypeError("Read not yet implemented".to_string())),

          SpecialForm::Print => Err(EvalError::TypeError(
            "Print not yet implemented".to_string(),
          )),

          SpecialForm::Symbol => Err(EvalError::TypeError(
            "Symbol not yet implemented".to_string(),
          )),
        },

        _ => Err(EvalError::WrongHeadForm),
      }
    }

    _ => Ok(value.clone()),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::value::{BinaryOperator, LishpValue, SpecialForm, cons};

  #[test]
  fn test_eval_integer() {
    let value = LishpValue::Integer(42);
    assert_eq!(eval(&value).unwrap(), LishpValue::Integer(42));
  }

  #[test]
  fn test_eval_binary_operator_add() {
    // (_+_ 2 3)
    let expr = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(5));
  }

  #[test]
  fn test_eval_binary_operator_multiply() {
    // (_*_ 4 5)
    let expr = cons(
      LishpValue::BinaryOperator(BinaryOperator::Multiply),
      cons(
        LishpValue::Integer(4),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(20));
  }

  #[test]
  fn test_eval_nested_expression() {
    // (_+_ (_*_ 2 3) 5) = 11
    let inner = cons(
      LishpValue::BinaryOperator(BinaryOperator::Multiply),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    let expr = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(inner, cons(LishpValue::Integer(5), LishpValue::Nil)),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(11));
  }

  #[test]
  fn test_eval_quote() {
    // (quote (1 2 3))
    let list = cons(
      LishpValue::Integer(1),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Quote),
      cons(list.clone(), LishpValue::Nil),
    );
    assert_eq!(eval(&expr).unwrap(), list);
  }

  #[test]
  fn test_eval_car() {
    // (car (quote (1 2 3))) = 1
    let list = cons(
      LishpValue::Integer(1),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    let quoted = cons(
      LishpValue::SpecialForm(SpecialForm::Quote),
      cons(list, LishpValue::Nil),
    );
    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Car),
      cons(quoted, LishpValue::Nil),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(1));
  }

  #[test]
  fn test_eval_cdr() {
    // (cdr (quote (1 2 3))) = (2 3)
    let list = cons(
      LishpValue::Integer(1),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    let quoted = cons(
      LishpValue::SpecialForm(SpecialForm::Quote),
      cons(list, LishpValue::Nil),
    );
    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Cdr),
      cons(quoted, LishpValue::Nil),
    );
    let expected = cons(
      LishpValue::Integer(2),
      cons(LishpValue::Integer(3), LishpValue::Nil),
    );
    assert_eq!(eval(&expr).unwrap(), expected);
  }

  #[test]
  fn test_eval_symbol_as_operator() {
    // (+ 2 3) = 5
    let expr = cons(
      LishpValue::Symbol("+".into()),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(5));
  }

  #[test]
  fn test_eval_symbol_multiply() {
    // (* 4 5) = 20
    let expr = cons(
      LishpValue::Symbol("*".into()),
      cons(
        LishpValue::Integer(4),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(20));
  }

  #[test]
  fn test_eval_symbol_predicate() {
    // (< 3 5) = true
    let expr = cons(
      LishpValue::Symbol("<".into()),
      cons(
        LishpValue::Integer(3),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Bool(true));
  }

  #[test]
  fn test_eval_nested_with_symbols() {
    // (+ (* 2 3) (- 10 5)) = 11
    let multiply = cons(
      LishpValue::Symbol("*".into()),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    let subtract = cons(
      LishpValue::Symbol("-".into()),
      cons(
        LishpValue::Integer(10),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );
    let expr = cons(
      LishpValue::Symbol("+".into()),
      cons(multiply, cons(subtract, LishpValue::Nil)),
    );
    assert_eq!(eval(&expr).unwrap(), LishpValue::Integer(11));
  }
}
