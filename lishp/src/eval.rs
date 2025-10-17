use thiserror::Error;

use crate::io::IoAdapter;
use crate::parser;
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

  #[error("IO error: {0}")]
  IoError(String),

  #[error("Parse error: {0}")]
  ParseError(String),
}

impl From<std::io::Error> for EvalError {
  fn from(err: std::io::Error) -> Self {
    EvalError::IoError(err.to_string())
  }
}

impl From<crate::parser::ParseError> for EvalError {
  fn from(err: crate::parser::ParseError) -> Self {
    EvalError::ParseError(format!("{:?}", err))
  }
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

/// Check if a value is truthy (anything except false and nil is truthy)
fn is_truthy(value: &LishpValue) -> bool {
  !matches!(value, LishpValue::Bool(false) | LishpValue::Nil)
}

/// Evaluator with I/O capabilities
pub struct Evaluator<'a> {
  io: &'a mut dyn IoAdapter,
}

impl<'a> Evaluator<'a> {
  pub fn new(io: &'a mut dyn IoAdapter) -> Self {
    Self { io }
  }

  pub fn eval(&mut self, value: &LishpValue) -> Result<LishpValue, EvalError> {
    match value {
      LishpValue::Cons(_, _) => {
        if matches!(value, LishpValue::Nil) {
          return Err(EvalError::EvalNil);
        }

        let head = car(value).ok_or(EvalError::EvalNil)?;
        let evaluated_head = self.eval(head)?;

        let tail = cdr(value).ok_or(EvalError::EvalNil)?;

        match evaluated_head {
          LishpValue::BinaryOperator(operator) => {
            let arguments = get_elements(tail, 2)?;
            let left = self.eval(&arguments[0])?;
            let right = self.eval(&arguments[1])?;
            eval_binary_operator(operator, left, right)
          }

          LishpValue::BinaryPredicate(predicate) => {
            let arguments = get_elements(tail, 2)?;
            let left = self.eval(&arguments[0])?;
            let right = self.eval(&arguments[1])?;
            eval_binary_predicate(predicate, left, right)
          }

          LishpValue::SpecialForm(form) => self.eval_special_form(form, tail),

          _ => Err(EvalError::WrongHeadForm),
        }
      }

      _ => Ok(value.clone()),
    }
  }

  fn eval_special_form(
    &mut self,
    form: SpecialForm,
    tail: &LishpValue,
  ) -> Result<LishpValue, EvalError> {
    match form {
      SpecialForm::Quote => {
        let arguments = get_elements(tail, 1)?;
        Ok(arguments[0].clone())
      }

      SpecialForm::Cons => {
        let arguments = get_elements(tail, 2)?;
        let first = self.eval(&arguments[0])?;
        let second = self.eval(&arguments[1])?;

        if !is_list(&second) {
          return Err(EvalError::ExpectedList(second.to_string()));
        }

        Ok(cons(first, second))
      }

      SpecialForm::Car => {
        let arguments = get_elements(tail, 1)?;
        let list_value = self.eval(&arguments[0])?;

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
        let list_value = self.eval(&arguments[0])?;

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
        let evaluated_value = self.eval(&arguments[0])?;

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

      SpecialForm::If => {
        let condition_value = car(tail).ok_or(EvalError::WrongArgumentCount {
          form: "if".to_string(),
          expected: 2,
          got: 0,
        })?;

        let after_condition = cdr(tail).ok_or(EvalError::WrongArgumentCount {
          form: "if".to_string(),
          expected: 2,
          got: 1,
        })?;

        let then_value = car(after_condition).ok_or(EvalError::WrongArgumentCount {
          form: "if".to_string(),
          expected: 2,
          got: 1,
        })?;

        let after_then = cdr(after_condition).unwrap_or(&LishpValue::Nil);
        let else_value = if !matches!(after_then, LishpValue::Nil) {
          car(after_then)
        } else {
          None
        };

        let condition = self.eval(&condition_value)?;

        if is_truthy(&condition) {
          self.eval(&then_value)
        } else if let Some(else_val) = else_value {
          self.eval(&else_val)
        } else {
          Ok(LishpValue::Nil)
        }
      }

      SpecialForm::Eval => {
        let arguments = get_elements(tail, 1)?;
        let expression = self.eval(&arguments[0])?;
        self.eval(&expression)
      }

      SpecialForm::Do => {
        let mut current = tail;
        let mut last_result = LishpValue::Nil;

        while !matches!(current, LishpValue::Nil) {
          if let Some(form) = car(current) {
            last_result = self.eval(form)?;
            if let Some(rest) = cdr(current) {
              current = rest;
            } else {
              break;
            }
          } else {
            break;
          }
        }

        Ok(last_result)
      }

      SpecialForm::Print => {
        let arguments = get_elements(tail, 1)?;
        let value = self.eval(&arguments[0])?;
        self.io.println(&value.to_string())?;
        Ok(value)
      }

      SpecialForm::Read => {
        let line = self.io.read_line()?;
        let parsed = parser::parse(&line)?;
        if let Some((value, _)) = parsed {
          Ok(value)
        } else {
          Ok(LishpValue::Nil)
        }
      }

      SpecialForm::Define => Err(EvalError::TypeError(
        "Define not yet implemented".to_string(),
      )),

      SpecialForm::Symbol => Err(EvalError::TypeError(
        "Symbol not yet implemented".to_string(),
      )),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::io::MockIoAdapter;
  use crate::value::{BinaryOperator, BinaryPredicate, LishpValue, SpecialForm, cons};

  #[test]
  fn test_eval_print_with_mock_io() {
    // (print "hello")
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Print),
      cons(LishpValue::String("hello".into()), LishpValue::Nil),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::String("hello".into()));
    // Strings are printed with quotes because to_string() includes them
    assert_eq!(io.output(), &["\"hello\"", "\n"]);
  }

  #[test]
  fn test_eval_print_expression() {
    // (print (_+_ 2 3))
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let add_expr = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );

    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Print),
      cons(add_expr, LishpValue::Nil),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Integer(5));
    assert_eq!(io.output(), &["5", "\n"]);
  }

  #[test]
  fn test_eval_read_with_mock_io() {
    // (read)
    let mut io = MockIoAdapter::new(vec!["42".to_string()]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(LishpValue::SpecialForm(SpecialForm::Read), LishpValue::Nil);

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Integer(42));
  }

  #[test]
  fn test_eval_read_string() {
    // (read)
    let mut io = MockIoAdapter::new(vec!["\"hello world\"".to_string()]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(LishpValue::SpecialForm(SpecialForm::Read), LishpValue::Nil);

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::String("hello world".into()));
  }

  #[test]
  fn test_eval_read_and_eval() {
    // (eval (read)) where input is "(_+_ 2 3)"
    let mut io = MockIoAdapter::new(vec!["(_+_ 2 3)".to_string()]);
    let mut evaluator = Evaluator::new(&mut io);

    let read_expr = cons(LishpValue::SpecialForm(SpecialForm::Read), LishpValue::Nil);
    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Eval),
      cons(read_expr, LishpValue::Nil),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Integer(5));
  }

  #[test]
  fn test_eval_do_with_print() {
    // (do (print "first") (print "second") 42)
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let print1 = cons(
      LishpValue::SpecialForm(SpecialForm::Print),
      cons(LishpValue::String("first".into()), LishpValue::Nil),
    );
    let print2 = cons(
      LishpValue::SpecialForm(SpecialForm::Print),
      cons(LishpValue::String("second".into()), LishpValue::Nil),
    );
    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Do),
      cons(
        print1,
        cons(print2, cons(LishpValue::Integer(42), LishpValue::Nil)),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Integer(42));
    // Strings are printed with quotes because to_string() includes them
    assert_eq!(io.output(), &["\"first\"", "\n", "\"second\"", "\n"]);
  }

  #[test]
  fn test_eval_predicate_equals_true() {
    // (_==_ 5 5) = true
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::Equals),
      cons(
        LishpValue::Integer(5),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(true));
  }

  #[test]
  fn test_eval_predicate_equals_false() {
    // (_==_ 5 10) = false
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::Equals),
      cons(
        LishpValue::Integer(5),
        cons(LishpValue::Integer(10), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(false));
  }

  #[test]
  fn test_eval_predicate_less_than_true() {
    // (_<_ 3 5) = true
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::LessThan),
      cons(
        LishpValue::Integer(3),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(true));
  }

  #[test]
  fn test_eval_predicate_less_than_false() {
    // (_<_ 10 5) = false
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::LessThan),
      cons(
        LishpValue::Integer(10),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(false));
  }

  #[test]
  fn test_eval_predicate_greater_than_true() {
    // (_>_ 10 5) = true
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::GreaterThan),
      cons(
        LishpValue::Integer(10),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(true));
  }

  #[test]
  fn test_eval_predicate_greater_than_false() {
    // (_>_ 3 5) = false
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::GreaterThan),
      cons(
        LishpValue::Integer(3),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(false));
  }

  #[test]
  fn test_eval_predicate_with_doubles() {
    // (_<_ 3.14 5.0) = true
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::LessThan),
      cons(
        LishpValue::Double(3.14),
        cons(LishpValue::Double(5.0), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(true));
  }

  #[test]
  fn test_eval_predicate_in_if() {
    // (if (_<_ 3 5) "yes" "no") = "yes"
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let condition = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::LessThan),
      cons(
        LishpValue::Integer(3),
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );

    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::If),
      cons(
        condition,
        cons(
          LishpValue::String("yes".into()),
          cons(LishpValue::String("no".into()), LishpValue::Nil),
        ),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::String("yes".into()));
  }

  #[test]
  fn test_eval_predicate_equals_different_types() {
    // (_==_ 5 "5") = false
    let mut io = MockIoAdapter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut io);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::Equals),
      cons(
        LishpValue::Integer(5),
        cons(LishpValue::String("5".into()), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Bool(false));
  }
}
