use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use ecow::EcoString;
use thiserror::Error;

use crate::io::IoAdapter;
use crate::parser;
use crate::value::{
  BinaryOperator, BinaryPredicate, LishpValue, SpecialForm, car, cdr, cons, is_list,
};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum EvalError {
  #[error("Cannot set undefined variable: {0}")]
  CannotSetUndefinedVariable(String),

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

  #[error("Division by zero")]
  DivisionByZero,

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

  #[error("Undefined variable: {0}")]
  UndefinedVariable(String),

  #[error("Cannot define non-symbol")]
  CannotDefineNonSymbol,

  #[error("Cannot set non-symbol")]
  CannotSetNonSymbol,

  #[error("Unsupported feature: {0}")]
  UnsupportedFeature(String),
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

#[derive(Debug, Clone)]
pub struct Environment {
  frame: HashMap<EcoString, LishpValue>,
  parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
  pub fn new() -> Self {
    Self {
      frame: HashMap::new(),
      parent: None,
    }
  }

  pub fn define(&mut self, name: &str, value: LishpValue) {
    self.frame.insert(name.into(), value);
  }

  pub fn set(&mut self, name: &str, value: LishpValue) -> Result<(), EvalError> {
    if self.frame.contains_key(name) {
      self.frame.insert(name.into(), value);
      return Ok(());
    }

    let mut current_parent = self.parent.clone();

    while let Some(parent_rc) = current_parent {
      let mut parent = parent_rc.borrow_mut();
      if parent.frame.contains_key(name) {
        parent.frame.insert(name.into(), value);
        return Ok(());
      }
      current_parent = parent.parent.clone();
    }

    Err(EvalError::CannotSetUndefinedVariable(name.to_string()))
  }

  pub fn get(&self, name: &str) -> Option<LishpValue> {
    if let Some(value) = self.frame.get(name) {
      return Some(value.clone());
    }

    let mut current_parent = self.parent.clone();

    while let Some(parent_rc) = current_parent {
      let parent = parent_rc.borrow();
      if let Some(value) = parent.frame.get(name) {
        return Some(value.clone());
      }
      current_parent = parent.parent.clone();
    }

    None
  }

  pub fn contains(&self, name: &str) -> bool {
    self.frame.contains_key(name)
  }
}

impl Default for Environment {
  fn default() -> Self {
    Self::new()
  }
}

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
    BinaryOperator::Divide => {
      if matches!(right, LishpValue::Integer(0) | LishpValue::Double(0.0)) {
        return Err(EvalError::DivisionByZero);
      }

      match (left, right) {
        (LishpValue::Integer(left_val), LishpValue::Integer(right_val)) => {
          let result = left_val as f64 / right_val as f64;
          if result.fract() == 0.0 {
            Ok(LishpValue::Integer(result.trunc() as i64))
          } else {
            Ok(LishpValue::Double(result))
          }
        }
        (LishpValue::Double(left_val), LishpValue::Double(right_val)) => {
          Ok(LishpValue::Double(left_val / right_val))
        }
        (LishpValue::Integer(left_val), LishpValue::Double(right_val)) => {
          Ok(LishpValue::Double(left_val as f64 / right_val))
        }
        (LishpValue::Double(left_val), LishpValue::Integer(right_val)) => {
          Ok(LishpValue::Double(left_val / right_val as f64))
        }
        _ => Err(EvalError::InvalidBinaryOperator(operator)),
      }
    }
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

fn is_truthy(value: &LishpValue) -> bool {
  !matches!(value, LishpValue::Bool(false) | LishpValue::Nil)
}

pub struct Evaluator<'io, 'env> {
  io: &'io mut dyn IoAdapter,
  env: &'env mut Environment,
}

impl<'io, 'env> Evaluator<'io, 'env> {
  pub fn with_environment(io: &'io mut dyn IoAdapter, env: &'env mut Environment) -> Self {
    Self { io, env }
  }

  pub fn environment(&self) -> &Environment {
    self.env
  }

  pub fn environment_mut(&mut self) -> &mut Environment {
    self.env
  }

  pub fn into_environment(self) -> &'env mut Environment {
    self.env
  }

  pub fn eval(&mut self, value: &LishpValue) -> Result<LishpValue, EvalError> {
    match value {
      LishpValue::Symbol { name } => self
        .env
        .get(name)
        .ok_or_else(|| EvalError::UndefinedVariable(name.to_string())),

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

          LishpValue::Lambda {
            arguments,
            body,
            environment,
          } => {
            let mut lambda_env = environment.borrow().clone();
            let param_count = arguments.len();
            let mut current_tail = tail;

            for (idx, param_name) in arguments.iter().enumerate() {
              let is_last_param = idx == param_count - 1;

              if matches!(current_tail, LishpValue::Nil) {
                let remaining_args = arguments[idx..].to_vec();

                return Ok(LishpValue::Lambda {
                  arguments: remaining_args,
                  body: body.clone(),
                  environment: Rc::new(RefCell::new(lambda_env)),
                });
              }

              let arg_value = car(current_tail).unwrap(); // safe: Nil checked above
              let next_tail = cdr(current_tail).unwrap_or(&LishpValue::Nil);

              if is_last_param && !matches!(next_tail, LishpValue::Nil) {
                let mut rest_args = Vec::new();
                let mut rest_tail = current_tail;

                while !matches!(rest_tail, LishpValue::Nil) {
                  if let Some(arg_val) = car(rest_tail) {
                    rest_args.push(self.eval(arg_val)?);
                  }
                  rest_tail = cdr(rest_tail).unwrap_or(&LishpValue::Nil);
                }

                let rest_list = rest_args
                  .into_iter()
                  .rev()
                  .fold(LishpValue::Nil, |acc, item| cons(item, acc));

                lambda_env.define(param_name, rest_list);
              } else {
                let evaluated_arg = self.eval(arg_value)?;
                lambda_env.define(param_name, evaluated_arg);
                current_tail = next_tail;
              }
            }

            let mut lambda_evaluator = Evaluator::with_environment(self.io, &mut lambda_env);
            lambda_evaluator.eval(&body)
          }

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
      SpecialForm::Lambda => {
        let elements = get_elements(tail, 2)?;
        let args_list = &elements[0];
        let body = &elements[1];

        if !is_list(args_list) {
          return Err(EvalError::TypeError(format!(
            "lambda expects a list of arguments, got: {}",
            args_list
          )));
        }

        let mut arguments = Vec::new();
        let mut current = args_list;
        while !matches!(current, LishpValue::Nil) {
          if let Some(head) = car(current) {
            match head {
              LishpValue::Symbol { name } => arguments.push(name.clone()),
              _ => {
                return Err(EvalError::TypeError(format!(
                  "lambda argument must be a symbol, got: {}",
                  head
                )));
              }
            }
            if let Some(tail) = cdr(current) {
              current = tail;
            } else {
              break;
            }
          } else {
            break;
          }
        }

        let environment = self.env.clone();

        Ok(LishpValue::Lambda {
          arguments,
          body: Rc::new(body.clone()),
          environment: Rc::new(RefCell::new(environment)),
        })
      }

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
          LishpValue::Bool(_) => "bool",
          LishpValue::Nil => "nil",
          LishpValue::Cons(_, _) => "cons",
          LishpValue::SpecialForm(_) => "special-form",
          LishpValue::Lambda { .. } => "lambda",
          _ => "symbol",
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

        let condition = self.eval(condition_value)?;

        if is_truthy(&condition) {
          self.eval(then_value)
        } else if let Some(else_val) = else_value {
          self.eval(else_val)
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

      SpecialForm::Symbol => {
        let arguments = get_elements(tail, 1)?;
        let value = self.eval(&arguments[0])?;

        match value {
          LishpValue::String(s) => Ok(LishpValue::Symbol { name: s }),
          _ => Err(EvalError::TypeError(
            "symbol expects a string argument".to_string(),
          )),
        }
      }

      SpecialForm::Define => {
        let arguments = get_elements(tail, 2)?;

        if let LishpValue::Symbol { name } = &arguments[0] {
          let value = self.eval(&arguments[1])?;

          if let LishpValue::Lambda { environment, .. } = &value {
            environment.borrow_mut().define(name, value.clone());
          }

          self.env.define(name, value);
          Ok(LishpValue::Nil)
        } else {
          Err(EvalError::CannotDefineNonSymbol)
        }
      }

      SpecialForm::Set => {
        let arguments = get_elements(tail, 2)?;

        if let LishpValue::Symbol { name } = &arguments[0] {
          if !self.env.contains(name) {
            return Err(EvalError::UndefinedVariable(name.to_string()));
          }

          let value = self.eval(&arguments[1])?;
          self.env.set(name, value)?;
          Ok(LishpValue::Nil)
        } else {
          Err(EvalError::CannotSetNonSymbol)
        }
      }

      SpecialForm::Load => {
        let arguments = get_elements(tail, 1)?;
        let path_value = self.eval(&arguments[0])?;

        let path = match path_value {
          LishpValue::String(s) => s.to_string(),
          _ => {
            return Err(EvalError::TypeError(
              "load expects a string path argument".to_string(),
            ));
          }
        };

        let contents = self.io.read_file(&path)?;

        let mut remaining = contents.as_str();
        let mut last_result = LishpValue::Nil;

        while !remaining.trim().is_empty() {
          match parser::parse(remaining) {
            Ok(Some((value, rest))) => {
              last_result = self.eval(&value)?;
              remaining = rest;
            }
            Ok(None) => break,
            Err(e) => return Err(e.into()),
          }
        }

        Ok(last_result)
      }
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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let expr = cons(LishpValue::SpecialForm(SpecialForm::Read), LishpValue::Nil);

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Integer(42));
  }

  #[test]
  fn test_eval_read_string() {
    // (read)
    let mut io = MockIoAdapter::new(vec!["\"hello world\"".to_string()]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let expr = cons(LishpValue::SpecialForm(SpecialForm::Read), LishpValue::Nil);

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::String("hello world".into()));
  }

  #[test]
  fn test_eval_read_and_eval() {
    // (eval (read)) where input is "(_+_ 2 3)"
    let mut io = MockIoAdapter::new(vec!["(_+_ 2 3)".to_string()]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    // (_=_ 5 5) = true
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    // (_=_ 5 10) = false
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    // (_<_ 3.5 5.0) = true
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let expr = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::LessThan),
      cons(
        LishpValue::Double(3.5),
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
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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
    // (_=_ 5 "5") = false
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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

  #[test]
  fn test_eval_define() {
    // (def x 42)
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Define),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Integer(42), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Nil);

    // Now x should be defined
    let x_value = evaluator
      .eval(&LishpValue::Symbol { name: "x".into() })
      .unwrap();
    assert_eq!(x_value, LishpValue::Integer(42));
  }

  #[test]
  fn test_eval_define_expression() {
    // (def x (_+_ 2 3))
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let add_expr = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );

    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Define),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(add_expr, LishpValue::Nil),
      ),
    );

    evaluator.eval(&expr).unwrap();

    // x should equal 5
    let x_value = evaluator
      .eval(&LishpValue::Symbol { name: "x".into() })
      .unwrap();
    assert_eq!(x_value, LishpValue::Integer(5));
  }

  #[test]
  fn test_eval_set() {
    // (def x 10) (set! x 20)
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Define x = 10
    let def_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Define),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Integer(10), LishpValue::Nil),
      ),
    );
    evaluator.eval(&def_expr).unwrap();

    // Set x = 20
    let set_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Set),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Integer(20), LishpValue::Nil),
      ),
    );
    let result = evaluator.eval(&set_expr).unwrap();
    assert_eq!(result, LishpValue::Nil);

    // x should now equal 20
    let x_value = evaluator
      .eval(&LishpValue::Symbol { name: "x".into() })
      .unwrap();
    assert_eq!(x_value, LishpValue::Integer(20));
  }

  #[test]
  fn test_eval_set_undefined_variable() {
    // (set! x 20) - should fail because x is not defined
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let set_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Set),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Integer(20), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&set_expr);
    assert!(result.is_err());
    assert!(matches!(result, Err(EvalError::UndefinedVariable(_))));
  }

  #[test]
  fn test_eval_symbol_form() {
    // (symbol "x") = x
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let expr = cons(
      LishpValue::SpecialForm(SpecialForm::Symbol),
      cons(LishpValue::String("x".into()), LishpValue::Nil),
    );

    let result = evaluator.eval(&expr).unwrap();
    assert_eq!(result, LishpValue::Symbol { name: "x".into() });
  }

  #[test]
  fn test_eval_undefined_symbol() {
    // x - should fail because x is not defined
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    let result = evaluator.eval(&LishpValue::Symbol { name: "x".into() });
    assert!(result.is_err());
    assert!(matches!(result, Err(EvalError::UndefinedVariable(_))));
  }

  #[test]
  fn test_eval_variable_in_expression() {
    // (def x 5) (_+_ x 3) = 8
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Define x = 5
    let def_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Define),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Integer(5), LishpValue::Nil),
      ),
    );
    evaluator.eval(&def_expr).unwrap();

    // (_+_ x 3)
    let add_expr = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&add_expr).unwrap();
    assert_eq!(result, LishpValue::Integer(8));
  }

  #[test]
  fn test_eval_lambda_simple() {
    // (lambda (x y) (_+_ x y)) вызванная с (2 3) = 5
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Список аргументов: (x y)
    let args = cons(
      LishpValue::Symbol { name: "x".into() },
      cons(LishpValue::Symbol { name: "y".into() }, LishpValue::Nil),
    );

    // Тело: (_+_ x y)
    let body = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Symbol { name: "y".into() }, LishpValue::Nil),
      ),
    );

    // (lambda (x y) (_+_ x y))
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(args, cons(body, LishpValue::Nil)),
    );

    let lambda = evaluator.eval(&lambda_expr).unwrap();

    // Вызываем lambda с аргументами (2 3)
    let call_expr = cons(
      lambda,
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );

    let result = evaluator.eval(&call_expr).unwrap();
    assert_eq!(result, LishpValue::Integer(5));
  }

  #[test]
  fn test_eval_lambda_currying() {
    // ((lambda (x y) (_+_ x y)) 2) вызванная с недостаточным количеством аргументов
    // должна вернуть частично примененную lambda
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Список аргументов: (x y)
    let args = cons(
      LishpValue::Symbol { name: "x".into() },
      cons(LishpValue::Symbol { name: "y".into() }, LishpValue::Nil),
    );

    // Тело: (_+_ x y)
    let body = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Symbol { name: "y".into() }, LishpValue::Nil),
      ),
    );

    // (lambda (x y) (_+_ x y))
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(args, cons(body, LishpValue::Nil)),
    );

    let lambda = evaluator.eval(&lambda_expr).unwrap();

    // Вызываем lambda с одним аргументом (2)
    let partial_call = cons(lambda, cons(LishpValue::Integer(2), LishpValue::Nil));

    let partial_lambda = evaluator.eval(&partial_call).unwrap();

    // Проверяем, что это Lambda
    match &partial_lambda {
      LishpValue::Lambda { arguments, .. } => {
        assert_eq!(arguments.len(), 1);
        assert_eq!(arguments[0], "y");
      }
      _ => panic!("Expected Lambda, got {:?}", partial_lambda),
    }

    // Теперь вызываем частично примененную lambda с оставшимся аргументом (3)
    let final_call = cons(
      partial_lambda,
      cons(LishpValue::Integer(3), LishpValue::Nil),
    );
    let result = evaluator.eval(&final_call).unwrap();
    assert_eq!(result, LishpValue::Integer(5));
  }

  #[test]
  fn test_eval_lambda_currying_three_args() {
    // (lambda (x y z) (_+_ (_+_ x y) z)) с каррированием
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Список аргументов: (x y z)
    let args = cons(
      LishpValue::Symbol { name: "x".into() },
      cons(
        LishpValue::Symbol { name: "y".into() },
        cons(LishpValue::Symbol { name: "z".into() }, LishpValue::Nil),
      ),
    );

    // Тело: (_+_ (_+_ x y) z)
    let inner_add = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        LishpValue::Symbol { name: "x".into() },
        cons(LishpValue::Symbol { name: "y".into() }, LishpValue::Nil),
      ),
    );
    let body = cons(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      cons(
        inner_add,
        cons(LishpValue::Symbol { name: "z".into() }, LishpValue::Nil),
      ),
    );

    // (lambda (x y z) (_+_ (_+_ x y) z))
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(args, cons(body, LishpValue::Nil)),
    );

    let lambda = evaluator.eval(&lambda_expr).unwrap();

    // Передаем один аргумент
    let partial1 = cons(lambda, cons(LishpValue::Integer(1), LishpValue::Nil));
    let lambda1 = evaluator.eval(&partial1).unwrap();

    // Передаем второй аргумент
    let partial2 = cons(lambda1, cons(LishpValue::Integer(2), LishpValue::Nil));
    let lambda2 = evaluator.eval(&partial2).unwrap();

    // Передаем третий аргумент
    let final_call = cons(lambda2, cons(LishpValue::Integer(3), LishpValue::Nil));
    let result = evaluator.eval(&final_call).unwrap();

    // 1 + 2 + 3 = 6
    assert_eq!(result, LishpValue::Integer(6));
  }

  #[test]
  fn test_eval_lambda_variadic_args() {
    // (lambda (x y) y) вызванная с (1 2 3 4 5)
    // x = 1, y = (2 3 4 5)
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Список аргументов: (x y)
    let args = cons(
      LishpValue::Symbol { name: "x".into() },
      cons(LishpValue::Symbol { name: "y".into() }, LishpValue::Nil),
    );

    // Тело: y (возвращаем второй аргумент)
    let body = LishpValue::Symbol { name: "y".into() };

    // (lambda (x y) y)
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(args, cons(body, LishpValue::Nil)),
    );

    let lambda = evaluator.eval(&lambda_expr).unwrap();

    // Вызываем lambda с аргументами (1 2 3 4 5)
    let call_expr = cons(
      lambda,
      cons(
        LishpValue::Integer(1),
        cons(
          LishpValue::Integer(2),
          cons(
            LishpValue::Integer(3),
            cons(
              LishpValue::Integer(4),
              cons(LishpValue::Integer(5), LishpValue::Nil),
            ),
          ),
        ),
      ),
    );

    let result = evaluator.eval(&call_expr).unwrap();

    // y должен быть списком (2 3 4 5)
    let expected = cons(
      LishpValue::Integer(2),
      cons(
        LishpValue::Integer(3),
        cons(
          LishpValue::Integer(4),
          cons(LishpValue::Integer(5), LishpValue::Nil),
        ),
      ),
    );
    assert_eq!(result, expected);
  }

  #[test]
  fn test_eval_lambda_variadic_single_param() {
    // (lambda (x) x) вызванная с (1 2 3)
    // x = (1 2 3)
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Список аргументов: (x)
    let args = cons(LishpValue::Symbol { name: "x".into() }, LishpValue::Nil);

    // Тело: x
    let body = LishpValue::Symbol { name: "x".into() };

    // (lambda (x) x)
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(args, cons(body, LishpValue::Nil)),
    );

    let lambda = evaluator.eval(&lambda_expr).unwrap();

    // Вызываем lambda с аргументами (1 2 3)
    let call_expr = cons(
      lambda,
      cons(
        LishpValue::Integer(1),
        cons(
          LishpValue::Integer(2),
          cons(LishpValue::Integer(3), LishpValue::Nil),
        ),
      ),
    );

    let result = evaluator.eval(&call_expr).unwrap();

    // x должен быть списком (1 2 3)
    let expected = cons(
      LishpValue::Integer(1),
      cons(
        LishpValue::Integer(2),
        cons(LishpValue::Integer(3), LishpValue::Nil),
      ),
    );
    assert_eq!(result, expected);
  }

  #[test]
  fn test_eval_lambda_variadic_with_car() {
    // (lambda (x rest) x) вызванная с (10 20 30 40)
    // x = 10, rest = (20 30 40)
    // Проверяем, что можем использовать car на rest
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Список аргументов: (x rest)
    let args = cons(
      LishpValue::Symbol { name: "x".into() },
      cons(
        LishpValue::Symbol {
          name: "rest".into(),
        },
        LishpValue::Nil,
      ),
    );

    // Тело: (car rest)
    let body = cons(
      LishpValue::SpecialForm(SpecialForm::Car),
      cons(
        LishpValue::Symbol {
          name: "rest".into(),
        },
        LishpValue::Nil,
      ),
    );

    // (lambda (x rest) (car rest))
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(args, cons(body, LishpValue::Nil)),
    );

    let lambda = evaluator.eval(&lambda_expr).unwrap();

    // Вызываем lambda с аргументами (10 20 30 40)
    let call_expr = cons(
      lambda,
      cons(
        LishpValue::Integer(10),
        cons(
          LishpValue::Integer(20),
          cons(
            LishpValue::Integer(30),
            cons(LishpValue::Integer(40), LishpValue::Nil),
          ),
        ),
      ),
    );

    let result = evaluator.eval(&call_expr).unwrap();

    // (car rest) = (car (20 30 40)) = 20
    assert_eq!(result, LishpValue::Integer(20));
  }

  #[test]
  fn test_eval_lambda_recursive_factorial() {
    // (def factorial (lambda (n) (if (_>_ n 1) (_*_ n (factorial (_-_ n 1))) 1)))
    // (factorial 5) = 120
    let mut io = MockIoAdapter::new(vec![]);
    let mut env = Environment::new();
    let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

    // Внутреннее условие: (_>_ n 1)
    let condition = cons(
      LishpValue::BinaryPredicate(BinaryPredicate::GreaterThan),
      cons(
        LishpValue::Symbol { name: "n".into() },
        cons(LishpValue::Integer(1), LishpValue::Nil),
      ),
    );

    // Рекурсивный вызов: (_-_ n 1)
    let n_minus_1 = cons(
      LishpValue::BinaryOperator(BinaryOperator::Subtract),
      cons(
        LishpValue::Symbol { name: "n".into() },
        cons(LishpValue::Integer(1), LishpValue::Nil),
      ),
    );

    // (factorial (_-_ n 1))
    let recursive_call = cons(
      LishpValue::Symbol {
        name: "factorial".into(),
      },
      cons(n_minus_1, LishpValue::Nil),
    );

    // (_*_ n (factorial (_-_ n 1)))
    let multiply = cons(
      LishpValue::BinaryOperator(BinaryOperator::Multiply),
      cons(
        LishpValue::Symbol { name: "n".into() },
        cons(recursive_call, LishpValue::Nil),
      ),
    );

    // (if (_>_ n 1) (_*_ n (factorial (_-_ n 1))) 1)
    let if_expr = cons(
      LishpValue::SpecialForm(SpecialForm::If),
      cons(
        condition,
        cons(multiply, cons(LishpValue::Integer(1), LishpValue::Nil)),
      ),
    );

    // (lambda (n) (if ...))
    let lambda_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      cons(
        cons(LishpValue::Symbol { name: "n".into() }, LishpValue::Nil),
        cons(if_expr, LishpValue::Nil),
      ),
    );

    // (def factorial (lambda ...))
    let def_expr = cons(
      LishpValue::SpecialForm(SpecialForm::Define),
      cons(
        LishpValue::Symbol {
          name: "factorial".into(),
        },
        cons(lambda_expr, LishpValue::Nil),
      ),
    );

    evaluator.eval(&def_expr).unwrap();

    // Теперь вызываем (factorial 5)
    let call_expr = cons(
      LishpValue::Symbol {
        name: "factorial".into(),
      },
      cons(LishpValue::Integer(5), LishpValue::Nil),
    );

    let result = evaluator.eval(&call_expr).unwrap();
    assert_eq!(result, LishpValue::Integer(120)); // 5! = 120
  }
}
