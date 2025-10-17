use std::{fmt, rc::Rc, str::FromStr};

use ecow::EcoString;

#[derive(Debug, Clone, PartialEq)]
pub enum SpecialForm {
  Define,
  If,
  Quote,
  Eval,
  Car,
  Cdr,
  Cons,
  Do,
  TypeOf,
  Read,
  Print,
  Symbol,
}

impl FromStr for SpecialForm {
  type Err = String;

  fn from_str(value: &str) -> Result<Self, Self::Err> {
    match value {
      "def" => Ok(SpecialForm::Define),
      "if" => Ok(SpecialForm::If),
      "quote" => Ok(SpecialForm::Quote),
      "eval" => Ok(SpecialForm::Eval),
      "car" => Ok(SpecialForm::Car),
      "cdr" => Ok(SpecialForm::Cdr),
      "cons" => Ok(SpecialForm::Cons),
      "do" => Ok(SpecialForm::Do),
      "typeof" => Ok(SpecialForm::TypeOf),
      "read" => Ok(SpecialForm::Read),
      "print" => Ok(SpecialForm::Print),
      "symbol" => Ok(SpecialForm::Symbol),
      _ => Err(format!("Invalid special form: {}", value)),
    }
  }
}

impl fmt::Display for SpecialForm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      SpecialForm::Define => write!(f, "def"),
      SpecialForm::If => write!(f, "if"),
      SpecialForm::Quote => write!(f, "quote"),
      SpecialForm::Eval => write!(f, "eval"),
      SpecialForm::Car => write!(f, "car"),
      SpecialForm::Cdr => write!(f, "cdr"),
      SpecialForm::Cons => write!(f, "cons"),
      SpecialForm::Do => write!(f, "do"),
      SpecialForm::TypeOf => write!(f, "typeof"),
      SpecialForm::Read => write!(f, "read"),
      SpecialForm::Print => write!(f, "print"),
      SpecialForm::Symbol => write!(f, "symbol"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  StrConcat,
}

impl FromStr for BinaryOperator {
  type Err = String;

  fn from_str(value: &str) -> Result<Self, Self::Err> {
    match value {
      "_+_" => Ok(BinaryOperator::Add),
      "_-_" => Ok(BinaryOperator::Subtract),
      "_*_" => Ok(BinaryOperator::Multiply),
      "_/_" => Ok(BinaryOperator::Divide),
      "_%_" => Ok(BinaryOperator::Modulo),
      "_++_" => Ok(BinaryOperator::StrConcat),
      _ => Err(format!("Invalid binary operator: {}", value)),
    }
  }
}

impl fmt::Display for BinaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BinaryOperator::Add => write!(f, "_+_"),
      BinaryOperator::Subtract => write!(f, "_-_"),
      BinaryOperator::Multiply => write!(f, "_*_"),
      BinaryOperator::Divide => write!(f, "_/_"),
      BinaryOperator::Modulo => write!(f, "_%_"),
      BinaryOperator::StrConcat => write!(f, "_++_"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryPredicate {
  Equals,
  LessThan,
  GreaterThan,
}

impl FromStr for BinaryPredicate {
  type Err = String;

  fn from_str(value: &str) -> Result<Self, Self::Err> {
    match value {
      "_=_" => Ok(BinaryPredicate::Equals),
      "_<_" => Ok(BinaryPredicate::LessThan),
      "_>_" => Ok(BinaryPredicate::GreaterThan),
      _ => Err(format!("Invalid binary predicate: {}", value)),
    }
  }
}

impl fmt::Display for BinaryPredicate {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BinaryPredicate::Equals => write!(f, "_=__"),
      BinaryPredicate::LessThan => write!(f, "_<_"),
      BinaryPredicate::GreaterThan => write!(f, "_>_"),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LishpValue {
  Integer(i64),
  Double(f64),
  String(EcoString),
  Symbol(EcoString),
  Bool(bool),
  Nil,

  SpecialForm(SpecialForm),
  BinaryOperator(BinaryOperator),
  BinaryPredicate(BinaryPredicate),

  Cons(Rc<LishpValue>, Rc<LishpValue>),
}

impl From<i64> for LishpValue {
  fn from(value: i64) -> Self {
    LishpValue::Integer(value)
  }
}

impl From<f64> for LishpValue {
  fn from(value: f64) -> Self {
    LishpValue::Double(value)
  }
}

impl From<String> for LishpValue {
  fn from(value: String) -> Self {
    LishpValue::String(value.into())
  }
}

impl From<&str> for LishpValue {
  fn from(value: &str) -> Self {
    LishpValue::String(value.into())
  }
}

impl From<bool> for LishpValue {
  fn from(value: bool) -> Self {
    LishpValue::Bool(value)
  }
}

impl fmt::Display for LishpValue {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LishpValue::Integer(value) => write!(f, "{}", value),
      LishpValue::Double(value) => write!(f, "{}", value),
      LishpValue::String(value) => {
        write!(f, "\"")?;
        for c in value.chars() {
          match c {
            '\n' => write!(f, "\\n")?,
            '\r' => write!(f, "\\r")?,
            '\t' => write!(f, "\\t")?,
            '\\' => write!(f, "\\\\")?,
            '"' => write!(f, "\\\"")?,
            c => write!(f, "{}", c)?,
          }
        }
        write!(f, "\"")
      }
      LishpValue::Symbol(value) => write!(f, "{}", value),
      LishpValue::Bool(value) => write!(f, "{}", value),
      LishpValue::Nil => write!(f, "nil"),
      LishpValue::SpecialForm(value) => write!(f, "{}", value),
      LishpValue::BinaryOperator(value) => write!(f, "{}", value),
      LishpValue::BinaryPredicate(value) => write!(f, "{}", value),
      LishpValue::Cons(_, _) => {
        write!(f, "(")?;

        let mut current = self;
        let mut first = true;

        while let LishpValue::Cons(head, tail) = current {
          if !first {
            write!(f, " ")?;
          }
          write!(f, "{}", head)?;
          first = false;
          current = tail;
        }

        write!(f, ")")
      }
    }
  }
}

pub fn cons(a: LishpValue, b: LishpValue) -> LishpValue {
  LishpValue::Cons(Rc::new(a), Rc::new(b))
}

pub fn car(list: &LishpValue) -> Option<&LishpValue> {
  match list {
    LishpValue::Cons(a, _) => Some(a),
    _ => None,
  }
}

pub fn cdr(list: &LishpValue) -> Option<&LishpValue> {
  match list {
    LishpValue::Cons(_, b) => Some(b),
    _ => None,
  }
}

pub fn is_list(value: &LishpValue) -> bool {
  match value {
    LishpValue::Cons(_, tail) => is_list(tail),
    LishpValue::Nil => true,
    _ => false,
  }
}

pub fn is_pair(value: &LishpValue) -> bool {
  matches!(value, LishpValue::Cons(_, _))
}

pub fn list(items: Vec<LishpValue>) -> LishpValue {
  items
    .into_iter()
    .rev()
    .fold(LishpValue::Nil, |acc, item| cons(item, acc))
}

pub struct Symbol(pub EcoString);

impl From<Symbol> for LishpValue {
  fn from(value: Symbol) -> Self {
    LishpValue::Symbol(value.0.into())
  }
}

#[macro_export]
macro_rules! lishp_list {
    [] => {
        LishpValue::Nil
    };
    [$($elem:expr),* $(,)?] => {{
        let mut result = LishpValue::Nil;
        let elements = vec![$(<LishpValue as From<_>>::from($elem)),*];
        for elem in elements.into_iter().rev() {
            result = cons(elem, result);
        }
        result
    }};
}

#[macro_export]
macro_rules! sym {
  ($s:expr) => {
    $crate::value::Symbol($s.into())
  };
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_cons() {
    let list = lishp_list![1, 2, 3];

    assert_eq!(
      list,
      cons(1.into(), cons(2.into(), cons(3.into(), LishpValue::Nil)))
    );
  }

  #[test]
  fn test_display() {
    let list = lishp_list!["str", 2, 3];
    assert_eq!(list.to_string(), "(\"str\" 2 3)");
  }
}
