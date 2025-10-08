use std::{fmt, rc::Rc, str::FromStr};

#[derive(Debug, Clone, PartialEq)]
pub enum SpecialForm {
  Define,
  If,
  Quote,
}

impl FromStr for SpecialForm {
  type Err = String;

  fn from_str(value: &str) -> Result<Self, Self::Err> {
    match value {
      "def" => Ok(SpecialForm::Define),
      "if" => Ok(SpecialForm::If),
      "quote" => Ok(SpecialForm::Quote),
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
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LishpValue {
  Integer(i64),
  Double(f64),
  String(String),
  Symbol(String),
  Bool(bool),
  Nil,

  SpecialForm(SpecialForm),

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
    LishpValue::String(value)
  }
}

impl From<&str> for LishpValue {
  fn from(value: &str) -> Self {
    LishpValue::String(value.to_string())
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

pub struct Symbol(pub &'static str);

impl From<Symbol> for LishpValue {
  fn from(value: Symbol) -> Self {
    LishpValue::Symbol(value.0.to_string())
  }
}

#[macro_export]
macro_rules! lishp_list {
    [] => {
        LishpValue::Nil
    };
    [$($elem:expr),* $(,)?] => {{
        let mut result = LishpValue::Nil;
        let elems = vec![$(<LishpValue as From<_>>::from($elem)),*];
        for elem in elems.into_iter().rev() {
            result = cons(elem, result);
        }
        result
    }};
}

#[macro_export]
macro_rules! sym {
  ($s:expr) => {
    $crate::value::Symbol($s)
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
      LishpValue::Cons(
        Rc::new(1.into()),
        Rc::new(LishpValue::Cons(
          Rc::new(2.into()),
          Rc::new(LishpValue::Cons(
            Rc::new(3.into()),
            Rc::new(LishpValue::Nil)
          ))
        ))
      )
    );
  }

  #[test]
  fn test_display() {
    let list = lishp_list!["str", 2, 3];
    assert_eq!(list.to_string(), "(\"str\" 2 3)");
  }
}
