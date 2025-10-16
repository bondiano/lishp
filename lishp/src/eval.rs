use std::rc::Rc;

use thiserror::Error;

use crate::value::{BinaryOperator, LishpValue, cons};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum EvalError {
  #[error("Invalid binary operator: {0}")]
  InvalidBinaryOperator(BinaryOperator),
}

// pub fn eval(value: LishpValue) -> Result<LishpValue, EvalError> {
//   match value {
//     LishpValue::Cons(head, tail) => {
//       let head = eval(head)?;
//       let tail = eval(tail)?;
//       Ok(cons(head, tail))
//     }
//     v => Ok(v.clone()),
//   }
// }

fn apply_binary_operator(
  operator: BinaryOperator,
  a: LishpValue,
  b: LishpValue,
) -> Result<LishpValue, EvalError> {
  match (operator, a, b) {
    (BinaryOperator::Add, LishpValue::Integer(a), LishpValue::Integer(b)) => {
      Ok(LishpValue::Integer(a + b))
    }
    (BinaryOperator::Subtract, LishpValue::Integer(a), LishpValue::Integer(b)) => {
      Ok(LishpValue::Integer(a - b))
    }
    (BinaryOperator::Multiply, LishpValue::Integer(a), LishpValue::Integer(b)) => {
      Ok(LishpValue::Integer(a * b))
    }
    (BinaryOperator::Divide, LishpValue::Integer(a), LishpValue::Integer(b)) => {
      Ok(LishpValue::Integer(a / b))
    }
    (BinaryOperator::Add, LishpValue::Double(a), LishpValue::Double(b)) => {
      Ok(LishpValue::Double(a + b))
    }
    (BinaryOperator::Subtract, LishpValue::Double(a), LishpValue::Double(b)) => {
      Ok(LishpValue::Double(a - b))
    }
    (BinaryOperator::Multiply, LishpValue::Double(a), LishpValue::Double(b)) => {
      Ok(LishpValue::Double(a * b))
    }
    (BinaryOperator::Divide, LishpValue::Double(a), LishpValue::Double(b)) => {
      Ok(LishpValue::Double(a / b))
    }
    (BinaryOperator::StrConcat, LishpValue::String(mut a), LishpValue::String(b)) => {
      a.push_str(&b);
      Ok(LishpValue::String(a))
    }
    (operator, _, _) => Err(EvalError::InvalidBinaryOperator(operator)),
  }
}
