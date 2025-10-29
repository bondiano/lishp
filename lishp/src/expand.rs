use std::collections::HashMap;

use ecow::EcoString;

use crate::value::{LishpValue, car, cdr, cons};
use crate::{Environment, EvalError};

pub struct MacroExpander<'env> {
  env: &'env Environment,
}

impl<'env> MacroExpander<'env> {
  pub fn new(env: &'env Environment) -> Self {
    Self { env }
  }

  pub fn expand(&self, value: &LishpValue) -> Result<LishpValue, EvalError> {
    match value {
      LishpValue::Cons(_, _) => {
        if matches!(value, LishpValue::Nil) {
          return Ok(LishpValue::Nil);
        }

        let head = car(value).ok_or(EvalError::EvalNil)?;
        let tail = cdr(value).ok_or(EvalError::EvalNil)?;

        if let LishpValue::Symbol { name } = head
          && let Some(LishpValue::Macro {
            arguments,
            variadic_arg,
            body,
          }) = self.env.get(name)
        {
          let expanded = self.expand_macro(&arguments, variadic_arg.as_ref(), &body, tail)?;
          return self.expand(&expanded);
        }

        let expanded_head = self.expand(head)?;
        let expanded_tail = self.expand_list(tail)?;
        Ok(cons(expanded_head, expanded_tail))
      }

      _ => Ok(value.clone()),
    }
  }

  fn expand_list(&self, list: &LishpValue) -> Result<LishpValue, EvalError> {
    match list {
      LishpValue::Nil => Ok(LishpValue::Nil),
      LishpValue::Cons(head, tail) => {
        let expanded_head = self.expand(head)?;
        let expanded_tail = self.expand_list(tail)?;
        Ok(cons(expanded_head, expanded_tail))
      }
      _ => Ok(list.clone()),
    }
  }

  fn expand_macro(
    &self,
    parameters: &[EcoString],
    variadic_param: Option<&EcoString>,
    body: &LishpValue,
    args: &LishpValue,
  ) -> Result<LishpValue, EvalError> {
    let mut arg_values = Vec::new();
    let mut current = args;

    while !matches!(current, LishpValue::Nil) {
      if let Some(arg) = car(current) {
        arg_values.push(arg.clone());
        current = cdr(current).unwrap_or(&LishpValue::Nil);
      } else {
        break;
      }
    }

    if let Some(_) = variadic_param {
      if arg_values.len() < parameters.len() {
        return Err(EvalError::WrongArgumentCount {
          form: "macro".to_string(),
          expected: parameters.len(),
          got: arg_values.len(),
        });
      }
    } else {
      if arg_values.len() != parameters.len() {
        return Err(EvalError::WrongArgumentCount {
          form: "macro".to_string(),
          expected: parameters.len(),
          got: arg_values.len(),
        });
      }
    }

    let mut substitutions = HashMap::new();
    for (param, arg) in parameters.iter().zip(arg_values.iter()) {
      substitutions.insert(param.clone(), arg.clone());
    }

    // Handle variadic parameter if present
    if let Some(variadic_name) = variadic_param {
      let rest_args = arg_values[parameters.len()..]
        .iter()
        .rev()
        .fold(LishpValue::Nil, |acc, arg| cons(arg.clone(), acc));
      substitutions.insert(variadic_name.clone(), rest_args);
    }

    Self::substitute(body, &substitutions)
  }

  fn substitute(
    expr: &LishpValue,
    substitutions: &HashMap<EcoString, LishpValue>,
  ) -> Result<LishpValue, EvalError> {
    match expr {
      LishpValue::Symbol { name } => {
        if let Some(replacement) = substitutions.get(name) {
          Ok(replacement.clone())
        } else {
          Ok(expr.clone())
        }
      }

      LishpValue::Cons(head, tail) => {
        let new_head = Self::substitute(head, substitutions)?;
        let new_tail = Self::substitute(tail, substitutions)?;
        Ok(cons(new_head, new_tail))
      }

      _ => Ok(expr.clone()),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::value::{BinaryOperator, SpecialForm};
  use crate::{lishp_list, sym};
  use std::rc::Rc;

  #[test]
  fn test_expand_no_macros() {
    let env = Environment::new();
    let expander = MacroExpander::new(&env);

    let expr = lishp_list![sym!("+"), 1, 2];
    let result = expander.expand(&expr).unwrap();

    assert_eq!(result, expr);
  }

  #[test]
  fn test_expand_simple_macro() {
    // (def my-macro (macro (x) x))
    // (my-macro 42) should expand to 42
    let mut env = Environment::new();

    let macro_value = LishpValue::Macro {
      arguments: vec!["x".into()],
      variadic_arg: None,
      body: Rc::new(LishpValue::Symbol { name: "x".into() }),
    };
    env.define("my-macro", macro_value);

    let expander = MacroExpander::new(&env);

    let expr = lishp_list![sym!("my-macro"), 42];
    let result = expander.expand(&expr).unwrap();

    assert_eq!(result, LishpValue::Integer(42));
  }

  #[test]
  fn test_expand_macro_with_code() {
    // (def unless (macro (cond then else) (if cond else then)))
    // (unless false 1 2) should expand to (if false 2 1)
    let mut env = Environment::new();

    let if_expr = lishp_list![
      LishpValue::SpecialForm(SpecialForm::If),
      sym!("cond"),
      sym!("else"),
      sym!("then")
    ];

    let macro_value = LishpValue::Macro {
      arguments: vec!["cond".into(), "then".into(), "else".into()],
      variadic_arg: None,
      body: Rc::new(if_expr),
    };
    env.define("unless", macro_value);

    let expander = MacroExpander::new(&env);

    let expr = lishp_list![sym!("unless"), false, 1, 2];
    let result = expander.expand(&expr).unwrap();

    let expected = lishp_list![LishpValue::SpecialForm(SpecialForm::If), false, 2, 1];
    assert_eq!(result, expected);
  }

  #[test]
  fn test_expand_nested_macros() {
    let mut env = Environment::new();

    // (def id (macro (x) x))
    let id_macro = LishpValue::Macro {
      arguments: vec!["x".into()],
      variadic_arg: None,
      body: Rc::new(LishpValue::Symbol { name: "x".into() }),
    };
    env.define("id", id_macro);

    // (def wrap (macro (x) (id x)))
    let wrap_body = lishp_list![sym!("id"), sym!("x")];
    let wrap_macro = LishpValue::Macro {
      arguments: vec!["x".into()],
      variadic_arg: None,
      body: Rc::new(wrap_body),
    };
    env.define("wrap", wrap_macro);

    let expander = MacroExpander::new(&env);

    // (wrap 42) should expand to 42 (through id)
    let expr = lishp_list![sym!("wrap"), 42];
    let result = expander.expand(&expr).unwrap();

    assert_eq!(result, LishpValue::Integer(42));
  }

  #[test]
  fn test_expand_macro_generating_expression() {
    // (def add-one (macro (x) (_+_ x 1)))
    // (add-one 5) should expand to (_+_ 5 1)
    let mut env = Environment::new();

    let add_expr = lishp_list![
      LishpValue::BinaryOperator(BinaryOperator::Add),
      sym!("x"),
      1
    ];

    let macro_value = LishpValue::Macro {
      arguments: vec!["x".into()],
      variadic_arg: None,
      body: Rc::new(add_expr),
    };
    env.define("add-one", macro_value);

    let expander = MacroExpander::new(&env);

    let expr = lishp_list![sym!("add-one"), 5];
    let result = expander.expand(&expr).unwrap();

    let expected = lishp_list![LishpValue::BinaryOperator(BinaryOperator::Add), 5, 1];
    assert_eq!(result, expected);
  }

  #[test]
  fn test_expand_wrong_argument_count() {
    let mut env = Environment::new();

    let macro_value = LishpValue::Macro {
      arguments: vec!["x".into(), "y".into()],
      variadic_arg: None,
      body: Rc::new(sym!("x").into()),
    };
    env.define("my-macro", macro_value);

    let expander = MacroExpander::new(&env);

    let expr = lishp_list![sym!("my-macro"), 1];
    let result = expander.expand(&expr);

    assert!(result.is_err());
    assert!(matches!(result, Err(EvalError::WrongArgumentCount { .. })));
  }
}
