use lishp::*;

#[test]
fn test_macro_simple_identity() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def my-macro (macro (x) x))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  // Call macro: (my-macro 42)
  let (call_expr, _) = parser::parse("(my-macro 42)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(42));
}

#[test]
fn test_macro_code_transformation() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def unless (macro (cond then else) (if cond else then)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (call_expr, _) = parser::parse("(unless false 1 2)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(1));
}

#[test]
fn test_macro_with_computation() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def add-one (macro (x) (_+_ x 1)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (call_expr, _) = parser::parse("(add-one 5)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(6));
}

#[test]
fn test_macro_nested_expansion() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_id, _) = parser::parse("(def id (macro (x) x))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_id).unwrap();

  let (def_wrap, _) = parser::parse("(def wrap (macro (x) (id x)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_wrap).unwrap();

  let (call_expr, _) = parser::parse("(wrap 42)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(42));
}

#[test]
fn test_macro_with_unevaluated_args() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def quote-it (macro (x) (quote x)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (call_expr, _) = parser::parse("(quote-it undefined-symbol)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(
    result,
    value::LishpValue::Symbol {
      name: "undefined-symbol".into()
    }
  );
}

#[test]
fn test_expand_macro_simple() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def my-macro (macro (x) x))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (expand_expr, _) = parser::parse("(expand-macro (my-macro 42))")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&expand_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(42));
}

#[test]
fn test_expand_macro_unless() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def unless (macro (cond then else) (if cond else then)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (expand_expr, _) = parser::parse("(expand-macro (unless false 1 2))")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&expand_expr).unwrap();

  let (expected, _) = parser::parse("(if false 2 1)")
    .expect("parse failed")
    .expect("no expression");
  assert_eq!(result, expected);
}

#[test]
fn test_expand_macro_nested() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_id, _) = parser::parse("(def id (macro (x) x))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_id).unwrap();

  let (def_wrap, _) = parser::parse("(def wrap (macro (x) (id x)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_wrap).unwrap();

  let (expand_expr, _) = parser::parse("(expand-macro (wrap 42))")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&expand_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(42));
}

#[test]
fn test_expand_macro_no_macros() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (expand_expr, _) = parser::parse("(expand-macro (_+_ 1 2))")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&expand_expr).unwrap();

  let (expected, _) = parser::parse("(_+_ 1 2)")
    .expect("parse failed")
    .expect("no expression");
  assert_eq!(result, expected);
}

#[test]
fn test_macro_multi_step() {
  let mut io = MockIoAdapter::new(vec![]);
  let mut env = Environment::new();
  let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

  let (def_expr, _) = parser::parse("(def when (macro (cond body) (if cond (do body) nil)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (call_expr, _) = parser::parse("(when true 42)")
    .expect("parse failed")
    .expect("no expression");
  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(42));

  let (expand_expr, _) = parser::parse("(expand-macro (when true 42))")
    .expect("parse failed")
    .expect("no expression");
  let expanded = evaluator.eval(&expand_expr).unwrap();

  let (expected, _) = parser::parse("(if true (do 42) nil)")
    .expect("parse failed")
    .expect("no expression");
  assert_eq!(expanded, expected);
}
