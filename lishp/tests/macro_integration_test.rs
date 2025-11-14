use lishp::*;
use std::cell::RefCell;
use std::rc::Rc;

#[test]
fn test_macro_simple_identity() {
  let mut io = MockIoAdapter::new(vec![]);
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def unless (macro (cond then else) (cons (quote if) (cons cond (cons else (cons then nil))))))")
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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def add-one (macro (x) (_+_ x 1)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).expect("macro definition failed");

  let (call_expr, _) = parser::parse("(add-one 5)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).expect("macro call failed");
  assert_eq!(result, value::LishpValue::Integer(6));
}

#[test]
fn test_macro_nested_expansion() {
  let mut io = MockIoAdapter::new(vec![]);
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_id, _) = parser::parse("(def id (macro (x) x))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_id).expect("macro definition failed");

  let (def_wrap, _) = parser::parse("(def wrap (macro (x) (id x)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_wrap).expect("macro definition failed");

  let (call_expr, _) = parser::parse("(wrap 42)")
    .expect("parse failed")
    .expect("no expression");

  let result = evaluator.eval(&call_expr).expect("macro call failed");
  assert_eq!(result, value::LishpValue::Integer(42));
}

#[test]
fn test_macro_with_unevaluated_args() {
  let mut io = MockIoAdapter::new(vec![]);
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def quote-it (macro (x) (cons (quote quote) (cons x nil))))")
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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def unless (macro (cond then else) (cons (quote if) (cons cond (cons else (cons then nil))))))")
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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

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
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def when (macro (cond body) (cons (quote if) (cons cond (cons (cons (quote do) (cons body nil)) (cons nil nil))))))")
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
#[test]
fn test_macro_parameter_cleanup() {
  let mut io = MockIoAdapter::new(vec![]);
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def test-macro (macro (x) (_+_ x 1)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (call_expr, _) = parser::parse("(test-macro 5)")
    .expect("parse failed")
    .expect("no expression");
  let result = evaluator.eval(&call_expr).unwrap();
  assert_eq!(result, value::LishpValue::Integer(6));

  assert!(
    env.borrow().get("x").is_none(),
    "Macro parameter x should not leak into environment"
  );
}

#[test]
fn test_macro_parameter_restoration() {
  let mut io = MockIoAdapter::new(vec![]);
  let env = Rc::new(RefCell::new(Environment::new()));

  env
    .borrow_mut()
    .define("x", value::LishpValue::Integer(100));

  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  let (def_expr, _) = parser::parse("(def test-macro (macro (x) (_+_ x 1)))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_expr).unwrap();

  let (call_expr, _) = parser::parse("(test-macro 5)")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&call_expr).unwrap();

  assert_eq!(
    env.borrow().get("x"),
    Some(value::LishpValue::Integer(100)),
    "Original x should be restored after macro expansion"
  );
}

#[test]
fn test_macro_in_constructed_list_with_eval() {
  let mut io = MockIoAdapter::new(vec![]);
  let env = Rc::new(RefCell::new(Environment::new()));
  let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

  // Define += macro using binary operator _+_ instead of + function
  let (def_macro, _) = parser::parse("(def += (macro (x v) (cons (quote set!) (cons x (cons (cons (quote _+_) (cons x (cons v nil))) nil)))))")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_macro).unwrap();

  // Define initial value
  let (def_x, _) = parser::parse("(def x 5)")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&def_x).unwrap();

  // Test 1: Direct macro call should work
  let (direct_call, _) = parser::parse("(+= x 3)")
    .expect("parse failed")
    .expect("no expression");
  evaluator.eval(&direct_call).unwrap();
  assert_eq!(env.borrow().get("x"), Some(value::LishpValue::Integer(8)));

  // Test 2: Macro in constructed list with eval should also work
  // This simulates: (eval (list '+= 'x 2))
  let (constructed_call, _) =
    parser::parse("(eval (cons (quote +=) (cons (quote x) (cons 2 nil))))")
      .expect("parse failed")
      .expect("no expression");
  evaluator.eval(&constructed_call).unwrap();
  assert_eq!(env.borrow().get("x"), Some(value::LishpValue::Integer(10)));
}
