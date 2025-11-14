use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use lishp::{Environment, Evaluator, StringIoAdapter, parser};
use std::cell::RefCell;
use std::hint::black_box;
use std::rc::Rc;

fn bench_factorial_eval(c: &mut Criterion) {
  let mut group = c.benchmark_group("factorial_eval");

  for n in [5, 10, 15, 20].iter() {
    group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
      b.iter(|| {
        let program = format!(
          r#"
(def n {})
(def res 1)
(def f '(do
      (set! res (_*_ res n))
      (set! n (_-_ n 1))
      (if (_<_ 0 n) (eval f) 0)
      ))
(eval f)
res
"#,
          n
        );

        let mut io = StringIoAdapter::output_only();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

        let mut remaining = program.as_str();
        let mut last_result = None;

        while !remaining.is_empty() {
          match parser::parse(remaining) {
            Ok(Some((value, rest))) => {
              last_result = evaluator.eval(&value).ok();
              remaining = rest.trim();
            }
            Ok(None) => break,
            Err(_) => break,
          }
        }

        black_box(last_result)
      })
    });
  }

  group.finish();
}

fn bench_factorial_lambda(c: &mut Criterion) {
  let mut group = c.benchmark_group("factorial_lambda");

  for n in [5, 10, 15, 20].iter() {
    group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
      b.iter(|| {
        let program = format!(
          r#"
(def factorial (lambda (n) (if (_>_ n 1) (_*_ n (factorial (_-_ n 1))) 1)))
(def result (factorial {}))
"#,
          n
        );

        let mut io = StringIoAdapter::output_only();
        let env = Rc::new(RefCell::new(Environment::new()));
        let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

        let mut remaining = program.as_str();
        let mut last_result = None;

        while !remaining.is_empty() {
          match parser::parse(remaining) {
            Ok(Some((value, rest))) => {
              last_result = evaluator.eval(&value).ok();
              remaining = rest.trim();
            }
            Ok(None) => break,
            Err(_) => break,
          }
        }

        black_box(last_result)
      })
    });
  }

  group.finish();
}

fn bench_lambda_overhead(c: &mut Criterion) {
  c.bench_function("lambda_creation", |b| {
    b.iter(|| {
      let program = r#"
(def + (lambda (x y) (_+_ x y)))
(def * (lambda (x y) (_*_ x y)))
(def - (lambda (x y) (_-_ x y)))

(+ 1 (* 2 (- 3 4)))
"#;

      let mut io = StringIoAdapter::output_only();
      let env = Rc::new(RefCell::new(Environment::new()));
      let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

      let mut remaining = program;
      while !remaining.is_empty() {
        match parser::parse(remaining) {
          Ok(Some((value, rest))) => {
            let _ = evaluator.eval(&value);
            remaining = rest.trim();
          }
          Ok(None) => break,
          Err(_) => break,
        }
      }

      black_box(())
    })
  });

  c.bench_function("lambda_currying", |b| {
    b.iter(|| {
      let program = r#"
(def add (lambda (x y z) (_+_ (_+_ x y) z)))
(def add1 (add 1))
(def add1and2 (add1 2))
(add1and2 3)
"#;

      let mut io = StringIoAdapter::output_only();
      let env = Rc::new(RefCell::new(Environment::new()));
      let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

      let mut remaining = program;
      let mut last_result = None;

      while !remaining.is_empty() {
        match parser::parse(remaining) {
          Ok(Some((value, rest))) => {
            last_result = evaluator.eval(&value).ok();
            remaining = rest.trim();
          }
          Ok(None) => break,
          Err(_) => break,
        }
      }

      black_box(last_result)
    })
  });

  c.bench_function("lambda_variadic", |b| {
    b.iter(|| {
      let program = r#"
(def make-list (lambda (x) x))
(make-list 1 2 3 4 5 6 7 8 9 10)
"#;

      let mut io = StringIoAdapter::output_only();
      let env = Rc::new(RefCell::new(Environment::new()));
      let mut evaluator = Evaluator::with_environment(&mut io, env.clone());

      let mut remaining = program;
      let mut last_result = None;

      while !remaining.is_empty() {
        match parser::parse(remaining) {
          Ok(Some((value, rest))) => {
            last_result = evaluator.eval(&value).ok();
            remaining = rest.trim();
          }
          Ok(None) => break,
          Err(_) => break,
        }
      }

      black_box(last_result)
    })
  });
}

criterion_group!(
  benches,
  bench_factorial_eval,
  bench_factorial_lambda,
  bench_lambda_overhead
);

criterion_main!(benches);
