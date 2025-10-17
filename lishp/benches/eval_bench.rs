use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use lishp::{Environment, Evaluator, StringIoAdapter, parser};
use std::hint::black_box;

fn bench_factorial(c: &mut Criterion) {
  let mut group = c.benchmark_group("factorial");

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
        let mut env = Environment::new();
        let mut evaluator = Evaluator::with_environment(&mut io, &mut env);

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

criterion_group!(benches, bench_factorial,);

criterion_main!(benches);
