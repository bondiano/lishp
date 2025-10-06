use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use lishp::parser::parse;

fn generate_complex_program() -> String {
  let mut program = String::new();

  program.push_str(
    r#"
; First level comment
;; Second level comment
;;; Third level comment

(def MAX-ITERATIONS 1000)
(def config
  (list "debug" false
        "timeout" 5000
        "retries" 3))

(def sample-data
  (list (list "id" 1 "name" "Alice" "age" 30)
        (list "id" 2 "name" "Bob" "age" 25)
        (list "id" 3 "name" "Charlie" "age" 35)
        (list "id" 4 "name" "Diana" "age" 28)
        (list "id" 5 "name" "Eve" "age" 32)))

(def factorial
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial (- n 1))))))

(def process-message
  (lambda (msg)
    (cond
      ((eq msg error) (println "Error"))
      ((eq msg warning) (println "Warning"))
      ((eq msg info) (println "Info"))
      (t (println "Unknown")))))

(def transform
  (lambda (x f opts)
    (let ((current x)
          (depth 0))
      (while (< depth 10)
        (let ((next (f current)))
          (if (eq next current)
            next
            (begin
              (set! current next)
              (set! depth (+ depth 1)))))))))

(def analyze-data
  (lambda (users)
    (let ((stats (calculate-stats users))
          (first-user (car users))
          (rest-users (cdr users)))
      (list "statistics" stats
            "first" first-user
            "rest" rest-users))))

; Function examples
(def inc-fn (lambda (x) (+ x 1)))
(def add-fn (lambda (x y) (+ x y)))
(map (lambda (x) (* x 2)) (list 1 2 3 4 5))
(filter (lambda (x) (> x 10)) sample-data)
(reduce (lambda (acc x) (+ acc x)) 0 (list 1 2 3 4 5))

(def complex-structure
  (list "users" (list (list "id" 0 "username" "user0" "email" "user0@example.com")
                      (list "id" 1 "username" "user1" "email" "user1@example.com")
                      (list "id" 2 "username" "user2" "email" "user2@example.com")
                      (list "id" 3 "username" "user3" "email" "user3@example.com")
                      (list "id" 4 "username" "user4" "email" "user4@example.com"))
        "posts" (list (list "id" 0 "author-id" 0 "title" "Post 0" "content" "Content for post 0")
                      (list "id" 1 "author-id" 1 "title" "Post 1" "content" "Content for post 1")
                      (list "id" 2 "author-id" 2 "title" "Post 2" "content" "Content for post 2")
                      (list "id" 3 "author-id" 3 "title" "Post 3" "content" "Content for post 3")
                      (list "id" 4 "author-id" 4 "title" "Post 4" "content" "Content for post 4"))
        "metadata" (list "created-at" "2024-01-01"
                         "updated-at" "2024-01-15"
                         "version" 42)))

(def template
  "Hello world, your account has credits.")

(quote (a b c))

(if true
  (process 1)
  (handle-nil))

(let ((result (expensive-computation)))
  (println result))

(begin
  (println "Line 1")
  (println "Line 2")
  (println "Line 3"))

; Fibonacci function
(def fib
  (lambda (n)
    (if (eq n 0) 0
      (if (eq n 1) 1
        (+ (fib (- n 1)) (fib (- n 2)))))))

; Single arity function
(def factorial-multi
  (lambda (n)
    (if (<= n 1)
      1
      (* n (factorial-multi (- n 1))))))

; Pattern destructuring in function args
(def process-point
  (lambda (point)
    (let ((x (car point))
          (y (car (cdr point))))
      (sqrt (+ (* x x) (* y y))))))

; Simple nested lists
(def nested-data
  (list (list 1 2 3)
        (list 4 5 6)
        (list 7 8 9)))
"#,
  );

  for i in 0..120 {
    let data_elements = (0..12)
      .map(|j| format!("{} {}", j, j * 2))
      .collect::<Vec<_>>()
      .join(" ");
    let var_name = format!("var-{}", i);
    let func_name = format!("func-{}", i);
    let data_name = format!("data-{}", i);
    let value_name = format!("value-{}", i);

    program.push_str(&format!(
      r#"
(def {} {})
(def {}
  (lambda (x y)
    (let ((a (* x 2))
          (b (+ y 3)))
      (if (> a b) a b))))

(def {}
  (list {}))

(cond
  ((eq {} a) {})
  (t nil))

; While loop with mutable counter
(def counter 0)
(while (< counter 5)
  (begin
    (println counter)
    (set! counter (+ counter 1))))

; Function usage in iteration
(map (lambda (x) (+ x 10)) (list 1 2 3 4 5))

; Quote expressions
(quote (process-batch items))
(quote (list first middle last))
"#,
      var_name, i, func_name, data_name, data_elements, value_name, value_name,
    ));
  }

  program
}

fn parse_throughput(c: &mut Criterion) {
  let program = generate_complex_program();
  let size_bytes = program.len();

  c.benchmark_group("throughput")
    .throughput(Throughput::Bytes(size_bytes as u64))
    .bench_function("complex_program_throughput", |b| {
      b.iter(|| {
        let input = std::hint::black_box(&program);
        let mut remaining = input.as_str();
        let mut count = 0;

        while !remaining.is_empty() {
          match parse(remaining) {
            Ok(Some((_, rest))) => {
              remaining = rest;
              count += 1;
            }
            Ok(None) => break,
            Err(_) => break,
          }
        }

        std::hint::black_box(count)
      })
    });
}

fn parse_different_sizes(c: &mut Criterion) {
  let mut group = c.benchmark_group("file_sizes");

  let small = "(def add (lambda (x y) (+ x y)))";
  group.throughput(Throughput::Bytes(small.len() as u64));
  group.bench_function("small_file", |b| {
    b.iter(|| {
      let input = std::hint::black_box(small);
      match parse(input) {
        Ok(Some((value, _))) => std::hint::black_box(value),
        _ => panic!("Failed to parse"),
      }
    })
  });

  let medium = r#"
(def factorial
  (lambda (n)
    (if (<= n 1) 1 (* n (factorial (- n 1))))))

(def fibonacci
  (lambda (n)
    (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(def constants (list pi 3.14159 e 2.71828))
"#;
  group.throughput(Throughput::Bytes(medium.len() as u64));
  group.bench_function("medium_file", |b| {
    b.iter(|| {
      let input = std::hint::black_box(medium);
      let mut remaining = input;
      let mut count = 0;

      while !remaining.is_empty() {
        match parse(remaining) {
          Ok(Some((_, rest))) => {
            remaining = rest;
            count += 1;
          }
          Ok(None) => break,
          Err(_) => break,
        }
      }

      std::hint::black_box(count)
    })
  });

  let large = generate_complex_program();
  group.throughput(Throughput::Bytes(large.len() as u64));
  group.bench_function("large_file", |b| {
    b.iter(|| {
      let input = std::hint::black_box(&large);
      let mut remaining = input.as_str();
      let mut count = 0;

      while !remaining.is_empty() {
        match parse(remaining) {
          Ok(Some((_, rest))) => {
            remaining = rest;
            count += 1;
          }
          Ok(None) => break,
          Err(_) => break,
        }
      }

      std::hint::black_box(count)
    })
  });

  group.finish();
}

criterion_group!(benches, parse_throughput, parse_different_sizes);

criterion_main!(benches);
