(def \n "\n")
(def fn lambda)

(def list (lambda (. args) args))
(def nil? (lambda (x)
  (_=_ x nil)))

(def defmacro-codegen (lambda (name args body-list)
  (list def name (list 'macro args
    (if (nil? body-list) nil
      (if (nil? (cdr body-list))
        (car body-list)
        (cons do body-list)))))))
(def defmacro (macro (name args . body)
  (eval (defmacro-codegen name args body))))

(def defn-codegen (lambda (name args body-list)
  (list def name (list 'lambda args
    (if (nil? body-list) nil
      (if (nil? (cdr body-list))
        (car body-list)
        (cons do body-list)))))))
(defmacro defn (name args . body)
  (eval (defn-codegen name args body)))
(defmacro defun (name args . body)
  (eval (defn-codegen name args body)))

(defmacro comment (. a)
  nil)

(defn cadr (l) (car (cdr l)))
(defn cddr (l) (cdr (cdr l)))

(defn println (x)
  (print (str x \n)))


;; (cond condition body ... [default])
;; Examples: (cond true 1)  (cond false 1 2)  (cond false 1 true 2)
(defmacro cond (. clauses)
  (if (nil? clauses)
    nil
    (if (nil? (cdr clauses))
      (car clauses)
      (list 'if (car clauses) (cadr clauses) (cons 'cond (cddr clauses))))))

(defn foldl (f init lst)
  (if (nil? lst)
      init
      (reduce f (f init (car lst)) (cdr lst))))

(def reduce foldl)

(defn foldr (f init lst)
  (if (nil? lst)
      init
      (f (foldr f init (cdr lst)) (car lst))))

;; Variadic wrapper for core forms

(defn str (. xs)
  (reduce (fn (a b) (_++_ a b)) "" xs))

(defn + (. xs)
  (reduce (fn (a b) (_+_ a b)) 0 xs))

(defn * (. xs)
  (reduce (fn (a b) (_*_ a b)) 1 xs))

(defn - (. xs)
  (cond
    (nil? xs) (raise "- requires at least one argument")
    (nil? (cdr xs)) (_-_ 0 (car xs))
    (reduce (lambda (a b) (_-_ a b)) (car xs) (cdr xs))))

;; Predicates

(defn bp-core (bp a xs)
  (if (nil? xs) true
    (if (bp a (car xs))
      (bp-core bp (car xs) (cdr xs))
      false)))
(defn < (. xs)
  (if (nil? xs)
      true
      (bp-core _<_ (car xs) (cdr xs))))
(defn > (a . xs)
  (if (nil? xs)
      true
      (bp-core _>_ a xs)))
(defn _<=_ (a b) (if (_<_ a b) true (_=_ a b)))
(defn <= (a . xs)
  (if (nil? xs)
      true
      (bp-core _<=_ a xs)))
(defn _>=_ (a b) (if (_>_ a b) true (_=_ a b)))
(defn >= (a . xs)
  (if (nil? xs)
      true
      (bp-core _>=_ a xs)))

(defn =3-tmp-service-fun (a b c) (if (_=_ a b) (_=_ b c) false))
(defn bin-eq (a b)
  (def type-a (typeof a))
  (def type-b (typeof b))
  (cond
    (=3-tmp-service-fun type-a type-b "symbol") (_=_ (str a) (str b))
    (=3-tmp-service-fun type-a type-b "cons") (_=_ (str a) (str b))
    (_=_ a b)))
(defn = (. args) (if (nil? args) true (bp-core bin-eq (car args) (cdr args))))

(defn not (x) (if x false true))

; (and 1 2 3) = (if 1 (if 2 3 false) false)
(defmacro and (. args)
  (if (nil? args)
    true
    (if (nil? (cdr args))
      (car args)
      (list 'if (car args) (cons 'and (cdr args)) false))))

(defmacro or (. args)
  (if (nil? args)
    false
    (if (nil? (cdr args))
      (car args)
      (list 'if (car args) true (cons 'or (cdr args))))))

;; List manipulation

(defn range (a b)
	(if (< a b) (cons a (range (+ 1 a) b)) nil))

(defn range-tail-core (a b acc)
	(if (< a b)
	  (range-tail-core a (- b 1) (cons (- b 1) acc))
	  acc))
(defn range-tail (a b) (range-tail-core a b nil))

(defn map (func xs)
  (if (nil? xs) nil
    (cons (func (car xs)) (map func (cdr xs)))))

(defn reverse-core (xs acc)
  (if (nil? xs) acc
  (reverse-core (cdr xs) (cons (car xs) acc))))
(defn reverse (xs) (reverse-core xs nil))

(defn map-reverse-core (func xs acc)
	(if (nil? xs) acc
    (map-reverse-core func (cdr xs) (cons (func (car xs)) acc))))
(defn map-reverse (func xs) (map-reverse-core func xs nil))
(defn map-tail (func xs) (reverse (map-reverse func xs)))

(defn filter (pred xs)
	(cond (nil? xs) nil
			  (pred (car xs)) (cons (car xs) (filter pred (cdr xs)))
			  (filter pred (cdr xs))))

(defn take (n xs)
	(cond (< n 1) nil
	  		(nil? xs) nil
	  		(cons (car xs) (take (- n 1) (cdr xs)))))

(defn drop (n xs)
	(cond (< n 1) xs
	  		(nil? xs) nil
	  		(drop (- n 1) (cdr xs))))

(defn append-reverse (a b)
	(if (nil? a) b (append-reverse (cdr a) (cons (car a) b))))
(defn append (a b) (append-reverse (reverse a) b))

(defn concat (xs) (reduce append nil xs))

(defn length (xs) (reduce (fn (a b) (+ a 1)) 0 xs))

(defn all? (pred xs)
	(cond (nil? xs) true
	      (pred (car xs)) (all? pred (cdr xs))
	      false))

(defn any? (pred xs)
	(cond (nil? xs) false
	      (pred (car xs)) true
	      (any? pred (cdr xs))))

(defn indexof-from (elem xs idx)
	(cond (nil? xs) -1
	      (= elem (car xs)) idx
	      (indexof-from elem (cdr xs) (+ 1 idx))))
(defn indexof (elem xs) (indexof-from elem xs 0))

(defn range (a b)
	(if (< a b) (cons a (range (+ 1 a) b)) nil))

(defn range-tail-core (a b acc)
	(if (< a b) (range-tail-core a (- b 1) (cons (- b 1) acc)) acc))
(defn range-tail (a b) (range-tail-core a b nil))

(defn list-ref (n xs)
	(def tmp (drop n xs))
	(if (nil? tmp) (raise (++ "index out of bounds: " n " of " xs))
	    (car tmp)))

(defn zip-with (f a b)
	(if (or (nil? a) (nil? b)) nil
	    (cons (f (car a) (car b)) (zip-with f (cdr a) (cdr b)))))

(defn flatten (xs)
  (cond (!= (typeof xss) "List") (list xs)
        (nil? xs) nil
        (append (flatten (car xs)) (flatten (cdr xs)))))

(defn dedupe (xs)
  (if (nil? xs) nil
      (cons (car xs) (dedupe (filter (fn (v) (!= v (car xs))) (cdr xs))))))
