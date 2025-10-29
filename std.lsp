(def \n "\n")
(def fn lambda)

(def list (lambda (. args) args))

(def defmacro-codegen (lambda (name args body-list)
  (list def name (list macro args (cons do body-list)))))
(def defmacro (macro (name args . body)
  (eval (defmacro-codegen 'name 'args 'body))))

(def defn-codegen (lambda (name args body-list)
  (list def name (cons lambda (cons args body-list)))))
(defmacro defn (name args . body)
  (eval (defn-codegen 'name 'args 'body)))

(defn nil? (x)
  (_=_ x nil))

;; (cond condition body ... [default])
;; Examples: (cond true 1)  (cond false 1 2)  (cond false 1 true 2)
(defn cond-gen (clauses)
  (if (nil? clauses)
      nil
      (if (nil? (cdr clauses))
          (car clauses)
          (list 'if (car clauses) (car (cdr clauses)) (cond-gen (cdr (cdr clauses)))))))

(defmacro cond (. clauses) (eval (cond-gen 'clauses)))

(defn foldl (f init lst)
  (if (nil? lst)
      init
      (reduce f (f init (car lst)) (cdr lst))))

(def reduce foldl)

(defn foldr (f init lst)
  (if (nil? lst)
      init
      (f (foldr f init (cdr lst)) (car lst))))

(defn str (. xs)
  (reduce (lambda (a b) (_++_ a b)) "" xs))

(defn + (. xs)
  (reduce (lambda (a b) (_+_ a b)) 0 xs))

(defn * (. xs)
  (reduce (lambda (a b) (_*_ a b)) 1 xs))

(defn - (. xs)
  (if (nil? xs)
      (rise "- requires at least one argument")
      (reduce (lambda (a b) (_-_ a b)) (car xs) (cdr xs))))

(defmacro comment (. a)
  nil)
