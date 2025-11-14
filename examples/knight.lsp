(def n 5)
(def m 4)
(def start-position '(1 1))

;; path - '((1 1) (4 2) (0 0))

(def knight-moves '((2 1) (2 -1) (-2 1) (-2 -1) (1 2) (1 -2) (-1 2) (-1 -2)))

(defn check-pos? (p) (and (<= 1 (car p) n) (<= 1 (car (cdr p)) m)))

(def all-cells
    (concat (map (lambda (r)
         (map (lambda (c)
              (list r c))
              (range 1 (+ 1 m))))
         (range 1 (+ 1 n)))))

(def valid-steps
  (concat (map (lambda (cell)
                 (filter (lambda (step) (check-pos? (car step)))
                         (map (lambda (delta-move) (list (zip-with + cell delta-move) cell))
                              knight-moves)))
           all-cells)))

(def steps-init-code
  (cons do (map (lambda (step) (list def (symbol (++ step)) false)) valid-steps)))

(defn steps-set-gen ()
  (eval steps-init-code)
  (defn add (x) (eval (list set! (symbol (++ x)) true)))
  (defn has? (x) (eval (symbol (++ x))))
  (defn remove (x) (eval (list set! (symbol (++ x)) false)))
  (lambda (cmd) (eval cmd)))

(def steps-set (steps-set-gen))

(defn check-step? (step path) (not ((steps-set 'has?) step)))

;; (defn check-step? (step path)  ; '(to from)
;;   (cond (or (nil? path) (nil? (cdr path))) true
;;         (= (take 2 path) step) false
;;         (check-step? step (cdr path))))

(def cells-init-code
  (cons do (map (lambda (cell) (list def (symbol (++ cell)) 0)) all-cells)))

(defn cells-multiset-gen ()
  (eval cells-init-code)
  (def distinct 0)
  (defn add (x) (do
                (def name (symbol (++ x)))
                (when (= (eval name) 0) (+= distinct 1))
                (eval (list '+= name 1))))
  (defn remove (x) (do
                (def name (symbol (++ x)))
                (when (= (eval name) 1) (-= distinct 1))
                (eval (list '-= name 1))))
  (lambda (cmd) (eval cmd)))

(def cells-multiset (cells-multiset-gen))

(defn dfs (path)
  (if (and (= (car path) start-position)
           (= (cells-multiset 'distinct) (* n m)))
      path
      (reduce (lambda (acc, x)
                (if (!= acc false) acc
                    (do
                        (def new-pos (zip-with + x (car path)))
                        (def step (list new-pos (car path)))
                        (if (and (check-pos? new-pos) (check-step? step path))
                            (do ((steps-set 'add) step)
                                ((cells-multiset 'add) new-pos)
                                (def res (dfs (cons new-pos path)))
                                ((steps-set 'remove) step)
                                ((cells-multiset 'remove) new-pos)
                                res)
                            false))))
              false
              knight-moves)))
