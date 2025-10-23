(def unless (macro (cond then else) (if cond else then)))

(print (unless false 1 2))

(print (expand-macro (unless false 1 2)))

(def add-one (macro (x) (_+_ x 1)))

(print (add-one 5))

(print (expand-macro (add-one 5)))

(def id (macro (x) x))
(def wrap (macro (x) (id x)))

(print (wrap 42))

(print (expand-macro (wrap 42)))
