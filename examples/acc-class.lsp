(def acc-class
  (lambda ()
   (do
     (def get '(lambda () val))
     (def add
       '(lambda (delta)
         (do
           (set! val (_+_ val delta))
           val)))
     (def sub
       '(lambda (delta)
         (do
           (def new (_-_ val delta))
           (if (_<_ new 0) (print "аяяй\n") (set! val new))
           val)))
     (def acc-constructor (lambda (val) (lambda (method) (eval (eval method)))))
     acc-constructor)))
