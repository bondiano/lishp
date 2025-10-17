; Factorial test
(print "input a number")
(def n (read))
(def res 1)
(def f
  '(do
    (set! res (_*_ res n))
    (set! n (_-_ n 1))
    (if (_<_ 0 n) (eval f) 0)))
(eval f)
(print res)

