#lang play
(require "T2.rkt")

(print-only-errors #t)

(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop '{not true}) (p-not (tt)))
(test (parse-prop '{and true false false}) (p-and (list (tt) (ff) (ff))))
(test (parse-prop '{or true false false}) (p-or (list (tt) (ff) (ff))))
(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '{x where [x true]}) (p-where (p-id 'x) 'x (tt)))






(test (parse 'x) (id 'x))
(test (parse 1) (real 1))
(test (parse '(3 i)) (imaginary 3))
(test (parse '(+ 3 2)) (add (real 3) (real 2)))
(test (parse '(- 3 2)) (sub (real 3) (real 2)))
(test (parse '(if0 0 3 2)) (if0 (real 0) (real 3) (real 2)))
(test (parse '(with [(x 0) (y 3)] (+ x y))) (with (list (cons 'x (real 0)) (cons 'y (real 3))) (add (id 'x) (id 'y))))