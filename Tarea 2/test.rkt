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
