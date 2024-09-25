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


(test (p-subst (p-not (p-id 'x)) 'x (p-and (list (tt) (ff)))) (p-not (p-and (list (tt)(ff)))))

(test (p-eval (p-or (list (tt) (ff)))) (ttV))
(test (p-eval (p-where (p-not (p-or (list (ff) (p-id 'x)))) 'x (p-and (list (tt) (ff))))) (ttV))