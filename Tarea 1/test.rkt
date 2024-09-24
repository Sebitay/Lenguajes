#lang play

(require "T1.rkt")
(require math/flonum)
(print-only-errors #t)

;;eval tests
(test (eval (simple 2)) 2)
(test (eval (compound 1 2 (simple 2))) 2)
(test (eval (compound 1 3 (compound 4 4 (simple 2)))) 1.5)

;;degree tests
(test (degree (simple 2)) 0)
(test (degree (compound 1 2 (simple 2))) 1)
(test (degree (compound 1 3 (compound 4 4 (simple 2)))) 2)

;;fold-eval tests
(test (eval2 (simple 2)) 2)
(test (eval2 (compound 1 2 (simple 2))) 2)
(test (eval2 (compound 1 3 (compound 4 4 (simple 2)))) 1.5)

;;fold-degree tests
(test (degree2 (simple 2)) 0)
(test (degree2 (compound 1 2 (simple 2))) 1)
(test (degree2 (compound 1 3 (compound 4 4 (simple 2)))) 2)

;;mysterious-cf tests
(test (mysterious-cf 0) (simple 6))
(test (mysterious-cf 1) (compound 6 1 (simple 6)))
(test (mysterious-cf 2) (compound 6 1 (compound 6 9 (simple 6))))
(test (mysterious-cf 3) (compound 6 1 (compound 6 9 (compound 6 25 (simple 6)))))
(test (mysterious-cf 4) (compound 6 1 (compound 6 9 (compound 6 25 (compound 6 49 (simple 6))))))

;;from-to tests
(test (from-to 0 1) (list 0))
(test (from-to 0 5) (list 0 1 2 3 4))
(test (from-to 3 6) (list 3 4 5))

;;mysterious-list tests
(test (mysterious-list 1) (map (λ (x) (fl x)) (list 3)))
(test (mysterious-list 2) (map (λ (x) (fl x)) (list 3 (+ 3 (/ 1 6)))))
(test (mysterious-list 3) (map (λ (x) (fl x)) (list 3 (+ 3 (/ 1 6)) (+ 3 (/ 1 (+ 6 (/ 9 6)))))))


;;rac-to-cf test
(test (rac-to-cf 4) (simple 4))
(test (rac-to-cf (+ 4 1/2)) (compound 4 1 (simple 2)))
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
