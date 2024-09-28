#lang play
(require "T2.rkt")

(print-only-errors #t)

;;----- ;;
;; P1.b ;;
;;----- ;;
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop '{not true}) (p-not (tt)))
(test/exn (parse-prop ' (and )) "parse-prop: and expects at least two operands")
(test/exn (parse-prop ' (or )) "parse-prop: or expects at least two operands")
(test/exn (parse-prop ' (and false)) "parse-prop: and expects at least two operands")
(test/exn (parse-prop ' (or true)) "parse-prop: or expects at least two operands")
(test (parse-prop '{and true false false}) (p-and (list (tt) (ff) (ff))))
(test (parse-prop '{or true false false}) (p-or (list (tt) (ff) (ff))))
(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '{x where [x true]}) (p-where (p-id 'x) 'x (tt)))

;;----- ;;
;; P1.c ;;
;;----- ;;
(test (from-Pvalue (ttV)) (tt))
(test (from-Pvalue (ffV)) (ff))

;;----- ;;
;; P1.d ;;
;;----- ;;
(test (p-subst (tt) 'x (ff)) (tt))
(test (p-subst (ff) 'x (tt)) (ff))
(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'y) 'x (tt)) (p-id 'y))
(test (p-subst (p-not (p-id 'x)) 'x (tt)) (p-not (tt)))
(test (p-subst (p-and (list (tt) (p-id 'x) (p-id 'y))) 'x (p-or (list (tt) (ff)))) (p-and (list (tt) (p-or (list (tt) (ff))) (p-id 'y))))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))

;;----- ;;
;; P1.e ;;
;;----- ;;
(test (eval-or '()) (ffV))
(test (eval-or (list (ff))) (ffV))
(test (eval-or (list (tt))) (ttV))
(test (eval-or (list (ff) (ff) (tt))) (ttV))
(test (eval-or (list (ff) (ff) (ff))) (ffV))

(test (eval-and '()) (ttV))
(test (eval-and (list (ff))) (ffV))
(test (eval-and (list (tt))) (ttV))
(test (eval-and (list (tt) (tt) (tt))) (ttV))
(test (eval-and (list (tt) (tt) (ff))) (ffV))

(test (p-eval (tt)) (ttV))
(test (p-eval (ff)) (ffV))
(test (p-eval (p-not (tt))) (ffV))
(test (p-eval (p-and (list (tt) (p-not (ff))))) (ttV))
(test (p-eval (p-or (list (ff) (p-not (tt))))) (ffV))
(test (p-eval (p-where (p-and (list (p-id 'x) (tt))) 'x (tt))) (ttV))
(test/exn (p-eval (p-where (p-and (list (p-id 'x) (tt))) 'y (tt))) "p-eval: free identifier")
(test/exn (p-eval (p-id 'x)) "p-eval: free identifier")

;;----- ;;
;; P2.b ;;
;;----- ;;
(test (parse 'x) (id 'x))
(test (parse 1) (real 1))
(test (parse '(3 i)) (imaginary 3))
(test (parse '(+ 3 2)) (add (real 3) (real 2)))
(test (parse '(- 3 2)) (sub (real 3) (real 2)))
(test (parse '(if0 0 3 2)) (if0 (real 0) (real 3) (real 2)))
(test (parse '(with [(x 0) (y 3)] (+ x y))) (with (list (cons 'x (real 0)) (cons 'y (real 3))) (add (id 'x) (id 'y))))
(test/exn (parse '(with [] (+ 2 3))) "parse: 'with' expects at least one definition")

;;----- ;;
;; P2.c ;;
;;----- ;;
(test (from-CValue (compV 3 2)) (add (real 3) (imaginary 2)))
(test (from-CValue (compV 3 0)) (real 3))
(test (from-CValue (compV 0 3)) (imaginary 3))
(test (from-CValue (compV 0 0)) (real 0))

(test (cmplx+ (compV 3 2) (compV 2 3)) (compV 5 5))
(test (cmplx- (compV 3 2) (compV 2 1)) (compV 1 1))
(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 0 1)) #f)

;;----- ;;
;; P2.d ;;
;;----- ;;
(test (subst-defs (list (cons 'x (id 'y))) 'y (real 5)) (list (cons 'x (real 5))))
(test (subst-defs (list (cons 'x (id 'y))) 'x (real 5)) (list (cons 'x (id 'y))))
(test (subst-defs (list (cons 'x (real 5)) (cons 'y (id 'z))) 'x (imaginary 3)) (list (cons 'x (real 5)) (cons 'y (id 'z))))
(test (subst-defs (list (cons 'x (real 5)) (cons 'y (id 'z))) 'z (imaginary 3)) (list (cons 'x (real 5)) (cons 'y (imaginary 3))))

(test (defs-contain? (list (cons 'x (real 5)) (cons 'y (id 'x)) (cons 'z (imaginary 3))) 'z) #t)
(test (defs-contain? (list (cons 'x (real 5)) (cons 'y (id 'x)) (cons 'z (imaginary 3))) 'a) #f)


(test (subst (real 6) 'x (real 3)) (real 6))
(test (subst (imaginary 6) 'x (real 3)) (imaginary 6))
(test (subst (add (id 'x) (id 'y)) 'x (imaginary 5)) (add (imaginary 5) (id 'y)))
(test (subst (sub (id 'x) (id 'y)) 'y (imaginary 5)) (sub (id 'x) (imaginary 5)))
(test (subst (if0 (id 'x) (real 5) (imaginary 3)) 'x (real 0)) (if0 (real 0) (real 5) (imaginary 3)))
(test (subst (id 'x) 'x (real 18)) (real 18))
(test (subst (with (list (cons 'x (real 1)) (cons 'y (id 'x))) (add (id 'y) (id 'z))) 'z (real 5)) (with (list (cons 'x (real 1)) (cons 'y (id 'x))) (add (id 'y) (real 5))))
(test (subst (with (list (cons 'x (real 1)) (cons 'y (id 'z))) (add (id 'y) (id 'z))) 'z (real 5)) (with (list (cons 'x (real 1)) (cons 'y (real 5))) (add (id 'y) (real 5))))

;;----- ;;
;; P2.e ;;
;;----- ;;
(test (interp (real 5)) (compV 5 0))
(test (interp (imaginary 5)) (compV 0 5))
(test (interp (add (real 5) (imaginary 5))) (compV 5 5))
(test (interp (sub (real 5) (imaginary 5))) (compV 5 -5))
(test (interp (if0 (real 0) (real 5) (imaginary 5))) (compV 5 0))
(test (interp (if0 (real 1) (real 5) (imaginary 5))) (compV 0 5))
(test (interp (with (list (cons 'x (add (real 5) (imaginary 5))) (cons 'y (add (real 0) (imaginary 3)))) (sub (id 'x) (id 'y)))) (compV 5 2))
(test/exn (interp (id 'x)) "interp: free identifier")