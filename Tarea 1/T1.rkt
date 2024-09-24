#lang play
(require math/flonum)

#|
Complete sus datos personales:
NOMBRE Y APELLIDO: Sebastian Valenzuela
RUT:20.286.155-5
|#

;; Parte a)

#|
<cfrac> ::= (simple <num>)
         | (compound <num> <num> <cfrac>)
|#
(deftype CFraction
  (simple n)
  (compound a b cfrac))


;; Parte b)
;; eval :: CFraction -> Rational
;; evaluates a CFraction
(define (eval cfrac)
  (match cfrac
    [(simple n) n]
    [(compound a b cfrac) (+ a (/ b (eval cfrac)))]))


;; Parte c)
;; degree ::  CFraction -> Integer
;; calculates a CFraction's degree
(define (degree cfrac)
  (match cfrac
    [(simple n) 0]
    [(compound a b cfrac) (+ 1 (degree cfrac))]))


;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Fold over CFractions
(define (fold-cfraction f g)
  (λ (cfrac)
    (match cfrac
      [(simple n) (f n)]
      [(compound a b c) (g a b ((fold-cfraction f g) c))])))


;; Parte e)
;; eva2 :: CFraction -> Rational
;; evaluates a CFraction using fold-cfraction
(define (eval2 cfrac) 
  ((fold-cfraction (λ (x) x) (λ (x y z) (+ x (/ y z)))) cfrac))


;; degree2 ::  CFraction -> Integer
;; calculates a CFraction's degree using fold-cfraction
(define (degree2 cfrac) 
  ((fold-cfraction (λ (x) 0) (λ (x y z) (+ 1 z))) cfrac))


;; Parte f)
;; mysterious-cf-aux :: Integer Integer -> CFraction
;; mysterious-cf :: Integer -> CFraction
;; function that generates CRfaction of degree n
(define (mysterious-cf-aux n i)
  (match i
    [i #:when (equal? n i) (simple 6)]
    [i (compound 6 (sqr (+ (* 2 i) 1)) (mysterious-cf-aux n (+ i 1)))]))

;; mysterious-cf :: Integer -> CFraction
;; generates a CFraction of degree n using mysteriouse-cf-aux
(define (mysterious-cf n)
  (match n
    [n #:when (< n 0) (error "mysterious-cf uses non-negative integers")]
    [n (mysterious-cf-aux n 0)]))


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; generates a list of integers from the first integer to the second
(define (from-to a b)
  (match a
    [a #:when (> a b) (error "firt integer must be smaller than the second integer")]
    [a #:when (equal? (+ a 1) b) (list a)]
    [a (list* a (from-to (+ a 1) b))]))

;; mysterious-list :: Integer -> ListOf Float
;; generates a list of the first n values of mysterious-cf
(define (mysterious-list n)
  (map (λ (x) (fl (- (eval (mysterious-cf x)) 3))) (from-to 0 n)))

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; A pi (3.1415)

;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; transforms a non-negative rational to its CFraction form
(define (rac-to-cf x)
  (let ([rest (floor x)])
    (match x
      [x #:when (equal? x rest) (simple rest)]
      [x (compound rest 1 (rac-to-cf (/ 1 (- x rest))))])))