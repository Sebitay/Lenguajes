#lang play

#|

Hizo Ud uso de la whiteboard policy: (Indique SI/NO)
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-
NO

|#

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#|
<prop> ::= (tt)
         | (ff)
         | (p-not <prop>)
         | (p-and listOf<prop>)
         | (p-or listOf<prop>)
         | (p-id <symbol>)
         | (p-where <prop> <symbol> <prop>)
|#

(deftype Prop
  (tt)
  (ff)
  (p-not prop)
  (p-and props)
  (p-or props)
  (p-id sym)
  (p-where prop sym prop2)
  )

;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | (list 'not <s-prop>)
          | (list 'and <s-prop> <s-prop> <s-prop>*)
          | (list 'or <s-prop> <s-prop> <s-prop>*)
          | <symbol>
          | (list <s-prop> 'where list(<symbol> <s-prop>))
|#

;; parse-prop : <s-prop> -> Prop
(define (parse-prop s-expr)
  (match s-expr
    ['true (tt)]
    ['false (ff)]
    [(list 'not prop) (p-not (parse-prop prop))]
    [(list 'and rest ...) #:when (empty? rest) (error 'parse-prop "and expects at least two operands")]
    [(list 'and prop1 rest ...) #:when (empty? rest) (error 'parse-prop "and expects at least two operands")]
    [(list 'and props ...) (p-and (map parse-prop props))]
    [(list 'or rest ...) #:when (empty? rest) (error 'parse-prop "or expects at least two operands")]
    [(list 'or prop1 rest ...) #:when (empty? rest) (error 'parse-prop "or expects at least two operands")]
    [(list 'or props ...) (p-or (map parse-prop props))]
    [(? symbol? p) (p-id p)]
    [(list expr1 'where (list symbol expr2)) (p-where (parse-prop expr1) symbol (parse-prop expr2))]
    ))


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= ...
|#

(deftype PValue
  (ttV)
  (ffV))

;; from-Pvalue : PValue -> Prop
(define (from-Pvalue p-value)
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
(define (p-subst target name substitution)
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-not prop) (p-not (p-subst prop name substitution))]
    [(p-and props) (p-and (map (λ (p) (p-subst p name substitution)) props))]
    [(p-or props) (p-or (map (λ (p) (p-subst p name substitution)) props))]
    [(p-id sym)
     (if (symbol=? sym name) substitution (p-id sym))]
    [(p-where prop sym prop2)
     (if (symbol=? sym name) (p-where prop sym prop2) (p-where (p-subst prop name substitution) sym prop2))]))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps)
  (match ps
    [(list) (ffV)]
    [(list prop rest ...) #:when (equal? (p-eval prop) (ttV)) (ttV)]
    [(list prop rest ...) #:when (equal? (p-eval prop) (ffV)) (eval-or rest)])) 

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps)
  (match ps
    [(list) (ttV)]
    [(list prop rest ...) #:when (equal? (p-eval prop) (ffV)) (ffV)]
    [(list prop rest ...) #:when (equal? (p-eval prop) (ttV)) (eval-and rest)]))

;; p-eval : Prop -> PValue
(define (p-eval p)
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-not prop) (if (equal? (p-eval prop) (ttV)) (ffV) (ttV))]
    [(p-and props) (eval-and props)]
    [(p-or props) (eval-or props)]
    [(p-where prop sym prop2) (p-eval (p-subst prop sym prop2))]
    [(p-id sym) (error 'p-eval "free identifier")]
    ))

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= ...
        | (real <num>)
        | (imaginary <num>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (id <sym>)
        | (with listOf( (cons <sym> <expr>) ) <expr>)
|#
(deftype Expr
  (real n)
  (imaginary n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id s)
  (with d x)
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= ...
        | <num>
        | (<num> i)
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | <sym>
        | (with ( (<sym> <s-expr>)* ) <s-expr>)
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr)
  (match s-expr
    [(? number? n) (real n)]
    [(? symbol? x) (id x)]
    [(list n 'i) #:when (number? n) (imaginary n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with ds x)
     (with (map (λ (d)
              (let ((s (first d))
                    (n (second d)))
                (cons s (parse n))))
            ds)
   (parse x))]))

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
