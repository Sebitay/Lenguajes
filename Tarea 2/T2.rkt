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
(define (eval-or ps) '???)

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps) '???)

;; p-eval : Prop -> PValue
(define (p-eval p) '???)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= ...
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | ...
|#
(deftype Expr
  ; ...
  (add l r)
  (sub l r)
  (if0 c t f)
  ; ...
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= ...
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | ...
|#

;; parse : <s-expr> -> Expr

(define (parse s-expr) '???)

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
