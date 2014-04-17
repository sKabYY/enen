#lang racket

; Expression = Variable
;            | Number
;            | (lambda Variable Expression)
;            | (fix Variable Variable Expression)
;            | (+ Expression Expression)
;            | (- Expression Expression)
;            | (iszero Expression)
;            | (Expression Expression)

(define (interp exp1) (value-of (translate exp1)))

(define (translate exp1)
  (match exp1
    ; an if statement
    [`(if ,e1 ,e2 ,e3)
     `((,(translate e1) ,(translate e2)) ,(translate e3))]
    ; a let statement
    [`(let ,var ,e1 ,body)
     `((lambda ,var ,(translate body)) ,(translate e1))]
    ; a letrec statement
    [`(letrec ,f ,a ,b ,body)
     `((lambda ,f ,(translate body)) (fix ,f ,a ,(translate b)))]
    ; a procedure
    [`(lambda ,a ,b) `(lambda ,a ,(translate b))]
    ; a recursive procedure
    [`(fix ,f ,a ,b) `(fix ,f ,a ,(translate b))]
    ; primitive operations
    [`(+ ,e1 ,e2) `(+ ,(translate e1) ,(translate e2))]
    [`(- ,e1 ,e2) `(- ,(translate e1) ,(translate e2))]
    [`(iszero ,e1) `(iszero ,(translate e1))]
    ; an application
    [`(,e1 ,e2) `(,(translate e1) ,(translate e2))]
    [else exp1]))

(define (value-of exp1)
  (match exp1
    ; a variable
    [(? symbol? s) s]
    ; a number
    [(? number? n) n]
    ; a procedure
    [`(lambda ,a ,b) exp1]
    ; a recursive procedure
    [`(fix ,f ,a ,b)
     `(lambda ,a ,(substitute b f exp1))]
    ; primitive operations
    [`(+ ,e1 ,e2) (+ (value-of e1) (value-of e2))]
    [`(- ,e1 ,e2) (- (value-of e1) (value-of e2))]
    [`(iszero ,e1) (iszero (value-of e1))]
    ; an application
    [`(,e1 ,e2)
     (match (value-of e1)
       [`(lambda ,var ,body)
        (value-of (substitute body var e2))])]))

(define (substitute exp1 var val)
  (define (>> e) (substitute e var val))
  (match exp1
    ; a variable
    [(? symbol? s) (if (eqv? s var) val s)]
    ; a procedure
    [`(lambda ,a ,b)
     (if (eqv? a var)
         exp1
         (let ((tmp-var (new-tmp-var)))
           `(lambda ,tmp-var ,(>> (substitute b a tmp-var)))))]
    ; a recursive procedure
    [`(fix ,f ,a ,b)
     (if (or (eqv? f var) (eqv? a var))
         exp1
         (let ((tmp-f (new-tmp-var))
               (tmp-a (new-tmp-var)))
           `(fix ,tmp-f ,tmp-a
                 ,(>> (substitute (substitute b a tmp-a) f tmp-f)))))]
    ; primitive operations
    [`(+ ,e1 ,e2) `(+ ,(>> e1) ,(>> e2))]
    [`(- ,e1 ,e2) `(- ,(>> e1) ,(>> e2))]
    [`(iszero ,e1) `(iszero ,(>> e1))]
    ; an application
    [`(,e1 ,e2) `(,(>> e1) ,(>> e2))]
    ; else
    [else exp1]))

(define the-true '(lambda x (lambda y x)))
(define the-false '(lambda x (lambda y y)))

(define (iszero n)
  (if (= n 0)
      the-true
      the-false))

; temporary variable
(define the-temp-idx 0)
(define (new-tmp-var)
  (set! the-temp-idx (+ the-temp-idx 1))
  (string->symbol (string-append "#" (number->string the-temp-idx))))

; test
(require "test-cases.rkt")
(test interp iswim-lazy-cases)
