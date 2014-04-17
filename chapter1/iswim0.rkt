#lang racket

; Expression = Variable
;            | Number
;            | (lambda Variable Expression)
;            | (+ Expression Expression)
;            | (- Expression Expression)
;            | (Expression Expression)

(define (interp exp1) (value-of exp1))

(define (value-of exp1)
  (match exp1
    ; a variable
    [(? symbol? s) s]
    ; a number
    [(? number? n) n]
    ; a procedure
    [`(lambda ,a ,b) exp1]
    ; primitive operations
    [`(+ ,e1 ,e2) (+ (value-of e1) (value-of e2))]
    [`(- ,e1 ,e2) (- (value-of e1) (value-of e2))]
    ; an application
    [`(,e1 ,e2)
     (match (value-of e1)
       [`(lambda ,var ,body)
        (value-of (substitute body var (value-of e2)))])]))

(define (substitute exp1 var val)
  (define (>> e) (substitute e var val))
  (match exp1
    ; a variable
    [(? symbol? s) (if (eqv? s var) val s)]
    ; a number
    [(? number? n) n]
    ; a procedure
    [`(lambda ,a ,b)
     (if (eqv? a var)
         exp1
         (let ((tmp-var (new-tmp-var)))
           `(lambda ,tmp-var ,(>> (substitute b a tmp-var)))))]
    ; primitive operations
    [`(+ ,e1 ,e2) `(+ ,(>> e1) ,(>> e2))]
    [`(- ,e1 ,e2) `(- ,(>> e1) ,(>> e2))]
    ; an application
    [`(,e1 ,e2) `(,(>> e1) ,(>> e2))]))

; temporary variable
(define the-temp-idx 0)
(define (new-tmp-var)
  (set! the-temp-idx (+ the-temp-idx 1))
  (string->symbol (string-append "#" (number->string the-temp-idx))))

; test
(require "test-cases.rkt")
(test interp iswim0-cases)
