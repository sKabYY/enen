#lang racket

; Expression = Variable
;            | Number
;            | (lambda Variable Expression)
;            | (fix Variable Variable Expression)
;            | (+ Expression Expression)
;            | (- Expression Expression)
;            | (iszero Expression)
;            | (Expression Expression)

(define (interp exp1) (value-of/k (translate exp1) (end-cont)))

(define (translate exp1)
  (match exp1
    ; an if statement
    [`(if ,e1 ,e2 ,e3)
     `(((,(translate e1)
         (lambda 0 ,(translate e2)))
        (lambda 0 ,(translate e3)))
       0)]
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

(define (value-of/k exp1 cont)
  (match exp1
    ; a variable
    [(? symbol? s) (apply-cont cont s)]
    ; a number
    [(? number? n) (apply-cont cont n)]
    ; a procedure
    [`(lambda ,a ,b) (apply-cont cont exp1)]
    ; a recursive procedure
    [`(fix ,f ,a ,b)
     (apply-cont cont `(lambda ,a ,(substitute b f exp1)))]
    ; primitive operations
    [`(+ ,e1 ,e2)
     (value-of/k e1 (opt-cont cont + (list e2) '()))]
    [`(- ,e1 ,e2)
     (value-of/k e1 (opt-cont cont - (list e2) '()))]
    [`(iszero ,e1)
     (value-of/k e1 (opt-cont cont iszero '() '()))]
    ; an application
    [`(,e1 ,e2)
     (value-of/k e1 (arg-cont cont e2))]))

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

; continuation
(struct end-cont ())

(struct continuation (cont data))
(struct data-arg-cont (rand-exp))
(struct data-fun-cont (rator))
(struct data-opt-cont (opt exps vals))

(define (arg-cont cont rand-exp)
  (continuation cont (data-arg-cont rand-exp)))
(define (apply-arg-cont cont rand-exp v)
  (value-of/k rand-exp (fun-cont cont v)))

(define (fun-cont cont rator)
  (continuation cont (data-fun-cont rator)))
(define (apply-fun-cont cont rator v)
  (match rator
    [`(lambda ,var ,body)
     (value-of/k (substitute body var v) cont)]))

(define (opt-cont cont opt exps vals)
  (continuation cont (data-opt-cont opt exps vals)))
(define (apply-opt-cont cont opt exps vals v)
  (if (null? exps)
      (value-of/k (apply opt (append vals (list v))) cont)
      (value-of/k (car exps)
                  (opt-cont cont opt (cdr exps) (append vals (list v))))))

(define (apply-cont cont v)
    (if (end-cont? cont)
        (begin
          (displayln ">> Done!")
          v)
        (let ((up-cont (continuation-cont cont))
              (data (continuation-data cont)))
          (cond
           [(data-arg-cont? data)
            (apply-arg-cont up-cont (data-arg-cont-rand-exp data) v)]
           [(data-fun-cont? data)
            (apply-fun-cont up-cont (data-fun-cont-rator data) v)]
           [(data-opt-cont? data)
            (apply-opt-cont up-cont
                            (data-opt-cont-opt data)
                            (data-opt-cont-exps data)
                            (data-opt-cont-vals data)
                            v)]))))
; test
(require "test-cases.rkt")
(test interp iswim-cases)
