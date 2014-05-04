#lang racket

; Expression = Variable
;            | Number
;            | (lambda Variable Expression)
;            | (fix Variable Variable Expression)
;            | (+ Expression Expression)
;            | (- Expression Expression)
;            | (iszero Expression)
;            | (Expression Expression)

(define (interp exp1)
  (set-exp! (translate exp1))
  (set-cont! (end-cont))
  (set-pc! value-of/k)
  (mainloop))

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

(define the-exp 'uninitialized)
(define the-cont 'uninitialized)
(define the-pc 'uninitialized)
(define (set-exp! exp1) (set! the-exp exp1))
(define (set-cont! cont) (set! the-cont cont))
(define (set-pc! pc) (set! the-pc pc))

(define (mainloop)
  (if the-pc
      (begin
        (the-pc)
        (mainloop))
      the-exp))

(define (value-of/k)
  (match the-exp
    ; a variable
    [(? symbol? s) (set-pc! apply-cont)]
    ; a number
    [(? number? n) (set-pc! apply-cont)]
    ; a procedure
    [`(lambda ,a ,b) (set-pc! apply-cont)]
    ; a recursive procedure
    [`(fix ,f ,a ,b)
     (set-exp! `(lambda ,a ,(substitute b f the-exp)))
     (set-pc! apply-cont)]
    ; primitive operations
    [`(+ ,e1 ,e2)
     (set-cont! (opd-cont the-cont + (list e2) '()))
     (set-exp! e1)]
    [`(- ,e1 ,e2)
     (set-cont! (opd-cont the-cont - (list e2) '()))
     (set-exp! e1)]
    [`(iszero ,e1)
     (set-cont! (opd-cont the-cont iszero '() '()))
     (set-exp! e1)]
    ; an application
    [`(,e1 ,e2)
     (set-cont! (arg-cont the-cont e2))
     (set-exp! e1)]))

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
(define (apply-cont) (the-cont))

(define (end-cont)
  (lambda ()
      (set-exp! the-exp)
      (displayln ">> Done!")
      (set-pc! #f)))

(define (arg-cont cont rand-exp)
  (lambda ()
    (set-cont! (fun-cont cont the-exp))
    (set-exp! rand-exp)
    (set-pc! value-of/k)))

(define (fun-cont cont rator)
  (lambda ()
    (match rator
      [`(lambda ,var ,body)
       (set-cont! cont)
       (set-exp! (substitute body var the-exp))
       (set-pc! value-of/k)])))

(define (opd-cont cont opd exps vals)
  (lambda ()
    (if (null? exps)
        (begin
          (set-cont! cont)
          (set-exp! (apply opd (append vals (list the-exp))))
          (set-pc! value-of/k))
        (begin
          (set-cont! (opd-cont cont opd (cdr exps) (append vals (list the-exp))))
          (set-exp! (car exps))
          (set-pc! value-of/k)))))

; test
(require "test-cases.rkt")
(test interp iswim-cases)
