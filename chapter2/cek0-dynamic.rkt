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
  (value-of/k (translate exp1)
              (empty-env)
              (end-cont)))

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

(define (value-of/k exp1 env cont)
  (match exp1
    ; a variable
    [(? symbol? s)
     (apply-cont cont env (apply-env env s))]
    ; a number
    [(? number? n)
     (apply-cont cont env n)]
    ; a procedure
    [`(lambda ,a ,b)
     (apply-cont cont env exp1)]
    ; a recursive procedure
    [`(fix ,f ,a ,b)
     (apply-cont cont
                 (extendrec-env env f a b)
                 `(lambda ,a ,b))]
    ; primitive operations
    [`(+ ,e1 ,e2)
     (value-of/k e1 env (opt-cont cont + (list e2) '()))]
    [`(- ,e1 ,e2)
     (value-of/k e1 env (opt-cont cont - (list e2) '()))]
    [`(iszero ,e1)
     (value-of/k e1 env (opt-cont cont iszero '() '()))]
    ; an application
    [`(,e1 ,e2)
     (value-of/k e1 env (arg-cont cont e2))]))

(define (iszero n)
  (if (= n 0)
      the-true
      the-false))

; closure
(struct closure (exp env) #:transparent)

(define (apply-proc/k proc env val cont)
  (match proc
    [`(lambda ,arg ,body)
     (value-of/k body (extend-env env arg val) cont)]))

; environment
(struct empty-env () #:transparent)
(struct extend-env (env var val) #:transparent)
(struct extendrec-env (env var arg body) #:transparent)

(define (apply-env env search-var)
  (cond
   [(empty-env? env)
    (report-unbound-var search-var)]
   [(extend-env? env)
    (if (eqv? search-var (extend-env-var env))
        (extend-env-val env)
        (apply-env (extend-env-env env) search-var))]
   [(extendrec-env? env)
    (if (eqv? search-var (extendrec-env-var env))
        (closure `(lambda ,(extendrec-env-arg env)
                    ,(extendrec-env-body env))
                 env)
        (apply-env (extendrec-env-env env) search-var))]))

(define the-true '(lambda x (lambda y x)))
(define the-false '(lambda x (lambda y y)))

; continuation
(define (end-cont)
  (lambda (env v)
    (displayln ">> Done!")
    v))

(define (arg-cont cont rand-exp)
  (lambda (env v)
    (value-of/k rand-exp env (fun-cont cont v))))

(define (fun-cont cont rator)
  (lambda (env v)
    (apply-proc/k rator env v cont)))

(define (opt-cont cont opt exps vals)
  (lambda (env v)
    (let ((new-vals (append vals (list v))))
      (if (null? exps)
          (apply-cont cont env (apply opt new-vals))
          (value-of/k (car exps)
                      env
                      (opt-cont cont opt (cdr exps) new-vals))))))

(define (apply-cont cont env val) (cont env val))

(define (report-unbound-var var)
  (error "unbound var" var))

; test
(require "test-cases.rkt")
(test interp cek0-dynamic-cases)
