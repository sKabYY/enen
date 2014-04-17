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
  (value-of/k (translate exp1) (empty-env) (end-cont)))

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
    [(? symbol? s) (apply-cont cont (apply-env env s))]
    ; a number
    [(? number? n) (apply-cont cont n)]
    ; a procedure
    [`(lambda ,a ,b) (apply-cont cont (closure a b env))]
    ; a recursive procedure todo
    [`(fix ,f ,a ,b)
     (apply-cont cont (closure a b (extendrec-env env f a b)))]
    ; primitive operations
    [`(+ ,e1 ,e2)
     (value-of/k e1 env (opt-cont cont env + (list e2) '()))]
    [`(- ,e1 ,e2)
     (value-of/k e1 env (opt-cont cont env - (list e2) '()))]
    [`(iszero ,e1)
     (value-of/k e1 env (opt-cont cont env iszero '() '()))]
    ; an application
    [`(,e1 ,e2)
     (value-of/k e1 env (arg-cont cont env e2))]))

(define (iszero n)
  (if (= n 0)
      the-true
      the-false))

; closure
(struct closure (arg body env) #:transparent)
(define (apply-closure/k clo val cont)
 (let ((arg (closure-arg clo))
        (body (closure-body clo))
        (env (closure-env clo)))
    (value-of/k body (extend-env env arg val) cont)))

; environment
(struct empty-env ())
(struct extend-env (env var val))
(struct extendrec-env (env var arg body))

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
        (closure (extendrec-env-arg env)
                 (extendrec-env-body env)
                 env)
        (apply-env (extendrec-env-env env) search-var))]))

(define the-true (closure 'x '(lambda y x) (empty-env)))
(define the-false (closure 'x '(lambda y y) (empty-env)))

; continuation
(struct end-cont ())

(struct continuation (cont env data))
(struct data-arg-cont (rand-exp))
(struct data-fun-cont (rator))
(struct data-opt-cont (opt exps vals))

(define (arg-cont cont env rand-exp)
  (continuation cont env (data-arg-cont rand-exp)))
(define (apply-arg-cont cont env rand-exp v)
  (value-of/k rand-exp env (fun-cont cont env v)))

(define (fun-cont cont env rator)
  (continuation cont env (data-fun-cont rator)))
(define (apply-fun-cont cont env rator v)
  (apply-closure/k rator v cont))

(define (opt-cont cont env opt exps vals)
  (continuation cont env (data-opt-cont opt exps vals)))
(define (apply-opt-cont cont env opt exps vals v)
  (if (null? exps)
      (apply-cont cont (apply opt (append vals (list v))))
      (value-of/k (car exps)
                  env
                  (opt-cont cont env opt (cdr exps) (append vals (list v))))))

(define (apply-cont cont v)
    (if (end-cont? cont)
        (begin
          (displayln ">> Done!")
          v)
        (let ((up-cont (continuation-cont cont))
              (env (continuation-env cont))
              (data (continuation-data cont)))
          (cond
           [(data-arg-cont? data)
            (apply-arg-cont up-cont env (data-arg-cont-rand-exp data) v)]
           [(data-fun-cont? data)
            (apply-fun-cont up-cont env (data-fun-cont-rator data) v)]
           [(data-opt-cont? data)
            (apply-opt-cont up-cont
                            env
                            (data-opt-cont-opt data)
                            (data-opt-cont-exps data)
                            (data-opt-cont-vals data)
                            v)]))))

(define (report-unbound-var var)
  (error "unbound var" var))

; test
(require "test-cases.rkt")
(test interp cek0-cases)
