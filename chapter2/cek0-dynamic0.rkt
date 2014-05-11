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
  (closure-exp
   (value-of/k (closure (translate exp1) (empty-env))
               (end-cont))))

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

(define (value-of/k clo cont)
  (let ((exp1 (closure-exp clo))
        (env (closure-env clo)))
    (match exp1
      ; a variable
      [(? symbol? s)
       (apply-cont cont (apply-env env s))]
      ; a number
      [(? number? n)
       (apply-cont cont (closure n env))]
      ; a procedure
      [`(lambda ,a ,b)
       (apply-cont cont (closure exp1 env))]
      ; a recursive procedure
      [`(fix ,f ,a ,b)
       (apply-cont cont
                   (closure `(lambda ,a ,b) (extendrec-env env f a b)))]
      ; primitive operations
      [`(+ ,e1 ,e2)
       (value-of/k (closure e1 env) (opt-cont cont + (list (closure e2 env)) '()))]
      [`(- ,e1 ,e2)
       (value-of/k (closure e1 env) (opt-cont cont - (list (closure e2 env)) '()))]
      [`(iszero ,e1)
       (value-of/k (closure e1 env) (opt-cont cont iszero '() '()))]
      ; an application
      [`(,e1 ,e2)
       (value-of/k (closure e1 env) (arg-cont cont (closure e2 env)))])))

(define (iszero n)
  (if (= n 0)
      the-true
      the-false))

; closure
(struct closure (exp env) #:transparent)

(define (apply-closure/k clo arg-clo cont)
  (match (closure-exp clo)
    [`(lambda ,arg ,body)
     (value-of/k (closure body
                          (extend-env (closure-env arg-clo) arg arg-clo))
                 cont)]))

; environment
(struct empty-env () #:transparent)
(struct extend-env (env var clo) #:transparent)
(struct extendrec-env (env var arg body) #:transparent)

(define (apply-env env search-var)
  (cond
   [(empty-env? env)
    (report-unbound-var search-var)]
   [(extend-env? env)
    (if (eqv? search-var (extend-env-var env))
        (extend-env-clo env)
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
  (lambda (c)
    (displayln ">> Done!")
    c))

(define (arg-cont cont rand-clo)
  (lambda (c)
    (value-of/k rand-clo (fun-cont cont c))))

(define (fun-cont cont rator-clo)
  (lambda (c)
    (apply-closure/k rator-clo c cont)))

(define (opt-cont cont opt clos vals)
  (lambda (c)
    (let ((new-vals (append vals (list (closure-exp c)))))
      (if (null? clos)
          (apply-cont cont (closure (apply opt new-vals) (closure-env c)))
          (value-of/k (car clos)
                      (opt-cont cont opt (cdr clos) new-vals))))))

(define (apply-cont cont clo) (cont clo))

(define (report-unbound-var var)
  (error "unbound var" var))

; test
(require "test-cases.rkt")
(test interp cek0-dynamic-cases)
