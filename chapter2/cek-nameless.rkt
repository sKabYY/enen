#lang racket

; Expression = Variable
;            | Number
;            | Boolean
;            | (lambda (Variable*) Expression)
;            | (if Expression Expression Expression)
;            | (fix Variable (Variable*) Expression)
;            | (Expression Expression*)
;
; Value = Number
;       | Boolean
;       | <closure (Variable*) Expression Environment>
;       | opt
;
; opt = + | - | remainder | iszero
;
; (let ((X1 N1)...(Xn Nn)) M)
; = ((lambda (X1...Xn) M) N1...Nn)
;
; (letrec ((X1 (X11...X1i) N1)...(Xn (Xn1...Xnj) Nn)) M)
; = ((lambda (X1...Xn) M) (fix X1 (X11...X1i) N1)...(fix Xn (Xn1...Xnj) Nn))
;

(provide interp)
(define (interp exp1)
  (let ((src (nameless-translate (macro-translate exp1) (empty-env))))
    (pretty-print src)
    (value-of/k src
                (empty-env)
                (end-cont))))

(define (macro-translate exp1)
  (define (>> exp1) (macro-translate exp1))
  (define (let-decs->vars decs) (map car decs))
  (define (let-decs->exps decs) (map cadr decs))
  (define (letrec-decs->vars decs) (map car decs))
  (define (letrec-decs->fixs decs)
    (map (lambda (dec)
           (let ((f (car dec))
                 (as (cadr dec))
                 (b (caddr dec)))
             `(fix ,f ,as ,(>> b))))
         decs))
  (match exp1
    ; a if expression
    [`(if ,e1 ,e2 ,e3)
     `(if ,(>> e1) ,(>> e2) ,(>> e3))]
    ; a let expression
    [`(let ,decs ,body)
     (let ((vars (let-decs->vars decs))
           (exps (let-decs->exps decs)))
       `((lambda ,vars ,(>> body)) . ,(map >> exps)))]
    ; a letrec expression
    [`(letrec ,decs ,body)
     (let ((vars (letrec-decs->vars decs))
           (fixs (letrec-decs->fixs decs)))
       `((lambda ,vars ,(>> body)) . ,fixs))]
    ; a procedure
    [`(lambda ,a ,b) `(lambda ,a ,(>> b))]
    ; a recursive procedure
    [`(fix ,f ,a ,b) `(fix ,f ,a ,(>> b))]
    ; an application
    [`(,ef . ,exps) `(,(>> ef) . ,(map >> exps))]
    [else exp1]))

(define (nameless-translate exp1 senv)
  (define (>> e) (nameless-translate e senv))
  (match exp1
    ; a variable
    [(? symbol? s) (get-envref-or-opt senv s)]
    ; a number
    [(? number? n) n]
    ; a procedure
    [`(lambda ,args ,body)
     `(lambda ,(length args)
        ,(nameless-translate body (extend-env-all senv args)))]
    ; a if expression
    [`(if ,e1 ,e2 ,e3) `(if ,(>> e1) ,(>> e2) ,(>> e3))]
    ; a recursive procedure
    [`(fix ,f ,args ,body)
     `(fix ,(length args)
           ,(nameless-translate body
                                (extend-env-all (extend-env senv f) args)))]
    ; an application
    [`(,ef . ,exps) `(,(>> ef) . ,(map >> exps))]))

(define (value-of/k exp1 env cont)
  (match exp1
    ; a variable
    [(? envref? r) (apply-cont cont (apply-env env r))]
    ; a number
    [(? number? n) (apply-cont cont n)]
    ; an opterator
    [(? operator? opt) (apply-cont cont opt)]
    ; a procedure
    [`(lambda ,argc ,body) (apply-cont cont (closure argc body env))]
    ; an if expression
    [`(if ,e1 ,e2 ,e3)
     (value-of/k e1 env (if-cont cont env e2 e3))]
    ; a recursive procedure
    [`(fix ,argc ,body)
     (apply-cont cont (closure argc body (extendrec-env env argc body)))]
    ; an application
    [`(,ef . ,exps)
     (value-of/k ef env (args-cont cont env exps))]))

; environment
(struct empty-env () #:transparent)
(struct extend-env (env val) #:transparent)
(struct extendrec-env (env argc body) #:transparent)

(struct envref (val) #:transparent)

(define (extend-env-all env vals)
  (if (null? vals)
      env
      (extend-env-all (extend-env env (car vals)) (cdr vals))))

(define (get-envref-f env search-var f)
  (define (iter acc env)
    (cond
     [(empty-env? env) (f)]
     [(extend-env? env)
      (if (eqv? search-var (extend-env-val env))
          (envref acc)
          (iter (+ acc 1) (extend-env-env env)))]))
  (iter 0 env))

(define (get-envref-or-opt env search-var)
  (get-envref-f env
                search-var
                (lambda ()
                  (let ((opt (find-opt search-var)))
                    (if opt
                        opt
                        (report-unbound-var search-var))))))

(define (apply-env-f env envref f)
  (define (iter env idx)
    (cond
     [(empty-env? env) (f)]
     [(extend-env? env)
      (if (= idx 0)
          (extend-env-val env)
          (iter (extend-env-env env) (- idx 1)))]
     [(extendrec-env? env)
      (if (= idx 0)
          (closure (extendrec-env-argc env)
                   (extendrec-env-body env)
                   env)
          (iter (extendrec-env-env env) (- idx 1)))]))
  (iter env (envref-val envref)))

(define (apply-env env search-var)
  (apply-env-f env
               search-var
               (lambda () (report-unbound-var search-var))))

; continuation
(define (end-cont)
  (lambda (v)
    (displayln ">> Done!")
    v))

(define (if-cont cont env then-exp else-exp)
  (lambda (v)
    (cond
     [(true? v) (value-of/k then-exp env cont)]
     [(false? v) (value-of/k else-exp env cont)]
     [else (report-condition-not-boolean v)])))

(define (args-cont cont env exps)
  (lambda (v)
    (if (null? exps)
        (apply-proc/k v '() cont)
        (value-of/k (car exps)
                    env
                    (fun-cont cont env v '() (cdr exps))))))

(define (fun-cont cont env rator vals exps)
  (lambda (v)
    (let ((new-vals (append vals (list v))))
      (if (null? exps)
          (apply-proc/k rator new-vals cont)
          (value-of/k (car exps)
                      env
                      (fun-cont cont env rator new-vals (cdr exps)))))))

(define (apply-cont cont v) (cont v))

; closure
(struct closure (args body env) #:transparent)

(define (apply-closure/k clo vals cont)
 (let ((body (closure-body clo))
       (env (closure-env clo)))
   (value-of/k body (extend-env-all env vals) cont)))

; opt
(struct operator (name opt) #:transparent)

(define (find-opt s)
  (let ((p (assoc s opt-table)))
    (if p (cdr p) p)))

(define (iszero n) (= n 0))
(define (true? x) (eqv? x #t))
(define (false? x) (eqv? x #f))

(define opt-table
  (map (lambda (p)
         (cons (car p) (operator (car p) (cdr p))))
       (list
        (cons '+ +)
        (cons '- -)
        (cons 'remainder remainder)
        (cons 'iszero iszero))))

(define (apply-opt/k opt vals cont)
  (apply-cont cont (apply (operator-opt opt) vals)))

; apply-proc
(define (apply-proc/k proc vals cont)
  (if (closure? proc)
      (apply-closure/k proc vals cont)
      (apply-opt/k proc vals cont)))

; report error
(define (report-unbound-var var)
  (error "unbound var" var))

(define (report-condition-not-boolean var)
  (error "not a boolean" var))

(define (report-num-args-not-match nvars nvals)
  (error "num args not match (nvars nvals)" nvars nvals))

; test
(require "test-cases.rkt")
(test interp cek-multiargs-cases)
