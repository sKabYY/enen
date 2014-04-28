#lang racket

; Expression = Variable
;            | Number
;            | Boolean
;            | (lambda (Variable*) Expression)
;            | (letrec ((Variable (Variable*) Expression)*) Expression)
;            | (if Expression Expression Expression)
;            | (begin Expression*)
;            | (set! Variable Expression)
;            | (letcc Variable Expression)
;            | (raise Expression)
;            | (catch Expression Variable Expression)
;            | (Expression Expression*)
;
; Value = Number
;       | Boolean
;       | Closure
;       | Opt
;       | <void>
;       | Mutpair
;       | Continuation
;
; Closure: (Variable*) * Expression * Environment
; Mutpair: Reference * Reference
;
; Opt:
;  void: () -> <void>
;  +: Number * Number * ... -> Number
;  -: Number * Number * ...-> Number
;  *: Number * Number * ...-> Number
;  remainder: Number * Number * ...-> Number
;  zero?: Number -> Boolean
;  print: Value -> <void>
;  pair: Value * Value -> Mutpair
;  mutpair?: Value -> Boolean
;  left: Mutpair -> Value
;  right: Mutpair -> Value
;  setleft!: Mutpair * Value -> <void>
;  setright!: Mutpair * Value -> <void>
;
; (if L M N)
; = ((ifv L (lambda () M) (lambda () N)))
;
; (let ((X1 N1)...(Xn Nn)) M)
; = ((lambda (X1...Xn) M) N1...Nn)
;
; (fix Xf (X1...Xn) M)
; = (letrec ((X1 (lambda (X1...Xn) M))) X1)
;
; (cc M N)
; = (M N)
;
; Environment: Variable -> Reference
; Store: Reference -> Value
;
;===cps=================================================
;
; SiExp = Variable
;       | Number
;       | Boolean
;       | (lambda (Variable*) TfExp)
;
; TfExp = SiExp
;       | (letrec ((Variable (Variable*) TfExp)*) TfExp)
;       | (if SiExp TfExp TfExp)
;       | (set! Variable SiExp Cont ECont)
;       | (SiExp SiExp* Cont ECont)
;
; Cont = ECont = Variable | (lambda (Variable) TfExp)
;
; Value = Number
;       | Boolean
;       | Closure
;       | Opt/k
;       | <void>
;       | Mutpair
;
; (opt/k args... cont econt) = (cont (opt args...))

(define the-max-store-size 96)
(define (interp exp1)
  (reset-space-cost!)
  (initialize-store! the-max-store-size)
  (let ((cps-exp (cps-translate (macro-translate exp1)
                                '(lambda (v) v)
                                '(lambda (e) e)
                                (mk-newvar))))
    (displayln "CPS:")
    (pretty-print cps-exp)
    (let ((ret (value-of cps-exp (init-env))))
      ;(displayln (store-info))
      (printf "#SpaceCost=~a~%" the-space-cost)
      ret)) )

(define the-space-cost 0)
(define (reset-space-cost!)
  (set! the-space-cost 0))
(define (update-space-cost!)
  (let ((cost (- the-store-offset
                 (length opt-table)
                 (length the-store-free-list))))
    (set! the-space-cost (max the-space-cost cost))))

(define dec-var car)
(define dec-args cadr)
(define dec-body caddr)

(define (macro-translate exp1)
  (define (>> exp1) (macro-translate exp1))
  (match exp1
    ; a variable
    [(? symbol? s) s]
    ; a number
    [(? number? n) n]
    ; a boolean
    [(? boolean? b) b]
    ; a procedure
    [`(lambda ,a ,b) `(lambda ,a ,(>> b))]
    ; a letrec expression
    [`(letrec ,decs ,body)
     `(letrec ,(map (lambda (dec)
                      (list (dec-var dec) (dec-args dec) (>> (dec-body dec))))
                    decs)
        ,(>> body))]
    ; a begin expression
    [`(begin . ,exps) `(begin . ,(map >> exps))]
    ; a set expression
    [`(set! ,v ,e) `(set! ,v ,(>> e))]
    ; a letcc expression
    [`(letcc ,v ,e) `(letcc ,v ,(>> e))]
    ; a raise expression
    [`(raise ,e) `(raise ,(>> e))]
    ; a catch expression
    [`(catch ,body ,var ,p-body) `(catch ,(>> body) ,var ,(>> p-body))]
    ; a if expression
    [`(if ,e1 ,e2 ,e3) `(if ,(>> e1) ,(>> e2) ,(>> e3))]
    ; a let expression
    [`(let ,decs ,body)
     (let ((vars (map car decs))
           (exps (map cadr decs)))
       `((lambda ,vars ,(>> body)) . ,(map >> exps)))]
    ; a recursive procedure
    [`(fix ,f ,as ,b)
     `(letrec ((,f ,as ,(>> b))) ,f)]
    ; a cc epxression
    [`(cc ,e1 ,e2) `(,(>> e1) ,(>> e2))]
    ; an application
    [`(,ef . ,exps) `(,(>> ef) . ,(map >> exps))]))

; tmp variable
(define (mk-newvar)
  (let ((i 0))
    (lambda ()
      (set! i (+ i 1))
      (string->symbol
       (string-append "#" (number->string i))))))

; a variable stands for continuation
(define vark (string->symbol "#k"))
(define varek (string->symbol "#ek"))

; a variable stands for useless arguement
(define var_ (string->symbol "#_"))

(define (pushback lst . vs) (append lst vs))

(define (list-last lst)
  ; assert (not (null? lst))
  (if (null? (cdr lst)) (car lst) (list-last (cdr lst))))

(define (simple-exp? exp1)
  (match exp1
    [(? symbol? s) #t]
    [(? number? n) #t]
    [(? boolean? b) #t]
    [`(lambda ,as ,b) #t]
    [else #f]))

(define (cps-translate exp1 cont econt newvar)
  (define (>> e) (cps-translate e cont econt newvar))
  (define (>>/k e k ek) (cps-translate e k ek newvar))
  (define (>>s sexp)
    (match sexp
      ; a variable
      [(? symbol? s) s]
      ; a number
      [(? number? n) n]
      ; a boolean
      [(? boolean? b) b]
      ; a procedure
      [`(lambda ,as ,b)
       `(lambda ,(pushback as vark varek) ,(>>/k b vark varek))]))
  (define (>>lst acc exps put get)
    (if (null? exps)
        (get acc)
        (let ((e (car exps)))
          (if (simple-exp? e)
              (>>lst (put acc e) (cdr exps) put get)
              (let ((w (newvar)))
                (>>/k e
                      `(lambda (,w)
                         ,(>>lst (put acc w) (cdr exps) put get))
                      econt))))))
  (match exp1
    ; a SiExp
    [(? simple-exp? sexp) `(,cont ,(>>s sexp))]
    ; a letrec expression
    [`(letrec ,decs ,body)
     `(letrec ,(map (lambda (dec)
                      (list (dec-var dec)
                            (pushback (dec-args dec) vark varek)
                            (>>/k (dec-body dec) vark varek)))
                    decs)
        ,(>> body))]
    ; an if expression
    [`(if ,e1 ,e2 ,e3)
     (let ((w (newvar)))
       (>>/k e1 `(lambda (,w) (if ,w ,(>> e2) ,(>> e3))) econt))]
    ; a begin expression (the begin expression disappears)
    [`(begin . ,exps)
     (>>lst '(void)
            exps
            (lambda (acc v) v)
            (lambda (acc) (>> acc)))]
    ; a set expression
    [`(set! ,v ,e)
     (if (simple-exp? e)
         `(set! ,v ,(>>s e) ,cont ,econt)
         (let ((w (newvar)))
           (>>/k e `(lambda (,w) (set! ,v ,w ,cont)) econt)))]
    ; a letcc expression (the letcc expression disappears)
    [`(letcc ,v ,e)
     (let ((w (newvar)))
       `((lambda (,v) ,(>> e)) (lambda (,w ,var_ ,var_) (,cont ,w))))]
    ; a raise expression
    [`(raise ,e)
     (let ((w (newvar)))
       (>>/k e `(lambda (,w) (,econt ,w)) econt))]
    ; a catch expression (the catch expression disappears)
    [`(catch ,body ,var ,p-body)
     (>>/k body cont `(lambda (,var) ,(>> p-body)))]
    ; an application
    [`(,ef . ,exps)
     (>>lst '()
            exp1
            (lambda (racc v) (cons v racc))
            (lambda (racc)
              (pushback (map >>s (reverse racc)) cont econt)))]))

; value-of
(define (value-of cps-exp env)
  (define (>> sexp)
    (match sexp
      ; a variable
      [(? symbol? s) (deref (apply-env env s))]
      ; a number
      [(? number? n) n]
      ; a boolean
      [(? boolean? b) b]
      ; a procedure
      [`(lambda ,as ,b) (closure as b env)]))
  ; gc
  (update-space-cost!)
  (gc env)
  ; eval
  (match cps-exp
    ; SiExp
    [(? simple-exp? sexp) (>> sexp)]
    ; letrec
    [`(letrec ,decs ,body)
     (let ((vars (map dec-var decs))
           (argss (map dec-args decs))
           (bodies (map dec-body decs)))
       (value-of body (extend-env-letrec env vars argss bodies)))]
    ; if
    [`(if ,se ,e2 ,e3)
     (if (true? (>> se)) (value-of e2 env) (value-of e3 env))]
    ; set!
    [`(set! ,var ,se ,cont, econt)
     (setref! (apply-env env var) (>> se))
     (apply-closure (>> cont) (list (void)))]
    ; application
    (`(,sef . ,ses)
     (apply-proc (>> sef) (map >> ses)))))

; apply-proc
(define (apply-proc proc vals)
  (cond
   [(closure? proc) (apply-closure proc vals)]
   [(operator? proc) (apply-opt proc vals)]
   [else (report-not-a-proc proc)]))

; closure
(struct closure (args body env))
(define (apply-closure clo vals)
  (value-of (closure-body clo)
            (extend-env-let (closure-env clo)
                            (closure-args clo)
                            vals)))

; environment
(struct empty-env () #:transparent)
(struct extend-env (env vars refs) #:transparent)

(define (extend-env-let env vars vals)
  (let ((nvars (length vars))
        (nvals (length vals))
        (refs (map newref vals)))
    (if (= nvars nvals)
        (extend-env env vars refs)
        (report-num-args-not-match nvars nvals))))

(define (extend-env-letrec env vars argss bodies)
  ; assert (= nvars nargss nbodies)
  (define (allocn n)
    (if (= n 0)
        '()
        (cons (newref 'uninitialized) (allocn (- n 1)))))
  (let* ((refs (allocn (length vars)))
         (new-env (extend-env env vars refs)))
    (for-each setref!
              refs
              (map (lambda (args body) (closure args body new-env))
                   argss bodies))
    new-env))

(define (apply-env-f env search-var f)
  (cond
   [(empty-env? env) (f)]
   [(extend-env? env)
    (define (iter vars refs)
      (if (null? vars)
          (apply-env-f (extend-env-env env) search-var f)
          (if (eqv? search-var (car vars))
              (car refs)
              (iter (cdr vars) (cdr refs)))))
    (iter (extend-env-vars env) (extend-env-refs env))]))

(define (apply-env env search-var)
  (apply-env-f env
               search-var
               (lambda () (report-unbound-var search-var))))

; store
(define the-store 'uninitialized)
(define the-store-offset 'uninitialized)
(define the-store-free-list 'uninitialized)
(struct reference (n) #:transparent)

(define (initialize-store! size)
  (set! the-store (make-vector size))
  (set! the-store-offset 0)
  (set! the-store-free-list '()))

(define (newref val)
  (reference
   (if (null? the-store-free-list)
       (if (< the-store-offset the-max-store-size)
           (begin
             (vector-set! the-store the-store-offset val)
             (let ((off the-store-offset))
               (set! the-store-offset (+ the-store-offset 1))
               off))
           (report-out-of-memory))
       (let ((off (car the-store-free-list)))
         (vector-set! the-store off val)
         (set! the-store-free-list (cdr the-store-free-list))
         off))))

(define (deref ref)
  (vector-ref the-store (reference-n ref)))

(define (setref! ref val)
  (vector-set! the-store (reference-n ref) val))

(define (store-info)
  (let* ((start (length opt-table))
         (size (- the-store-offset start))
         (vec (make-vector size)))
    (define (iter i)
      (if (< i size)
          (begin
            (vector-set! vec i (vector-ref the-store (+ start i)))
            (iter (+ i 1)))
          (void)))
    (iter 0)
    (cons start vec)))

; opt
(struct operator (name opt) #:transparent)
(struct mutpair (left-ref right-ref) #:transparent)

(define (-pair vl vr) (mutpair (newref vl) (newref vr)))
(define (-left pair) (deref (mutpair-left-ref pair)))
(define (-right pair) (deref (mutpair-right-ref pair)))

(define (-setleft! pair val)
  (setref! (mutpair-left-ref pair) val)
  (void))
(define (-setright! pair val)
  (setref! (mutpair-right-ref pair) val)
  (void))

(define (-print val) (displayln val))

(define (true? x) (eqv? x #t))
(define (false? x) (eqv? x #f))

(define opt-table
  (map (lambda (p)
         (cons (car p) (operator (car p) (cdr p))))
       (list
        (cons 'void void)
        (cons '+ +)
        (cons '* *)
        (cons '- -)
        (cons 'remainder remainder)
        (cons 'zero? zero?)
        (cons 'pair -pair)
        (cons 'mutpair? mutpair?)
        (cons 'left -left)
        (cons 'right -right)
        (cons 'setleft! -setleft!)
        (cons 'setright! -setright!)
        (cons 'print -print))))

(define (init-env)
  (extend-env-let (empty-env) (map car opt-table) (map cdr opt-table)))

(define (apply-opt opt vals)
  (define (partition-last-f racc lst f)
    ; assert (>= (length lst) 2)
    (if (null? (cddr lst))
        (f (car lst) (cadr lst) (reverse racc))
        (partition-last-f (cons (car lst) racc) (cdr lst) f)))
  (partition-last-f '()
                    vals
                    (lambda (cont econt vals)
                      (apply-closure cont
                                     (list (apply (operator-opt opt) vals))))))

; gc
(define (gc env)
  (define flags (make-vector the-store-offset 'white))
  (define grays '())
  (define (mark-ref ref)
    (let ((i (reference-n ref)))
      (if (eqv? (vector-ref flags i) 'white)
          (begin
            (vector-set! flags i 'gray)
            (set! grays (cons i grays)))
          (void))))
  (define (mark-env env)
    (if (empty-env? env)
        (void)
        (let ((refs (extend-env-refs env))
              (closing-env (extend-env-env env)))
          (for-each mark-ref refs)
          (mark-env closing-env))))
  (define (set-free-list!)
    (define (whites acc i)
      (if (< i 0)
          acc
          (if (eqv? (vector-ref flags i) 'white)
              (whites (cons i acc) (- i 1))
              (whites acc (- i 1)))))
    (set! the-store-free-list (whites '() (- the-store-offset 1))))
  (define (mark-val v)
    (cond
     [(closure? v)
      (mark-env (closure-env v))]
     [(mutpair? v)
      (mark-ref (mutpair-left-ref v))
      (mark-ref (mutpair-right-ref v))]))
  (define (iter)
    (if (null? grays)
        (void)
        (begin
          (let* ((i (car grays))
                 (v (vector-ref the-store i)))
            (set! grays (cdr grays))
            (vector-set! flags i 'black)
            (mark-val v))
          (iter))))
  (begin
    (mark-env env)
    (iter)
    (set-free-list!)))

; report error
(define (report-unbound-var var)
  (error "[Error] unbound var:" var))

(define (report-not-a-proc proc)
  (error "[Error] not a procedure:" proc))

(define (report-num-args-not-match nvars nvals)
  (error "[Error] num args not match (nvars nvals):" nvars nvals))

(define (report-out-of-memory)
  (error "[Error] running out of memory:" (store-info)))

; test
(require "test-cases.rkt")
(test interp alligator-cases)
