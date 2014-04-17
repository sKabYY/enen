#lang racket

(provide (all-defined-out))

(define (test interp cases)
  (if (null? cases)
      (void)
      (let ((exp1 (car cases))
            (rest (cdr cases)))
        (pretty-print exp1)
        (let ((ret (interp exp1)))
          (display ">> ")
          (pretty-print ret)
          (newline))
        (test interp rest))))

(define iswim0-cases
  '(

a
12
(+ 12 13)
(- 32 23)
(lambda x (+ x 1))
((lambda x (- x 1)) 22)
(((lambda x x) (lambda y y)) 11)
(((lambda x (lambda y x)) y) 0)

))

(define iswim-cases1
  '(

(iszero 0)
(iszero 1)
(((iszero 0) 1) 2)
(((iszero 1) 1) 2)
(if (iszero 0) 1 2)
(if (iszero 1) 1 2)

((lambda f ((f f) 4))
 (lambda p
   (lambda n
     (if (iszero n)
         0
         (+ 2 ((p p) (- n 1)))))))

(((lambda x
    ((lambda v (v v))
     (lambda f (x (lambda u ((f f) u))))))
  (lambda f
    (lambda n
      (if (iszero n)
          0
          (+ 2 (f (- n 1)))))))
 4)

(let a 1
  (+ a 2))

(let f (lambda p
         (lambda n
           (if (iszero n)
               0
               (+ 2 ((p p) (- n 1))))))
  ((f f) 4))

(let double ((lambda v (v v))
             (lambda f
               (lambda n
                 (if (iszero n)
                     0
                     (+ 2 ((f f) (- n 1)))))))
  (double 4))

(let Yv (lambda x
          ((lambda v (v v))
           (lambda f (x (lambda u ((f f) u))))))
  (let mkdouble (lambda f
                  (lambda n
                    (if (iszero n)
                        0
                        (+ 2 (f (- n 1))))))
    ((Yv mkdouble) 4)))

((fix f n
      (if (iszero n)
          0
          (+ 2 (f (- n 1)))))
 4)

(letrec f n (if (iszero n)
                0
                (+ 2 (f (- n 1))))
        (f 4))

))
(define iswim-cases (append iswim0-cases iswim-cases1))

(define iswim-lazy-cases1
  '(

((lambda x (+ x x)) (+ 12 21))

(((lambda y (lambda x x))
  ((lambda x (x x)) (lambda x (x x))))
 (+ 12 21))

(let Y
    (lambda x
      ((lambda v (v v))
       (lambda f (x (f f)))))
  (let mkdouble
      (lambda f
        (lambda n
          (if (iszero n)
              0
              (+ 2 (f (- n 1))))))
    ((Y mkdouble) 4)))

))
(define iswim-lazy-cases (append iswim-cases iswim-lazy-cases1))
