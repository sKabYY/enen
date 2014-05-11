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

(define cek0-dynamic-cases
  '(

12
(+ 12 13)
(- 32 23)
(lambda x (+ x 1))
((lambda x (- x 1)) 22)
(((lambda x x) (lambda y y)) 11)

(iszero 0)
(iszero 1)

(let a 1
  (let p (lambda x
           (+ a x))
    (let a 2
      (p 10))))

(let double (lambda dummy not-reach-here)
  (let double (lambda n
                (if (iszero n)
                    0
                    (+ 2 (double (- n 1)))))
    (double 4)))

(((iszero 0) 1) 2)
(((iszero 1) 1) 2)
(if (iszero 0) 1 2)
(if (iszero 1) 1 2)

))

(define cek-cases
  '(

12
(+ 12 13)
(- 32 23)
(lambda x (+ x 1))
((lambda x (- x 1)) 22)
(((lambda x x) (lambda y y)) 11)

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

(((lambda f
    ((lambda x (f (x x)))
     (lambda x (f (lambda v ((x x) v))))))
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

(define cek-multiargs-cases
  '(

12
(+ 12 13)
(- 32 23)
(lambda (x) (+ x 1))
((lambda (x) (- x 1)) 22)
(((lambda (x) x) (lambda (y) y)) 11)

(iszero 0)
(iszero 1)
(if (iszero 0) 1 2)
(if (iszero 1) 1 2)

((lambda (f) ((f f) 4))
 (lambda (p)
   (lambda (n)
     (if (iszero n)
         0
         (+ 2 ((p p) (- n 1)))))))

(((lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (lambda (v) ((x x) v))))))
  (lambda (f)
    (lambda (n)
      (if (iszero n)
          0
          (+ 2 (f (- n 1)))))))
 4)

(let ((a 1))
  (+ a 2))

(let ((f (lambda (p)
           (lambda (n)
             (if (iszero n)
                 0
                 (+ 2 ((p p) (- n 1))))))))
  ((f f) 4))

((fix f (n)
      (if (iszero n)
          0
          (+ 2 (f (- n 1)))))
 4)

(letrec ((f (n) (if (iszero n)
                     0
                     (+ 2 (f (- n 1))))))
        (f 4))

(letrec ((gcd (a b) (if (iszero a)
                        b
                        (gcd (remainder b a) a))))
  (gcd 144 12144))

(((lambda (x) x) +) 12 23)

))
