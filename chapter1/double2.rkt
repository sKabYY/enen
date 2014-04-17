#lang racket

(define (double n)
  (if (= n 0)
      0
      (+ 2 (double (- n 1)))))

(define (value-of/k n cont)
  (if (= n 0)
      (apply-cont cont 0)
      (value-of/k (- n 1)
                  (lambda (v)
                    (apply-cont cont (+ 2 v))))))

(define (apply-cont cont n) (cont n))

(define (double1 n)
  (value-of/k n (lambda (v) v)))

(double 4)
(double1 4)
