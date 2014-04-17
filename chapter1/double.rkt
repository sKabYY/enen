#lang racket

(define (double n)
  (if (= n 0)
      0
      (+ 2 (double (- n 1)))))

(define (value-of/k n cont)
  (if (= n 0)
      (apply-cont cont 0)
      (value-of/k (- n 1) (cons '+2 cont))))

(define (apply-cont cont n)
  (if (null? cont)
      n
      (let ((m (+ n 2)))
        (apply-cont (cdr cont) m))))

(define (double1 n) (value-of/k n '()))

(double 4)
(double1 4)
