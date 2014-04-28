#lang racket

(require "cek.rkt")

(interp
 '(letrec ((fab (n) (if (iszero n)
                        0
                        (if (iszero (- n 1))
                            1
                            (+ (fab (- n 1)) (fab (- n 2)))))))
    (fab 28))
 )
