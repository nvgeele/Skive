#lang racket

(require "../src/expand.rkt")
(require "../src/analyse.rkt")
(require "../src/compile.rkt")
(require "../src/skive.rkt")

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define-skive (test)
  (let ((almost-fib (lambda (f n)
                      (if (= n 0)
                          1
                          (if (= n 1)
                              1
                              (+ (f f (- n 1)) (f f (- n 2))))))))
    (let ((fib (lambda (n)
                 (almost-fib almost-fib n))))
      (cons 25 (cons "fib" (fib 25))))))
