#lang racket

(require "../../src/expand.rkt")
(require "../../src/analyse.rkt")
(require "../../src/compile.rkt")
(require "../../src/skive.rkt")

(define (do-test prog)
  (let* ((output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (display (compile-skive-to-if1 prog) output)
    (close-output-port output)
    (display "done!\n")))

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 2)) (fib (- n 1))))))

(define fib*
  (letrec ((fib-iter (lambda (n a b)
                       (if (= n 0)
                           a
                           (fib-iter (- n 1) b (+ a b)))))
           (fib* (lambda (n)
                   (fib-iter n 0 1))))
    fib*))

(define prog
  `(letrec ((fib (lambda (n)
                   (if (= n 0)
                       1
                       (if (= n 1)
                           1
                           (+ (fib (- n 2)) (fib (- n 1)))))))
            (fib-iter (lambda (n a b)
                        (if (= n 0)
                            a
                            (fib-iter (- n 1) b (+ a b)))))
            (fib* (lambda (n)
                    (fib-iter n 0 1)))
            (vec (vector ,@(range 1 301))))
     (map-vector fib* vec)))

(define test (lambda-skive prog))
