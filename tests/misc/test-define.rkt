#lang racket

(require "../../src/expand.rkt")
(require "../../src/analyse.rkt")
(require "../../src/compile.rkt")
(require "../../src/skive.rkt")

(define prog1
  '((let ((o 5))
      (define a 1)
      (define (fac n)
        (if (= n 0)
            1
            (* n (fac (- n 1)))))
      (fac o))))

(define-skive (test)
  (define x 1)
  (define (fac n)
    (if (= n 0)
        1
        (* n (fac (- n 1)))))
  (define (even? n)
    (if (= n 0)
        #t
        (odd? (- n x))))
  (define (odd? n)
    (if (= n 0)
        #f
        (even? (- n x))))
  (let ((guard (even? 20)))
    (if guard (fac 10) (fac 5))))

(define a-program
  '((define x 1)
    (define (fac n)
      (if (= n 0)
          1
          (* n (fac (- n 1)))))
    (define (even? n)
      (if (= n 0)
          #t
          (odd? (- n x))))
    (define (odd? n)
      (if (= n 0)
          #f
          (even? (- n x))))
    (let ((guard (even? 20)))
      (if guard (fac 10) (fac 5)))))

(define (do-test prog)
  (let* ((output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (display (compile-sequence-to-if1 prog) output)
    (close-output-port output)
    (display "done!\n")))
