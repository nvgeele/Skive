#lang racket

(require "../src/expand.rkt")
(require "../src/analyse.rkt")
(require "../src/compile.rkt")
(require "../src/skive.rkt")

(define prog1
  '(let ((o 5))
     (define a 1)
     (define (fac n)
       (if (= n 0)
           1
           (* n (fac (- n 1)))))
     (fac o)))

(define (do-test prog)
  (let* ((output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (display (compile-skive-to-if1 prog) output)
    (close-output-port output)
    (display "done!\n")))

(display (expand prog1))
