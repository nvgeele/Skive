#lang racket

(require "../src/expand.rkt")
(require "../src/analyse.rkt")
(require "../src/compile.rkt")
(require "../src/skive.rkt")

(define prog
  ;;'(cons 1 (cons 2 (cons 3 '())))
  '(cdr (list 1 2 3 4 5 "lel" '(1 2 3) (list 4 5 6))))

(define prog2
  '(list '(1 2 3)))

(define-skive (test)
  (cdr (list 1 2 3 4 5 "lel" '(1 2 3) (list 4 5 6))))

(define-skive (test2)
  (list '(1 2 3)))

(define (do-test prog)
  (let* ((output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (display (compile-skive-to-if1 prog) output)
    (close-output-port output)
    (display "done!\n")))
