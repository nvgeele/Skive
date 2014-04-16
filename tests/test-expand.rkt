#lang racket

(require rackunit
         "../src/expand.rkt")

(provide expand-tests)

(define expand-tests
  (test-suite
   "Tests for expand.rkt"

   (let ((source '())
         (dest '()))
     (check-equal? source dest))

   (let ((source '(list 1 2 a-var))
         (dest '(list 1 2 a-var)))
     (check-equal? source dest))))
