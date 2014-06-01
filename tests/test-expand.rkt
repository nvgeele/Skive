#lang racket

(require rackunit
         "../src/expand.rkt")

(provide expand-tests)

(define (do-test source dest)
  (check-equal? source dest))

(define expand-tests
  (test-suite
   "Tests for expand.rkt"

   (do-test '()
            '())
   (do-test '(list 1 2 a-var)
            '(list 1 2 a-var))
   (do-test '#(1 2 3)
            '(vector 1 2 3))
   (do-test '(let ((x 1))
               x)
            '((lambda (x) x) 1))
   (do-test '(letrec
                 ((rec (lambda (i) (rec i))))
               (rec 10))
            '((lambda (rec)
                ((lambda (rec) (rec 10))
                 (lambda (i) (rec i rec))))
              (lambda (i rec) (rec i rec))))
   (do-test '(or 1 2 3)
            '((lambda (let_0)
                (if let_0
                    let_0
                    ((lambda (let_1)
                       (if let_1
                           let_1
                           3))
                     2)))
              1))
   (do-test '(and 1 2 3)
            '((lambda (let_0)
                (if let_0
                    ((lambda (let_1) let_1)
                     ((lambda (let_2)
                        (if let_2
                            ((lambda (let_3) let_3) 3)
                            #f))
                      2))
                    #f))
              1))
   (do-test '(if 1 2 3)
            '(if 1 2 3))
   (do-test '(let* ((a 2)
                    (b (* a a))
                    (c (* b b)))
               c)
            '((lambda (a)
                ((lambda (b)
                   ((lambda (c)
                      c)
                    (* b b)))
                 (* a a)))
              2))))
