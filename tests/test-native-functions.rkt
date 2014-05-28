#lang racket

(require rackunit
         "../src/skive.rkt")

(provide native-function-tests)

(define (do-test source check)
  (let ((thunk (lambda-skive source)))
    (check-equal? (thunk) check)))

(define native-function-tests
  (test-suite
   "Tests for various native Skive functions."

   ;; Test arithmetic functions
   (do-test '(+ (* 1 2)
                (/ 1 (- 3 2)))
            3)

   ;; Test cons, car, cdr
   (do-test '(cons (car (cons 1 2))
                   (cdr (cons 1 2)))
            (cons 1 2))

   ;; Equality tests, lists
   (do-test '(list (= 1 1)   ;; #t
                   (= 1 2)   ;; #f
                   (= 'a 'b) ;; #f
                   (= 'a 'a) ;; #t
                   (= #t #t) ;; #t
                   (= #t #f) ;; #f
                   (= 1 #t)) ;; #f
            (list #t #f #f #t #t #f #f))

   ;; Check lazyness of and/or, list?
   (do-test '(cons (and #f (/ 1 0))
                   (or #f #f (list? (cons 1 '())) (/ 1 0)))
            (cons #f #t))

   ;; Test application of anonymous function
   (do-test '(apply (lambda (a b)
                      (cons a b))
                    (vector 1 2))
            (cons 1 2))

   ;; Test map-vector, vector-ref, make-vector, vector-length
   (do-test '(let ((v (map-vector (lambda (i v)
                                    (* i i))
                                  (make-vector 10 0))))
               (list v (vector-ref v 8) (vector-length v)))
            (list (for/vector ([i (range 0 10)])
                    (* i i))
                  64
                  10))))
