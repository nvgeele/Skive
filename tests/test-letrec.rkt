#lang racket

(require "../src/expand.rkt")
(require "../src/analyse.rkt")
(require "../src/compile.rkt")
(require "../src/skive.rkt")

(define prog
  '(letrec ((fac (lambda (n)
                   (if (= n 0)
                       x
                       (* n (fac (- n 1))))))
            (even? (lambda (n)
                     (if (= n 0)
                         #t
                         (odd? (- n 1)))))
            (odd? (lambda (n)
                    (if (= n 0)
                        #f
                        (even? (- n 1)))))
            (ok (lambda () "OK"))
            (x 1))
     (if (even? 20)
         (fac 10)
         (fac 5))))

(define-skive (test)
  (letrec ((fac (lambda (n)
                  (if (= n 0)
                      x
                      (* n (fac (- n x))))))
           (even? (lambda (n)
                    (if (= n 0)
                        #t
                        (odd? (- n x)))))
           (odd? (lambda (n)
                   (if (= n 0)
                       #f
                       (even? (- n x)))))
           (ok (lambda () "OK"))
           (x 1))
    (if (even? 20)
        (fac 10)
        (fac 5))))

(define (do-test prog)
  (let* ((output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (display (compile-skive-to-if1 prog) output)
    (close-output-port output)
    (display "done!\n")))
