#lang racket

(require "../../src/expand.rkt")
(require "../../src/analyse.rkt")
(require "../../src/compile.rkt")
(require "../../src/skive.rkt")

(define prog1
  '(if (= 0 1) 1 2))

(define prog2
  '(let ((y (lambda (f)
              ((lambda (x) (x x))
               (lambda (x) (f (lambda (y) ((x x) y)))))))
         (almost-fac (lambda (f)
                       (lambda (n)
                         (if (= n 0)
                             1
                             (* n (f (- n 1))))))))
     (let ((fac (y almost-fac)))
       (fac 10))))

(define prog3
  '(let ((y (lambda (f)
              ((lambda (x) (x x))
               (lambda (x) (f (lambda (y) ((x x) y)))))))
         (almost-fib (lambda (f)
                       (lambda (n)
                         (if (= n 0)
                             1
                             (if (= n 1)
                                 1
                                 (+ (f (- n 1)) (f (- n 2)))))))))
     (let ((fib (y almost-fib)))
       (fib 30))))

(define prog4
  '(let ((fac (lambda (f n)
                (if (= n 0)
                    1
                    (* n (f f (- n 1)))))))
     (fac fac 10)))

(define prog5
  '(let ((almost-fib (lambda (f n)
                       (if (= n 0)
                           1
                           (if (= n 1)
                               1
                               (+ (f f (- n 1)) (f f (- n 2))))))))
    (let ((fib (lambda (n)
                 (almost-fib almost-fib n))))
      (fib 25))))

(define (do-test prog)
  (let* ((output (open-output-file "/tmp/code.if1" #:exists 'truncate)))
    (display (compile-skive-to-if1 prog) output)
    (close-output-port output)
    (display "done!\n")))

(define (fibrec n)
  (if (or (= n 0) (= n 1))
      1
      (+ (fibrec (- n 1)) (fibrec (- n 2)))))

(define (y f)
  ((lambda (x) (x x))
   (lambda (x) (f (lambda (y) ((x x) y))))))

(define (almost-fib f)
  (lambda (n)
    (if (= n 0)
        1
        (if (= n 1)
            1
            (+ (f (- n 1)) (f (- n 2)))))))

(define fiby (y almost-fib))

(define (gewaagde-fib f n)
  (if (= n 0)
      1
      (if (= n 1)
          1
          (+ (f f (- n 1)) (f f (- n 2))))))

(define (fib n)
  (gewaagde-fib gewaagde-fib n))

(define (gewaagd f n)
  (if (= n 0)
      1
      (* n (f f (- n 1)))))

(define (fac n)
  (gewaagd gewaagd n))

(define (almost-fac f)
  (lambda (n)
    (if (= n 0)
        1
        (* n (f (- n 1))))))

(define fac-y
  (y almost-fac))
